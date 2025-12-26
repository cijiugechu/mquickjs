use crate::atom::AtomTables;
use crate::capi_defs::{JSInterruptHandler, JSWriteFunc};
use crate::containers::{
    string_alloc_size, value_array_alloc_size, StringHeader, ValueArrayHeader, JS_STRING_LEN_MAX,
};
use crate::cutils::utf8_get;
use crate::enums::JSObjectClass;
use crate::gc_ref::GcRefState;
use crate::heap::{HeapLayout, JS_MIN_FREE_SIZE};
use crate::jsvalue::{
    from_bits, is_ptr, new_short_int, raw_bits, value_from_ptr, value_make_special, value_to_ptr,
    JS_TAG_STRING_CHAR,
};
use crate::jsvalue::{JSValue, JSWord, JSW, JS_NULL, JS_UNDEFINED};
use crate::memblock::{Float64Header, MbHeader, MTag};
use crate::object::{Object, ObjectHeader};
use crate::stdlib::stdlib_image::{StdlibImage, StdlibWord};
use crate::string::js_string::is_ascii_bytes;
use crate::string::string_pos_cache::{StringPosCacheEntry, JS_STRING_POS_CACHE_SIZE};
use core::ffi::c_void;
use core::mem::size_of;
use core::ptr::{self, NonNull};
use core::slice;

const MIN_CONTEXT_BYTES: usize = 1024;
const ROOT_PREFIX_LEN: usize = 4;

const ROOT_CURRENT_EXCEPTION: usize = 0;
const ROOT_EMPTY_PROPS: usize = 1;
const ROOT_GLOBAL_OBJ: usize = 2;
const ROOT_MINUS_ZERO: usize = 3;

/// C: JSContext initialization parameters (JS_NewContext2).
pub struct ContextConfig<'a> {
    pub image: &'a StdlibImage,
    pub memory_size: usize,
    pub prepare_compilation: bool,
}

#[derive(Debug)]
pub enum ContextError {
    MemoryTooSmall { min: usize, actual: usize },
    StdlibWordBytesMismatch { expected: usize, actual: usize },
    StdlibWordTooLarge(u64),
    ClassCountOverflow(u32),
    RomOffsetOutOfBounds { offset: usize, len: usize },
    InvalidValueArrayHeader { offset: usize, tag: MTag },
    InvalidRomString { offset: usize, tag: MTag },
    MissingEmptyAtom,
    StringTooLong { len: usize, max: JSWord },
    OutOfMemory,
}

// Relocated ROM table built from StdlibImage words.
struct RomTable {
    words: Box<[JSWord]>,
    word_bytes: usize,
}

impl RomTable {
    fn from_image(image: &StdlibImage) -> Result<Self, ContextError> {
        let word_bytes = image.word_bytes as usize;
        let mut words = vec![0 as JSWord; image.words.len()].into_boxed_slice();
        let base = words.as_mut_ptr() as *mut u8;
        for (idx, word) in image.words.iter().enumerate() {
            let value = match *word {
                StdlibWord::Raw(raw) => {
                    #[cfg(target_pointer_width = "32")]
                    let max = JSWord::MAX as u64;
                    #[cfg(target_pointer_width = "64")]
                    let max = JSWord::MAX;
                    if raw > max {
                        return Err(ContextError::StdlibWordTooLarge(raw));
                    }
                    raw as JSWord
                }
                StdlibWord::RomOffset(offset) => {
                    let ptr = unsafe {
                        // SAFETY: base is valid for the table size and offset is word-based.
                        base.add(offset as usize * word_bytes)
                    };
                    let ptr = NonNull::new(ptr).expect("ROM offset must be non-null");
                    raw_bits(value_from_ptr(ptr))
                }
            };
            words[idx] = value;
        }
        Ok(Self { words, word_bytes })
    }

    fn len(&self) -> usize {
        self.words.len()
    }

    fn base_ptr(&self) -> *mut u8 {
        self.words.as_ptr() as *mut u8
    }

    fn bytes_len(&self) -> usize {
        self.len() * self.word_bytes
    }

    fn word(&self, offset: usize) -> Result<JSWord, ContextError> {
        self.words
            .get(offset)
            .copied()
            .ok_or(ContextError::RomOffsetOutOfBounds {
                offset,
                len: self.words.len(),
            })
    }

    fn value_array(&self, offset: usize) -> Result<Vec<JSValue>, ContextError> {
        let header_word = self.word(offset)?;
        let header = MbHeader::from_word(header_word);
        if header.tag() != MTag::ValueArray {
            return Err(ContextError::InvalidValueArrayHeader {
                offset,
                tag: header.tag(),
            });
        }
        let size = ValueArrayHeader::from(header).size() as usize;
        let start = offset + 1;
        let end = start + size;
        if end > self.len() {
            return Err(ContextError::RomOffsetOutOfBounds {
                offset: end,
                len: self.len(),
            });
        }
        let mut values = Vec::with_capacity(size);
        for word in &self.words[start..end] {
            values.push(from_bits(*word));
        }
        Ok(values)
    }
}

/// Rust runtime context mirroring the C JSContext memory map and roots.
///
/// Invariants:
/// - Heap/stack share one contiguous buffer; stack_top is the buffer end.
/// - `class_roots` stores current_exception/empty_props/global_obj/minus_zero
///   followed by class_proto[] and class_obj[].
/// - ROM atoms remain alive via AtomTables/ROM table ownership.
#[allow(dead_code)]
pub struct JSContext {
    heap_storage: Box<[JSWord]>,
    heap: HeapLayout,
    sp: NonNull<JSValue>,
    fp: NonNull<JSValue>,
    class_roots: Vec<JSValue>,
    class_count: u16,
    class_proto_offset: usize,
    class_obj_offset: usize,
    atom_tables: AtomTables,
    rom_table: Option<RomTable>,
    n_rom_atom_tables: u8,
    string_pos_cache: [StringPosCacheEntry; JS_STRING_POS_CACHE_SIZE],
    string_pos_cache_counter: u8,
    gc_refs: GcRefState,
    random_state: u64,
    interrupt_counter: i16,
    interrupt_handler: Option<JSInterruptHandler>,
    write_func: JSWriteFunc,
    opaque: *mut c_void,
    in_out_of_memory: bool,
    current_exception_is_uncatchable: bool,
    js_call_rec_count: i32,
}

impl JSContext {
    pub fn new(config: ContextConfig<'_>) -> Result<Self, ContextError> {
        let word_bytes = JSW as usize;
        if config.image.word_bytes as usize != word_bytes {
            return Err(ContextError::StdlibWordBytesMismatch {
                expected: word_bytes,
                actual: config.image.word_bytes as usize,
            });
        }
        let mem_size = config.memory_size & !(word_bytes - 1);
        if mem_size < MIN_CONTEXT_BYTES {
            return Err(ContextError::MemoryTooSmall {
                min: MIN_CONTEXT_BYTES,
                actual: mem_size,
            });
        }
        let class_count = u16::try_from(config.image.class_count)
            .map_err(|_| ContextError::ClassCountOverflow(config.image.class_count))?;

        let words = mem_size / word_bytes;
        let mut heap_storage = vec![0 as JSWord; words].into_boxed_slice();
        let base = NonNull::new(heap_storage.as_mut_ptr() as *mut u8)
            .expect("heap storage must be non-null");
        let stack_top = unsafe {
            // SAFETY: heap_storage has `mem_size` writable bytes.
            NonNull::new_unchecked(base.as_ptr().add(mem_size))
        };
        let stack_bottom = NonNull::new(stack_top.as_ptr() as *mut JSValue)
            .expect("stack bottom must be non-null");
        let heap = HeapLayout::new(base, base, stack_top, stack_bottom, JS_MIN_FREE_SIZE as usize);

        let root_len = ROOT_PREFIX_LEN + (class_count as usize) * 2;
        let mut class_roots = vec![JS_NULL; root_len];
        class_roots[ROOT_CURRENT_EXCEPTION] = JS_UNDEFINED;

        let mut ctx = Self {
            heap_storage,
            heap,
            sp: stack_bottom,
            fp: stack_bottom,
            class_roots,
            class_count,
            class_proto_offset: ROOT_PREFIX_LEN,
            class_obj_offset: ROOT_PREFIX_LEN + class_count as usize,
            atom_tables: AtomTables::new(),
            rom_table: None,
            n_rom_atom_tables: 0,
            string_pos_cache: [StringPosCacheEntry::new(JS_NULL, 0, 0); JS_STRING_POS_CACHE_SIZE],
            string_pos_cache_counter: 0,
            gc_refs: GcRefState::new(),
            random_state: 1,
            interrupt_counter: 0,
            interrupt_handler: None,
            write_func: dummy_write_func,
            opaque: ptr::null_mut(),
            in_out_of_memory: false,
            current_exception_is_uncatchable: false,
            js_call_rec_count: 0,
        };

        ctx.init_atoms(config.image, config.prepare_compilation)?;
        ctx.init_roots()?;

        Ok(ctx)
    }

    pub fn heap(&self) -> &HeapLayout {
        &self.heap
    }

    pub fn heap_mut(&mut self) -> &mut HeapLayout {
        &mut self.heap
    }

    pub fn sp(&self) -> NonNull<JSValue> {
        self.sp
    }

    pub fn fp(&self) -> NonNull<JSValue> {
        self.fp
    }

    pub fn stack_top(&self) -> NonNull<u8> {
        self.heap.stack_top()
    }

    pub fn stack_bottom(&self) -> NonNull<JSValue> {
        self.heap.stack_bottom()
    }

    pub fn class_count(&self) -> u16 {
        self.class_count
    }

    pub fn class_proto(&self) -> &[JSValue] {
        let start = self.class_proto_offset;
        let end = start + self.class_count as usize;
        &self.class_roots[start..end]
    }

    pub fn class_proto_mut(&mut self) -> &mut [JSValue] {
        let start = self.class_proto_offset;
        let end = start + self.class_count as usize;
        &mut self.class_roots[start..end]
    }

    pub fn class_obj(&self) -> &[JSValue] {
        let start = self.class_obj_offset;
        let end = start + self.class_count as usize;
        &self.class_roots[start..end]
    }

    pub fn class_obj_mut(&mut self) -> &mut [JSValue] {
        let start = self.class_obj_offset;
        let end = start + self.class_count as usize;
        &mut self.class_roots[start..end]
    }

    pub fn n_rom_atom_tables(&self) -> u8 {
        self.n_rom_atom_tables
    }

    pub fn atom_tables(&self) -> &AtomTables {
        &self.atom_tables
    }

    pub fn atom_tables_mut(&mut self) -> &mut AtomTables {
        &mut self.atom_tables
    }

    pub fn empty_props(&self) -> JSValue {
        self.class_roots[ROOT_EMPTY_PROPS]
    }

    pub fn global_obj(&self) -> JSValue {
        self.class_roots[ROOT_GLOBAL_OBJ]
    }

    pub fn minus_zero(&self) -> JSValue {
        self.class_roots[ROOT_MINUS_ZERO]
    }

    pub fn new_string_len(&mut self, bytes: &[u8]) -> Result<JSValue, ContextError> {
        if bytes.is_empty() {
            return self
                .atom_tables
                .empty_string_atom()
                .ok_or(ContextError::MissingEmptyAtom);
        }
        if let Some(codepoint) = single_codepoint(bytes) {
            return Ok(value_make_special(JS_TAG_STRING_CHAR, codepoint));
        }
        let is_ascii = is_ascii_bytes(bytes);
        self.alloc_string_bytes(bytes, false, is_ascii, false)
    }

    pub fn new_string(&mut self, input: &str) -> Result<JSValue, ContextError> {
        self.new_string_len(input.as_bytes())
    }

    pub fn intern_string(&mut self, bytes: &[u8]) -> Result<JSValue, ContextError> {
        let val = self.new_string_len(bytes)?;
        Ok(self.atom_tables.make_unique_string(val))
    }

    fn init_atoms(&mut self, image: &StdlibImage, prepare_compilation: bool) -> Result<(), ContextError> {
        let rom_table = RomTable::from_image(image)?;
        let raw_atoms = rom_table.value_array(image.sorted_atoms_offset as usize)?;
        let mut atoms = Vec::with_capacity(raw_atoms.len());
        for val in raw_atoms {
            atoms.push(self.decode_rom_atom(&rom_table, val)?);
        }
        if prepare_compilation {
            self.atom_tables.set_unique_strings(atoms);
            self.n_rom_atom_tables = 0;
        } else {
            self.atom_tables.add_rom_table(atoms);
            self.n_rom_atom_tables = 1;
        }
        self.rom_table = Some(rom_table);
        Ok(())
    }

    fn decode_rom_atom(&mut self, rom_table: &RomTable, val: JSValue) -> Result<JSValue, ContextError> {
        if !is_ptr(val) {
            return Ok(val);
        }
        let Some(ptr) = value_to_ptr::<u8>(val) else {
            return Ok(val);
        };
        let base = rom_table.base_ptr() as usize;
        let addr = ptr.as_ptr() as usize;
        if addr < base {
            return Err(ContextError::RomOffsetOutOfBounds {
                offset: addr,
                len: rom_table.bytes_len(),
            });
        }
        let offset = addr - base;
        let end = offset + size_of::<JSWord>();
        if end > rom_table.bytes_len() {
            return Err(ContextError::RomOffsetOutOfBounds {
                offset: end,
                len: rom_table.bytes_len(),
            });
        }
        let header_word = unsafe {
            // SAFETY: ptr is within the ROM table bounds.
            ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
        };
        let header = MbHeader::from_word(header_word);
        if header.tag() != MTag::String {
            return Err(ContextError::InvalidRomString {
                offset,
                tag: header.tag(),
            });
        }
        let header = StringHeader::from(header);
        let len = header.len() as usize;
        let start = offset + size_of::<JSWord>();
        let end = start + len;
        if end > rom_table.bytes_len() {
            return Err(ContextError::RomOffsetOutOfBounds {
                offset: end,
                len: rom_table.bytes_len(),
            });
        }
        let bytes = unsafe {
            // SAFETY: bytes are within the ROM table bounds.
            slice::from_raw_parts(ptr.as_ptr().add(size_of::<JSWord>()), len)
        };
        self.alloc_string_bytes(bytes, header.is_unique(), header.is_ascii(), header.is_numeric())
    }

    fn init_roots(&mut self) -> Result<(), ContextError> {
        let empty_props = self.alloc_empty_props()?;
        self.class_roots[ROOT_EMPTY_PROPS] = empty_props;

        let obj_proto = self.alloc_object(JSObjectClass::Object, JS_NULL, 0)?;
        self.class_proto_mut()[JSObjectClass::Object as usize] = obj_proto;

        let closure_proto = self.alloc_object(JSObjectClass::Object, obj_proto, 0)?;
        self.class_proto_mut()[JSObjectClass::Closure as usize] = closure_proto;

        let global_obj = self.alloc_object(JSObjectClass::Object, obj_proto, 0)?;
        self.class_roots[ROOT_GLOBAL_OBJ] = global_obj;

        let minus_zero = self.alloc_float64(-0.0)?;
        self.class_roots[ROOT_MINUS_ZERO] = minus_zero;

        Ok(())
    }

    fn alloc_empty_props(&mut self) -> Result<JSValue, ContextError> {
        let ptr = self.alloc_value_array(3)?;
        unsafe {
            // SAFETY: ptr points to a freshly allocated value array.
            let arr = ptr.as_ptr().add(size_of::<JSWord>()) as *mut JSValue;
            ptr::write_unaligned(arr, new_short_int(0));
            ptr::write_unaligned(arr.add(1), new_short_int(0));
            ptr::write_unaligned(arr.add(2), new_short_int(0));
        }
        Ok(value_from_ptr(ptr))
    }

    fn alloc_value_array(&mut self, size: usize) -> Result<NonNull<u8>, ContextError> {
        let alloc_size = value_array_alloc_size(size as JSWord);
        let ptr = self
            .heap
            .malloc(alloc_size, MTag::ValueArray, |_| {})
            .ok_or(ContextError::OutOfMemory)?;
        let header = ValueArrayHeader::new(size as JSWord, false);
        unsafe {
            // SAFETY: ptr points to a writable ValueArray header word.
            ptr::write_unaligned(ptr.as_ptr().cast::<JSWord>(), MbHeader::from(header).word());
            let arr = ptr.as_ptr().add(size_of::<JSWord>()) as *mut JSValue;
            for i in 0..size {
                ptr::write_unaligned(arr.add(i), JS_UNDEFINED);
            }
        }
        Ok(ptr)
    }

    fn alloc_object(
        &mut self,
        class_id: JSObjectClass,
        proto: JSValue,
        extra_size_bytes: usize,
    ) -> Result<JSValue, ContextError> {
        let extra_words = extra_size_bytes.div_ceil(JSW as usize);
        let size = Object::PAYLOAD_OFFSET + extra_words * JSW as usize;
        let ptr = self
            .heap
            .malloc(size, MTag::Object, |_| {})
            .ok_or(ContextError::OutOfMemory)?;
        let header = ObjectHeader::new(class_id as u8, extra_words as JSWord, false);
        unsafe {
            // SAFETY: ptr is a writable object allocation.
            ptr::write_unaligned(ptr.as_ptr().cast::<JSWord>(), header.header().word());
            let proto_ptr = ptr.as_ptr().add(Object::PROTO_OFFSET) as *mut JSValue;
            let props_ptr = ptr.as_ptr().add(Object::PROPS_OFFSET) as *mut JSValue;
            ptr::write_unaligned(proto_ptr, proto);
            ptr::write_unaligned(props_ptr, self.empty_props());
            if extra_words > 0 {
                ptr::write_bytes(ptr.as_ptr().add(Object::PAYLOAD_OFFSET), 0, extra_words * JSW as usize);
            }
        }
        Ok(value_from_ptr(ptr))
    }

    fn alloc_float64(&mut self, value: f64) -> Result<JSValue, ContextError> {
        let size = size_of::<JSWord>() + size_of::<f64>();
        let ptr = self
            .heap
            .malloc(size, MTag::Float64, |_| {})
            .ok_or(ContextError::OutOfMemory)?;
        let header = Float64Header::new(false);
        unsafe {
            // SAFETY: ptr is a writable float64 allocation.
            ptr::write_unaligned(ptr.as_ptr().cast::<JSWord>(), MbHeader::from(header).word());
            let payload = ptr.as_ptr().add(size_of::<JSWord>()) as *mut f64;
            ptr::write_unaligned(payload, value);
        }
        Ok(value_from_ptr(ptr))
    }

    fn alloc_string_bytes(
        &mut self,
        bytes: &[u8],
        is_unique: bool,
        is_ascii: bool,
        is_numeric: bool,
    ) -> Result<JSValue, ContextError> {
        let len = JSWord::try_from(bytes.len()).map_err(|_| ContextError::StringTooLong {
            len: bytes.len(),
            max: JS_STRING_LEN_MAX,
        })?;
        if len > JS_STRING_LEN_MAX {
            return Err(ContextError::StringTooLong {
                len: bytes.len(),
                max: JS_STRING_LEN_MAX,
            });
        }
        let size = string_alloc_size(len);
        let ptr = self
            .heap
            .malloc(size, MTag::String, |_| {})
            .ok_or(ContextError::OutOfMemory)?;
        let header = StringHeader::new(len, is_unique, is_ascii, is_numeric, false);
        unsafe {
            // SAFETY: `ptr` is a valid string allocation in the heap.
            ptr::write_unaligned(ptr.as_ptr().cast::<JSWord>(), MbHeader::from(header).word());
            let payload = ptr.as_ptr().add(size_of::<JSWord>());
            ptr::write_bytes(payload, 0, size - size_of::<JSWord>());
            ptr::copy_nonoverlapping(bytes.as_ptr(), payload, bytes.len());
        }
        Ok(value_from_ptr(ptr))
    }

}

fn single_codepoint(bytes: &[u8]) -> Option<u32> {
    let mut clen = 0usize;
    let c = utf8_get(bytes, &mut clen);
    if c < 0 {
        return None;
    }
    if clen == bytes.len() {
        Some(c as u32)
    } else {
        None
    }
}

unsafe extern "C" fn dummy_write_func(_opaque: *mut c_void, _buf: *const c_void, _buf_len: usize) {}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;
    use crate::jsvalue::{
        is_int, is_ptr, value_get_int, value_get_special_tag, value_get_special_value,
        JS_TAG_STRING_CHAR,
    };
    use crate::memblock::Float64Header;
    use crate::stdlib::MQUICKJS_STDLIB_IMAGE;
    use core::slice;

    #[test]
    fn context_initializes_layout_and_roots() {
        let ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
        })
        .expect("context init");

        assert!(ctx.heap().is_valid());
        assert_eq!(ctx.sp(), ctx.fp());
        assert_eq!(ctx.sp(), ctx.stack_bottom());
        assert!(ctx.stack_top().as_ptr() as usize >= ctx.sp().as_ptr() as usize);
        assert_eq!(ctx.class_count() as u32, MQUICKJS_STDLIB_IMAGE.class_count);

        let class_proto = ctx.class_proto();
        assert_eq!(class_proto.len() as u32, MQUICKJS_STDLIB_IMAGE.class_count);
        assert!(is_ptr(class_proto[JSObjectClass::Object as usize]));
        assert!(is_ptr(class_proto[JSObjectClass::Closure as usize]));

        assert!(is_ptr(ctx.global_obj()));
        assert!(is_ptr(ctx.minus_zero()));
    }

    #[test]
    fn context_empty_props_matches_c_layout() {
        let ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
        })
        .expect("context init");

        let ptr = value_to_ptr::<u8>(ctx.empty_props()).expect("empty_props ptr");
        let header_word = unsafe {
            // SAFETY: ptr points to a readable ValueArray header.
            ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
        };
        let header = ValueArrayHeader::from(MbHeader::from_word(header_word));
        assert_eq!(header.size(), 3);

        let arr = unsafe {
            // SAFETY: array elements are within the ValueArray allocation.
            ptr.as_ptr().add(size_of::<JSWord>()) as *const JSValue
        };
        for i in 0..3 {
            let val = unsafe { ptr::read_unaligned(arr.add(i)) };
            assert!(is_int(val));
            assert_eq!(value_get_int(val), 0);
        }
    }

    #[test]
    fn context_minus_zero_is_float64() {
        let ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
        })
        .expect("context init");

        let ptr = value_to_ptr::<u8>(ctx.minus_zero()).expect("minus_zero ptr");
        let header_word = unsafe {
            // SAFETY: ptr points to a readable Float64 header.
            ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
        };
        let header = Float64Header::from(MbHeader::from_word(header_word));
        assert_eq!(header.header().tag(), MTag::Float64);
        let payload = unsafe {
            // SAFETY: payload lies within the Float64 allocation.
            ptr::read_unaligned(ptr.as_ptr().add(size_of::<JSWord>()) as *const f64)
        };
        assert_eq!(payload.to_bits(), (-0.0f64).to_bits());
    }

    #[test]
    fn context_new_string_len_handles_empty_and_char() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
        })
        .expect("context init");

        let empty_atom = ctx
            .atom_tables()
            .empty_string_atom()
            .expect("empty atom");
        let empty = ctx.new_string_len(b"").expect("empty string");
        assert_eq!(empty, empty_atom);

        let char_val = ctx.new_string_len(b"a").expect("char string");
        assert_eq!(value_get_special_tag(char_val), JS_TAG_STRING_CHAR);
        assert_eq!(value_get_special_value(char_val), b'a' as i32);

        let string_val = ctx.new_string_len(b"ab").expect("string");
        assert!(is_ptr(string_val));
        let ptr = value_to_ptr::<u8>(string_val).expect("string ptr");
        let header_word = unsafe {
            // SAFETY: ptr points to a readable String header.
            ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
        };
        let header = StringHeader::from(MbHeader::from_word(header_word));
        assert_eq!(header.len(), 2);
        let bytes = unsafe {
            // SAFETY: string payload lies within the allocation.
            slice::from_raw_parts(ptr.as_ptr().add(size_of::<JSWord>()), 2)
        };
        assert_eq!(bytes, b"ab");
    }

    #[test]
    fn context_rom_atoms_live_in_heap() {
        let ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
        })
        .expect("context init");

        let empty_atom = ctx
            .atom_tables()
            .empty_string_atom()
            .expect("empty atom");
        let ptr = value_to_ptr::<u8>(empty_atom).expect("empty atom ptr");
        assert!(ctx.heap().ptr_in_heap(ptr));
    }

    #[test]
    fn prepare_compilation_moves_atoms_into_unique_strings() {
        let ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 32 * 1024,
            prepare_compilation: true,
        })
        .expect("context init");

        let rom_table = RomTable::from_image(&MQUICKJS_STDLIB_IMAGE).expect("rom table");
        let expected = rom_table
            .value_array(MQUICKJS_STDLIB_IMAGE.sorted_atoms_offset as usize)
            .expect("sorted atoms")
            .len();
        assert_eq!(ctx.atom_tables().unique_len(), expected);
        assert_eq!(ctx.n_rom_atom_tables(), 0);
    }
}
