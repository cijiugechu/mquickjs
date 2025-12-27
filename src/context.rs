use crate::atom::AtomTables;
use crate::capi_defs::{JSInterruptHandler, JSWriteFunc};
use crate::containers::{
    string_alloc_size, value_array_alloc_size, StringHeader, ValueArrayHeader, JS_STRING_LEN_MAX,
};
use crate::cutils::{float64_as_uint64, unicode_to_utf8, utf8_get};
use crate::enums::JSObjectClass;
use crate::gc_ref::GcRefState;
use crate::heap::{HeapLayout, JS_MIN_FREE_SIZE};
use crate::jsvalue::{
    from_bits, is_ptr, new_short_int, raw_bits, value_from_ptr, value_get_special_tag,
    value_get_special_value, value_make_special, JSValue, JSWord, JSW, JS_NULL, JS_SHORTINT_MAX,
    JS_SHORTINT_MIN, JS_TAG_STRING_CHAR, JS_UNDEFINED,
};
use crate::memblock::{Float64Header, MbHeader, MTag};
use crate::object::{Object, ObjectHeader};
use crate::stdlib::stdlib_image::{StdlibImage, StdlibWord};
use crate::string::js_string::is_ascii_bytes;
use crate::string::runtime::{is_valid_len4_utf8, string_view, utf8_char_len};
use crate::string::string_pos_cache::{
    StringPosCacheEntry, StringPosType, JS_STRING_POS_CACHE_MIN_LEN, JS_STRING_POS_CACHE_SIZE,
};
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

struct HeapStorage {
    ptr: NonNull<[JSWord]>,
}

impl HeapStorage {
    fn new(words: usize) -> Self {
        let boxed = vec![0 as JSWord; words].into_boxed_slice();
        let ptr = NonNull::new(Box::into_raw(boxed)).expect("heap storage must be non-null");
        Self { ptr }
    }

    fn base_ptr(&self) -> NonNull<u8> {
        let data = self.ptr.as_ptr().cast::<JSWord>();
        NonNull::new(data as *mut u8).expect("heap storage must be non-null")
    }
}

impl Drop for HeapStorage {
    fn drop(&mut self) {
        unsafe {
            // SAFETY: ptr was created from Box::into_raw in HeapStorage::new.
            drop(Box::from_raw(self.ptr.as_ptr()));
        }
    }
}

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
    heap_storage: HeapStorage,
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
        let heap_storage = HeapStorage::new(words);
        let base = heap_storage.base_ptr();
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
            heap: HeapLayout::new(
                NonNull::dangling(),
                NonNull::dangling(),
                NonNull::dangling(),
                NonNull::dangling(),
                JS_MIN_FREE_SIZE as usize,
            ),
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

        ctx.heap = heap;

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

    pub fn new_float64(&mut self, value: f64) -> Result<JSValue, ContextError> {
        if value >= JS_SHORTINT_MIN as f64 && value <= JS_SHORTINT_MAX as f64 {
            let val = value as i32;
            if float64_as_uint64(value) == float64_as_uint64(val as f64) {
                return Ok(new_short_int(val));
            }
        }
        self.new_float64_slow(value)
    }

    fn new_float64_slow(&mut self, value: f64) -> Result<JSValue, ContextError> {
        if float64_as_uint64(value) == 0x8000_0000_0000_0000 {
            return Ok(self.minus_zero());
        }
        #[cfg(target_pointer_width = "64")]
        {
            let abs = value.abs();
            let min = 2.0_f64.powi(-127);
            let max = 2.0_f64.powi(128);
            if abs >= min && abs <= max {
                return Ok(crate::jsvalue::short_float_from_f64(value));
            }
        }
        self.alloc_float64(value)
    }

    pub fn is_rom_ptr(&self, ptr: NonNull<u8>) -> bool {
        let addr = ptr.as_ptr() as usize;
        let base = self.heap.heap_base().as_ptr() as usize;
        let end = self.stack_top().as_ptr() as usize;
        addr < base || addr >= end
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

    #[allow(dead_code)]
    pub(crate) fn string_utf16_to_utf8_pos(&mut self, val: JSValue, utf16_pos: u32) -> u32 {
        string_convert_pos(
            &mut self.string_pos_cache,
            &mut self.string_pos_cache_counter,
            val,
            utf16_pos,
            StringPosType::Utf16,
        )
    }

    #[allow(dead_code)]
    pub(crate) fn string_utf8_to_utf16_pos(&mut self, val: JSValue, utf8_pos: u32) -> u32 {
        string_convert_pos(
            &mut self.string_pos_cache,
            &mut self.string_pos_cache_counter,
            val,
            utf8_pos,
            StringPosType::Utf8,
        )
    }

    #[allow(dead_code)]
    pub(crate) fn string_len(&mut self, val: JSValue) -> u32 {
        if value_get_special_tag(val) == JS_TAG_STRING_CHAR {
            let codepoint = value_get_special_value(val) as u32;
            return if codepoint >= 0x10000 { 2 } else { 1 };
        }
        let mut scratch = [0u8; 5];
        let Some(view) = string_view(val, &mut scratch) else {
            debug_assert!(false, "expected string value");
            return 0;
        };
        let len = view.bytes().len() as u32;
        if view.is_ascii() {
            len
        } else {
            self.string_utf8_to_utf16_pos(val, len * 2)
        }
    }

    #[allow(dead_code)]
    pub(crate) fn string_getcp(&mut self, val: JSValue, utf16_pos: u32, is_codepoint: bool) -> i32 {
        let utf8_pos = self.string_utf16_to_utf8_pos(val, utf16_pos);
        let surrogate_flag = (utf8_pos & 1) != 0;
        let utf8_pos = (utf8_pos >> 1) as usize;
        let mut scratch = [0u8; 5];
        let Some(view) = string_view(val, &mut scratch) else {
            debug_assert!(false, "expected string value");
            return -1;
        };
        if utf8_pos >= view.bytes().len() {
            return -1;
        }
        let mut clen = 0usize;
        let c = utf8_get(&view.bytes()[utf8_pos..], &mut clen);
        if c < 0 {
            return -1;
        }
        let c = c as u32;
        if c < 0x10000 || (!surrogate_flag && is_codepoint) {
            return c as i32;
        }
        let c = c - 0x10000;
        if !surrogate_flag {
            (0xd800 + (c >> 10)) as i32
        } else {
            (0xdc00 + (c & 0x3ff)) as i32
        }
    }

    #[allow(dead_code)]
    pub(crate) fn string_getc(&mut self, val: JSValue, utf16_pos: u32) -> i32 {
        self.string_getcp(val, utf16_pos, false)
    }

    #[allow(dead_code)]
    pub(crate) fn sub_string_utf8(
        &mut self,
        val: JSValue,
        start0: u32,
        end0: u32,
    ) -> Result<JSValue, ContextError> {
        if end0 <= start0 {
            return self.new_string_len(b"");
        }
        let start_surrogate = (start0 & 1) != 0;
        let end_surrogate = (end0 & 1) != 0;
        let mut start = (start0 >> 1) as usize;
        let end = (end0 >> 1) as usize;
        let len = end.saturating_sub(start);
        let mut scratch = [0u8; 5];
        let Some(view) = string_view(val, &mut scratch) else {
            debug_assert!(false, "expected string value");
            return self.new_string_len(b"");
        };
        let bytes = view.bytes();
        if !start_surrogate && !end_surrogate {
            if len == 0 {
                return self.new_string_len(b"");
            }
            if start < bytes.len() && utf8_char_len(bytes[start]) == len {
                let mut clen = 0usize;
                let c = utf8_get(&bytes[start..], &mut clen);
                if c >= 0 {
                    return Ok(value_make_special(JS_TAG_STRING_CHAR, c as u32));
                }
            }
            let slice = if start <= end && end <= bytes.len() {
                &bytes[start..end]
            } else {
                &bytes[0..0]
            };
            let is_ascii = if view.is_ascii() { true } else { is_ascii_bytes(slice) };
            return self.alloc_string_bytes(slice, false, is_ascii, false);
        }

        let extra = if end_surrogate { 3 } else { 0 };
        let target_len = len.saturating_sub(start_surrogate as usize) + extra;
        let mut out = Vec::with_capacity(target_len);
        if start_surrogate && start < bytes.len() {
            let mut clen = 0usize;
            let c = utf8_get(&bytes[start..], &mut clen) as u32;
            let c = 0xdc00 + (c.wrapping_sub(0x10000) & 0x3ff);
            let mut tmp = [0u8; 4];
            let written = unicode_to_utf8(&mut tmp, c);
            out.extend_from_slice(&tmp[..written]);
            start = start.saturating_add(4);
        }
        if start <= end && end <= bytes.len() {
            out.extend_from_slice(&bytes[start..end]);
        }
        if end_surrogate && end <= bytes.len() {
            let mut clen = 0usize;
            let c = utf8_get(&bytes[end..], &mut clen) as u32;
            let c = 0xd800 + (c.wrapping_sub(0x10000) >> 10);
            let mut tmp = [0u8; 4];
            let written = unicode_to_utf8(&mut tmp, c);
            out.extend_from_slice(&tmp[..written]);
        }
        self.alloc_string_bytes(&out, false, false, false)
    }

    #[allow(dead_code)]
    pub(crate) fn sub_string(
        &mut self,
        val: JSValue,
        start: u32,
        end: u32,
    ) -> Result<JSValue, ContextError> {
        if end <= start {
            return self.new_string_len(b"");
        }
        let start_utf8 = self.string_utf16_to_utf8_pos(val, start);
        let end_utf8 = self.string_utf16_to_utf8_pos(val, end);
        self.sub_string_utf8(val, start_utf8, end_utf8)
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
        let tagged = raw_bits(val) as usize;
        let addr = tagged & !((JSW as usize) - 1);
        let base = rom_table.base_ptr() as usize;
        if addr < base {
            return Err(ContextError::RomOffsetOutOfBounds {
                offset: addr,
                len: rom_table.bytes_len(),
            });
        }
        let offset = addr - base;
        let ptr = unsafe {
            // SAFETY: offset is bounds-checked against the ROM table size.
            rom_table.base_ptr().add(offset)
        };
        let end = offset + size_of::<JSWord>();
        if end > rom_table.bytes_len() {
            return Err(ContextError::RomOffsetOutOfBounds {
                offset: end,
                len: rom_table.bytes_len(),
            });
        }
        let header_word = unsafe {
            // SAFETY: ptr is within the ROM table bounds.
            ptr::read_unaligned(ptr.cast::<JSWord>())
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
            slice::from_raw_parts(ptr.add(size_of::<JSWord>()), len)
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

    pub(crate) fn alloc_value_array(&mut self, size: usize) -> Result<NonNull<u8>, ContextError> {
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

    pub(crate) fn alloc_object(
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

    pub(crate) fn alloc_string_bytes(
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

#[allow(dead_code)]
fn string_convert_pos(
    cache: &mut [StringPosCacheEntry; JS_STRING_POS_CACHE_SIZE],
    cache_counter: &mut u8,
    val: JSValue,
    mut pos: u32,
    pos_type: StringPosType,
) -> u32 {
    let mut scratch = [0u8; 5];
    let Some(view) = string_view(val, &mut scratch) else {
        debug_assert!(false, "expected string value");
        return 0;
    };
    let bytes = view.bytes();
    let len = bytes.len() as u32;
    if view.is_ascii() {
        return match pos_type {
            StringPosType::Utf8 => len.min(pos / 2),
            StringPosType::Utf16 => len.min(pos) * 2,
        };
    }

    let mut has_surrogate = 0u32;
    if pos_type == StringPosType::Utf8 {
        has_surrogate = pos & 1;
        pos >>= 1;
    }

    let mut ce_index = None;
    let mut i = 0usize;
    let mut j = 0u32;
    if len >= JS_STRING_POS_CACHE_MIN_LEN {
        let mut d_min = pos;
        for (idx, entry) in cache.iter().enumerate() {
            if entry.str() == val {
                let entry_pos = entry.pos(pos_type);
                let d = entry_pos.abs_diff(pos);
                if d < d_min {
                    d_min = d;
                    ce_index = Some(idx);
                }
            }
        }
        if ce_index.is_none() {
            let idx = *cache_counter as usize;
            *cache_counter += 1;
            if (*cache_counter as usize) == JS_STRING_POS_CACHE_SIZE {
                *cache_counter = 0;
            }
            cache[idx].set_str(val);
            cache[idx].set_positions(0, 0);
            ce_index = Some(idx);
        }
        if let Some(idx) = ce_index {
            i = cache[idx].pos(StringPosType::Utf8) as usize;
            j = cache[idx].pos(StringPosType::Utf16);
        }
    }

    let ce_pos = ce_index.map(|idx| cache[idx].pos(pos_type)).unwrap_or(0);
    let forward = ce_index.is_none() || ce_pos <= pos;
    let mut surrogate_flag = 0u32;
    if forward {
        let mut limit = u32::MAX;
        let mut len_limit = bytes.len();
        if pos_type == StringPosType::Utf8 {
            len_limit = (pos as usize).min(bytes.len());
        } else {
            limit = pos;
        }
        while i < len_limit {
            if j == limit {
                break;
            }
            let clen = utf8_char_len(bytes[i]);
            if clen == 4 && is_valid_len4_utf8(&bytes[i..]) {
                if (j + 1) == limit {
                    surrogate_flag = 1;
                    break;
                }
                j = j.saturating_add(2);
            } else {
                j = j.saturating_add(1);
            }
            i = i.saturating_add(clen);
        }
    } else {
        let (start, limit) = if pos_type == StringPosType::Utf8 {
            (pos as usize, u32::MAX)
        } else {
            (0usize, pos)
        };
        while i > start {
            let i0 = i;
            i -= 1;
            while i > 0 && (bytes[i] & 0xc0) == 0x80 {
                i -= 1;
            }
            let clen = i0 - i;
            if clen == 4 && is_valid_len4_utf8(&bytes[i..]) {
                j = j.saturating_sub(2);
                if (j + 1) == limit {
                    surrogate_flag = 1;
                    break;
                }
            } else {
                j = j.saturating_sub(1);
            }
            if j == limit {
                break;
            }
        }
    }

    if let Some(idx) = ce_index {
        cache[idx].set_positions(i as u32, j);
    }

    match pos_type {
        StringPosType::Utf8 => j + has_surrogate,
        StringPosType::Utf16 => (i.saturating_mul(2) as u32) + surrogate_flag,
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
    use crate::cutils::unicode_to_utf8;
    use crate::jsvalue::{
        is_int, is_ptr, value_get_int, value_get_special_tag, value_get_special_value,
        value_to_ptr, JS_TAG_STRING_CHAR,
    };
    use crate::memblock::Float64Header;
    use crate::string::runtime::string_view;
    use crate::stdlib::MQUICKJS_STDLIB_IMAGE;
    use core::slice;

    fn bytes_from_val(val: JSValue) -> Vec<u8> {
        let mut scratch = [0u8; 5];
        let view = string_view(val, &mut scratch).expect("string view");
        view.bytes().to_vec()
    }

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
    fn context_new_float64_int_roundtrip() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
        })
        .expect("context init");

        let val = ctx.new_float64(42.0).expect("alloc");
        assert!(is_int(val));
        assert_eq!(value_get_int(val), 42);
    }

    #[test]
    fn context_new_float64_allocates_large_value() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
        })
        .expect("context init");

        let val = ctx.new_float64(1.0e300).expect("alloc");
        assert!(is_ptr(val));
    }

    #[test]
    fn context_new_float64_caches_minus_zero() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
        })
        .expect("context init");

        let first = ctx.new_float64(-0.0).expect("alloc");
        let second = ctx.new_float64(-0.0).expect("alloc");
        assert_eq!(first, second);
        assert_eq!(first, ctx.minus_zero());
    }

    #[cfg(target_pointer_width = "64")]
    #[test]
    fn context_new_float64_uses_short_float() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
        })
        .expect("context init");

        let val = ctx.new_float64(1.5).expect("alloc");
        assert!(crate::jsvalue::is_short_float(val));
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
    fn string_pos_ascii_roundtrip() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
        })
        .expect("context init");

        let val = ctx.new_string("hello").expect("string");
        let utf8_pos = ctx.string_utf16_to_utf8_pos(val, 3);
        assert_eq!(utf8_pos, 6);
        let utf16_pos = ctx.string_utf8_to_utf16_pos(val, utf8_pos);
        assert_eq!(utf16_pos, 3);
    }

    #[test]
    fn string_len_and_getc_handles_surrogates() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
        })
        .expect("context init");

        let mut bytes = Vec::new();
        bytes.push(b'a');
        let mut tmp = [0u8; 4];
        let n = unicode_to_utf8(&mut tmp, 0x1f600);
        bytes.extend_from_slice(&tmp[..n]);
        bytes.push(b'b');

        let val = ctx.new_string_len(&bytes).expect("string");
        assert_eq!(ctx.string_len(val), 4);
        assert_eq!(ctx.string_getc(val, 0), b'a' as i32);
        assert_eq!(ctx.string_getc(val, 1), 0xd83d);
        assert_eq!(ctx.string_getc(val, 2), 0xde00);
        assert_eq!(ctx.string_getc(val, 3), b'b' as i32);
    }

    #[test]
    fn sub_string_utf8_splits_surrogate_pair() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
        })
        .expect("context init");

        let mut buf = [0u8; 4];
        let len = unicode_to_utf8(&mut buf, 0x1f600);
        let emoji = ctx.new_string_len(&buf[..len]).expect("emoji");

        let start = ctx.string_utf16_to_utf8_pos(emoji, 0);
        let mid = ctx.string_utf16_to_utf8_pos(emoji, 1);
        let end = ctx.string_utf16_to_utf8_pos(emoji, 2);

        let left = ctx.sub_string_utf8(emoji, start, mid).expect("left");
        let right = ctx.sub_string_utf8(emoji, mid, end).expect("right");
        assert!(is_ptr(left));
        assert!(is_ptr(right));

        let mut hi = [0u8; 4];
        let hi_len = unicode_to_utf8(&mut hi, 0xd83d);
        let mut lo = [0u8; 4];
        let lo_len = unicode_to_utf8(&mut lo, 0xde00);
        assert_eq!(bytes_from_val(left), hi[..hi_len]);
        assert_eq!(bytes_from_val(right), lo[..lo_len]);

        let full = ctx.sub_string(emoji, 0, 2).expect("full");
        assert_eq!(value_get_special_tag(full), JS_TAG_STRING_CHAR);
        assert_eq!(value_get_special_value(full), 0x1f600);
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
