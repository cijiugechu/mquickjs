use crate::array_data::ArrayData;
use crate::atom::AtomTables;
use crate::capi_defs::{JSInterruptHandler, JSWriteFunc};
use crate::cfunction_data::{CFunctionData, CFUNC_PARAMS_OFFSET};
use crate::containers::{
    byte_array_alloc_size, string_alloc_size, value_array_alloc_size, ByteArrayHeader, StringHeader,
    ValueArrayHeader, JS_BYTE_ARRAY_SIZE_MAX, JS_STRING_LEN_MAX,
};
use crate::cutils::{float64_as_uint64, unicode_to_utf8, utf8_get};
use crate::enums::JSObjectClass;
use crate::exception::{format_js_message, JsFormatArg};
use crate::gc::{GcMarkConfig};
use crate::gc_ref::{GcRef, GcRefState};
use crate::gc_runtime::{gc_collect, GcRuntimeRoots};
use crate::heap::{mblock_size, HeapLayout, JS_MIN_CRITICAL_FREE_SIZE, JS_MIN_FREE_SIZE, JS_STACK_SLACK};
use crate::jsvalue::{
    from_bits, new_short_int, raw_bits, value_from_ptr, value_get_int, value_get_special_tag,
    value_get_special_value, value_make_special, value_to_ptr, JSValue, JSWord, JSW, JS_NULL,
    JS_SHORTINT_MAX, JS_SHORTINT_MIN, JS_TAG_PTR, JS_TAG_SHORT_FUNC, JS_TAG_STRING_CHAR,
    JS_UNDEFINED,
};
use crate::function_bytecode::{FunctionBytecode, FunctionBytecodeFields, FunctionBytecodeHeader};
use crate::memblock::{Float64Header, MbHeader, MTag};
use crate::object::{Object, ObjectHeader};
use crate::parser::pc2line::find_line_col;
use crate::parser::parse_state::JSParseState;
use crate::property::{define_property_varref, PropertyError};
use crate::closure_data::ClosureData;
use crate::stdlib::cfunc::{build_c_function_table, CFunctionDef};
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

const BACKTRACE_MAX_LEN: usize = 127;
const ERROR_MESSAGE_MAX_LEN: usize = 127;
const BACKTRACE_MAX_LEVELS: usize = 10;
const JS_MAX_CALL_RECURSE: i32 = 8;

// Keep in sync with the interpreter frame layout.
const FRAME_OFFSET_FUNC_OBJ: isize = 3;
const FRAME_OFFSET_SAVED_FP: isize = 0;
const FRAME_OFFSET_CUR_PC: isize = -1;

pub(crate) struct BacktraceLocation<'a> {
    pub filename: &'a str,
    pub line: i32,
    pub column: i32,
}

struct HeapStorage {
    ptr: NonNull<[JSWord]>,
    owned: bool,
}

impl HeapStorage {
    fn new(words: usize) -> Self {
        let boxed = vec![0 as JSWord; words].into_boxed_slice();
        let ptr = NonNull::new(Box::into_raw(boxed)).expect("heap storage must be non-null");
        Self { ptr, owned: true }
    }

    unsafe fn from_raw(mem_start: NonNull<u8>, mem_size: usize) -> Result<Self, ContextError> {
        let word_bytes = JSW as usize;
        let addr = mem_start.as_ptr() as usize;
        if !addr.is_multiple_of(word_bytes) {
            return Err(ContextError::MemoryUnaligned {
                addr,
                align: word_bytes,
            });
        }
        let mem_size = mem_size & !(word_bytes - 1);
        if mem_size < MIN_CONTEXT_BYTES {
            return Err(ContextError::MemoryTooSmall {
                min: MIN_CONTEXT_BYTES,
                actual: mem_size,
            });
        }
        let words = mem_size / word_bytes;
        let data = mem_start.as_ptr().cast::<JSWord>();
        let slice = core::ptr::slice_from_raw_parts_mut(data, words);
        let ptr = NonNull::new(slice).expect("heap storage must be non-null");
        Ok(Self { ptr, owned: false })
    }

    fn base_ptr(&self) -> NonNull<u8> {
        let data = self.ptr.as_ptr().cast::<JSWord>();
        NonNull::new(data as *mut u8).expect("heap storage must be non-null")
    }
}

impl Drop for HeapStorage {
    fn drop(&mut self) {
        if self.owned {
            unsafe {
                // SAFETY: ptr was created from Box::into_raw in HeapStorage::new.
                drop(Box::from_raw(self.ptr.as_ptr()));
            }
        }
    }
}

/// C: JSContext initialization parameters (JS_NewContext2).
pub struct ContextConfig<'a> {
    pub image: &'a StdlibImage,
    pub memory_size: usize,
    pub prepare_compilation: bool,
    pub finalizers: &'a [Option<crate::capi_defs::JSCFinalizer>],
}

#[derive(Debug)]
pub enum ContextError {
    MemoryTooSmall { min: usize, actual: usize },
    MemoryUnaligned { addr: usize, align: usize },
    StdlibWordBytesMismatch { expected: usize, actual: usize },
    StdlibWordTooLarge(u64),
    ClassCountOverflow(u32),
    ClassIdOutOfBounds { class_id: u8, max: u16 },
    RomOffsetOutOfBounds { offset: usize, len: usize },
    InvalidValueArrayHeader { offset: usize, tag: MTag },
    InvalidRomString { offset: usize, tag: MTag },
    InvalidRomClassValue,
    InvalidRomClass { tag: MTag },
    StdlibCFunctionIndexOutOfBounds { index: usize },
    UnknownClassId { name: &'static str },
    InvalidCFunctionProto { name: &'static str, proto: &'static str },
    InvalidCFunctionMagic { name: &'static str, magic: &'static str },
    MissingEmptyAtom,
    ArrayTooLong { len: usize, max: u32 },
    StringTooLong { len: usize, max: JSWord },
    ByteArrayTooLong { len: usize, max: JSWord },
    OutOfMemory,
}

// Relocated ROM table built from StdlibImage words.
struct RomTable {
    words: NonNull<JSWord>,
    len: usize,
    word_bytes: usize,
    base_ptr: NonNull<u8>,
}

impl RomTable {
    fn from_image(image: &StdlibImage) -> Result<Self, ContextError> {
        let word_bytes = image.word_bytes as usize;
        let mut words = vec![0 as JSWord; image.words.len()].into_boxed_slice();
        let base_addr = words.as_mut_ptr() as usize;
        let tag_mask = (JSW as usize) - 1;
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
                    let addr = base_addr + offset as usize * word_bytes;
                    debug_assert!((addr & tag_mask) == 0);
                    (addr | JS_TAG_PTR as usize) as JSWord
                }
            };
            words[idx] = value;
        }
        let len = words.len();
        let words_ptr = Box::into_raw(words);
        let data_ptr = words_ptr as *mut JSWord;
        let words = NonNull::new(data_ptr).expect("ROM base must be non-null");
        let base_ptr = NonNull::new(data_ptr.cast::<u8>()).expect("ROM base must be non-null");
        Ok(Self {
            words,
            len,
            word_bytes,
            base_ptr,
        })
    }

    fn len(&self) -> usize {
        self.len
    }

    fn base_ptr(&self) -> *mut u8 {
        self.base_ptr.as_ptr()
    }

    fn bytes_len(&self) -> usize {
        self.len() * self.word_bytes
    }

    fn offset_from_addr(&self, addr: usize) -> Option<usize> {
        let base = self.base_ptr() as usize;
        if addr < base {
            return None;
        }
        let offset = addr - base;
        if offset >= self.bytes_len() {
            return None;
        }
        Some(offset)
    }

    fn word(&self, offset: usize) -> Result<JSWord, ContextError> {
        if offset >= self.len {
            return Err(ContextError::RomOffsetOutOfBounds {
                offset,
                len: self.len,
            });
        }
        Ok(unsafe {
            // SAFETY: offset is bounds-checked against the ROM table length.
            ptr::read_unaligned(self.words.as_ptr().add(offset))
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
        for idx in start..end {
            values.push(self.value_from_word(self.word(idx)?));
        }
        Ok(values)
    }

    fn value_from_word(&self, word: JSWord) -> JSValue {
        let tag_mask = (JSW as usize) - 1;
        let tag = (word as usize) & tag_mask;
        if tag == JS_TAG_PTR as usize {
            let addr = (word as usize) & !tag_mask;
            if let Some(offset) = self.offset_from_addr(addr) {
                let ptr = unsafe {
                    // SAFETY: offset is within the ROM table bounds.
                    self.base_ptr().add(offset)
                };
                if let Some(ptr) = NonNull::new(ptr) {
                    return value_from_ptr(ptr);
                }
            }
        }
        from_bits(word)
    }
}

impl Drop for RomTable {
    fn drop(&mut self) {
        unsafe {
            // SAFETY: words/len come from Box::into_raw in RomTable::from_image.
            let slice = slice::from_raw_parts_mut(self.words.as_ptr(), self.len);
            drop(Box::from_raw(slice));
        }
    }
}

// C: `JSROMClass` in mquickjs.c (ROM-only layout view).
//
// Invariants:
// - Backed by ROM table memory (never GC moved).
// - Header tag is `MTag::Object`.
// - `ctor_idx` is a signed 32-bit value (>=0 for constructors, -1 for plain objects).
struct RomClassView {
    base: NonNull<u8>,
    rom_start: usize,
    rom_end: usize,
}

impl RomClassView {
    const PROPS_OFFSET: usize = size_of::<JSWord>();
    const CTOR_OFFSET: usize = size_of::<JSWord>() * 2;
    const PROTO_PROPS_OFFSET: usize = size_of::<JSWord>() * 3;
    const PARENT_CLASS_OFFSET: usize = size_of::<JSWord>() * 4;

    fn from_value(rom_table: &RomTable, val: JSValue) -> Result<Self, ContextError> {
        if !val.is_ptr() {
            return Err(ContextError::InvalidRomClassValue);
        }
        let tagged = raw_bits(val) as usize;
        let addr = tagged & !((JSW as usize) - 1);
        let rom_start = rom_table.base_ptr() as usize;
        let rom_end = rom_start + rom_table.bytes_len();
        let offset = rom_table
            .offset_from_addr(addr)
            .ok_or(ContextError::InvalidRomClassValue)?;
        let needed = size_of::<JSWord>() * 5;
        if offset + needed > rom_table.bytes_len() {
            return Err(ContextError::InvalidRomClassValue);
        }
        let base = unsafe {
            // SAFETY: offset is within the ROM table bounds.
            rom_table.base_ptr().add(offset)
        };
        let base = NonNull::new(base).ok_or(ContextError::InvalidRomClassValue)?;
        let header_word = unsafe {
            // SAFETY: base points to readable ROM table memory.
            ptr::read_unaligned(base.as_ptr().cast::<JSWord>())
        };
        let header = MbHeader::from_word(header_word);
        if header.tag() != MTag::Object {
            return Err(ContextError::InvalidRomClass { tag: header.tag() });
        }
        Ok(Self {
            base,
            rom_start,
            rom_end,
        })
    }

    fn props(&self) -> JSValue {
        self.read_value(Self::PROPS_OFFSET)
    }

    fn ctor_idx(&self) -> i32 {
        self.read_word(Self::CTOR_OFFSET) as i32
    }

    fn proto_props(&self) -> JSValue {
        self.read_value(Self::PROTO_PROPS_OFFSET)
    }

    fn parent_class(&self) -> JSValue {
        self.read_value(Self::PARENT_CLASS_OFFSET)
    }

    fn read_word(&self, offset: usize) -> JSWord {
        unsafe {
            // SAFETY: offsets are within the fixed ROM class layout.
            ptr::read_unaligned(self.base.as_ptr().add(offset).cast::<JSWord>())
        }
    }

    fn read_value(&self, offset: usize) -> JSValue {
        self.value_from_word(self.read_word(offset))
    }

    fn value_from_word(&self, word: JSWord) -> JSValue {
        let tag_mask = (JSW as usize) - 1;
        let tag = (word as usize) & tag_mask;
        if tag == JS_TAG_PTR as usize {
            let addr = (word as usize) & !tag_mask;
            if addr >= self.rom_start && addr < self.rom_end {
                let ptr = self.base.as_ptr().with_addr(addr);
                if let Some(ptr) = NonNull::new(ptr) {
                    return value_from_ptr(ptr);
                }
            }
        }
        from_bits(word)
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
    c_function_table: Vec<CFunctionDef>,
    finalizers: Vec<Option<crate::capi_defs::JSCFinalizer>>,
    n_rom_atom_tables: u8,
    sorted_atoms_offset: usize,
    string_pos_cache: [StringPosCacheEntry; JS_STRING_POS_CACHE_SIZE],
    string_pos_cache_counter: u8,
    gc_refs: GcRefState,
    parse_state: Option<NonNull<JSParseState>>,
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
        let mem_size = config.memory_size & !(word_bytes - 1);
        let words = mem_size / word_bytes;
        let heap_storage = HeapStorage::new(words);
        Self::init_with_storage(heap_storage, mem_size, config)
    }

    /// # Safety
    /// `mem_start` must be valid for `mem_size` bytes and remain alive for
    /// the lifetime of the context.
    pub unsafe fn new_in_memory(
        mem_start: NonNull<u8>,
        mem_size: usize,
        config: ContextConfig<'_>,
    ) -> Result<Self, ContextError> {
        let word_bytes = JSW as usize;
        let mem_size = mem_size & !(word_bytes - 1);
        let heap_storage = unsafe { HeapStorage::from_raw(mem_start, mem_size)? };
        Self::init_with_storage(heap_storage, mem_size, config)
    }

    fn init_with_storage(
        heap_storage: HeapStorage,
        mem_size: usize,
        config: ContextConfig<'_>,
    ) -> Result<Self, ContextError> {
        let word_bytes = JSW as usize;
        if config.image.word_bytes as usize != word_bytes {
            return Err(ContextError::StdlibWordBytesMismatch {
                expected: word_bytes,
                actual: config.image.word_bytes as usize,
            });
        }
        let mem_size = mem_size & !(word_bytes - 1);
        if mem_size < MIN_CONTEXT_BYTES {
            return Err(ContextError::MemoryTooSmall {
                min: MIN_CONTEXT_BYTES,
                actual: mem_size,
            });
        }
        let class_count = u16::try_from(config.image.class_count)
            .map_err(|_| ContextError::ClassCountOverflow(config.image.class_count))?;

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
            c_function_table: Vec::new(),
            finalizers: config.finalizers.to_vec(),
            n_rom_atom_tables: 0,
            sorted_atoms_offset: config.image.sorted_atoms_offset as usize,
            string_pos_cache: [StringPosCacheEntry::new(JS_NULL, 0, 0); JS_STRING_POS_CACHE_SIZE],
            string_pos_cache_counter: 0,
            gc_refs: GcRefState::new(),
            parse_state: None,
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
        ctx.init_c_functions(config.image)?;
        ctx.init_roots()?;
        if !config.prepare_compilation {
            ctx.init_stdlib(config.image)?;
        }

        Ok(ctx)
    }

    pub fn heap(&self) -> &HeapLayout {
        &self.heap
    }

    pub fn heap_mut(&mut self) -> &mut HeapLayout {
        &mut self.heap
    }

    pub fn set_opaque(&mut self, opaque: *mut c_void) {
        self.opaque = opaque;
    }

    pub fn opaque(&self) -> *mut c_void {
        self.opaque
    }

    pub fn set_interrupt_handler(&mut self, handler: Option<JSInterruptHandler>) {
        self.interrupt_handler = handler;
    }

    pub fn set_log_func(&mut self, write_func: Option<JSWriteFunc>) {
        self.write_func = write_func.unwrap_or(dummy_write_func);
    }

    pub fn set_random_seed(&mut self, seed: u64) {
        self.random_state = seed;
    }

    pub fn push_gc_ref(&mut self, reference: &mut GcRef) -> *mut JSValue {
        self.gc_refs.push_gc_ref(reference)
    }

    pub fn pop_gc_ref(&mut self, reference: &GcRef) -> JSValue {
        self.gc_refs.pop_gc_ref(reference)
    }

    pub fn add_gc_ref(&mut self, reference: &mut GcRef) -> *mut JSValue {
        self.gc_refs.add_gc_ref(reference)
    }

    pub fn delete_gc_ref(&mut self, reference: &GcRef) {
        self.gc_refs.delete_gc_ref(reference);
    }

    pub fn write_log(&self, buf: &[u8]) {
        unsafe {
            // SAFETY: write_func follows the JSWriteFunc contract.
            (self.write_func)(self.opaque, buf.as_ptr().cast::<c_void>(), buf.len());
        }
    }

    pub fn gc(&mut self) {
        let config = GcMarkConfig {
            keep_atoms: true,
            ..GcMarkConfig::default()
        };
        let heap = &mut self.heap as *mut HeapLayout;
        let ctx_ptr = self as *mut JSContext;
        unsafe {
            // SAFETY: ctx_ptr/heap remain valid for the duration of the GC call.
            (*ctx_ptr).run_gc_with_config(&mut *heap, config);
        }
    }

    pub fn sp(&self) -> NonNull<JSValue> {
        self.sp
    }

    pub(crate) fn set_sp(&mut self, sp: NonNull<JSValue>) {
        self.sp = sp;
        self.heap.set_stack_bottom(sp);
    }

    pub fn stack_check(&mut self, len: u32) -> Result<(), ContextError> {
        let len = len as usize + JS_STACK_SLACK as usize;
        let sp = self.sp.as_ptr();
        let new_sp = unsafe { sp.sub(len) };
        let new_bottom = NonNull::new(new_sp).expect("stack bottom must be non-null");
        let size = len * size_of::<JSValue>();
        let ctx_ptr = self as *mut JSContext;
        let ok = self.heap.check_free_mem(new_bottom, size, |heap| unsafe {
            let ctx = &mut *ctx_ptr;
            if ctx.in_out_of_memory {
                return;
            }
            ctx.in_out_of_memory = true;
            ctx.run_gc_with_config(heap, GcMarkConfig {
                keep_atoms: true,
                ..GcMarkConfig::default()
            });
            ctx.in_out_of_memory = false;
        });
        if !ok {
            let _ = self.throw_out_of_memory();
            return Err(ContextError::OutOfMemory);
        }
        self.heap.set_stack_bottom(new_bottom);
        Ok(())
    }

    pub fn push_arg(&mut self, val: JSValue) {
        let new_sp = unsafe { self.sp.as_ptr().sub(1) };
        unsafe {
            // SAFETY: caller ensures the stack has room via stack_check.
            ptr::write_unaligned(new_sp, val);
        }
        let new_sp = NonNull::new(new_sp).expect("stack pointer");
        self.sp = new_sp;
        if new_sp.as_ptr() < self.heap.stack_bottom().as_ptr() {
            self.heap.set_stack_bottom(new_sp);
        }
    }

    pub(crate) fn enter_call(&mut self) -> bool {
        if self.js_call_rec_count >= JS_MAX_CALL_RECURSE {
            return false;
        }
        self.js_call_rec_count += 1;
        true
    }

    pub(crate) fn exit_call(&mut self) {
        self.js_call_rec_count = self.js_call_rec_count.saturating_sub(1);
    }

    pub fn fp(&self) -> NonNull<JSValue> {
        self.fp
    }

    pub(crate) fn set_fp(&mut self, fp: NonNull<JSValue>) {
        self.fp = fp;
    }

    pub fn stack_top(&self) -> NonNull<u8> {
        self.heap.stack_top()
    }

    pub fn stack_bottom(&self) -> NonNull<JSValue> {
        self.heap.stack_bottom()
    }

    pub(crate) fn heap_free_ptr(&self) -> NonNull<u8> {
        self.heap.heap_free()
    }

    pub(crate) fn swap_parse_state(
        &mut self,
        state: Option<NonNull<JSParseState>>,
    ) -> Option<NonNull<JSParseState>> {
        core::mem::replace(&mut self.parse_state, state)
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

    pub fn next_random_u64(&mut self) -> u64 {
        let mut x = self.random_state;
        x ^= x >> 12;
        x ^= x << 25;
        x ^= x >> 27;
        self.random_state = x;
        x.wrapping_mul(0x2545_f491_4f6c_dd1d)
    }

    pub fn c_function_table(&self) -> &[CFunctionDef] {
        &self.c_function_table
    }

    pub fn c_function(&self, index: usize) -> Option<&CFunctionDef> {
        self.c_function_table.get(index)
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

    pub(crate) fn add_rom_atom_table(&mut self, table: Vec<JSValue>) {
        self.atom_tables.add_rom_table(table);
        self.n_rom_atom_tables = self.n_rom_atom_tables.saturating_add(1);
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

    pub(crate) fn set_current_exception(&mut self, value: JSValue) {
        self.class_roots[ROOT_CURRENT_EXCEPTION] = value;
    }

    pub(crate) fn current_exception(&self) -> JSValue {
        self.class_roots[ROOT_CURRENT_EXCEPTION]
    }

    pub(crate) fn take_current_exception(&mut self) -> JSValue {
        let value = self.class_roots[ROOT_CURRENT_EXCEPTION];
        self.class_roots[ROOT_CURRENT_EXCEPTION] = JS_NULL;
        value
    }

    pub(crate) fn current_exception_is_uncatchable(&self) -> bool {
        self.current_exception_is_uncatchable
    }

    pub(crate) fn throw(&mut self, obj: JSValue) -> JSValue {
        self.set_current_exception(obj);
        self.current_exception_is_uncatchable = false;
        crate::jsvalue::JS_EXCEPTION
    }

    pub(crate) fn throw_error(&mut self, class: JSObjectClass, message: &str) -> JSValue {
        let msg_val = match self.new_string(message) {
            Ok(val) => val,
            Err(_) => return self.throw_out_of_memory(),
        };
        self.throw_error_with_message(class, msg_val)
    }

    pub(crate) fn throw_error_fmt(
        &mut self,
        class: JSObjectClass,
        fmt: &str,
        args: &[JsFormatArg<'_>],
    ) -> JSValue {
        let mut msg_bytes = format_js_message(fmt, args, ERROR_MESSAGE_MAX_LEN);
        if let Some(pos) = msg_bytes.iter().position(|b| *b == 0) {
            msg_bytes.truncate(pos);
        }
        let msg_val = match self.new_string_len(&msg_bytes) {
            Ok(val) => val,
            Err(_) => return self.throw_out_of_memory(),
        };
        self.throw_error_with_message(class, msg_val)
    }

    fn throw_error_with_message(&mut self, class: JSObjectClass, msg_val: JSValue) -> JSValue {
        let mut msg_ref = GcRef::new(JS_UNDEFINED);
        let msg_slot = self.gc_refs.push_gc_ref(&mut msg_ref);
        unsafe {
            // SAFETY: msg_slot points to a GC root slot for msg_ref.
            *msg_slot = msg_val;
        }
        let proto = self.class_proto()[class as usize];
        let error_obj = match self.alloc_error(proto) {
            Ok(obj) => obj,
            Err(_) => {
                let _ = self.gc_refs.pop_gc_ref(&msg_ref);
                return self.throw_out_of_memory();
            }
        };
        let _ = self.gc_refs.pop_gc_ref(&msg_ref);
        if self.set_error_message(error_obj, msg_val).is_err() {
            return self.throw_out_of_memory();
        }
        if class != JSObjectClass::SyntaxError {
            let _ = self.build_backtrace(error_obj, None, 0);
        }
        self.throw(error_obj)
    }

    pub(crate) fn throw_type_error(&mut self, message: &str) -> JSValue {
        self.throw_error(JSObjectClass::TypeError, message)
    }

    pub(crate) fn throw_type_error_fmt(&mut self, fmt: &str, args: &[JsFormatArg<'_>]) -> JSValue {
        self.throw_error_fmt(JSObjectClass::TypeError, fmt, args)
    }

    #[allow(dead_code)]
    pub(crate) fn throw_reference_error(&mut self, message: &str) -> JSValue {
        self.throw_error(JSObjectClass::ReferenceError, message)
    }

    #[allow(dead_code)]
    pub(crate) fn throw_reference_error_fmt(
        &mut self,
        fmt: &str,
        args: &[JsFormatArg<'_>],
    ) -> JSValue {
        self.throw_error_fmt(JSObjectClass::ReferenceError, fmt, args)
    }

    #[allow(dead_code)]
    pub(crate) fn throw_range_error(&mut self, message: &str) -> JSValue {
        self.throw_error(JSObjectClass::RangeError, message)
    }

    #[allow(dead_code)]
    pub(crate) fn throw_range_error_fmt(&mut self, fmt: &str, args: &[JsFormatArg<'_>]) -> JSValue {
        self.throw_error_fmt(JSObjectClass::RangeError, fmt, args)
    }

    pub(crate) fn throw_syntax_error(&mut self, message: &str) -> JSValue {
        self.throw_error(JSObjectClass::SyntaxError, message)
    }

    pub(crate) fn throw_syntax_error_fmt(
        &mut self,
        fmt: &str,
        args: &[JsFormatArg<'_>],
    ) -> JSValue {
        self.throw_error_fmt(JSObjectClass::SyntaxError, fmt, args)
    }

    pub(crate) fn throw_internal_error(&mut self, message: &str) -> JSValue {
        self.throw_error(JSObjectClass::InternalError, message)
    }

    pub(crate) fn throw_internal_error_fmt(
        &mut self,
        fmt: &str,
        args: &[JsFormatArg<'_>],
    ) -> JSValue {
        self.throw_error_fmt(JSObjectClass::InternalError, fmt, args)
    }

    pub(crate) fn throw_out_of_memory(&mut self) -> JSValue {
        if self.in_out_of_memory {
            return self.throw(JS_NULL);
        }
        self.in_out_of_memory = true;
        self.heap.set_min_free_size(JS_MIN_CRITICAL_FREE_SIZE as usize);
        let val = self.throw_internal_error("out of memory");
        self.in_out_of_memory = false;
        self.heap.set_min_free_size(JS_MIN_FREE_SIZE as usize);
        val
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

    pub fn new_int64(&mut self, value: i64) -> Result<JSValue, ContextError> {
        if value >= JS_SHORTINT_MIN as i64 && value <= JS_SHORTINT_MAX as i64 {
            return Ok(new_short_int(value as i32));
        }
        self.new_float64(value as f64)
    }

    pub fn new_int32(&mut self, value: i32) -> Result<JSValue, ContextError> {
        self.new_int64(i64::from(value))
    }

    pub fn new_uint32(&mut self, value: u32) -> Result<JSValue, ContextError> {
        self.new_int64(i64::from(value))
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

    pub(crate) fn rom_value_from_word(&self, word: JSWord) -> JSValue {
        if let Some(table) = self.rom_table.as_ref() {
            table.value_from_word(word)
        } else {
            from_bits(word)
        }
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

    /// Creates a string from a single codepoint.
    pub fn new_string_char(&mut self, codepoint: u32) -> Result<JSValue, ContextError> {
        if codepoint <= 0x7f {
            let buf = [codepoint as u8];
            return self.new_string_len(&buf);
        }
        let mut buf = [0u8; 4];
        let len = crate::cutils::unicode_to_utf8(&mut buf, codepoint);
        self.new_string_len(&buf[..len])
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

    fn init_c_functions(&mut self, image: &StdlibImage) -> Result<(), ContextError> {
        self.c_function_table = build_c_function_table(self, image)?;
        Ok(())
    }

    fn decode_rom_atom(&mut self, rom_table: &RomTable, val: JSValue) -> Result<JSValue, ContextError> {
        if !val.is_ptr() {
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

    fn init_stdlib(&mut self, image: &StdlibImage) -> Result<(), ContextError> {
        let Some(rom_table) = self.rom_table.as_ref() else {
            return Ok(());
        };
        let (rom_base, rom_base_ptr, rom_len, rom_word_bytes, entries) = {
            let rom_base_ptr = rom_table.base_ptr();
            let rom_base = rom_base_ptr as usize;
            let rom_len = rom_table.bytes_len();
            let rom_word_bytes = rom_table.word_bytes;
            let entries = rom_table.value_array(image.global_object_offset as usize)?;
            (rom_base, rom_base_ptr, rom_len, rom_word_bytes, entries)
        };
        debug_assert_eq!(entries.len() % 2, 0);
        let global_obj = self.global_obj();
        for pair in entries.chunks_exact(2) {
            let name = self.resolve_rom_atom_value(pair[0])?;
            let mut val = pair[1];
            if val.is_ptr() {
                let tagged = raw_bits(val) as usize;
                let addr = tagged & !((JSW as usize) - 1);
                if addr >= rom_base && addr < rom_base + rom_len {
                    let offset = addr - rom_base;
                    if offset.is_multiple_of(rom_word_bytes) {
                        let header_ptr = unsafe {
                            // SAFETY: offset is within the ROM table bounds.
                            rom_base_ptr.add(offset)
                        };
                        let header_word = unsafe {
                            // SAFETY: header_ptr points within the ROM table.
                            ptr::read_unaligned(header_ptr.cast::<JSWord>())
                        };
                        if MbHeader::from_word(header_word).tag() == MTag::Object {
                            val = self.stdlib_init_class(image, val)?;
                        }
                    }
                }
            }
            if val == JS_NULL {
                val = global_obj;
            }
            define_property_varref(self, global_obj, name, val).map_err(map_property_error)?;
        }
        Ok(())
    }

    fn stdlib_init_class(&mut self, image: &StdlibImage, val: JSValue) -> Result<JSValue, ContextError> {
        let rom_table = self
            .rom_table
            .as_ref()
            .map(|table| table as *const RomTable)
            .ok_or(ContextError::InvalidRomClassValue)?;
        let class_def = unsafe {
            // SAFETY: rom_table points to the context ROM table allocation.
            RomClassView::from_value(&*rom_table, val)?
        };
        let ctor_idx = class_def.ctor_idx();
        if ctor_idx >= 0 {
            let ctor_idx = ctor_idx as usize;
            let class_id = class_id_from_ctor(image, ctor_idx)?;
            let class_idx = class_id as usize;
            let existing = self.class_obj()[class_idx];
            if existing != JS_NULL {
                return Ok(existing);
            }

            let parent_val = class_def.parent_class();
            let (mut parent_class, parent_proto) = if parent_val != JS_NULL {
                let parent_class_id = unsafe {
                    // SAFETY: rom_table points to the context ROM table allocation.
                    let parent_def = RomClassView::from_value(&*rom_table, parent_val)?;
                    let parent_ctor_idx = parent_def.ctor_idx();
                    if parent_ctor_idx < 0 {
                        return Err(ContextError::InvalidRomClassValue);
                    }
                    class_id_from_ctor(image, parent_ctor_idx as usize)?
                };
                let parent_obj = self.stdlib_init_class(image, parent_val)?;
                let parent_proto = self.class_proto()[parent_class_id as usize];
                (parent_obj, parent_proto)
            } else {
                (JS_NULL, self.class_proto()[JSObjectClass::Object as usize])
            };

            let proto = match self.class_proto()[class_idx] {
                JS_NULL => {
                    let created = self.alloc_object(JSObjectClass::Object, parent_proto, 0)?;
                    self.class_proto_mut()[class_idx] = created;
                    created
                }
                existing => existing,
            };
            let proto_props = class_def.proto_props();
            if proto_props != JS_NULL {
                self.set_object_props(proto, proto_props)?;
            }

            if parent_class == JS_NULL {
                parent_class = self.class_proto()[JSObjectClass::Closure as usize];
            }
            let ctor = self.new_cfunction_proto(ctor_idx as u32, parent_class, None)?;
            self.class_obj_mut()[class_idx] = ctor;
            let class_props = class_def.props();
            if class_props != JS_NULL {
                self.set_object_props(ctor, class_props)?;
            }
            Ok(ctor)
        } else {
            let proto = self.class_proto()[JSObjectClass::Object as usize];
            let obj = self.alloc_object(JSObjectClass::Object, proto, 0)?;
            let class_props = class_def.props();
            if class_props != JS_NULL {
                self.set_object_props(obj, class_props)?;
            }
            Ok(obj)
        }
    }

    pub(crate) fn resolve_rom_atom_value(&mut self, val: JSValue) -> Result<JSValue, ContextError> {
        if !val.is_ptr() {
            return Ok(val);
        }
        let tagged = raw_bits(val) as usize;
        let addr = tagged & !((JSW as usize) - 1);
        let heap_base = self.heap.heap_base().as_ptr() as usize;
        let heap_end = self.stack_top().as_ptr() as usize;
        if addr >= heap_base && addr < heap_end {
            return Ok(val);
        }
        let Some(rom_table) = self.rom_table.as_ref().map(|table| table as *const RomTable) else {
            return Ok(val);
        };
        let target = raw_bits(val);
        let raw_atoms = unsafe {
            // SAFETY: rom_table points to the context ROM table allocation.
            (&*rom_table).value_array(self.sorted_atoms_offset)?
        };
        if let Some(index) = raw_atoms
            .iter()
            .position(|entry| raw_bits(*entry) == target)
            && let Some(mapped) = self.atom_tables().rom_table_entry(index)
        {
            return Ok(mapped);
        }
        let rom_table = unsafe {
            // SAFETY: rom_table points to the context ROM table allocation.
            &*rom_table
        };
        self.decode_rom_atom(rom_table, val)
    }

    fn new_cfunction_proto(
        &mut self,
        idx: u32,
        proto: JSValue,
        params: Option<JSValue>,
    ) -> Result<JSValue, ContextError> {
        let extra_size_bytes = if params.is_some() {
            size_of::<CFunctionData>()
        } else {
            size_of::<u32>()
        };
        let func = self.alloc_object(JSObjectClass::CFunction, proto, extra_size_bytes)?;
        let obj_ptr = value_to_ptr::<Object>(func).expect("cfunction allocation must be a pointer");
        unsafe {
            // SAFETY: payload region is writable for the requested size.
            let payload = Object::payload_ptr(obj_ptr.as_ptr()).cast::<u8>();
            ptr::write_unaligned(payload.cast::<u32>(), idx);
            if let Some(params) = params {
                let params_ptr = payload.add(CFUNC_PARAMS_OFFSET);
                ptr::write_unaligned(params_ptr.cast::<JSValue>(), params);
            }
        }
        Ok(func)
    }

    /// Creates a new CFunction object with parameters (for bound functions, etc.).
    pub fn new_cfunction_params(
        &mut self,
        idx: u32,
        params: JSValue,
    ) -> Result<JSValue, ContextError> {
        let proto = self.class_proto()[JSObjectClass::Closure as usize];
        self.new_cfunction_proto(idx, proto, Some(params))
    }

    fn set_object_props(&mut self, obj: JSValue, props: JSValue) -> Result<(), ContextError> {
        let obj_ptr = value_to_ptr::<u8>(obj).ok_or(ContextError::InvalidRomClassValue)?;
        let header_word = unsafe {
            // SAFETY: pointer is expected to reference an object header word.
            ptr::read_unaligned(obj_ptr.as_ptr().cast::<JSWord>())
        };
        let header = ObjectHeader::from_word(header_word);
        if header.tag() != MTag::Object {
            return Err(ContextError::InvalidRomClass { tag: header.tag() });
        }
        unsafe {
            // SAFETY: `obj_ptr` references a writable object allocation.
            ptr::write_unaligned(Object::props_ptr(obj_ptr.as_ptr().cast::<Object>()), props);
        }
        Ok(())
    }

    fn run_gc(&mut self, heap: &mut HeapLayout) {
        self.run_gc_with_config(heap, GcMarkConfig::default());
    }

    fn run_gc_with_config(&mut self, heap: &mut HeapLayout, mark_config: GcMarkConfig) {
        let parse_state = self.parse_state;
        let gc_refs = &self.gc_refs as *const GcRefState;
        let sp = self.sp.as_ptr();
        let stack_top = heap.stack_top().as_ptr() as *mut JSValue;
        debug_assert!(sp <= stack_top);
        let len = (stack_top as usize)
            .saturating_sub(sp as usize)
            / size_of::<JSValue>();
        let stack_roots = unsafe {
            // SAFETY: sp..stack_top is the active JS stack range.
            slice::from_raw_parts_mut(sp, len)
        };
        let (class_roots, atom_tables, string_pos_cache) = (
            &mut self.class_roots,
            &mut self.atom_tables,
            &mut self.string_pos_cache,
        );
        let mut roots = GcRuntimeRoots::new(class_roots, stack_roots)
            .with_gc_refs(unsafe {
                // SAFETY: gc_refs is a stable reference for the duration of this call.
                &*gc_refs
            })
            .with_atom_tables(atom_tables)
            .with_string_pos_cache(string_pos_cache);
        if let Some(state) = parse_state {
            roots = roots.with_parse_state(unsafe {
                // SAFETY: parse_state is registered by the parser and valid while set.
                state.as_ref()
            });
        }
        unsafe {
            // SAFETY: heap layout and roots are valid for the duration of collection.
            gc_collect(heap, &mut roots, mark_config, None);
        }
    }

    pub(crate) fn alloc_mblock(&mut self, size: usize, tag: MTag) -> Result<NonNull<u8>, ContextError> {
        let ctx_ptr = self as *mut JSContext;
        let ptr = self.heap.malloc(size, tag, |heap| unsafe {
            let ctx = &mut *ctx_ptr;
            if ctx.in_out_of_memory {
                return;
            }
            ctx.in_out_of_memory = true;
            ctx.run_gc(heap);
            ctx.in_out_of_memory = false;
        });
        ptr.ok_or(ContextError::OutOfMemory)
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
        let ptr = self.alloc_mblock(alloc_size, MTag::ValueArray)?;
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

    pub(crate) fn alloc_byte_array(&mut self, bytes: &[u8]) -> Result<JSValue, ContextError> {
        let len = JSWord::try_from(bytes.len()).map_err(|_| ContextError::ByteArrayTooLong {
            len: bytes.len(),
            max: JS_BYTE_ARRAY_SIZE_MAX,
        })?;
        if len > JS_BYTE_ARRAY_SIZE_MAX {
            return Err(ContextError::ByteArrayTooLong {
                len: bytes.len(),
                max: JS_BYTE_ARRAY_SIZE_MAX,
            });
        }
        let size = byte_array_alloc_size(len);
        let ptr = self.alloc_mblock(size, MTag::ByteArray)?;
        let header = ByteArrayHeader::new(len, false);
        unsafe {
            // SAFETY: `ptr` points to writable byte array storage.
            ptr::write_unaligned(ptr.as_ptr().cast::<JSWord>(), MbHeader::from(header).word());
            let payload = ptr.as_ptr().add(size_of::<JSWord>());
            ptr::write_bytes(payload, 0, len as usize);
            ptr::copy_nonoverlapping(bytes.as_ptr(), payload, bytes.len());
        }
        Ok(value_from_ptr(ptr))
    }

    pub(crate) fn alloc_object(
        &mut self,
        class_id: JSObjectClass,
        proto: JSValue,
        extra_size_bytes: usize,
    ) -> Result<JSValue, ContextError> {
        let extra_words = extra_size_bytes.div_ceil(JSW as usize);
        let size = Object::PAYLOAD_OFFSET + extra_words * JSW as usize;
        let ptr = self.alloc_mblock(size, MTag::Object)?;
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

    pub fn alloc_object_class_id(
        &mut self,
        class_id: u8,
        proto: JSValue,
        extra_size_bytes: usize,
    ) -> Result<JSValue, ContextError> {
        if class_id as usize >= self.class_count as usize {
            return Err(ContextError::ClassIdOutOfBounds {
                class_id,
                max: self.class_count,
            });
        }
        let extra_words = extra_size_bytes.div_ceil(JSW as usize);
        let size = Object::PAYLOAD_OFFSET + extra_words * JSW as usize;
        let ptr = self.alloc_mblock(size, MTag::Object)?;
        let header = ObjectHeader::new(class_id, extra_words as JSWord, false);
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

    /// Allocates a new plain Object with default prototype.
    pub fn alloc_object_default(&mut self) -> Result<JSValue, ContextError> {
        let proto = self.class_proto()[JSObjectClass::Object as usize];
        self.alloc_object(JSObjectClass::Object, proto, 0)
    }

    /// Allocates a new Object with a specific prototype.
    pub fn alloc_object_with_proto(&mut self, proto: JSValue) -> Result<JSValue, ContextError> {
        self.alloc_object(JSObjectClass::Object, proto, 0)
    }

    pub fn alloc_array(&mut self, len: usize) -> Result<JSValue, ContextError> {
        let len_u32 = u32::try_from(len).map_err(|_| ContextError::ArrayTooLong {
            len,
            max: ArrayData::LEN_MAX,
        })?;
        if len_u32 > ArrayData::LEN_MAX {
            return Err(ContextError::ArrayTooLong {
                len,
                max: ArrayData::LEN_MAX,
            });
        }
        let tab = if len == 0 {
            JS_NULL
        } else {
            let ptr = self.alloc_value_array(len)?;
            value_from_ptr(ptr)
        };
        let proto = self.class_proto()[JSObjectClass::Array as usize];
        let array = self.alloc_object(JSObjectClass::Array, proto, size_of::<ArrayData>())?;
        let obj_ptr = value_to_ptr::<Object>(array).expect("array allocation must be a pointer");
        unsafe {
            // SAFETY: array points at a writable object payload.
            let payload = Object::payload_ptr(obj_ptr.as_ptr());
            let array_ptr = core::ptr::addr_of_mut!((*payload).array);
            ptr::write_unaligned(array_ptr, ArrayData::new(tab, len_u32));
        }
        Ok(array)
    }

    /// Resizes an array to a new length, allocating more space if needed.
    pub fn array_resize(&mut self, arr: JSValue, new_len: usize) -> Result<(), ContextError> {
        let obj_ptr = value_to_ptr::<Object>(arr).ok_or(ContextError::InvalidRomClassValue)?;
        let header_word = unsafe { ptr::read_unaligned(obj_ptr.as_ptr().cast::<JSWord>()) };
        let header = ObjectHeader::from_word(header_word);
        if header.tag() != MTag::Object || header.class_id() != JSObjectClass::Array as u8 {
            return Err(ContextError::InvalidRomClassValue);
        }

        let data = unsafe {
            let payload = Object::payload_ptr(obj_ptr.as_ptr());
            ptr::read_unaligned(core::ptr::addr_of!((*payload).array))
        };
        let old_len = data.len() as usize;
        let old_tab = data.tab();

        if new_len > ArrayData::LEN_MAX as usize {
            return Err(ContextError::ArrayTooLong {
                len: new_len,
                max: ArrayData::LEN_MAX,
            });
        }

        let new_tab = if new_len == 0 {
            JS_NULL
        } else if old_tab == JS_NULL {
            let ptr = self.alloc_value_array(new_len)?;
            let arr = unsafe { ptr.as_ptr().add(size_of::<JSWord>()) as *mut JSValue };
            unsafe {
                for i in 0..new_len {
                    ptr::write_unaligned(arr.add(i), JS_UNDEFINED);
                }
            }
            value_from_ptr(ptr)
        } else {
            // Need to resize existing array
            let old_ptr = value_to_ptr::<u8>(old_tab).ok_or(ContextError::OutOfMemory)?;
            let new_ptr = self.alloc_value_array(new_len)?;
            let old_arr = unsafe { old_ptr.as_ptr().add(size_of::<JSWord>()) as *const JSValue };
            let new_arr = unsafe { new_ptr.as_ptr().add(size_of::<JSWord>()) as *mut JSValue };
            unsafe {
                // Copy old elements
                let copy_len = old_len.min(new_len);
                for i in 0..copy_len {
                    ptr::write_unaligned(new_arr.add(i), ptr::read_unaligned(old_arr.add(i)));
                }
                // Initialize new elements to undefined
                for i in copy_len..new_len {
                    ptr::write_unaligned(new_arr.add(i), JS_UNDEFINED);
                }
            }
            value_from_ptr(new_ptr)
        };

        // Update the array data
        unsafe {
            let payload = Object::payload_ptr(obj_ptr.as_ptr());
            let array_ptr = core::ptr::addr_of_mut!((*payload).array);
            ptr::write_unaligned(array_ptr, ArrayData::new(new_tab, new_len as u32));
        }
        Ok(())
    }

    /// Sets the length of an array.
    pub fn array_set_length(&mut self, arr: JSValue, new_len: u32) -> Result<(), ContextError> {
        let obj_ptr = value_to_ptr::<Object>(arr).ok_or(ContextError::InvalidRomClassValue)?;
        let header_word = unsafe { ptr::read_unaligned(obj_ptr.as_ptr().cast::<JSWord>()) };
        let header = ObjectHeader::from_word(header_word);
        if header.tag() != MTag::Object || header.class_id() != JSObjectClass::Array as u8 {
            return Err(ContextError::InvalidRomClassValue);
        }

        let data = unsafe {
            let payload = Object::payload_ptr(obj_ptr.as_ptr());
            ptr::read_unaligned(core::ptr::addr_of!((*payload).array))
        };
        let old_len = data.len();

        if new_len == old_len {
            return Ok(());
        }

        // Just update the length - the tab will be handled separately if needed
        unsafe {
            let payload = Object::payload_ptr(obj_ptr.as_ptr());
            let array_ptr = core::ptr::addr_of_mut!((*payload).array);
            ptr::write_unaligned(array_ptr, ArrayData::new(data.tab(), new_len));
        }
        Ok(())
    }

    /// Allocates a new Error object.
    pub fn alloc_error(&mut self, proto: JSValue) -> Result<JSValue, ContextError> {
        use crate::error_data::ErrorData;
        let error = self.alloc_object(JSObjectClass::Error, proto, size_of::<ErrorData>())?;
        let obj_ptr = value_to_ptr::<Object>(error).expect("error allocation must be a pointer");
        unsafe {
            // SAFETY: error points at a writable error object allocation.
            let payload = Object::payload_ptr(obj_ptr.as_ptr());
            let error_ptr = core::ptr::addr_of_mut!((*payload).error);
            ptr::write_unaligned(error_ptr, ErrorData::new(JS_NULL, JS_NULL));
        }
        Ok(error)
    }

    /// Sets the message on an Error object.
    pub fn set_error_message(&mut self, error: JSValue, message: JSValue) -> Result<(), ContextError> {
        let obj_ptr = value_to_ptr::<Object>(error).ok_or(ContextError::InvalidRomClassValue)?;
        unsafe {
            let payload = Object::payload_ptr(obj_ptr.as_ptr());
            let error_ptr = core::ptr::addr_of_mut!((*payload).error);
            let msg_ptr = crate::error_data::ErrorData::message_ptr(error_ptr);
            ptr::write_unaligned(msg_ptr, message);
        }
        Ok(())
    }

    /// Sets the stack on an Error object.
    pub fn set_error_stack(&mut self, error: JSValue, stack: JSValue) -> Result<(), ContextError> {
        let obj_ptr = value_to_ptr::<Object>(error).ok_or(ContextError::InvalidRomClassValue)?;
        unsafe {
            let payload = Object::payload_ptr(obj_ptr.as_ptr());
            let error_ptr = core::ptr::addr_of_mut!((*payload).error);
            let stack_ptr = crate::error_data::ErrorData::stack_ptr(error_ptr);
            ptr::write_unaligned(stack_ptr, stack);
        }
        Ok(())
    }

    /// Gets the message from an Error object.
    pub fn get_error_message(&self, error: JSValue) -> Result<JSValue, ContextError> {
        let obj_ptr = value_to_ptr::<Object>(error).ok_or(ContextError::InvalidRomClassValue)?;
        let data = unsafe {
            let payload = Object::payload_ptr(obj_ptr.as_ptr());
            ptr::read_unaligned(core::ptr::addr_of!((*payload).error))
        };
        Ok(data.message())
    }

    /// Gets the stack from an Error object.
    pub fn get_error_stack(&self, error: JSValue) -> Result<JSValue, ContextError> {
        let obj_ptr = value_to_ptr::<Object>(error).ok_or(ContextError::InvalidRomClassValue)?;
        let data = unsafe {
            let payload = Object::payload_ptr(obj_ptr.as_ptr());
            ptr::read_unaligned(core::ptr::addr_of!((*payload).error))
        };
        Ok(data.stack())
    }

    pub(crate) fn build_backtrace(
        &mut self,
        error_obj: JSValue,
        location: Option<BacktraceLocation<'_>>,
        mut skip_level: usize,
    ) -> Result<(), ContextError> {
        if !self.is_error_object(error_obj) {
            return Ok(());
        }

        let mut buf = String::new();
        if let Some(loc) = location {
            let line = format!("    at {}:{}:{}\n", loc.filename, loc.line, loc.column);
            if !push_backtrace_line(&mut buf, &line) {
                return self.finish_backtrace(error_obj, &buf);
            }
        }

        let stack_top = self.stack_top().as_ptr() as *mut JSValue;
        let mut fp = self.fp.as_ptr();
        let mut level = 0usize;

        while fp != stack_top && level < BACKTRACE_MAX_LEVELS {
            if skip_level != 0 {
                skip_level -= 1;
            } else {
                let func_obj = unsafe { ptr::read_unaligned(fp.offset(FRAME_OFFSET_FUNC_OBJ)) };
                let (func_name, bytecode) = self.backtrace_func_info(func_obj);
                let mut line = String::new();
                if let Some(bytecode) = bytecode {
                    let filename = self
                        .string_from_value(bytecode.filename())
                        .unwrap_or_else(|| "<unknown>".to_string());
                    let pc_val = unsafe { ptr::read_unaligned(fp.offset(FRAME_OFFSET_CUR_PC)) };
                    let pc = if pc_val.is_int() {
                        let pc = value_get_int(pc_val);
                        pc.saturating_sub(1) as u32
                    } else {
                        0
                    };
                    let byte_code = byte_array_view(bytecode.byte_code())
                        .map(|view| unsafe { view.as_slice() });
                    let pc2line = byte_array_view(bytecode.pc2line())
                        .map(|view| unsafe { view.as_slice() });
                    let (line_num, col_num) = match byte_code {
                        Some(code) => find_line_col(code, pc2line, bytecode.has_column(), pc),
                        None => (0, 0),
                    };
                    line.push_str("    at ");
                    line.push_str(&func_name);
                    line.push_str(" (");
                    line.push_str(&filename);
                    if line_num != 0 {
                        line.push(':');
                        line.push_str(&line_num.to_string());
                        if col_num != 0 {
                            line.push(':');
                            line.push_str(&col_num.to_string());
                        }
                    }
                    line.push_str(")\n");
                } else {
                    line = format!("    at {} (native)\n", func_name);
                }
                if !push_backtrace_line(&mut buf, &line) {
                    break;
                }
                level += 1;
            }

            let saved_fp_val = unsafe { ptr::read_unaligned(fp.offset(FRAME_OFFSET_SAVED_FP)) };
            let Some(next_fp) = decode_stack_ptr(stack_top, saved_fp_val) else {
                break;
            };
            fp = next_fp;
        }

        self.finish_backtrace(error_obj, &buf)
    }

    fn finish_backtrace(&mut self, error_obj: JSValue, buf: &str) -> Result<(), ContextError> {
        let mut error_ref = GcRef::new(JS_UNDEFINED);
        let slot = self.gc_refs.push_gc_ref(&mut error_ref);
        unsafe {
            // SAFETY: slot points to a GC root slot for error_ref.
            *slot = error_obj;
        }
        let stack_val = self.new_string_len(buf.as_bytes()).unwrap_or(JS_NULL);
        let _ = self.gc_refs.pop_gc_ref(&error_ref);
        self.set_error_stack(error_obj, stack_val)
    }

    fn is_error_object(&self, val: JSValue) -> bool {
        if !val.is_ptr() {
            return false;
        }
        let Some(obj_ptr) = value_to_ptr::<Object>(val) else {
            return false;
        };
        let header_word = unsafe { ptr::read_unaligned(obj_ptr.as_ptr().cast::<JSWord>()) };
        let header = ObjectHeader::from_word(header_word);
        header.tag() == MTag::Object && header.class_id() == JSObjectClass::Error as u8
    }

    fn backtrace_func_info(&self, func_obj: JSValue) -> (String, Option<FunctionBytecode>) {
        if !func_obj.is_ptr() {
            if value_get_special_tag(func_obj) == JS_TAG_SHORT_FUNC {
                let idx = value_get_special_value(func_obj);
                if idx >= 0 && let Some(def) = self.c_function(idx as usize) {
                    return (def.name_str.to_string(), None);
                }
            }
            return ("<anonymous>".to_string(), None);
        }

        let Some(obj_ptr) = value_to_ptr::<Object>(func_obj) else {
            return ("<anonymous>".to_string(), None);
        };
        let header_word = unsafe { ptr::read_unaligned(obj_ptr.as_ptr().cast::<JSWord>()) };
        let header = ObjectHeader::from_word(header_word);
        if header.tag() != MTag::Object {
            return ("<anonymous>".to_string(), None);
        }

        match header.class_id() {
            c if c == JSObjectClass::Closure as u8 => {
                let func_val = unsafe {
                    let payload = Object::payload_ptr(obj_ptr.as_ptr());
                    let closure = core::ptr::addr_of_mut!((*payload).closure);
                    ptr::read_unaligned(ClosureData::func_bytecode_ptr(closure))
                };
                let Some(func_ptr) = function_bytecode_ptr(func_val) else {
                    return ("<anonymous>".to_string(), None);
                };
                let func = unsafe { ptr::read_unaligned(func_ptr.as_ptr()) };
                let name = if func.func_name() == JS_NULL {
                    String::new()
                } else {
                    self.string_from_value(func.func_name())
                        .unwrap_or_default()
                };
                (name, Some(func))
            }
            c if c == JSObjectClass::CFunction as u8 => {
                let data = unsafe {
                    let payload = Object::payload_ptr(obj_ptr.as_ptr());
                    ptr::read_unaligned(core::ptr::addr_of!((*payload).cfunc))
                };
                if let Some(def) = self.c_function(data.idx() as usize) {
                    (def.name_str.to_string(), None)
                } else {
                    ("<anonymous>".to_string(), None)
                }
            }
            _ => ("<anonymous>".to_string(), None),
        }
    }

    fn string_from_value(&self, val: JSValue) -> Option<String> {
        let mut scratch = [0u8; 5];
        let view = string_view(val, &mut scratch)?;
        Some(String::from_utf8_lossy(view.bytes()).into_owned())
    }

    /// Allocates a new RegExp object.
    pub fn alloc_regexp(
        &mut self,
        source: JSValue,
        byte_code: JSValue,
        flags: i32,
    ) -> Result<JSValue, ContextError> {
        use crate::object::RegExp;
        let proto = self.class_proto()[JSObjectClass::RegExp as usize];
        let regexp = self.alloc_object(JSObjectClass::RegExp, proto, size_of::<RegExp>())?;
        let obj_ptr = value_to_ptr::<Object>(regexp).expect("regexp allocation must be a pointer");
        unsafe {
            // SAFETY: regexp points at a writable regexp object allocation.
            let payload = Object::payload_ptr(obj_ptr.as_ptr());
            let regexp_ptr = core::ptr::addr_of_mut!((*payload).regexp);
            ptr::write_unaligned(regexp_ptr, RegExp::new(source, byte_code, flags));
        }
        Ok(regexp)
    }

    /// Allocates a new ArrayBuffer object with zeroed byte storage.
    pub fn alloc_array_buffer(&mut self, len: usize) -> Result<JSValue, ContextError> {
        use crate::array_buffer::ArrayBuffer;
        let len_word = JSWord::try_from(len).map_err(|_| ContextError::ByteArrayTooLong {
            len,
            max: JS_BYTE_ARRAY_SIZE_MAX,
        })?;
        if len_word > JS_BYTE_ARRAY_SIZE_MAX {
            return Err(ContextError::ByteArrayTooLong {
                len,
                max: JS_BYTE_ARRAY_SIZE_MAX,
            });
        }
        // Allocate byte array storage (zeroed)
        let byte_size = byte_array_alloc_size(len_word);
        let byte_ptr = self.alloc_mblock(byte_size, MTag::ByteArray)?;
        let byte_header = ByteArrayHeader::new(len_word, false);
        unsafe {
            // SAFETY: byte_ptr points to writable byte array storage.
            ptr::write_unaligned(byte_ptr.as_ptr().cast::<JSWord>(), MbHeader::from(byte_header).word());
            let payload = byte_ptr.as_ptr().add(size_of::<JSWord>());
            ptr::write_bytes(payload, 0, len);
        }
        let byte_buffer = value_from_ptr(byte_ptr);

        // Create ArrayBuffer object
        let proto = self.class_proto()[JSObjectClass::ArrayBuffer as usize];
        let array_buffer = self.alloc_object(JSObjectClass::ArrayBuffer, proto, size_of::<ArrayBuffer>())?;
        let obj_ptr = value_to_ptr::<Object>(array_buffer).expect("array_buffer allocation must be a pointer");
        unsafe {
            // SAFETY: array_buffer points at a writable object allocation.
            let payload = Object::payload_ptr(obj_ptr.as_ptr());
            let ab_ptr = core::ptr::addr_of_mut!((*payload).array_buffer);
            ptr::write_unaligned(ab_ptr, ArrayBuffer::new(byte_buffer));
        }
        Ok(array_buffer)
    }

    /// Allocates a new TypedArray object with given class, buffer, offset and length.
    pub fn alloc_typed_array(
        &mut self,
        class_id: JSObjectClass,
        buffer: JSValue,
        offset: u32,
        len: u32,
    ) -> Result<JSValue, ContextError> {
        use crate::typed_array::TypedArray;
        let proto = self.class_proto()[class_id as usize];
        let typed_array = self.alloc_object(class_id, proto, size_of::<TypedArray>())?;
        let obj_ptr = value_to_ptr::<Object>(typed_array).expect("typed_array allocation must be a pointer");
        unsafe {
            // SAFETY: typed_array points at a writable object allocation.
            let payload = Object::payload_ptr(obj_ptr.as_ptr());
            let ta_ptr = core::ptr::addr_of_mut!((*payload).typed_array);
            ptr::write_unaligned(ta_ptr, TypedArray::new(buffer, len, offset));
        }
        Ok(typed_array)
    }

    #[allow(dead_code)]
    pub(crate) fn alloc_function_bytecode(
        &mut self,
        header: FunctionBytecodeHeader,
        fields: FunctionBytecodeFields,
    ) -> Result<JSValue, ContextError> {
        let size = size_of::<FunctionBytecode>();
        let ptr = self.alloc_mblock(size, MTag::FunctionBytecode)?;
        let func = FunctionBytecode::from_fields(header, fields);
        unsafe {
            // SAFETY: `ptr` points to writable function bytecode storage.
            ptr::write(ptr.as_ptr() as *mut FunctionBytecode, func);
        }
        Ok(value_from_ptr(ptr))
    }

    pub(crate) fn alloc_closure(
        &mut self,
        func_bytecode: JSValue,
        var_refs_len: usize,
    ) -> Result<JSValue, ContextError> {
        let proto = self.class_proto()[JSObjectClass::Closure as usize];
        let extra_size_bytes = (1usize + var_refs_len) * size_of::<JSValue>();
        let closure = self.alloc_object(JSObjectClass::Closure, proto, extra_size_bytes)?;
        let obj_ptr = value_to_ptr::<Object>(closure).expect("closure allocation must be a pointer");
        unsafe {
            // SAFETY: closure points at a writable closure object allocation.
            let payload = Object::payload_ptr(obj_ptr.as_ptr());
            let closure_data = core::ptr::addr_of_mut!((*payload).closure);
            *ClosureData::func_bytecode_ptr(closure_data) = func_bytecode;
        }
        Ok(closure)
    }

    fn alloc_float64(&mut self, value: f64) -> Result<JSValue, ContextError> {
        let size = size_of::<JSWord>() + size_of::<f64>();
        let ptr = self.alloc_mblock(size, MTag::Float64)?;
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
        let ptr = self.alloc_mblock(size, MTag::String)?;
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

    fn run_finalizers(&mut self) {
        if self.finalizers.is_empty() {
            return;
        }
        let base = self.heap.heap_base().as_ptr();
        let heap_free = self.heap.heap_free().as_ptr();
        let mut ptr = base;
        while ptr < heap_free {
            let size = unsafe {
                // SAFETY: ptr is within the heap and points at a memblock header.
                mblock_size(NonNull::new_unchecked(ptr))
            };
            let header_word = unsafe {
                // SAFETY: ptr points to a readable header word.
                ptr::read_unaligned(ptr.cast::<JSWord>())
            };
            let header = MbHeader::from_word(header_word);
            if header.tag() == MTag::Object {
                let obj_header = ObjectHeader::from_word(header_word);
                let class_id = obj_header.class_id();
                let base_id = JSObjectClass::User as u8;
                if class_id >= base_id {
                    let idx = (class_id - base_id) as usize;
                    if let Some(Some(finalizer)) = self.finalizers.get(idx) {
                        let obj = ptr as *mut Object;
                        let payload = unsafe {
                            // SAFETY: obj points at a valid object payload.
                            Object::payload_ptr(obj)
                        };
                        let user = unsafe {
                            // SAFETY: payload layout matches the object header for user classes.
                            core::ptr::addr_of!((*payload).user)
                        };
                        let opaque = unsafe {
                            // SAFETY: user points at initialized user data.
                            (*user).opaque()
                        };
                        unsafe {
                            // SAFETY: finalizer expects a C-style JSContext pointer.
                            finalizer(
                                self as *mut JSContext as *mut crate::capi_defs::JSContext,
                                opaque,
                            );
                        }
                    }
                }
            }
            ptr = unsafe {
                // SAFETY: size is the memblock size for ptr.
                ptr.add(size)
            };
        }
    }

}

impl Drop for JSContext {
    fn drop(&mut self) {
        self.run_finalizers();
    }
}

fn push_backtrace_line(buf: &mut String, line: &str) -> bool {
    if buf.len().saturating_add(line.len()) > BACKTRACE_MAX_LEN {
        return false;
    }
    buf.push_str(line);
    true
}

fn decode_stack_ptr(stack_top: *mut JSValue, val: JSValue) -> Option<*mut JSValue> {
    if !val.is_int() {
        return None;
    }
    let offset = value_get_int(val);
    if offset < 0 {
        return None;
    }
    Some(unsafe { stack_top.offset(-(offset as isize)) })
}

struct ByteArrayView {
    ptr: *const u8,
    len: usize,
}

impl ByteArrayView {
    unsafe fn as_slice<'a>(&self) -> &'a [u8] {
        // SAFETY: caller ensures the ByteArrayView points at valid byte data.
        unsafe { slice::from_raw_parts(self.ptr, self.len) }
    }
}

fn byte_array_view(val: JSValue) -> Option<ByteArrayView> {
    let ptr = value_to_ptr::<u8>(val)?;
    let header_word = unsafe {
        // SAFETY: ptr points at a readable memblock header.
        ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
    };
    let header = MbHeader::from_word(header_word);
    if header.tag() != MTag::ByteArray {
        return None;
    }
    let size = ByteArrayHeader::from(header).size() as usize;
    let payload = unsafe { ptr.as_ptr().add(size_of::<JSWord>()) };
    Some(ByteArrayView { ptr: payload, len: size })
}

fn function_bytecode_ptr(val: JSValue) -> Option<NonNull<FunctionBytecode>> {
    let ptr = value_to_ptr::<FunctionBytecode>(val)?;
    let header_word = unsafe {
        // SAFETY: ptr points at a readable function bytecode header word.
        ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
    };
    let header = MbHeader::from_word(header_word);
    if header.tag() != MTag::FunctionBytecode {
        return None;
    }
    Some(ptr)
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

fn map_property_error(err: PropertyError) -> ContextError {
    debug_assert!(
        matches!(&err, PropertyError::OutOfMemory),
        "unexpected property error: {:?}",
        err
    );
    ContextError::OutOfMemory
}

fn class_id_from_ctor(image: &StdlibImage, ctor_idx: usize) -> Result<JSObjectClass, ContextError> {
    let meta = image
        .c_functions
        .get(ctor_idx)
        .ok_or(ContextError::StdlibCFunctionIndexOutOfBounds { index: ctor_idx })?;
    class_id_from_name(meta.magic)
        .ok_or(ContextError::UnknownClassId { name: meta.magic })
}

fn class_id_from_name(name: &str) -> Option<JSObjectClass> {
    match name {
        "JS_CLASS_OBJECT" => Some(JSObjectClass::Object),
        "JS_CLASS_ARRAY" => Some(JSObjectClass::Array),
        "JS_CLASS_C_FUNCTION" => Some(JSObjectClass::CFunction),
        "JS_CLASS_CLOSURE" => Some(JSObjectClass::Closure),
        "JS_CLASS_NUMBER" => Some(JSObjectClass::Number),
        "JS_CLASS_BOOLEAN" => Some(JSObjectClass::Boolean),
        "JS_CLASS_STRING" => Some(JSObjectClass::String),
        "JS_CLASS_DATE" => Some(JSObjectClass::Date),
        "JS_CLASS_REGEXP" => Some(JSObjectClass::RegExp),
        "JS_CLASS_ERROR" => Some(JSObjectClass::Error),
        "JS_CLASS_EVAL_ERROR" => Some(JSObjectClass::EvalError),
        "JS_CLASS_RANGE_ERROR" => Some(JSObjectClass::RangeError),
        "JS_CLASS_REFERENCE_ERROR" => Some(JSObjectClass::ReferenceError),
        "JS_CLASS_SYNTAX_ERROR" => Some(JSObjectClass::SyntaxError),
        "JS_CLASS_TYPE_ERROR" => Some(JSObjectClass::TypeError),
        "JS_CLASS_URI_ERROR" => Some(JSObjectClass::UriError),
        "JS_CLASS_INTERNAL_ERROR" => Some(JSObjectClass::InternalError),
        "JS_CLASS_ARRAY_BUFFER" => Some(JSObjectClass::ArrayBuffer),
        "JS_CLASS_TYPED_ARRAY" => Some(JSObjectClass::TypedArray),
        "JS_CLASS_UINT8C_ARRAY" => Some(JSObjectClass::Uint8CArray),
        "JS_CLASS_INT8_ARRAY" => Some(JSObjectClass::Int8Array),
        "JS_CLASS_UINT8_ARRAY" => Some(JSObjectClass::Uint8Array),
        "JS_CLASS_INT16_ARRAY" => Some(JSObjectClass::Int16Array),
        "JS_CLASS_UINT16_ARRAY" => Some(JSObjectClass::Uint16Array),
        "JS_CLASS_INT32_ARRAY" => Some(JSObjectClass::Int32Array),
        "JS_CLASS_UINT32_ARRAY" => Some(JSObjectClass::Uint32Array),
        "JS_CLASS_FLOAT32_ARRAY" => Some(JSObjectClass::Float32Array),
        "JS_CLASS_FLOAT64_ARRAY" => Some(JSObjectClass::Float64Array),
        "JS_CLASS_USER" => Some(JSObjectClass::User),
        _ => None,
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cutils::unicode_to_utf8;
    use crate::exception::JsFormatArg;
    use crate::jsvalue::{
        value_get_int, value_get_special_tag, value_get_special_value, value_to_ptr,
        JS_TAG_SHORT_FUNC, JS_TAG_STRING_CHAR, JS_EXCEPTION,
    };
    use crate::memblock::Float64Header;
    use crate::object::{Object, ObjectUserData};
    use crate::property::get_property;
    use crate::string::runtime::string_view;
    use crate::stdlib::MQUICKJS_STDLIB_IMAGE;
    use core::ffi::c_void;
    use core::slice;
    use core::sync::atomic::{AtomicUsize, Ordering};

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
            finalizers: &[],
        })
        .expect("context init");

        assert!(ctx.heap().is_valid());
        assert_eq!(ctx.sp(), ctx.fp());
        assert_eq!(ctx.sp(), ctx.stack_bottom());
        assert!(ctx.stack_top().as_ptr() as usize >= ctx.sp().as_ptr() as usize);
        assert_eq!(ctx.class_count() as u32, MQUICKJS_STDLIB_IMAGE.class_count);

        let class_proto = ctx.class_proto();
        assert_eq!(class_proto.len() as u32, MQUICKJS_STDLIB_IMAGE.class_count);
        assert!(class_proto[JSObjectClass::Object as usize].is_ptr());
        assert!(class_proto[JSObjectClass::Closure as usize].is_ptr());

        assert!(ctx.global_obj().is_ptr());
        assert!(ctx.minus_zero().is_ptr());
    }

    #[test]
    fn stdlib_object_function_prototype_chain() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");

        let object_key = ctx.intern_string(b"Object").expect("atom");
        let function_key = ctx.intern_string(b"Function").expect("atom");
        let proto_key = ctx.intern_string(b"prototype").expect("atom");
        let ctor_key = ctx.intern_string(b"constructor").expect("atom");
        let has_own_key = ctx.intern_string(b"hasOwnProperty").expect("atom");
        let to_string_key = ctx.intern_string(b"toString").expect("atom");
        let call_key = ctx.intern_string(b"call").expect("atom");
        let global_this_key = ctx.intern_string(b"globalThis").expect("atom");

        let global = ctx.global_obj();
        let object_ctor = get_property(&mut ctx, global, object_key).expect("Object ctor");
        let function_ctor = get_property(&mut ctx, global, function_key).expect("Function ctor");
        assert_eq!(object_ctor, ctx.class_obj()[JSObjectClass::Object as usize]);
        assert_eq!(function_ctor, ctx.class_obj()[JSObjectClass::Closure as usize]);

        let object_proto = ctx.class_proto()[JSObjectClass::Object as usize];
        let function_proto = ctx.class_proto()[JSObjectClass::Closure as usize];
        assert_eq!(
            get_property(&mut ctx, object_ctor, proto_key).expect("Object.prototype"),
            object_proto
        );
        assert_eq!(
            get_property(&mut ctx, function_ctor, proto_key).expect("Function.prototype"),
            function_proto
        );
        assert_eq!(
            get_property(&mut ctx, object_proto, ctor_key).expect("Object.prototype.constructor"),
            object_ctor
        );
        assert_eq!(
            get_property(&mut ctx, function_proto, ctor_key).expect("Function.prototype.constructor"),
            function_ctor
        );

        let has_own = get_property(&mut ctx, object_proto, has_own_key).expect("hasOwnProperty");
        assert_eq!(value_get_special_tag(has_own), JS_TAG_SHORT_FUNC);
        let to_string = get_property(&mut ctx, object_proto, to_string_key).expect("toString");
        assert_eq!(value_get_special_tag(to_string), JS_TAG_SHORT_FUNC);
        let call = get_property(&mut ctx, function_proto, call_key).expect("call");
        assert_eq!(value_get_special_tag(call), JS_TAG_SHORT_FUNC);

        let global_this = get_property(&mut ctx, global, global_this_key).expect("globalThis");
        assert_eq!(global_this, global);
    }

    #[test]
    fn context_empty_props_matches_c_layout() {
        let ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
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
            assert!(val.is_int());
            assert_eq!(value_get_int(val), 0);
        }
    }

    #[test]
    fn context_minus_zero_is_float64() {
        let ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
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
            finalizers: &[],
        })
        .expect("context init");

        let val = ctx.new_float64(42.0).expect("alloc");
        assert!(val.is_int());
        assert_eq!(value_get_int(val), 42);
    }

    #[test]
    fn context_new_float64_allocates_large_value() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");

        let val = ctx.new_float64(1.0e300).expect("alloc");
        assert!(val.is_ptr());
    }

    #[test]
    fn context_new_float64_caches_minus_zero() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
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
            finalizers: &[],
        })
        .expect("context init");

        let val = ctx.new_float64(1.5).expect("alloc");
        assert!(val.is_short_float());
    }

    #[test]
    fn context_new_string_len_handles_empty_and_char() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
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
        assert!(string_val.is_ptr());
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
            finalizers: &[],
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
            finalizers: &[],
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
            finalizers: &[],
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
        assert!(left.is_ptr());
        assert!(right.is_ptr());

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
            finalizers: &[],
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
            finalizers: &[],
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

    unsafe extern "C" fn record_finalizer(
        _ctx: *mut crate::capi_defs::JSContext,
        opaque: *mut c_void,
    ) {
        let counter = unsafe {
            // SAFETY: opaque points at the AtomicUsize set by the test.
            &*(opaque as *const AtomicUsize)
        };
        counter.fetch_add(1, Ordering::SeqCst);
    }

    #[test]
    fn drop_runs_user_finalizers() {
        let counter = AtomicUsize::new(0);
        let finalizers: [Option<crate::capi_defs::JSCFinalizer>; 1] = [Some(record_finalizer)];
        {
            let mut ctx = JSContext::new(ContextConfig {
                image: &MQUICKJS_STDLIB_IMAGE,
                memory_size: 16 * 1024,
                prepare_compilation: false,
                finalizers: &finalizers,
            })
            .expect("context init");
            let proto = ctx.class_proto()[JSObjectClass::Object as usize];
            let obj = ctx
                .alloc_object(JSObjectClass::Object, proto, size_of::<ObjectUserData>())
                .expect("user object");
            let obj_ptr = value_to_ptr::<Object>(obj).expect("object ptr");
            unsafe {
                // SAFETY: obj_ptr points at a valid object payload.
                let header_word = ptr::read_unaligned(obj_ptr.as_ptr().cast::<JSWord>());
                let header = ObjectHeader::from_word(header_word);
                let patched = ObjectHeader::new(JSObjectClass::User as u8, header.extra_size(), header.gc_mark());
                ptr::write_unaligned(obj_ptr.as_ptr().cast::<JSWord>(), patched.header().word());
                let payload = Object::payload_ptr(obj_ptr.as_ptr());
                let user = core::ptr::addr_of_mut!((*payload).user);
                let counter_ptr = core::ptr::addr_of!(counter) as *const AtomicUsize as *mut c_void;
                ptr::write_unaligned(user, ObjectUserData::new(counter_ptr));
            }
        }
        assert_eq!(counter.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn throw_error_format_includes_jsvalue() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 32 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let prop = ctx.new_string("abc").expect("prop");
        let val = ctx.throw_type_error_fmt(
            "cannot read property '%o' of null",
            &[JsFormatArg::from(prop)],
        );
        assert_eq!(val, JS_EXCEPTION);
        let err = ctx.take_current_exception();
        let msg = ctx.get_error_message(err).expect("message");
        assert_eq!(
            bytes_from_val(msg),
            b"cannot read property 'abc' of null"
        );
    }

    #[test]
    fn throw_error_format_zero_pads_hex() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 32 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let val = ctx.throw_internal_error_fmt(
            "invalid opcode: pc=%u opcode=0x%02x",
            &[JsFormatArg::from(12u32), JsFormatArg::from(3u32)],
        );
        assert_eq!(val, JS_EXCEPTION);
        let err = ctx.take_current_exception();
        let msg = ctx.get_error_message(err).expect("message");
        assert_eq!(bytes_from_val(msg), b"invalid opcode: pc=12 opcode=0x03");
    }

}
