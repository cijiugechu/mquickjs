use crate::atom::AtomTables;
use crate::capi_defs::{JSCFinalizer, JSContext};
use crate::containers::ValueArrayHeader;
use crate::enums::JSObjectClass;
use crate::function_bytecode::FunctionBytecode;
use crate::gc_ref::GcRefState;
use crate::heap::{mblock_size, set_free_block, HeapLayout};
use crate::jsvalue::{value_from_ptr, value_to_ptr, JSValue, JSWord, JS_NULL};
use crate::memblock::{MbHeader, MTag};
use crate::object::{Object, ObjectHeader, PrimitiveValue};
use crate::parser::parse_state::JSParseState;
use crate::string::string_pos_cache::{StringPosCacheEntry, JS_STRING_POS_CACHE_SIZE};
use core::mem::size_of;
use core::ptr::{self, NonNull};

#[derive(Copy, Clone, Debug)]
pub struct GcMarkConfig {
    pub keep_atoms: bool,
    pub mark_stack_slots: usize,
}

impl Default for GcMarkConfig {
    fn default() -> Self {
        Self {
            keep_atoms: false,
            mark_stack_slots: usize::MAX,
        }
    }
}

pub struct GcRoots<'a> {
    class_roots: &'a [JSValue],
    stack_roots: &'a [JSValue],
    gc_refs: Option<&'a GcRefState>,
    parse_state: Option<&'a JSParseState>,
    atom_tables: Option<&'a mut AtomTables>,
    string_pos_cache: Option<&'a mut [StringPosCacheEntry; JS_STRING_POS_CACHE_SIZE]>,
}

impl<'a> GcRoots<'a> {
    pub fn new(class_roots: &'a [JSValue], stack_roots: &'a [JSValue]) -> Self {
        Self {
            class_roots,
            stack_roots,
            gc_refs: None,
            parse_state: None,
            atom_tables: None,
            string_pos_cache: None,
        }
    }

    pub fn with_gc_refs(mut self, refs: &'a GcRefState) -> Self {
        self.gc_refs = Some(refs);
        self
    }

    pub fn with_parse_state(mut self, state: &'a JSParseState) -> Self {
        self.parse_state = Some(state);
        self
    }

    pub fn with_atom_tables(mut self, tables: &'a mut AtomTables) -> Self {
        self.atom_tables = Some(tables);
        self
    }

    pub fn with_string_pos_cache(
        mut self,
        cache: &'a mut [StringPosCacheEntry; JS_STRING_POS_CACHE_SIZE],
    ) -> Self {
        self.string_pos_cache = Some(cache);
        self
    }
}

#[derive(Copy, Clone, Debug)]
pub struct GcSweepFinalizers<'a> {
    ctx: NonNull<JSContext>,
    table: &'a [Option<JSCFinalizer>],
}

impl<'a> GcSweepFinalizers<'a> {
    pub fn new(ctx: NonNull<JSContext>, table: &'a [Option<JSCFinalizer>]) -> Self {
        Self { ctx, table }
    }

    fn finalizer_for(self, class_id: u8) -> Option<JSCFinalizer> {
        let base = JSObjectClass::User as u8;
        if class_id < base {
            return None;
        }
        let idx = (class_id - base) as usize;
        self.table.get(idx).copied().flatten()
    }
}

pub fn gc_mark_all(heap: &HeapLayout, roots: &mut GcRoots<'_>, config: GcMarkConfig) {
    let mut marker = GcMarker::new(heap, config.mark_stack_slots);

    if let Some(tables) = roots.atom_tables.as_ref() {
        let tables = &**tables;
        tables.visit_rom_atoms(|val| marker.mark_root(val));
        if config.keep_atoms {
            for &val in tables.unique_strings() {
                marker.mark_root(val);
            }
        }
    }

    for &val in roots.class_roots {
        marker.mark_root(val);
    }
    for &val in roots.stack_roots {
        marker.mark_root(val);
    }

    if let Some(refs) = roots.gc_refs {
        refs.visit_roots(|val| marker.mark_root(val));
    }

    if let Some(state) = roots.parse_state {
        for val in state.gc_roots() {
            marker.mark_root(val);
        }
    }

    marker.drain_overflow();

    if let Some(tables) = roots.atom_tables.as_mut() {
        let _ = tables.sweep_unique_strings(|val| gc_is_marked(heap, val));
    }

    if let Some(cache) = roots.string_pos_cache.as_mut() {
        for entry in cache.iter_mut() {
            if !gc_is_marked(heap, entry.str()) {
                entry.set_str(JS_NULL);
            }
        }
    }
}

pub fn gc_is_marked(heap: &HeapLayout, val: JSValue) -> bool {
    let Some(ptr) = value_to_ptr::<u8>(val) else {
        return false;
    };
    if !ptr_in_heap(heap, ptr) {
        return true;
    }
    let header_word = unsafe { ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>()) };
    MbHeader::from_word(header_word).gc_mark()
}

/// # Safety
/// The heap layout must refer to a valid, contiguous heap buffer with
/// internally consistent memblocks.
pub unsafe fn gc_sweep(heap: &mut HeapLayout, finalizers: Option<&GcSweepFinalizers<'_>>) {
    let mut ptr = heap.heap_base().as_ptr();
    let heap_free = heap.heap_free().as_ptr();

    while ptr < heap_free {
        let size = unsafe {
            // SAFETY: `ptr` is within the heap and points at a memblock header.
            mblock_size(NonNull::new_unchecked(ptr))
        };
        debug_assert!(size > 0);
        let header_word = unsafe {
            // SAFETY: `ptr` points to a readable header word.
            ptr::read_unaligned(ptr.cast::<JSWord>())
        };
        let header = MbHeader::from_word(header_word);

        if header.gc_mark() {
            let cleared = header.with_gc_mark(false);
            unsafe {
                // SAFETY: `ptr` points to writable header storage.
                ptr::write_unaligned(ptr.cast::<JSWord>(), cleared.word());
            }
            ptr = unsafe {
                // SAFETY: `ptr` stays within the heap after advancing by `size`.
                ptr.add(size)
            };
            continue;
        }

        if header.tag() == MTag::Object {
            let obj_header = ObjectHeader::from_word(header_word);
            if let Some(finalizers) = finalizers
                && let Some(finalizer) = finalizers.finalizer_for(obj_header.class_id())
            {
                let obj = ptr as *mut Object;
                let payload = unsafe {
                    // SAFETY: `obj` points at a valid object payload within the heap.
                    Object::payload_ptr(obj)
                };
                let user = unsafe {
                    // SAFETY: payload layout matches the object header for user classes.
                    core::ptr::addr_of!((*payload).user)
                };
                let opaque = unsafe {
                    // SAFETY: `user` points at initialized user data.
                    (*user).opaque()
                };
                unsafe {
                    // SAFETY: caller guarantees `ctx` and `opaque` are valid for the finalizer.
                    finalizer(finalizers.ctx.as_ptr(), opaque);
                }
            }
        }

        let mut next_ptr = unsafe {
            // SAFETY: `ptr` stays within the heap after advancing by `size`.
            ptr.add(size)
        };
        while next_ptr < heap_free {
            let next_header_word = unsafe {
                // SAFETY: `next_ptr` points to a readable header word.
                ptr::read_unaligned(next_ptr.cast::<JSWord>())
            };
            let next_header = MbHeader::from_word(next_header_word);
            if next_header.gc_mark() {
                break;
            }
            let next_size = unsafe {
                // SAFETY: `next_ptr` is within the heap and points at a memblock header.
                mblock_size(NonNull::new_unchecked(next_ptr))
            };
            debug_assert!(next_size > 0);
            next_ptr = unsafe {
                // SAFETY: `next_ptr` stays within the heap after advancing by `next_size`.
                next_ptr.add(next_size)
            };
        }

        let span = next_ptr as usize - ptr as usize;
        unsafe {
            // SAFETY: `ptr` is a valid memblock start and `span` stays within the heap.
            set_free_block(NonNull::new_unchecked(ptr), span);
        }
        ptr = next_ptr;
    }
}

fn ptr_in_heap(heap: &HeapLayout, ptr: NonNull<u8>) -> bool {
    let base = heap.heap_base().as_ptr() as usize;
    let free = heap.heap_free().as_ptr() as usize;
    let addr = ptr.as_ptr() as usize;
    addr >= base && addr < free
}

#[derive(Copy, Clone, Debug)]
enum MarkItem {
    Value(JSValue),
    ValueArray { value: JSValue, pos: u32 },
}

impl MarkItem {
    fn slots(self) -> usize {
        match self {
            MarkItem::Value(_) => 1,
            MarkItem::ValueArray { .. } => 2,
        }
    }
}

#[derive(Debug)]
struct MarkStack {
    items: Vec<MarkItem>,
    slots: usize,
    limit: usize,
}

impl MarkStack {
    fn new(limit: usize) -> Self {
        Self {
            items: Vec::new(),
            slots: 0,
            limit,
        }
    }

    fn try_push(&mut self, item: MarkItem) -> bool {
        let needed = item.slots();
        let next = match self.slots.checked_add(needed) {
            Some(next) => next,
            None => return false,
        };
        if next > self.limit {
            return false;
        }
        self.items.push(item);
        self.slots = next;
        true
    }

    fn pop(&mut self) -> Option<MarkItem> {
        let item = self.items.pop()?;
        self.slots = self.slots.saturating_sub(item.slots());
        Some(item)
    }
}

struct GcMarker {
    heap_base: NonNull<u8>,
    heap_free: NonNull<u8>,
    stack: MarkStack,
    overflow: bool,
}

impl GcMarker {
    fn new(heap: &HeapLayout, mark_stack_slots: usize) -> Self {
        Self {
            heap_base: heap.heap_base(),
            heap_free: heap.heap_free(),
            stack: MarkStack::new(mark_stack_slots),
            overflow: false,
        }
    }

    fn mark_root(&mut self, val: JSValue) {
        self.mark_value(val);
        self.flush();
    }

    fn mark_value(&mut self, val: JSValue) {
        let Some(ptr) = value_to_ptr::<u8>(val) else {
            return;
        };
        if !self.ptr_in_heap(ptr) {
            return;
        }
        let header_word = unsafe { ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>()) };
        let header = MbHeader::from_word(header_word);
        if header.gc_mark() {
            return;
        }
        let header = header.with_gc_mark(true);
        unsafe {
            ptr::write_unaligned(ptr.as_ptr().cast::<JSWord>(), header.word());
        }
        if !mtag_has_references(header.tag()) {
            return;
        }
        let item = if header.tag() == MTag::ValueArray {
            MarkItem::ValueArray { value: val, pos: 0 }
        } else {
            MarkItem::Value(val)
        };
        if !self.stack.try_push(item) {
            self.overflow = true;
        }
    }

    fn flush(&mut self) {
        while let Some(item) = self.stack.pop() {
            match item {
                MarkItem::Value(val) => self.flush_value(val),
                MarkItem::ValueArray { value, pos } => self.flush_value_array(value, pos),
            }
        }
    }

    fn flush_value(&mut self, val: JSValue) {
        let Some(ptr) = value_to_ptr::<u8>(val) else {
            return;
        };
        if !self.ptr_in_heap(ptr) {
            return;
        }
        let header_word = unsafe { ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>()) };
        let header = MbHeader::from_word(header_word);
        match header.tag() {
            MTag::Object => unsafe { self.mark_object(ptr.as_ptr()) },
            MTag::VarRef => unsafe { self.mark_var_ref(ptr.as_ptr()) },
            MTag::FunctionBytecode => unsafe { self.mark_function_bytecode(ptr.as_ptr()) },
            _ => {}
        }
    }

    fn flush_value_array(&mut self, value: JSValue, pos: u32) {
        let Some(ptr) = value_to_ptr::<u8>(value) else {
            return;
        };
        if !self.ptr_in_heap(ptr) {
            return;
        }
        let header_word = unsafe { ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>()) };
        let header = MbHeader::from_word(header_word);
        if header.tag() != MTag::ValueArray {
            return;
        }
        let array_header = ValueArrayHeader::from(header);
        let size = array_header.size() as usize;
        let arr_ptr = unsafe { ptr.as_ptr().add(size_of::<JSWord>()) as *mut JSValue };
        let mut idx = pos as usize;

        while idx < size {
            let val = unsafe { *arr_ptr.add(idx) };
            if value_to_ptr::<u8>(val).is_some() {
                break;
            }
            idx += 1;
        }

        if idx < size {
            if idx + 1 < size {
                let item = MarkItem::ValueArray {
                    value,
                    pos: (idx + 1) as u32,
                };
                if !self.stack.try_push(item) {
                    self.overflow = true;
                }
            }
            let val = unsafe { *arr_ptr.add(idx) };
            self.mark_value(val);
        }
    }

    fn drain_overflow(&mut self) {
        while self.overflow {
            self.overflow = false;
            let mut ptr = self.heap_base.as_ptr();
            while ptr < self.heap_free.as_ptr() {
                let size = unsafe { mblock_size(NonNull::new_unchecked(ptr)) };
                let header_word = unsafe { ptr::read_unaligned(ptr.cast::<JSWord>()) };
                let header = MbHeader::from_word(header_word);
                if header.gc_mark() && mtag_has_references(header.tag()) {
                    let val = unsafe { value_from_ptr(NonNull::new_unchecked(ptr)) };
                    let item = if header.tag() == MTag::ValueArray {
                        MarkItem::ValueArray { value: val, pos: 0 }
                    } else {
                        MarkItem::Value(val)
                    };
                    if !self.stack.try_push(item) {
                        self.overflow = true;
                    } else {
                        self.flush();
                    }
                }
                ptr = unsafe { ptr.add(size) };
            }
        }
    }

    fn ptr_in_heap(&self, ptr: NonNull<u8>) -> bool {
        let base = self.heap_base.as_ptr() as usize;
        let free = self.heap_free.as_ptr() as usize;
        let addr = ptr.as_ptr() as usize;
        addr >= base && addr < free
    }

    unsafe fn mark_object(&mut self, ptr: *mut u8) {
        let obj = ptr as *mut Object;
        unsafe {
            self.mark_value(*Object::proto_ptr(obj));
            self.mark_value(*Object::props_ptr(obj));
        }
        let header_word = unsafe { ptr::read_unaligned(ptr.cast::<JSWord>()) };
        let header = ObjectHeader::from_word(header_word);
        let class_id = header.class_id();
        match class_id {
            x if x == JSObjectClass::Closure as u8 => unsafe {
                let payload = Object::payload_ptr(obj);
                let closure = core::ptr::addr_of_mut!((*payload).closure);
                self.mark_value(*crate::closure_data::ClosureData::func_bytecode_ptr(closure));
                if header.extra_size() > 1 {
                    let var_refs = crate::closure_data::ClosureData::var_refs_ptr(closure);
                    let count = (header.extra_size() - 1) as usize;
                    for idx in 0..count {
                        self.mark_value(*var_refs.add(idx));
                    }
                }
            },
            x if x == JSObjectClass::CFunction as u8 => unsafe {
                if header.extra_size() > 1 {
                    let payload = Object::payload_ptr(obj);
                    let cfunc = core::ptr::addr_of_mut!((*payload).cfunc);
                    self.mark_value(*crate::cfunction_data::CFunctionData::params_ptr(cfunc));
                }
            },
            x if x == JSObjectClass::Array as u8 => unsafe {
                let payload = Object::payload_ptr(obj);
                let array = core::ptr::addr_of_mut!((*payload).array);
                self.mark_value(*crate::array_data::ArrayData::tab_ptr(array));
            },
            x if x == JSObjectClass::Error as u8 => unsafe {
                let payload = Object::payload_ptr(obj);
                let error = core::ptr::addr_of_mut!((*payload).error);
                self.mark_value(*crate::error_data::ErrorData::message_ptr(error));
                self.mark_value(*crate::error_data::ErrorData::stack_ptr(error));
            },
            x if x == JSObjectClass::ArrayBuffer as u8 => unsafe {
                let payload = Object::payload_ptr(obj);
                let buffer = core::ptr::addr_of_mut!((*payload).array_buffer);
                self.mark_value(*crate::array_buffer::ArrayBuffer::byte_buffer_ptr(buffer));
            },
            x if x == JSObjectClass::Uint8CArray as u8
                || x == JSObjectClass::Int8Array as u8
                || x == JSObjectClass::Uint8Array as u8
                || x == JSObjectClass::Int16Array as u8
                || x == JSObjectClass::Uint16Array as u8
                || x == JSObjectClass::Int32Array as u8
                || x == JSObjectClass::Uint32Array as u8
                || x == JSObjectClass::Float32Array as u8
                || x == JSObjectClass::Float64Array as u8 =>
            unsafe {
                let payload = Object::payload_ptr(obj);
                let array = core::ptr::addr_of_mut!((*payload).typed_array);
                self.mark_value(*crate::typed_array::TypedArray::buffer_ptr(array));
            },
            x if x == JSObjectClass::RegExp as u8 => unsafe {
                let payload = Object::payload_ptr(obj);
                let regexp = core::ptr::addr_of_mut!((*payload).regexp);
                self.mark_value(*crate::object::RegExp::source_ptr(regexp));
                self.mark_value(*crate::object::RegExp::byte_code_ptr(regexp));
            },
            x if x == JSObjectClass::Number as u8
                || x == JSObjectClass::Boolean as u8
                || x == JSObjectClass::String as u8 =>
            unsafe {
                let payload = Object::payload_ptr(obj);
                let primitive = core::ptr::addr_of_mut!((*payload).primitive);
                self.mark_value(*PrimitiveValue::value_ptr(primitive));
            },
            _ => {}
        }
    }

    unsafe fn mark_var_ref(&mut self, ptr: *mut u8) {
        let val_ptr = unsafe { ptr.add(size_of::<JSWord>()) as *mut JSValue };
        unsafe {
            self.mark_value(*val_ptr);
        }
    }

    unsafe fn mark_function_bytecode(&mut self, ptr: *mut u8) {
        let func = ptr as *mut FunctionBytecode;
        unsafe {
            self.mark_value((*func).func_name());
            self.mark_value((*func).byte_code());
            self.mark_value((*func).cpool());
            self.mark_value((*func).vars());
            self.mark_value((*func).ext_vars());
            self.mark_value((*func).filename());
            self.mark_value((*func).pc2line());
        }
    }
}

fn mtag_has_references(tag: MTag) -> bool {
    matches!(
        tag,
        MTag::Object | MTag::ValueArray | MTag::VarRef | MTag::FunctionBytecode
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::closure_data::ClosureData;
    use crate::containers::{ByteArrayHeader, ValueArrayHeader};
    use crate::function_bytecode::{FunctionBytecodeFields, FunctionBytecodeHeader};
    use crate::heap::HeapLayout;
    use crate::jsvalue::{value_from_ptr, JSW, JS_NULL};
    use crate::memblock::{MbHeader, MTag};
    use crate::object::{Object, ObjectUserData};
    use crate::capi_defs::JSContext;
    use core::ffi::c_void;
    use core::sync::atomic::{AtomicUsize, Ordering};

    struct HeapArena {
        _storage: Vec<JSWord>,
        layout: HeapLayout,
    }

    impl HeapArena {
        fn new(words: usize, stack_words: usize) -> Self {
            let mut storage = vec![0 as JSWord; words];
            let base = NonNull::new(storage.as_mut_ptr() as *mut u8).unwrap();
            let stack_top =
                unsafe { NonNull::new_unchecked(base.as_ptr().add(words * JSW as usize)) };
            let stack_bottom = unsafe {
                NonNull::new_unchecked(
                    stack_top
                        .as_ptr()
                        .sub(stack_words * JSW as usize) as *mut JSValue,
                )
            };
            let layout = HeapLayout::new(base, base, stack_top, stack_bottom, 0);
            Self {
                _storage: storage,
                layout,
            }
        }
    }

    unsafe fn write_header(ptr: NonNull<u8>, header: MbHeader) {
        unsafe {
            ptr::write_unaligned(ptr.as_ptr().cast::<JSWord>(), header.word());
        }
    }

    #[test]
    fn marks_object_graph() {
        let mut arena = HeapArena::new(256, 16);

        let byte_size = crate::containers::byte_array_alloc_size(4);
        let byte_ptr = arena
            .layout
            .malloc(byte_size, MTag::ByteArray, |_| {})
            .unwrap();
        let byte_header = ByteArrayHeader::new(4, false);
        unsafe {
            write_header(byte_ptr, MbHeader::from(byte_header));
        }
        let byte_val = value_from_ptr(byte_ptr);

        let value_array_len = 2usize;
        let array_size = size_of::<JSWord>() + value_array_len * size_of::<JSValue>();
        let array_ptr = arena
            .layout
            .malloc(array_size, MTag::ValueArray, |_| {})
            .unwrap();
        let array_header = ValueArrayHeader::new(value_array_len as JSWord, false);
        unsafe {
            write_header(array_ptr, MbHeader::from(array_header));
            let arr_ptr = array_ptr.as_ptr().add(size_of::<JSWord>()) as *mut JSValue;
            *arr_ptr.add(0) = byte_val;
            *arr_ptr.add(1) = JS_NULL;
        }
        let array_val = value_from_ptr(array_ptr);

        let func_ptr = arena
            .layout
            .malloc(size_of::<FunctionBytecode>(), MTag::FunctionBytecode, |_| {})
            .unwrap();
        let func_header = FunctionBytecodeHeader::new(false, false, false, 0, false);
        let func_fields = FunctionBytecodeFields {
            func_name: JS_NULL,
            byte_code: JS_NULL,
            cpool: JS_NULL,
            vars: JS_NULL,
            ext_vars: JS_NULL,
            stack_size: 0,
            ext_vars_len: 0,
            filename: JS_NULL,
            pc2line: JS_NULL,
            source_pos: 0,
        };
        let func = FunctionBytecode::from_fields(func_header, func_fields);
        unsafe {
            ptr::write(func_ptr.as_ptr() as *mut FunctionBytecode, func);
        }
        let func_val = value_from_ptr(func_ptr);

        let extra_size = 2;
        let obj_size = Object::PAYLOAD_OFFSET + (extra_size as usize) * (JSW as usize);
        let obj_ptr = arena
            .layout
            .malloc(obj_size, MTag::Object, |_| {})
            .unwrap();
        let obj_header = ObjectHeader::new(JSObjectClass::Closure as u8, extra_size, false);
        unsafe {
            write_header(obj_ptr, obj_header.header());
            let obj = obj_ptr.as_ptr() as *mut Object;
            *Object::proto_ptr(obj) = JS_NULL;
            *Object::props_ptr(obj) = JS_NULL;
            let payload = Object::payload_ptr(obj);
            let closure = core::ptr::addr_of_mut!((*payload).closure);
            *ClosureData::func_bytecode_ptr(closure) = func_val;
            let var_refs = ClosureData::var_refs_ptr(closure);
            *var_refs = array_val;
        }
        let obj_val = value_from_ptr(obj_ptr);

        let other_ptr = arena
            .layout
            .malloc(byte_size, MTag::ByteArray, |_| {})
            .unwrap();
        let other_header = ByteArrayHeader::new(2, false);
        unsafe {
            write_header(other_ptr, MbHeader::from(other_header));
        }
        let other_val = value_from_ptr(other_ptr);

        let roots = [obj_val];
        let mut gc_roots = GcRoots::new(&roots, &[]);
        gc_mark_all(&arena.layout, &mut gc_roots, GcMarkConfig::default());

        assert!(gc_is_marked(&arena.layout, obj_val));
        assert!(gc_is_marked(&arena.layout, func_val));
        assert!(gc_is_marked(&arena.layout, array_val));
        assert!(gc_is_marked(&arena.layout, byte_val));
        assert!(!gc_is_marked(&arena.layout, other_val));
    }

    #[test]
    fn marks_gc_refs() {
        let mut arena = HeapArena::new(64, 8);
        let byte_size = crate::containers::byte_array_alloc_size(3);
        let byte_ptr = arena
            .layout
            .malloc(byte_size, MTag::ByteArray, |_| {})
            .unwrap();
        let byte_header = ByteArrayHeader::new(3, false);
        unsafe {
            write_header(byte_ptr, MbHeader::from(byte_header));
        }
        let byte_val = value_from_ptr(byte_ptr);

        let mut refs = GcRefState::new();
        let mut slot_ref = Box::new(crate::gc_ref::GcRef::new(JS_NULL));
        let slot = refs.push_gc_ref(slot_ref.as_mut());
        unsafe {
            *slot = byte_val;
        }

        let mut gc_roots = GcRoots::new(&[], &[]).with_gc_refs(&refs);
        gc_mark_all(&arena.layout, &mut gc_roots, GcMarkConfig::default());

        assert!(gc_is_marked(&arena.layout, byte_val));

        let _ = refs.pop_gc_ref(slot_ref.as_ref());
    }

    #[test]
    fn clears_unmarked_string_pos_cache() {
        let mut arena = HeapArena::new(64, 8);
        let byte_size = crate::containers::byte_array_alloc_size(2);
        let byte_ptr = arena
            .layout
            .malloc(byte_size, MTag::ByteArray, |_| {})
            .unwrap();
        let byte_header = ByteArrayHeader::new(2, false);
        unsafe {
            write_header(byte_ptr, MbHeader::from(byte_header));
        }
        let byte_val = value_from_ptr(byte_ptr);

        let mut cache = [StringPosCacheEntry::new(byte_val, 0, 0); JS_STRING_POS_CACHE_SIZE];
        let mut gc_roots = GcRoots::new(&[], &[]).with_string_pos_cache(&mut cache);
        gc_mark_all(&arena.layout, &mut gc_roots, GcMarkConfig::default());

        assert_eq!(cache[0].str(), JS_NULL);
        assert_eq!(cache[1].str(), JS_NULL);
    }

    #[test]
    fn sweep_coalesces_unmarked_blocks_and_calls_finalizer() {
        static CALLS: AtomicUsize = AtomicUsize::new(0);
        static LAST_OPAQUE: AtomicUsize = AtomicUsize::new(0);

        unsafe extern "C" fn record_finalizer(_ctx: *mut JSContext, opaque: *mut c_void) {
            CALLS.fetch_add(1, Ordering::SeqCst);
            LAST_OPAQUE.store(opaque as usize, Ordering::SeqCst);
        }

        let mut arena = HeapArena::new(256, 16);

        let live_size = crate::containers::byte_array_alloc_size(3);
        let live_ptr = arena
            .layout
            .malloc(live_size, MTag::ByteArray, |_| {})
            .unwrap();
        let live_header = ByteArrayHeader::new(3, true);
        unsafe {
            // SAFETY: `live_ptr` comes from the heap allocator and is writable.
            write_header(live_ptr, MbHeader::from(live_header));
        }

        let obj_size = Object::PAYLOAD_OFFSET + JSW as usize;
        let obj_ptr = arena
            .layout
            .malloc(obj_size, MTag::Object, |_| {})
            .unwrap();
        let obj_header = ObjectHeader::new(JSObjectClass::User as u8, 1, false);
        unsafe {
            // SAFETY: `obj_ptr` comes from the heap allocator and is writable.
            write_header(obj_ptr, obj_header.header());
        }
        let obj = obj_ptr.as_ptr() as *mut Object;
        let mut opaque_byte = 0u8;
        let opaque_ptr = core::ptr::addr_of_mut!(opaque_byte) as *mut c_void;
        unsafe {
            // SAFETY: object fields are within the allocated object payload.
            *Object::proto_ptr(obj) = JS_NULL;
            *Object::props_ptr(obj) = JS_NULL;
            let payload = Object::payload_ptr(obj);
            let user = core::ptr::addr_of_mut!((*payload).user);
            *user = ObjectUserData::new(opaque_ptr);
        }

        let dead_size = crate::containers::byte_array_alloc_size(2);
        let dead_ptr = arena
            .layout
            .malloc(dead_size, MTag::ByteArray, |_| {})
            .unwrap();
        let dead_header = ByteArrayHeader::new(2, false);
        unsafe {
            // SAFETY: `dead_ptr` comes from the heap allocator and is writable.
            write_header(dead_ptr, MbHeader::from(dead_header));
        }
        let obj_block_size = unsafe {
            // SAFETY: `obj_ptr` points at a valid memblock header.
            crate::heap::mblock_size(obj_ptr)
        };
        let dead_block_size = unsafe {
            // SAFETY: `dead_ptr` points at a valid memblock header.
            crate::heap::mblock_size(dead_ptr)
        };

        let mut ctx_byte = 0u8;
        let ctx_ptr = NonNull::new(&mut ctx_byte as *mut u8)
            .unwrap()
            .cast::<JSContext>();
        let finalizers: [Option<JSCFinalizer>; 1] = [Some(record_finalizer)];
        let finalizer_table = GcSweepFinalizers::new(ctx_ptr, finalizers.as_slice());

        unsafe {
            // SAFETY: the heap arena has valid, contiguous memblocks.
            gc_sweep(&mut arena.layout, Some(&finalizer_table));
        }

        let live_word = unsafe {
            // SAFETY: `live_ptr` points to a readable header word.
            ptr::read_unaligned(live_ptr.as_ptr().cast::<JSWord>())
        };
        let live_header = MbHeader::from_word(live_word);
        assert_eq!(live_header.tag(), MTag::ByteArray);
        assert!(!live_header.gc_mark());

        let free_word = unsafe {
            // SAFETY: `obj_ptr` points to a readable header word.
            ptr::read_unaligned(obj_ptr.as_ptr().cast::<JSWord>())
        };
        let free_header = MbHeader::from_word(free_word);
        assert_eq!(free_header.tag(), MTag::Free);

        let free_size = unsafe {
            // SAFETY: `obj_ptr` now points to the free block header.
            crate::heap::mblock_size(obj_ptr)
        };
        assert_eq!(free_size, obj_block_size + dead_block_size);

        assert_eq!(CALLS.load(Ordering::SeqCst), 1);
        assert_eq!(LAST_OPAQUE.load(Ordering::SeqCst), opaque_ptr as usize);
    }
}
