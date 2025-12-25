use crate::containers::{ByteArrayHeader, StringHeader, ValueArrayHeader, VarRefHeader};
use crate::enums::JSObjectClass;
use crate::function_bytecode::FunctionBytecode;
use crate::jsvalue::{is_ptr, value_from_ptr, value_to_ptr, JSValue, JSWord, JSW};
use crate::memblock::{FreeBlockHeader, MTag, MbHeader};
use crate::object::{Object, ObjectHeader, RegExp};
use core::mem::size_of;
use core::ptr::{self, NonNull};

pub const JS_STACK_SLACK: u32 = 16;
pub const JS_MIN_FREE_SIZE: u32 = 512;
pub const JS_MIN_CRITICAL_FREE_SIZE: u32 = JS_MIN_FREE_SIZE - 256;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct HeapLayout {
    heap_base: NonNull<u8>,
    heap_free: NonNull<u8>,
    stack_top: NonNull<u8>,
    stack_bottom: NonNull<JSValue>,
    min_free_size: usize,
}

impl HeapLayout {
    pub fn new(
        heap_base: NonNull<u8>,
        heap_free: NonNull<u8>,
        stack_top: NonNull<u8>,
        stack_bottom: NonNull<JSValue>,
        min_free_size: usize,
    ) -> Self {
        Self {
            heap_base,
            heap_free,
            stack_top,
            stack_bottom,
            min_free_size,
        }
    }

    pub fn from_raw(
        heap_base: *mut u8,
        heap_free: *mut u8,
        stack_top: *mut u8,
        stack_bottom: *mut JSValue,
        min_free_size: usize,
    ) -> Option<Self> {
        Some(Self::new(
            NonNull::new(heap_base)?,
            NonNull::new(heap_free)?,
            NonNull::new(stack_top)?,
            NonNull::new(stack_bottom)?,
            min_free_size,
        ))
    }

    pub const fn heap_base(self) -> NonNull<u8> {
        self.heap_base
    }

    pub const fn heap_free(self) -> NonNull<u8> {
        self.heap_free
    }

    pub const fn stack_top(self) -> NonNull<u8> {
        self.stack_top
    }

    pub const fn stack_bottom(self) -> NonNull<JSValue> {
        self.stack_bottom
    }

    pub const fn min_free_size(self) -> usize {
        self.min_free_size
    }

    pub fn set_heap_free(&mut self, heap_free: NonNull<u8>) {
        self.heap_free = heap_free;
    }

    pub fn set_stack_bottom(&mut self, stack_bottom: NonNull<JSValue>) {
        self.stack_bottom = stack_bottom;
    }

    pub fn is_valid(self) -> bool {
        let heap_base = self.heap_base.as_ptr() as usize;
        let heap_free = self.heap_free.as_ptr() as usize;
        let stack_top = self.stack_top.as_ptr() as usize;
        let stack_bottom = self.stack_bottom.as_ptr() as usize;

        heap_base <= heap_free
            && heap_free <= stack_top
            && heap_free <= stack_bottom
            && stack_bottom <= stack_top
    }

    pub fn check_free_mem<F>(&mut self, stack_bottom: NonNull<JSValue>, size: usize, mut gc: F) -> bool
    where
        F: FnMut(&mut Self),
    {
        if self.free_space(stack_bottom) < size + self.min_free_size {
            gc(self);
            if self.free_space(stack_bottom) < size + self.min_free_size {
                return false;
            }
        }
        true
    }

    pub fn malloc<F>(&mut self, size: usize, tag: MTag, mut gc: F) -> Option<NonNull<u8>>
    where
        F: FnMut(&mut Self),
    {
        if size == 0 {
            return None;
        }
        let size = align_up(size, JSW as usize);
        if !self.check_free_mem(self.stack_bottom, size, &mut gc) {
            return None;
        }
        let ptr = self.heap_free.as_ptr();
        let next_ptr = unsafe {
            // SAFETY: `ptr` is within the heap layout and `size` fits the free space.
            ptr.add(size)
        };
        self.heap_free = unsafe {
            // SAFETY: `next_ptr` stays within the heap layout and is non-null.
            NonNull::new_unchecked(next_ptr)
        };
        unsafe {
            // SAFETY: `ptr` points into the heap and we just reserved `size` bytes for it.
            write_header_word(ptr, MbHeader::new(tag, false).word());
        }
        Some(unsafe {
            // SAFETY: `ptr` is non-null and points into the heap.
            NonNull::new_unchecked(ptr)
        })
    }

    pub fn mallocz<F>(&mut self, size: usize, tag: MTag, mut gc: F) -> Option<NonNull<u8>>
    where
        F: FnMut(&mut Self),
    {
        let ptr = self.malloc(size, tag, &mut gc)?;
        if size > size_of::<u32>() {
            unsafe {
                // SAFETY: allocation returned a valid block of at least `size` bytes.
                ptr::write_bytes(
                    ptr.as_ptr().add(size_of::<u32>()),
                    0,
                    size - size_of::<u32>(),
                );
            }
        }
        Some(ptr)
    }

    /// # Safety
    /// `ptr` must be a valid memblock start within this heap layout.
    pub unsafe fn free_last(&mut self, ptr: NonNull<u8>) {
        let end = unsafe { ptr.as_ptr().add(mblock_size(ptr)) };
        if end == self.heap_free.as_ptr() {
            self.heap_free = ptr;
        }
    }

    /// # Safety
    /// `ptr` must be a valid memblock start within this heap layout.
    pub unsafe fn shrink(&mut self, ptr: NonNull<u8>, new_size: usize) -> Option<NonNull<u8>> {
        let new_size = align_up(new_size, JSW as usize);
        if new_size == 0 {
            unsafe {
                self.free_last(ptr);
            }
            return None;
        }
        let old_size = unsafe { mblock_size(ptr) };
        debug_assert!(new_size <= old_size);
        let diff = old_size - new_size;
        if diff == 0 {
            return Some(ptr);
        }
        unsafe {
            let free_ptr = ptr.as_ptr().add(new_size);
            set_free_block(
                NonNull::new_unchecked(free_ptr),
                diff,
            );
        }
        Some(ptr)
    }

    fn free_space(&self, stack_bottom: NonNull<JSValue>) -> usize {
        let heap_free = self.heap_free.as_ptr() as usize;
        let stack_bottom = stack_bottom.as_ptr() as usize;
        stack_bottom.saturating_sub(heap_free)
    }
}

fn align_up(size: usize, align: usize) -> usize {
    debug_assert!(align.is_power_of_two());
    (size + align - 1) & !(align - 1)
}

unsafe fn read_header_word(ptr: *const u8) -> JSWord {
    // SAFETY: caller guarantees `ptr` points to a readable header word.
    unsafe { ptr::read_unaligned(ptr.cast::<JSWord>()) }
}

unsafe fn write_header_word(ptr: *mut u8, word: JSWord) {
    // SAFETY: caller guarantees `ptr` points to writable header word storage.
    unsafe { ptr::write_unaligned(ptr.cast::<JSWord>(), word) }
}

/// # Safety
/// `ptr` must be writable for at least `size` bytes within a heap buffer.
pub unsafe fn set_free_block(ptr: NonNull<u8>, size: usize) {
    debug_assert!(size >= size_of::<JSWord>());
    debug_assert_eq!(size % (JSW as usize), 0);
    let size_words = (size - size_of::<JSWord>()) / (JSW as usize);
    let header = FreeBlockHeader::new(size_words as JSWord, false);
    unsafe {
        // SAFETY: caller guarantees `ptr` is writable for at least `size` bytes.
        write_header_word(ptr.as_ptr(), MbHeader::from(header).word());
    }
}

/// # Safety
/// `ptr` must point to a readable memblock header.
pub unsafe fn mblock_tag(ptr: NonNull<u8>) -> MTag {
    let word = unsafe { read_header_word(ptr.as_ptr()) };
    MbHeader::from_word(word).tag()
}

/// # Safety
/// `ptr` must point to a valid memblock header within a heap buffer.
pub unsafe fn mblock_size(ptr: NonNull<u8>) -> usize {
    let word = unsafe { read_header_word(ptr.as_ptr()) };
    let header = MbHeader::from_word(word);
    match header.tag() {
        MTag::Object => {
            let obj_header = ObjectHeader::from_word(word);
            Object::PAYLOAD_OFFSET + (obj_header.extra_size() as usize) * (JSW as usize)
        }
        MTag::Float64 => size_of::<JSWord>() + size_of::<f64>(),
        MTag::String => {
            let header = StringHeader::from(header);
            let len = header.len() as usize;
            size_of::<JSWord>() + align_up(len + 1, JSW as usize)
        }
        MTag::ByteArray => {
            let header = ByteArrayHeader::from(header);
            let len = header.size() as usize;
            size_of::<JSWord>() + align_up(len, JSW as usize)
        }
        MTag::ValueArray => {
            let header = ValueArrayHeader::from(header);
            size_of::<JSWord>() + (header.size() as usize) * size_of::<JSValue>()
        }
        MTag::Free => {
            let header = FreeBlockHeader::from(header);
            size_of::<JSWord>() + (header.size() as usize) * (JSW as usize)
        }
        MTag::VarRef => {
            let header = VarRefHeader::from(header);
            let base = size_of::<JSWord>() + size_of::<JSValue>() + size_of::<*mut JSValue>();
            if header.is_detached() {
                base - size_of::<JSValue>()
            } else {
                base
            }
        }
        MTag::FunctionBytecode => size_of::<FunctionBytecode>(),
    }
}

/// # Safety
/// `pval` must be a valid pointer to a JSValue slot.
pub unsafe fn thread_pointer(pval: *mut JSValue) {
    let val = unsafe { *pval };
    if !is_ptr(val) {
        return;
    }
    let ptr = match value_to_ptr::<u8>(val) {
        Some(ptr) => ptr,
        None => return,
    };
    let header_ptr = ptr.as_ptr() as *mut JSValue;
    unsafe {
        let prev = *header_ptr;
        *pval = prev;
        let pval_ptr = NonNull::new_unchecked(pval);
        *header_ptr = value_from_ptr(pval_ptr);
    }
}

/// # Safety
/// `ptr` must point to a memblock header threaded by `thread_pointer`.
pub unsafe fn update_threaded_pointers(ptr: *mut u8, new_ptr: *mut u8) {
    let header_ptr = ptr as *mut JSValue;
    let mut val = unsafe { *header_ptr };
    while let Some(pv) = value_to_ptr::<JSValue>(val) {
        let next_val = unsafe { *pv.as_ptr() };
        let new_ptr = NonNull::new(new_ptr).expect("new_ptr should be non-null");
        unsafe {
            *pv.as_ptr() = value_from_ptr(new_ptr);
        }
        val = next_val;
    }
    unsafe {
        *header_ptr = val;
    }
}

/// # Safety
/// `ptr` must point to a readable memblock start.
pub unsafe fn thread_block(ptr: *mut u8) {
    let header = unsafe { MbHeader::from_word(read_header_word(ptr)) };
    match header.tag() {
        MTag::Object => unsafe { thread_object(ptr) },
        MTag::ValueArray => unsafe { thread_value_array(ptr, header) },
        MTag::VarRef => unsafe { thread_var_ref(ptr) },
        MTag::FunctionBytecode => unsafe { thread_function_bytecode(ptr) },
        _ => {}
    }
}

unsafe fn thread_value_array(ptr: *mut u8, header: MbHeader) {
    let header = ValueArrayHeader::from(header);
    let len = header.size() as usize;
    let arr_ptr = unsafe { ptr.add(size_of::<JSWord>()) as *mut JSValue };
    for idx in 0..len {
        unsafe {
            thread_pointer(arr_ptr.add(idx));
        }
    }
}

unsafe fn thread_var_ref(ptr: *mut u8) {
    let val_ptr = unsafe { ptr.add(size_of::<JSWord>()) as *mut JSValue };
    unsafe {
        thread_pointer(val_ptr);
    }
}

unsafe fn thread_function_bytecode(ptr: *mut u8) {
    let func = ptr as *mut FunctionBytecode;
    unsafe {
        thread_pointer(FunctionBytecode::func_name_ptr(func));
        thread_pointer(FunctionBytecode::byte_code_ptr(func));
        thread_pointer(FunctionBytecode::cpool_ptr(func));
        thread_pointer(FunctionBytecode::vars_ptr(func));
        thread_pointer(FunctionBytecode::ext_vars_ptr(func));
        thread_pointer(FunctionBytecode::filename_ptr(func));
        thread_pointer(FunctionBytecode::pc2line_ptr(func));
    }
}

unsafe fn thread_object(ptr: *mut u8) {
    let obj = ptr as *mut Object;
    let header_word = unsafe { read_header_word(ptr) };
    let header = ObjectHeader::from_word(header_word);
    unsafe {
        thread_pointer(Object::proto_ptr(obj));
        thread_pointer(Object::props_ptr(obj));
    }
    let class_id = header.class_id();
    match class_id {
        x if x == JSObjectClass::Closure as u8 => unsafe {
            let payload = Object::payload_ptr(obj);
            let closure = core::ptr::addr_of_mut!((*payload).closure);
            thread_pointer(crate::closure_data::ClosureData::func_bytecode_ptr(closure));
            if header.extra_size() > 1 {
                let var_refs = crate::closure_data::ClosureData::var_refs_ptr(closure);
                let count = (header.extra_size() - 1) as usize;
                for idx in 0..count {
                    thread_pointer(var_refs.add(idx));
                }
            }
        },
        x if x == JSObjectClass::CFunction as u8 => unsafe {
            if header.extra_size() > 1 {
                let payload = Object::payload_ptr(obj);
                let cfunc = core::ptr::addr_of_mut!((*payload).cfunc);
                thread_pointer(crate::cfunction_data::CFunctionData::params_ptr(cfunc));
            }
        },
        x if x == JSObjectClass::Array as u8 => unsafe {
            let payload = Object::payload_ptr(obj);
            let array = core::ptr::addr_of_mut!((*payload).array);
            thread_pointer(crate::array_data::ArrayData::tab_ptr(array));
        },
        x if x == JSObjectClass::Error as u8 => unsafe {
            let payload = Object::payload_ptr(obj);
            let error = core::ptr::addr_of_mut!((*payload).error);
            thread_pointer(crate::error_data::ErrorData::message_ptr(error));
            thread_pointer(crate::error_data::ErrorData::stack_ptr(error));
        },
        x if x == JSObjectClass::ArrayBuffer as u8 => unsafe {
            let payload = Object::payload_ptr(obj);
            let buffer = core::ptr::addr_of_mut!((*payload).array_buffer);
            thread_pointer(crate::array_buffer::ArrayBuffer::byte_buffer_ptr(buffer));
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
            thread_pointer(crate::typed_array::TypedArray::buffer_ptr(array));
        },
        x if x == JSObjectClass::RegExp as u8 => unsafe {
            let payload = Object::payload_ptr(obj);
            let regexp = core::ptr::addr_of_mut!((*payload).regexp);
            thread_pointer(RegExp::source_ptr(regexp));
            thread_pointer(RegExp::byte_code_ptr(regexp));
        },
        _ => {}
    }
}

/// # Safety
/// All roots and heap memblocks must be valid and internally consistent.
pub unsafe fn compact_heap<R>(heap: &mut HeapLayout, roots: &mut R)
where
    R: RootVisitor,
{
    roots.visit_roots(|root| unsafe {
        thread_pointer(root);
    });

    let mut new_ptr = heap.heap_base.as_ptr();
    let mut ptr = heap.heap_base.as_ptr();
    while ptr < heap.heap_free.as_ptr() {
        unsafe {
            update_threaded_pointers(ptr, new_ptr);
        }
        let size = unsafe { mblock_size(NonNull::new_unchecked(ptr)) };
        if unsafe { mblock_tag(NonNull::new_unchecked(ptr)) } != MTag::Free {
            unsafe {
                thread_block(ptr);
            }
            new_ptr = unsafe { new_ptr.add(size) };
        }
        ptr = unsafe { ptr.add(size) };
    }

    new_ptr = heap.heap_base.as_ptr();
    ptr = heap.heap_base.as_ptr();
    while ptr < heap.heap_free.as_ptr() {
        unsafe {
            update_threaded_pointers(ptr, new_ptr);
        }
        let size = unsafe { mblock_size(NonNull::new_unchecked(ptr)) };
        if unsafe { mblock_tag(NonNull::new_unchecked(ptr)) } != MTag::Free {
            if new_ptr != ptr {
                unsafe {
                    // SAFETY: ranges are within the heap and may overlap.
                    ptr::copy(ptr, new_ptr, size);
                }
            }
            new_ptr = unsafe { new_ptr.add(size) };
        }
        ptr = unsafe { ptr.add(size) };
    }
    heap.heap_free = unsafe {
        // SAFETY: new_ptr stays within heap bounds.
        NonNull::new_unchecked(new_ptr)
    };
}

pub trait RootVisitor {
    fn visit_roots<F>(&mut self, f: F)
    where
        F: FnMut(*mut JSValue);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::containers::{ByteArrayHeader, StringHeader, ValueArrayHeader, VarRefHeader};
    use crate::memblock::{Float64Header, MTag};
    use core::mem::size_of;

    struct HeapArena {
        _storage: Vec<JSWord>,
        layout: HeapLayout,
    }

    impl HeapArena {
        fn new(words: usize, stack_words: usize, min_free_size: usize) -> Self {
            let mut storage = vec![0 as JSWord; words];
            let base = NonNull::new(storage.as_mut_ptr() as *mut u8).unwrap();
            let stack_top = unsafe { NonNull::new_unchecked(base.as_ptr().add(words * JSW as usize)) };
            let stack_bottom = unsafe {
                NonNull::new_unchecked(
                    stack_top
                        .as_ptr()
                        .sub(stack_words * JSW as usize) as *mut JSValue,
                )
            };
            let layout = HeapLayout::new(base, base, stack_top, stack_bottom, min_free_size);
            Self {
                _storage: storage,
                layout,
            }
        }
    }

    struct RootSlice<'a> {
        roots: &'a mut [JSValue],
    }

    impl<'a> RootVisitor for RootSlice<'a> {
        fn visit_roots<F>(&mut self, mut f: F)
        where
            F: FnMut(*mut JSValue),
        {
            for root in &mut *self.roots {
                f(root as *mut JSValue);
            }
        }
    }

    #[test]
    fn alloc_aligns_and_sets_header() {
        let mut arena = HeapArena::new(64, 8, 0);
        let ptr = arena.layout.malloc(3, MTag::ByteArray, |_| {}).unwrap();
        let base = arena.layout.heap_base().as_ptr() as usize;
        let ptr_addr = ptr.as_ptr() as usize;
        assert_eq!((ptr_addr - base) % (JSW as usize), 0);
        let header = unsafe { MbHeader::from_word(read_header_word(ptr.as_ptr())) };
        assert_eq!(header.tag(), MTag::ByteArray);
        assert!(!header.gc_mark());
        let expected_free = unsafe { ptr.as_ptr().add(JSW as usize) };
        assert_eq!(arena.layout.heap_free().as_ptr(), expected_free);
    }

    #[test]
    fn free_last_only_updates_tail() {
        let mut arena = HeapArena::new(64, 8, 0);
        let first = arena.layout.malloc(JSW as usize, MTag::Free, |_| {}).unwrap();
        let second = arena.layout.malloc(JSW as usize, MTag::Free, |_| {}).unwrap();
        let heap_free = arena.layout.heap_free().as_ptr();
        unsafe {
            arena.layout.free_last(first);
        }
        assert_eq!(arena.layout.heap_free().as_ptr(), heap_free);
        unsafe {
            arena.layout.free_last(second);
        }
        assert_eq!(arena.layout.heap_free().as_ptr(), second.as_ptr());
    }

    #[test]
    fn shrink_sets_free_block() {
        let mut arena = HeapArena::new(64, 8, 0);
        let ptr = arena.layout.malloc(JSW as usize * 3, MTag::Object, |_| {}).unwrap();
        let new_size = JSW as usize * 2;
        unsafe {
            arena.layout.shrink(ptr, new_size);
        }
        let free_ptr = unsafe { ptr.as_ptr().add(new_size) };
        let header = unsafe { MbHeader::from_word(read_header_word(free_ptr)) };
        assert_eq!(header.tag(), MTag::Free);
    }

    #[test]
    fn mblock_size_matches_tag_layouts() {
        let mut arena = HeapArena::new(128, 16, 0);
        let obj = arena.layout.malloc(JSW as usize * 4, MTag::Object, |_| {}).unwrap();
        let obj_header = ObjectHeader::new(JSObjectClass::Object as u8, 3, false);
        unsafe {
            write_header_word(obj.as_ptr(), obj_header.header().word());
        }
        let obj_size = unsafe { mblock_size(obj) };
        assert_eq!(obj_size, Object::PAYLOAD_OFFSET + 3 * (JSW as usize));

        let float_ptr = arena.layout.malloc(JSW as usize * 2, MTag::Float64, |_| {}).unwrap();
        let float_header = Float64Header::new(false);
        unsafe {
            write_header_word(float_ptr.as_ptr(), MbHeader::from(float_header).word());
        }
        assert_eq!(unsafe { mblock_size(float_ptr) }, size_of::<JSWord>() + size_of::<f64>());

        let string_ptr = arena.layout.malloc(JSW as usize * 3, MTag::String, |_| {}).unwrap();
        let string_header = StringHeader::new(3, false, false, false, false);
        unsafe {
            write_header_word(string_ptr.as_ptr(), MbHeader::from(string_header).word());
        }
        assert_eq!(
            unsafe { mblock_size(string_ptr) },
            size_of::<JSWord>() + align_up(4, JSW as usize)
        );

        let byte_ptr = arena.layout.malloc(JSW as usize * 2, MTag::ByteArray, |_| {}).unwrap();
        let byte_header = ByteArrayHeader::new(7, false);
        unsafe {
            write_header_word(byte_ptr.as_ptr(), MbHeader::from(byte_header).word());
        }
        assert_eq!(
            unsafe { mblock_size(byte_ptr) },
            size_of::<JSWord>() + align_up(7, JSW as usize)
        );

        let value_ptr = arena.layout.malloc(JSW as usize * 5, MTag::ValueArray, |_| {}).unwrap();
        let value_header = ValueArrayHeader::new(3, false);
        unsafe {
            write_header_word(value_ptr.as_ptr(), MbHeader::from(value_header).word());
        }
        assert_eq!(
            unsafe { mblock_size(value_ptr) },
            size_of::<JSWord>() + 3 * size_of::<JSValue>()
        );

        let free_ptr = arena.layout.malloc(JSW as usize * 3, MTag::Free, |_| {}).unwrap();
        unsafe {
            set_free_block(free_ptr, JSW as usize * 3);
        }
        assert_eq!(
            unsafe { mblock_size(free_ptr) },
            size_of::<JSWord>() + 2 * (JSW as usize)
        );

        let var_ptr = arena.layout.malloc(JSW as usize * 3, MTag::VarRef, |_| {}).unwrap();
        let var_header = VarRefHeader::new(true, false);
        unsafe {
            write_header_word(var_ptr.as_ptr(), MbHeader::from(var_header).word());
        }
        let base = size_of::<JSWord>() + size_of::<JSValue>() + size_of::<*mut JSValue>();
        assert_eq!(unsafe { mblock_size(var_ptr) }, base - size_of::<JSValue>());
    }

    #[test]
    fn compact_moves_live_blocks_and_updates_roots() {
        let mut arena = HeapArena::new(256, 16, 0);
        let array_size = size_of::<JSWord>() + size_of::<JSValue>();
        let array_ptr = arena.layout.malloc(array_size, MTag::ValueArray, |_| {}).unwrap();
        let array_header = ValueArrayHeader::new(1, false);
        unsafe {
            write_header_word(array_ptr.as_ptr(), MbHeader::from(array_header).word());
        }
        let free_ptr = arena.layout.malloc(JSW as usize * 2, MTag::Free, |_| {}).unwrap();
        unsafe {
            set_free_block(free_ptr, JSW as usize * 2);
        }
        let target_ptr = arena.layout.malloc(JSW as usize * 2, MTag::ByteArray, |_| {}).unwrap();
        let target_header = ByteArrayHeader::new(4, false);
        unsafe {
            write_header_word(target_ptr.as_ptr(), MbHeader::from(target_header).word());
        }
        let arr_payload = unsafe { array_ptr.as_ptr().add(size_of::<JSWord>()) as *mut JSValue };
        unsafe {
            let target_val = value_from_ptr(NonNull::new_unchecked(target_ptr.as_ptr()));
            *arr_payload = target_val;
        }
        let mut roots = [unsafe { value_from_ptr(NonNull::new_unchecked(array_ptr.as_ptr())) }];
        let mut root_slice = RootSlice { roots: &mut roots };
        unsafe {
            compact_heap(&mut arena.layout, &mut root_slice);
        }
        let new_array_ptr = match value_to_ptr::<u8>(roots[0]) {
            Some(ptr) => ptr,
            None => panic!("root not updated"),
        };
        assert_eq!(new_array_ptr.as_ptr(), arena.layout.heap_base().as_ptr());
        let new_arr_payload =
            unsafe { new_array_ptr.as_ptr().add(size_of::<JSWord>()) as *mut JSValue };
        let updated = unsafe { *new_arr_payload };
        let updated_ptr = value_to_ptr::<u8>(updated).expect("array element updated");
        let expected_target = unsafe { arena.layout.heap_base().as_ptr().add(array_size) };
        assert_eq!(updated_ptr.as_ptr(), expected_target);
        assert_eq!(
            arena.layout.heap_free().as_ptr(),
            unsafe { expected_target.add(JSW as usize * 2) }
        );
    }
}
