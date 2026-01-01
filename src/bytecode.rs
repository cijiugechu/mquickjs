use crate::containers::{StringHeader, ValueArrayHeader};
use crate::function_bytecode::FunctionBytecode;
use crate::heap::mblock_size;
use crate::jsvalue::{JSValue, JSWord};
use crate::memblock::{MbHeader, MTag};
use crate::stdlib::stdlib_def::{BytecodeHeader, JS_BYTECODE_MAGIC, JS_BYTECODE_VERSION};
use core::mem::size_of;
use core::ptr;

#[cfg(target_pointer_width = "64")]
use crate::gc::GcMarkConfig;
#[cfg(target_pointer_width = "64")]
use crate::gc_runtime::GcRuntimeRoots;
#[cfg(target_pointer_width = "64")]
use crate::heap::{mblock_tag, thread_block, thread_pointer, HeapLayout, RootVisitor};
#[cfg(target_pointer_width = "64")]
use crate::containers::ByteArrayHeader;
#[cfg(target_pointer_width = "64")]
use crate::memblock::MbHeader as MbHeader64;
#[cfg(target_pointer_width = "64")]
use crate::stdlib::stdlib_def::{BytecodeHeader32, JS_BYTECODE_VERSION_32};
#[cfg(target_pointer_width = "64")]
use core::mem::size_of as size_of_32;
#[cfg(target_pointer_width = "64")]
use core::ptr::NonNull;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BytecodeRelocError {
    BufferTooSmall,
    InvalidMagic,
    InvalidVersion,
}

#[cfg(target_pointer_width = "64")]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BytecodePrepareError {
    OutOfMemory,
    StringTooLong,
    HeapTooLarge,
    UnsupportedTag(MTag),
}

#[cfg(target_pointer_width = "64")]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct BytecodeImage32 {
    pub header: BytecodeHeader32,
    pub len: usize,
}

pub trait BytecodeAtomResolver {
    fn resolve_unique_string(&mut self, val: JSValue) -> Option<JSValue>;
}

#[cfg(target_pointer_width = "64")]
/// # Safety
/// The heap layout must describe a valid contiguous heap, and `roots` must
/// cover all live JSValue slots, including `unique_strings` and `main_func`.
/// After success, the heap buffer is rewritten in 32-bit layout and should
/// not be used as a 64-bit heap again.
pub unsafe fn prepare_bytecode_64to32(
    heap: &mut HeapLayout,
    roots: &mut GcRuntimeRoots<'_>,
    unique_strings: *mut JSValue,
    main_func: *mut JSValue,
    mark_config: GcMarkConfig,
) -> Result<BytecodeImage32, BytecodePrepareError> {
    let mark_config = GcMarkConfig {
        keep_atoms: false,
        ..mark_config
    };
    let mut mark_roots = roots.mark_roots();

    crate::gc::gc_mark_all(heap, &mut mark_roots, mark_config);
    unsafe {
        // SAFETY: caller guarantees heap layout is valid for sweeping.
        crate::gc::gc_sweep(heap, None);
    }

    #[cfg(target_pointer_width = "64")]
    {
        expand_short_floats(heap)?;
    }

    let len = unsafe {
        // SAFETY: caller guarantees roots and heap are valid for conversion.
        compact_heap_64to32(heap, roots)?
    };
    let unique_strings = unsafe {
        // SAFETY: caller guarantees unique_strings points at a live JSValue slot.
        ptr::read_unaligned(unique_strings)
    };
    let main_func = unsafe {
        // SAFETY: caller guarantees main_func points at a live JSValue slot.
        ptr::read_unaligned(main_func)
    };
    let header = BytecodeHeader32 {
        magic: JS_BYTECODE_MAGIC,
        version: JS_BYTECODE_VERSION_32,
        base_addr: 0,
        unique_strings: jsvalue_to_u32(unique_strings)?,
        main_func: jsvalue_to_u32(main_func)?,
    };
    Ok(BytecodeImage32 { header, len })
}

#[cfg(target_pointer_width = "64")]
const WORD_32: usize = size_of_32::<u32>();

#[cfg(target_pointer_width = "64")]
const JS_STRING_LEN_MAX_32: JSWord = ((1u32 << (32 - crate::memblock::JS_MTAG_BITS - 3)) - 1) as JSWord;

#[cfg(target_pointer_width = "64")]
#[repr(C)]
struct FunctionBytecode32 {
    header: u32,
    func_name: u32,
    byte_code: u32,
    cpool: u32,
    vars: u32,
    ext_vars: u32,
    stack_size: u16,
    ext_vars_len: u16,
    filename: u32,
    pc2line: u32,
    source_pos: u32,
}

#[cfg(target_pointer_width = "64")]
fn align_up_32(size: usize) -> usize {
    let align = WORD_32;
    debug_assert!(align.is_power_of_two());
    (size + align - 1) & !(align - 1)
}

#[cfg(target_pointer_width = "64")]
fn jsvalue_to_u32(val: JSValue) -> Result<u32, BytecodePrepareError> {
    let bits = val.raw_bits();
    if bits > u32::MAX as JSWord {
        return Err(BytecodePrepareError::HeapTooLarge);
    }
    Ok(bits as u32)
}

#[cfg(target_pointer_width = "64")]
fn mblock_size_32(ptr: *const u8) -> Result<usize, BytecodePrepareError> {
    let header_word = unsafe {
        // SAFETY: caller provides a readable memblock header pointer.
        ptr::read_unaligned(ptr.cast::<JSWord>())
    };
    let header = MbHeader64::from_word(header_word);
    match header.tag() {
        MTag::FunctionBytecode => Ok(size_of_32::<FunctionBytecode32>()),
        MTag::Float64 => Ok(WORD_32 + size_of_32::<f64>()),
        MTag::ValueArray => {
            let header = ValueArrayHeader::from(header);
            Ok(WORD_32 + (header.size() as usize) * WORD_32)
        }
        MTag::ByteArray => {
            let header = ByteArrayHeader::from(header);
            Ok(WORD_32 + align_up_32(header.size() as usize))
        }
        MTag::String => {
            let header = StringHeader::from(header);
            if header.len() > JS_STRING_LEN_MAX_32 {
                return Err(BytecodePrepareError::StringTooLong);
            }
            Ok(WORD_32 + align_up_32(header.len() as usize + 1))
        }
        tag => Err(BytecodePrepareError::UnsupportedTag(tag)),
    }
}

#[cfg(target_pointer_width = "64")]
fn update_threaded_pointers_64to32(ptr: *mut u8, new_offset: usize) -> Result<(), BytecodePrepareError> {
    if new_offset > (u32::MAX as usize).saturating_sub(JSValue::JS_TAG_PTR as usize) {
        return Err(BytecodePrepareError::HeapTooLarge);
    }
    let new_bits = (new_offset as JSWord) | (JSValue::JS_TAG_PTR as JSWord);
    let header_ptr = ptr as *mut JSValue;
    let mut val = unsafe {
        // SAFETY: caller guarantees `ptr` is readable as a JSValue header.
        *header_ptr
    };
    while let Some(pv) = val.to_ptr::<JSValue>() {
        let next_val = unsafe {
            // SAFETY: threaded pointer points at a valid JSValue slot.
            *pv.as_ptr()
        };
        unsafe {
            // SAFETY: `pv` points at a writable JSValue slot.
            *pv.as_ptr() = JSValue::from_bits(new_bits);
        }
        val = next_val;
    }
    unsafe {
        // SAFETY: caller guarantees `ptr` is writable for a JSValue header.
        *header_ptr = val;
    }
    Ok(())
}

#[cfg(target_pointer_width = "64")]
fn convert_mblock_64to32(dst: *mut u8, src: *const u8) -> Result<(), BytecodePrepareError> {
    let header_word = unsafe {
        // SAFETY: caller guarantees `src` points at a readable memblock header.
        ptr::read_unaligned(src.cast::<JSWord>())
    };
    let header = MbHeader64::from_word(header_word);
    match header.tag() {
        MTag::FunctionBytecode => {
            let func = unsafe {
                // SAFETY: caller guarantees `src` points at a valid JSFunctionBytecode.
                ptr::read_unaligned(src.cast::<FunctionBytecode>())
            };
            let header = func.header().header().word() as u32;
            let func32 = FunctionBytecode32 {
                header,
                func_name: jsvalue_to_u32(func.func_name())?,
                byte_code: jsvalue_to_u32(func.byte_code())?,
                cpool: jsvalue_to_u32(func.cpool())?,
                vars: jsvalue_to_u32(func.vars())?,
                ext_vars: jsvalue_to_u32(func.ext_vars())?,
                stack_size: func.stack_size(),
                ext_vars_len: func.ext_vars_len(),
                filename: jsvalue_to_u32(func.filename())?,
                pc2line: jsvalue_to_u32(func.pc2line())?,
                source_pos: func.source_pos(),
            };
            unsafe {
                // SAFETY: caller guarantees `dst` is writable for the 32-bit struct.
                ptr::write_unaligned(dst.cast::<FunctionBytecode32>(), func32);
            }
        }
        MTag::Float64 => {
            let header32 = header.word() as u32;
            unsafe {
                // SAFETY: caller guarantees `dst` is writable for a u32 header.
                ptr::write_unaligned(dst.cast::<u32>(), header32);
                let src_payload = src.add(size_of::<JSWord>());
                let dst_payload = dst.add(WORD_32);
                // SAFETY: ranges may overlap; copy preserves payload bytes.
                ptr::copy(src_payload, dst_payload, size_of_32::<f64>());
            }
        }
        MTag::ValueArray => {
            let header32 = header.word() as u32;
            let array_header = ValueArrayHeader::from(header);
            unsafe {
                // SAFETY: caller guarantees `dst` is writable for a u32 header.
                ptr::write_unaligned(dst.cast::<u32>(), header32);
            }
            let src_arr = unsafe { src.add(size_of::<JSWord>()).cast::<JSValue>() };
            let dst_arr = unsafe { dst.add(WORD_32).cast::<u32>() };
            for idx in 0..array_header.size() as usize {
                let val = unsafe {
                    // SAFETY: `src_arr` points within the source array.
                    *src_arr.add(idx)
                };
                let raw = jsvalue_to_u32(val)?;
                unsafe {
                    // SAFETY: `dst_arr` points within the destination array.
                    ptr::write_unaligned(dst_arr.add(idx), raw);
                }
            }
        }
        MTag::ByteArray => {
            let header32 = header.word() as u32;
            let array_header = ByteArrayHeader::from(header);
            unsafe {
                // SAFETY: caller guarantees `dst` is writable for a u32 header.
                ptr::write_unaligned(dst.cast::<u32>(), header32);
                let src_payload = src.add(size_of::<JSWord>());
                let dst_payload = dst.add(WORD_32);
                // SAFETY: ranges may overlap; copy preserves payload bytes.
                ptr::copy(src_payload, dst_payload, array_header.size() as usize);
            }
        }
        MTag::String => {
            let header32 = header.word() as u32;
            let str_header = StringHeader::from(header);
            if str_header.len() > JS_STRING_LEN_MAX_32 {
                return Err(BytecodePrepareError::StringTooLong);
            }
            unsafe {
                // SAFETY: caller guarantees `dst` is writable for a u32 header.
                ptr::write_unaligned(dst.cast::<u32>(), header32);
                let src_payload = src.add(size_of::<JSWord>());
                let dst_payload = dst.add(WORD_32);
                let len_plus_nul = str_header.len() as usize + 1;
                // SAFETY: ranges may overlap; copy preserves string bytes.
                ptr::copy(src_payload, dst_payload, len_plus_nul);
            }
        }
        tag => return Err(BytecodePrepareError::UnsupportedTag(tag)),
    }
    Ok(())
}

#[cfg(target_pointer_width = "64")]
/// # Safety
/// The heap must be valid and `roots` must include all live JSValue slots.
/// On success the heap buffer is rewritten in 32-bit layout and is no longer
/// safe to access with 64-bit heap helpers.
pub unsafe fn compact_heap_64to32<R>(
    heap: &mut HeapLayout,
    roots: &mut R,
) -> Result<usize, BytecodePrepareError>
where
    R: RootVisitor,
{
    roots.visit_roots(|root| unsafe {
        thread_pointer(heap, root);
    });

    let base = heap.heap_base().as_ptr();
    let heap_free = heap.heap_free().as_ptr();

    let mut new_offset = 0usize;
    let mut ptr = base;
    while ptr < heap_free {
        update_threaded_pointers_64to32(ptr, new_offset)?;
        let size = unsafe {
            // SAFETY: `ptr` points within the heap to a memblock header.
            mblock_size(NonNull::new_unchecked(ptr))
        };
        if unsafe { mblock_tag(NonNull::new_unchecked(ptr)) } != MTag::Free {
            unsafe {
                // SAFETY: `ptr` is a live memblock start.
                thread_block(heap, ptr);
            }
            let size_32 = mblock_size_32(ptr)?;
            new_offset = new_offset
                .checked_add(size_32)
                .ok_or(BytecodePrepareError::HeapTooLarge)?;
        }
        ptr = unsafe {
            // SAFETY: `size` is the memblock size at `ptr`.
            ptr.add(size)
        };
    }

    new_offset = 0;
    ptr = base;
    while ptr < heap_free {
        update_threaded_pointers_64to32(ptr, new_offset)?;
        let size = unsafe {
            // SAFETY: `ptr` points within the heap to a memblock header.
            mblock_size(NonNull::new_unchecked(ptr))
        };
        if unsafe { mblock_tag(NonNull::new_unchecked(ptr)) } != MTag::Free {
            let size_32 = mblock_size_32(ptr)?;
            unsafe {
                // SAFETY: destination is within the heap; ranges may overlap.
                convert_mblock_64to32(base.add(new_offset), ptr)?;
            }
            new_offset = new_offset
                .checked_add(size_32)
                .ok_or(BytecodePrepareError::HeapTooLarge)?;
        }
        ptr = unsafe {
            // SAFETY: `size` is the memblock size at `ptr`.
            ptr.add(size)
        };
    }

    let new_free = unsafe {
        // SAFETY: base + new_offset stays within heap bounds.
        NonNull::new_unchecked(base.add(new_offset))
    };
    heap.set_heap_free(new_free);
    Ok(new_offset)
}

#[cfg(target_pointer_width = "64")]
fn expand_short_floats(heap: &mut HeapLayout) -> Result<(), BytecodePrepareError> {
    let mut ptr = heap.heap_base().as_ptr();
    let end = heap.heap_free().as_ptr();

    while ptr < end {
        let size = unsafe {
            // SAFETY: `ptr` points within the heap to a memblock header.
            mblock_size(NonNull::new_unchecked(ptr))
        };
        let header_word = unsafe {
            // SAFETY: `ptr` points to a readable header word.
            ptr::read_unaligned(ptr.cast::<JSWord>())
        };
        let header = MbHeader64::from_word(header_word);
        match header.tag() {
            MTag::FunctionBytecode => {}
            MTag::ValueArray => {
                let header = ValueArrayHeader::from(header);
                let arr_ptr = unsafe { ptr.add(size_of::<JSWord>()).cast::<JSValue>() };
                for idx in 0..header.size() as usize {
                    let slot = unsafe { arr_ptr.add(idx) };
                    let val = unsafe {
                        // SAFETY: `slot` points within the value array.
                        *slot
                    };
                    if val.is_short_float() {
                        let float = val.short_float_to_f64();
                        let size = size_of::<JSWord>() + size_of::<f64>();
                        let float_ptr = heap
                            .malloc(size, MTag::Float64, |_| {})
                            .ok_or(BytecodePrepareError::OutOfMemory)?;
                        let payload = unsafe { float_ptr.as_ptr().add(size_of::<JSWord>()) as *mut f64 };
                        unsafe {
                            // SAFETY: payload points within the newly allocated float64 block.
                            ptr::write_unaligned(payload, float);
                            *slot = JSValue::from_ptr(float_ptr);
                        }
                    }
                }
            }
            MTag::String | MTag::Float64 | MTag::ByteArray | MTag::Free => {}
            tag => return Err(BytecodePrepareError::UnsupportedTag(tag)),
        }
        ptr = unsafe {
            // SAFETY: `size` is the memblock size at `ptr`.
            ptr.add(size)
        };
    }
    Ok(())
}

pub fn is_bytecode(buf: &[u8]) -> bool {
    if buf.len() < size_of::<BytecodeHeader>() {
        return false;
    }
    let header = unsafe {
        // SAFETY: buffer length ensures header storage is readable.
        ptr::read_unaligned(buf.as_ptr().cast::<BytecodeHeader>())
    };
    header.magic == JS_BYTECODE_MAGIC
}

/// # Safety
/// `header` and `data` must describe a valid bytecode heap buffer with
/// contiguous memblocks. All pointer-valued `JSValue`s must be non-null
/// and point within the original heap base recorded in `header`. The
/// `new_base_addr` is the desired base address for pointers in the buffer.
/// It may differ from `data` when producing deterministic bytecode output.
pub unsafe fn relocate_bytecode(
    header: &mut BytecodeHeader,
    data: &mut [u8],
    new_base_addr: usize,
    mut resolver: Option<&mut dyn BytecodeAtomResolver>,
) -> Result<(), BytecodeRelocError> {
    if header.magic != JS_BYTECODE_MAGIC {
        return Err(BytecodeRelocError::InvalidMagic);
    }
    if header.version != JS_BYTECODE_VERSION {
        return Err(BytecodeRelocError::InvalidVersion);
    }

    let data_base = data.as_mut_ptr();
    let data_len = data.len();
    let old_base_addr = header.base_addr;
    relocate_value(
        &mut header.unique_strings,
        old_base_addr,
        new_base_addr,
        data_base,
        data_len,
        &mut resolver,
    );
    relocate_value(
        &mut header.main_func,
        old_base_addr,
        new_base_addr,
        data_base,
        data_len,
        &mut resolver,
    );

    let mut ptr = data_base;
    let end = unsafe { data_base.add(data.len()) };
    while ptr < end {
        let size = unsafe {
            // SAFETY: caller guarantees `ptr` references a valid memblock header.
            mblock_size(core::ptr::NonNull::new_unchecked(ptr))
        };
        let header_word = unsafe {
            // SAFETY: `ptr` points to readable header storage.
            ptr::read_unaligned(ptr.cast::<JSWord>())
        };
        let mb_header = MbHeader::from_word(header_word);
        match mb_header.tag() {
            MTag::FunctionBytecode => unsafe {
                let func = ptr.cast::<FunctionBytecode>();
                relocate_value(
                    FunctionBytecode::func_name_ptr(func),
                    old_base_addr,
                    new_base_addr,
                    data_base,
                    data_len,
                    &mut resolver,
                );
                relocate_value(
                    FunctionBytecode::byte_code_ptr(func),
                    old_base_addr,
                    new_base_addr,
                    data_base,
                    data_len,
                    &mut resolver,
                );
                relocate_value(
                    FunctionBytecode::cpool_ptr(func),
                    old_base_addr,
                    new_base_addr,
                    data_base,
                    data_len,
                    &mut resolver,
                );
                relocate_value(
                    FunctionBytecode::vars_ptr(func),
                    old_base_addr,
                    new_base_addr,
                    data_base,
                    data_len,
                    &mut resolver,
                );
                relocate_value(
                    FunctionBytecode::ext_vars_ptr(func),
                    old_base_addr,
                    new_base_addr,
                    data_base,
                    data_len,
                    &mut resolver,
                );
                relocate_value(
                    FunctionBytecode::filename_ptr(func),
                    old_base_addr,
                    new_base_addr,
                    data_base,
                    data_len,
                    &mut resolver,
                );
                relocate_value(
                    FunctionBytecode::pc2line_ptr(func),
                    old_base_addr,
                    new_base_addr,
                    data_base,
                    data_len,
                    &mut resolver,
                );
            },
            MTag::ValueArray => unsafe {
                let header = ValueArrayHeader::from(mb_header);
                let arr_ptr = ptr.add(size_of::<JSWord>()).cast::<JSValue>();
                for idx in 0..header.size() as usize {
                    relocate_value(
                        arr_ptr.add(idx),
                        old_base_addr,
                        new_base_addr,
                        data_base,
                        data_len,
                        &mut resolver,
                    );
                }
            },
            MTag::String | MTag::Float64 | MTag::ByteArray | MTag::Free | MTag::VarRef
            | MTag::Object => {}
        }
        ptr = unsafe {
            // SAFETY: `size` is the memblock size for `ptr`, so advancing stays in `data`.
            ptr.add(size)
        };
    }

    header.base_addr = new_base_addr;
    Ok(())
}

/// # Safety
/// `buf` must contain a `BytecodeHeader` followed by a valid bytecode heap
/// buffer. Pointer values must be non-null and refer to the original base.
pub unsafe fn relocate_bytecode_in_place(
    buf: &mut [u8],
    resolver: Option<&mut dyn BytecodeAtomResolver>,
) -> Result<(), BytecodeRelocError> {
    if buf.len() < size_of::<BytecodeHeader>() {
        return Err(BytecodeRelocError::BufferTooSmall);
    }
    let header_ptr = buf.as_mut_ptr().cast::<BytecodeHeader>();
    let mut header = unsafe {
        // SAFETY: buffer length ensures header storage is readable.
        ptr::read_unaligned(header_ptr)
    };
    let data = &mut buf[size_of::<BytecodeHeader>()..];
    let new_base_addr = data.as_mut_ptr() as usize;
    unsafe {
        relocate_bytecode(&mut header, data, new_base_addr, resolver)?;
        // SAFETY: header storage is writable and aligned for unaligned writes.
        ptr::write_unaligned(header_ptr, header);
    }
    Ok(())
}

fn relocate_value(
    pval: *mut JSValue,
    old_base_addr: usize,
    new_base_addr: usize,
    data_base: *mut u8,
    data_len: usize,
    resolver: &mut Option<&mut dyn BytecodeAtomResolver>,
) {
    let val = unsafe {
        // SAFETY: caller guarantees `pval` is a valid JSValue slot.
        *pval
    };
    let Some(ptr) = val.to_ptr::<u8>() else {
        return;
    };
    let old_addr = ptr.as_ptr().addr();
    let old_end = old_base_addr.saturating_add(data_len);
    if old_addr < old_base_addr || old_addr >= old_end {
        return;
    }
    let offset = old_addr.wrapping_sub(old_base_addr);
    let new_addr = new_base_addr.wrapping_add(offset);
    let new_ptr = data_base.with_addr(new_addr);
    let mut new_val = JSValue::from_ptr_raw(new_ptr).unwrap_or_else(|| {
        panic!(
            "relocated pointer must be non-null (old_base={old_base_addr:#x}, new_base={new_base_addr:#x}, old_addr={old_addr:#x}, data_len={data_len})"
        )
    });
    if let Some(resolver) = resolver.as_deref_mut() {
        new_val = resolve_unique_string(new_val, resolver);
    }
    unsafe {
        // SAFETY: caller guarantees `pval` is writable.
        *pval = new_val;
    }
}

fn resolve_unique_string(val: JSValue, resolver: &mut dyn BytecodeAtomResolver) -> JSValue {
    let Some(ptr) = val.to_ptr::<u8>() else {
        return val;
    };
    let header_word = unsafe {
        // SAFETY: caller guarantees `val` points to a valid heap memblock.
        ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
    };
    let header = MbHeader::from_word(header_word);
    if header.tag() != MTag::String {
        return val;
    }
    let str_header = StringHeader::from(header);
    if !str_header.is_unique() {
        return val;
    }
    resolver.resolve_unique_string(val).unwrap_or(val)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::containers::string_alloc_size;
    use crate::function_bytecode::{FunctionBytecodeFields, FunctionBytecodeHeader};
    use core::ptr::NonNull;

    fn align_up(size: usize, align: usize) -> usize {
        debug_assert!(align.is_power_of_two());
        (size + align - 1) & !(align - 1)
    }

    fn write_string(ptr: *mut u8, bytes: &[u8], is_unique: bool) {
        let header = StringHeader::new(
            bytes.len() as JSWord,
            is_unique,
            true,
            false,
            false,
        );
        unsafe {
            // SAFETY: caller provides writable storage for the header word.
            ptr::write_unaligned(ptr.cast::<JSWord>(), header.header().word());
            let buf_ptr = ptr.add(size_of::<JSWord>());
            ptr::copy_nonoverlapping(bytes.as_ptr(), buf_ptr, bytes.len());
            *buf_ptr.add(bytes.len()) = 0;
        }
    }

    #[cfg(target_pointer_width = "64")]
    struct HeapArena {
        _storage: Vec<JSWord>,
        layout: HeapLayout,
    }

    #[cfg(target_pointer_width = "64")]
    impl HeapArena {
        fn new(words: usize) -> Self {
            let mut storage = vec![0 as JSWord; words];
            let base = NonNull::new(storage.as_mut_ptr() as *mut u8).unwrap();
            let bytes = words * JSValue::JSW as usize;
            let stack_top = unsafe { NonNull::new_unchecked(base.as_ptr().add(bytes)) };
            let stack_bottom = unsafe {
                // SAFETY: stack_top is non-null and within the allocation.
                NonNull::new_unchecked(stack_top.as_ptr() as *mut JSValue)
            };
            let layout = HeapLayout::new(base, base, stack_top, stack_bottom, 0);
            Self {
                _storage: storage,
                layout,
            }
        }
    }

    #[cfg(target_pointer_width = "64")]
    struct RootSlot {
        slot: *mut JSValue,
    }

    #[cfg(target_pointer_width = "64")]
    impl RootSlot {
        fn new(slot: &mut JSValue) -> Self {
            Self {
                slot: slot as *mut JSValue,
            }
        }
    }

    #[cfg(target_pointer_width = "64")]
    impl RootVisitor for RootSlot {
        fn visit_roots<F>(&mut self, mut f: F)
        where
            F: FnMut(*mut JSValue),
        {
            f(self.slot);
        }
    }

    #[test]
    fn is_bytecode_checks_magic() {
        let mut header = BytecodeHeader {
            magic: JS_BYTECODE_MAGIC,
            version: JS_BYTECODE_VERSION,
            base_addr: 0,
            unique_strings: JSValue::JS_NULL,
            main_func: JSValue::JS_NULL,
        };
        let mut buf = vec![0u8; size_of::<BytecodeHeader>()];
        unsafe {
            // SAFETY: buffer is sized for the header.
            ptr::write_unaligned(buf.as_mut_ptr().cast::<BytecodeHeader>(), header);
        }
        assert!(is_bytecode(&buf));

        header.magic = 0;
        unsafe {
            ptr::write_unaligned(buf.as_mut_ptr().cast::<BytecodeHeader>(), header);
        }
        assert!(!is_bytecode(&buf));
        assert!(!is_bytecode(&buf[..buf.len() - 1]));
    }

    #[test]
    fn relocate_bytecode_updates_pointers() {
        let func_size = size_of::<FunctionBytecode>();
        let array_size = size_of::<JSWord>() + 2 * size_of::<JSValue>();
        let string_size = string_alloc_size(3);
        assert_eq!(func_size % (JSValue::JSW as usize), 0);
        assert_eq!(array_size % (JSValue::JSW as usize), 0);
        assert_eq!(string_size % (JSValue::JSW as usize), 0);

        let total = func_size + array_size + string_size;
        let words = align_up(total, JSValue::JSW as usize) / (JSValue::JSW as usize);
        let mut old_words = vec![0 as JSWord; words];
        let old_base = old_words.as_mut_ptr().cast::<u8>();
        let old_base_addr = old_base as usize;

        let func_ptr = old_base;
        let array_ptr = unsafe { func_ptr.add(func_size) };
        let string_ptr = unsafe { array_ptr.add(array_size) };

        write_string(string_ptr, b"foo", false);

        let array_header = ValueArrayHeader::new(2, false);
        unsafe {
            ptr::write_unaligned(array_ptr.cast::<JSWord>(), array_header.header().word());
            let arr_ptr = array_ptr.add(size_of::<JSWord>()).cast::<JSValue>();
            *arr_ptr.add(0) = JSValue::from_ptr(NonNull::new(string_ptr).unwrap());
            *arr_ptr.add(1) = JSValue::JS_NULL;
        }

        let func_header = FunctionBytecodeHeader::new(false, false, false, 0, false);
        let func_fields = FunctionBytecodeFields {
            func_name: JSValue::from_ptr(NonNull::new(string_ptr).unwrap()),
            byte_code: JSValue::JS_NULL,
            cpool: JSValue::from_ptr(NonNull::new(array_ptr).unwrap()),
            vars: JSValue::JS_NULL,
            ext_vars: JSValue::JS_NULL,
            stack_size: 0,
            ext_vars_len: 0,
            filename: JSValue::JS_NULL,
            pc2line: JSValue::JS_NULL,
            source_pos: 0,
        };
        let func = FunctionBytecode::from_fields(func_header, func_fields);
        unsafe {
            ptr::write_unaligned(func_ptr.cast::<FunctionBytecode>(), func);
        }

        let mut header = BytecodeHeader {
            magic: JS_BYTECODE_MAGIC,
            version: JS_BYTECODE_VERSION,
            base_addr: old_base_addr,
            unique_strings: JSValue::from_ptr(NonNull::new(array_ptr).unwrap()),
            main_func: JSValue::from_ptr(NonNull::new(func_ptr).unwrap()),
        };

        let mut new_words = vec![0 as JSWord; words];
        new_words.copy_from_slice(&old_words);
        let new_base = new_words.as_mut_ptr().cast::<u8>();
        let new_base_addr = new_base as usize;
        let data = unsafe { core::slice::from_raw_parts_mut(new_base, total) };
        unsafe {
            relocate_bytecode(&mut header, data, new_base_addr, None).unwrap();
        }

        let expected_string =
            JSValue::from_ptr(NonNull::new(unsafe { new_base.add(func_size + array_size) }).unwrap());
        let expected_array =
            JSValue::from_ptr(NonNull::new(unsafe { new_base.add(func_size) }).unwrap());
        let expected_func = JSValue::from_ptr(NonNull::new(new_base).unwrap());

        assert_eq!(header.base_addr, new_base_addr);
        assert_eq!(header.unique_strings, expected_array);
        assert_eq!(header.main_func, expected_func);

        let func_after =
            unsafe { ptr::read_unaligned(new_base.cast::<FunctionBytecode>()) };
        assert_eq!(func_after.func_name(), expected_string);
        assert_eq!(func_after.cpool(), expected_array);

        let arr_ptr = unsafe {
            new_base
                .add(func_size + size_of::<JSWord>())
                .cast::<JSValue>()
        };
        assert_eq!(unsafe { *arr_ptr.add(0) }, expected_string);
    }

    #[test]
    fn relocate_bytecode_resolves_unique_strings() {
        struct Resolver {
            target: JSValue,
            replacement: JSValue,
            hits: usize,
        }

        impl BytecodeAtomResolver for Resolver {
            fn resolve_unique_string(&mut self, val: JSValue) -> Option<JSValue> {
                if val == self.target {
                    self.hits += 1;
                    return Some(self.replacement);
                }
                None
            }
        }

        let func_size = size_of::<FunctionBytecode>();
        let array_size = size_of::<JSWord>() + size_of::<JSValue>();
        let string_size = string_alloc_size(3);
        let total = func_size + array_size + string_size;
        let words = align_up(total, JSValue::JSW as usize) / (JSValue::JSW as usize);
        let mut old_words = vec![0 as JSWord; words];
        let old_base = old_words.as_mut_ptr().cast::<u8>();
        let old_base_addr = old_base as usize;

        let func_ptr = old_base;
        let array_ptr = unsafe { func_ptr.add(func_size) };
        let string_ptr = unsafe { array_ptr.add(array_size) };

        write_string(string_ptr, b"bar", true);

        let array_header = ValueArrayHeader::new(1, false);
        unsafe {
            ptr::write_unaligned(array_ptr.cast::<JSWord>(), array_header.header().word());
            let arr_ptr = array_ptr.add(size_of::<JSWord>()).cast::<JSValue>();
            *arr_ptr = JSValue::from_ptr(NonNull::new(string_ptr).unwrap());
        }

        let func_header = FunctionBytecodeHeader::new(false, false, false, 0, false);
        let func_fields = FunctionBytecodeFields {
            func_name: JSValue::from_ptr(NonNull::new(string_ptr).unwrap()),
            byte_code: JSValue::JS_NULL,
            cpool: JSValue::from_ptr(NonNull::new(array_ptr).unwrap()),
            vars: JSValue::JS_NULL,
            ext_vars: JSValue::JS_NULL,
            stack_size: 0,
            ext_vars_len: 0,
            filename: JSValue::JS_NULL,
            pc2line: JSValue::JS_NULL,
            source_pos: 0,
        };
        let func = FunctionBytecode::from_fields(func_header, func_fields);
        unsafe {
            ptr::write_unaligned(func_ptr.cast::<FunctionBytecode>(), func);
        }

        let mut header = BytecodeHeader {
            magic: JS_BYTECODE_MAGIC,
            version: JS_BYTECODE_VERSION,
            base_addr: old_base_addr,
            unique_strings: JSValue::from_ptr(NonNull::new(array_ptr).unwrap()),
            main_func: JSValue::from_ptr(NonNull::new(func_ptr).unwrap()),
        };

        let mut replacement_mem = vec![
            0 as JSWord;
            align_up(string_size, JSValue::JSW as usize) / (JSValue::JSW as usize)
        ];
        let replacement_ptr = replacement_mem.as_mut_ptr().cast::<u8>();
        write_string(replacement_ptr, b"bar", true);
        let replacement_val = JSValue::from_ptr(NonNull::new(replacement_ptr).unwrap());

        let mut new_words = vec![0 as JSWord; words];
        new_words.copy_from_slice(&old_words);
        let new_base = new_words.as_mut_ptr().cast::<u8>();
        let new_base_addr = new_base as usize;
        let new_string_ptr = unsafe { new_base.add(func_size + array_size) };
        let target_val = JSValue::from_ptr(NonNull::new(new_string_ptr).unwrap());
        let mut resolver = Resolver {
            target: target_val,
            replacement: replacement_val,
            hits: 0,
        };
        let data = unsafe { core::slice::from_raw_parts_mut(new_base, total) };
        unsafe {
            relocate_bytecode(&mut header, data, new_base_addr, Some(&mut resolver)).unwrap();
        }

        let func_after =
            unsafe { ptr::read_unaligned(new_base.cast::<FunctionBytecode>()) };
        assert_eq!(func_after.func_name(), replacement_val);
        let arr_ptr = unsafe {
            new_base
                .add(func_size + size_of::<JSWord>())
                .cast::<JSValue>()
        };
        assert_eq!(unsafe { *arr_ptr }, replacement_val);
        assert!(resolver.hits > 0);
    }

    #[cfg(target_pointer_width = "64")]
    #[test]
    fn expand_short_floats_rewrites_value_array_slots() {
        let mut arena = HeapArena::new(128);
        let array_size = crate::containers::value_array_alloc_size(1);
        let array_ptr = arena
            .layout
            .malloc(array_size, MTag::ValueArray, |_| {})
            .expect("alloc value array");
        let header = ValueArrayHeader::new(1, false);
        unsafe {
            ptr::write_unaligned(array_ptr.as_ptr().cast::<JSWord>(), header.header().word());
            let arr_ptr = array_ptr.as_ptr().add(size_of::<JSWord>()).cast::<JSValue>();
            *arr_ptr = JSValue::short_float_from_f64(1.5);
        }

        expand_short_floats(&mut arena.layout).expect("expand short floats");

        let arr_ptr = unsafe { array_ptr.as_ptr().add(size_of::<JSWord>()).cast::<JSValue>() };
        let val = unsafe { *arr_ptr };
        assert!(val.is_ptr());
        let ptr = val.to_ptr::<u8>().expect("float64 pointer");
        let payload = unsafe { ptr.as_ptr().add(size_of::<JSWord>()) as *const f64 };
        let stored = unsafe { ptr::read_unaligned(payload) };
        assert_eq!(stored.to_bits(), 1.5_f64.to_bits());
    }

    #[cfg(target_pointer_width = "64")]
    #[test]
    fn compact_heap_64to32_updates_roots_and_payload() {
        let mut arena = HeapArena::new(128);
        let byte_len = 3usize;
        let byte_size = crate::containers::byte_array_alloc_size(byte_len as JSWord);
        let byte_ptr = arena
            .layout
            .malloc(byte_size, MTag::ByteArray, |_| {})
            .expect("alloc byte array");
        let byte_header = ByteArrayHeader::new(byte_len as JSWord, false);
        unsafe {
            ptr::write_unaligned(byte_ptr.as_ptr().cast::<JSWord>(), byte_header.header().word());
            let payload = byte_ptr.as_ptr().add(size_of::<JSWord>());
            let data = [1u8, 2, 3];
            ptr::copy_nonoverlapping(data.as_ptr(), payload, data.len());
        }

        let array_size = crate::containers::value_array_alloc_size(1);
        let array_ptr = arena
            .layout
            .malloc(array_size, MTag::ValueArray, |_| {})
            .expect("alloc value array");
        let array_header = ValueArrayHeader::new(1, false);
        unsafe {
            ptr::write_unaligned(array_ptr.as_ptr().cast::<JSWord>(), array_header.header().word());
            let arr_ptr = array_ptr.as_ptr().add(size_of::<JSWord>()).cast::<JSValue>();
            *arr_ptr = JSValue::from_ptr(byte_ptr);
        }

        let mut root_val = JSValue::from_ptr(array_ptr);
        let mut roots = RootSlot::new(&mut root_val);
        let len = unsafe { compact_heap_64to32(&mut arena.layout, &mut roots) }
            .expect("compact 64->32");

        let expected_byte_size = WORD_32 + super::align_up_32(byte_len);
        let expected_array_size = WORD_32 + WORD_32;
        let expected_len = expected_byte_size + expected_array_size;
        assert_eq!(len, expected_len);

        let expected_root = (expected_byte_size as u32) | (JSValue::JS_TAG_PTR as u32);
        assert_eq!(root_val.raw_bits() as u32, expected_root);

        let base = arena.layout.heap_base().as_ptr();
        let array32 = unsafe { base.add(expected_byte_size) };
        let arr_val = unsafe { ptr::read_unaligned(array32.add(WORD_32) as *const u32) };
        assert_eq!(arr_val, JSValue::JS_TAG_PTR as u32);

        let payload = unsafe { base.add(WORD_32) };
        let bytes = unsafe { core::slice::from_raw_parts(payload, byte_len) };
        assert_eq!(bytes, &[1, 2, 3]);
    }

    #[cfg(target_pointer_width = "64")]
    #[test]
    fn compact_heap_64to32_rejects_unsupported_tags() {
        let mut arena = HeapArena::new(128);
        let base = size_of::<JSWord>() + size_of::<JSValue>() + size_of::<*mut JSValue>();
        let var_ptr = arena
            .layout
            .malloc(base, MTag::VarRef, |_| {})
            .expect("alloc varref");
        let header = crate::containers::VarRefHeader::new(false, false);
        unsafe {
            ptr::write_unaligned(var_ptr.as_ptr().cast::<JSWord>(), header.header().word());
        }

        let mut root_val = JSValue::from_ptr(var_ptr);
        let mut roots = RootSlot::new(&mut root_val);
        let err = unsafe { compact_heap_64to32(&mut arena.layout, &mut roots) }
            .expect_err("unsupported tag");
        assert_eq!(err, BytecodePrepareError::UnsupportedTag(MTag::VarRef));
    }
}
