use crate::containers::{StringHeader, ValueArrayHeader};
use crate::function_bytecode::FunctionBytecode;
use crate::heap::mblock_size;
use crate::jsvalue::{value_from_ptr_raw, value_to_ptr, JSValue, JSWord};
use crate::memblock::{MbHeader, MTag};
use crate::stdlib::stdlib_def::{BytecodeHeader, JS_BYTECODE_MAGIC, JS_BYTECODE_VERSION};
use core::mem::size_of;
use core::ptr;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BytecodeRelocError {
    BufferTooSmall,
    InvalidMagic,
    InvalidVersion,
}

pub trait BytecodeAtomResolver {
    fn resolve_unique_string(&mut self, val: JSValue) -> Option<JSValue>;
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
/// `new_base_addr` must match the address of `data`.
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
    debug_assert_eq!(data_base.addr(), new_base_addr);
    let old_base_addr = header.base_addr;
    relocate_value(
        &mut header.unique_strings,
        old_base_addr,
        new_base_addr,
        data_base,
        &mut resolver,
    );
    relocate_value(
        &mut header.main_func,
        old_base_addr,
        new_base_addr,
        data_base,
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
                    &mut resolver,
                );
                relocate_value(
                    FunctionBytecode::byte_code_ptr(func),
                    old_base_addr,
                    new_base_addr,
                    data_base,
                    &mut resolver,
                );
                relocate_value(
                    FunctionBytecode::cpool_ptr(func),
                    old_base_addr,
                    new_base_addr,
                    data_base,
                    &mut resolver,
                );
                relocate_value(
                    FunctionBytecode::vars_ptr(func),
                    old_base_addr,
                    new_base_addr,
                    data_base,
                    &mut resolver,
                );
                relocate_value(
                    FunctionBytecode::ext_vars_ptr(func),
                    old_base_addr,
                    new_base_addr,
                    data_base,
                    &mut resolver,
                );
                relocate_value(
                    FunctionBytecode::filename_ptr(func),
                    old_base_addr,
                    new_base_addr,
                    data_base,
                    &mut resolver,
                );
                relocate_value(
                    FunctionBytecode::pc2line_ptr(func),
                    old_base_addr,
                    new_base_addr,
                    data_base,
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
    resolver: &mut Option<&mut dyn BytecodeAtomResolver>,
) {
    let val = unsafe {
        // SAFETY: caller guarantees `pval` is a valid JSValue slot.
        *pval
    };
    let Some(ptr) = value_to_ptr::<u8>(val) else {
        return;
    };
    let old_addr = ptr.as_ptr().addr();
    let offset = old_addr.wrapping_sub(old_base_addr);
    let new_addr = new_base_addr.wrapping_add(offset);
    let new_ptr = data_base.with_addr(new_addr);
    let mut new_val = value_from_ptr_raw(new_ptr).expect("relocated pointer must be non-null");
    if let Some(resolver) = resolver.as_deref_mut() {
        new_val = resolve_unique_string(new_val, resolver);
    }
    unsafe {
        // SAFETY: caller guarantees `pval` is writable.
        *pval = new_val;
    }
}

fn resolve_unique_string(val: JSValue, resolver: &mut dyn BytecodeAtomResolver) -> JSValue {
    let Some(ptr) = value_to_ptr::<u8>(val) else {
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
    use crate::jsvalue::{value_from_ptr, JS_NULL};
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

    #[test]
    fn is_bytecode_checks_magic() {
        let mut header = BytecodeHeader {
            magic: JS_BYTECODE_MAGIC,
            version: JS_BYTECODE_VERSION,
            base_addr: 0,
            unique_strings: JS_NULL,
            main_func: JS_NULL,
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
        assert_eq!(func_size % (crate::jsvalue::JSW as usize), 0);
        assert_eq!(array_size % (crate::jsvalue::JSW as usize), 0);
        assert_eq!(string_size % (crate::jsvalue::JSW as usize), 0);

        let total = func_size + array_size + string_size;
        let words = align_up(total, crate::jsvalue::JSW as usize) / (crate::jsvalue::JSW as usize);
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
            *arr_ptr.add(0) = value_from_ptr(NonNull::new(string_ptr).unwrap());
            *arr_ptr.add(1) = JS_NULL;
        }

        let func_header = FunctionBytecodeHeader::new(false, false, false, 0, false);
        let func_fields = FunctionBytecodeFields {
            func_name: value_from_ptr(NonNull::new(string_ptr).unwrap()),
            byte_code: JS_NULL,
            cpool: value_from_ptr(NonNull::new(array_ptr).unwrap()),
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
            ptr::write_unaligned(func_ptr.cast::<FunctionBytecode>(), func);
        }

        let mut header = BytecodeHeader {
            magic: JS_BYTECODE_MAGIC,
            version: JS_BYTECODE_VERSION,
            base_addr: old_base_addr,
            unique_strings: value_from_ptr(NonNull::new(array_ptr).unwrap()),
            main_func: value_from_ptr(NonNull::new(func_ptr).unwrap()),
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
            value_from_ptr(NonNull::new(unsafe { new_base.add(func_size + array_size) }).unwrap());
        let expected_array =
            value_from_ptr(NonNull::new(unsafe { new_base.add(func_size) }).unwrap());
        let expected_func = value_from_ptr(NonNull::new(new_base).unwrap());

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
        let words = align_up(total, crate::jsvalue::JSW as usize) / (crate::jsvalue::JSW as usize);
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
            *arr_ptr = value_from_ptr(NonNull::new(string_ptr).unwrap());
        }

        let func_header = FunctionBytecodeHeader::new(false, false, false, 0, false);
        let func_fields = FunctionBytecodeFields {
            func_name: value_from_ptr(NonNull::new(string_ptr).unwrap()),
            byte_code: JS_NULL,
            cpool: value_from_ptr(NonNull::new(array_ptr).unwrap()),
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
            ptr::write_unaligned(func_ptr.cast::<FunctionBytecode>(), func);
        }

        let mut header = BytecodeHeader {
            magic: JS_BYTECODE_MAGIC,
            version: JS_BYTECODE_VERSION,
            base_addr: old_base_addr,
            unique_strings: value_from_ptr(NonNull::new(array_ptr).unwrap()),
            main_func: value_from_ptr(NonNull::new(func_ptr).unwrap()),
        };

        let mut replacement_mem = vec![
            0 as JSWord;
            align_up(string_size, crate::jsvalue::JSW as usize) / (crate::jsvalue::JSW as usize)
        ];
        let replacement_ptr = replacement_mem.as_mut_ptr().cast::<u8>();
        write_string(replacement_ptr, b"bar", true);
        let replacement_val = value_from_ptr(NonNull::new(replacement_ptr).unwrap());

        let mut new_words = vec![0 as JSWord; words];
        new_words.copy_from_slice(&old_words);
        let new_base = new_words.as_mut_ptr().cast::<u8>();
        let new_base_addr = new_base as usize;
        let new_string_ptr = unsafe { new_base.add(func_size + array_size) };
        let target_val = value_from_ptr(NonNull::new(new_string_ptr).unwrap());
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
}
