use crate::jsvalue::JSValue;

// C: `JSClosureData` in mquickjs.c (flexible array layout).
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[repr(C)]
pub struct ClosureData {
    func_bytecode: JSValue,
    var_refs: [JSValue; 0],
}

impl ClosureData {
    pub const fn new(func_bytecode: JSValue) -> Self {
        Self {
            func_bytecode,
            var_refs: [],
        }
    }

    pub const fn func_bytecode(self) -> JSValue {
        self.func_bytecode
    }

    pub(crate) unsafe fn func_bytecode_ptr(this: *mut Self) -> *mut JSValue {
        // SAFETY: caller guarantees `this` is valid for writes.
        unsafe { core::ptr::addr_of_mut!((*this).func_bytecode) }
    }

    pub(crate) unsafe fn var_refs_ptr(this: *mut Self) -> *mut JSValue {
        // SAFETY: caller guarantees `this` points at a ClosureData header.
        unsafe { core::ptr::addr_of_mut!((*this).var_refs).cast::<JSValue>() }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn closure_data_var_refs_offset() {
        let mut slots = [crate::jsvalue::JS_NULL; 2];
        let base = slots.as_mut_ptr();
        let closure = base.cast::<ClosureData>();
        unsafe {
            // SAFETY: slots provides writable storage for ClosureData + one var ref.
            *ClosureData::func_bytecode_ptr(closure) = crate::jsvalue::JS_TRUE;
            *ClosureData::var_refs_ptr(closure) = crate::jsvalue::JS_FALSE;
        }
        unsafe {
            // SAFETY: slots still owns the backing storage.
            assert_eq!(*base, crate::jsvalue::JS_TRUE);
            assert_eq!(*base.add(1), crate::jsvalue::JS_FALSE);
        }
    }
}
