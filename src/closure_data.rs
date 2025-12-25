use crate::jsvalue::JSValue;

// C: `JSClosureData` in mquickjs.c.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct ClosureData {
    func_bytecode: JSValue,
    var_refs: JSValue,
}

impl ClosureData {
    pub const fn new(func_bytecode: JSValue, var_refs: JSValue) -> Self {
        Self {
            func_bytecode,
            var_refs,
        }
    }

    pub const fn func_bytecode(self) -> JSValue {
        self.func_bytecode
    }

    pub const fn var_refs(self) -> JSValue {
        self.var_refs
    }

    pub(crate) unsafe fn func_bytecode_ptr(this: *mut Self) -> *mut JSValue {
        // SAFETY: caller guarantees `this` is valid for writes.
        unsafe { core::ptr::addr_of_mut!((*this).func_bytecode) }
    }

    pub(crate) unsafe fn var_refs_ptr(this: *mut Self) -> *mut JSValue {
        // SAFETY: caller guarantees `this` is valid for writes.
        unsafe { core::ptr::addr_of_mut!((*this).var_refs) }
    }
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    #[test]
    fn closure_data_roundtrip() {
        let data = ClosureData::new(crate::jsvalue::JS_NULL, crate::jsvalue::JS_UNDEFINED);
        assert_eq!(data.func_bytecode(), crate::jsvalue::JS_NULL);
        assert_eq!(data.var_refs(), crate::jsvalue::JS_UNDEFINED);
    }
}
