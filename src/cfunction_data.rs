use crate::jsvalue::JSValue;

// C: `JSCFunctionData` in mquickjs.c.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct CFunctionData {
    idx: u32,
    params: JSValue,
}

pub(crate) const CFUNC_PARAMS_OFFSET: usize = core::mem::offset_of!(CFunctionData, params);

impl CFunctionData {
    pub const fn new(idx: u32, params: JSValue) -> Self {
        Self { idx, params }
    }

    pub const fn idx(self) -> u32 {
        self.idx
    }

    pub const fn params(self) -> JSValue {
        self.params
    }

    pub(crate) unsafe fn params_ptr(this: *mut Self) -> *mut JSValue {
        // SAFETY: caller guarantees `this` is valid for writes.
        unsafe { core::ptr::addr_of_mut!((*this).params) }
    }
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    #[test]
    fn cfunction_data_roundtrip() {
        let data = CFunctionData::new(7, crate::jsvalue::JS_NULL);
        assert_eq!(data.idx(), 7);
        assert_eq!(data.params(), crate::jsvalue::JS_NULL);
    }
}
