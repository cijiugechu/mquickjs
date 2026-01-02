use crate::jsvalue::JSValue;

// C: `JSCFunctionData` in mquickjs.c.
#[repr(C)]
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
