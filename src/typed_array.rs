use crate::jsvalue::JSValue;

// C: `JSTypedArray` in mquickjs.c.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct TypedArray {
    buffer: JSValue,
    len: u32,
    offset: u32,
}

impl TypedArray {
    pub const fn new(buffer: JSValue, len: u32, offset: u32) -> Self {
        Self { buffer, len, offset }
    }

    pub const fn buffer(self) -> JSValue {
        self.buffer
    }

    pub(crate) unsafe fn buffer_ptr(this: *mut Self) -> *mut JSValue {
        // SAFETY: caller guarantees `this` is valid for writes.
        unsafe { core::ptr::addr_of_mut!((*this).buffer) }
    }

    pub const fn len(self) -> u32 {
        self.len
    }

    pub const fn is_empty(self) -> bool {
        self.len == 0
    }

    pub const fn offset(self) -> u32 {
        self.offset
    }
}
