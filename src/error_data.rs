use crate::jsvalue::JSValue;

// C: `JSErrorData` in mquickjs.c.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct ErrorData {
    message: JSValue,
    stack: JSValue,
}

impl ErrorData {
    pub const fn new(message: JSValue, stack: JSValue) -> Self {
        Self { message, stack }
    }

    pub const fn message(self) -> JSValue {
        self.message
    }

    pub const fn stack(self) -> JSValue {
        self.stack
    }

    pub(crate) unsafe fn message_ptr(this: *mut Self) -> *mut JSValue {
        // SAFETY: caller guarantees `this` is valid for writes.
        unsafe { core::ptr::addr_of_mut!((*this).message) }
    }

    pub(crate) unsafe fn stack_ptr(this: *mut Self) -> *mut JSValue {
        // SAFETY: caller guarantees `this` is valid for writes.
        unsafe { core::ptr::addr_of_mut!((*this).stack) }
    }
}
