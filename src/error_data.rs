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
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    #[test]
    fn error_data_roundtrip() {
        let data = ErrorData::new(crate::jsvalue::JS_NULL, crate::jsvalue::JS_UNDEFINED);
        assert_eq!(data.message(), crate::jsvalue::JS_NULL);
        assert_eq!(data.stack(), crate::jsvalue::JS_UNDEFINED);
    }
}
