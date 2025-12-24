use crate::jsvalue::JSValue;

// C: `JSArrayBuffer` in mquickjs.c.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct ArrayBuffer {
    byte_buffer: JSValue,
}

impl ArrayBuffer {
    pub const fn new(byte_buffer: JSValue) -> Self {
        Self { byte_buffer }
    }

    pub const fn byte_buffer(self) -> JSValue {
        self.byte_buffer
    }
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    #[test]
    fn array_buffer_roundtrip() {
        let data = ArrayBuffer::new(crate::jsvalue::JS_NULL);
        assert_eq!(data.byte_buffer(), crate::jsvalue::JS_NULL);
    }
}
