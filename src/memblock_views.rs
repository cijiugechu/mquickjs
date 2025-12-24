use crate::containers::{ByteArrayHeader, ValueArrayHeader};
use crate::jsvalue::{JSValue, JSWord};
use crate::memblock::{Float64Header, MTag, MbHeader};
use core::ptr::NonNull;

// C: `JSFloat64` in mquickjs.c (read-only view).
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Float64View {
    header: Float64Header,
    dval: f64,
}

impl Float64View {
    pub fn new(gc_mark: bool, dval: f64) -> Self {
        Self {
            header: Float64Header::new(gc_mark),
            dval,
        }
    }

    pub const fn header(self) -> Float64Header {
        self.header
    }

    pub const fn dval(self) -> f64 {
        self.dval
    }
}

// C: `JSByteArray` in mquickjs.c (read-only view).
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct ByteArrayView {
    header: ByteArrayHeader,
    buf: NonNull<u8>,
}

impl ByteArrayView {
    pub fn new(header: ByteArrayHeader, buf: NonNull<u8>) -> Self {
        debug_assert!(MbHeader::from(header).tag() == MTag::ByteArray);
        Self { header, buf }
    }

    pub const fn header(self) -> ByteArrayHeader {
        self.header
    }

    pub const fn size(self) -> JSWord {
        self.header.size()
    }

    pub const fn buf(self) -> NonNull<u8> {
        self.buf
    }
}

// C: `JSValueArray` in mquickjs.c (read-only view).
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct ValueArrayView {
    header: ValueArrayHeader,
    arr: NonNull<JSValue>,
}

impl ValueArrayView {
    pub fn new(header: ValueArrayHeader, arr: NonNull<JSValue>) -> Self {
        debug_assert!(MbHeader::from(header).tag() == MTag::ValueArray);
        Self { header, arr }
    }

    pub const fn header(self) -> ValueArrayHeader {
        self.header
    }

    pub const fn size(self) -> JSWord {
        self.header.size()
    }

    pub const fn arr(self) -> NonNull<JSValue> {
        self.arr
    }
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    #[test]
    fn float64_view_roundtrip() {
        let view = Float64View::new(true, 1.25);
        assert_eq!(view.header().header().tag(), MTag::Float64);
        assert!(view.header().header().gc_mark());
        assert_eq!(view.dval(), 1.25);
    }

    #[test]
    fn byte_array_view_roundtrip() {
        let header = ByteArrayHeader::new(12, false);
        let view = ByteArrayView::new(header, NonNull::new(0x1000 as *mut u8).unwrap());
        assert_eq!(view.size(), 12);
        assert_eq!(view.buf().as_ptr(), 0x1000 as *mut u8);
    }

    #[test]
    fn value_array_view_roundtrip() {
        let header = ValueArrayHeader::new(4, true);
        let view = ValueArrayView::new(
            header,
            NonNull::new(0x2000 as *mut JSValue).unwrap(),
        );
        assert_eq!(view.size(), 4);
        assert_eq!(view.arr().as_ptr(), 0x2000 as *mut JSValue);
    }
}
