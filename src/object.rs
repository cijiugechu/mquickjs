use crate::array_buffer::ArrayBuffer;
use crate::array_data::ArrayData;
use crate::cfunction_data::CFunctionData;
use crate::closure_data::ClosureData;
use crate::error_data::ErrorData;
use crate::memblock::{MbHeader, MTag, JS_MTAG_BITS};
use crate::jsvalue::{JSValue, JSWord};
use crate::typed_array::TypedArray;
use core::ffi::c_void;

// C: `JSObject` header bitfields in mquickjs.c.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct ObjectHeader(JSWord);

impl ObjectHeader {
    pub const CLASS_ID_BITS: u32 = 8;
    pub const CLASS_ID_SHIFT: u32 = JS_MTAG_BITS;
    pub const EXTRA_SIZE_SHIFT: u32 = JS_MTAG_BITS + Self::CLASS_ID_BITS;

    pub const fn word_bits() -> u32 {
        (JSValue::JSW as u32) * 8
    }

    pub const EXTRA_SIZE_BITS: u32 = Self::word_bits() - Self::EXTRA_SIZE_SHIFT;
    pub const CLASS_ID_MASK: JSWord = ((1 as JSWord) << Self::CLASS_ID_BITS) - 1;
    pub const EXTRA_SIZE_MASK: JSWord = ((1 as JSWord) << Self::EXTRA_SIZE_BITS) - 1;

    pub fn new(class_id: u8, extra_size: JSWord, gc_mark: bool) -> Self {
        debug_assert!((class_id as JSWord) <= Self::CLASS_ID_MASK);
        debug_assert!(extra_size <= Self::EXTRA_SIZE_MASK);
        let word = MbHeader::new(MTag::Object, gc_mark).word()
            | ((class_id as JSWord) << Self::CLASS_ID_SHIFT)
            | (extra_size << Self::EXTRA_SIZE_SHIFT);
        Self(word)
    }

    pub const fn from_word(word: JSWord) -> Self {
        Self(word)
    }

    pub const fn header(self) -> MbHeader {
        MbHeader::from_word(self.0)
    }

    pub const fn gc_mark(self) -> bool {
        MbHeader::from_word(self.0).gc_mark()
    }

    pub fn tag(self) -> MTag {
        MbHeader::from_word(self.0).tag()
    }

    pub const fn class_id(self) -> u8 {
        ((self.0 >> Self::CLASS_ID_SHIFT) & Self::CLASS_ID_MASK) as u8
    }

    pub const fn extra_size(self) -> JSWord {
        (self.0 >> Self::EXTRA_SIZE_SHIFT) & Self::EXTRA_SIZE_MASK
    }
}

// C: `JSRegExp` in mquickjs.c.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct RegExp {
    source: JSValue,
    byte_code: JSValue,
    last_index: i32,
}

impl RegExp {
    pub const fn new(source: JSValue, byte_code: JSValue, last_index: i32) -> Self {
        Self {
            source,
            byte_code,
            last_index,
        }
    }

    pub const fn source(self) -> JSValue {
        self.source
    }

    pub const fn byte_code(self) -> JSValue {
        self.byte_code
    }

    pub const fn last_index(self) -> i32 {
        self.last_index
    }

    pub(crate) unsafe fn source_ptr(this: *mut Self) -> *mut JSValue {
        // SAFETY: caller guarantees `this` is valid for writes.
        unsafe { core::ptr::addr_of_mut!((*this).source) }
    }

    pub(crate) unsafe fn byte_code_ptr(this: *mut Self) -> *mut JSValue {
        // SAFETY: caller guarantees `this` is valid for writes.
        unsafe { core::ptr::addr_of_mut!((*this).byte_code) }
    }

    #[allow(dead_code)]
    pub(crate) unsafe fn last_index_ptr(this: *mut Self) -> *mut i32 {
        // SAFETY: caller guarantees `this` is valid for writes.
        unsafe { core::ptr::addr_of_mut!((*this).last_index) }
    }
}

// C: boxed primitive value stored in object payload.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[repr(C)]
pub struct PrimitiveValue {
    value: JSValue,
}

impl PrimitiveValue {
    pub const fn new(value: JSValue) -> Self {
        Self { value }
    }

    pub const fn value(self) -> JSValue {
        self.value
    }

    pub(crate) unsafe fn value_ptr(this: *mut Self) -> *mut JSValue {
        // SAFETY: caller guarantees `this` is valid for writes.
        unsafe { core::ptr::addr_of_mut!((*this).value) }
    }
}

// C: `JSObjectUserData` in mquickjs.c.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct ObjectUserData {
    opaque: *mut c_void,
}

impl ObjectUserData {
    pub const fn new(opaque: *mut c_void) -> Self {
        Self { opaque }
    }

    pub const fn opaque(self) -> *mut c_void {
        self.opaque
    }
}

// C: `JSObject` union payload in mquickjs.c (`JSObject::u`).
#[repr(C)]
#[derive(Copy, Clone)]
pub union ObjectPayload {
    pub closure: ClosureData,
    pub cfunc: CFunctionData,
    pub array: ArrayData,
    pub error: ErrorData,
    pub array_buffer: ArrayBuffer,
    pub typed_array: TypedArray,
    pub regexp: RegExp,
    pub primitive: PrimitiveValue,
    pub user: ObjectUserData,
}

// C: `JSObject` in mquickjs.c.
#[repr(C)]
#[derive(Copy, Clone)]
pub struct Object {
    header: ObjectHeader,
    proto: JSValue,
    props: JSValue,
    payload: ObjectPayload,
}

impl Object {
    pub const PAYLOAD_OFFSET: usize = core::mem::offset_of!(Object, payload);
    pub const PROTO_OFFSET: usize = core::mem::offset_of!(Object, proto);
    pub const PROPS_OFFSET: usize = core::mem::offset_of!(Object, props);

    pub const fn new(header: ObjectHeader, proto: JSValue, props: JSValue, payload: ObjectPayload) -> Self {
        Self {
            header,
            proto,
            props,
            payload,
        }
    }

    pub const fn header(self) -> ObjectHeader {
        self.header
    }

    pub const fn proto(self) -> JSValue {
        self.proto
    }

    pub const fn props(self) -> JSValue {
        self.props
    }

    pub const fn payload(self) -> ObjectPayload {
        self.payload
    }

    pub(crate) unsafe fn proto_ptr(this: *mut Self) -> *mut JSValue {
        // SAFETY: caller guarantees `this` is valid for writes.
        unsafe { core::ptr::addr_of_mut!((*this).proto) }
    }

    pub(crate) unsafe fn props_ptr(this: *mut Self) -> *mut JSValue {
        // SAFETY: caller guarantees `this` is valid for writes.
        unsafe { core::ptr::addr_of_mut!((*this).props) }
    }

    pub(crate) unsafe fn payload_ptr(this: *mut Self) -> *mut ObjectPayload {
        // SAFETY: caller guarantees `this` is valid for writes.
        unsafe { core::ptr::addr_of_mut!((*this).payload) }
    }
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;
    use core::mem::size_of;

    #[test]
    fn object_header_max_values() {
        let header = ObjectHeader::new(ObjectHeader::CLASS_ID_MASK as u8, ObjectHeader::EXTRA_SIZE_MASK, false);
        assert_eq!(header.class_id(), ObjectHeader::CLASS_ID_MASK as u8);
        assert_eq!(header.extra_size(), ObjectHeader::EXTRA_SIZE_MASK);
    }

    #[test]
    fn object_payload_size() {
        let sizes = [
            size_of::<ClosureData>(),
            size_of::<CFunctionData>(),
            size_of::<ArrayData>(),
            size_of::<ErrorData>(),
            size_of::<ArrayBuffer>(),
            size_of::<TypedArray>(),
            size_of::<RegExp>(),
            size_of::<PrimitiveValue>(),
            size_of::<ObjectUserData>(),
        ];
        let max = sizes.iter().copied().max().unwrap();
        assert_eq!(size_of::<ObjectPayload>(), max);
    }
}
