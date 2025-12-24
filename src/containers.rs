use crate::jsvalue::{JSValue, JSWord, JSW};
use crate::memblock::{MbHeader, MTag, JS_MTAG_BITS};

const JS_STRING_FLAG_UNIQUE_SHIFT: u32 = JS_MTAG_BITS;
const JS_STRING_FLAG_ASCII_SHIFT: u32 = JS_MTAG_BITS + 1;
const JS_STRING_FLAG_NUMERIC_SHIFT: u32 = JS_MTAG_BITS + 2;
const JS_STRING_LEN_SHIFT: u32 = JS_MTAG_BITS + 3;

pub const JS_STRING_LEN_MAX: JSWord = if JSW == 8 {
    0x7ffffffe
} else {
    (1 << (32 - JS_MTAG_BITS - 3)) - 1
};

pub const JS_BYTE_ARRAY_SIZE_MAX: JSWord = (1 << (32 - JS_MTAG_BITS)) - 1;
pub const JS_VALUE_ARRAY_SIZE_MAX: JSWord = JS_BYTE_ARRAY_SIZE_MAX;

// C: `JSString` header bitfields in mquickjs.c.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct StringHeader(MbHeader);

impl StringHeader {
    pub fn new(len: JSWord, is_unique: bool, is_ascii: bool, is_numeric: bool, gc_mark: bool) -> Self {
        debug_assert!(len <= JS_STRING_LEN_MAX);
        let mut word = MbHeader::new(MTag::String, gc_mark).word();
        word |= (is_unique as JSWord) << JS_STRING_FLAG_UNIQUE_SHIFT;
        word |= (is_ascii as JSWord) << JS_STRING_FLAG_ASCII_SHIFT;
        word |= (is_numeric as JSWord) << JS_STRING_FLAG_NUMERIC_SHIFT;
        Self(MbHeader::from_word(word | (len << JS_STRING_LEN_SHIFT)))
    }

    pub const fn header(self) -> MbHeader {
        self.0
    }

    pub const fn len(self) -> JSWord {
        self.0.word() >> JS_STRING_LEN_SHIFT
    }

    pub const fn is_empty(self) -> bool {
        self.len() == 0
    }

    pub const fn is_unique(self) -> bool {
        (self.0.word() >> JS_STRING_FLAG_UNIQUE_SHIFT) & 1 != 0
    }

    pub const fn is_ascii(self) -> bool {
        (self.0.word() >> JS_STRING_FLAG_ASCII_SHIFT) & 1 != 0
    }

    pub const fn is_numeric(self) -> bool {
        (self.0.word() >> JS_STRING_FLAG_NUMERIC_SHIFT) & 1 != 0
    }
}

impl From<StringHeader> for MbHeader {
    fn from(header: StringHeader) -> Self {
        header.0
    }
}

impl From<MbHeader> for StringHeader {
    fn from(header: MbHeader) -> Self {
        debug_assert!(header.tag() == MTag::String);
        Self(header)
    }
}

pub fn string_alloc_size(len: JSWord) -> usize {
    debug_assert!(len <= JS_STRING_LEN_MAX);
    let len_plus_nul = (len as usize).saturating_add(1);
    let aligned = len_plus_nul.next_multiple_of(JSW as usize);
    aligned + core::mem::size_of::<JSWord>()
}

// C: `JSByteArray` header bitfields in mquickjs.c.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct ByteArrayHeader(MbHeader);

impl ByteArrayHeader {
    pub fn new(size: JSWord, gc_mark: bool) -> Self {
        debug_assert!(size <= JS_BYTE_ARRAY_SIZE_MAX);
        let word = MbHeader::new(MTag::ByteArray, gc_mark).word() | (size << JS_MTAG_BITS);
        Self(MbHeader::from_word(word))
    }

    pub const fn header(self) -> MbHeader {
        self.0
    }

    pub const fn size(self) -> JSWord {
        self.0.word() >> JS_MTAG_BITS
    }
}

impl From<ByteArrayHeader> for MbHeader {
    fn from(header: ByteArrayHeader) -> Self {
        header.0
    }
}

impl From<MbHeader> for ByteArrayHeader {
    fn from(header: MbHeader) -> Self {
        debug_assert!(header.tag() == MTag::ByteArray);
        Self(header)
    }
}

pub fn byte_array_alloc_size(size: JSWord) -> usize {
    debug_assert!(size <= JS_BYTE_ARRAY_SIZE_MAX);
    core::mem::size_of::<JSWord>() + size as usize
}

// C: `JSValueArray` header bitfields in mquickjs.c.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct ValueArrayHeader(MbHeader);

impl ValueArrayHeader {
    pub fn new(size: JSWord, gc_mark: bool) -> Self {
        debug_assert!(size <= JS_VALUE_ARRAY_SIZE_MAX);
        let word = MbHeader::new(MTag::ValueArray, gc_mark).word() | (size << JS_MTAG_BITS);
        Self(MbHeader::from_word(word))
    }

    pub const fn header(self) -> MbHeader {
        self.0
    }

    pub const fn size(self) -> JSWord {
        self.0.word() >> JS_MTAG_BITS
    }
}

impl From<ValueArrayHeader> for MbHeader {
    fn from(header: ValueArrayHeader) -> Self {
        header.0
    }
}

impl From<MbHeader> for ValueArrayHeader {
    fn from(header: MbHeader) -> Self {
        debug_assert!(header.tag() == MTag::ValueArray);
        Self(header)
    }
}

pub fn value_array_alloc_size(size: JSWord) -> usize {
    debug_assert!(size <= JS_VALUE_ARRAY_SIZE_MAX);
    core::mem::size_of::<JSWord>() + (size as usize) * core::mem::size_of::<JSValue>()
}

// C: `JSVarRef` header bitfields in mquickjs.c.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct VarRefHeader(MbHeader);

impl VarRefHeader {
    pub fn new(is_detached: bool, gc_mark: bool) -> Self {
        let word = MbHeader::new(MTag::VarRef, gc_mark).word() | ((is_detached as JSWord) << JS_MTAG_BITS);
        Self(MbHeader::from_word(word))
    }

    pub const fn header(self) -> MbHeader {
        self.0
    }

    pub const fn is_detached(self) -> bool {
        (self.0.word() >> JS_MTAG_BITS) & 1 != 0
    }
}

impl From<VarRefHeader> for MbHeader {
    fn from(header: VarRefHeader) -> Self {
        header.0
    }
}

impl From<MbHeader> for VarRefHeader {
    fn from(header: MbHeader) -> Self {
        debug_assert!(header.tag() == MTag::VarRef);
        Self(header)
    }
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    #[test]
    fn string_header_roundtrip() {
        let header = StringHeader::new(42, true, false, true, true);
        assert_eq!(header.len(), 42);
        assert!(header.is_unique());
        assert!(!header.is_ascii());
        assert!(header.is_numeric());
    }

    #[test]
    fn string_size_matches_alignment() {
        let size = string_alloc_size(0);
        assert_eq!(size, core::mem::size_of::<JSWord>() + JSW as usize);
        let size = string_alloc_size(JSW);
        assert_eq!(size, core::mem::size_of::<JSWord>() + (JSW as usize) * 2);
    }

    #[test]
    fn byte_array_header_roundtrip() {
        let header = ByteArrayHeader::new(128, false);
        assert_eq!(header.size(), 128);
    }

    #[test]
    fn value_array_header_roundtrip() {
        let header = ValueArrayHeader::new(7, true);
        assert_eq!(header.size(), 7);
    }

    #[test]
    fn var_ref_header_roundtrip() {
        let header = VarRefHeader::new(true, false);
        assert!(header.is_detached());
    }
}
