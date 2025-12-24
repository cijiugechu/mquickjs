use crate::jsvalue::JSWord;

// C: mquickjs_priv.h `JS_MTAG_BITS` and `JS_MB_HEADER` bitfield layout.
pub const JS_MTAG_BITS: u32 = 4;
pub const JS_MTAG_SHIFT: u32 = 1;
pub const JS_MTAG_WIDTH: u32 = JS_MTAG_BITS - 1;
pub const JS_MTAG_MASK: JSWord = ((1 as JSWord) << JS_MTAG_WIDTH) - 1;
pub const JS_MTAG_FIELD_MASK: JSWord = JS_MTAG_MASK << JS_MTAG_SHIFT;

// C: enum `JS_MTAG_*` in mquickjs_priv.h.
#[repr(u8)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum MTag {
    Free = 0,
    Object = 1,
    Float64 = 2,
    String = 3,
    FunctionBytecode = 4,
    ValueArray = 5,
    ByteArray = 6,
    VarRef = 7,
}

impl MTag {
    pub const COUNT: JSWord = 8;

    pub const fn as_word(self) -> JSWord {
        self as JSWord
    }

    pub fn try_from_word(tag: JSWord) -> Option<Self> {
        match tag {
            0 => Some(MTag::Free),
            1 => Some(MTag::Object),
            2 => Some(MTag::Float64),
            3 => Some(MTag::String),
            4 => Some(MTag::FunctionBytecode),
            5 => Some(MTag::ValueArray),
            6 => Some(MTag::ByteArray),
            7 => Some(MTag::VarRef),
            _ => None,
        }
    }
}

// C: `JS_MTAG_*` values wrapped as a typed tag.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct MbTag(MTag);

impl MbTag {
    pub const fn new(tag: MTag) -> Self {
        Self(tag)
    }

    pub const fn word(self) -> JSWord {
        self.0.as_word()
    }

    pub const fn kind(self) -> MTag {
        self.0
    }

    pub fn from_word(tag: JSWord) -> Self {
        debug_assert!((tag & !JS_MTAG_MASK) == 0);
        match MTag::try_from_word(tag) {
            Some(kind) => Self(kind),
            None => {
                debug_assert!(false, "invalid memblock tag");
                Self(MTag::Free)
            }
        }
    }
}

impl From<MTag> for MbTag {
    fn from(tag: MTag) -> Self {
        Self::new(tag)
    }
}

impl From<MbTag> for MTag {
    fn from(tag: MbTag) -> Self {
        tag.kind()
    }
}

impl TryFrom<JSWord> for MTag {
    type Error = ();

    fn try_from(value: JSWord) -> Result<Self, Self::Error> {
        MTag::try_from_word(value).ok_or(())
    }
}

impl TryFrom<JSWord> for MbTag {
    type Error = ();

    fn try_from(value: JSWord) -> Result<Self, Self::Error> {
        MTag::try_from_word(value).map(MbTag).ok_or(())
    }
}

// C: `JS_MB_HEADER` bitfield prefix in mquickjs_priv.h / mquickjs.c.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct MbHeader(JSWord);

impl MbHeader {
    pub const fn from_tag(tag: MbTag) -> Self {
        Self(tag.word() << JS_MTAG_SHIFT)
    }

    pub const fn from_kind(tag: MTag) -> Self {
        Self((tag as JSWord) << JS_MTAG_SHIFT)
    }

    pub const fn from_word(word: JSWord) -> Self {
        Self(word)
    }

    pub fn new(tag: MTag, gc_mark: bool) -> Self {
        Self::from_kind(tag).with_gc_mark(gc_mark)
    }

    pub const fn word(self) -> JSWord {
        self.0
    }

    pub fn tag(self) -> MTag {
        let tag = (self.0 & JS_MTAG_FIELD_MASK) >> JS_MTAG_SHIFT;
        match MTag::try_from_word(tag) {
            Some(kind) => kind,
            None => {
                debug_assert!(false, "invalid memblock header tag");
                MTag::Free
            }
        }
    }

    pub const fn tag_bits(self) -> JSWord {
        (self.0 & JS_MTAG_FIELD_MASK) >> JS_MTAG_SHIFT
    }

    pub const fn gc_mark(self) -> bool {
        (self.0 & 1) != 0
    }

    pub const fn with_gc_mark(self, gc_mark: bool) -> Self {
        Self((self.0 & !1) | (gc_mark as JSWord))
    }

    pub const fn value_array_header(size: JSWord) -> Self {
        Self::from_kind(MTag::ValueArray).with_value_array_size(size)
    }

    pub const fn value_array_size(self) -> JSWord {
        self.0 >> JS_MTAG_BITS
    }

    pub const fn with_value_array_size(self, size: JSWord) -> Self {
        Self(self.0 | (size << JS_MTAG_BITS))
    }
}

impl From<MbHeader> for JSWord {
    fn from(header: MbHeader) -> Self {
        header.word()
    }
}

impl From<JSWord> for MbHeader {
    fn from(word: JSWord) -> Self {
        MbHeader::from_word(word)
    }
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    #[test]
    fn mtag_fields_roundtrip() {
        let header = MbHeader::new(MTag::String, true);
        assert_eq!(header.tag(), MTag::String);
        assert!(header.gc_mark());
        assert_eq!(MbHeader::new(MTag::Object, false).tag(), MTag::Object);
        assert!(!MbHeader::new(MTag::Object, false).gc_mark());
    }

    #[test]
    fn mtag_mask_covers_all_tags() {
        assert_eq!(MTag::COUNT, JS_MTAG_MASK + 1);
        for tag in 0..MTag::COUNT {
            let header = MbHeader::new(MTag::try_from(tag).unwrap(), false);
            assert_eq!(header.tag_bits(), tag);
        }
    }

    #[test]
    fn value_array_header_roundtrip() {
        let size = 0x1234;
        let header = MbHeader::value_array_header(size);
        assert_eq!(header.tag(), MTag::ValueArray);
        assert_eq!(header.value_array_size(), size);
    }
}
