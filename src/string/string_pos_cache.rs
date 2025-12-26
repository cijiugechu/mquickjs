use crate::jsvalue::JSValue;

// C: `JS_STRING_POS_CACHE_SIZE` in mquickjs.c.
pub const JS_STRING_POS_CACHE_SIZE: usize = 2;
// C: `JS_STRING_POS_CACHE_MIN_LEN` in mquickjs.c.
pub const JS_STRING_POS_CACHE_MIN_LEN: u32 = 16;

// C: `StringPosTypeEnum` in mquickjs.c.
#[repr(u8)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum StringPosType {
    Utf8 = 0,
    Utf16 = 1,
}

// C: `JSStringPosCacheEntry` in mquickjs.c.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct StringPosCacheEntry {
    str: JSValue,
    str_pos: [u32; 2],
}

impl StringPosCacheEntry {
    pub const fn new(str: JSValue, utf8_pos: u32, utf16_pos: u32) -> Self {
        Self {
            str,
            str_pos: [utf8_pos, utf16_pos],
        }
    }

    pub const fn str(self) -> JSValue {
        self.str
    }

    pub fn set_str(&mut self, val: JSValue) {
        self.str = val;
    }

    pub(crate) fn str_ptr(&mut self) -> *mut JSValue {
        &mut self.str
    }

    pub const fn utf8_pos(self) -> u32 {
        self.str_pos[0]
    }

    pub const fn utf16_pos(self) -> u32 {
        self.str_pos[1]
    }
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    #[test]
    fn string_pos_cache_constants() {
        assert_eq!(JS_STRING_POS_CACHE_SIZE, 2);
        assert_eq!(JS_STRING_POS_CACHE_MIN_LEN, 16);
        assert_eq!(StringPosType::Utf8 as u8, 0);
        assert_eq!(StringPosType::Utf16 as u8, 1);
    }

    #[test]
    fn entry_roundtrip() {
        let entry = StringPosCacheEntry::new(crate::jsvalue::JS_NULL, 3, 7);
        assert_eq!(entry.str(), crate::jsvalue::JS_NULL);
        assert_eq!(entry.utf8_pos(), 3);
        assert_eq!(entry.utf16_pos(), 7);
    }
}
