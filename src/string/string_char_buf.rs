use crate::containers::StringHeader;
use crate::jsvalue::JSWord;

// C: `JSStringCharBuf` in mquickjs.c.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct StringCharBuf {
    string_buf: [JSWord; Self::STRING_BUF_WORDS],
    buf: [u8; 5],
}

impl StringCharBuf {
    pub const STRING_BUF_WORDS: usize =
        core::mem::size_of::<StringHeader>() / core::mem::size_of::<JSWord>();

    pub const fn new() -> Self {
        Self {
            string_buf: [0; Self::STRING_BUF_WORDS],
            buf: [0; 5],
        }
    }

    pub const fn buf(self) -> [u8; 5] {
        self.buf
    }

    pub const fn string_buf(self) -> [JSWord; Self::STRING_BUF_WORDS] {
        self.string_buf
    }
}

impl Default for StringCharBuf {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    #[test]
    fn string_char_buf_defaults() {
        let buf = StringCharBuf::new();
        assert_eq!(buf.buf(), [0; 5]);
        assert_eq!(buf.string_buf(), [0; StringCharBuf::STRING_BUF_WORDS]);
    }

    #[test]
    fn string_char_buf_word_count_matches_c() {
        let words = core::mem::size_of::<StringHeader>() / core::mem::size_of::<JSWord>();
        assert_eq!(StringCharBuf::STRING_BUF_WORDS, words);
    }
}
