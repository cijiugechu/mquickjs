use crate::containers::{StringHeader, JS_STRING_LEN_MAX};
use crate::jsvalue::JSWord;
use core::cell::Cell;

#[derive(Debug)]
pub struct JSString {
    header: Cell<StringHeader>,
    buf: Vec<u8>,
}

impl JSString {
    pub fn new(buf: Vec<u8>, is_unique: bool, is_ascii: bool, is_numeric: bool) -> Option<Self> {
        let len = buf.len();
        if len > JS_STRING_LEN_MAX as usize {
            return None;
        }
        // Keep raw bytes only; callers use the header length for bounds.
        let header = StringHeader::new(len as JSWord, is_unique, is_ascii, is_numeric, false);
        Some(Self {
            header: Cell::new(header),
            buf,
        })
    }

    pub fn header(&self) -> StringHeader {
        self.header.get()
    }

    pub fn len(&self) -> usize {
        self.header.get().len() as usize
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn is_ascii(&self) -> bool {
        self.header.get().is_ascii()
    }

    pub fn is_unique(&self) -> bool {
        self.header.get().is_unique()
    }

    pub fn is_numeric(&self) -> bool {
        self.header.get().is_numeric()
    }

    pub fn set_unique_flags(&self, is_unique: bool, is_numeric: bool) {
        let header = self.header.get();
        let len = header.len();
        let is_ascii = header.is_ascii();
        let gc_mark = header.header().gc_mark();
        self.header
            .set(StringHeader::new(len, is_unique, is_ascii, is_numeric, gc_mark));
    }

    pub fn buf(&self) -> &[u8] {
        &self.buf[..self.len()]
    }
}

pub fn is_ascii_bytes(buf: &[u8]) -> bool {
    buf.iter().all(|b| *b <= 0x7f)
}
