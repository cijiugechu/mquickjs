#![allow(dead_code)]

use crate::containers::StringHeader;
use crate::cutils::unicode_to_utf8;
use crate::jsvalue::{
    is_ptr, value_get_special_tag, value_get_special_value, value_to_ptr, JSValue, JSWord,
    JS_TAG_STRING_CHAR,
};
use crate::memblock::{MbHeader, MTag};
use core::mem::size_of;
use core::ptr;
use core::ptr::NonNull;
use core::slice;

// Invariants:
// - String bytes are stored as UTF-8, but may include surrogate code points.
// - UTF-8 positions are encoded as `byte_pos * 2 + surrogate_flag`.
// - JS_TAG_STRING_CHAR stores a single code point encoded on demand.
#[derive(Copy, Clone)]
pub(crate) struct StringView<'a> {
    bytes: &'a [u8],
    is_ascii: bool,
}

impl<'a> StringView<'a> {
    pub(crate) const fn bytes(self) -> &'a [u8] {
        self.bytes
    }

    pub(crate) const fn is_ascii(self) -> bool {
        self.is_ascii
    }
}

pub(crate) fn string_view<'a>(val: JSValue, scratch: &'a mut [u8; 5]) -> Option<StringView<'a>> {
    if value_get_special_tag(val) == JS_TAG_STRING_CHAR {
        let codepoint = value_get_special_value(val) as u32;
        let len = unicode_to_utf8(scratch, codepoint);
        if len == 0 {
            return None;
        }
        return Some(StringView {
            bytes: &scratch[..len],
            is_ascii: codepoint <= 0x7f,
        });
    }
    if !is_ptr(val) {
        return None;
    }
    let ptr = value_to_ptr::<u8>(val)?;
    let header = string_header(ptr)?;
    let bytes = string_bytes_from_ptr(ptr, header);
    Some(StringView {
        bytes,
        is_ascii: header.is_ascii(),
    })
}

pub(crate) fn utf8_char_len(c: u8) -> usize {
    if c < 0xc0 {
        1
    } else if c < 0xe0 {
        2
    } else if c < 0xf0 {
        3
    } else if c < 0xf8 {
        4
    } else {
        1
    }
}

pub(crate) fn is_valid_len4_utf8(buf: &[u8]) -> bool {
    if buf.len() < 2 {
        return false;
    }
    (((buf[0] & 0x0f) << 6) | (buf[1] & 0x3f)) >= 0x10
}

fn string_header(ptr: NonNull<u8>) -> Option<StringHeader> {
    let header_word = unsafe {
        // SAFETY: `ptr` points to readable header storage.
        ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
    };
    let header = MbHeader::from_word(header_word);
    if header.tag() != MTag::String {
        return None;
    }
    Some(StringHeader::from(header))
}

fn string_bytes_from_ptr<'a>(ptr: NonNull<u8>, header: StringHeader) -> &'a [u8] {
    let len = header.len() as usize;
    unsafe {
        // SAFETY: caller ensures `ptr` points to a string memblock with `len` bytes.
        slice::from_raw_parts(ptr.as_ptr().add(size_of::<JSWord>()), len)
    }
}
