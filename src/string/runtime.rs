#![allow(dead_code)]

use crate::containers::StringHeader;
use crate::cutils::{unicode_to_utf8, utf8_get};
use crate::jsvalue::{JSValue, JSWord};
use crate::memblock::{MbHeader, MTag};
use core::mem::size_of;
use core::ptr;
use core::ptr::NonNull;
use core::slice;

// Invariants:
// - String bytes are stored as UTF-8, but may include surrogate code points.
// - UTF-8 positions are encoded as `byte_pos * 2 + surrogate_flag`.
// - JSValue::JS_TAG_STRING_CHAR stores a single code point encoded on demand.
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
    if val.get_special_tag() == JSValue::JS_TAG_STRING_CHAR {
        let codepoint = val.get_special_value() as u32;
        let len = unicode_to_utf8(scratch, codepoint);
        if len == 0 {
            return None;
        }
        return Some(StringView {
            bytes: &scratch[..len],
            is_ascii: codepoint <= 0x7f,
        });
    }
    if !val.is_ptr() {
        return None;
    }
    let ptr = val.to_ptr::<u8>()?;
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
    let top = ((buf[0] & 0x0f) as u32) << 6;
    let low = (buf[1] & 0x3f) as u32;
    (top | low) >= 0x10
}

pub(crate) fn append_utf8_with_surrogate_merge(out: &mut Vec<u8>, bytes: &[u8]) {
    if out.len() >= 3 && bytes.len() >= 3 {
        let left = &out[out.len() - 3..];
        let right = &bytes[..3];
        if is_utf8_left_surrogate(left) && is_utf8_right_surrogate(right) {
            let mut clen = 0usize;
            let left_cp = utf8_get(left, &mut clen);
            let right_cp = utf8_get(right, &mut clen);
            if left_cp >= 0 && right_cp >= 0 {
                let codepoint = 0x10000
                    + (((left_cp as u32) & 0x3ff) << 10)
                    + ((right_cp as u32) & 0x3ff);
                let mut buf = [0u8; 4];
                let len = unicode_to_utf8(&mut buf, codepoint);
                if len != 0 {
                    out.truncate(out.len() - 3);
                    out.extend_from_slice(&buf[..len]);
                    out.extend_from_slice(&bytes[3..]);
                    return;
                }
            }
        }
    }
    out.extend_from_slice(bytes);
}

fn is_utf8_left_surrogate(p: &[u8]) -> bool {
    p.len() >= 3 && p[0] == 0xed && (0xa0..=0xaf).contains(&p[1]) && p[2] >= 0x80
}

fn is_utf8_right_surrogate(p: &[u8]) -> bool {
    p.len() >= 3 && p[0] == 0xed && (0xb0..=0xbf).contains(&p[1]) && p[2] >= 0x80
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
