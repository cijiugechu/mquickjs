use crate::jsvalue::{JSValue, JSWord, JSW};
use crate::memblock::{mb_header, JS_MTAG_BITS, JS_MTAG_BYTE_ARRAY, JS_MTAG_STRING, JS_MTAG_VALUE_ARRAY, JS_MTAG_VARREF};

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

pub fn string_header(len: JSWord, is_unique: bool, is_ascii: bool, is_numeric: bool, gc_mark: bool) -> JSWord {
    debug_assert!(len <= JS_STRING_LEN_MAX);
    let mut header = mb_header(JS_MTAG_STRING, gc_mark);
    header |= (is_unique as JSWord) << JS_STRING_FLAG_UNIQUE_SHIFT;
    header |= (is_ascii as JSWord) << JS_STRING_FLAG_ASCII_SHIFT;
    header |= (is_numeric as JSWord) << JS_STRING_FLAG_NUMERIC_SHIFT;
    header | (len << JS_STRING_LEN_SHIFT)
}

pub fn string_len(header: JSWord) -> JSWord {
    header >> JS_STRING_LEN_SHIFT
}

pub fn string_is_unique(header: JSWord) -> bool {
    (header >> JS_STRING_FLAG_UNIQUE_SHIFT) & 1 != 0
}

pub fn string_is_ascii(header: JSWord) -> bool {
    (header >> JS_STRING_FLAG_ASCII_SHIFT) & 1 != 0
}

pub fn string_is_numeric(header: JSWord) -> bool {
    (header >> JS_STRING_FLAG_NUMERIC_SHIFT) & 1 != 0
}

pub fn string_alloc_size(len: JSWord) -> usize {
    debug_assert!(len <= JS_STRING_LEN_MAX);
    let len_plus_nul = (len as usize).saturating_add(1);
    let aligned = len_plus_nul.next_multiple_of(JSW as usize);
    aligned + core::mem::size_of::<JSWord>()
}

pub fn byte_array_header(size: JSWord, gc_mark: bool) -> JSWord {
    debug_assert!(size <= JS_BYTE_ARRAY_SIZE_MAX);
    mb_header(JS_MTAG_BYTE_ARRAY, gc_mark) | (size << JS_MTAG_BITS)
}

pub fn byte_array_size(header: JSWord) -> JSWord {
    header >> JS_MTAG_BITS
}

pub fn byte_array_alloc_size(size: JSWord) -> usize {
    debug_assert!(size <= JS_BYTE_ARRAY_SIZE_MAX);
    core::mem::size_of::<JSWord>() + size as usize
}

pub fn value_array_header(size: JSWord, gc_mark: bool) -> JSWord {
    debug_assert!(size <= JS_VALUE_ARRAY_SIZE_MAX);
    mb_header(JS_MTAG_VALUE_ARRAY, gc_mark) | (size << JS_MTAG_BITS)
}

pub fn value_array_size(header: JSWord) -> JSWord {
    header >> JS_MTAG_BITS
}

pub fn value_array_alloc_size(size: JSWord) -> usize {
    debug_assert!(size <= JS_VALUE_ARRAY_SIZE_MAX);
    core::mem::size_of::<JSWord>() + (size as usize) * core::mem::size_of::<JSValue>()
}

pub fn var_ref_header(is_detached: bool, gc_mark: bool) -> JSWord {
    mb_header(JS_MTAG_VARREF, gc_mark) | ((is_detached as JSWord) << JS_MTAG_BITS)
}

pub fn var_ref_is_detached(header: JSWord) -> bool {
    (header >> JS_MTAG_BITS) & 1 != 0
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    #[test]
    fn string_header_roundtrip() {
        let header = string_header(42, true, false, true, true);
        assert_eq!(string_len(header), 42);
        assert!(string_is_unique(header));
        assert!(!string_is_ascii(header));
        assert!(string_is_numeric(header));
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
        let header = byte_array_header(128, false);
        assert_eq!(byte_array_size(header), 128);
    }

    #[test]
    fn value_array_header_roundtrip() {
        let header = value_array_header(7, true);
        assert_eq!(value_array_size(header), 7);
    }

    #[test]
    fn var_ref_header_roundtrip() {
        let header = var_ref_header(true, false);
        assert!(var_ref_is_detached(header));
    }
}
