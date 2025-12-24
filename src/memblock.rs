use crate::jsvalue::JSWord;

pub const JS_MTAG_BITS: u32 = 4;
pub const JS_MTAG_SHIFT: u32 = 1;
pub const JS_MTAG_WIDTH: u32 = JS_MTAG_BITS - 1;
pub const JS_MTAG_MASK: JSWord = ((1 as JSWord) << JS_MTAG_WIDTH) - 1;
pub const JS_MTAG_FIELD_MASK: JSWord = JS_MTAG_MASK << JS_MTAG_SHIFT;

pub const JS_MTAG_FREE: JSWord = 0;
pub const JS_MTAG_OBJECT: JSWord = 1;
pub const JS_MTAG_FLOAT64: JSWord = 2;
pub const JS_MTAG_STRING: JSWord = 3;
pub const JS_MTAG_FUNCTION_BYTECODE: JSWord = 4;
pub const JS_MTAG_VALUE_ARRAY: JSWord = 5;
pub const JS_MTAG_BYTE_ARRAY: JSWord = 6;
pub const JS_MTAG_VARREF: JSWord = 7;
pub const JS_MTAG_COUNT: JSWord = 8;

pub const fn mb_header_def(tag: JSWord) -> JSWord {
    tag << JS_MTAG_SHIFT
}

pub fn mb_header(tag: JSWord, gc_mark: bool) -> JSWord {
    debug_assert!((tag & !JS_MTAG_MASK) == 0);
    mb_header_def(tag) | (gc_mark as JSWord)
}

pub fn mb_header_tag(header: JSWord) -> JSWord {
    (header & JS_MTAG_FIELD_MASK) >> JS_MTAG_SHIFT
}

pub fn mb_header_gc_mark(header: JSWord) -> bool {
    (header & 1) != 0
}

pub fn value_array_header(size: JSWord) -> JSWord {
    mb_header_def(JS_MTAG_VALUE_ARRAY) | (size << JS_MTAG_BITS)
}

pub fn value_array_size(header: JSWord) -> JSWord {
    header >> JS_MTAG_BITS
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    #[test]
    fn mtag_fields_roundtrip() {
        let header = mb_header(JS_MTAG_STRING, true);
        assert_eq!(mb_header_tag(header), JS_MTAG_STRING);
        assert!(mb_header_gc_mark(header));
        assert_eq!(mb_header_tag(mb_header(JS_MTAG_OBJECT, false)), JS_MTAG_OBJECT);
        assert!(!mb_header_gc_mark(mb_header(JS_MTAG_OBJECT, false)));
    }

    #[test]
    fn mtag_mask_covers_all_tags() {
        assert_eq!(JS_MTAG_COUNT, JS_MTAG_MASK + 1);
        for tag in 0..JS_MTAG_COUNT {
            let header = mb_header(tag, false);
            assert_eq!(mb_header_tag(header), tag);
        }
    }

    #[test]
    fn value_array_header_roundtrip() {
        let size = 0x1234;
        let header = value_array_header(size);
        assert_eq!(mb_header_tag(header), JS_MTAG_VALUE_ARRAY);
        assert_eq!(value_array_size(header), size);
    }
}
