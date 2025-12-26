use crate::cutils::{float64_as_uint64, uint64_as_float64};
use crate::tagged_ptr::TaggedPtr;
use core::ptr::NonNull;

#[cfg(target_pointer_width = "64")]
pub type JSWord = u64;
#[cfg(target_pointer_width = "32")]
pub type JSWord = u32;

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct JSValue(TaggedPtr);

pub const JSW: JSWord = core::mem::size_of::<JSValue>() as JSWord;

// Tagged value layout: low bits select tag, pointer values are addr + 1.
pub const JS_TAG_INT: JSWord = 0;
pub const JS_TAG_PTR: JSWord = 1;
pub const JS_TAG_SPECIAL: JSWord = 3;
pub const JS_TAG_BOOL: JSWord = JS_TAG_SPECIAL | ((0 as JSWord) << 2);
pub const JS_TAG_NULL: JSWord = JS_TAG_SPECIAL | ((1 as JSWord) << 2);
pub const JS_TAG_UNDEFINED: JSWord = JS_TAG_SPECIAL | ((2 as JSWord) << 2);
pub const JS_TAG_EXCEPTION: JSWord = JS_TAG_SPECIAL | ((3 as JSWord) << 2);
pub const JS_TAG_SHORT_FUNC: JSWord = JS_TAG_SPECIAL | ((4 as JSWord) << 2);
pub const JS_TAG_UNINITIALIZED: JSWord = JS_TAG_SPECIAL | ((5 as JSWord) << 2);
pub const JS_TAG_STRING_CHAR: JSWord = JS_TAG_SPECIAL | ((6 as JSWord) << 2);
pub const JS_TAG_CATCH_OFFSET: JSWord = JS_TAG_SPECIAL | ((7 as JSWord) << 2);

#[cfg(target_pointer_width = "64")]
pub const JS_TAG_SHORT_FLOAT: JSWord = 5;

pub const JS_TAG_SPECIAL_BITS: u32 = 5;

pub const JS_NULL: JSValue = value_make_special(JS_TAG_NULL, 0);
pub const JS_UNDEFINED: JSValue = value_make_special(JS_TAG_UNDEFINED, 0);
pub const JS_UNINITIALIZED: JSValue = value_make_special(JS_TAG_UNINITIALIZED, 0);
pub const JS_FALSE: JSValue = value_make_special(JS_TAG_BOOL, 0);
pub const JS_TRUE: JSValue = value_make_special(JS_TAG_BOOL, 1);

pub const JS_EX_NORMAL: u32 = 0;
pub const JS_EX_CALL: u32 = 1;
pub const JS_EXCEPTION: JSValue = value_make_special(JS_TAG_EXCEPTION, JS_EX_NORMAL);

pub const JS_SHORTINT_MIN: i32 = -(1 << 30);
pub const JS_SHORTINT_MAX: i32 = (1 << 30) - 1;

pub(crate) const fn from_bits(bits: JSWord) -> JSValue {
    JSValue(TaggedPtr::from_bits(bits as usize))
}

pub(crate) fn raw_bits(v: JSValue) -> JSWord {
    v.0.addr() as JSWord
}

pub const fn value_make_special(tag: JSWord, v: u32) -> JSValue {
    from_bits(tag | ((v as JSWord) << JS_TAG_SPECIAL_BITS))
}

#[cfg(target_pointer_width = "64")]
pub fn value_get_int(v: JSValue) -> i32 {
    ((raw_bits(v) as u32) as i32) >> 1
}

#[cfg(target_pointer_width = "32")]
pub fn value_get_int(v: JSValue) -> i32 {
    (raw_bits(v) as i32) >> 1
}

#[cfg(target_pointer_width = "64")]
pub fn value_get_special_value(v: JSValue) -> i32 {
    ((raw_bits(v) as u32) as i32) >> JS_TAG_SPECIAL_BITS
}

#[cfg(target_pointer_width = "32")]
pub fn value_get_special_value(v: JSValue) -> i32 {
    (raw_bits(v) as i32) >> JS_TAG_SPECIAL_BITS
}

pub fn value_get_special_tag(v: JSValue) -> JSWord {
    raw_bits(v) & (((1 as JSWord) << JS_TAG_SPECIAL_BITS) - 1)
}

pub const fn new_short_int(val: i32) -> JSValue {
    from_bits(((val as u32 as JSWord) << 1) | JS_TAG_INT)
}

pub const fn new_bool(val: i32) -> JSValue {
    value_make_special(JS_TAG_BOOL, (val != 0) as u32)
}

pub fn is_int(v: JSValue) -> bool {
    (raw_bits(v) & 1) == JS_TAG_INT
}

pub fn is_ptr(v: JSValue) -> bool {
    (raw_bits(v) & (JSW - 1)) == JS_TAG_PTR
}

#[cfg(target_pointer_width = "64")]
pub fn is_short_float(v: JSValue) -> bool {
    (raw_bits(v) & (JSW - 1)) == JS_TAG_SHORT_FLOAT
}

pub fn is_bool(v: JSValue) -> bool {
    value_get_special_tag(v) == JS_TAG_BOOL
}

pub fn is_null(v: JSValue) -> bool {
    raw_bits(v) == raw_bits(JS_NULL)
}

pub fn is_undefined(v: JSValue) -> bool {
    raw_bits(v) == raw_bits(JS_UNDEFINED)
}

pub fn is_uninitialized(v: JSValue) -> bool {
    raw_bits(v) == raw_bits(JS_UNINITIALIZED)
}

pub fn is_exception(v: JSValue) -> bool {
    raw_bits(v) == raw_bits(JS_EXCEPTION)
}

pub fn value_from_ptr<T>(ptr: NonNull<T>) -> JSValue {
    JSValue(TaggedPtr::from_ptr(
        ptr,
        JS_TAG_PTR as usize,
        (JSW - 1) as usize,
    ))
}

pub fn value_from_ptr_raw<T>(ptr: *mut T) -> Option<JSValue> {
    NonNull::new(ptr).map(value_from_ptr)
}

pub fn value_to_ptr<T>(val: JSValue) -> Option<NonNull<T>> {
    val.0.to_ptr(JS_TAG_PTR as usize, (JSW - 1) as usize)
}

#[cfg(target_pointer_width = "64")]
const JS_FLOAT64_VALUE_EXP_MIN: i64 = 1023 - 127;
#[cfg(target_pointer_width = "64")]
const JS_FLOAT64_VALUE_ADDEND: u64 =
    ((JS_FLOAT64_VALUE_EXP_MIN - ((JS_TAG_SHORT_FLOAT as i64) << 8)) as u64) << 52;

#[cfg(target_pointer_width = "64")]
pub fn short_float_to_f64(v: JSValue) -> f64 {
    let bits = raw_bits(v) as u64;
    uint64_as_float64(bits.rotate_left(60).wrapping_add(JS_FLOAT64_VALUE_ADDEND))
}

#[cfg(target_pointer_width = "64")]
pub fn short_float_from_f64(d: f64) -> JSValue {
    from_bits(
        float64_as_uint64(d)
        .wrapping_sub(JS_FLOAT64_VALUE_ADDEND)
        .rotate_left(4) as JSWord,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::mem;

    #[test]
    fn word_size_matches_value() {
        assert_eq!(JSW as usize, mem::size_of::<JSValue>());
    }

    #[test]
    fn special_tag_roundtrip() {
        let val = value_make_special(JS_TAG_STRING_CHAR, 0x1f642);
        assert_eq!(value_get_special_tag(val), JS_TAG_STRING_CHAR);
        assert_eq!(value_get_special_value(val), 0x1f642);
    }

    #[test]
    fn bool_values_match_tags() {
        assert!(is_bool(JS_FALSE));
        assert!(is_bool(JS_TRUE));
        assert_eq!(value_get_special_value(JS_FALSE), 0);
        assert_eq!(value_get_special_value(JS_TRUE), 1);
    }

    #[test]
    fn null_undefined_uninitialized() {
        assert!(is_null(JS_NULL));
        assert!(is_undefined(JS_UNDEFINED));
        assert!(is_uninitialized(JS_UNINITIALIZED));
    }

    #[test]
    fn int_roundtrip() {
        let samples = [JS_SHORTINT_MIN, -1, 0, 1, 123, JS_SHORTINT_MAX];
        for &val in &samples {
            let tagged = new_short_int(val);
            assert!(is_int(tagged));
            assert_eq!(value_get_int(tagged), val);
        }
    }

    #[test]
    fn ptr_roundtrip() {
        let mut value = 0u64;
        let ptr = NonNull::new(&mut value as *mut u64).expect("non-null");
        let tagged = value_from_ptr(ptr);
        assert!(is_ptr(tagged));
        assert_eq!(value_to_ptr::<u64>(tagged), Some(ptr));
    }

    #[cfg(target_pointer_width = "64")]
    #[test]
    fn short_float_roundtrip() {
        let d = 1.5_f64;
        let tagged = short_float_from_f64(d);
        assert!(is_short_float(tagged));
        assert_eq!(short_float_to_f64(tagged).to_bits(), d.to_bits());
    }
}
