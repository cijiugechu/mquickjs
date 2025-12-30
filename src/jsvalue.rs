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

impl JSValue {
    pub const JSW: JSWord = core::mem::size_of::<JSValue>() as JSWord;

    // Tagged value layout: low bits select tag, pointer values are addr + 1.
    pub const JS_TAG_INT: JSWord = 0;
    pub const JS_TAG_PTR: JSWord = 1;
    pub const JS_TAG_SPECIAL: JSWord = 3;
    pub const JS_TAG_BOOL: JSWord = Self::JS_TAG_SPECIAL | ((0 as JSWord) << 2);
    pub const JS_TAG_NULL: JSWord = Self::JS_TAG_SPECIAL | ((1 as JSWord) << 2);
    pub const JS_TAG_UNDEFINED: JSWord = Self::JS_TAG_SPECIAL | ((2 as JSWord) << 2);
    pub const JS_TAG_EXCEPTION: JSWord = Self::JS_TAG_SPECIAL | ((3 as JSWord) << 2);
    pub const JS_TAG_SHORT_FUNC: JSWord = Self::JS_TAG_SPECIAL | ((4 as JSWord) << 2);
    pub const JS_TAG_UNINITIALIZED: JSWord = Self::JS_TAG_SPECIAL | ((5 as JSWord) << 2);
    pub const JS_TAG_STRING_CHAR: JSWord = Self::JS_TAG_SPECIAL | ((6 as JSWord) << 2);
    pub const JS_TAG_CATCH_OFFSET: JSWord = Self::JS_TAG_SPECIAL | ((7 as JSWord) << 2);

    #[cfg(target_pointer_width = "64")]
    pub const JS_TAG_SHORT_FLOAT: JSWord = 5;

    pub const JS_TAG_SPECIAL_BITS: u32 = 5;

    pub const JS_NULL: JSValue = Self::value_make_special(Self::JS_TAG_NULL, 0);
    pub const JS_UNDEFINED: JSValue = Self::value_make_special(Self::JS_TAG_UNDEFINED, 0);
    pub const JS_UNINITIALIZED: JSValue = Self::value_make_special(Self::JS_TAG_UNINITIALIZED, 0);
    pub const JS_FALSE: JSValue = Self::value_make_special(Self::JS_TAG_BOOL, 0);
    pub const JS_TRUE: JSValue = Self::value_make_special(Self::JS_TAG_BOOL, 1);

    pub const JS_EX_NORMAL: u32 = 0;
    pub const JS_EX_CALL: u32 = 1;
    pub const JS_EXCEPTION: JSValue =
        Self::value_make_special(Self::JS_TAG_EXCEPTION, Self::JS_EX_NORMAL);

    pub const JS_SHORTINT_MIN: i32 = -(1 << 30);
    pub const JS_SHORTINT_MAX: i32 = (1 << 30) - 1;

    pub(crate) const fn from_bits(bits: JSWord) -> JSValue {
        JSValue(TaggedPtr::from_bits(bits as usize))
    }

    pub(crate) fn raw_bits(&self) -> JSWord {
        self.0.addr() as JSWord
    }

    pub const fn value_make_special(tag: JSWord, v: u32) -> JSValue {
        Self::from_bits(tag | ((v as JSWord) << Self::JS_TAG_SPECIAL_BITS))
    }

    #[cfg(target_pointer_width = "64")]
    pub fn get_int(&self) -> i32 {
        ((self.raw_bits() as u32) as i32) >> 1
    }

    #[cfg(target_pointer_width = "32")]
    pub fn get_int(&self) -> i32 {
        (self.raw_bits() as i32) >> 1
    }

    #[cfg(target_pointer_width = "64")]
    pub fn get_special_value(&self) -> i32 {
        ((self.raw_bits() as u32) as i32) >> Self::JS_TAG_SPECIAL_BITS
    }

    #[cfg(target_pointer_width = "32")]
    pub fn get_special_value(&self) -> i32 {
        (self.raw_bits() as i32) >> Self::JS_TAG_SPECIAL_BITS
    }

    pub fn get_special_tag(&self) -> JSWord {
        self.raw_bits() & (((1 as JSWord) << Self::JS_TAG_SPECIAL_BITS) - 1)
    }

    pub const fn new_short_int(val: i32) -> JSValue {
        Self::from_bits(((val as u32 as JSWord) << 1) | Self::JS_TAG_INT)
    }

    pub const fn new_bool(val: i32) -> JSValue {
        Self::value_make_special(Self::JS_TAG_BOOL, (val != 0) as u32)
    }

    pub fn is_int(&self) -> bool {
        (self.raw_bits() & 1) == Self::JS_TAG_INT
    }

    pub fn is_ptr(&self) -> bool {
        (self.raw_bits() & (Self::JSW - 1)) == Self::JS_TAG_PTR
    }

    #[cfg(target_pointer_width = "64")]
    pub fn is_short_float(&self) -> bool {
        (self.raw_bits() & (Self::JSW - 1)) == Self::JS_TAG_SHORT_FLOAT
    }

    pub fn is_bool(&self) -> bool {
        self.get_special_tag() == Self::JS_TAG_BOOL
    }

    pub fn is_null(&self) -> bool {
        self.raw_bits() == Self::JS_NULL.raw_bits()
    }

    pub fn is_undefined(&self) -> bool {
        self.raw_bits() == Self::JS_UNDEFINED.raw_bits()
    }

    pub fn is_uninitialized(&self) -> bool {
        self.raw_bits() == Self::JS_UNINITIALIZED.raw_bits()
    }

    pub fn is_exception(&self) -> bool {
        self.raw_bits() == Self::JS_EXCEPTION.raw_bits()
    }
    pub fn from_ptr<T>(ptr: NonNull<T>) -> JSValue {
        JSValue(TaggedPtr::from_ptr(
            ptr,
            Self::JS_TAG_PTR as usize,
            (Self::JSW - 1) as usize,
        ))
    }

    pub fn from_ptr_raw<T>(ptr: *mut T) -> Option<JSValue> {
        NonNull::new(ptr).map(Self::from_ptr)
    }

    pub fn to_ptr<T>(&self) -> Option<NonNull<T>> {
        self.0
            .to_ptr(Self::JS_TAG_PTR as usize, (Self::JSW - 1) as usize)
    }

    #[cfg(target_pointer_width = "64")]
    const JS_FLOAT64_VALUE_EXP_MIN: i64 = 1023 - 127;
    #[cfg(target_pointer_width = "64")]
    const JS_FLOAT64_VALUE_ADDEND: u64 =
        ((Self::JS_FLOAT64_VALUE_EXP_MIN - ((Self::JS_TAG_SHORT_FLOAT as i64) << 8)) as u64)
            << 52;

    #[cfg(target_pointer_width = "64")]
    pub fn short_float_to_f64(&self) -> f64 {
        let bits = self.raw_bits();
        uint64_as_float64(bits.rotate_left(60).wrapping_add(Self::JS_FLOAT64_VALUE_ADDEND))
    }

    #[cfg(target_pointer_width = "64")]
    pub fn short_float_from_f64(d: f64) -> JSValue {
        Self::from_bits(
            float64_as_uint64(d)
            .wrapping_sub(Self::JS_FLOAT64_VALUE_ADDEND)
            .rotate_left(4) as JSWord,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::mem;

    #[test]
    fn word_size_matches_value() {
        assert_eq!(JSValue::JSW as usize, mem::size_of::<JSValue>());
    }

    #[test]
    fn bool_values_match_tags() {
        assert!(JSValue::JS_FALSE.is_bool());
        assert!(JSValue::JS_TRUE.is_bool());
        assert_eq!(JSValue::JS_FALSE.get_special_value(), 0);
        assert_eq!(JSValue::JS_TRUE.get_special_value(), 1);
    }

    #[test]
    fn null_undefined_uninitialized() {
        assert!(JSValue::JS_NULL.is_null());
        assert!(JSValue::JS_UNDEFINED.is_undefined());
        assert!(JSValue::JS_UNINITIALIZED.is_uninitialized());
    }

}
