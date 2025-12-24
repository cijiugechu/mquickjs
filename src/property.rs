use crate::enums::JSPropType;
use crate::jsvalue::JSValue;

// C: `JSProperty` bitfields in mquickjs.c (`hash_next:30`, `prop_type:2`).
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct PropertyMeta(u32);

impl PropertyMeta {
    pub const HASH_NEXT_BITS: u32 = 30;
    pub const HASH_NEXT_MASK: u32 = (1 << Self::HASH_NEXT_BITS) - 1;

    pub fn new(hash_next: u32, prop_type: JSPropType) -> Self {
        debug_assert!(hash_next <= Self::HASH_NEXT_MASK);
        let type_bits = (prop_type as u32) & 0x3;
        Self((hash_next & Self::HASH_NEXT_MASK) | (type_bits << Self::HASH_NEXT_BITS))
    }

    pub const fn hash_next(self) -> u32 {
        self.0 & Self::HASH_NEXT_MASK
    }

    pub fn prop_type(self) -> JSPropType {
        match (self.0 >> Self::HASH_NEXT_BITS) & 0x3 {
            0 => JSPropType::Normal,
            1 => JSPropType::GetSet,
            2 => JSPropType::VarRef,
            _ => JSPropType::Special,
        }
    }
}

// C: `JSProperty` in mquickjs.c.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Property {
    key: JSValue,
    value: JSValue,
    meta: PropertyMeta,
}

impl Property {
    pub const fn new(key: JSValue, value: JSValue, meta: PropertyMeta) -> Self {
        Self { key, value, meta }
    }

    pub const fn key(self) -> JSValue {
        self.key
    }

    pub const fn value(self) -> JSValue {
        self.value
    }

    pub const fn meta(self) -> PropertyMeta {
        self.meta
    }
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    #[test]
    fn property_meta_roundtrip() {
        let meta = PropertyMeta::new(123, JSPropType::VarRef);
        assert_eq!(meta.hash_next(), 123);
        assert_eq!(meta.prop_type(), JSPropType::VarRef);
    }

    #[test]
    fn property_meta_masks_hash() {
        let meta = PropertyMeta::new(PropertyMeta::HASH_NEXT_MASK, JSPropType::Normal);
        assert_eq!(meta.hash_next(), PropertyMeta::HASH_NEXT_MASK);
    }

    #[test]
    fn property_roundtrip() {
        let meta = PropertyMeta::new(7, JSPropType::GetSet);
        let prop = Property::new(crate::jsvalue::JS_NULL, crate::jsvalue::JS_UNDEFINED, meta);
        assert_eq!(prop.key(), crate::jsvalue::JS_NULL);
        assert_eq!(prop.value(), crate::jsvalue::JS_UNDEFINED);
        assert_eq!(prop.meta(), meta);
    }
}
