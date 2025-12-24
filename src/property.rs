use crate::enums::JSPropType;

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
}
