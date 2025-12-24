use crate::jsvalue::JSValue;

// C: `JSROMClass` in mquickjs.c.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct RomClass {
    props: JSValue,
    ctor_idx: i32,
    proto_props: JSValue,
    parent_class: JSValue,
}

impl RomClass {
    pub const fn new(props: JSValue, ctor_idx: i32, proto_props: JSValue, parent_class: JSValue) -> Self {
        Self {
            props,
            ctor_idx,
            proto_props,
            parent_class,
        }
    }

    pub const fn props(self) -> JSValue {
        self.props
    }

    pub const fn ctor_idx(self) -> i32 {
        self.ctor_idx
    }

    pub const fn proto_props(self) -> JSValue {
        self.proto_props
    }

    pub const fn parent_class(self) -> JSValue {
        self.parent_class
    }
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    #[test]
    fn rom_class_roundtrip() {
        let rc = RomClass::new(crate::jsvalue::JS_NULL, -1, crate::jsvalue::JS_UNDEFINED, crate::jsvalue::JS_NULL);
        assert_eq!(rc.props(), crate::jsvalue::JS_NULL);
        assert_eq!(rc.ctor_idx(), -1);
        assert_eq!(rc.proto_props(), crate::jsvalue::JS_UNDEFINED);
        assert_eq!(rc.parent_class(), crate::jsvalue::JS_NULL);
    }
}
