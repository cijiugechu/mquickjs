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
