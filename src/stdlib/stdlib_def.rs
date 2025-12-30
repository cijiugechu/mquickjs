use crate::jsvalue::JSValue;

// Rust-only function prototypes for builtin dispatch, derived from stdlib metadata.
#[repr(u8)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BuiltinProto {
    Generic = 0,
    GenericMagic = 1,
    Constructor = 2,
    ConstructorMagic = 3,
    GenericParams = 4,
    FF = 5,
}

impl BuiltinProto {
    pub fn from_cproto_name(name: &str) -> Option<Self> {
        match name {
            "generic" => Some(Self::Generic),
            "generic_magic" => Some(Self::GenericMagic),
            "constructor" => Some(Self::Constructor),
            "constructor_magic" => Some(Self::ConstructorMagic),
            "generic_params" => Some(Self::GenericParams),
            "f_f" => Some(Self::FF),
            _ => None,
        }
    }
}

pub const JS_BYTECODE_MAGIC: u16 = 0xacfb;
pub const JS_BYTECODE_VERSION_32: u16 = 0x0001;
pub const JS_BYTECODE_VERSION: u16 = JS_BYTECODE_VERSION_32 | (((JSValue::JSW as u16) & 8) << 12);

// C: `JSBytecodeHeader` in mquickjs.h.
#[repr(C)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct BytecodeHeader {
    pub magic: u16,
    pub version: u16,
    pub base_addr: usize,
    pub unique_strings: JSValue,
    pub main_func: JSValue,
}

// C: `JSBytecodeHeader32` in mquickjs.h (only used on 64-bit hosts).
#[cfg(target_pointer_width = "64")]
#[repr(C)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct BytecodeHeader32 {
    pub magic: u16,
    pub version: u16,
    pub base_addr: u32,
    pub unique_strings: u32,
    pub main_func: u32,
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;
    use core::mem::size_of;

    #[test]
    fn builtin_proto_discriminants() {
        assert_eq!(BuiltinProto::Generic as u8, 0);
        assert_eq!(BuiltinProto::GenericMagic as u8, 1);
        assert_eq!(BuiltinProto::Constructor as u8, 2);
        assert_eq!(BuiltinProto::ConstructorMagic as u8, 3);
        assert_eq!(BuiltinProto::GenericParams as u8, 4);
        assert_eq!(BuiltinProto::FF as u8, 5);
    }

    #[test]
    fn builtin_proto_from_cproto_name() {
        assert_eq!(BuiltinProto::from_cproto_name("generic"), Some(BuiltinProto::Generic));
        assert_eq!(BuiltinProto::from_cproto_name("generic_magic"), Some(BuiltinProto::GenericMagic));
        assert_eq!(BuiltinProto::from_cproto_name("constructor"), Some(BuiltinProto::Constructor));
        assert_eq!(BuiltinProto::from_cproto_name("constructor_magic"), Some(BuiltinProto::ConstructorMagic));
        assert_eq!(BuiltinProto::from_cproto_name("generic_params"), Some(BuiltinProto::GenericParams));
        assert_eq!(BuiltinProto::from_cproto_name("f_f"), Some(BuiltinProto::FF));
        assert_eq!(BuiltinProto::from_cproto_name("unknown"), None);
    }

    #[test]
    fn bytecode_version_matches_pointer_width() {
        #[cfg(target_pointer_width = "64")]
        {
            assert_eq!(JS_BYTECODE_VERSION & 0x8000, 0x8000);
        }
        #[cfg(target_pointer_width = "32")]
        {
            assert_eq!(JS_BYTECODE_VERSION & 0x8000, 0);
        }
    }

    #[test]
    fn bytecode_header_sizes() {
        #[cfg(target_pointer_width = "64")]
        {
            assert_eq!(size_of::<BytecodeHeader>(), 32);
        }
        #[cfg(target_pointer_width = "32")]
        {
            assert_eq!(size_of::<BytecodeHeader>(), 16);
        }
    }

    #[cfg(target_pointer_width = "64")]
    #[test]
    fn bytecode_header32_size() {
        assert_eq!(size_of::<BytecodeHeader32>(), 16);
    }
}
