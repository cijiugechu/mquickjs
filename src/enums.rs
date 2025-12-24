// Minimal enum ports from mquickjs_priv.h / mquickjs.c.

// C: `JSPropTypeEnum` in mquickjs_priv.h.
#[repr(u8)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum JSPropType {
    Normal = 0,
    GetSet = 1,
    VarRef = 2,
    Special = 3,
}

// C: `JSVarRefKindEnum` in mquickjs.c.
#[repr(u8)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum JSVarRefKind {
    Arg = 0,
    Var = 1,
    VarRef = 2,
    Global = 3,
}

// C: `JSObjectClassEnum` in mquickjs.h.
#[repr(u8)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum JSObjectClass {
    Object = 0,
    Array = 1,
    CFunction = 2,
    Closure = 3,
    Number = 4,
    Boolean = 5,
    String = 6,
    Date = 7,
    RegExp = 8,
    Error = 9,
    EvalError = 10,
    RangeError = 11,
    ReferenceError = 12,
    SyntaxError = 13,
    TypeError = 14,
    UriError = 15,
    InternalError = 16,
    ArrayBuffer = 17,
    TypedArray = 18,
    Uint8CArray = 19,
    Int8Array = 20,
    Uint8Array = 21,
    Int16Array = 22,
    Uint16Array = 23,
    Int32Array = 24,
    Uint32Array = 25,
    Float32Array = 26,
    Float64Array = 27,
    User = 28,
}

// C: `JSCFunctionEnum` in mquickjs.h.
#[repr(u8)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum JSCFunction {
    Bound = 0,
    User = 1,
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    #[test]
    fn enum_discriminants_match_c() {
        assert_eq!(JSPropType::Normal as u8, 0);
        assert_eq!(JSPropType::GetSet as u8, 1);
        assert_eq!(JSPropType::VarRef as u8, 2);
        assert_eq!(JSPropType::Special as u8, 3);
        assert_eq!(JSVarRefKind::Arg as u8, 0);
        assert_eq!(JSVarRefKind::Var as u8, 1);
        assert_eq!(JSVarRefKind::VarRef as u8, 2);
        assert_eq!(JSVarRefKind::Global as u8, 3);
    }

    #[test]
    fn object_class_discriminants_match_c() {
        assert_eq!(JSObjectClass::Object as u8, 0);
        assert_eq!(JSObjectClass::Array as u8, 1);
        assert_eq!(JSObjectClass::CFunction as u8, 2);
        assert_eq!(JSObjectClass::Closure as u8, 3);
        assert_eq!(JSObjectClass::Number as u8, 4);
        assert_eq!(JSObjectClass::Boolean as u8, 5);
        assert_eq!(JSObjectClass::String as u8, 6);
        assert_eq!(JSObjectClass::Date as u8, 7);
        assert_eq!(JSObjectClass::RegExp as u8, 8);
        assert_eq!(JSObjectClass::Error as u8, 9);
        assert_eq!(JSObjectClass::EvalError as u8, 10);
        assert_eq!(JSObjectClass::RangeError as u8, 11);
        assert_eq!(JSObjectClass::ReferenceError as u8, 12);
        assert_eq!(JSObjectClass::SyntaxError as u8, 13);
        assert_eq!(JSObjectClass::TypeError as u8, 14);
        assert_eq!(JSObjectClass::UriError as u8, 15);
        assert_eq!(JSObjectClass::InternalError as u8, 16);
        assert_eq!(JSObjectClass::ArrayBuffer as u8, 17);
        assert_eq!(JSObjectClass::TypedArray as u8, 18);
        assert_eq!(JSObjectClass::Uint8CArray as u8, 19);
        assert_eq!(JSObjectClass::Int8Array as u8, 20);
        assert_eq!(JSObjectClass::Uint8Array as u8, 21);
        assert_eq!(JSObjectClass::Int16Array as u8, 22);
        assert_eq!(JSObjectClass::Uint16Array as u8, 23);
        assert_eq!(JSObjectClass::Int32Array as u8, 24);
        assert_eq!(JSObjectClass::Uint32Array as u8, 25);
        assert_eq!(JSObjectClass::Float32Array as u8, 26);
        assert_eq!(JSObjectClass::Float64Array as u8, 27);
        assert_eq!(JSObjectClass::User as u8, 28);
    }

    #[test]
    fn cfunction_discriminants_match_c() {
        assert_eq!(JSCFunction::Bound as u8, 0);
        assert_eq!(JSCFunction::User as u8, 1);
    }
}
