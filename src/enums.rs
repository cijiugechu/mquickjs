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
}
