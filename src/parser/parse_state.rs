// C: parse state flags and enums from mquickjs.c.

pub const PARSE_STATE_INIT: u8 = 0xfe;
pub const PARSE_STATE_RET: u8 = 0xff;

pub const PF_NO_IN: u32 = 1 << 0;
pub const PF_DROP: u32 = 1 << 1;
pub const PF_ACCEPT_LPAREN: u32 = 1 << 2;
pub const PF_LEVEL_SHIFT: u32 = 4;
pub const PF_LEVEL_MASK: u32 = 0x0f << PF_LEVEL_SHIFT;

// C: `JSParseFunctionEnum` in mquickjs.c.
#[repr(u8)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum JSParseFunction {
    Statement = 0,
    Expr = 1,
    Method = 2,
}

// C: parse property kinds in mquickjs.c.
#[repr(u8)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ParseProp {
    Field = 0,
    Get = 1,
    Set = 2,
    Method = 3,
}

// C: `ParseExprFuncEnum` in mquickjs.c.
#[repr(u8)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ParseExprFunc {
    JsParseExprComma = 0,
    JsParseAssignExpr = 1,
    JsParseCondExpr = 2,
    JsParseLogicalAndOr = 3,
    JsParseExprBinary = 4,
    JsParseUnary = 5,
    JsParsePostfixExpr = 6,
    JsParseStatement = 7,
    JsParseBlock = 8,
    JsParseJsonValue = 9,
    ReParseAlternative = 10,
    ReParseDisjunction = 11,
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    #[test]
    fn parse_state_values_match_c() {
        assert_eq!(PARSE_STATE_INIT, 0xfe);
        assert_eq!(PARSE_STATE_RET, 0xff);
    }

    #[test]
    fn parse_flag_values_match_c() {
        assert_eq!(PF_NO_IN, 1 << 0);
        assert_eq!(PF_DROP, 1 << 1);
        assert_eq!(PF_ACCEPT_LPAREN, 1 << 2);
        assert_eq!(PF_LEVEL_SHIFT, 4);
        assert_eq!(PF_LEVEL_MASK, 0x0f << 4);
    }

    #[test]
    fn parse_function_discriminants_match_c() {
        assert_eq!(JSParseFunction::Statement as u8, 0);
        assert_eq!(JSParseFunction::Expr as u8, 1);
        assert_eq!(JSParseFunction::Method as u8, 2);
    }

    #[test]
    fn parse_prop_discriminants_match_c() {
        assert_eq!(ParseProp::Field as u8, 0);
        assert_eq!(ParseProp::Get as u8, 1);
        assert_eq!(ParseProp::Set as u8, 2);
        assert_eq!(ParseProp::Method as u8, 3);
    }

    #[test]
    fn parse_expr_func_order_matches_c() {
        assert_eq!(ParseExprFunc::JsParseExprComma as u8, 0);
        assert_eq!(ParseExprFunc::JsParseAssignExpr as u8, 1);
        assert_eq!(ParseExprFunc::JsParseCondExpr as u8, 2);
        assert_eq!(ParseExprFunc::JsParseLogicalAndOr as u8, 3);
        assert_eq!(ParseExprFunc::JsParseExprBinary as u8, 4);
        assert_eq!(ParseExprFunc::JsParseUnary as u8, 5);
        assert_eq!(ParseExprFunc::JsParsePostfixExpr as u8, 6);
        assert_eq!(ParseExprFunc::JsParseStatement as u8, 7);
        assert_eq!(ParseExprFunc::JsParseBlock as u8, 8);
        assert_eq!(ParseExprFunc::JsParseJsonValue as u8, 9);
        assert_eq!(ParseExprFunc::ReParseAlternative as u8, 10);
        assert_eq!(ParseExprFunc::ReParseDisjunction as u8, 11);
    }
}
