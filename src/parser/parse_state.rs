use core::ptr::NonNull;

use bitflags::bitflags;

use crate::capi_defs::JSContext;
use crate::jsvalue::{JSValue, JS_NULL};

use super::types::{SourcePos, Token, TokenExtra};

// C: parse state flags and enums from mquickjs.c.

pub const PARSE_STATE_INIT: u8 = 0xfe;
pub const PARSE_STATE_RET: u8 = 0xff;

pub const PF_NO_IN: u32 = 1 << 0;
pub const PF_DROP: u32 = 1 << 1;
pub const PF_ACCEPT_LPAREN: u32 = 1 << 2;
pub const PF_LEVEL_SHIFT: u32 = 4;
pub const PF_LEVEL_MASK: u32 = 0x0f << PF_LEVEL_SHIFT;

bitflags! {
    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    struct ParseStateFlags: u16 {
        const GOT_LF = 1 << 0;
        const IS_EVAL = 1 << 1;
        const HAS_RETVAL = 1 << 2;
        const IS_REPL = 1 << 3;
        const HAS_COLUMN = 1 << 4;
        const DROPPED_RESULT = 1 << 5;
        const RE_IN_JS = 1 << 6;
        const MULTI_LINE = 1 << 7;
        const DOTALL = 1 << 8;
        const IGNORE_CASE = 1 << 9;
        const IS_UNICODE = 1 << 10;
    }
}

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

const ERROR_MSG_LEN: usize = 64;

// C: `JSParseState` in mquickjs.c.
#[allow(dead_code)]
pub struct JSParseState {
    ctx: NonNull<JSContext>,
    token: Token,
    flags: ParseStateFlags,
    source_str: JSValue,
    filename_str: JSValue,
    // source_buf is a read-only view into the source bytes; buf_len is authoritative.
    // When source_str is a JSString, source_buf can move after allocations/GC, so callers
    // must refresh it as needed. When buf_len == 0, source_buf may be null.
    source_buf: *const u8,
    buf_pos: u32,
    buf_len: u32,
    cur_func: JSValue,
    byte_code: JSValue,
    byte_code_len: u32,
    last_opcode_pos: i32,
    last_pc2line_pos: i32,
    last_pc2line_source_pos: SourcePos,
    pc2line_bit_len: u32,
    pc2line_source_pos: SourcePos,
    cpool_len: u16,
    hoisted_code_len: u32,
    local_vars_len: u16,
    eval_ret_idx: i32,
    top_break: JSValue,
    capture_count: u8,
    error_msg: [u8; ERROR_MSG_LEN],
}

impl JSParseState {
    pub fn new(ctx: NonNull<JSContext>, has_column: bool) -> Self {
        let mut flags = ParseStateFlags::empty();
        flags.set(ParseStateFlags::HAS_COLUMN, has_column);
        Self {
            ctx,
            token: Token::new(0, 0, TokenExtra::None, JS_NULL),
            flags,
            source_str: JS_NULL,
            filename_str: JS_NULL,
            source_buf: core::ptr::null(),
            buf_pos: 0,
            buf_len: 0,
            cur_func: JS_NULL,
            byte_code: JS_NULL,
            byte_code_len: 0,
            last_opcode_pos: 0,
            last_pc2line_pos: 0,
            last_pc2line_source_pos: 0,
            pc2line_bit_len: 0,
            pc2line_source_pos: 0,
            cpool_len: 0,
            hoisted_code_len: 0,
            local_vars_len: 0,
            eval_ret_idx: 0,
            top_break: JS_NULL,
            capture_count: 0,
            error_msg: [0; ERROR_MSG_LEN],
        }
    }

    pub fn set_source(&mut self, source_buf: *const u8, buf_len: u32) {
        if buf_len != 0 {
            debug_assert!(!source_buf.is_null());
        }
        self.source_buf = source_buf;
        self.buf_len = buf_len;
    }

    pub fn reset_parse_state(&mut self, input_pos: u32, cur_func: JSValue) {
        debug_assert!(input_pos <= self.buf_len);
        self.buf_pos = input_pos;
        self.token = Token::new(b' ' as i32, 0, TokenExtra::None, JS_NULL);

        self.cur_func = cur_func;
        self.byte_code = JS_NULL;
        self.byte_code_len = 0;
        self.last_opcode_pos = -1;

        self.pc2line_bit_len = 0;
        self.pc2line_source_pos = 0;

        self.cpool_len = 0;
        self.hoisted_code_len = 0;

        self.local_vars_len = 0;

        self.eval_ret_idx = -1;
    }

    pub fn is_valid(&self) -> bool {
        if self.buf_pos > self.buf_len {
            return false;
        }
        if self.buf_len != 0 && self.source_buf.is_null() {
            return false;
        }
        self.token.source_pos() <= self.buf_len
    }

    pub fn cur_func(&self) -> JSValue {
        self.cur_func
    }

    pub fn set_cur_func(&mut self, cur_func: JSValue) {
        self.cur_func = cur_func;
    }

    pub fn local_vars_len(&self) -> u16 {
        self.local_vars_len
    }

    pub fn set_local_vars_len(&mut self, len: u16) {
        self.local_vars_len = len;
    }

    pub fn byte_code_len(&self) -> u32 {
        self.byte_code_len
    }

    pub fn set_byte_code_len(&mut self, len: u32) {
        self.byte_code_len = len;
    }

    pub fn is_eval(&self) -> bool {
        self.flags.contains(ParseStateFlags::IS_EVAL)
    }

    pub fn set_is_eval(&mut self, is_eval: bool) {
        self.flags.set(ParseStateFlags::IS_EVAL, is_eval);
    }

    pub fn is_repl(&self) -> bool {
        self.flags.contains(ParseStateFlags::IS_REPL)
    }

    pub fn set_is_repl(&mut self, is_repl: bool) {
        self.flags.set(ParseStateFlags::IS_REPL, is_repl);
    }

    pub fn has_retval(&self) -> bool {
        self.flags.contains(ParseStateFlags::HAS_RETVAL)
    }

    pub fn set_has_retval(&mut self, has_retval: bool) {
        self.flags.set(ParseStateFlags::HAS_RETVAL, has_retval);
    }

    pub fn has_column(&self) -> bool {
        self.flags.contains(ParseStateFlags::HAS_COLUMN)
    }

    pub fn set_has_column(&mut self, has_column: bool) {
        self.flags.set(ParseStateFlags::HAS_COLUMN, has_column);
    }

    pub fn cpool_len(&self) -> u16 {
        self.cpool_len
    }

    pub fn set_cpool_len(&mut self, len: u16) {
        self.cpool_len = len;
    }

    pub fn hoisted_code_len(&self) -> u32 {
        self.hoisted_code_len
    }

    pub fn set_hoisted_code_len(&mut self, len: u32) {
        self.hoisted_code_len = len;
    }

    pub fn add_hoisted_code_len(&mut self, delta: u32) {
        self.hoisted_code_len = self.hoisted_code_len.saturating_add(delta);
    }

    pub fn eval_ret_idx(&self) -> i32 {
        self.eval_ret_idx
    }

    pub fn set_eval_ret_idx(&mut self, idx: i32) {
        self.eval_ret_idx = idx;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::ptr::NonNull;

    use crate::jsvalue::new_short_int;

    #[test]
    #[cfg(not(miri))]
    fn parse_state_values_match_c() {
        assert_eq!(PARSE_STATE_INIT, 0xfe);
        assert_eq!(PARSE_STATE_RET, 0xff);
    }

    #[test]
    #[cfg(not(miri))]
    fn parse_flag_values_match_c() {
        assert_eq!(PF_NO_IN, 1 << 0);
        assert_eq!(PF_DROP, 1 << 1);
        assert_eq!(PF_ACCEPT_LPAREN, 1 << 2);
        assert_eq!(PF_LEVEL_SHIFT, 4);
        assert_eq!(PF_LEVEL_MASK, 0x0f << 4);
    }

    #[test]
    #[cfg(not(miri))]
    fn parse_function_discriminants_match_c() {
        assert_eq!(JSParseFunction::Statement as u8, 0);
        assert_eq!(JSParseFunction::Expr as u8, 1);
        assert_eq!(JSParseFunction::Method as u8, 2);
    }

    #[test]
    #[cfg(not(miri))]
    fn parse_prop_discriminants_match_c() {
        assert_eq!(ParseProp::Field as u8, 0);
        assert_eq!(ParseProp::Get as u8, 1);
        assert_eq!(ParseProp::Set as u8, 2);
        assert_eq!(ParseProp::Method as u8, 3);
    }

    #[test]
    #[cfg(not(miri))]
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

    #[test]
    fn parse_state_reset_matches_c() {
        let ctx = NonNull::dangling();
        let mut state = JSParseState::new(ctx, true);
        let buf = [0u8; 10];
        state.set_source(buf.as_ptr(), buf.len() as u32);
        state.buf_pos = 5;
        state.token = Token::new(123, 7, TokenExtra::None, JS_NULL);
        state.cur_func = new_short_int(1);
        state.byte_code = new_short_int(2);
        state.byte_code_len = 3;
        state.last_opcode_pos = 17;
        state.pc2line_bit_len = 9;
        state.pc2line_source_pos = 7;
        state.cpool_len = 2;
        state.hoisted_code_len = 11;
        state.local_vars_len = 4;
        state.eval_ret_idx = 2;

        state.reset_parse_state(2, JS_NULL);

        assert_eq!(state.buf_pos, 2);
        assert_eq!(state.token.val(), b' ' as i32);
        assert_eq!(state.cur_func, JS_NULL);
        assert_eq!(state.byte_code, JS_NULL);
        assert_eq!(state.byte_code_len, 0);
        assert_eq!(state.last_opcode_pos, -1);
        assert_eq!(state.pc2line_bit_len, 0);
        assert_eq!(state.pc2line_source_pos, 0);
        assert_eq!(state.cpool_len, 0);
        assert_eq!(state.hoisted_code_len, 0);
        assert_eq!(state.local_vars_len, 0);
        assert_eq!(state.eval_ret_idx, -1);
    }

    #[test]
    fn parse_state_validates_buffer_invariants() {
        let ctx = NonNull::dangling();
        let mut state = JSParseState::new(ctx, false);
        assert!(state.is_valid());

        let buf = [0u8; 4];
        state.set_source(buf.as_ptr(), buf.len() as u32);
        state.buf_pos = 5;
        assert!(!state.is_valid());
    }
}
