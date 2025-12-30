use core::cell::Cell;
use core::ptr::NonNull;

use bitflags::bitflags;

use crate::capi_defs::JSContext;
use crate::jsvalue::{value_to_ptr, JSValue, JSWord, JS_NULL};
use crate::memblock::{MbHeader, MTag};
use core::mem::size_of;
use core::ptr;

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
    token: Cell<Token>,
    token_value: Cell<JSValue>,
    flags: Cell<ParseStateFlags>,
    source_str: Cell<JSValue>,
    filename_str: Cell<JSValue>,
    // source_buf is a read-only view into the source bytes; buf_len is authoritative.
    // When source_str is a heap string, source_buf can move after allocations/GC, so callers
    // must refresh it as needed. When buf_len == 0, source_buf may be null.
    source_buf: Cell<*const u8>,
    buf_pos: Cell<u32>,
    buf_len: Cell<u32>,
    cur_func: Cell<JSValue>,
    byte_code: Cell<JSValue>,
    byte_code_len: Cell<u32>,
    last_opcode_pos: Cell<i32>,
    last_pc2line_pos: Cell<i32>,
    last_pc2line_source_pos: Cell<SourcePos>,
    pc2line_bit_len: Cell<u32>,
    pc2line_source_pos: Cell<SourcePos>,
    cpool_len: Cell<u16>,
    hoisted_code_len: Cell<u32>,
    local_vars_len: Cell<u16>,
    eval_ret_idx: Cell<i32>,
    top_break: Cell<JSValue>,
    capture_count: Cell<u8>,
    error_msg: Cell<[u8; ERROR_MSG_LEN]>,
}

impl JSParseState {
    pub fn new(ctx: NonNull<JSContext>, has_column: bool) -> Self {
        let mut flags = ParseStateFlags::empty();
        flags.set(ParseStateFlags::HAS_COLUMN, has_column);
        Self {
            ctx,
            token: Cell::new(Token::new(0, 0, TokenExtra::None, JS_NULL)),
            token_value: Cell::new(JS_NULL),
            flags: Cell::new(flags),
            source_str: Cell::new(JS_NULL),
            filename_str: Cell::new(JS_NULL),
            source_buf: Cell::new(core::ptr::null()),
            buf_pos: Cell::new(0),
            buf_len: Cell::new(0),
            cur_func: Cell::new(JS_NULL),
            byte_code: Cell::new(JS_NULL),
            byte_code_len: Cell::new(0),
            last_opcode_pos: Cell::new(0),
            last_pc2line_pos: Cell::new(0),
            last_pc2line_source_pos: Cell::new(0),
            pc2line_bit_len: Cell::new(0),
            pc2line_source_pos: Cell::new(0),
            cpool_len: Cell::new(0),
            hoisted_code_len: Cell::new(0),
            local_vars_len: Cell::new(0),
            eval_ret_idx: Cell::new(0),
            top_break: Cell::new(JS_NULL),
            capture_count: Cell::new(0),
            error_msg: Cell::new([0; ERROR_MSG_LEN]),
        }
    }

    pub fn set_source(&self, source_buf: *const u8, buf_len: u32) {
        if buf_len != 0 {
            debug_assert!(!source_buf.is_null());
        }
        self.source_buf.set(source_buf);
        self.buf_len.set(buf_len);
    }

    pub fn reset_parse_state(&self, input_pos: u32, cur_func: JSValue) {
        debug_assert!(input_pos <= self.buf_len.get());
        self.buf_pos.set(input_pos);
        let token = Token::new(b' ' as i32, 0, TokenExtra::None, JS_NULL);
        self.set_token(token);

        self.cur_func.set(cur_func);
        self.byte_code.set(JS_NULL);
        self.byte_code_len.set(0);
        self.last_opcode_pos.set(-1);

        self.pc2line_bit_len.set(0);
        self.pc2line_source_pos.set(0);

        self.cpool_len.set(0);
        self.hoisted_code_len.set(0);

        self.local_vars_len.set(0);

        self.eval_ret_idx.set(-1);
    }

    pub fn is_valid(&self) -> bool {
        let buf_pos = self.buf_pos.get();
        let buf_len = self.buf_len.get();
        if buf_pos > buf_len {
            return false;
        }
        if buf_len != 0 && self.source_buf.get().is_null() {
            return false;
        }
        self.token.get().source_pos() <= buf_len
    }

    pub fn cur_func(&self) -> JSValue {
        self.cur_func.get()
    }

    pub fn set_cur_func(&self, cur_func: JSValue) {
        self.cur_func.set(cur_func);
    }

    pub(crate) fn set_token(&self, token: Token) {
        self.token.set(token);
        self.token_value.set(token.value());
    }

    pub(crate) unsafe fn set_token_raw(state: *mut JSParseState, token: Token) {
        debug_assert!(!state.is_null());
        // SAFETY: caller ensures `state` is a valid JSParseState pointer for raw updates.
        unsafe {
            let token_ptr = ptr::addr_of_mut!((*state).token);
            (*token_ptr).set(token);
            let value_ptr = ptr::addr_of_mut!((*state).token_value);
            (*value_ptr).set(token.value());
        }
    }

    pub fn local_vars_len(&self) -> u16 {
        self.local_vars_len.get()
    }

    pub fn set_local_vars_len(&self, len: u16) {
        self.local_vars_len.set(len);
    }

    pub fn byte_code_len(&self) -> u32 {
        self.byte_code_len.get()
    }

    pub fn set_byte_code_len(&self, len: u32) {
        self.byte_code_len.set(len);
    }

    pub fn is_eval(&self) -> bool {
        self.flags.get().contains(ParseStateFlags::IS_EVAL)
    }

    pub fn set_is_eval(&self, is_eval: bool) {
        let mut flags = self.flags.get();
        flags.set(ParseStateFlags::IS_EVAL, is_eval);
        self.flags.set(flags);
    }

    pub fn is_repl(&self) -> bool {
        self.flags.get().contains(ParseStateFlags::IS_REPL)
    }

    pub fn set_is_repl(&self, is_repl: bool) {
        let mut flags = self.flags.get();
        flags.set(ParseStateFlags::IS_REPL, is_repl);
        self.flags.set(flags);
    }

    pub fn has_retval(&self) -> bool {
        self.flags.get().contains(ParseStateFlags::HAS_RETVAL)
    }

    pub fn set_has_retval(&self, has_retval: bool) {
        let mut flags = self.flags.get();
        flags.set(ParseStateFlags::HAS_RETVAL, has_retval);
        self.flags.set(flags);
    }

    pub fn has_column(&self) -> bool {
        self.flags.get().contains(ParseStateFlags::HAS_COLUMN)
    }

    pub fn set_has_column(&self, has_column: bool) {
        let mut flags = self.flags.get();
        flags.set(ParseStateFlags::HAS_COLUMN, has_column);
        self.flags.set(flags);
    }

    pub fn cpool_len(&self) -> u16 {
        self.cpool_len.get()
    }

    pub fn set_cpool_len(&self, len: u16) {
        self.cpool_len.set(len);
    }

    pub fn hoisted_code_len(&self) -> u32 {
        self.hoisted_code_len.get()
    }

    pub fn set_hoisted_code_len(&self, len: u32) {
        self.hoisted_code_len.set(len);
    }

    pub fn add_hoisted_code_len(&self, delta: u32) {
        let len = self.hoisted_code_len.get().saturating_add(delta);
        self.hoisted_code_len.set(len);
    }

    pub fn eval_ret_idx(&self) -> i32 {
        self.eval_ret_idx.get()
    }

    pub fn set_eval_ret_idx(&self, idx: i32) {
        self.eval_ret_idx.set(idx);
    }

    pub(crate) fn gc_roots(&self) -> [JSValue; 5] {
        [
            self.source_str.get(),
            self.filename_str.get(),
            self.token_value.get(),
            self.cur_func.get(),
            self.byte_code.get(),
        ]
    }

    pub(crate) fn gc_root_slots(&self) -> [*mut JSValue; 5] {
        [
            self.source_str.as_ptr(),
            self.filename_str.as_ptr(),
            self.token_value.as_ptr(),
            self.cur_func.as_ptr(),
            self.byte_code.as_ptr(),
        ]
    }

    pub(crate) fn refresh_source_buf_from_str(&self) {
        let Some(ptr) = value_to_ptr::<u8>(self.source_str.get()) else {
            return;
        };
        let header_word = unsafe {
            // SAFETY: `ptr` points to readable header storage.
            ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
        };
        let header = MbHeader::from_word(header_word);
        if header.tag() != MTag::String {
            return;
        }
        let source_buf = unsafe {
            // SAFETY: string bytes follow the header word.
            ptr.as_ptr().add(size_of::<JSWord>())
        };
        self.source_buf.set(source_buf);
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
        let state = JSParseState::new(ctx, true);
        let buf = [0u8; 10];
        state.set_source(buf.as_ptr(), buf.len() as u32);
        state.buf_pos.set(5);
        state.set_token(Token::new(123, 7, TokenExtra::None, JS_NULL));
        state.cur_func.set(new_short_int(1));
        state.byte_code.set(new_short_int(2));
        state.byte_code_len.set(3);
        state.last_opcode_pos.set(17);
        state.pc2line_bit_len.set(9);
        state.pc2line_source_pos.set(7);
        state.cpool_len.set(2);
        state.hoisted_code_len.set(11);
        state.local_vars_len.set(4);
        state.eval_ret_idx.set(2);

        state.reset_parse_state(2, JS_NULL);

        assert_eq!(state.buf_pos.get(), 2);
        assert_eq!(state.token.get().val(), b' ' as i32);
        assert_eq!(state.cur_func.get(), JS_NULL);
        assert_eq!(state.byte_code.get(), JS_NULL);
        assert_eq!(state.byte_code_len.get(), 0);
        assert_eq!(state.last_opcode_pos.get(), -1);
        assert_eq!(state.pc2line_bit_len.get(), 0);
        assert_eq!(state.pc2line_source_pos.get(), 0);
        assert_eq!(state.cpool_len.get(), 0);
        assert_eq!(state.hoisted_code_len.get(), 0);
        assert_eq!(state.local_vars_len.get(), 0);
        assert_eq!(state.eval_ret_idx.get(), -1);
    }

    #[test]
    fn parse_state_validates_buffer_invariants() {
        let ctx = NonNull::dangling();
        let state = JSParseState::new(ctx, false);
        assert!(state.is_valid());

        let buf = [0u8; 4];
        state.set_source(buf.as_ptr(), buf.len() as u32);
        state.buf_pos.set(5);
        assert!(!state.is_valid());
    }

    #[test]
    fn refresh_source_buf_from_string() {
        use crate::context::{ContextConfig, JSContext};
        use crate::string::runtime::string_view;
        use crate::stdlib::MQUICKJS_STDLIB_IMAGE;

        let ctx = NonNull::dangling();
        let state = JSParseState::new(ctx, true);
        let mut runtime = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let source_bytes = b"abc";
        let source = runtime.new_string_len(source_bytes).expect("string");
        state.source_str.set(source);
        let dummy = [0u8; 1];
        state.set_source(dummy.as_ptr(), source_bytes.len() as u32);
        state.refresh_source_buf_from_str();
        let mut scratch = [0u8; 5];
        let view = string_view(source, &mut scratch).expect("string view");
        assert_eq!(state.source_buf.get(), view.bytes().as_ptr());
    }
}