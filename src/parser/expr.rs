use core::ptr::NonNull;

use crate::cutils::{get_u16, put_u16, unicode_to_utf8, UTF8_CHAR_LEN_MAX};
use crate::enums::JSVarRefKind;
use crate::function_bytecode::{FunctionBytecode, FunctionBytecodeFields, FunctionBytecodeHeader};
use crate::jsvalue::{
    is_int, is_ptr, value_get_int, value_get_special_tag, value_get_special_value, value_from_ptr,
    value_to_ptr, JSValue, JS_NULL, JS_SHORTINT_MAX, JS_SHORTINT_MIN, JS_TAG_INT,
    JS_TAG_SPECIAL_BITS, JS_TAG_STRING_CHAR,
};
use crate::memblock_views::Float64View;
use crate::opcode::{
    OpCode, OP_ADD, OP_AND, OP_ARRAY_FROM, OP_CALL, OP_CALL_CONSTRUCTOR, OP_CALL_METHOD,
    OP_DEC, OP_DEFINE_FIELD, OP_DELETE, OP_DIV, OP_DROP, OP_DUP, OP_EQ, OP_GOTO, OP_GTE, OP_GT,
    OP_IF_FALSE, OP_IF_TRUE, OP_IN, OP_INSTANCEOF, OP_LNOT, OP_LT, OP_LTE, OP_MOD, OP_MUL,
    OP_NEG, OP_NEQ, OP_NEW_TARGET, OP_NOT, OP_NULL, OP_OBJECT, OP_OR, OP_PLUS, OP_POST_DEC,
    OP_POW, OP_PUSH_CONST, OP_PUSH_FALSE, OP_PUSH_THIS, OP_PUSH_TRUE, OP_PUSH_VALUE, OP_REGEXP,
    OP_SAR, OP_SET_PROTO, OP_SHL, OP_SHR, OP_STRICT_EQ, OP_STRICT_NEQ, OP_SUB, OP_TYPEOF,
    OP_UNDEFINED, OP_XOR, OP_GET_ARG, OP_GET_ARRAY_EL,
    OP_GET_ARRAY_EL2, OP_GET_FIELD, OP_GET_FIELD2, OP_GET_LENGTH, OP_GET_LENGTH2, OP_GET_LOC,
    OP_GET_VAR_REF, OP_GET_VAR_REF_NOCHECK, OP_INVALID, OP_PUT_ARRAY_EL,
};
use crate::parser::emit::{BytecodeEmitter, Label};
use crate::parser::error::{parse_expect, ParserError, ParserErrorKind, ERR_NO_MEM};
use crate::parser::lexer::{value_matches_bytes, ParseState};
use crate::parser::lvalue::{get_lvalue, put_lvalue, LValueError, PutLValue};
use crate::parser::parse_stack::{ParseStack, ParseStackError};
use crate::parser::parse_state::{
    ParseExprFunc, JSParseState, PF_ACCEPT_LPAREN, PF_DROP, PF_LEVEL_MASK, PF_LEVEL_SHIFT,
    PF_NO_IN, PARSE_STATE_INIT, PARSE_STATE_RET,
};
use crate::parser::property_name::parse_property_name;
use crate::parser::regexp::{compile_regexp, RegExpError};
use crate::parser::tokens::{
    TOK_DEC, TOK_FIRST_KEYWORD, TOK_IDENT, TOK_INC, TOK_LAND, TOK_LOR, TOK_MUL_ASSIGN,
    TOK_NEQ, TOK_NUMBER, TOK_OR_ASSIGN, TOK_POW, TOK_REGEXP, TOK_SAR, TOK_SHL, TOK_SHR,
    TOK_STRICT_EQ, TOK_STRICT_NEQ, TOK_STRING, TOK_EQ, TOK_GTE, TOK_LTE, TOK_TRUE, TOK_FALSE,
    TOK_NULL, TOK_THIS, TOK_FUNCTION, TOK_NEW, TOK_INSTANCEOF, TOK_IN, TOK_DELETE, TOK_TYPEOF,
    TOK_VOID,
};
use crate::parser::types::{SourcePos, TokenExtra};
use crate::parser::vars::{add_ext_var, add_var, find_ext_var, find_var, get_ext_var_name, VarAllocator, VarError, ValueArray};
use crate::string::js_string::JSString;

const ERR_TOO_MANY_CONSTANTS: &str = "too many constants";
const ERR_TOO_MANY_CALL_ARGUMENTS: &str = "too many call arguments";
const ERR_TOO_MANY_ELEMENTS: &str = "too many elements";
const ERR_INVALID_LVALUE_FOR_DELETE: &str = "invalid lvalue for delete";
const ERR_EXPECTING_TARGET: &str = "expecting target";
const ERR_UNEXPECTED_CHAR_EXPR: &str = "unexpected character in expression";
const ERR_EXPECTING_FIELD_NAME: &str = "expecting field name";
const ERR_DIRECT_EVAL: &str =
    "direct eval is not supported. Use (1,eval) instead for indirect eval";
const ERR_DUP_PROTO: &str = "duplicate __proto__ property name";
const ERR_FUNCTION_UNSUPPORTED: &str = "function parsing not implemented";
const ERR_INVALID_STRING: &str = "invalid string value";

const MAX_CALL_ARGUMENTS: i32 = 65535;
const DEFAULT_STACK_SIZE: usize = 256;

const TOK_PAREN_OPEN: i32 = b'(' as i32;
const TOK_BRACE_OPEN: i32 = b'{' as i32;
const TOK_BRACKET_OPEN: i32 = b'[' as i32;
const TOK_PLUS: i32 = b'+' as i32;
const TOK_MINUS: i32 = b'-' as i32;
const TOK_BANG: i32 = b'!' as i32;
const TOK_TILDE: i32 = b'~' as i32;
const TOK_STAR: i32 = b'*' as i32;
const TOK_SLASH: i32 = b'/' as i32;
const TOK_PERCENT: i32 = b'%' as i32;
const TOK_LT_CHAR: i32 = b'<' as i32;
const TOK_GT_CHAR: i32 = b'>' as i32;
const TOK_AMP: i32 = b'&' as i32;
const TOK_CARET: i32 = b'^' as i32;
const TOK_PIPE: i32 = b'|' as i32;

fn encode_call(state: u8, func: ParseExprFunc, param: i32) -> i32 {
    (state as i32) | ((func as i32) << 8) | (param << 16)
}

fn parse_flags_level(flags: i32) -> u32 {
    ((flags as u32) & PF_LEVEL_MASK) >> PF_LEVEL_SHIFT
}

fn has_flag(flags: i32, mask: u32) -> bool {
    (flags as u32) & mask != 0
}

fn is_negative_zero(value: f64) -> bool {
    value == 0.0 && value.is_sign_negative()
}

fn encode_short_int(val: i32) -> u32 {
    ((val as u32) << 1) | (JS_TAG_INT as u32)
}

fn encode_jsvalue(val: JSValue) -> u32 {
    if is_int(val) {
        return encode_short_int(value_get_int(val));
    }
    let tag = value_get_special_tag(val) as u32;
    let payload = value_get_special_value(val) as u32;
    tag | (payload << JS_TAG_SPECIAL_BITS)
}

fn value_bytes(val: JSValue) -> Result<Vec<u8>, ParserError> {
    if value_get_special_tag(val) == JS_TAG_STRING_CHAR {
        let mut buf = [0u8; UTF8_CHAR_LEN_MAX];
        let len = unicode_to_utf8(&mut buf, value_get_special_value(val) as u32);
        return Ok(buf[..len].to_vec());
    }
    let Some(ptr) = value_to_ptr::<JSString>(val) else {
        return Err(ParserError::new(ParserErrorKind::Static(ERR_INVALID_STRING), 0));
    };
    // SAFETY: ExprParser owns the JSString allocations used by its lexer.
    Ok(unsafe { ptr.as_ref().buf().to_vec() })
}

fn is_short_float_value(val: JSValue) -> bool {
    #[cfg(target_pointer_width = "64")]
    {
        crate::jsvalue::is_short_float(val)
    }
    #[cfg(target_pointer_width = "32")]
    {
        let _ = val;
        false
    }
}

#[derive(Default)]
struct AtomCache {
    length_value: Option<JSValue>,
    length_cpool: Option<u16>,
}

// Temporary owner for float64 allocations: C uses JS_NewFloat64 (GC-managed),
// but the Rust parser lacks runtime GC, so we keep boxed Float64View alive
// for the parser lifetime.
struct FloatAllocator {
    floats: Vec<NonNull<Float64View>>,
}

impl FloatAllocator {
    fn new() -> Self {
        Self { floats: Vec::new() }
    }

    fn alloc(&mut self, value: f64) -> JSValue {
        let boxed = Box::new(Float64View::new(false, value));
        // SAFETY: Box never yields a null pointer.
        let ptr = unsafe { NonNull::new_unchecked(Box::into_raw(boxed)) };
        self.floats.push(ptr);
        value_from_ptr(ptr)
    }
}

impl Drop for FloatAllocator {
    fn drop(&mut self) {
        for ptr in self.floats.drain(..) {
            // SAFETY: pointers were created from Box::into_raw in this allocator.
            unsafe { drop(Box::from_raw(ptr.as_ptr())) };
        }
    }
}

struct OwnedFunctionBytecode {
    ptr: NonNull<FunctionBytecode>,
}

impl OwnedFunctionBytecode {
    fn new(header: FunctionBytecodeHeader, fields: FunctionBytecodeFields) -> Self {
        let boxed = Box::new(FunctionBytecode::from_fields(header, fields));
        // SAFETY: Box never yields a null pointer.
        let ptr = unsafe { NonNull::new_unchecked(Box::into_raw(boxed)) };
        Self { ptr }
    }

    fn value(&self) -> JSValue {
        value_from_ptr(self.ptr)
    }

    fn as_ref(&self) -> &FunctionBytecode {
        // SAFETY: pointer remains valid for the lifetime of OwnedFunctionBytecode.
        unsafe { self.ptr.as_ref() }
    }

    fn as_mut(&mut self) -> &mut FunctionBytecode {
        // SAFETY: pointer remains valid for the lifetime of OwnedFunctionBytecode.
        unsafe { self.ptr.as_mut() }
    }
}

impl Drop for OwnedFunctionBytecode {
    fn drop(&mut self) {
        // SAFETY: pointer was created from Box::into_raw in OwnedFunctionBytecode::new.
        unsafe { drop(Box::from_raw(self.ptr.as_ptr())) };
    }
}

pub struct ExprParser<'a> {
    lexer: ParseState<'a>,
    emitter: BytecodeEmitter<'a>,
    parse_state: JSParseState,
    alloc: VarAllocator,
    floats: FloatAllocator,
    func: OwnedFunctionBytecode,
    dropped_result: bool,
    atoms: AtomCache,
}

impl<'a> ExprParser<'a> {
    pub fn new(source: &'a [u8]) -> Self {
        let header = FunctionBytecodeHeader::new(false, false, false, 0, false);
        let fields = FunctionBytecodeFields {
            func_name: JS_NULL,
            byte_code: JS_NULL,
            cpool: JS_NULL,
            vars: JS_NULL,
            ext_vars: JS_NULL,
            stack_size: 0,
            ext_vars_len: 0,
            filename: JS_NULL,
            pc2line: JS_NULL,
            source_pos: 0,
        };
        let func = OwnedFunctionBytecode::new(header, fields);
        let mut parse_state = JSParseState::new(NonNull::dangling(), false);
        parse_state.set_cur_func(func.value());
        Self {
            lexer: ParseState::new(source),
            emitter: BytecodeEmitter::new(source, 0, false),
            parse_state,
            alloc: VarAllocator::new(),
            floats: FloatAllocator::new(),
            func,
            dropped_result: false,
            atoms: AtomCache::default(),
        }
    }

    pub fn parse_expression(&mut self, parse_flags: u32) -> Result<(), ParserError> {
        self.lexer.next_token().map_err(ParserError::from)?;
        let mut storage = vec![JS_NULL; DEFAULT_STACK_SIZE];
        let mut stack = ParseStack::new(&mut storage);
        self.parse_call(&mut stack, ParseExprFunc::JsParseExprComma, parse_flags as i32)
    }

    pub fn bytecode(&self) -> &[u8] {
        self.emitter.byte_code()
    }

    pub fn add_local_var(&mut self, name: &str) -> Result<JSValue, ParserError> {
        let value = self
            .lexer
            .intern_identifier(name.as_bytes(), 0)
            .map_err(ParserError::from)?;
        add_var(&mut self.parse_state, &mut self.alloc, value)
            .map_err(|err| Self::var_error(err, 0))?;
        Ok(value)
    }

    fn parse_call(
        &mut self,
        stack: &mut ParseStack<'_>,
        func_idx: ParseExprFunc,
        param: i32,
    ) -> Result<(), ParserError> {
        let stack_top = stack.sp();
        let mut parse_state = PARSE_STATE_INIT;
        let mut func_idx = func_idx;
        let mut param = param;
        loop {
            let ret = self.dispatch_parse(stack, func_idx, parse_state, param)?;
            parse_state = (ret & 0xff) as u8;
            if parse_state == PARSE_STATE_RET {
                if stack.sp() == stack_top {
                    break;
                }
                let saved = stack.pop_int();
                parse_state = (saved & 0xff) as u8;
                func_idx = Self::parse_func_from_u8(((saved >> 8) & 0xff) as u8);
                param = -1;
            } else {
                let saved = (parse_state as i32) | ((func_idx as i32) << 8);
                stack.push_int(saved).map_err(Self::stack_error)?;
                parse_state = PARSE_STATE_INIT;
                func_idx = Self::parse_func_from_u8(((ret >> 8) & 0xff) as u8);
                param = ret >> 16;
            }
        }
        Ok(())
    }

    fn dispatch_parse(
        &mut self,
        stack: &mut ParseStack<'_>,
        func_idx: ParseExprFunc,
        state: u8,
        param: i32,
    ) -> Result<i32, ParserError> {
        match func_idx {
            ParseExprFunc::JsParseExprComma => self.parse_expr_comma(stack, state, param),
            ParseExprFunc::JsParseAssignExpr => self.parse_assign_expr(stack, state, param),
            ParseExprFunc::JsParseCondExpr => self.parse_cond_expr(stack, state, param),
            ParseExprFunc::JsParseLogicalAndOr => self.parse_logical_and_or(stack, state, param),
            ParseExprFunc::JsParseExprBinary => self.parse_expr_binary(stack, state, param),
            ParseExprFunc::JsParseUnary => self.parse_unary(stack, state, param),
            ParseExprFunc::JsParsePostfixExpr => self.parse_postfix_expr(stack, state, param),
            _ => Err(ParserError::new(
                ParserErrorKind::Static(ERR_UNEXPECTED_CHAR_EXPR),
                self.lexer.token().source_pos() as usize,
            )),
        }
    }

    fn parse_func_from_u8(value: u8) -> ParseExprFunc {
        match value {
            0 => ParseExprFunc::JsParseExprComma,
            1 => ParseExprFunc::JsParseAssignExpr,
            2 => ParseExprFunc::JsParseCondExpr,
            3 => ParseExprFunc::JsParseLogicalAndOr,
            4 => ParseExprFunc::JsParseExprBinary,
            5 => ParseExprFunc::JsParseUnary,
            6 => ParseExprFunc::JsParsePostfixExpr,
            _ => ParseExprFunc::JsParseExprComma,
        }
    }

    fn next_token(&mut self) -> Result<(), ParserError> {
        self.lexer.next_token().map_err(ParserError::from)
    }

    fn expect(&mut self, ch: i32) -> Result<(), ParserError> {
        parse_expect(&mut self.lexer, ch)
    }

    fn parser_error(&self, message: &'static str) -> ParserError {
        ParserError::new(
            ParserErrorKind::Static(message),
            self.lexer.token().source_pos() as usize,
        )
    }

    fn stack_error(err: ParseStackError) -> ParserError {
        ParserError::new(ParserErrorKind::Static(err.message()), 0)
    }

    fn var_error(err: VarError, pos: usize) -> ParserError {
        ParserError::new(ParserErrorKind::Static(err.message()), pos)
    }

    fn lvalue_error(err: LValueError) -> ParserError {
        ParserError::new(ParserErrorKind::Static(err.message()), err.position())
    }

    fn regexp_error(err: RegExpError) -> ParserError {
        ParserError::new(ParserErrorKind::Static(err.message()), err.position())
    }

    fn maybe_drop_result(&self, parse_flags: i32) -> bool {
        if !has_flag(parse_flags, PF_DROP) {
            return false;
        }
        let tok = self.lexer.token().val();
        tok == ';' as i32 || tok == ')' as i32 || tok == ',' as i32
    }

    fn cpool_add(&mut self, val: JSValue) -> Result<u16, ParserError> {
        let len = self.parse_state.cpool_len() as usize;
        let mut cpool_val = self.func.as_ref().cpool();
        if cpool_val != JS_NULL {
            let ptr = value_to_ptr::<ValueArray>(cpool_val).ok_or_else(|| {
                ParserError::new(ParserErrorKind::Static(ERR_NO_MEM), 0)
            })?;
            // SAFETY: cpool array is owned by the allocator and stable here.
            let arr = unsafe { ptr.as_ref() };
            for (idx, value) in arr.values().iter().take(len).enumerate() {
                if *value == val {
                    return Ok(idx as u16);
                }
            }
        }

        if len >= u16::MAX as usize {
            return Err(self.parser_error(ERR_TOO_MANY_CONSTANTS));
        }

        let new_size = (len + 1).max(4);
        if cpool_val == JS_NULL {
            cpool_val = self.alloc.alloc_value_array(new_size);
            self.func.as_mut().set_cpool(cpool_val);
        } else {
            let mut ptr = value_to_ptr::<ValueArray>(cpool_val).ok_or_else(|| {
                ParserError::new(ParserErrorKind::Static(ERR_NO_MEM), 0)
            })?;
            // SAFETY: cpool array is owned by the allocator and mutated here.
            let arr = unsafe { ptr.as_mut() };
            arr.ensure_size(new_size);
        }

        let mut ptr = value_to_ptr::<ValueArray>(cpool_val).ok_or_else(|| {
            ParserError::new(ParserErrorKind::Static(ERR_NO_MEM), 0)
        })?;
        // SAFETY: cpool array is owned by the allocator and mutated here.
        let arr = unsafe { ptr.as_mut() };
        arr.values_mut()[len] = val;
        self.parse_state.set_cpool_len((len + 1) as u16);
        Ok(len as u16)
    }

    fn length_atom_value(&mut self) -> Result<JSValue, ParserError> {
        if let Some(value) = self.atoms.length_value {
            return Ok(value);
        }
        let value = self
            .lexer
            .intern_identifier(b"length", 0)
            .map_err(ParserError::from)?;
        self.atoms.length_value = Some(value);
        Ok(value)
    }

    fn length_atom_index(&mut self) -> Result<u16, ParserError> {
        if let Some(idx) = self.atoms.length_cpool {
            return Ok(idx);
        }
        let value = self.length_atom_value()?;
        let idx = self.cpool_add(value)?;
        self.atoms.length_cpool = Some(idx);
        Ok(idx)
    }

    fn emit_push_const(&mut self, val: JSValue) -> Result<(), ParserError> {
        if is_ptr(val) || is_short_float_value(val) {
            let idx = self.cpool_add(val)?;
            self.emitter.emit_op(OP_PUSH_CONST);
            self.emitter.emit_u16(idx);
        } else {
            self.emitter.emit_op(OP_PUSH_VALUE);
            self.emitter.emit_u32(encode_jsvalue(val));
        }
        Ok(())
    }

    fn emit_push_number(&mut self, value: f64) -> Result<(), ParserError> {
        if value.is_finite()
            && !is_negative_zero(value)
            && value >= JS_SHORTINT_MIN as f64
            && value <= JS_SHORTINT_MAX as f64
            && value == (value as i32) as f64
        {
            self.emitter.emit_push_short_int(value as i32);
            return Ok(());
        }
        let js_val = self.floats.alloc(value);
        self.emit_push_const(js_val)
    }

    fn emit_delete(&mut self) -> Result<(), ParserError> {
        let opcode = self.emitter.get_prev_opcode();
        match opcode {
            OP_GET_FIELD => {
                let pos = self.emitter.last_opcode_pos().unwrap_or(0);
                let prop_idx = {
                    let bytes = self.emitter.byte_code();
                    get_u16(&bytes[pos + 1..pos + 3])
                };
                self.emitter.remove_last_op();
                self.emitter.emit_op(OP_PUSH_CONST);
                self.emitter.emit_u16(prop_idx as u16);
            }
            OP_GET_LENGTH => {
                self.emitter.remove_last_op();
                let idx = self.length_atom_index()?;
                self.emitter.emit_op(OP_PUSH_CONST);
                self.emitter.emit_u16(idx);
            }
            OP_GET_ARRAY_EL => {
                self.emitter.remove_last_op();
            }
            _ => return Err(self.parser_error(ERR_INVALID_LVALUE_FOR_DELETE)),
        }
        self.emitter.emit_op(OP_DELETE);
        Ok(())
    }

    fn parse_postfix_expr(
        &mut self,
        stack: &mut ParseStack<'_>,
        state: u8,
        parse_flags: i32,
    ) -> Result<i32, ParserError> {
        #[derive(Copy, Clone, Debug, PartialEq, Eq)]
        enum Phase {
            Start,
            AfterParen,
            ObjectLoop,
            ObjectAfterValue,
            ArraySmallLoop,
            ArraySmallAfterValue,
            ArrayLargeLoop,
            ArrayLargeAfterValue,
            AfterNew,
            CallArg,
            IndexExpr,
            Suffix,
        }

        let mut parse_flags = parse_flags;
        let mut is_new = false;
        let mut prop_idx: i32 = 0;
        let mut count_pos: i32 = 0;
        let mut has_proto = false;
        let mut idx: u32 = 0;
        let mut opcode = OP_INVALID;
        let mut arg_count: i32 = 0;
        let mut op_source_pos: SourcePos = 0;

        let mut phase = match state {
            PARSE_STATE_INIT => Phase::Start,
            0 => {
                parse_flags = stack.pop_int();
                Phase::AfterParen
            }
            1 => {
                count_pos = stack.pop_int();
                has_proto = stack.pop_int() != 0;
                parse_flags = stack.pop_int();
                prop_idx = stack.pop_int();
                Phase::ObjectAfterValue
            }
            2 => {
                parse_flags = stack.pop_int();
                idx = stack.pop_int() as u32;
                Phase::ArraySmallAfterValue
            }
            3 => {
                parse_flags = stack.pop_int();
                idx = stack.pop_int() as u32;
                Phase::ArrayLargeAfterValue
            }
            4 => {
                parse_flags = stack.pop_int();
                Phase::AfterNew
            }
            5 => {
                op_source_pos = stack.pop_int() as SourcePos;
                is_new = stack.pop_int() != 0;
                opcode = OpCode(stack.pop_int() as u16);
                arg_count = stack.pop_int();
                parse_flags = stack.pop_int();
                Phase::CallArg
            }
            6 => {
                op_source_pos = stack.pop_int() as SourcePos;
                is_new = stack.pop_int() != 0;
                parse_flags = stack.pop_int();
                Phase::IndexExpr
            }
            _ => unreachable!("unexpected parse state"),
        };

        loop {
            match phase {
                Phase::Start => {
                    let token = self.lexer.token();
                    match token.val() {
                        TOK_NUMBER => {
                            let number = match token.extra() {
                                TokenExtra::Number(value) => value,
                                _ => return Err(self.parser_error(ERR_UNEXPECTED_CHAR_EXPR)),
                            };
                            self.emit_push_number(number)?;
                            self.next_token()?;
                            phase = Phase::Suffix;
                        }
                        TOK_STRING => {
                            self.emit_push_const(token.value())?;
                            self.next_token()?;
                            phase = Phase::Suffix;
                        }
                        TOK_REGEXP => {
                            let (flags, _) = match token.extra() {
                                TokenExtra::RegExp { flags, end_pos } => (flags, end_pos),
                                _ => return Err(self.parser_error(ERR_UNEXPECTED_CHAR_EXPR)),
                            };
                            let bytes = value_bytes(token.value())?;
                            let bytecode = compile_regexp(&bytes, flags)
                                .map_err(Self::regexp_error)?;
                            let re_value = self.alloc.alloc_byte_array(bytecode.bytes().to_vec());
                            self.emit_push_const(token.value())?;
                            self.emit_push_const(re_value)?;
                            self.emitter.emit_op(OP_REGEXP);
                            self.next_token()?;
                            phase = Phase::Suffix;
                        }
                        TOK_PAREN_OPEN => {
                            self.next_token()?;
                            stack.push_int(parse_flags).map_err(Self::stack_error)?;
                            return Ok(encode_call(
                                0,
                                ParseExprFunc::JsParseExprComma,
                                0,
                            ));
                        }
                        TOK_FUNCTION => {
                            return Err(self.parser_error(ERR_FUNCTION_UNSUPPORTED));
                        }
                        TOK_NULL => {
                            self.emitter.emit_op(OP_NULL);
                            self.next_token()?;
                            phase = Phase::Suffix;
                        }
                        TOK_THIS => {
                            self.emitter.emit_op(OP_PUSH_THIS);
                            self.next_token()?;
                            phase = Phase::Suffix;
                        }
                        TOK_FALSE | TOK_TRUE => {
                            let op = if token.val() == TOK_TRUE {
                                OP_PUSH_TRUE
                            } else {
                                OP_PUSH_FALSE
                            };
                            self.emitter.emit_op(op);
                            self.next_token()?;
                            phase = Phase::Suffix;
                        }
                        TOK_IDENT => {
                            let name = token.value();
                            let var_idx = find_var(&self.parse_state, name);
                            if let Some(var_idx) = var_idx {
                                let arg_count = self.func.as_ref().arg_count() as i32;
                                let mut idx = i32::from(var_idx);
                                let opcode = if idx < arg_count {
                                    OP_GET_ARG
                                } else {
                                    idx -= arg_count;
                                    OP_GET_LOC
                                };
                                self.emitter.emit_var(opcode, idx as u32, token.source_pos());
                            } else {
                                let var_idx = find_ext_var(&self.parse_state, name);
                                let var_idx = if let Some(var_idx) = var_idx {
                                    i32::from(var_idx)
                                } else {
                                    let decl =
                                        (JSVarRefKind::Global as i32) << 16;
                                    i32::from(add_ext_var(
                                        &self.parse_state,
                                        &mut self.alloc,
                                        name,
                                        decl,
                                    )
                                    .map_err(|err| Self::var_error(err, token.source_pos() as usize))?)
                                };
                                self.emitter.emit_var(OP_GET_VAR_REF, var_idx as u32, token.source_pos());
                            }
                            self.next_token()?;
                            phase = Phase::Suffix;
                        }
                        TOK_BRACE_OPEN => {
                            self.next_token()?;
                            self.emitter.emit_op(OP_OBJECT);
                            count_pos = self.emitter.byte_code().len() as i32;
                            self.emitter.emit_u16(0);
                            has_proto = false;
                            phase = Phase::ObjectLoop;
                        }
                        TOK_BRACKET_OPEN => {
                            self.next_token()?;
                            idx = 0;
                            phase = Phase::ArraySmallLoop;
                        }
                        TOK_NEW => {
                            self.next_token()?;
                            if self.lexer.token().val() == '.' as i32 {
                                self.next_token()?;
                                let token = self.lexer.token();
                                if token.val() != TOK_IDENT
                                    || !value_matches_bytes(token.value(), b"target")
                                {
                                    return Err(self.parser_error(ERR_EXPECTING_TARGET));
                                }
                                self.next_token()?;
                                self.emitter.emit_op(OP_NEW_TARGET);
                                phase = Phase::Suffix;
                            } else {
                                stack.push_int(parse_flags).map_err(Self::stack_error)?;
                                return Ok(encode_call(
                                    4,
                                    ParseExprFunc::JsParsePostfixExpr,
                                    0,
                                ));
                            }
                        }
                        _ => return Err(self.parser_error(ERR_UNEXPECTED_CHAR_EXPR)),
                    }
                }
                Phase::AfterParen => {
                    self.expect(')' as i32)?;
                    phase = Phase::Suffix;
                }
                Phase::ObjectLoop => {
                    if self.lexer.token().val() == '}' as i32 {
                        self.next_token()?;
                        phase = Phase::Suffix;
                        continue;
                    }
                    let (prop_type, name) = parse_property_name(&mut self.lexer)?;
                    if prop_type == super::parse_state::ParseProp::Field
                        && value_matches_bytes(name, b"__proto__")
                    {
                        if has_proto {
                            return Err(self.parser_error(ERR_DUP_PROTO));
                        }
                        has_proto = true;
                        prop_idx = -1;
                    } else {
                        let idx = self.cpool_add(name)? as i32;
                        prop_idx = idx;
                        let pos = count_pos as usize;
                        let count = get_u16(&self.emitter.byte_code()[pos..pos + 2]);
                        let new_count = count.saturating_add(1) as u16;
                        let bytes = self.emitter.byte_code_mut();
                        put_u16(&mut bytes[pos..pos + 2], new_count);
                    }

                    if prop_type == super::parse_state::ParseProp::Field {
                        self.expect(':' as i32)?;
                        stack.push_int(prop_idx).map_err(Self::stack_error)?;
                        stack.push_int(parse_flags).map_err(Self::stack_error)?;
                        stack.push_int(has_proto as i32).map_err(Self::stack_error)?;
                        stack.push_int(count_pos).map_err(Self::stack_error)?;
                        return Ok(encode_call(
                            1,
                            ParseExprFunc::JsParseAssignExpr,
                            0,
                        ));
                    }
                    return Err(self.parser_error(ERR_FUNCTION_UNSUPPORTED));
                }
                Phase::ObjectAfterValue => {
                    if prop_idx >= 0 {
                        self.emitter.emit_op(OP_DEFINE_FIELD);
                        self.emitter.emit_u16(prop_idx as u16);
                    } else {
                        self.emitter.emit_op(OP_SET_PROTO);
                    }
                    if self.lexer.token().val() == ',' as i32 {
                        self.next_token()?;
                        phase = Phase::ObjectLoop;
                    } else {
                        self.expect('}' as i32)?;
                        phase = Phase::Suffix;
                    }
                }
                Phase::ArraySmallLoop => {
                    if self.lexer.token().val() == ']' as i32 || idx >= 32 {
                        self.emitter.emit_op_param(OP_ARRAY_FROM, idx, self.emitter.pc2line_source_pos());
                        if self.lexer.token().val() == ']' as i32 {
                            self.next_token()?;
                            phase = Phase::Suffix;
                        } else {
                            phase = Phase::ArrayLargeLoop;
                        }
                        continue;
                    }
                    stack.push_int(idx as i32).map_err(Self::stack_error)?;
                    stack.push_int(parse_flags).map_err(Self::stack_error)?;
                    return Ok(encode_call(
                        2,
                        ParseExprFunc::JsParseAssignExpr,
                        0,
                    ));
                }
                Phase::ArraySmallAfterValue => {
                    idx += 1;
                    if self.lexer.token().val() == ',' as i32 {
                        self.next_token()?;
                    } else if self.lexer.token().val() != ']' as i32 {
                        self.expect(']' as i32)?;
                        phase = Phase::Suffix;
                        continue;
                    }
                    phase = Phase::ArraySmallLoop;
                }
                Phase::ArrayLargeLoop => {
                    if self.lexer.token().val() == ']' as i32 {
                        self.next_token()?;
                        phase = Phase::Suffix;
                        continue;
                    }
                    if idx >= JS_SHORTINT_MAX as u32 {
                        return Err(self.parser_error(ERR_TOO_MANY_ELEMENTS));
                    }
                    self.emitter.emit_op(OP_DUP);
                    self.emitter.emit_push_short_int(idx as i32);
                    stack.push_int(idx as i32).map_err(Self::stack_error)?;
                    stack.push_int(parse_flags).map_err(Self::stack_error)?;
                    return Ok(encode_call(
                        3,
                        ParseExprFunc::JsParseAssignExpr,
                        0,
                    ));
                }
                Phase::ArrayLargeAfterValue => {
                    self.emitter.emit_op(OP_PUT_ARRAY_EL);
                    idx += 1;
                    if self.lexer.token().val() == ',' as i32 {
                        self.next_token()?;
                    }
                    phase = Phase::ArrayLargeLoop;
                }
                Phase::AfterNew => {
                    if self.lexer.token().val() != '(' as i32 {
                        self.emitter.emit_op_param(
                            OP_CALL_CONSTRUCTOR,
                            0,
                            self.lexer.token().source_pos(),
                        );
                        phase = Phase::Suffix;
                    } else {
                        is_new = true;
                        phase = Phase::Suffix;
                    }
                }
                Phase::CallArg => {
                    if self.lexer.token().val() == ')' as i32 {
                        self.next_token()?;
                        if opcode == OP_GET_FIELD || opcode == OP_GET_LENGTH || opcode == OP_GET_ARRAY_EL {
                            self.emitter.emit_op_param(
                                OP_CALL_METHOD,
                                arg_count as u32,
                                op_source_pos,
                            );
                        } else if is_new {
                            self.emitter.emit_op_param(
                                OP_CALL_CONSTRUCTOR,
                                arg_count as u32,
                                op_source_pos,
                            );
                        } else {
                            self.emitter.emit_op_param(
                                OP_CALL,
                                arg_count as u32,
                                op_source_pos,
                            );
                        }
                        is_new = false;
                        phase = Phase::Suffix;
                        continue;
                    }
                    self.expect(',' as i32)?;
                    if arg_count >= MAX_CALL_ARGUMENTS {
                        return Err(self.parser_error(ERR_TOO_MANY_CALL_ARGUMENTS));
                    }
                    arg_count += 1;
                    stack.push_int(parse_flags).map_err(Self::stack_error)?;
                    stack.push_int(arg_count).map_err(Self::stack_error)?;
                    stack.push_int(opcode.0 as i32).map_err(Self::stack_error)?;
                    stack.push_int(is_new as i32).map_err(Self::stack_error)?;
                    stack.push_int(op_source_pos as i32)
                        .map_err(Self::stack_error)?;
                    return Ok(encode_call(
                        5,
                        ParseExprFunc::JsParseAssignExpr,
                        0,
                    ));
                }
                Phase::IndexExpr => {
                    self.expect(']' as i32)?;
                    self.emitter.emit_op_pos(OP_GET_ARRAY_EL, op_source_pos);
                    phase = Phase::Suffix;
                }
                Phase::Suffix => {
                    if self.lexer.token().val() == '(' as i32 && has_flag(parse_flags, PF_ACCEPT_LPAREN) {
                        op_source_pos = self.lexer.token().source_pos();
                        self.next_token()?;
                        if !is_new {
                            let prev = self.emitter.get_prev_opcode();
                            match prev {
                                OP_GET_FIELD => {
                                    if let Some(pos) = self.emitter.last_opcode_pos() {
                                        self.emitter.byte_code_mut()[pos] = OP_GET_FIELD2.0 as u8;
                                    }
                                    opcode = OP_GET_FIELD;
                                }
                                OP_GET_LENGTH => {
                                    if let Some(pos) = self.emitter.last_opcode_pos() {
                                        self.emitter.byte_code_mut()[pos] = OP_GET_LENGTH2.0 as u8;
                                    }
                                    opcode = OP_GET_LENGTH;
                                }
                                OP_GET_ARRAY_EL => {
                                    if let Some(pos) = self.emitter.last_opcode_pos() {
                                        self.emitter.byte_code_mut()[pos] = OP_GET_ARRAY_EL2.0 as u8;
                                    }
                                    opcode = OP_GET_ARRAY_EL;
                                }
                                OP_GET_VAR_REF => {
                                    if let Some(pos) = self.emitter.last_opcode_pos() {
                                        let bytes = self.emitter.byte_code();
                                        let var_idx = get_u16(&bytes[pos + 1..pos + 3]) as u16;
                                        let var_idx =
                                            crate::parser::vars::ExtVarIndex::from(var_idx);
                                        if let Some(name) =
                                            get_ext_var_name(&self.parse_state, var_idx)
                                            && value_matches_bytes(name, b"eval")
                                        {
                                            return Err(self.parser_error(ERR_DIRECT_EVAL));
                                        }
                                    }
                                    opcode = OP_INVALID;
                                }
                                _ => {
                                    opcode = OP_INVALID;
                                }
                            }
                        } else {
                            opcode = OP_INVALID;
                        }
                        arg_count = 0;
                        if self.lexer.token().val() == ')' as i32 {
                            self.next_token()?;
                            if opcode == OP_GET_FIELD
                                || opcode == OP_GET_LENGTH
                                || opcode == OP_GET_ARRAY_EL
                            {
                                self.emitter.emit_op_param(
                                    OP_CALL_METHOD,
                                    0,
                                    op_source_pos,
                                );
                            } else if is_new {
                                self.emitter.emit_op_param(
                                    OP_CALL_CONSTRUCTOR,
                                    0,
                                    op_source_pos,
                                );
                            } else {
                                self.emitter.emit_op_param(OP_CALL, 0, op_source_pos);
                            }
                            is_new = false;
                            continue;
                        }
                        if arg_count >= MAX_CALL_ARGUMENTS {
                            return Err(self.parser_error(ERR_TOO_MANY_CALL_ARGUMENTS));
                        }
                        arg_count += 1;
                        stack.push_int(parse_flags).map_err(Self::stack_error)?;
                        stack.push_int(arg_count).map_err(Self::stack_error)?;
                        stack.push_int(opcode.0 as i32).map_err(Self::stack_error)?;
                        stack.push_int(is_new as i32).map_err(Self::stack_error)?;
                        stack.push_int(op_source_pos as i32)
                            .map_err(Self::stack_error)?;
                        return Ok(encode_call(
                            5,
                            ParseExprFunc::JsParseAssignExpr,
                            0,
                        ));
                    } else if self.lexer.token().val() == '.' as i32 {
                        op_source_pos = self.lexer.token().source_pos();
                        self.next_token()?;
                        let token = self.lexer.token();
                        if !(token.val() == TOK_IDENT || token.val() >= TOK_FIRST_KEYWORD) {
                            return Err(self.parser_error(ERR_EXPECTING_FIELD_NAME));
                        }
                        if value_matches_bytes(token.value(), b"NaN")
                            || value_matches_bytes(token.value(), b"Infinity")
                        {
                            self.emit_push_const(token.value())?;
                            self.emitter.emit_op_pos(OP_GET_ARRAY_EL, op_source_pos);
                        } else if value_matches_bytes(token.value(), b"length") {
                            self.emitter.emit_op_pos(OP_GET_LENGTH, op_source_pos);
                        } else {
                            let idx = self.cpool_add(token.value())?;
                            self.emitter.emit_op_pos(OP_GET_FIELD, op_source_pos);
                            self.emitter.emit_u16(idx);
                        }
                        self.next_token()?;
                    } else if self.lexer.token().val() == '[' as i32 {
                        op_source_pos = self.lexer.token().source_pos();
                        self.next_token()?;
                        stack.push_int(parse_flags).map_err(Self::stack_error)?;
                        stack.push_int(is_new as i32).map_err(Self::stack_error)?;
                        stack.push_int(op_source_pos as i32)
                            .map_err(Self::stack_error)?;
                        return Ok(encode_call(
                            6,
                            ParseExprFunc::JsParseExprComma,
                            0,
                        ));
                    } else if !self.lexer.got_lf()
                        && (self.lexer.token().val() == TOK_DEC
                            || self.lexer.token().val() == TOK_INC)
                    {
                        let op = self.lexer.token().val();
                        op_source_pos = self.lexer.token().source_pos();
                        self.next_token()?;
                        let lvalue =
                            get_lvalue(&mut self.emitter, true).map_err(Self::lvalue_error)?;
                        if self.maybe_drop_result(parse_flags) {
                            self.dropped_result = true;
                            let op = OpCode(OP_DEC.0 + (op - TOK_DEC) as u16);
                            self.emitter.emit_op_pos(op, op_source_pos);
                            let length = self.length_atom_index()?;
                            put_lvalue(
                                &mut self.emitter,
                                lvalue,
                                PutLValue::NoKeepTop,
                                self.parse_state.is_repl(),
                                length,
                            )
                            .map_err(Self::lvalue_error)?;
                        } else {
                            let op = OpCode(OP_POST_DEC.0 + (op - TOK_DEC) as u16);
                            self.emitter.emit_op_pos(op, op_source_pos);
                            let length = self.length_atom_index()?;
                            put_lvalue(
                                &mut self.emitter,
                                lvalue,
                                PutLValue::KeepSecond,
                                self.parse_state.is_repl(),
                                length,
                            )
                            .map_err(Self::lvalue_error)?;
                        }
                    } else {
                        break;
                    }
                }
            }
        }
        Ok(PARSE_STATE_RET as i32)
    }

    fn parse_unary(
        &mut self,
        stack: &mut ParseStack<'_>,
        state: u8,
        parse_flags: i32,
    ) -> Result<i32, ParserError> {
        #[derive(Copy, Clone, Debug, PartialEq, Eq)]
        enum Phase {
            Start,
            AfterUnary,
            AfterVoid,
            AfterIncDec,
            AfterTypeof,
            AfterDelete,
            AfterPostfix,
            AfterPow,
        }

        let mut parse_flags = parse_flags;
        let mut op: i32 = 0;
        let mut op_source_pos: SourcePos = 0;

        let phase = match state {
            PARSE_STATE_INIT => Phase::Start,
            0 => {
                op_source_pos = stack.pop_int() as SourcePos;
                op = stack.pop_int();
                Phase::AfterUnary
            }
            1 => Phase::AfterVoid,
            2 => {
                op_source_pos = stack.pop_int() as SourcePos;
                parse_flags = stack.pop_int();
                op = stack.pop_int();
                Phase::AfterIncDec
            }
            3 => Phase::AfterTypeof,
            4 => Phase::AfterDelete,
            5 => Phase::AfterPostfix,
            6 => {
                op_source_pos = stack.pop_int() as SourcePos;
                Phase::AfterPow
            }
            _ => unreachable!("unexpected parse state"),
        };

        match phase {
            Phase::Start => {
                let token = self.lexer.token();
                match token.val() {
                    TOK_PLUS | TOK_MINUS | TOK_BANG | TOK_TILDE => {
                        op = token.val();
                        op_source_pos = token.source_pos();
                        self.next_token()?;
                        if self.lexer.token().val() == TOK_NUMBER
                            && (op == TOK_MINUS || op == TOK_PLUS)
                        {
                            let number = match self.lexer.token().extra() {
                                TokenExtra::Number(value) => value,
                                _ => return Err(self.parser_error(ERR_UNEXPECTED_CHAR_EXPR)),
                            };
                            let number = if op == TOK_MINUS { -number } else { number };
                            self.emit_push_number(number)?;
                            self.next_token()?;
                            return Ok(PARSE_STATE_RET as i32);
                        }
                        stack.push_int(op).map_err(Self::stack_error)?;
                        stack.push_int(op_source_pos as i32)
                            .map_err(Self::stack_error)?;
                        Ok(encode_call(0, ParseExprFunc::JsParseUnary, 0))
                    }
                    TOK_VOID => {
                        self.next_token()?;
                        Ok(encode_call(1, ParseExprFunc::JsParseUnary, 0))
                    }
                    TOK_DEC | TOK_INC => {
                        op = token.val();
                        op_source_pos = token.source_pos();
                        self.next_token()?;
                        stack.push_int(op).map_err(Self::stack_error)?;
                        stack.push_int(parse_flags).map_err(Self::stack_error)?;
                        stack.push_int(op_source_pos as i32)
                            .map_err(Self::stack_error)?;
                        Ok(encode_call(2, ParseExprFunc::JsParseUnary, 0))
                    }
                    TOK_TYPEOF => {
                        self.next_token()?;
                        Ok(encode_call(3, ParseExprFunc::JsParseUnary, 0))
                    }
                    TOK_DELETE => {
                        self.next_token()?;
                        Ok(encode_call(4, ParseExprFunc::JsParseUnary, 0))
                    }
                    _ => Ok(encode_call(
                        5,
                        ParseExprFunc::JsParsePostfixExpr,
                        parse_flags | PF_ACCEPT_LPAREN as i32,
                    )),
                }
            }
            Phase::AfterUnary => {
                let op_code = match op {
                    TOK_MINUS => OP_NEG,
                    TOK_PLUS => OP_PLUS,
                    TOK_BANG => OP_LNOT,
                    TOK_TILDE => OP_NOT,
                    _ => return Err(self.parser_error(ERR_UNEXPECTED_CHAR_EXPR)),
                };
                self.emitter.emit_op_pos(op_code, op_source_pos);
                Ok(PARSE_STATE_RET as i32)
            }
            Phase::AfterVoid => {
                self.emitter.emit_op(OP_DROP);
                self.emitter.emit_op(OP_UNDEFINED);
                Ok(PARSE_STATE_RET as i32)
            }
            Phase::AfterIncDec => {
                let lvalue = get_lvalue(&mut self.emitter, true).map_err(Self::lvalue_error)?;
                let op_code = OpCode(OP_DEC.0 + (op - TOK_DEC) as u16);
                self.emitter.emit_op_pos(op_code, op_source_pos);
                let special = if self.maybe_drop_result(parse_flags) {
                    self.dropped_result = true;
                    PutLValue::NoKeepTop
                } else {
                    PutLValue::KeepTop
                };
                let length = self.length_atom_index()?;
                put_lvalue(
                    &mut self.emitter,
                    lvalue,
                    special,
                    self.parse_state.is_repl(),
                    length,
                )
                .map_err(Self::lvalue_error)?;
                Ok(PARSE_STATE_RET as i32)
            }
            Phase::AfterTypeof => {
                if self.emitter.get_prev_opcode() == OP_GET_VAR_REF
                    && let Some(pos) = self.emitter.last_opcode_pos()
                {
                    self.emitter.byte_code_mut()[pos] = OP_GET_VAR_REF_NOCHECK.0 as u8;
                }
                self.emitter.emit_op(OP_TYPEOF);
                Ok(PARSE_STATE_RET as i32)
            }
            Phase::AfterDelete => {
                self.emit_delete()?;
                Ok(PARSE_STATE_RET as i32)
            }
            Phase::AfterPostfix => {
                if self.lexer.token().val() == TOK_POW {
                    op_source_pos = self.lexer.token().source_pos();
                    self.next_token()?;
                    stack.push_int(op_source_pos as i32)
                        .map_err(Self::stack_error)?;
                    Ok(encode_call(6, ParseExprFunc::JsParseUnary, 0))
                } else {
                    Ok(PARSE_STATE_RET as i32)
                }
            }
            Phase::AfterPow => {
                self.emitter.emit_op_pos(OP_POW, op_source_pos);
                Ok(PARSE_STATE_RET as i32)
            }
        }
    }

    fn parse_expr_binary(
        &mut self,
        stack: &mut ParseStack<'_>,
        state: u8,
        parse_flags: i32,
    ) -> Result<i32, ParserError> {
        #[derive(Copy, Clone, Debug, PartialEq, Eq)]
        enum Phase {
            Start,
            AfterLeft,
            AfterRight,
        }

        let mut parse_flags = parse_flags;
        let mut opcode = OP_INVALID;
        let mut op_source_pos: SourcePos = 0;

        let mut phase = match state {
            PARSE_STATE_INIT => Phase::Start,
            0 => return Ok(PARSE_STATE_RET as i32),
            1 => {
                parse_flags = stack.pop_int();
                Phase::AfterLeft
            }
            2 => {
                op_source_pos = stack.pop_int() as SourcePos;
                opcode = OpCode(stack.pop_int() as u16);
                parse_flags = stack.pop_int();
                Phase::AfterRight
            }
            _ => unreachable!("unexpected parse state"),
        };

        loop {
            match phase {
                Phase::Start => {
                    let level = parse_flags_level(parse_flags);
                    if level == 0 {
                        return Ok(encode_call(
                            0,
                            ParseExprFunc::JsParseUnary,
                            parse_flags,
                        ));
                    }
                    stack.push_int(parse_flags).map_err(Self::stack_error)?;
                    return Ok(encode_call(
                        1,
                        ParseExprFunc::JsParseExprBinary,
                        parse_flags - (1 << PF_LEVEL_SHIFT),
                    ));
                }
                Phase::AfterLeft => {
                    parse_flags &= !(PF_DROP as i32);
                    let op = self.lexer.token().val();
                    op_source_pos = self.lexer.token().source_pos();
                    let level = parse_flags_level(parse_flags);
                    opcode = match level {
                        1 => match op {
                            TOK_STAR => OP_MUL,
                            TOK_SLASH => OP_DIV,
                            TOK_PERCENT => OP_MOD,
                            _ => return Ok(PARSE_STATE_RET as i32),
                        },
                        2 => match op {
                            TOK_PLUS => OP_ADD,
                            TOK_MINUS => OP_SUB,
                            _ => return Ok(PARSE_STATE_RET as i32),
                        },
                        3 => match op {
                            TOK_SHL => OP_SHL,
                            TOK_SAR => OP_SAR,
                            TOK_SHR => OP_SHR,
                            _ => return Ok(PARSE_STATE_RET as i32),
                        },
                        4 => match op {
                            TOK_LT_CHAR => OP_LT,
                            TOK_GT_CHAR => OP_GT,
                            TOK_LTE => OP_LTE,
                            TOK_GTE => OP_GTE,
                            TOK_INSTANCEOF => OP_INSTANCEOF,
                            TOK_IN => {
                                if has_flag(parse_flags, PF_NO_IN) {
                                    return Ok(PARSE_STATE_RET as i32);
                                }
                                OP_IN
                            }
                            _ => return Ok(PARSE_STATE_RET as i32),
                        },
                        5 => match op {
                            TOK_EQ => OP_EQ,
                            TOK_NEQ => OP_NEQ,
                            TOK_STRICT_EQ => OP_STRICT_EQ,
                            TOK_STRICT_NEQ => OP_STRICT_NEQ,
                            _ => return Ok(PARSE_STATE_RET as i32),
                        },
                        6 => match op {
                            TOK_AMP => OP_AND,
                            _ => return Ok(PARSE_STATE_RET as i32),
                        },
                        7 => match op {
                            TOK_CARET => OP_XOR,
                            _ => return Ok(PARSE_STATE_RET as i32),
                        },
                        8 => match op {
                            TOK_PIPE => OP_OR,
                            _ => return Ok(PARSE_STATE_RET as i32),
                        },
                        _ => return Ok(PARSE_STATE_RET as i32),
                    };
                    self.next_token()?;
                    stack.push_int(parse_flags).map_err(Self::stack_error)?;
                    stack.push_int(opcode.0 as i32).map_err(Self::stack_error)?;
                    stack.push_int(op_source_pos as i32)
                        .map_err(Self::stack_error)?;
                    return Ok(encode_call(
                        2,
                        ParseExprFunc::JsParseExprBinary,
                        parse_flags - (1 << PF_LEVEL_SHIFT),
                    ));
                }
                Phase::AfterRight => {
                    self.emitter.emit_op_pos(opcode, op_source_pos);
                    phase = Phase::AfterLeft;
                }
            }
        }
    }

    fn parse_logical_and_or(
        &mut self,
        stack: &mut ParseStack<'_>,
        state: u8,
        parse_flags: i32,
    ) -> Result<i32, ParserError> {
        #[derive(Copy, Clone, Debug, PartialEq, Eq)]
        enum Phase {
            Start,
            AfterLeft,
            Loop,
            AfterRight,
        }

        let mut parse_flags = parse_flags;
        let mut label = Label::none();
        let mut op: i32 = 0;

        let mut phase = match state {
            PARSE_STATE_INIT => Phase::Start,
            0 => return Ok(PARSE_STATE_RET as i32),
            1 => {
                parse_flags = stack.pop_int();
                Phase::AfterLeft
            }
            2 => {
                parse_flags = stack.pop_int();
                label = Label::from_raw(stack.pop_int());
                Phase::AfterRight
            }
            _ => unreachable!("unexpected parse state"),
        };

        loop {
            match phase {
                Phase::Start => {
                    let level = parse_flags_level(parse_flags);
                    if level == 0 {
                        let next_flags =
                            (parse_flags & !(PF_LEVEL_MASK as i32))
                                | ((8u32 << PF_LEVEL_SHIFT) as i32);
                        return Ok(encode_call(
                            0,
                            ParseExprFunc::JsParseExprBinary,
                            next_flags,
                        ));
                    }
                    stack.push_int(parse_flags).map_err(Self::stack_error)?;
                    return Ok(encode_call(
                        1,
                        ParseExprFunc::JsParseLogicalAndOr,
                        parse_flags - (1 << PF_LEVEL_SHIFT),
                    ));
                }
                Phase::AfterLeft => {
                    let level = parse_flags_level(parse_flags);
                    op = if level == 1 { TOK_LAND } else { TOK_LOR };
                    parse_flags &= !(PF_DROP as i32);
                    if self.lexer.token().val() != op {
                        return Ok(PARSE_STATE_RET as i32);
                    }
                    label = Label::new();
                    phase = Phase::Loop;
                }
                Phase::Loop => {
                    self.next_token()?;
                    self.emitter.emit_op(OP_DUP);
                    if op == TOK_LAND {
                        self.emitter.emit_goto(OP_IF_FALSE, &mut label);
                    } else {
                        self.emitter.emit_goto(OP_IF_TRUE, &mut label);
                    }
                    self.emitter.emit_op(OP_DROP);
                    stack.push_int(label.raw()).map_err(Self::stack_error)?;
                    stack.push_int(parse_flags).map_err(Self::stack_error)?;
                    return Ok(encode_call(
                        2,
                        ParseExprFunc::JsParseLogicalAndOr,
                        parse_flags - (1 << PF_LEVEL_SHIFT),
                    ));
                }
                Phase::AfterRight => {
                    let level = parse_flags_level(parse_flags);
                    op = if level == 1 { TOK_LAND } else { TOK_LOR };
                    if self.lexer.token().val() != op {
                        self.emitter.emit_label(&mut label);
                        return Ok(PARSE_STATE_RET as i32);
                    }
                    phase = Phase::Loop;
                }
            }
        }
    }

    fn parse_cond_expr(
        &mut self,
        stack: &mut ParseStack<'_>,
        state: u8,
        parse_flags: i32,
    ) -> Result<i32, ParserError> {
        #[derive(Copy, Clone, Debug, PartialEq, Eq)]
        enum Phase {
            Start,
            AfterCond,
            AfterThen,
            AfterElse,
        }

        let mut parse_flags = parse_flags;
        let mut label1 = Label::none();
        let mut label2 = Label::none();

        let phase = match state {
            PARSE_STATE_INIT => Phase::Start,
            0 => {
                parse_flags = stack.pop_int();
                label1 = Label::from_raw(stack.pop_int());
                Phase::AfterThen
            }
            1 => {
                parse_flags = stack.pop_int();
                label2 = Label::from_raw(stack.pop_int());
                Phase::AfterElse
            }
            2 => {
                parse_flags = stack.pop_int();
                Phase::AfterCond
            }
            _ => unreachable!("unexpected parse state"),
        };

        match phase {
            Phase::Start => {
                stack.push_int(parse_flags).map_err(Self::stack_error)?;
                Ok(encode_call(
                    2,
                    ParseExprFunc::JsParseLogicalAndOr,
                    parse_flags | ((2u32 << PF_LEVEL_SHIFT) as i32),
                ))
            }
            Phase::AfterCond => {
                parse_flags &= !(PF_DROP as i32);
                if self.lexer.token().val() != '?' as i32 {
                    return Ok(PARSE_STATE_RET as i32);
                }
                self.next_token()?;
                label1 = Label::new();
                self.emitter.emit_goto(OP_IF_FALSE, &mut label1);
                stack.push_int(label1.raw()).map_err(Self::stack_error)?;
                stack.push_int(parse_flags).map_err(Self::stack_error)?;
                Ok(encode_call(
                    0,
                    ParseExprFunc::JsParseAssignExpr,
                    parse_flags,
                ))
            }
            Phase::AfterThen => {
                label2 = Label::new();
                self.emitter.emit_goto(OP_GOTO, &mut label2);
                self.expect(':' as i32)?;
                self.emitter.emit_label(&mut label1);
                stack.push_int(label2.raw()).map_err(Self::stack_error)?;
                stack.push_int(parse_flags).map_err(Self::stack_error)?;
                Ok(encode_call(
                    1,
                    ParseExprFunc::JsParseAssignExpr,
                    parse_flags,
                ))
            }
            Phase::AfterElse => {
                self.emitter.emit_label(&mut label2);
                Ok(PARSE_STATE_RET as i32)
            }
        }
    }

    fn parse_assign_expr(
        &mut self,
        stack: &mut ParseStack<'_>,
        state: u8,
        parse_flags: i32,
    ) -> Result<i32, ParserError> {
        #[derive(Copy, Clone, Debug, PartialEq, Eq)]
        enum Phase {
            Start,
            AfterCond,
            AfterAssign,
        }

        let mut parse_flags = parse_flags;
        let mut op: i32 = 0;
        let mut opcode = OP_INVALID;
        let mut var_idx: i32 = 0;
        let mut op_source_pos: SourcePos = 0;
        let mut source_pos: SourcePos = 0;

        let phase = match state {
            PARSE_STATE_INIT => Phase::Start,
            0 => {
                source_pos = stack.pop_int() as SourcePos;
                op_source_pos = stack.pop_int() as SourcePos;
                parse_flags = stack.pop_int();
                var_idx = stack.pop_int();
                opcode = OpCode(stack.pop_int() as u16);
                op = stack.pop_int();
                Phase::AfterAssign
            }
            1 => {
                parse_flags = stack.pop_int();
                Phase::AfterCond
            }
            _ => unreachable!("unexpected parse state"),
        };

        match phase {
            Phase::Start => {
                stack.push_int(parse_flags).map_err(Self::stack_error)?;
                Ok(encode_call(
                    1,
                    ParseExprFunc::JsParseCondExpr,
                    parse_flags,
                ))
            }
            Phase::AfterCond => {
                op = self.lexer.token().val();
                if op == '=' as i32
                    || (TOK_MUL_ASSIGN..=TOK_OR_ASSIGN).contains(&op)
                {
                    op_source_pos = self.lexer.token().source_pos();
                    self.next_token()?;
                    let is_compound = op != '=' as i32;
                    let lvalue =
                        get_lvalue(&mut self.emitter, is_compound).map_err(Self::lvalue_error)?;
                    opcode = lvalue.opcode();
                    var_idx = lvalue.var_idx();
                    source_pos = lvalue.source_pos();
                    stack.push_int(op).map_err(Self::stack_error)?;
                    stack.push_int(opcode.0 as i32).map_err(Self::stack_error)?;
                    stack.push_int(var_idx).map_err(Self::stack_error)?;
                    stack.push_int(parse_flags).map_err(Self::stack_error)?;
                    stack.push_int(op_source_pos as i32)
                        .map_err(Self::stack_error)?;
                    stack.push_int(source_pos as i32).map_err(Self::stack_error)?;
                    Ok(encode_call(
                        0,
                        ParseExprFunc::JsParseAssignExpr,
                        parse_flags & !(PF_DROP as i32),
                    ))
                } else {
                    Ok(PARSE_STATE_RET as i32)
                }
            }
            Phase::AfterAssign => {
                if op != '=' as i32 {
                    let assign_ops = [
                        OP_MUL, OP_DIV, OP_MOD, OP_ADD, OP_SUB, OP_SHL, OP_SAR, OP_SHR, OP_AND,
                        OP_XOR, OP_OR, OP_POW,
                    ];
                    let idx = (op - TOK_MUL_ASSIGN) as usize;
                    if let Some(opcode) = assign_ops.get(idx) {
                        self.emitter.emit_op_pos(*opcode, op_source_pos);
                    }
                }

                let special = if self.maybe_drop_result(parse_flags) {
                    self.dropped_result = true;
                    PutLValue::NoKeepTop
                } else {
                    PutLValue::KeepTop
                };
                let length = self.length_atom_index()?;
                let lvalue = crate::parser::lvalue::LValue::new(opcode, var_idx, source_pos);
                put_lvalue(
                    &mut self.emitter,
                    lvalue,
                    special,
                    self.parse_state.is_repl(),
                    length,
                )
                .map_err(Self::lvalue_error)?;
                Ok(PARSE_STATE_RET as i32)
            }
        }
    }

    fn parse_expr_comma(
        &mut self,
        stack: &mut ParseStack<'_>,
        state: u8,
        parse_flags: i32,
    ) -> Result<i32, ParserError> {
        #[derive(Copy, Clone, Debug, PartialEq, Eq)]
        enum Phase {
            Start,
            AfterAssign,
        }

        let mut parse_flags = parse_flags;
        let mut comma = false;

        let mut phase = match state {
            PARSE_STATE_INIT => Phase::Start,
            0 => {
                parse_flags = stack.pop_int();
                comma = stack.pop_int() != 0;
                Phase::AfterAssign
            }
            _ => unreachable!("unexpected parse state"),
        };

        loop {
            match phase {
                Phase::Start => {
                    self.dropped_result = false;
                    stack.push_int(comma as i32).map_err(Self::stack_error)?;
                    stack.push_int(parse_flags).map_err(Self::stack_error)?;
                    return Ok(encode_call(
                        0,
                        ParseExprFunc::JsParseAssignExpr,
                        parse_flags,
                    ));
                }
                Phase::AfterAssign => {
                    if comma {
                        self.emitter.clear_last_opcode();
                    }
                    if self.lexer.token().val() != ',' as i32 {
                        break;
                    }
                    comma = true;
                    if !self.dropped_result {
                        self.emitter.emit_op(OP_DROP);
                    }
                    self.next_token()?;
                    phase = Phase::Start;
                }
            }
        }

        if has_flag(parse_flags, PF_DROP) && !self.dropped_result {
            self.emitter.emit_op(OP_DROP);
        }
        Ok(PARSE_STATE_RET as i32)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::opcode::{OP_ADD, OP_ARRAY_FROM, OP_DUP, OP_MUL, OP_PUT_LOC0, OP_PUSH_1, OP_PUSH_2, OP_PUSH_3, OP_PUSH_FALSE, OP_PUSH_TRUE, OPCODES};

    fn decode_ops(bytes: &[u8]) -> Vec<OpCode> {
        let mut ops = Vec::new();
        let mut pos = 0usize;
        while pos < bytes.len() {
            let op = OpCode(bytes[pos] as u16);
            ops.push(op);
            let size = OPCODES[op.as_usize()].size as usize;
            if size == 0 {
                break;
            }
            pos += size;
        }
        ops
    }

    fn parse_ops(input: &str) -> Vec<OpCode> {
        let mut parser = ExprParser::new(input.as_bytes());
        parser.parse_expression(0).expect("parse");
        decode_ops(parser.bytecode())
    }

    #[test]
    fn expr_precedence_mul_before_add() {
        let ops = parse_ops("1+2*3");
        assert_eq!(ops, [OP_PUSH_1, OP_PUSH_2, OP_PUSH_3, OP_MUL, OP_ADD]);
    }

    #[test]
    fn expr_array_literal_small() {
        let ops = parse_ops("[1,2]");
        assert_eq!(ops, [OP_PUSH_1, OP_PUSH_2, OP_ARRAY_FROM]);
    }

    #[test]
    fn expr_assignment_emits_put_loc() {
        let mut parser = ExprParser::new(b"x=1");
        parser.add_local_var("x").expect("var");
        parser.parse_expression(0).expect("parse");
        let ops = decode_ops(parser.bytecode());
        assert_eq!(ops, [OP_PUSH_1, OP_DUP, OP_PUT_LOC0]);
    }

    #[test]
    fn expr_logical_and_short_circuits() {
        let ops = parse_ops("true && false");
        assert_eq!(ops, [OP_PUSH_TRUE, OP_DUP, OP_IF_FALSE, OP_DROP, OP_PUSH_FALSE]);
    }
}
