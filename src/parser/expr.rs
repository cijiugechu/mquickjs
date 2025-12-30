use core::marker::PhantomData;
use core::mem::size_of;
use core::ptr::{self, NonNull};

use crate::context::JSContext;
use crate::cutils::{get_u16, put_u16};
use crate::enums::JSVarRefKind;
use crate::function_bytecode::{FunctionBytecode, FunctionBytecodeFields, FunctionBytecodeHeader};
use crate::jsvalue::{JSValue, JSWord};
use crate::opcode::{
    OpCode, OP_ADD, OP_AND, OP_ARGUMENTS, OP_ARRAY_FROM, OP_CALL, OP_CALL_CONSTRUCTOR,
    OP_CALL_METHOD, OP_CATCH, OP_DEC, OP_DEFINE_FIELD, OP_DEFINE_GETTER, OP_DEFINE_SETTER,
    OP_DELETE, OP_DIV, OP_DROP, OP_DUP, OP_EQ, OP_FCLOSURE, OP_FOR_IN_START, OP_FOR_OF_NEXT,
    OP_FOR_OF_START, OP_GOSUB, OP_GOTO, OP_GTE, OP_GT, OP_IF_FALSE, OP_IF_TRUE, OP_IN,
    OP_INSTANCEOF, OP_LNOT, OP_LT, OP_LTE, OP_MOD, OP_MUL, OP_NEG, OP_NEQ, OP_NEW_TARGET, OP_NOT,
    OP_NULL, OP_OBJECT, OP_OR, OP_PLUS, OP_POST_DEC, OP_POW, OP_PUSH_CONST, OP_PUSH_FALSE,
    OP_PUSH_THIS, OP_PUSH_TRUE, OP_PUSH_VALUE, OP_REGEXP, OP_RET, OP_RETURN, OP_RETURN_UNDEF,
    OP_SAR,
    OP_SET_PROTO, OP_SHL, OP_SHR, OP_STRICT_EQ, OP_STRICT_NEQ, OP_SUB, OP_THIS_FUNC, OP_THROW,
    OP_TYPEOF, OP_UNDEFINED, OP_XOR, OP_GET_ARG, OP_GET_ARRAY_EL, OP_GET_ARRAY_EL2, OP_GET_FIELD,
    OP_GET_FIELD2, OP_GET_LENGTH, OP_GET_LENGTH2, OP_GET_LOC, OP_GET_VAR_REF,
    OP_GET_VAR_REF_NOCHECK, OP_INVALID, OP_PUT_ARG, OP_PUT_ARRAY_EL, OP_PUT_LOC,
    OP_PUT_VAR_REF_NOCHECK,
};
use crate::parser::control_flow::{emit_break, emit_return, BreakStack, ControlFlowError};
use crate::parser::emit::{BytecodeEmitter, Label};
use crate::parser::error::{
    parse_expect, parse_expect1, parse_expect_semi, ParserError, ParserErrorKind, ERR_NO_MEM,
};
use crate::parser::lexer::{
    value_matches_bytes, ParseState, SKIP_HAS_ARGUMENTS, SKIP_HAS_FUNC_NAME, SKIP_HAS_SEMI,
};
use crate::parser::lvalue::{get_lvalue, put_lvalue, LValueError, PutLValue};
use crate::parser::parse_stack::{ParseStack, ParseStackError};
use crate::parser::parse_state::{
    ParseExprFunc, ParseProp, JSParseFunction, JSParseState, PF_ACCEPT_LPAREN, PF_DROP,
    PF_LEVEL_MASK, PF_LEVEL_SHIFT, PF_NO_IN, PARSE_STATE_INIT, PARSE_STATE_RET,
};
use crate::parser::property_name::parse_property_name;
use crate::parser::regexp::{compile_regexp, RegExpError};
use crate::parser::stack_size::{compute_stack_size, StackSizeError};
use crate::parser::tokens::{
    TOK_BREAK, TOK_CASE, TOK_CATCH, TOK_CONTINUE, TOK_DEC, TOK_DEFAULT, TOK_DO, TOK_ELSE, TOK_EOF,
    TOK_FALSE, TOK_FIRST_KEYWORD, TOK_FOR, TOK_FUNCTION, TOK_IDENT, TOK_IF, TOK_INC, TOK_IN,
    TOK_INSTANCEOF, TOK_LAND, TOK_LOR, TOK_MUL_ASSIGN, TOK_NEQ, TOK_NEW, TOK_NULL, TOK_NUMBER,
    TOK_OR_ASSIGN, TOK_POW, TOK_REGEXP, TOK_RETURN, TOK_SAR, TOK_SHL, TOK_SHR, TOK_STRICT_EQ,
    TOK_STRICT_NEQ, TOK_STRING, TOK_SWITCH, TOK_THIS, TOK_THROW, TOK_TRY, TOK_TRUE, TOK_TYPEOF,
    TOK_VAR, TOK_VOID, TOK_WHILE, TOK_EQ, TOK_GTE, TOK_LTE, TOK_DELETE, TOK_FINALLY,
};
use crate::parser::types::{ParsePos, SourcePos, TokenExtra};
use crate::parser::vars::{
    add_ext_var, add_var, convert_ext_vars_to_local_vars, define_var, find_ext_var, find_var,
    get_ext_var_name, put_var, resolve_var_refs, ByteArray, VarAllocator, VarError, ValueArray,
};
use crate::string::runtime::string_view;

fn map_func_value(map: &[(JSValue, JSValue)], value: JSValue) -> JSValue {
    map.iter()
        .find(|(old, _)| *old == value)
        .map(|(_, new)| *new)
        .unwrap_or(value)
}

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
const ERR_FUNCTION_NAME_EXPECTED: &str = "function name expected";
const ERR_MISSING_FORMAL_PARAMETER: &str = "missing formal parameter";
const ERR_INVALID_ARGUMENT_NAME: &str = "invalid argument name";
const ERR_DUPLICATE_ARGUMENT_NAME: &str = "duplicate argument name";
const ERR_INVALID_STRING: &str = "invalid string value";
const ERR_INVALID_FUNCTION_PTR: &str = "invalid function bytecode pointer";
const ERR_INVALID_VALUE_ARRAY_PTR: &str = "invalid value array pointer";
const ERR_INVALID_BYTE_ARRAY_PTR: &str = "invalid byte array pointer";
const ERR_TOO_MANY_LOCAL_VARS: &str = "too many local variables";
const ERR_VARIABLE_NAME_EXPECTED: &str = "variable name expected";
const ERR_INVALID_VARIABLE_NAME: &str = "invalid variable name";
const ERR_RETURN_NOT_IN_FUNCTION: &str = "return not in a function";
const ERR_LINE_TERMINATOR_AFTER_THROW: &str = "line terminator not allowed after throw";
const ERR_EXPECTED_OF_OR_IN: &str = "expected 'of' or 'in' in for control expression";
const ERR_IDENTIFIER_EXPECTED: &str = "identifier expected";
const ERR_CATCH_VAR_EXISTS: &str = "catch variable already exists";
const ERR_DUPLICATE_DEFAULT: &str = "duplicate default";
const ERR_INVALID_SWITCH: &str = "invalid switch statement";
const ERR_EXPECTING_CATCH_OR_FINALLY: &str = "expecting catch or finally";
const ERR_DUPLICATE_LABEL: &str = "duplicate label name";

const MAX_CALL_ARGUMENTS: i32 = 65535;
const DEFAULT_STACK_SIZE: usize = 256;

const TOK_PAREN_OPEN: i32 = b'(' as i32;
const TOK_PAREN_CLOSE: i32 = b')' as i32;
const TOK_BRACE_OPEN: i32 = b'{' as i32;
const TOK_BRACE_CLOSE: i32 = b'}' as i32;
const TOK_BRACKET_OPEN: i32 = b'[' as i32;
const TOK_SEMI: i32 = b';' as i32;
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
    ((val as u32) << 1) | (JSValue::JS_TAG_INT as u32)
}

fn encode_jsvalue(val: JSValue) -> u32 {
    if val.is_int() {
        return encode_short_int(val.get_int());
    }
    let tag = val.get_special_tag() as u32;
    let payload = val.get_special_value() as u32;
    tag | (payload << JSValue::JS_TAG_SPECIAL_BITS)
}

fn value_bytes(val: JSValue) -> Result<Vec<u8>, ParserError> {
    let mut scratch = [0u8; 5];
    let Some(view) = string_view(val, &mut scratch) else {
        return Err(ParserError::new(ParserErrorKind::Static(ERR_INVALID_STRING), 0));
    };
    Ok(view.bytes().to_vec())
}

fn is_short_float_value(val: JSValue) -> bool {
    #[cfg(target_pointer_width = "64")]
    {
        val.is_short_float()
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
        JSValue::from_ptr(self.ptr)
    }

    fn ptr(&self) -> NonNull<FunctionBytecode> {
        self.ptr
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

pub struct ExprParser<'a, 'ctx> {
    ctx_marker: PhantomData<&'ctx mut JSContext>,
    source: &'a [u8],
    lexer: ParseState<'a>,
    emitter: BytecodeEmitter<'a>,
    parse_state_ptr: NonNull<JSParseState>,
    prev_parse_state: Option<NonNull<JSParseState>>,
    parse_state_attached: bool,
    alloc: VarAllocator,
    func: OwnedFunctionBytecode,
    nested_funcs: Vec<OwnedFunctionBytecode>,
    break_stack: BreakStack,
    dropped_result: bool,
    atoms: AtomCache,
}

impl<'a, 'ctx> ExprParser<'a, 'ctx> {
    pub fn new(ctx: &'ctx mut JSContext, source: &'a [u8]) -> Self {
        let header = FunctionBytecodeHeader::new(false, false, false, 0, false);
        let fields = FunctionBytecodeFields {
            func_name: JSValue::JS_NULL,
            byte_code: JSValue::JS_NULL,
            cpool: JSValue::JS_NULL,
            vars: JSValue::JS_NULL,
            ext_vars: JSValue::JS_NULL,
            stack_size: 0,
            ext_vars_len: 0,
            filename: JSValue::JS_NULL,
            pc2line: JSValue::JS_NULL,
            source_pos: 0,
        };
        let func = OwnedFunctionBytecode::new(header, fields);
        let lexer = ParseState::new(source, ctx);
        let parse_state = Box::new(JSParseState::new(NonNull::dangling(), false));
        parse_state.set_cur_func(func.value());
        parse_state.set_eval_ret_idx(-1);
        parse_state.set_source(source.as_ptr(), lexer.buf_len() as u32);
        // Keep parse_state in a stable heap allocation so raw pointers survive parser reborrows.
        let parse_state_ptr = NonNull::new(Box::into_raw(parse_state))
            .expect("parse_state allocation must be non-null");
        Self {
            ctx_marker: PhantomData,
            source,
            lexer,
            emitter: BytecodeEmitter::new(source, 0, false),
            parse_state_ptr,
            prev_parse_state: None,
            parse_state_attached: false,
            alloc: VarAllocator::new(),
            func,
            nested_funcs: Vec::new(),
            break_stack: BreakStack::new(),
            dropped_result: false,
            atoms: AtomCache::default(),
        }
    }

    fn parse_state_ref(&self) -> &JSParseState {
        // SAFETY: JSParseState relies on interior mutability (Cells) and we never hand out
        // &mut JSParseState, so shared references are safe.
        unsafe { self.parse_state_ptr.as_ref() }
    }

    fn with_parse_state_and_alloc<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&JSParseState, &mut VarAllocator) -> R,
    {
        let alloc = &mut self.alloc;
        // SAFETY: JSParseState relies on interior mutability and the pointer is valid here.
        let parse_state = unsafe { self.parse_state_ptr.as_ref() };
        f(parse_state, alloc)
    }

    pub(crate) fn attach_parse_state(&mut self) {
        let parse_state_ptr = self.parse_state_ptr;
        if !self.parse_state_attached {
            self.prev_parse_state = self.lexer.ctx_mut().swap_parse_state(Some(parse_state_ptr));
            self.parse_state_attached = true;
        } else {
            let _ = self.lexer.ctx_mut().swap_parse_state(Some(parse_state_ptr));
        }
        self.lexer.set_parse_state(Some(parse_state_ptr));
    }

    pub fn has_column(&self) -> bool {
        self.parse_state_ref().has_column()
    }

    pub fn set_has_column(&mut self, has_column: bool) {
        self.parse_state_ref().set_has_column(has_column);
    }

    pub fn parse_expression(&mut self, parse_flags: u32) -> Result<(), ParserError> {
        self.attach_parse_state();
        self.lexer.next_token().map_err(ParserError::from)?;
        self.parse_expr2(parse_flags as i32)
    }

    pub fn parse_statement(&mut self) -> Result<(), ParserError> {
        self.attach_parse_state();
        self.lexer.next_token().map_err(ParserError::from)?;
        let mut storage = vec![JSValue::JS_NULL; DEFAULT_STACK_SIZE];
        let mut stack = ParseStack::new(&mut storage);
        self.parse_call(&mut stack, ParseExprFunc::JsParseStatement, 0)
    }

    pub fn parse_program(&mut self) -> Result<(), ParserError> {
        self.parse_program_with_flags(false, false, false)
    }

    pub fn parse_program_with_flags(
        &mut self,
        is_eval: bool,
        has_retval: bool,
        is_repl: bool,
    ) -> Result<(), ParserError> {
        self.attach_parse_state();
        let func_val = self.func.value();
        self.nested_funcs.clear();
        self.parse_state_ref().reset_parse_state(0, func_val);
        self.parse_state_ref().set_is_eval(is_eval);
        self.parse_state_ref().set_is_repl(is_repl);
        self.parse_state_ref().set_has_retval(has_retval);
        self.parse_state_ref().set_hoisted_code_len(0);
        let has_column = self.parse_state_ref().has_column();
        self.func.as_mut().set_has_column(has_column);

        self.lexer.reset_to_pos(0);
        self.emitter = BytecodeEmitter::new(self.source, 0, has_column);
        self.break_stack = BreakStack::new();
        self.dropped_result = false;

        self.lexer.next_token().map_err(ParserError::from)?;

        if self.parse_state_ref().has_retval() {
            let name = self
                .lexer
                .intern_identifier(b"_ret_", 0)
                .map_err(ParserError::from)?;
            let idx = self
                .with_parse_state_and_alloc(|state, alloc| add_var(state, alloc, name))
                .map_err(|err| Self::var_error(err, 0))?;
            self.parse_state_ref().set_eval_ret_idx(i32::from(idx));
        }

        while self.lexer.token().val() != TOK_EOF {
            self.parse_source_element()?;
        }

        let eval_ret_idx = self.parse_state_ref().eval_ret_idx();
        if eval_ret_idx >= 0 {
            let idx = eval_ret_idx as u32;
            let source_pos = self.emitter.pc2line_source_pos();
            self.emitter.emit_var(OP_GET_LOC, idx, source_pos);
            self.emitter.emit_op(OP_RETURN);
        } else {
            self.emitter.emit_op(OP_RETURN_UNDEF);
        }

        self.store_current_function(is_eval)?;
        self.parse_local_functions(func_val)?;
        Ok(())
    }

    pub fn bytecode(&self) -> &[u8] {
        self.emitter.byte_code()
    }

    /// Returns the function bytecode JSValue after parsing.
    pub fn take_func_bytecode(&self) -> JSValue {
        self.func.value()
    }

    pub(crate) fn materialize_bytecode(&mut self) -> Result<JSValue, ParserError> {
        let mut func_values = Vec::with_capacity(1 + self.nested_funcs.len());
        func_values.push(self.func.value());
        for func in &self.nested_funcs {
            func_values.push(func.value());
        }

        let mut func_map = Vec::with_capacity(func_values.len());
        for func_val in &func_values {
            let (header, func_name, stack_size, ext_vars_len, filename, source_pos) = {
                let func = self.function_ref(*func_val)?;
                (
                    func.header(),
                    func.func_name(),
                    func.stack_size(),
                    func.ext_vars_len(),
                    func.filename(),
                    func.source_pos(),
                )
            };
            let fields = FunctionBytecodeFields {
                func_name,
                byte_code: JSValue::JS_NULL,
                cpool: JSValue::JS_NULL,
                vars: JSValue::JS_NULL,
                ext_vars: JSValue::JS_NULL,
                stack_size,
                ext_vars_len,
                filename,
                pc2line: JSValue::JS_NULL,
                source_pos,
            };
            let new_val = self
                .lexer
                .ctx_mut()
                .alloc_function_bytecode(header, fields)
                .map_err(|_| ParserError::new(ParserErrorKind::Static(ERR_NO_MEM), 0))?;
            func_map.push((*func_val, new_val));
        }

        let mut array_map: Vec<(JSValue, JSValue)> = Vec::new();
        for func_val in &func_values {
            let (byte_code, pc2line, cpool, vars, ext_vars) = {
                let func = self.function_ref(*func_val)?;
                (
                    func.byte_code(),
                    func.pc2line(),
                    func.cpool(),
                    func.vars(),
                    func.ext_vars(),
                )
            };
            let new_val = map_func_value(&func_map, *func_val);
            let mut func_ptr = new_val.to_ptr::<FunctionBytecode>().ok_or_else(|| {
                ParserError::new(ParserErrorKind::Static(ERR_INVALID_FUNCTION_PTR), 0)
            })?;
            // SAFETY: new_val points at a live function bytecode allocation.
            let new_func = unsafe { func_ptr.as_mut() };

            let byte_code = self.materialize_value(byte_code, &func_map, &mut array_map)?;
            let pc2line = self.materialize_value(pc2line, &func_map, &mut array_map)?;
            let cpool = self.materialize_value(cpool, &func_map, &mut array_map)?;
            let vars = self.materialize_value(vars, &func_map, &mut array_map)?;
            let ext_vars = self.materialize_value(ext_vars, &func_map, &mut array_map)?;

            new_func.set_byte_code(byte_code);
            new_func.set_pc2line(pc2line);
            new_func.set_cpool(cpool);
            new_func.set_vars(vars);
            new_func.set_ext_vars(ext_vars);
        }

        Ok(map_func_value(&func_map, self.func.value()))
    }

    pub fn add_local_var(&mut self, name: &str) -> Result<JSValue, ParserError> {
        let value = self
            .lexer
            .intern_identifier(name.as_bytes(), 0)
            .map_err(ParserError::from)?;
        self.with_parse_state_and_alloc(|state, alloc| add_var(state, alloc, value))
            .map_err(|err| Self::var_error(err, 0))?;
        Ok(value)
    }

    fn current_func_ref(&self) -> Result<&FunctionBytecode, ParserError> {
        self.function_ref(self.parse_state_ref().cur_func())
    }

    fn current_func_mut(&mut self) -> Result<&mut FunctionBytecode, ParserError> {
        self.function_mut(self.parse_state_ref().cur_func())
    }

    fn function_ref(&self, value: JSValue) -> Result<&FunctionBytecode, ParserError> {
        if value == self.func.value() {
            return Ok(self.func.as_ref());
        }
        for func in &self.nested_funcs {
            if func.value() == value {
                return Ok(func.as_ref());
            }
        }
        Err(ParserError::new(
            ParserErrorKind::Static(ERR_INVALID_FUNCTION_PTR),
            0,
        ))
    }

    fn function_mut(&mut self, value: JSValue) -> Result<&mut FunctionBytecode, ParserError> {
        if value == self.func.value() {
            return Ok(self.func.as_mut());
        }
        for func in &mut self.nested_funcs {
            if func.value() == value {
                return Ok(func.as_mut());
            }
        }
        Err(ParserError::new(
            ParserErrorKind::Static(ERR_INVALID_FUNCTION_PTR),
            0,
        ))
    }

    fn func_ptr(&self, value: JSValue) -> Option<NonNull<FunctionBytecode>> {
        if value == self.func.value() {
            return Some(self.func.ptr());
        }
        self.nested_funcs
            .iter()
            .find(|func| func.value() == value)
            .map(|func| func.ptr())
    }

    fn value_array_ref(&self, value: JSValue) -> Result<&ValueArray, ParserError> {
        let ptr = value.to_ptr::<ValueArray>().ok_or_else(|| {
            ParserError::new(ParserErrorKind::Static(ERR_INVALID_VALUE_ARRAY_PTR), 0)
        })?;
        // SAFETY: value points to a ValueArray allocated by VarAllocator.
        Ok(unsafe { ptr.as_ref() })
    }

    fn value_array_mut(&mut self, value: JSValue) -> Result<&mut ValueArray, ParserError> {
        let mut ptr = value.to_ptr::<ValueArray>().ok_or_else(|| {
            ParserError::new(ParserErrorKind::Static(ERR_INVALID_VALUE_ARRAY_PTR), 0)
        })?;
        // SAFETY: value points to a ValueArray allocated by VarAllocator.
        Ok(unsafe { ptr.as_mut() })
    }

    fn byte_array_ref(&self, value: JSValue) -> Result<&ByteArray, ParserError> {
        let ptr = value.to_ptr::<ByteArray>().ok_or_else(|| {
            ParserError::new(ParserErrorKind::Static(ERR_INVALID_BYTE_ARRAY_PTR), 0)
        })?;
        // SAFETY: value points to a ByteArray allocated by VarAllocator.
        Ok(unsafe { ptr.as_ref() })
    }

    fn byte_array_mut(&mut self, value: JSValue) -> Result<&mut ByteArray, ParserError> {
        let mut ptr = value.to_ptr::<ByteArray>().ok_or_else(|| {
            ParserError::new(ParserErrorKind::Static(ERR_INVALID_BYTE_ARRAY_PTR), 0)
        })?;
        // SAFETY: value points to a ByteArray allocated by VarAllocator.
        Ok(unsafe { ptr.as_mut() })
    }

    fn materialize_value(
        &mut self,
        value: JSValue,
        func_map: &[(JSValue, JSValue)],
        array_map: &mut Vec<(JSValue, JSValue)>,
    ) -> Result<JSValue, ParserError> {
        if let Some(mapped) = func_map.iter().find(|(old, _)| *old == value) {
            return Ok(mapped.1);
        }
        if let Some(mapped) = array_map.iter().find(|(old, _)| *old == value) {
            return Ok(mapped.1);
        }
        if let Some(array) = self.alloc.value_array_ref(value) {
            let items: Vec<JSValue> = array.values().to_vec();
            let ctx = self.lexer.ctx_mut();
            let ptr = ctx
                .alloc_value_array(items.len())
                .map_err(|_| ParserError::new(ParserErrorKind::Static(ERR_NO_MEM), 0))?;
            let new_val = JSValue::from_ptr(ptr);
            array_map.push((value, new_val));
            let base = unsafe { ptr.as_ptr().add(size_of::<JSWord>()) as *mut JSValue };
            for (idx, item) in items.iter().enumerate() {
                let out = self.materialize_value(*item, func_map, array_map)?;
                unsafe {
                    // SAFETY: base points at a writable ValueArray slot.
                    ptr::write_unaligned(base.add(idx), out);
                }
            }
            return Ok(new_val);
        }
        if let Some(array) = self.alloc.byte_array_ref(value) {
            let buf = array.buf().to_vec();
            let ctx = self.lexer.ctx_mut();
            let new_val = ctx
                .alloc_byte_array(&buf)
                .map_err(|_| ParserError::new(ParserErrorKind::Static(ERR_NO_MEM), 0))?;
            array_map.push((value, new_val));
            return Ok(new_val);
        }
        Ok(value)
    }

    fn parse_expr2(&mut self, parse_flags: i32) -> Result<(), ParserError> {
        let mut storage = vec![JSValue::JS_NULL; DEFAULT_STACK_SIZE];
        let mut stack = ParseStack::new(&mut storage);
        self.parse_call(&mut stack, ParseExprFunc::JsParseExprComma, parse_flags)
    }

    fn parse_assign_expr2(&mut self, parse_flags: i32) -> Result<(), ParserError> {
        let mut storage = vec![JSValue::JS_NULL; DEFAULT_STACK_SIZE];
        let mut stack = ParseStack::new(&mut storage);
        self.parse_call(&mut stack, ParseExprFunc::JsParseAssignExpr, parse_flags)
    }

    fn parse_expr(&mut self) -> Result<(), ParserError> {
        self.parse_expr2(0)
    }

    fn parse_expr_paren(&mut self) -> Result<(), ParserError> {
        parse_expect(&mut self.lexer, '(' as i32)?;
        self.parse_expr()?;
        parse_expect(&mut self.lexer, ')' as i32)
    }

    fn skip_expr(&mut self) -> Result<(), ParserError> {
        loop {
            match self.lexer.token().val() {
                TOK_PAREN_CLOSE => return Ok(()),
                TOK_SEMI | TOK_EOF => {
                    let pos = self.lexer.token().source_pos() as usize;
                    return Err(ParserError::new(
                        ParserErrorKind::ExpectingChar(b')'),
                        pos,
                    ));
                }
                TOK_PAREN_OPEN | TOK_BRACKET_OPEN | TOK_BRACE_OPEN => {
                    self.lexer.skip_parens(None).map_err(ParserError::from)?;
                }
                _ => {
                    self.next_token()?;
                }
            }
        }
    }

    fn parse_source_element(&mut self) -> Result<(), ParserError> {
        if self.lexer.token().val() == TOK_FUNCTION {
            self.parse_function_decl(JSParseFunction::Statement, JSValue::JS_NULL)?;
            return Ok(());
        }
        let mut storage = vec![JSValue::JS_NULL; DEFAULT_STACK_SIZE];
        let mut stack = ParseStack::new(&mut storage);
        self.parse_call(&mut stack, ParseExprFunc::JsParseStatement, 0)?;
        Ok(())
    }

    fn reset_for_function(&mut self, func: JSValue, source_pos: u32) -> Result<(), ParserError> {
        let has_column = self.function_ref(func)?.has_column();
        self.parse_state_ref().reset_parse_state(source_pos, func);
        self.parse_state_ref().set_is_eval(false);
        self.parse_state_ref().set_is_repl(false);
        self.parse_state_ref().set_has_retval(false);
        self.parse_state_ref().set_hoisted_code_len(0);
        self.lexer.reset_to_pos(source_pos as usize);
        self.emitter = BytecodeEmitter::new(self.source, 0, has_column);
        self.break_stack = BreakStack::new();
        self.dropped_result = false;
        Ok(())
    }

    fn parse_function_decl(
        &mut self,
        func_type: JSParseFunction,
        mut func_name: JSValue,
    ) -> Result<(), ParserError> {
        let is_expr = func_type != JSParseFunction::Statement;
        let mut name_pos = self.lexer.token().source_pos();
        if matches!(func_type, JSParseFunction::Statement | JSParseFunction::Expr) {
            self.next_token()?;
            name_pos = self.lexer.token().source_pos();
            if self.lexer.token().val() != TOK_IDENT && !is_expr {
                return Err(self.parser_error(ERR_FUNCTION_NAME_EXPECTED));
            }
            if self.lexer.token().val() == TOK_IDENT {
                func_name = self.lexer.token().value();
                self.next_token()?;
            }
        }

        let source_pos = self.lexer.token().source_pos();
        let header = FunctionBytecodeHeader::new(
            false,
            false,
            self.parse_state_ref().has_column(),
            0,
            false,
        );
        let fields = FunctionBytecodeFields {
            func_name,
            byte_code: JSValue::JS_NULL,
            cpool: JSValue::JS_NULL,
            vars: JSValue::JS_NULL,
            ext_vars: JSValue::JS_NULL,
            stack_size: 0,
            ext_vars_len: 0,
            filename: JSValue::JS_NULL,
            pc2line: JSValue::JS_NULL,
            source_pos,
        };
        let mut func = OwnedFunctionBytecode::new(header, fields);

        parse_expect1(&self.lexer, TOK_PAREN_OPEN)?;
        self.lexer.skip_parens(None).map_err(ParserError::from)?;

        parse_expect1(&self.lexer, TOK_BRACE_OPEN)?;
        let skip_bits = self
            .lexer
            .skip_parens(if is_expr { Some(func_name) } else { None })
            .map_err(ParserError::from)?;
        func.as_mut()
            .set_has_arguments((skip_bits & SKIP_HAS_ARGUMENTS) != 0);
        func.as_mut()
            .set_has_local_func_name((skip_bits & SKIP_HAS_FUNC_NAME) != 0);

        let func_value = func.value();
        self.nested_funcs.push(func);

        let idx = self.cpool_add(func_value)? as u32;

        if is_expr {
            self.emitter.emit_op(OP_FCLOSURE);
            self.emitter.emit_u16(idx as u16);
            return Ok(());
        }

        let (var_kind, var_idx) = self
            .with_parse_state_and_alloc(|state, alloc| define_var(state, alloc, func_name))
            .map_err(|err| Self::var_error(err, name_pos as usize))?;
        self.parse_state_ref().add_hoisted_code_len(6);

        let mut hoist_idx = var_idx;
        if var_kind == JSVarRefKind::Var {
            let arg_count = self.current_func_ref()?.arg_count() as i32;
            hoist_idx = hoist_idx.saturating_add(arg_count);
        }
        let hoist_idx = u16::try_from(hoist_idx)
            .map_err(|_| self.parser_error(ERR_TOO_MANY_LOCAL_VARS))?;
        self.function_mut(func_value)?
            .set_arg_count(hoist_idx.saturating_add(1));
        Ok(())
    }

    fn parse_function_body(&mut self) -> Result<(), ParserError> {
        self.next_token()?;
        parse_expect(&mut self.lexer, TOK_PAREN_OPEN)?;
        while self.lexer.token().val() != TOK_PAREN_CLOSE {
            if self.lexer.token().val() != TOK_IDENT {
                return Err(self.parser_error(ERR_MISSING_FORMAL_PARAMETER));
            }
            let name = self.lexer.token().value();
            if value_matches_bytes(name, b"eval") || value_matches_bytes(name, b"arguments") {
                return Err(self.parser_error(ERR_INVALID_ARGUMENT_NAME));
            }
            if find_var(self.parse_state_ref(), name).is_some() {
                return Err(self.parser_error(ERR_DUPLICATE_ARGUMENT_NAME));
            }
            self.with_parse_state_and_alloc(|state, alloc| add_var(state, alloc, name))
                .map_err(|err| Self::var_error(err, self.lexer.token().source_pos() as usize))?;
            self.next_token()?;
            if self.lexer.token().val() == TOK_PAREN_CLOSE {
                break;
            }
            parse_expect(&mut self.lexer, ',' as i32)?;
        }

        let arg_count = self.parse_state_ref().local_vars_len();
        self.current_func_mut()?.set_arg_count(arg_count);

        self.next_token()?;
        parse_expect(&mut self.lexer, TOK_BRACE_OPEN)?;

        let arg_count = arg_count as i32;
        let source_pos = self.emitter.pc2line_source_pos();
        let (has_arguments, has_local_name, func_name) = {
            let func = self.current_func_ref()?;
            (
                func.has_arguments(),
                func.has_local_func_name(),
                func.func_name(),
            )
        };

        if has_arguments {
            let name = self
                .lexer
                .intern_identifier(b"arguments", 0)
                .map_err(ParserError::from)?;
            let var_idx = self
                .with_parse_state_and_alloc(|state, alloc| add_var(state, alloc, name))
                .map_err(|err| Self::var_error(err, source_pos as usize))?;
            self.emitter.emit_op(OP_ARGUMENTS);
            put_var(
                &mut self.emitter,
                JSVarRefKind::Var,
                i32::from(var_idx) - arg_count,
                source_pos,
            )
            .map_err(|err| Self::var_error(err, source_pos as usize))?;
        }

        if has_local_name {
            let var_idx = self
                .with_parse_state_and_alloc(|state, alloc| add_var(state, alloc, func_name))
                .map_err(|err| Self::var_error(err, source_pos as usize))?;
            self.emitter.emit_op(OP_THIS_FUNC);
            put_var(
                &mut self.emitter,
                JSVarRefKind::Var,
                i32::from(var_idx) - arg_count,
                source_pos,
            )
            .map_err(|err| Self::var_error(err, source_pos as usize))?;
        }

        while self.lexer.token().val() != TOK_BRACE_CLOSE {
            self.parse_source_element()?;
        }

        if self.emitter.is_live_code() {
            self.emitter.emit_op(OP_RETURN_UNDEF);
        }

        self.next_token()?;
        self.store_current_function(false)
    }

    fn store_current_function(&mut self, is_eval: bool) -> Result<(), ParserError> {
        let hoisted_len = self.parse_state_ref().hoisted_code_len();
        self.define_hoisted_functions(is_eval)?;

        let byte_code = self.emitter.byte_code().to_vec();
        let byte_code_val = self.alloc.alloc_byte_array(byte_code);
        let byte_code_len = self.emitter.byte_code().len() as u32;

        let pc2line = self.emitter.finalize_pc2line(hoisted_len);
        let pc2line_val = self.alloc.alloc_byte_array(pc2line);

        let func = self.function_mut(self.parse_state_ref().cur_func())?;
        func.set_byte_code(byte_code_val);
        func.set_pc2line(pc2line_val);
        self.parse_state_ref().set_byte_code_len(byte_code_len);
        Ok(())
    }

    fn define_hoisted_functions(&mut self, is_eval: bool) -> Result<(), ParserError> {
        let hoisted_len = self.parse_state_ref().hoisted_code_len() as usize;
        if hoisted_len == 0 {
            return Ok(());
        }

        let mut hoisted = Vec::with_capacity(hoisted_len);
        let func = self.current_func_ref()?;
        let arg_count = func.arg_count() as i32;
        let cpool_val = func.cpool();
        if cpool_val != JSValue::JS_NULL {
            let cpool = self.value_array_ref(cpool_val)?;
            let cpool_len = self.parse_state_ref().cpool_len() as usize;
            for (idx, value) in cpool.values().iter().take(cpool_len).enumerate() {
                let Some(ptr) = self.func_ptr(*value) else {
                    continue;
                };
                // SAFETY: pointer is owned by this parser.
                let func = unsafe { ptr.as_ref() };
                let marker = func.arg_count();
                if marker == 0 {
                    continue;
                }
                let mut var_idx = marker as i32 - 1;
                let opcode = if is_eval {
                    OP_PUT_VAR_REF_NOCHECK
                } else if var_idx < arg_count {
                    OP_PUT_ARG
                } else {
                    var_idx -= arg_count;
                    OP_PUT_LOC
                };
                hoisted.push(OP_FCLOSURE.as_u8());
                hoisted.extend_from_slice(&(idx as u16).to_le_bytes());
                hoisted.push(opcode.as_u8());
                hoisted.extend_from_slice(&(var_idx as u16).to_le_bytes());
            }
        }

        self.emitter.prepend_bytes(&hoisted);
        Ok(())
    }

    fn finalize_parsed_function(&mut self) -> Result<(), ParserError> {
        convert_ext_vars_to_local_vars(self.parse_state_ref())
            .map_err(|err| Self::var_error(err, 0))?;

        let func_val = self.parse_state_ref().cur_func();
        let (cpool_val, vars_val, byte_code_val) = {
            let func = self.function_ref(func_val)?;
            (func.cpool(), func.vars(), func.byte_code())
        };
        let cpool_len = self.parse_state_ref().cpool_len() as usize;
        let local_len = self.parse_state_ref().local_vars_len() as usize;
        let byte_code_len = self.parse_state_ref().byte_code_len() as usize;

        if cpool_val != JSValue::JS_NULL {
            let cpool = self.value_array_mut(cpool_val)?;
            cpool.shrink_to(cpool_len);
        }
        if vars_val != JSValue::JS_NULL {
            let vars = self.value_array_mut(vars_val)?;
            vars.shrink_to(local_len);
        }

        if byte_code_val != JSValue::JS_NULL {
            let byte_code = self.byte_array_mut(byte_code_val)?;
            byte_code.shrink_to(byte_code_len);
        }

        let byte_code = self.byte_array_ref(byte_code_val)?.buf();
        let stack_size = compute_stack_size(byte_code).map_err(Self::stack_size_error)?;
        self.function_mut(func_val)?.set_stack_size(stack_size);
        Ok(())
    }

    fn parse_local_functions(&mut self, top_func: JSValue) -> Result<(), ParserError> {
        #[derive(Clone, Copy)]
        struct Frame {
            func: JSValue,
            parent: JSValue,
            cpool_pos: usize,
        }

        let mut stack = vec![Frame {
            func: top_func,
            parent: JSValue::JS_NULL,
            cpool_pos: 0,
        }];

        while let Some(mut frame) = stack.pop() {
            if frame.cpool_pos == 0 {
                self.finalize_parsed_function()?;
            }

            let cpool_values = {
                let func = self.function_ref(frame.func)?;
                let cpool_val = func.cpool();
                if cpool_val == JSValue::JS_NULL {
                    Vec::new()
                } else {
                    self.value_array_ref(cpool_val)?.values().to_vec()
                }
            };
            let mut pos = frame.cpool_pos;
            let mut pushed_child = false;
            while pos < cpool_values.len() {
                let value = cpool_values[pos];
                if self.func_ptr(value).is_some() {
                    let child_func = value;
                    let source_pos = self.function_ref(child_func)?.source_pos();
                    self.reset_for_function(child_func, source_pos)?;
                    self.parse_function_body()?;

                    frame.cpool_pos = pos + 1;
                    stack.push(frame);
                    stack.push(Frame {
                        func: child_func,
                        parent: frame.func,
                        cpool_pos: 0,
                    });
                    pushed_child = true;
                    break;
                }
                pos += 1;
            }
            if pushed_child {
                continue;
            }

            if frame.parent != JSValue::JS_NULL {
                resolve_var_refs(&mut self.alloc, frame.func, frame.parent)
                    .map_err(|err| Self::var_error(err, 0))?;
            }

            let (ext_vars_val, ext_vars_len) = {
                let func = self.function_ref(frame.func)?;
                (func.ext_vars(), func.ext_vars_len())
            };
            if ext_vars_val != JSValue::JS_NULL {
                let ext_vars = self.value_array_mut(ext_vars_val)?;
                ext_vars.shrink_to(ext_vars_len as usize * 2);
            }
        }

        Ok(())
    }

    fn parse_var(&mut self, in_accepted: bool) -> Result<(), ParserError> {
        loop {
            let ident_source_pos = self.lexer.token().source_pos();
            if self.lexer.token().val() != TOK_IDENT {
                return Err(self.parser_error(ERR_VARIABLE_NAME_EXPECTED));
            }
            let name = self.lexer.token().value();
            if value_matches_bytes(name, b"arguments") {
                return Err(self.parser_error(ERR_INVALID_VARIABLE_NAME));
            }
            let (var_kind, var_idx) = self
                .with_parse_state_and_alloc(|state, alloc| define_var(state, alloc, name))
                .map_err(|err| Self::var_error(err, ident_source_pos as usize))?;
            self.next_token()?;
            if self.lexer.token().val() == '=' as i32 {
                self.next_token()?;
                let flags = if in_accepted { 0 } else { PF_NO_IN as i32 };
                self.parse_assign_expr2(flags)?;
                put_var(&mut self.emitter, var_kind, var_idx, ident_source_pos)
                    .map_err(|err| Self::var_error(err, ident_source_pos as usize))?;
            }
            if self.lexer.token().val() != ',' as i32 {
                break;
            }
            self.next_token()?;
        }
        Ok(())
    }

    fn set_eval_ret_undefined(&mut self) {
        let eval_ret_idx = self.parse_state_ref().eval_ret_idx();
        if eval_ret_idx < 0 {
            return;
        }
        self.emitter.emit_op(OP_UNDEFINED);
        let idx = eval_ret_idx as u32;
        let source_pos = self.emitter.pc2line_source_pos();
        self.emitter.emit_var(OP_PUT_LOC, idx, source_pos);
    }

    fn parse_block_func(
        &mut self,
        _stack: &mut ParseStack<'_>,
        state: u8,
        _param: i32,
    ) -> Result<i32, ParserError> {
        match state {
            PARSE_STATE_INIT => {
                parse_expect(&mut self.lexer, TOK_BRACE_OPEN)?;
                if self.lexer.token().val() == TOK_BRACE_CLOSE {
                    self.next_token()?;
                    return Ok(PARSE_STATE_RET as i32);
                }
                Ok(encode_call(0, ParseExprFunc::JsParseStatement, 0))
            }
            0 => {
                if self.lexer.token().val() == TOK_BRACE_CLOSE {
                    self.next_token()?;
                    return Ok(PARSE_STATE_RET as i32);
                }
                Ok(encode_call(0, ParseExprFunc::JsParseStatement, 0))
            }
            _ => unreachable!("unexpected parse state"),
        }
    }

    fn parse_switch_body(
        &mut self,
        stack: &mut ParseStack<'_>,
        label_case: &mut Label,
        default_label_pos: &mut i32,
    ) -> Result<i32, ParserError> {
        loop {
            match self.lexer.token().val() {
                TOK_CASE => {
                    let mut label1 = Label::none();
                    if !label_case.is_none() {
                        label1 = Label::new();
                        self.emitter.emit_goto(OP_GOTO, &mut label1);
                        self.emitter.emit_label(label_case);
                        *label_case = Label::none();
                    }
                    loop {
                        self.next_token()?;
                        self.emitter.emit_op(OP_DUP);
                        self.parse_expr()?;
                        parse_expect(&mut self.lexer, ':' as i32)?;
                        self.emitter.emit_op(OP_STRICT_EQ);
                        if self.lexer.token().val() == TOK_CASE {
                            if label1.is_none() {
                                label1 = Label::new();
                            }
                            self.emitter.emit_goto(OP_IF_TRUE, &mut label1);
                        } else {
                            *label_case = Label::new();
                            self.emitter.emit_goto(OP_IF_FALSE, label_case);
                            if !label1.is_none() {
                                self.emitter.emit_label(&mut label1);
                            }
                            break;
                        }
                    }
                }
                TOK_DEFAULT => {
                    self.next_token()?;
                    parse_expect(&mut self.lexer, ':' as i32)?;
                    if *default_label_pos >= 0 {
                        return Err(self.parser_error(ERR_DUPLICATE_DEFAULT));
                    }
                    if label_case.is_none() {
                        *label_case = Label::new();
                        self.emitter.emit_goto(OP_GOTO, label_case);
                    }
                    *default_label_pos = self.emitter.byte_code().len() as i32;
                }
                TOK_BRACE_CLOSE => {
                    parse_expect(&mut self.lexer, TOK_BRACE_CLOSE)?;
                    if *default_label_pos >= 0 {
                        let pos = usize::try_from(*default_label_pos)
                            .unwrap_or_default();
                        self.emitter.emit_label_pos(label_case, pos);
                    } else if !label_case.is_none() {
                        self.emitter.emit_label(label_case);
                    }
                    let entry = self
                        .break_stack
                        .top_mut()
                        .expect("switch break entry");
                    self.emitter.emit_label(entry.label_break_mut());
                    self.emitter.emit_op(OP_DROP);
                    self.break_stack.pop();
                    return Ok(PARSE_STATE_RET as i32);
                }
                _ => {
                    if label_case.is_none() {
                        return Err(self.parser_error(ERR_INVALID_SWITCH));
                    }
                    stack.push_int(label_case.raw()).map_err(Self::stack_error)?;
                    stack
                        .push_int(*default_label_pos)
                        .map_err(Self::stack_error)?;
                    return Ok(encode_call(
                        7,
                        ParseExprFunc::JsParseStatement,
                        0,
                    ));
                }
            }
        }
    }

    fn parse_statement_func(
        &mut self,
        stack: &mut ParseStack<'_>,
        state: u8,
        _param: i32,
    ) -> Result<i32, ParserError> {
        match state {
            PARSE_STATE_INIT => {
                let mut label_name = JSValue::JS_NULL;
                if self.lexer.is_label() {
                    label_name = self.lexer.token().value();
                    self.next_token()?;
                    parse_expect(&mut self.lexer, ':' as i32)?;
                    for entry in self.break_stack.iter().rev() {
                        if entry.label_name() == label_name {
                            return Err(self.parser_error(ERR_DUPLICATE_LABEL));
                        }
                    }
                    let tok = self.lexer.token().val();
                    if tok != TOK_FOR && tok != TOK_DO && tok != TOK_WHILE {
                        self.break_stack
                            .push(label_name, Label::new(), Label::none(), 0);
                        return Ok(encode_call(
                            11,
                            ParseExprFunc::JsParseStatement,
                            0,
                        ));
                    }
                }

                match self.lexer.token().val() {
                    TOK_BRACE_OPEN => Ok(encode_call(0, ParseExprFunc::JsParseBlock, 0)),
                    TOK_RETURN => {
                        if self.parse_state_ref().is_eval() {
                            return Err(self.parser_error(ERR_RETURN_NOT_IN_FUNCTION));
                        }
                        let op_source_pos = self.lexer.token().source_pos();
                        self.next_token()?;
                        let has_val = if self.lexer.token().val() != ';' as i32
                            && self.lexer.token().val() != '}' as i32
                            && !self.lexer.got_lf()
                        {
                            self.parse_expr()?;
                            true
                        } else {
                            false
                        };
                        emit_return(
                            &mut self.emitter,
                            &mut self.break_stack,
                            has_val,
                            op_source_pos,
                        );
                        parse_expect_semi(&mut self.lexer)?;
                        Ok(PARSE_STATE_RET as i32)
                    }
                    TOK_THROW => {
                        let op_source_pos = self.lexer.token().source_pos();
                        self.next_token()?;
                        if self.lexer.got_lf() {
                            return Err(self.parser_error(ERR_LINE_TERMINATOR_AFTER_THROW));
                        }
                        self.parse_expr()?;
                        self.emitter.emit_op_pos(OP_THROW, op_source_pos);
                        parse_expect_semi(&mut self.lexer)?;
                        Ok(PARSE_STATE_RET as i32)
                    }
                    TOK_VAR => {
                        self.next_token()?;
                        self.parse_var(true)?;
                        parse_expect_semi(&mut self.lexer)?;
                        Ok(PARSE_STATE_RET as i32)
                    }
                    TOK_IF => {
                        self.next_token()?;
                        self.set_eval_ret_undefined();
                        self.parse_expr_paren()?;
                        let mut label1 = Label::new();
                        self.emitter.emit_goto(OP_IF_FALSE, &mut label1);
                        stack.push_int(label1.raw()).map_err(Self::stack_error)?;
                        Ok(encode_call(1, ParseExprFunc::JsParseStatement, 0))
                    }
                    TOK_WHILE => {
                        let idx = self
                            .break_stack
                            .push(label_name, Label::new(), Label::new(), 0);
                        self.next_token()?;
                        self.set_eval_ret_undefined();
                        {
                            let entry = self.break_stack.entry_mut(idx).expect("break entry");
                            self.emitter.emit_label(entry.label_cont_mut());
                        }
                        self.parse_expr_paren()?;
                        {
                            let entry = self.break_stack.entry_mut(idx).expect("break entry");
                            self.emitter.emit_goto(OP_IF_FALSE, entry.label_break_mut());
                        }
                        Ok(encode_call(3, ParseExprFunc::JsParseStatement, 0))
                    }
                    TOK_DO => {
                        let _idx = self
                            .break_stack
                            .push(label_name, Label::new(), Label::new(), 0);
                        let mut label1 = Label::new();
                        self.next_token()?;
                        self.set_eval_ret_undefined();
                        self.emitter.emit_label(&mut label1);
                        stack.push_int(label1.raw()).map_err(Self::stack_error)?;
                        Ok(encode_call(4, ParseExprFunc::JsParseStatement, 0))
                    }
                    TOK_FOR => {
                        let idx = self
                            .break_stack
                            .push(label_name, Label::new(), Label::new(), 0);
                        self.next_token()?;
                        self.set_eval_ret_undefined();
                        parse_expect1(&self.lexer, '(' as i32)?;
                        let bits = self.lexer.skip_parens_token().map_err(ParserError::from)?;
                        self.next_token()?;
                        if (bits & SKIP_HAS_SEMI) == 0 {
                            let mut label_expr = Label::new();
                            let mut label_body = Label::new();
                            let mut label_next = Label::new();
                            if let Some(entry) = self.break_stack.entry_mut(idx) {
                                entry.set_drop_count(1);
                            }
                            self.emitter.emit_goto(OP_GOTO, &mut label_expr);
                            self.emitter.emit_label(&mut label_next);
                            if self.lexer.token().val() == TOK_VAR {
                                self.next_token()?;
                                if self.lexer.token().val() != TOK_IDENT {
                                    return Err(self.parser_error(ERR_VARIABLE_NAME_EXPECTED));
                                }
                                let name = self.lexer.token().value();
                                let ident_source_pos = self.lexer.token().source_pos();
                                let (var_kind, var_idx) = self
                                    .with_parse_state_and_alloc(|state, alloc| {
                                        define_var(state, alloc, name)
                                    })
                                    .map_err(|err| {
                                        Self::var_error(err, ident_source_pos as usize)
                                    })?;
                                let source_pos = self.emitter.pc2line_source_pos();
                                put_var(
                                    &mut self.emitter,
                                    var_kind,
                                    var_idx,
                                    source_pos,
                                )
                                .map_err(|err| {
                                    Self::var_error(err, ident_source_pos as usize)
                                })?;
                                self.next_token()?;
                            } else {
                                self.parse_assign_expr2(PF_NO_IN as i32)?;
                                let lvalue = get_lvalue(&mut self.emitter, false)
                                    .map_err(Self::lvalue_error)?;
                                let is_repl = self.parse_state_ref().is_repl();
                                let length_idx = self.length_atom_index()?;
                                put_lvalue(
                                    &mut self.emitter,
                                    lvalue,
                                    PutLValue::NoKeepBottom,
                                    is_repl,
                                    length_idx,
                                )
                                .map_err(Self::lvalue_error)?;
                            }
                            self.emitter.emit_goto(OP_GOTO, &mut label_body);
                            let opcode = if self.lexer.token().val() == TOK_IN {
                                OP_FOR_IN_START
                            } else if self.lexer.token().val() == TOK_IDENT
                                && value_matches_bytes(self.lexer.token().value(), b"of")
                            {
                                OP_FOR_OF_START
                            } else {
                                return Err(self.parser_error(ERR_EXPECTED_OF_OR_IN));
                            };
                            self.next_token()?;
                            self.emitter.emit_label(&mut label_expr);
                            self.parse_expr()?;
                            self.emitter.emit_op(opcode);
                            {
                                let entry = self.break_stack.entry_mut(idx).expect("break entry");
                                self.emitter.emit_goto(OP_GOTO, entry.label_cont_mut());
                            }
                            parse_expect(&mut self.lexer, ')' as i32)?;
                            self.emitter.emit_label(&mut label_body);
                            stack.push_int(label_next.raw()).map_err(Self::stack_error)?;
                            Ok(encode_call(5, ParseExprFunc::JsParseStatement, 0))
                        } else {
                            if self.lexer.token().val() != ';' as i32 {
                                if self.lexer.token().val() == TOK_VAR {
                                    self.next_token()?;
                                    self.parse_var(false)?;
                                } else {
                                    self.parse_expr2((PF_NO_IN | PF_DROP) as i32)?;
                                }
                            }
                            parse_expect(&mut self.lexer, ';' as i32)?;

                            let mut label_test = Label::new();
                            self.emitter.emit_label(&mut label_test);
                            if self.lexer.token().val() != ';' as i32 {
                                self.parse_expr()?;
                                let entry = self.break_stack.entry_mut(idx).expect("break entry");
                                self.emitter.emit_goto(OP_IF_FALSE, entry.label_break_mut());
                            }
                            parse_expect(&mut self.lexer, ';' as i32)?;

                            let (expr3_source_pos, expr3_flags) =
                                if self.lexer.token().val() != ')' as i32 {
                                    let pos = self.lexer.get_pos();
                                    self.skip_expr()?;
                                    (
                                        pos.source_pos() as i32,
                                        (pos.got_lf() as i32)
                                            | ((pos.regexp_allowed() as i32) << 1),
                                    )
                                } else {
                                    (-1, 0)
                                };
                            parse_expect(&mut self.lexer, ')' as i32)?;

                            stack.push_int(label_test.raw()).map_err(Self::stack_error)?;
                            stack.push_int(expr3_flags).map_err(Self::stack_error)?;
                            stack.push_int(expr3_source_pos).map_err(Self::stack_error)?;
                            Ok(encode_call(6, ParseExprFunc::JsParseStatement, 0))
                        }
                    }
                    TOK_BREAK | TOK_CONTINUE => {
                        let is_cont = self.lexer.token().val() == TOK_CONTINUE;
                        let source_pos = self.lexer.token().source_pos();
                        self.next_token()?;
                        let label_name = if !self.lexer.got_lf()
                            && self.lexer.token().val() == TOK_IDENT
                        {
                            self.lexer.token().value()
                        } else {
                            JSValue::JS_NULL
                        };
                        emit_break(
                            &mut self.emitter,
                            &mut self.break_stack,
                            label_name,
                            is_cont,
                            source_pos,
                        )
                        .map_err(Self::control_flow_error)?;
                        if label_name != JSValue::JS_NULL {
                            self.next_token()?;
                        }
                        parse_expect_semi(&mut self.lexer)?;
                        Ok(PARSE_STATE_RET as i32)
                    }
                    TOK_SWITCH => {
                        self.next_token()?;
                        self.set_eval_ret_undefined();
                        self.parse_expr_paren()?;
                        parse_expect(&mut self.lexer, '{' as i32)?;
                        self.break_stack
                            .push(label_name, Label::new(), Label::none(), 1);
                        let mut label_case = Label::none();
                        let mut default_label_pos = -1;
                        self.parse_switch_body(stack, &mut label_case, &mut default_label_pos)
                    }
                    TOK_TRY => {
                        self.set_eval_ret_undefined();
                        self.next_token()?;
                        let mut label_catch = Label::new();
                        let label_finally = Label::new();
                        self.emitter.emit_goto(OP_CATCH, &mut label_catch);

                        let idx = self
                            .break_stack
                            .push(JSValue::JS_NULL, Label::none(), Label::none(), 1);
                        if let Some(entry) = self.break_stack.entry_mut(idx) {
                            *entry.label_finally_mut() = label_finally;
                        }
                        stack.push_int(label_catch.raw()).map_err(Self::stack_error)?;
                        Ok(encode_call(8, ParseExprFunc::JsParseBlock, 0))
                    }
                    TOK_SEMI => {
                        self.next_token()?;
                        Ok(PARSE_STATE_RET as i32)
                    }
                    _ => {
                        let eval_ret_idx = self.parse_state_ref().eval_ret_idx();
                        if eval_ret_idx >= 0 {
                            self.parse_expr()?;
                            let idx = eval_ret_idx as u32;
                            let source_pos = self.emitter.pc2line_source_pos();
                            self.emitter.emit_var(OP_PUT_LOC, idx, source_pos);
                        } else {
                            self.parse_expr2(PF_DROP as i32)?;
                        }
                        parse_expect_semi(&mut self.lexer)?;
                        Ok(PARSE_STATE_RET as i32)
                    }
                }
            }
            0 => Ok(PARSE_STATE_RET as i32),
            1 => {
                let mut label1 = Label::from_raw(stack.pop_int());
                if self.lexer.token().val() == TOK_ELSE {
                    self.next_token()?;
                    let mut label2 = Label::new();
                    self.emitter.emit_goto(OP_GOTO, &mut label2);
                    self.emitter.emit_label(&mut label1);
                    stack.push_int(label2.raw()).map_err(Self::stack_error)?;
                    return Ok(encode_call(2, ParseExprFunc::JsParseStatement, 0));
                }
                self.emitter.emit_label(&mut label1);
                Ok(PARSE_STATE_RET as i32)
            }
            2 => {
                let mut label2 = Label::from_raw(stack.pop_int());
                self.emitter.emit_label(&mut label2);
                Ok(PARSE_STATE_RET as i32)
            }
            3 => {
                let entry = self.break_stack.top_mut().expect("break entry");
                self.emitter.emit_goto(OP_GOTO, entry.label_cont_mut());
                self.emitter.emit_label(entry.label_break_mut());
                self.break_stack.pop();
                Ok(PARSE_STATE_RET as i32)
            }
            4 => {
                let mut label1 = Label::from_raw(stack.pop_int());
                {
                    let entry = self.break_stack.top_mut().expect("break entry");
                    self.emitter.emit_label(entry.label_cont_mut());
                }
                parse_expect(&mut self.lexer, TOK_WHILE)?;
                self.parse_expr_paren()?;
                if self.lexer.token().val() == ';' as i32 {
                    self.next_token()?;
                }
                self.emitter.emit_goto(OP_IF_TRUE, &mut label1);
                {
                    let entry = self.break_stack.top_mut().expect("break entry");
                    self.emitter.emit_label(entry.label_break_mut());
                }
                self.break_stack.pop();
                Ok(PARSE_STATE_RET as i32)
            }
            5 => {
                let mut label_next = Label::from_raw(stack.pop_int());
                let entry = self.break_stack.top_mut().expect("break entry");
                self.emitter.emit_label(entry.label_cont_mut());
                self.emitter.emit_op(OP_FOR_OF_NEXT);
                self.emitter.emit_goto(OP_IF_FALSE, &mut label_next);
                self.emitter.emit_op(OP_DROP);
                self.emitter.emit_label(entry.label_break_mut());
                self.emitter.emit_op(OP_DROP);
                self.break_stack.pop();
                Ok(PARSE_STATE_RET as i32)
            }
            6 => {
                let expr3_source_pos = stack.pop_int();
                let expr3_flags = stack.pop_int();
                let mut label_test = Label::from_raw(stack.pop_int());
                {
                    let entry = self.break_stack.top_mut().expect("break entry");
                    self.emitter.emit_label(entry.label_cont_mut());
                }
                if expr3_source_pos != -1 {
                    let end_pos = self.lexer.get_pos();
                    let expr3_pos = ParsePos::new(
                        (expr3_flags & 1) != 0,
                        (expr3_flags >> 1) != 0,
                        expr3_source_pos as u32,
                    );
                    self.lexer.seek_token(expr3_pos).map_err(ParserError::from)?;
                    self.parse_expr2(PF_DROP as i32)?;
                    self.lexer.seek_token(end_pos).map_err(ParserError::from)?;
                }
                self.emitter.emit_goto(OP_GOTO, &mut label_test);
                {
                    let entry = self.break_stack.top_mut().expect("break entry");
                    self.emitter.emit_label(entry.label_break_mut());
                }
                self.break_stack.pop();
                Ok(PARSE_STATE_RET as i32)
            }
            7 => {
                let mut default_label_pos = stack.pop_int();
                let mut label_case = Label::from_raw(stack.pop_int());
                self.parse_switch_body(stack, &mut label_case, &mut default_label_pos)
            }
            8 => {
                let mut label_catch = Label::from_raw(stack.pop_int());
                let label_finally = {
                    let entry = self.break_stack.top_mut().expect("try break entry");
                    let label = entry.label_finally();
                    self.break_stack.pop();
                    label
                };
                let mut label_finally = label_finally;

                self.emitter.emit_op(OP_DROP);
                self.emitter.emit_op(OP_UNDEFINED);
                self.emitter.emit_goto(OP_GOSUB, &mut label_finally);
                self.emitter.emit_op(OP_DROP);

                let mut label_end = Label::new();
                self.emitter.emit_goto(OP_GOTO, &mut label_end);

                if self.lexer.token().val() == TOK_CATCH {
                    let mut label_catch2 = Label::new();
                    self.next_token()?;
                    parse_expect(&mut self.lexer, '(' as i32)?;
                    if self.lexer.token().val() != TOK_IDENT {
                        return Err(self.parser_error(ERR_IDENTIFIER_EXPECTED));
                    }
                    let name = self.lexer.token().value();
                    let ident_source_pos = self.lexer.token().source_pos();
                    if find_var(self.parse_state_ref(), name).is_some()
                        || find_ext_var(self.parse_state_ref(), name).is_some()
                    {
                        return Err(self.parser_error(ERR_CATCH_VAR_EXISTS));
                    }
                    let var_idx = self
                        .with_parse_state_and_alloc(|state, alloc| add_var(state, alloc, name))
                        .map_err(|err| Self::var_error(err, ident_source_pos as usize))?;
                    self.next_token()?;
                    parse_expect(&mut self.lexer, ')' as i32)?;

                    self.emitter.emit_label(&mut label_catch);
                    let arg_count = self.func.as_ref().arg_count() as i32;
                    let idx = i32::from(var_idx) - arg_count;
                    self.emitter
                        .emit_var(OP_PUT_LOC, idx as u32, self.emitter.pc2line_source_pos());

                    self.emitter.emit_goto(OP_CATCH, &mut label_catch2);

                    let idx = self
                        .break_stack
                        .push(JSValue::JS_NULL, Label::none(), Label::none(), 1);
                    if let Some(entry) = self.break_stack.entry_mut(idx) {
                        *entry.label_finally_mut() = label_finally;
                    }

                    stack.push_int(label_end.raw()).map_err(Self::stack_error)?;
                    stack
                        .push_int(label_catch2.raw())
                        .map_err(Self::stack_error)?;
                    return Ok(encode_call(9, ParseExprFunc::JsParseBlock, 0));
                } else if self.lexer.token().val() == TOK_FINALLY {
                    self.emitter.emit_label(&mut label_catch);
                    self.emitter.emit_goto(OP_GOSUB, &mut label_finally);
                    self.emitter.emit_op(OP_THROW);
                } else {
                    return Err(self.parser_error(ERR_EXPECTING_CATCH_OR_FINALLY));
                }

                self.emitter.emit_label(&mut label_finally);
                if self.lexer.token().val() == TOK_FINALLY {
                    self.next_token()?;
                    self.break_stack
                        .push(JSValue::JS_NULL, Label::none(), Label::none(), 2);
                    stack.push_int(label_end.raw()).map_err(Self::stack_error)?;
                    return Ok(encode_call(10, ParseExprFunc::JsParseBlock, 0));
                }
                self.emitter.emit_op(OP_RET);
                self.emitter.emit_label(&mut label_end);
                Ok(PARSE_STATE_RET as i32)
            }
            9 => {
                let mut label_catch2 = Label::from_raw(stack.pop_int());
                let mut label_end = Label::from_raw(stack.pop_int());

                let label_finally = {
                    let entry = self.break_stack.top_mut().expect("catch break entry");
                    let label = entry.label_finally();
                    self.break_stack.pop();
                    label
                };
                let mut label_finally = label_finally;

                self.emitter.emit_op(OP_DROP);
                self.emitter.emit_op(OP_UNDEFINED);
                self.emitter.emit_goto(OP_GOSUB, &mut label_finally);
                self.emitter.emit_op(OP_DROP);
                self.emitter.emit_goto(OP_GOTO, &mut label_end);

                self.emitter.emit_label(&mut label_catch2);
                self.emitter.emit_goto(OP_GOSUB, &mut label_finally);
                self.emitter.emit_op(OP_THROW);

                self.emitter.emit_label(&mut label_finally);
                if self.lexer.token().val() == TOK_FINALLY {
                    self.next_token()?;
                    self.break_stack
                        .push(JSValue::JS_NULL, Label::none(), Label::none(), 2);
                    stack.push_int(label_end.raw()).map_err(Self::stack_error)?;
                    return Ok(encode_call(10, ParseExprFunc::JsParseBlock, 0));
                }
                self.emitter.emit_op(OP_RET);
                self.emitter.emit_label(&mut label_end);
                Ok(PARSE_STATE_RET as i32)
            }
            10 => {
                let mut label_end = Label::from_raw(stack.pop_int());
                self.break_stack.pop();
                self.emitter.emit_op(OP_RET);
                self.emitter.emit_label(&mut label_end);
                Ok(PARSE_STATE_RET as i32)
            }
            11 => {
                let entry = self.break_stack.top_mut().expect("label break entry");
                self.emitter.emit_label(entry.label_break_mut());
                self.break_stack.pop();
                Ok(PARSE_STATE_RET as i32)
            }
            _ => unreachable!("unexpected parse state"),
        }
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
            ParseExprFunc::JsParseStatement => self.parse_statement_func(stack, state, param),
            ParseExprFunc::JsParseBlock => self.parse_block_func(stack, state, param),
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
            7 => ParseExprFunc::JsParseStatement,
            8 => ParseExprFunc::JsParseBlock,
            9 => ParseExprFunc::JsParseJsonValue,
            10 => ParseExprFunc::ReParseAlternative,
            _ => ParseExprFunc::ReParseDisjunction,
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

    fn control_flow_error(err: ControlFlowError) -> ParserError {
        ParserError::new(ParserErrorKind::Static(err.message()), err.position())
    }

    fn stack_size_error(err: StackSizeError) -> ParserError {
        let message = match err.kind() {
            crate::parser::stack_size::StackSizeErrorKind::InvalidOpcode => "invalid opcode",
            crate::parser::stack_size::StackSizeErrorKind::BufferOverflow => {
                "bytecode buffer overflow"
            }
            crate::parser::stack_size::StackSizeErrorKind::StackUnderflow => "stack underflow",
            crate::parser::stack_size::StackSizeErrorKind::StackOverflow => "stack overflow",
            crate::parser::stack_size::StackSizeErrorKind::InconsistentStackSize { .. } => {
                "unconsistent stack size"
            }
        };
        ParserError::new(ParserErrorKind::Static(message), err.pc() as usize)
    }

    fn maybe_drop_result(&self, parse_flags: i32) -> bool {
        if !has_flag(parse_flags, PF_DROP) {
            return false;
        }
        let tok = self.lexer.token().val();
        tok == ';' as i32 || tok == ')' as i32 || tok == ',' as i32
    }

    fn cpool_add(&mut self, val: JSValue) -> Result<u16, ParserError> {
        let len = self.parse_state_ref().cpool_len() as usize;
        let mut cpool_val = self.func.as_ref().cpool();
        if cpool_val != JSValue::JS_NULL {
            let ptr = cpool_val.to_ptr::<ValueArray>().ok_or_else(|| {
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
        if cpool_val == JSValue::JS_NULL {
            cpool_val = self.alloc.alloc_value_array(new_size);
            self.func.as_mut().set_cpool(cpool_val);
        } else {
            let mut ptr = cpool_val.to_ptr::<ValueArray>().ok_or_else(|| {
                ParserError::new(ParserErrorKind::Static(ERR_NO_MEM), 0)
            })?;
            // SAFETY: cpool array is owned by the allocator and mutated here.
            let arr = unsafe { ptr.as_mut() };
            arr.ensure_size(new_size);
        }

        let mut ptr = cpool_val.to_ptr::<ValueArray>().ok_or_else(|| {
            ParserError::new(ParserErrorKind::Static(ERR_NO_MEM), 0)
        })?;
        // SAFETY: cpool array is owned by the allocator and mutated here.
        let arr = unsafe { ptr.as_mut() };
        arr.values_mut()[len] = val;
        self.parse_state_ref().set_cpool_len((len + 1) as u16);
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
        if val.is_ptr() || is_short_float_value(val) {
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
            && value >= JSValue::JS_SHORTINT_MIN as f64
            && value <= JSValue::JS_SHORTINT_MAX as f64
            && value == (value as i32) as f64
        {
            self.emitter.emit_push_short_int(value as i32);
            return Ok(());
        }
        let js_val = self.lexer.ctx_mut().new_float64(value).map_err(|_| {
            ParserError::new(
                ParserErrorKind::Static(ERR_NO_MEM),
                self.lexer.token().source_pos() as usize,
            )
        })?;
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
                opcode = OpCode::from_u16(stack.pop_int() as u16);
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
                            self.parse_function_decl(JSParseFunction::Expr, JSValue::JS_NULL)?;
                            phase = Phase::Suffix;
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
                            let var_idx = find_var(self.parse_state_ref(), name);
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
                                let var_idx = find_ext_var(self.parse_state_ref(), name);
                                let var_idx = if let Some(var_idx) = var_idx {
                                    i32::from(var_idx)
                                } else {
                                    let decl =
                                        (JSVarRefKind::Global as i32) << 16;
                                    i32::from(
                                        self.with_parse_state_and_alloc(|state, alloc| {
                                            add_ext_var(state, alloc, name, decl)
                                        })
                                        .map_err(|err| {
                                            Self::var_error(err, token.source_pos() as usize)
                                        })?,
                                    )
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
                    if prop_type == ParseProp::Field
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

                    if prop_type == ParseProp::Field {
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
                    self.parse_function_decl(JSParseFunction::Method, name)?;
                    let opcode = match prop_type {
                        ParseProp::Method => OP_DEFINE_FIELD,
                        ParseProp::Get => OP_DEFINE_GETTER,
                        ParseProp::Set => OP_DEFINE_SETTER,
                        ParseProp::Field => unreachable!("handled above"),
                    };
                    self.emitter.emit_op(opcode);
                    self.emitter.emit_u16(prop_idx as u16);
                    if self.lexer.token().val() == ',' as i32 {
                        self.next_token()?;
                        phase = Phase::ObjectLoop;
                    } else {
                        self.expect('}' as i32)?;
                        phase = Phase::Suffix;
                    }
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
                    if idx >= JSValue::JS_SHORTINT_MAX as u32 {
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
                    stack.push_int(opcode.as_u16() as i32)
                        .map_err(Self::stack_error)?;
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
                                        self.emitter.byte_code_mut()[pos] = OP_GET_FIELD2.as_u8();
                                    }
                                    opcode = OP_GET_FIELD;
                                }
                                OP_GET_LENGTH => {
                                    if let Some(pos) = self.emitter.last_opcode_pos() {
                                        self.emitter.byte_code_mut()[pos] = OP_GET_LENGTH2.as_u8();
                                    }
                                    opcode = OP_GET_LENGTH;
                                }
                                OP_GET_ARRAY_EL => {
                                    if let Some(pos) = self.emitter.last_opcode_pos() {
                                        self.emitter.byte_code_mut()[pos] = OP_GET_ARRAY_EL2.as_u8();
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
                                            get_ext_var_name(self.parse_state_ref(), var_idx)
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
                        stack.push_int(opcode.as_u16() as i32)
                            .map_err(Self::stack_error)?;
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
                        let is_repl = self.parse_state_ref().is_repl();
                        if self.maybe_drop_result(parse_flags) {
                            self.dropped_result = true;
                            let op = OpCode::from_u16(OP_DEC.as_u16() + (op - TOK_DEC) as u16);
                            self.emitter.emit_op_pos(op, op_source_pos);
                            let length = self.length_atom_index()?;
                            put_lvalue(
                                &mut self.emitter,
                                lvalue,
                                PutLValue::NoKeepTop,
                                is_repl,
                                length,
                            )
                            .map_err(Self::lvalue_error)?;
                        } else {
                            let op =
                                OpCode::from_u16(OP_POST_DEC.as_u16() + (op - TOK_DEC) as u16);
                            self.emitter.emit_op_pos(op, op_source_pos);
                            let length = self.length_atom_index()?;
                            put_lvalue(
                                &mut self.emitter,
                                lvalue,
                                PutLValue::KeepSecond,
                                is_repl,
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
                let op_code = OpCode::from_u16(OP_DEC.as_u16() + (op - TOK_DEC) as u16);
                self.emitter.emit_op_pos(op_code, op_source_pos);
                let special = if self.maybe_drop_result(parse_flags) {
                    self.dropped_result = true;
                    PutLValue::NoKeepTop
                } else {
                    PutLValue::KeepTop
                };
                let is_repl = self.parse_state_ref().is_repl();
                let length = self.length_atom_index()?;
                put_lvalue(
                    &mut self.emitter,
                    lvalue,
                    special,
                    is_repl,
                    length,
                )
                .map_err(Self::lvalue_error)?;
                Ok(PARSE_STATE_RET as i32)
            }
            Phase::AfterTypeof => {
                if self.emitter.get_prev_opcode() == OP_GET_VAR_REF
                    && let Some(pos) = self.emitter.last_opcode_pos()
                {
                    self.emitter.byte_code_mut()[pos] = OP_GET_VAR_REF_NOCHECK.as_u8();
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
                opcode = OpCode::from_u16(stack.pop_int() as u16);
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
                    stack.push_int(opcode.as_u16() as i32)
                        .map_err(Self::stack_error)?;
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
                opcode = OpCode::from_u16(stack.pop_int() as u16);
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
                    stack.push_int(opcode.as_u16() as i32)
                        .map_err(Self::stack_error)?;
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
                let is_repl = self.parse_state_ref().is_repl();
                let length = self.length_atom_index()?;
                let lvalue = crate::parser::lvalue::LValue::new(opcode, var_idx, source_pos);
                put_lvalue(
                    &mut self.emitter,
                    lvalue,
                    special,
                    is_repl,
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

impl Drop for ExprParser<'_, '_> {
    fn drop(&mut self) {
        if self.parse_state_attached {
            let prev = self.prev_parse_state;
            let _ = self.lexer.ctx_mut().swap_parse_state(prev);
        }
        unsafe {
            // SAFETY: parse_state_ptr was created from Box::into_raw in ExprParser::new.
            drop(Box::from_raw(self.parse_state_ptr.as_ptr()));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::{ContextConfig, JSContext};
    use crate::opcode::{
        OP_ADD, OP_ARGUMENTS, OP_ARRAY_FROM, OP_CALL, OP_DEFINE_GETTER, OP_DROP, OP_DUP, OP_EQ,
        OP_FCLOSURE, OP_GET_LOC0, OP_GET_LOC1, OP_GET_VAR_REF, OP_GOTO, OP_IF_FALSE, OP_IF_TRUE,
        OP_INC, OP_LT, OP_MUL, OP_PUT_LOC, OP_PUT_LOC0, OP_PUT_LOC1, OP_PUT_VAR_REF,
        OP_PUT_VAR_REF_NOCHECK, OP_PUSH_0, OP_PUSH_1, OP_PUSH_2, OP_PUSH_3, OP_PUSH_FALSE,
        OP_PUSH_TRUE, OP_RETURN, OP_RETURN_UNDEF, OP_STRICT_EQ, OP_THIS_FUNC, OPCODES,
    };
    use crate::stdlib::MQUICKJS_STDLIB_IMAGE;

    fn decode_ops(bytes: &[u8]) -> Vec<OpCode> {
        let mut ops = Vec::new();
        let mut pos = 0usize;
        while pos < bytes.len() {
            let op = OpCode::from_u16(bytes[pos] as u16);
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
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let mut parser = ExprParser::new(&mut ctx, input.as_bytes());
        parser.attach_parse_state();
        parser.parse_expression(0).expect("parse");
        decode_ops(parser.bytecode())
    }

    fn parse_statement_ops(input: &str) -> Vec<OpCode> {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let mut parser = ExprParser::new(&mut ctx, input.as_bytes());
        parser.attach_parse_state();
        parser.parse_statement().expect("parse");
        decode_ops(parser.bytecode())
    }

    fn parse_program_ops(parser: &ExprParser<'_, '_>) -> Vec<OpCode> {
        let root = parser
            .function_ref(parser.func.value())
            .expect("root func");
        let byte_code = parser
            .byte_array_ref(root.byte_code())
            .expect("bytecode")
            .buf();
        decode_ops(byte_code)
    }

    fn function_ops(parser: &ExprParser<'_, '_>, func: &FunctionBytecode) -> Vec<OpCode> {
        let byte_code = parser
            .byte_array_ref(func.byte_code())
            .expect("func bytecode")
            .buf();
        decode_ops(byte_code)
    }

    fn find_function_by_name<'a>(
        parser: &'a ExprParser<'_, '_>,
        root: &FunctionBytecode,
        name: &[u8],
    ) -> Option<&'a FunctionBytecode> {
        let cpool_val = root.cpool();
        if cpool_val != JSValue::JS_NULL {
            let cpool = parser.value_array_ref(cpool_val).ok()?;
            for &entry in cpool.values() {
                let Some(ptr) = parser.func_ptr(entry) else {
                    continue;
                };
                // SAFETY: parser owns the function allocations referenced by cpool entries.
                let func = unsafe { ptr.as_ref() };
                if value_matches_bytes(func.func_name(), name) {
                    return Some(func);
                }
            }
        }

        parser
            .nested_funcs
            .iter()
            .map(|func| func.as_ref())
            .find(|func| value_matches_bytes(func.func_name(), name))
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
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let mut parser = ExprParser::new(&mut ctx, b"x=1");
        parser.attach_parse_state();
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

    #[test]
    fn stmt_var_initializer_emits_put_loc() {
        let ops = parse_statement_ops("var x=1;");
        assert_eq!(ops, [OP_PUSH_1, OP_PUT_LOC0]);
    }

    #[test]
    fn stmt_if_else_emits_jumps() {
        let ops = parse_statement_ops("if(true) 1; else 2;");
        assert_eq!(
            ops,
            [OP_PUSH_TRUE, OP_IF_FALSE, OP_PUSH_1, OP_DROP, OP_GOTO, OP_PUSH_2, OP_DROP]
        );
    }

    #[test]
    fn stmt_return_with_value_emits_return() {
        let ops = parse_statement_ops("return 1;");
        assert_eq!(ops, [OP_PUSH_1, OP_RETURN]);
    }

    #[test]
    fn program_function_decl_hoists() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let mut parser = ExprParser::new(&mut ctx, b"function foo(){}");
        parser.attach_parse_state();
        parser.parse_program().expect("parse");

        let ops = parse_program_ops(&parser);
        assert_eq!(ops, [OP_FCLOSURE, OP_PUT_LOC, OP_RETURN_UNDEF]);

        let root = parser
            .function_ref(parser.func.value())
            .expect("root func");
        let cpool = parser.value_array_ref(root.cpool()).expect("cpool");
        assert_eq!(cpool.values().len(), 1);

        let func_val = cpool.values()[0];
        let func = parser.function_ref(func_val).expect("func");
        assert_eq!(func.arg_count(), 0);
        let func_ops = decode_ops(
            parser
                .byte_array_ref(func.byte_code())
                .expect("func bytecode")
                .buf(),
        );
        assert_eq!(func_ops, [OP_RETURN_UNDEF]);
    }

    #[test]
    fn program_named_function_expr_binds_name() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let mut parser = ExprParser::new(&mut ctx, b"var f = function foo(){ foo; };");
        parser.attach_parse_state();
        parser.parse_program().expect("parse");

        let root = parser
            .function_ref(parser.func.value())
            .expect("root func");
        let cpool = parser.value_array_ref(root.cpool()).expect("cpool");
        let func_val = cpool.values()[0];
        let func = parser.function_ref(func_val).expect("func");
        assert!(func.has_local_func_name());

        let func_ops = decode_ops(
            parser
                .byte_array_ref(func.byte_code())
                .expect("func bytecode")
                .buf(),
        );
        assert!(func_ops.contains(&OP_THIS_FUNC));
    }

    #[test]
    fn program_arguments_usage_sets_flag() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let mut parser = ExprParser::new(&mut ctx, b"function foo(){ arguments; }");
        parser.attach_parse_state();
        parser.parse_program().expect("parse");

        let root = parser
            .function_ref(parser.func.value())
            .expect("root func");
        let cpool = parser.value_array_ref(root.cpool()).expect("cpool");
        let func_val = cpool.values()[0];
        let func = parser.function_ref(func_val).expect("func");
        assert!(func.has_arguments());

        let func_ops = decode_ops(
            parser
                .byte_array_ref(func.byte_code())
                .expect("func bytecode")
                .buf(),
        );
        assert!(func_ops.contains(&OP_ARGUMENTS));
    }

    #[test]
    fn program_object_getter_emits_define_getter() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let mut parser = ExprParser::new(&mut ctx, b"({ get foo() { return 1; } });");
        parser.attach_parse_state();
        parser.parse_program().expect("parse");

        let ops = parse_program_ops(&parser);
        assert!(ops.contains(&OP_DEFINE_GETTER));
    }

    #[test]
    fn eval_program_var_assignment_matches_c_bytecode() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let mut parser = ExprParser::new(&mut ctx, b"var x = 1; x = x + 2;");
        parser.attach_parse_state();
        parser
            .parse_program_with_flags(true, false, false)
            .expect("parse");

        let ops = parse_program_ops(&parser);
        assert_eq!(
            ops,
            [
                OP_PUSH_1,
                OP_PUT_VAR_REF_NOCHECK,
                OP_GET_VAR_REF,
                OP_PUSH_2,
                OP_ADD,
                OP_PUT_VAR_REF,
                OP_RETURN_UNDEF,
            ]
        );
    }

    #[test]
    fn eval_program_loop_functions_match_c_bytecode() {
        let source = br#"
function test_while()
{
    var i, c;
    i = 0;
    c = 0;
    while (i < 3) {
        c++;
        i++;
    }
    assert(c === 3);
}

function test_while_break()
{
    var i, c;
    i = 0;
    c = 0;
    while (i < 3) {
        c++;
        if (i == 1)
            break;
        i++;
    }
    assert(c === 2 && i === 1);
}

function test_do_while()
{
    var i, c;
    i = 0;
    c = 0;
    do {
        c++;
        i++;
    } while (i < 3);
    assert(c === 3 && i === 3);
}
"#;

        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let mut parser = ExprParser::new(&mut ctx, source);
        parser.attach_parse_state();
        parser
            .parse_program_with_flags(true, false, false)
            .expect("parse");

        let root = parser
            .function_ref(parser.func.value())
            .expect("root func");

        let func = find_function_by_name(&parser, root, b"test_while").expect("test_while");
        let ops = function_ops(&parser, func);
        assert_eq!(
            ops,
            [
                OP_PUSH_0,
                OP_PUT_LOC0,
                OP_PUSH_0,
                OP_PUT_LOC1,
                OP_GET_LOC0,
                OP_PUSH_3,
                OP_LT,
                OP_IF_FALSE,
                OP_GET_LOC1,
                OP_INC,
                OP_PUT_LOC1,
                OP_GET_LOC0,
                OP_INC,
                OP_PUT_LOC0,
                OP_GOTO,
                OP_GET_VAR_REF,
                OP_GET_LOC1,
                OP_PUSH_3,
                OP_STRICT_EQ,
                OP_CALL,
                OP_DROP,
                OP_RETURN_UNDEF,
            ]
        );

        let func = find_function_by_name(&parser, root, b"test_while_break")
            .expect("test_while_break");
        let ops = function_ops(&parser, func);
        assert_eq!(
            ops,
            [
                OP_PUSH_0,
                OP_PUT_LOC0,
                OP_PUSH_0,
                OP_PUT_LOC1,
                OP_GET_LOC0,
                OP_PUSH_3,
                OP_LT,
                OP_IF_FALSE,
                OP_GET_LOC1,
                OP_INC,
                OP_PUT_LOC1,
                OP_GET_LOC0,
                OP_PUSH_1,
                OP_EQ,
                OP_IF_FALSE,
                OP_GOTO,
                OP_GET_LOC0,
                OP_INC,
                OP_PUT_LOC0,
                OP_GOTO,
                OP_GET_VAR_REF,
                OP_GET_LOC1,
                OP_PUSH_2,
                OP_STRICT_EQ,
                OP_DUP,
                OP_IF_FALSE,
                OP_DROP,
                OP_GET_LOC0,
                OP_PUSH_1,
                OP_STRICT_EQ,
                OP_CALL,
                OP_DROP,
                OP_RETURN_UNDEF,
            ]
        );

        let func = find_function_by_name(&parser, root, b"test_do_while").expect("test_do_while");
        let ops = function_ops(&parser, func);
        assert_eq!(
            ops,
            [
                OP_PUSH_0,
                OP_PUT_LOC0,
                OP_PUSH_0,
                OP_PUT_LOC1,
                OP_GET_LOC1,
                OP_INC,
                OP_PUT_LOC1,
                OP_GET_LOC0,
                OP_INC,
                OP_PUT_LOC0,
                OP_GET_LOC0,
                OP_PUSH_3,
                OP_LT,
                OP_IF_TRUE,
                OP_GET_VAR_REF,
                OP_GET_LOC1,
                OP_PUSH_3,
                OP_STRICT_EQ,
                OP_DUP,
                OP_IF_FALSE,
                OP_DROP,
                OP_GET_LOC0,
                OP_PUSH_3,
                OP_STRICT_EQ,
                OP_CALL,
                OP_DROP,
                OP_RETURN_UNDEF,
            ]
        );
    }
}
