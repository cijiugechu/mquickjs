//! Public API entry points for JavaScript evaluation and execution.
//!
//! This module provides the main entry points for evaluating JavaScript code:
//! - `js_eval` - Parse and execute JavaScript source code
//! - `js_parse` - Parse JavaScript source code without execution
//! - `js_call` - Call a JavaScript function with arguments
//! - `js_run` - Execute parsed bytecode

use crate::bytecode::{is_bytecode, relocate_bytecode, relocate_bytecode_in_place, BytecodeRelocError};
use crate::containers::ValueArrayHeader;
use crate::context::{BacktraceLocation, ContextError, JSContext};
pub use crate::exception::JsFormatArg;
use crate::conversion;
use crate::enums::JSObjectClass;
use crate::gc::GcMarkConfig;
use crate::gc_ref::GcRef;
use crate::interpreter::{
    call, call_with_this, call_with_this_flags, create_closure, InterpreterError,
};
use crate::jsvalue::{JSValue, JSWord};
use crate::memblock::{MbHeader, MTag};
use crate::object::{Object, ObjectHeader, ObjectUserData};
use crate::parser::entry::{parse_source, ParseError, ParseOutput};
use crate::parser::json::JsonValue;
use crate::parser::regexp::RegExpBytecode;
use crate::stdlib::stdlib_def::{BytecodeHeader, JS_BYTECODE_MAGIC, JS_BYTECODE_VERSION};
use crate::string::runtime::string_view;
use core::ffi::c_void;
use core::mem::size_of;
use core::ptr::{self, NonNull};
use core::slice;
#[cfg(target_pointer_width = "64")]
use crate::bytecode::{prepare_bytecode_64to32, BytecodePrepareError};
#[cfg(target_pointer_width = "64")]
use crate::stdlib::stdlib_def::BytecodeHeader32;

/// Error type for API operations.
#[derive(Clone, Debug)]
pub enum ApiError {
    Parse(ParseError),
    Runtime(String),
    NotAFunction,
    NotABytecode,
}

#[derive(Clone, Debug)]
pub enum BytecodeExportError {
    Context(ContextError),
    InvalidBytecodeFunction,
    #[cfg(target_pointer_width = "64")]
    Prepare32(BytecodePrepareError),
    Relocation(BytecodeRelocError),
}

impl From<ContextError> for BytecodeExportError {
    fn from(err: ContextError) -> Self {
        BytecodeExportError::Context(err)
    }
}

#[cfg(target_pointer_width = "64")]
impl From<BytecodePrepareError> for BytecodeExportError {
    fn from(err: BytecodePrepareError) -> Self {
        BytecodeExportError::Prepare32(err)
    }
}

impl From<BytecodeRelocError> for BytecodeExportError {
    fn from(err: BytecodeRelocError) -> Self {
        BytecodeExportError::Relocation(err)
    }
}

impl core::fmt::Display for BytecodeExportError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            BytecodeExportError::Context(err) => write!(f, "context error: {err:?}"),
            BytecodeExportError::InvalidBytecodeFunction => {
                write!(f, "bytecode function expected")
            }
            #[cfg(target_pointer_width = "64")]
            BytecodeExportError::Prepare32(err) => write!(f, "bytecode 32-bit prepare error: {err:?}"),
            BytecodeExportError::Relocation(err) => write!(f, "bytecode relocation error: {err:?}"),
        }
    }
}

impl From<ParseError> for ApiError {
    fn from(err: ParseError) -> Self {
        ApiError::Parse(err)
    }
}

const FRAME_CF_ARGC_MASK: i32 = 0xffff;
const N_ROM_ATOM_TABLES_MAX: u8 = 2;

fn with_call_depth<T>(ctx: &mut JSContext, f: impl FnOnce(&mut JSContext) -> T) -> Option<T> {
    if !ctx.enter_call() {
        return None;
    }
    let result = f(ctx);
    ctx.exit_call();
    Some(result)
}

/// Parse and execute JavaScript source code.
///
/// # Arguments
/// * `ctx` - The JavaScript context
/// * `source` - The JavaScript source code as bytes
/// * `eval_flags` - Evaluation flags (e.g., JS_EVAL_JSON, JS_EVAL_RETVAL)
///
/// # Returns
/// The result of evaluating the source code, or `JSValue::JS_EXCEPTION` on error.
pub fn js_eval(ctx: &mut JSContext, source: &[u8], eval_flags: u32) -> JSValue {
    match js_eval_internal(ctx, source, eval_flags) {
        Ok(val) => val,
        Err(err) => handle_api_error(ctx, err),
    }
}

/// Parse and execute JavaScript source code with a custom filename.
pub fn js_eval_with_filename(
    ctx: &mut JSContext,
    source: &[u8],
    eval_flags: u32,
    filename: &str,
) -> JSValue {
    match js_eval_internal(ctx, source, eval_flags) {
        Ok(val) => val,
        Err(err) => handle_api_error_with_filename(ctx, err, filename),
    }
}

// Intermediate result from parsing that doesn't hold context borrow
enum ParsedResult {
    Json(JsonValue),
    RegExp(RegExpBytecode, Vec<u8>), // bytecode, source bytes
    Bytecode(JSValue),               // function bytecode pointer
}

fn is_function_bytecode(val: JSValue) -> bool {
    let Some(ptr) = val.to_ptr::<u8>() else {
        return false;
    };
    let header_word = unsafe {
        // SAFETY: ptr points at a readable memblock header.
        ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
    };
    let header = MbHeader::from_word(header_word);
    header.tag() == MTag::FunctionBytecode
}

fn js_eval_internal(
    ctx: &mut JSContext,
    source: &[u8],
    eval_flags: u32,
) -> Result<JSValue, ApiError> {
    // First phase: parse and extract what we need without holding ctx borrow
    let parsed = {
        let output = parse_source(ctx, source, eval_flags)?;
        match output {
            ParseOutput::Json(json_val) => ParsedResult::Json(json_val),
            ParseOutput::RegExp(bytecode) => {
                ParsedResult::RegExp(bytecode, source.to_vec())
            }
            ParseOutput::Program(mut parser) => {
                let func_bytecode = parser
                    .materialize_bytecode()
                    .map_err(|err| ApiError::Runtime(err.message().into_owned()))?;
                ParsedResult::Bytecode(func_bytecode)
            }
        }
    };

    // Second phase: create JS values from the parsed result
    match parsed {
        ParsedResult::Json(json_val) => Ok(json_to_jsvalue(ctx, json_val)),
        ParsedResult::RegExp(bytecode, source_bytes) => {
            let source_val = ctx.new_string_len(&source_bytes).map_err(|_| {
                ApiError::Runtime("failed to create regexp source string".to_string())
            })?;
            let bytecode_val = ctx
                .alloc_byte_array(bytecode.bytes())
                .map_err(|_| ApiError::Runtime("failed to create regexp bytecode".to_string()))?;
            ctx.alloc_regexp(source_val, bytecode_val, bytecode.flags() as i32)
                .map_err(|_| ApiError::Runtime("failed to create regexp object".to_string()))
        }
        ParsedResult::Bytecode(func_bytecode) => {
            if func_bytecode == JSValue::JS_NULL {
                return Err(ApiError::NotABytecode);
            }
            // Create a closure from the bytecode (resolves external vars)
            let closure = create_closure(ctx, func_bytecode, None)
                .map_err(|e| ApiError::Runtime(format!("{:?}", e)))?;
            // Execute the closure
            match with_call_depth(ctx, |ctx| call(ctx, closure, &[])) {
                Some(result) => match result {
                    Ok(val) => Ok(val),
                    Err(err) => map_interpreter_error(ctx, err),
                },
                None => Err(ApiError::Runtime("C stack overflow".to_string())),
            }
        }
    }
}

/// Parse JavaScript source code without execution.
///
/// # Arguments
/// * `ctx` - The JavaScript context
/// * `source` - The JavaScript source code as bytes
/// * `eval_flags` - Evaluation flags
///
/// # Returns
/// The parsed bytecode as a function object, or `JSValue::JS_EXCEPTION` on error.
pub fn js_parse(ctx: &mut JSContext, source: &[u8], eval_flags: u32) -> JSValue {
    match js_parse_internal(ctx, source, eval_flags) {
        Ok(val) => val,
        Err(err) => handle_api_error(ctx, err),
    }
}

/// Parse JavaScript source code and return raw bytecode (no closure).
pub fn js_parse_bytecode(ctx: &mut JSContext, source: &[u8], eval_flags: u32) -> JSValue {
    match js_parse_bytecode_internal(ctx, source, eval_flags) {
        Ok(val) => val,
        Err(err) => handle_api_error(ctx, err),
    }
}

/// Parse JavaScript source code and return raw bytecode with a custom filename.
pub fn js_parse_bytecode_with_filename(
    ctx: &mut JSContext,
    source: &[u8],
    eval_flags: u32,
    filename: &str,
) -> JSValue {
    match js_parse_bytecode_internal(ctx, source, eval_flags) {
        Ok(val) => val,
        Err(err) => handle_api_error_with_filename(ctx, err, filename),
    }
}

/// Parse JavaScript source code without execution, with a custom filename.
pub fn js_parse_with_filename(
    ctx: &mut JSContext,
    source: &[u8],
    eval_flags: u32,
    filename: &str,
) -> JSValue {
    match js_parse_internal(ctx, source, eval_flags) {
        Ok(val) => val,
        Err(err) => handle_api_error_with_filename(ctx, err, filename),
    }
}

fn js_parse_internal(
    ctx: &mut JSContext,
    source: &[u8],
    eval_flags: u32,
) -> Result<JSValue, ApiError> {
    // First phase: parse and extract what we need without holding ctx borrow
    let parsed = {
        let output = parse_source(ctx, source, eval_flags)?;
        match output {
            ParseOutput::Json(json_val) => ParsedResult::Json(json_val),
            ParseOutput::RegExp(bytecode) => {
                ParsedResult::RegExp(bytecode, source.to_vec())
            }
            ParseOutput::Program(mut parser) => {
                let func_bytecode = parser
                    .materialize_bytecode()
                    .map_err(|err| ApiError::Runtime(err.message().into_owned()))?;
                ParsedResult::Bytecode(func_bytecode)
            }
        }
    };

    // Second phase: create JS values from the parsed result
    match parsed {
        ParsedResult::Json(json_val) => Ok(json_to_jsvalue(ctx, json_val)),
        ParsedResult::RegExp(bytecode, source_bytes) => {
            let source_val = ctx.new_string_len(&source_bytes).map_err(|_| {
                ApiError::Runtime("failed to create regexp source string".to_string())
            })?;
            let bytecode_val = ctx
                .alloc_byte_array(bytecode.bytes())
                .map_err(|_| ApiError::Runtime("failed to create regexp bytecode".to_string()))?;
            ctx.alloc_regexp(source_val, bytecode_val, bytecode.flags() as i32)
                .map_err(|_| ApiError::Runtime("failed to create regexp object".to_string()))
        }
        ParsedResult::Bytecode(func_bytecode) => {
            if func_bytecode == JSValue::JS_NULL {
                return Err(ApiError::NotABytecode);
            }
            // Return the closure without executing (resolves external vars)
            create_closure(ctx, func_bytecode, None)
                .map_err(|e| ApiError::Runtime(format!("{:?}", e)))
        }
    }
}

fn js_parse_bytecode_internal(
    ctx: &mut JSContext,
    source: &[u8],
    eval_flags: u32,
) -> Result<JSValue, ApiError> {
    let output = parse_source(ctx, source, eval_flags)?;
    match output {
        ParseOutput::Program(mut parser) => {
            let func_bytecode = parser
                .materialize_bytecode()
                .map_err(|err| ApiError::Runtime(err.message().into_owned()))?;
            if func_bytecode == JSValue::JS_NULL {
                return Err(ApiError::NotABytecode);
            }
            Ok(func_bytecode)
        }
        _ => Err(ApiError::NotABytecode),
    }
}

/// Call a JavaScript function with arguments.
///
/// # Arguments
/// * `ctx` - The JavaScript context
/// * `func` - The function to call
/// * `this_obj` - The `this` value for the call
/// * `args` - The arguments to pass to the function
///
/// # Returns
/// The result of calling the function, or `JSValue::JS_EXCEPTION` on error.
pub fn js_call(
    ctx: &mut JSContext,
    func: JSValue,
    this_obj: JSValue,
    args: &[JSValue],
) -> JSValue {
    if !func.is_function() {
        return ctx.throw_type_error("not a function");
    }
    match with_call_depth(ctx, |ctx| call_with_this(ctx, func, this_obj, args)) {
        Some(Ok(val)) => val,
        Some(Err(err)) => match map_interpreter_error(ctx, err) {
            Ok(val) => val,
            Err(api_err) => handle_api_error(ctx, api_err),
        },
        None => ctx.throw_internal_error("C stack overflow"),
    }
}

/// Execute a parsed function (closure or raw bytecode).
///
/// # Arguments
/// * `ctx` - The JavaScript context
/// * `func` - The parsed function/closure to execute
///
/// # Returns
/// The result of executing the function, or `JSValue::JS_EXCEPTION` on error.
pub fn js_run(ctx: &mut JSContext, func: JSValue) -> JSValue {
    let func = if func.is_function() {
        func
    } else if is_function_bytecode(func) {
        match create_closure(ctx, func, None) {
            Ok(closure) => closure,
            Err(_) => return JSValue::JS_EXCEPTION,
        }
    } else {
        return ctx.throw_type_error("bytecode function expected");
    };
    match with_call_depth(ctx, |ctx| call(ctx, func, &[])) {
        Some(Ok(val)) => val,
        Some(Err(err)) => match map_interpreter_error(ctx, err) {
            Ok(val) => val,
            Err(api_err) => handle_api_error(ctx, api_err),
        },
        None => ctx.throw_internal_error("C stack overflow"),
    }
}

/// Call a JavaScript function using the C-style stack layout.
pub fn js_call_stack(ctx: &mut JSContext, call_flags: i32) -> JSValue {
    let argc = (call_flags & FRAME_CF_ARGC_MASK) as usize;
    let sp = ctx.sp();
    let base = sp.as_ptr();
    let this_obj = unsafe {
        // SAFETY: stack holds this/func/args as required by the caller.
        ptr::read_unaligned(base)
    };
    let func = unsafe {
        // SAFETY: stack holds this/func/args as required by the caller.
        ptr::read_unaligned(base.add(1))
    };
    let args = unsafe {
        // SAFETY: caller guarantees argc arguments are on the stack.
        slice::from_raw_parts(base.add(2), argc)
    };
    let result = match with_call_depth(ctx, |ctx| {
        call_with_this_flags(ctx, func, this_obj, args, call_flags)
    }) {
        Some(Ok(val)) => val,
        Some(Err(err)) => match map_interpreter_error(ctx, err) {
            Ok(val) => val,
            Err(api_err) => handle_api_error(ctx, api_err),
        },
        None => return ctx.throw_internal_error("C stack overflow"),
    };
    let new_sp = unsafe { base.add(argc + 2) };
    ctx.set_sp(NonNull::new(new_sp).expect("stack pointer"));
    result
}

/// Run a full GC cycle.
pub fn js_gc(ctx: &mut JSContext) {
    ctx.gc();
}

/// Throw a JavaScript value as an exception.
pub fn js_throw(ctx: &mut JSContext, obj: JSValue) -> JSValue {
    ctx.throw(obj)
}

/// Throw a formatted Error instance with the given class.
pub fn js_throw_error(
    ctx: &mut JSContext,
    class: JSObjectClass,
    message: &str,
) -> JSValue {
    ctx.throw_error(class, message)
}

/// Throw a formatted Error instance with the given class.
pub fn js_throw_error_fmt(
    ctx: &mut JSContext,
    class: JSObjectClass,
    fmt: &str,
    args: &[JsFormatArg<'_>],
) -> JSValue {
    ctx.throw_error_fmt(class, fmt, args)
}

/// Throw a TypeError with a static message.
pub fn js_throw_type_error(ctx: &mut JSContext, message: &str) -> JSValue {
    ctx.throw_type_error(message)
}

/// Throw a TypeError with formatted args.
pub fn js_throw_type_error_fmt(
    ctx: &mut JSContext,
    fmt: &str,
    args: &[JsFormatArg<'_>],
) -> JSValue {
    ctx.throw_type_error_fmt(fmt, args)
}

/// Throw a ReferenceError with a static message.
pub fn js_throw_reference_error(ctx: &mut JSContext, message: &str) -> JSValue {
    ctx.throw_reference_error(message)
}

/// Throw a ReferenceError with formatted args.
pub fn js_throw_reference_error_fmt(
    ctx: &mut JSContext,
    fmt: &str,
    args: &[JsFormatArg<'_>],
) -> JSValue {
    ctx.throw_reference_error_fmt(fmt, args)
}

/// Throw a RangeError with a static message.
pub fn js_throw_range_error(ctx: &mut JSContext, message: &str) -> JSValue {
    ctx.throw_range_error(message)
}

/// Throw a RangeError with formatted args.
pub fn js_throw_range_error_fmt(
    ctx: &mut JSContext,
    fmt: &str,
    args: &[JsFormatArg<'_>],
) -> JSValue {
    ctx.throw_range_error_fmt(fmt, args)
}

/// Throw a SyntaxError with a static message.
pub fn js_throw_syntax_error(ctx: &mut JSContext, message: &str) -> JSValue {
    ctx.throw_syntax_error(message)
}

/// Throw a SyntaxError with formatted args.
pub fn js_throw_syntax_error_fmt(
    ctx: &mut JSContext,
    fmt: &str,
    args: &[JsFormatArg<'_>],
) -> JSValue {
    ctx.throw_syntax_error_fmt(fmt, args)
}

/// Throw an InternalError with a static message.
pub fn js_throw_internal_error(ctx: &mut JSContext, message: &str) -> JSValue {
    ctx.throw_internal_error(message)
}

/// Throw an InternalError with formatted args.
pub fn js_throw_internal_error_fmt(
    ctx: &mut JSContext,
    fmt: &str,
    args: &[JsFormatArg<'_>],
) -> JSValue {
    ctx.throw_internal_error_fmt(fmt, args)
}

/// Throw the standard out-of-memory error.
pub fn js_throw_out_of_memory(ctx: &mut JSContext) -> JSValue {
    ctx.throw_out_of_memory()
}

/// Get the global object from the context.
pub fn js_get_global_object(ctx: &JSContext) -> JSValue {
    ctx.global_obj()
}

/// Get a property from an object by string name.
pub fn js_get_property_str(ctx: &mut JSContext, obj: JSValue, name: &str) -> JSValue {
    let key = match ctx.new_string(name) {
        Ok(k) => k,
        Err(_) => return JSValue::JS_EXCEPTION,
    };
    let key = match conversion::to_property_key(ctx, key) {
        Ok(k) => k,
        Err(_) => return JSValue::JS_EXCEPTION,
    };
    match crate::property::get_property(ctx, obj, key) {
        Ok(val) => val,
        Err(_) => JSValue::JS_EXCEPTION,
    }
}

/// Set a property on an object by string name.
pub fn js_set_property_str(
    ctx: &mut JSContext,
    obj: JSValue,
    name: &str,
    val: JSValue,
) -> JSValue {
    let key = match ctx.new_string(name) {
        Ok(k) => k,
        Err(_) => return JSValue::JS_EXCEPTION,
    };
    let key = match conversion::to_property_key(ctx, key) {
        Ok(k) => k,
        Err(_) => return JSValue::JS_EXCEPTION,
    };
    match crate::property::set_property(ctx, obj, key, val) {
        Ok(()) => JSValue::JS_UNDEFINED,
        Err(_) => JSValue::JS_EXCEPTION,
    }
}

/// Get a property from an object by numeric index.
pub fn js_get_property_uint32(ctx: &mut JSContext, obj: JSValue, idx: u32) -> JSValue {
    if idx > JSValue::JS_SHORTINT_MAX as u32 {
        return ctx.throw_range_error("invalid array index");
    }
    let key = JSValue::new_short_int(idx as i32);
    match crate::property::get_property(ctx, obj, key) {
        Ok(val) => val,
        Err(_) => JSValue::JS_EXCEPTION,
    }
}

/// Set a property on an object by numeric index.
pub fn js_set_property_uint32(
    ctx: &mut JSContext,
    obj: JSValue,
    idx: u32,
    val: JSValue,
) -> JSValue {
    if idx > JSValue::JS_SHORTINT_MAX as u32 {
        return ctx.throw_range_error("invalid array index");
    }
    let key = JSValue::new_short_int(idx as i32);
    match crate::property::set_property(ctx, obj, key, val) {
        Ok(()) => JSValue::JS_UNDEFINED,
        Err(_) => JSValue::JS_EXCEPTION,
    }
}

fn handle_api_error(ctx: &mut JSContext, err: ApiError) -> JSValue {
    handle_api_error_with_filename(ctx, err, "<input>")
}

fn map_interpreter_error(ctx: &mut JSContext, err: InterpreterError) -> Result<JSValue, ApiError> {
    match err {
        InterpreterError::Thrown(thrown) => {
            if thrown != JSValue::JS_EXCEPTION {
                ctx.set_current_exception(thrown);
            }
            Ok(JSValue::JS_EXCEPTION)
        }
        InterpreterError::TypeError(msg) => {
            let _ = ctx.throw_type_error(msg);
            Ok(JSValue::JS_EXCEPTION)
        }
        InterpreterError::ReferenceError(msg) => {
            let _ = ctx.throw_reference_error(msg);
            Ok(JSValue::JS_EXCEPTION)
        }
        InterpreterError::NotAFunction => {
            let _ = ctx.throw_type_error("not a function");
            Ok(JSValue::JS_EXCEPTION)
        }
        InterpreterError::Context(ContextError::OutOfMemory) => {
            let _ = ctx.throw_out_of_memory();
            Ok(JSValue::JS_EXCEPTION)
        }
        err => Err(ApiError::Runtime(format!("{:?}", err))),
    }
}

fn handle_api_error_with_filename(ctx: &mut JSContext, err: ApiError, filename: &str) -> JSValue {
    match err {
        ApiError::Parse(err) => {
            let val = ctx.throw_syntax_error(err.message());
            let location = BacktraceLocation {
                filename,
                line: err.line() + 1,
                column: err.column() + 1,
            };
            let error_obj = ctx.current_exception();
            let _ = ctx.build_backtrace(error_obj, Some(location), 0);
            val
        }
        ApiError::NotAFunction => ctx.throw_type_error("not a function"),
        ApiError::NotABytecode => ctx.throw_type_error("bytecode function expected"),
        ApiError::Runtime(message) => ctx.throw_internal_error(&message),
    }
}

fn handle_conversion_error(ctx: &mut JSContext, err: conversion::ConversionError) {
    match err {
        conversion::ConversionError::Context(_) => {
            let _ = ctx.throw_out_of_memory();
        }
        conversion::ConversionError::TypeError(msg) => {
            let _ = ctx.throw_type_error(msg);
        }
        _ => {}
    }
}

/// Create a new JavaScript object.
pub fn js_new_object(ctx: &mut JSContext) -> JSValue {
    ctx.alloc_object_default().unwrap_or(JSValue::JS_EXCEPTION)
}

/// Create a new float64 value.
pub fn js_new_float64(ctx: &mut JSContext, val: f64) -> JSValue {
    ctx.new_float64(val).unwrap_or(JSValue::JS_EXCEPTION)
}

/// Create a new int64 value.
pub fn js_new_int64(ctx: &mut JSContext, val: i64) -> JSValue {
    ctx.new_int64(val).unwrap_or(JSValue::JS_EXCEPTION)
}

/// Create a new int32 value.
pub fn js_new_int32(ctx: &mut JSContext, val: i32) -> JSValue {
    ctx.new_int32(val).unwrap_or(JSValue::JS_EXCEPTION)
}

/// Create a new uint32 value.
pub fn js_new_uint32(ctx: &mut JSContext, val: u32) -> JSValue {
    ctx.new_uint32(val).unwrap_or(JSValue::JS_EXCEPTION)
}

/// Create a new JavaScript array.
pub fn js_new_array(ctx: &mut JSContext, len: usize) -> JSValue {
    ctx.alloc_array(len).unwrap_or(JSValue::JS_EXCEPTION)
}

/// Create a new user object for the specified class id.
pub fn js_new_object_class_user(ctx: &mut JSContext, class_id: u8) -> JSValue {
    if class_id < JSObjectClass::User as u8 {
        return ctx.throw_type_error("invalid user class id");
    }
    let proto = match ctx.class_proto().get(class_id as usize).copied() {
        Some(proto) => proto,
        None => return JSValue::JS_EXCEPTION,
    };
    let extra = size_of::<crate::object::ObjectUserData>();
    ctx.alloc_object_class_id(class_id, proto, extra).unwrap_or(JSValue::JS_EXCEPTION)
}

/// Create a new C function object with parameters.
pub fn js_new_cfunction_params(ctx: &mut JSContext, func_idx: u32, params: JSValue) -> JSValue {
    ctx.new_cfunction_params(func_idx, params).unwrap_or(JSValue::JS_EXCEPTION)
}

/// Create a new JavaScript string.
pub fn js_new_string(ctx: &mut JSContext, s: &str) -> JSValue {
    ctx.new_string(s).unwrap_or(JSValue::JS_EXCEPTION)
}

/// Create a new JavaScript string from raw bytes.
pub fn js_new_string_len(ctx: &mut JSContext, bytes: &[u8]) -> JSValue {
    ctx.new_string_len(bytes).unwrap_or(JSValue::JS_EXCEPTION)
}

/// Convert a JavaScript value to string.
pub fn js_to_string(ctx: &mut JSContext, val: JSValue) -> JSValue {
    match conversion::to_string(ctx, val) {
        Ok(s) => s,
        Err(_) => JSValue::JS_EXCEPTION,
    }
}

/// Convert a JavaScript value to number.
pub fn js_to_number(ctx: &mut JSContext, val: JSValue) -> f64 {
    match conversion::to_number(ctx, val) {
        Ok(num) => num,
        Err(err) => {
            handle_conversion_error(ctx, err);
            f64::NAN
        }
    }
}

/// Convert a JavaScript value to int32.
pub fn js_to_int32(ctx: &mut JSContext, val: JSValue) -> i32 {
    match conversion::to_int32(ctx, val) {
        Ok(num) => num,
        Err(err) => {
            handle_conversion_error(ctx, err);
            0
        }
    }
}

/// Convert a JavaScript value to uint32.
pub fn js_to_uint32(ctx: &mut JSContext, val: JSValue) -> u32 {
    match conversion::to_uint32(ctx, val) {
        Ok(num) => num,
        Err(err) => {
            handle_conversion_error(ctx, err);
            0
        }
    }
}

/// Convert a JavaScript value to int32 with saturation.
pub fn js_to_int32_sat(ctx: &mut JSContext, val: JSValue) -> i32 {
    match conversion::to_int32_sat(ctx, val) {
        Ok(num) => num,
        Err(err) => {
            handle_conversion_error(ctx, err);
            0
        }
    }
}

/// Convert a JavaScript value to raw string bytes (UTF-8, no implicit NUL).
pub fn js_to_cstring_len(ctx: &mut JSContext, val: JSValue) -> Option<Vec<u8>> {
    let val = conversion::to_string(ctx, val).ok()?;
    let mut scratch = [0u8; 5];
    let view = string_view(val, &mut scratch)?;
    Some(view.bytes().to_vec())
}

/// Convert a JavaScript value to a lossy UTF-8 String.
pub fn js_to_cstring(ctx: &mut JSContext, val: JSValue) -> Option<String> {
    let bytes = js_to_cstring_len(ctx, val)?;
    Some(String::from_utf8_lossy(&bytes).into_owned())
}

/// Format the current exception as a string (message + optional stack).
pub fn js_get_error_str(ctx: &mut JSContext) -> String {
    let obj = ctx.current_exception();
    let mut out = String::new();
    if let Some(bytes) = js_to_cstring_len(ctx, obj) {
        out.push_str(&String::from_utf8_lossy(&bytes));
    }
    if let Ok(stack) = ctx.get_error_stack(obj)
        && stack != JSValue::JS_NULL
        && let Some(bytes) = js_to_cstring_len(ctx, stack)
    {
        if !out.is_empty() {
            out.push('\n');
        }
        out.push_str(&String::from_utf8_lossy(&bytes));
    }
    out
}

/// Check if a buffer contains a bytecode header.
pub fn js_is_bytecode(buf: &[u8]) -> bool {
    is_bytecode(buf)
}

/// Relocate bytecode in-place so it can be executed later.
pub fn js_relocate_bytecode(buf: &mut [u8]) -> Result<(), BytecodeRelocError> {
    unsafe { relocate_bytecode_in_place(buf, None) }
}

/// Load a precompiled bytecode buffer (must remain alive for the context lifetime).
pub fn js_load_bytecode(ctx: &mut JSContext, buf: &[u8]) -> JSValue {
    if ctx.atom_tables().unique_len() != 0 {
        return ctx.throw_internal_error("no atom must be defined in RAM");
    }
    if ctx.n_rom_atom_tables() >= N_ROM_ATOM_TABLES_MAX {
        return ctx.throw_internal_error("too many rom atom tables");
    }
    if buf.len() < size_of::<BytecodeHeader>() {
        return ctx.throw_internal_error("bytecode buffer too small");
    }
    let header = unsafe {
        // SAFETY: buffer length covers the header.
        ptr::read_unaligned(buf.as_ptr().cast::<BytecodeHeader>())
    };
    if header.magic != crate::stdlib::stdlib_def::JS_BYTECODE_MAGIC {
        return ctx.throw_internal_error("invalid bytecode magic");
    }
    if header.version != crate::stdlib::stdlib_def::JS_BYTECODE_VERSION {
        return ctx.throw_internal_error("invalid bytecode version");
    }
    let data_ptr = unsafe { buf.as_ptr().add(size_of::<BytecodeHeader>()) } as usize;
    if header.base_addr != data_ptr {
        return ctx.throw_internal_error("bytecode not relocated");
    }
    let Some(unique_strings) = bytecode_value_array(header.unique_strings) else {
        return ctx.throw_internal_error("invalid bytecode unique strings");
    };
    ctx.add_rom_atom_table(unique_strings);
    header.main_func
}

/// Prepare a bytecode buffer for saving to disk (header + heap bytes).
pub fn js_prepare_bytecode(
    ctx: &mut JSContext,
    main_func: JSValue,
) -> Result<Vec<u8>, BytecodeExportError> {
    if !is_function_bytecode(main_func) {
        return Err(BytecodeExportError::InvalidBytecodeFunction);
    }

    ctx.clear_roots_for_bytecode();

    let mut func_ref = GcRef::new(JSValue::JS_UNDEFINED);
    let func_slot = ctx.add_gc_ref(&mut func_ref);
    unsafe {
        // SAFETY: func_slot is the GC root slot for func_ref.
        *func_slot = main_func;
    }

    let unique_strings = build_unique_strings_array(ctx)?;
    let mut unique_ref = GcRef::new(JSValue::JS_UNDEFINED);
    let unique_slot = ctx.add_gc_ref(&mut unique_ref);
    unsafe {
        // SAFETY: unique_slot is the GC root slot for unique_ref.
        *unique_slot = unique_strings;
    }

    ctx.gc_collect_with_config(GcMarkConfig::default());
    let unique_strings = unsafe {
        // SAFETY: unique_slot remains valid while the GC ref is registered.
        *unique_slot
    };
    let main_func = unsafe {
        // SAFETY: func_slot remains valid while the GC ref is registered.
        *func_slot
    };

    let result = (|| {
        let base = ctx.heap().heap_base().as_ptr();
        let heap_free = ctx.heap().heap_free().as_ptr();
        let data_len = heap_free as usize - base as usize;

        let header = BytecodeHeader {
            magic: JS_BYTECODE_MAGIC,
            version: JS_BYTECODE_VERSION,
            base_addr: base as usize,
            unique_strings,
            main_func,
        };

        let mut buf = Vec::with_capacity(size_of::<BytecodeHeader>() + data_len);
        buf.resize(size_of::<BytecodeHeader>(), 0);
        unsafe {
            // SAFETY: buffer has header space and is writable.
            ptr::write_unaligned(buf.as_mut_ptr().cast::<BytecodeHeader>(), header);
            let data_slice = slice::from_raw_parts(base, data_len);
            buf.extend_from_slice(data_slice);
        }

        unsafe {
            let header_ptr = buf.as_mut_ptr().cast::<BytecodeHeader>();
            let mut header = ptr::read_unaligned(header_ptr);
            let data = &mut buf[size_of::<BytecodeHeader>()..];
            relocate_bytecode(&mut header, data, JSValue::JSW as usize, None)?;
            ptr::write_unaligned(header_ptr, header);
        }

        Ok(buf)
    })();

    ctx.delete_gc_ref(&unique_ref);
    ctx.delete_gc_ref(&func_ref);

    result
}

#[cfg(target_pointer_width = "64")]
/// Prepare a 32-bit bytecode buffer for saving to disk.
pub fn js_prepare_bytecode_64to32(
    ctx: &mut JSContext,
    main_func: JSValue,
) -> Result<Vec<u8>, BytecodeExportError> {
    if !is_function_bytecode(main_func) {
        return Err(BytecodeExportError::InvalidBytecodeFunction);
    }

    ctx.clear_roots_for_bytecode();

    let mut func_ref = GcRef::new(JSValue::JS_UNDEFINED);
    let func_slot = ctx.add_gc_ref(&mut func_ref);
    unsafe {
        // SAFETY: func_slot is the GC root slot for func_ref.
        *func_slot = main_func;
    }

    let unique_strings = build_unique_strings_array(ctx)?;
    let mut unique_ref = GcRef::new(JSValue::JS_UNDEFINED);
    let unique_slot = ctx.add_gc_ref(&mut unique_ref);
    unsafe {
        // SAFETY: unique_slot is the GC root slot for unique_ref.
        *unique_slot = unique_strings;
    }

    let result = (|| {
        let (mut roots, heap) = ctx.gc_roots_for_export_with_heap();
        let image = unsafe {
            // SAFETY: heap points at the active heap layout and roots cover all live values.
            prepare_bytecode_64to32(
                &mut *heap,
                &mut roots,
                unique_slot,
                func_slot,
                GcMarkConfig::default(),
            )?
        };

        let base = ctx.heap().heap_base().as_ptr();
        let data_len = image.len;
        let mut buf = Vec::with_capacity(size_of::<BytecodeHeader32>() + data_len);
        buf.resize(size_of::<BytecodeHeader32>(), 0);
        unsafe {
            // SAFETY: buffer has header space and is writable.
            ptr::write_unaligned(buf.as_mut_ptr().cast::<BytecodeHeader32>(), image.header);
            let data_slice = slice::from_raw_parts(base, data_len);
            buf.extend_from_slice(data_slice);
        }
        Ok(buf)
    })();

    ctx.delete_gc_ref(&unique_ref);
    ctx.delete_gc_ref(&func_ref);

    result
}

/// Set the log function used by debug printing.
pub fn js_set_log_func(ctx: &mut JSContext, write_func: Option<crate::capi_defs::JSWriteFunc>) {
    ctx.set_log_func(write_func);
}

/// Print a JS value using the context log function.
pub fn js_print_value(ctx: &mut JSContext, val: JSValue) {
    if let Some(bytes) = js_to_cstring_len(ctx, val) {
        ctx.write_log(&bytes);
    }
}

/// Print a JS value using the context log function (flags currently ignored).
pub fn js_print_value_f(ctx: &mut JSContext, val: JSValue, _flags: i32) {
    js_print_value(ctx, val);
}

/// Dump a labeled JS value using the context log function (flags currently ignored).
pub fn js_dump_value_f(ctx: &mut JSContext, label: &str, val: JSValue, _flags: i32) {
    let mut buf = String::new();
    buf.push_str(label);
    buf.push_str(": ");
    if let Some(value) = js_to_cstring(ctx, val) {
        buf.push_str(&value);
    }
    ctx.write_log(buf.as_bytes());
}

/// Dump a labeled JS value using the context log function.
pub fn js_dump_value(ctx: &mut JSContext, label: &str, val: JSValue) {
    js_dump_value_f(ctx, label, val, 0);
}

/// Dump basic memory statistics using the context log function.
pub fn js_dump_memory(ctx: &mut JSContext, _is_long: bool) {
    let heap_base = ctx.heap().heap_base().as_ptr() as usize;
    let heap_free = ctx.heap().heap_free().as_ptr() as usize;
    let stack_top = ctx.stack_top().as_ptr() as usize;
    let sp = ctx.sp().as_ptr() as usize;
    let heap_used = heap_free.saturating_sub(heap_base);
    let stack_used = stack_top.saturating_sub(sp);
    let total = stack_top.saturating_sub(heap_base);
    let msg = format!(
        "heap size={}/{} stack_size={}\n",
        heap_used, total, stack_used
    );
    ctx.write_log(msg.as_bytes());
}

/// Get the object class id for a value.
pub fn js_get_class_id(val: JSValue) -> Option<u8> {
    object_ptr_and_header(val).map(|(_, header)| header.class_id())
}

/// Set the opaque pointer for a user class object.
pub fn js_set_opaque(val: JSValue, opaque: *mut c_void) -> bool {
    let Some((obj_ptr, header)) = object_ptr_and_header(val) else {
        return false;
    };
    if header.class_id() < JSObjectClass::User as u8 {
        return false;
    }
    unsafe {
        // SAFETY: obj_ptr points at a valid object payload.
        let payload = Object::payload_ptr(obj_ptr.as_ptr());
        let user = core::ptr::addr_of_mut!((*payload).user);
        ptr::write_unaligned(user, ObjectUserData::new(opaque));
    }
    true
}

/// Get the opaque pointer for a user class object.
pub fn js_get_opaque(val: JSValue) -> Option<*mut c_void> {
    let (obj_ptr, header) = object_ptr_and_header(val)?;
    if header.class_id() < JSObjectClass::User as u8 {
        return None;
    }
    let user = unsafe {
        // SAFETY: obj_ptr points at a valid object payload.
        let payload = Object::payload_ptr(obj_ptr.as_ptr());
        core::ptr::addr_of!((*payload).user)
    };
    let data = unsafe {
        // SAFETY: user points at initialized ObjectUserData.
        ptr::read_unaligned(user)
    };
    Some(data.opaque())
}

fn bytecode_value_array(val: JSValue) -> Option<Vec<JSValue>> {
    let ptr = val.to_ptr::<u8>()?;
    let header_word = unsafe {
        // SAFETY: pointer is expected to reference a memblock header.
        ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
    };
    let header = MbHeader::from_word(header_word);
    if header.tag() != MTag::ValueArray {
        return None;
    }
    let header = ValueArrayHeader::from(header);
    let len = header.size() as usize;
    let arr_ptr = unsafe { ptr.as_ptr().add(size_of::<JSWord>()) as *const JSValue };
    let slice = unsafe {
        // SAFETY: bytecode buffer holds len JSValue entries after the header.
        slice::from_raw_parts(arr_ptr, len)
    };
    Some(slice.to_vec())
}

fn build_unique_strings_array(ctx: &mut JSContext) -> Result<JSValue, ContextError> {
    let unique = ctx.atom_tables().unique_strings().to_vec();
    if unique.is_empty() {
        return Ok(JSValue::JS_NULL);
    }
    let ptr = ctx.alloc_value_array(unique.len())?;
    unsafe {
        // SAFETY: value array payload is writable for unique.len() entries.
        let arr = ptr.as_ptr().add(size_of::<JSWord>()) as *mut JSValue;
        for (idx, &val) in unique.iter().enumerate() {
            ptr::write_unaligned(arr.add(idx), val);
        }
    }
    Ok(JSValue::from_ptr(ptr))
}

fn object_ptr_and_header(val: JSValue) -> Option<(NonNull<Object>, ObjectHeader)> {
    let obj_ptr = val.to_ptr::<Object>()?;
    let header_word = unsafe {
        // SAFETY: obj_ptr points at a readable object header.
        ptr::read_unaligned(obj_ptr.as_ptr().cast::<JSWord>())
    };
    let header = ObjectHeader::from_word(header_word);
    if header.tag() != MTag::Object {
        return None;
    }
    Some((obj_ptr, header))
}

// Convert JSON value to JSValue
fn json_to_jsvalue(ctx: &mut JSContext, json: JsonValue) -> JSValue {
    match json {
        JsonValue::Null => JSValue::JS_NULL,
        JsonValue::Bool(b) => JSValue::new_bool(b as i32),
        JsonValue::Number(n) => {
            // Check if the f64 can be represented as a short int
            let i = n as i64;
            if (i as f64) == n
                && i >= JSValue::JS_SHORTINT_MIN as i64
                && i <= JSValue::JS_SHORTINT_MAX as i64
            {
                return JSValue::new_short_int(i as i32);
            }
            ctx.new_float64(n).unwrap_or(JSValue::JS_EXCEPTION)
        }
        JsonValue::String(s) => ctx.new_string_len(s.bytes()).unwrap_or(JSValue::JS_EXCEPTION),
        JsonValue::Array(arr) => {
            let result = match ctx.alloc_array(arr.len()) {
                Ok(a) => a,
                Err(_) => return JSValue::JS_EXCEPTION,
            };
            for (i, val) in arr.into_iter().enumerate() {
                let js_val = json_to_jsvalue(ctx, val);
                let _ = crate::property::set_property(
                    ctx,
                    result,
                    JSValue::new_short_int(i as i32),
                    js_val,
                );
            }
            result
        }
        JsonValue::Object(obj) => {
            let result = match ctx.alloc_object_default() {
                Ok(o) => o,
                Err(_) => return JSValue::JS_EXCEPTION,
            };
            for (key, val) in obj {
                let key_val = match ctx.intern_string(key.bytes()) {
                    Ok(k) => k,
                    Err(_) => return JSValue::JS_EXCEPTION,
                };
                let key_val = match conversion::to_property_key(ctx, key_val) {
                    Ok(k) => k,
                    Err(_) => return JSValue::JS_EXCEPTION,
                };
                let js_val = json_to_jsvalue(ctx, val);
                let _ = crate::property::define_property_value(ctx, result, key_val, js_val);
            }
            result
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::capi_defs::JS_EVAL_JSON;
    use crate::context::ContextConfig;
    use crate::function_bytecode::FunctionBytecode;
    use crate::memblock::{MbHeader, MTag};
    use crate::opcode::{OP_FCLOSURE, OPCODES};
    use crate::stdlib::MQUICKJS_STDLIB_IMAGE;

    #[test]
    fn eval_json_null() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 64 * 1024,
            prepare_compilation: true,
            finalizers: &[],
        })
        .expect("context init");
        let result = js_eval(&mut ctx, b"null", JS_EVAL_JSON);
        assert!(result.is_null());
    }

    #[test]
    fn eval_json_number() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 64 * 1024,
            prepare_compilation: true,
            finalizers: &[],
        })
        .expect("context init");
        let result = js_eval(&mut ctx, b"42", JS_EVAL_JSON);
        assert!(result.is_number());
        assert_eq!(js_to_number(&mut ctx, result), 42.0);
    }

    #[test]
    fn eval_json_string() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 64 * 1024,
            prepare_compilation: true,
            finalizers: &[],
        })
        .expect("context init");
        let result = js_eval(&mut ctx, b"\"hello\"", JS_EVAL_JSON);
        assert!(result.is_string());
    }

    #[test]
    fn eval_json_array() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 64 * 1024,
            prepare_compilation: true,
            finalizers: &[],
        })
        .expect("context init");
        let result = js_eval(&mut ctx, b"[1, 2, 3]", JS_EVAL_JSON);
        assert!(result.is_object());
    }

    #[test]
    fn eval_json_object() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 64 * 1024,
            prepare_compilation: true,
            finalizers: &[],
        })
        .expect("context init");
        let result = js_eval(&mut ctx, b"{\"a\": 1}", JS_EVAL_JSON);
        assert!(result.is_object());
    }

    #[test]
    fn new_object_and_property() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 64 * 1024,
            prepare_compilation: true,
            finalizers: &[],
        })
        .expect("context init");
        let obj = js_new_object(&mut ctx);
        assert!(obj.is_object());

        let val = JSValue::new_short_int(42);
        js_set_property_str(&mut ctx, obj, "x", val);

        let result = js_get_property_str(&mut ctx, obj, "x");
        assert_eq!(js_to_number(&mut ctx, result), 42.0);
    }

    #[test]
    fn new_array() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 64 * 1024,
            prepare_compilation: true,
            finalizers: &[],
        })
        .expect("context init");
        let arr = js_new_array(&mut ctx, 3);
        assert!(arr.is_object());
    }

    #[test]
    fn new_string() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 64 * 1024,
            prepare_compilation: true,
            finalizers: &[],
        })
        .expect("context init");
        let s = js_new_string(&mut ctx, "hello");
        assert!(s.is_string());
    }

    #[test]
    fn global_object_exists() {
        let ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 64 * 1024,
            prepare_compilation: true,
            finalizers: &[],
        })
        .expect("context init");
        let global = js_get_global_object(&ctx);
        assert!(global.is_object());
    }

    #[test]
    fn new_numbers_roundtrip() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 64 * 1024,
            prepare_compilation: true,
            finalizers: &[],
        })
        .expect("context init");
        let val = js_new_int32(&mut ctx, -5);
        assert_eq!(js_to_int32(&mut ctx, val), -5);

        let val = js_new_uint32(&mut ctx, 42);
        assert_eq!(js_to_uint32(&mut ctx, val), 42);

        let big = JSValue::JS_SHORTINT_MAX as i64 + 1;
        let val = js_new_int64(&mut ctx, big);
        assert_eq!(js_to_number(&mut ctx, val), big as f64);

        let val = js_new_float64(&mut ctx, 1.5);
        assert_eq!(js_to_number(&mut ctx, val), 1.5);
    }

    #[test]
    fn user_object_opaque_roundtrip() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 64 * 1024,
            prepare_compilation: true,
            finalizers: &[],
        })
        .expect("context init");
        let proto = ctx.class_proto()[JSObjectClass::Object as usize];
        let obj = ctx
            .alloc_object(JSObjectClass::Object, proto, size_of::<ObjectUserData>())
            .expect("user object");
        let obj_ptr = obj.to_ptr::<Object>().expect("object ptr");
        unsafe {
            // SAFETY: obj_ptr points at a valid object header word.
            let header_word = ptr::read_unaligned(obj_ptr.as_ptr().cast::<JSWord>());
            let header = ObjectHeader::from_word(header_word);
            let patched = ObjectHeader::new(JSObjectClass::User as u8, header.extra_size(), header.gc_mark());
            ptr::write_unaligned(obj_ptr.as_ptr().cast::<JSWord>(), patched.header().word());
        }
        assert!(obj.is_object());
        assert_eq!(js_get_class_id(obj), Some(JSObjectClass::User as u8));

        let mut value = 7u8;
        let ptr = core::ptr::addr_of_mut!(value).cast::<c_void>();
        assert!(js_set_opaque(obj, ptr));
        assert_eq!(js_get_opaque(obj), Some(ptr));
    }

    #[test]
    fn is_error_detects_error_object() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 64 * 1024,
            prepare_compilation: true,
            finalizers: &[],
        })
        .expect("context init");
        let _ = js_throw_type_error(&mut ctx, "boom");
        let err = ctx.current_exception();
        assert!(err.is_error());
        assert!(!JSValue::JS_NULL.is_error());
    }

    #[test]
    fn load_bytecode_registers_rom_table() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 64 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        assert_eq!(ctx.n_rom_atom_tables(), 1);

        let header_bytes = size_of::<BytecodeHeader>();
        let data_bytes = crate::containers::value_array_alloc_size(0);
        let total_bytes = header_bytes + data_bytes;
        let word_bytes = JSValue::JSW as usize;
        let word_len = (total_bytes + word_bytes - 1) / word_bytes;
        let mut storage = vec![0 as JSWord; word_len];
        let base = storage.as_mut_ptr().cast::<u8>();
        let data_ptr = unsafe { base.add(header_bytes) };
        let unique_strings = JSValue::from_ptr(NonNull::new(data_ptr).expect("bytecode table"));
        let header = BytecodeHeader {
            magic: crate::stdlib::stdlib_def::JS_BYTECODE_MAGIC,
            version: crate::stdlib::stdlib_def::JS_BYTECODE_VERSION,
            base_addr: data_ptr as usize,
            unique_strings,
            main_func: JSValue::JS_NULL,
        };
        unsafe {
            // SAFETY: base points to writable header bytes.
            ptr::write_unaligned(base.cast::<BytecodeHeader>(), header);
            // SAFETY: data_ptr points to writable value array header.
            let val_header = ValueArrayHeader::new(0, false);
            ptr::write_unaligned(data_ptr.cast::<JSWord>(), MbHeader::from(val_header).word());
        }
        let bytecode_bytes = unsafe { slice::from_raw_parts(base, total_bytes) };

        let main_func = js_load_bytecode(&mut ctx, bytecode_bytes);
        assert_eq!(main_func, JSValue::JS_NULL);
        assert_eq!(ctx.n_rom_atom_tables(), 2);
    }

    #[test]
    fn parse_bytecode_and_run() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 64 * 1024,
            prepare_compilation: true,
            finalizers: &[],
        })
        .expect("context init");
        let func = js_parse_bytecode(&mut ctx, b"1 + 2", crate::capi_defs::JS_EVAL_RETVAL);
        assert!(!func.is_exception());
        let result = js_run(&mut ctx, func);
        assert_eq!(js_to_number(&mut ctx, result), 3.0);
    }

    #[test]
    fn function_expression_cpool_contains_bytecode() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 64 * 1024,
            prepare_compilation: true,
            finalizers: &[],
        })
        .expect("context init");
        let func = js_parse_bytecode(
            &mut ctx,
            b"(function(){ globalThis.__timer = 7; })",
            crate::capi_defs::JS_EVAL_RETVAL,
        );
        assert!(!func.is_exception());
        let func_ptr = func.to_ptr::<FunctionBytecode>().expect("func ptr");
        let func_ref = unsafe {
            // SAFETY: func_ptr points at a function bytecode allocation.
            func_ptr.as_ref()
        };
        let byte_code = func_ref.byte_code();
        assert_ne!(byte_code, JSValue::JS_NULL);
        let byte_ptr = byte_code.to_ptr::<u8>().expect("bytecode ptr");
        let header_word = unsafe {
            // SAFETY: byte_ptr points at a readable header word.
            ptr::read_unaligned(byte_ptr.as_ptr().cast::<JSWord>())
        };
        let header = MbHeader::from_word(header_word);
        assert_eq!(header.tag(), MTag::ByteArray);
        let len = crate::containers::ByteArrayHeader::from(header).size() as usize;
        let bytecode = unsafe {
            // SAFETY: byte array header precedes bytecode payload.
            slice::from_raw_parts(byte_ptr.as_ptr().add(size_of::<JSWord>()), len)
        };
        let mut fclosure_idx = None;
        let mut pc = 0usize;
        while pc < bytecode.len() {
            let op = bytecode[pc] as usize;
            if op >= OPCODES.len() {
                break;
            }
            let info = OPCODES[op];
            let size = info.size as usize;
            if size == 0 || pc + size > bytecode.len() {
                break;
            }
            if info.name == "fclosure" {
                let idx = u16::from_le_bytes([bytecode[pc + 1], bytecode[pc + 2]]) as usize;
                fclosure_idx = Some(idx);
                break;
            }
            if bytecode[pc] == OP_FCLOSURE.as_u8() {
                let idx = u16::from_le_bytes([bytecode[pc + 1], bytecode[pc + 2]]) as usize;
                fclosure_idx = Some(idx);
                break;
            }
            pc += size;
        }
        let idx = fclosure_idx.expect("expected OP_FCLOSURE in bytecode");

        let cpool = func_ref.cpool();
        assert_ne!(cpool, JSValue::JS_NULL);
        let cpool_ptr = cpool.to_ptr::<u8>().expect("cpool ptr");
        let header_word = unsafe {
            // SAFETY: cpool_ptr points at a readable header word.
            ptr::read_unaligned(cpool_ptr.as_ptr().cast::<JSWord>())
        };
        let header = MbHeader::from_word(header_word);
        assert_eq!(header.tag(), MTag::ValueArray);
        let len = crate::containers::ValueArrayHeader::from(header).size() as usize;
        assert!(idx < len, "cpool index out of bounds");
        let entries_ptr = unsafe {
            // SAFETY: value array header precedes JSValue entries.
            cpool_ptr.as_ptr().add(size_of::<JSWord>()) as *const JSValue
        };
        let entry = unsafe {
            // SAFETY: entry_ptr points at a readable JSValue.
            ptr::read_unaligned(entries_ptr.add(idx))
        };
        let entry_mem_ptr = entry.to_ptr::<u8>().expect("cpool entry ptr");
        let entry_header_word = unsafe {
            // SAFETY: entry_ptr points at a readable header word.
            ptr::read_unaligned(entry_mem_ptr.as_ptr().cast::<JSWord>())
        };
        let entry_header = MbHeader::from_word(entry_header_word);
        let entry_tag = entry_header.tag();

        let mut function_idx = None;
        for i in 0..len {
            let entry = unsafe {
                // SAFETY: entry_ptr points at readable JSValue entries.
                ptr::read_unaligned(entries_ptr.add(i))
            };
            let Some(ptr) = entry.to_ptr::<u8>() else {
                continue;
            };
            let header_word = unsafe {
                // SAFETY: ptr points at a readable header word.
                ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
            };
            let header = MbHeader::from_word(header_word);
            if header.tag() == MTag::FunctionBytecode {
                function_idx = Some(i);
                break;
            }
        }
        assert_eq!(entry_tag, MTag::FunctionBytecode);
        assert_eq!(function_idx, Some(idx));
    }

    #[cfg(not(miri))]
    #[test]
    fn prepare_bytecode_roundtrip() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 128 * 1024,
            prepare_compilation: true,
            finalizers: &[],
        })
        .expect("context init");
        let func = js_parse_bytecode(&mut ctx, b"1 + 2", crate::capi_defs::JS_EVAL_RETVAL);
        let mut buf = js_prepare_bytecode(&mut ctx, func).expect("bytecode");
        assert!(js_is_bytecode(&buf));
        let header = unsafe {
            // SAFETY: buffer length covers header.
            ptr::read_unaligned(buf.as_ptr().cast::<BytecodeHeader>())
        };
        let base_addr = JSValue::JSW as usize;
        assert_eq!(header.base_addr, base_addr);
        let data_len = buf.len() - size_of::<BytecodeHeader>();
        let unique_addr = header
            .unique_strings
            .to_ptr::<u8>()
            .map(|ptr| ptr.as_ptr().addr())
            .unwrap_or_default();
        let main_addr = header
            .main_func
            .to_ptr::<u8>()
            .map(|ptr| ptr.as_ptr().addr())
            .unwrap_or_default();
        assert!(unique_addr >= base_addr);
        assert!(unique_addr < base_addr + data_len);
        assert!(main_addr >= base_addr);
        assert!(main_addr < base_addr + data_len);
        js_relocate_bytecode(&mut buf).expect("relocate");

        let mut ctx_run = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 128 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let main = js_load_bytecode(&mut ctx_run, &buf);
        let result = js_run(&mut ctx_run, main);
        assert_eq!(js_to_number(&mut ctx_run, result), 3.0);
    }

    #[cfg(target_pointer_width = "64")]
    #[test]
    fn prepare_bytecode_64to32_header() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 128 * 1024,
            prepare_compilation: true,
            finalizers: &[],
        })
        .expect("context init");
        let func = js_parse_bytecode(&mut ctx, b"1 + 2", crate::capi_defs::JS_EVAL_RETVAL);
        let buf = js_prepare_bytecode_64to32(&mut ctx, func).expect("bytecode");
        assert!(
            buf.len() >= size_of::<BytecodeHeader32>(),
            "bytecode buffer too small"
        );
        let header = unsafe {
            // SAFETY: buffer length covers header.
            ptr::read_unaligned(buf.as_ptr().cast::<BytecodeHeader32>())
        };
        assert_eq!(header.magic, crate::stdlib::stdlib_def::JS_BYTECODE_MAGIC);
        assert_eq!(header.version, crate::stdlib::stdlib_def::JS_BYTECODE_VERSION_32);
        assert_eq!(header.base_addr, 0);
    }

}
