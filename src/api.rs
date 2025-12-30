//! Public API entry points for JavaScript evaluation and execution.
//!
//! This module provides the main entry points for evaluating JavaScript code:
//! - `js_eval` - Parse and execute JavaScript source code
//! - `js_parse` - Parse JavaScript source code without execution
//! - `js_call` - Call a JavaScript function with arguments
//! - `js_run` - Execute parsed bytecode

use crate::bytecode::{is_bytecode, relocate_bytecode_in_place, BytecodeRelocError};
use crate::containers::ValueArrayHeader;
use crate::context::{BacktraceLocation, JSContext};
pub use crate::exception::JsFormatArg;
use crate::conversion;
use crate::enums::JSObjectClass;
use crate::interpreter::{call, call_with_this, call_with_this_flags, create_closure};
use crate::jsvalue::{
    new_short_int, value_to_ptr, JSValue, JSWord, JS_EXCEPTION, JS_NULL, JS_SHORTINT_MAX,
    JS_UNDEFINED,
};
use crate::memblock::{MbHeader, MTag};
use crate::object::{Object, ObjectHeader, ObjectUserData};
use crate::parser::entry::{parse_source, ParseError, ParseOutput};
use crate::parser::json::JsonValue;
use crate::parser::regexp::RegExpBytecode;
use crate::stdlib::stdlib_def::BytecodeHeader;
use crate::string::runtime::string_view;
use core::ffi::c_void;
use core::mem::size_of;
use core::ptr::{self, NonNull};
use core::slice;

/// Error type for API operations.
#[derive(Clone, Debug)]
pub enum ApiError {
    Parse(ParseError),
    Runtime(String),
    NotAFunction,
    NotABytecode,
}

impl From<ParseError> for ApiError {
    fn from(err: ParseError) -> Self {
        ApiError::Parse(err)
    }
}

const FRAME_CF_ARGC_MASK: i32 = 0xffff;
const N_ROM_ATOM_TABLES_MAX: u8 = 2;

struct CallDepth {
    ctx: *mut JSContext,
}

impl CallDepth {
    fn enter(ctx: &mut JSContext) -> Option<Self> {
        if !ctx.enter_call() {
            return None;
        }
        Some(Self { ctx })
    }
}

impl Drop for CallDepth {
    fn drop(&mut self) {
        unsafe {
            // SAFETY: ctx stays valid for the duration of the guard.
            (*self.ctx).exit_call();
        }
    }
}

/// Parse and execute JavaScript source code.
///
/// # Arguments
/// * `ctx` - The JavaScript context
/// * `source` - The JavaScript source code as bytes
/// * `eval_flags` - Evaluation flags (e.g., JS_EVAL_JSON, JS_EVAL_RETVAL)
///
/// # Returns
/// The result of evaluating the source code, or `JS_EXCEPTION` on error.
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
            if func_bytecode == JS_NULL {
                return Err(ApiError::NotABytecode);
            }
            // Create a closure from the bytecode (resolves external vars)
            let closure = create_closure(ctx, func_bytecode, None)
                .map_err(|e| ApiError::Runtime(format!("{:?}", e)))?;
            // Execute the closure
            let _guard = CallDepth::enter(ctx)
                .ok_or_else(|| ApiError::Runtime("C stack overflow".to_string()))?;
            call(ctx, closure, &[]).map_err(|e| ApiError::Runtime(format!("{:?}", e)))
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
/// The parsed bytecode as a function object, or `JS_EXCEPTION` on error.
pub fn js_parse(ctx: &mut JSContext, source: &[u8], eval_flags: u32) -> JSValue {
    match js_parse_internal(ctx, source, eval_flags) {
        Ok(val) => val,
        Err(err) => handle_api_error(ctx, err),
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
            if func_bytecode == JS_NULL {
                return Err(ApiError::NotABytecode);
            }
            // Return the closure without executing (resolves external vars)
            create_closure(ctx, func_bytecode, None)
                .map_err(|e| ApiError::Runtime(format!("{:?}", e)))
        }
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
/// The result of calling the function, or `JS_EXCEPTION` on error.
pub fn js_call(
    ctx: &mut JSContext,
    func: JSValue,
    this_obj: JSValue,
    args: &[JSValue],
) -> JSValue {
    if !func.is_function() {
        return ctx.throw_type_error("not a function");
    }
    let _guard = match CallDepth::enter(ctx) {
        Some(guard) => guard,
        None => return ctx.throw_internal_error("C stack overflow"),
    };
    match call_with_this(ctx, func, this_obj, args) {
        Ok(val) => val,
        Err(_) => JS_EXCEPTION,
    }
}

/// Execute a parsed function (closure).
///
/// # Arguments
/// * `ctx` - The JavaScript context
/// * `func` - The parsed function/closure to execute
///
/// # Returns
/// The result of executing the function, or `JS_EXCEPTION` on error.
pub fn js_run(ctx: &mut JSContext, func: JSValue) -> JSValue {
    if !func.is_function() {
        return ctx.throw_type_error("not a function");
    }
    let _guard = match CallDepth::enter(ctx) {
        Some(guard) => guard,
        None => return ctx.throw_internal_error("C stack overflow"),
    };
    match call(ctx, func, &[]) {
        Ok(val) => val,
        Err(_) => JS_EXCEPTION,
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
    let _guard = match CallDepth::enter(ctx) {
        Some(guard) => guard,
        None => return ctx.throw_internal_error("C stack overflow"),
    };
    let result = match call_with_this_flags(ctx, func, this_obj, args, call_flags) {
        Ok(val) => val,
        Err(_) => JS_EXCEPTION,
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
        Err(_) => return JS_EXCEPTION,
    };
    let key = match conversion::to_property_key(ctx, key) {
        Ok(k) => k,
        Err(_) => return JS_EXCEPTION,
    };
    match crate::property::get_property(ctx, obj, key) {
        Ok(val) => val,
        Err(_) => JS_EXCEPTION,
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
        Err(_) => return JS_EXCEPTION,
    };
    let key = match conversion::to_property_key(ctx, key) {
        Ok(k) => k,
        Err(_) => return JS_EXCEPTION,
    };
    match crate::property::set_property(ctx, obj, key, val) {
        Ok(()) => JS_UNDEFINED,
        Err(_) => JS_EXCEPTION,
    }
}

/// Get a property from an object by numeric index.
pub fn js_get_property_uint32(ctx: &mut JSContext, obj: JSValue, idx: u32) -> JSValue {
    if idx > JS_SHORTINT_MAX as u32 {
        return ctx.throw_range_error("invalid array index");
    }
    let key = new_short_int(idx as i32);
    match crate::property::get_property(ctx, obj, key) {
        Ok(val) => val,
        Err(_) => JS_EXCEPTION,
    }
}

/// Set a property on an object by numeric index.
pub fn js_set_property_uint32(
    ctx: &mut JSContext,
    obj: JSValue,
    idx: u32,
    val: JSValue,
) -> JSValue {
    if idx > JS_SHORTINT_MAX as u32 {
        return ctx.throw_range_error("invalid array index");
    }
    let key = new_short_int(idx as i32);
    match crate::property::set_property(ctx, obj, key, val) {
        Ok(()) => JS_UNDEFINED,
        Err(_) => JS_EXCEPTION,
    }
}

fn handle_api_error(ctx: &mut JSContext, err: ApiError) -> JSValue {
    handle_api_error_with_filename(ctx, err, "<input>")
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
    ctx.alloc_object_default().unwrap_or(JS_EXCEPTION)
}

/// Create a new float64 value.
pub fn js_new_float64(ctx: &mut JSContext, val: f64) -> JSValue {
    ctx.new_float64(val).unwrap_or(JS_EXCEPTION)
}

/// Create a new int64 value.
pub fn js_new_int64(ctx: &mut JSContext, val: i64) -> JSValue {
    ctx.new_int64(val).unwrap_or(JS_EXCEPTION)
}

/// Create a new int32 value.
pub fn js_new_int32(ctx: &mut JSContext, val: i32) -> JSValue {
    ctx.new_int32(val).unwrap_or(JS_EXCEPTION)
}

/// Create a new uint32 value.
pub fn js_new_uint32(ctx: &mut JSContext, val: u32) -> JSValue {
    ctx.new_uint32(val).unwrap_or(JS_EXCEPTION)
}

/// Create a new JavaScript array.
pub fn js_new_array(ctx: &mut JSContext, len: usize) -> JSValue {
    ctx.alloc_array(len).unwrap_or(JS_EXCEPTION)
}

/// Create a new user object for the specified class id.
pub fn js_new_object_class_user(ctx: &mut JSContext, class_id: u8) -> JSValue {
    if class_id < JSObjectClass::User as u8 {
        return ctx.throw_type_error("invalid user class id");
    }
    let proto = match ctx.class_proto().get(class_id as usize).copied() {
        Some(proto) => proto,
        None => return JS_EXCEPTION,
    };
    let extra = size_of::<crate::object::ObjectUserData>();
    ctx.alloc_object_class_id(class_id, proto, extra).unwrap_or(JS_EXCEPTION)
}

/// Create a new C function object with parameters.
pub fn js_new_cfunction_params(ctx: &mut JSContext, func_idx: u32, params: JSValue) -> JSValue {
    ctx.new_cfunction_params(func_idx, params).unwrap_or(JS_EXCEPTION)
}

/// Create a new JavaScript string.
pub fn js_new_string(ctx: &mut JSContext, s: &str) -> JSValue {
    ctx.new_string(s).unwrap_or(JS_EXCEPTION)
}

/// Create a new JavaScript string from raw bytes.
pub fn js_new_string_len(ctx: &mut JSContext, bytes: &[u8]) -> JSValue {
    ctx.new_string_len(bytes).unwrap_or(JS_EXCEPTION)
}

/// Convert a JavaScript value to string.
pub fn js_to_string(ctx: &mut JSContext, val: JSValue) -> JSValue {
    match conversion::to_string(ctx, val) {
        Ok(s) => s,
        Err(_) => JS_EXCEPTION,
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
        && stack != JS_NULL
        && let Some(bytes) = js_to_cstring_len(ctx, stack)
    {
        if !out.is_empty() {
            out.push('\n');
        }
        out.push_str(&String::from_utf8_lossy(&bytes));
    }
    out
}

/// Check if a value is undefined.
pub fn js_is_undefined(val: JSValue) -> bool {
    crate::jsvalue::is_undefined(val)
}

/// Check if a value is null.
pub fn js_is_null(val: JSValue) -> bool {
    crate::jsvalue::is_null(val)
}

/// Check if a value is a boolean.
pub fn js_is_bool(val: JSValue) -> bool {
    crate::jsvalue::is_bool(val)
}

/// Check if a value is a number.
pub fn js_is_number(val: JSValue) -> bool {
    val.is_number()
}

/// Check if a value is a string.
pub fn js_is_string(val: JSValue) -> bool {
    val.is_string()
}

/// Check if a value is an object.
pub fn js_is_object(val: JSValue) -> bool {
    val.is_object()
}

/// Check if a value is a function.
pub fn js_is_function(val: JSValue) -> bool {
    val.is_function()
}

/// Check if a value is an Error object.
pub fn js_is_error(val: JSValue) -> bool {
    matches!(
        js_get_class_id(val),
        Some(class_id) if class_id == JSObjectClass::Error as u8
    )
}

/// Check if a value is an exception marker.
pub fn js_is_exception(val: JSValue) -> bool {
    crate::jsvalue::is_exception(val)
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
    let ptr = value_to_ptr::<u8>(val)?;
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

fn object_ptr_and_header(val: JSValue) -> Option<(NonNull<Object>, ObjectHeader)> {
    let obj_ptr = value_to_ptr::<Object>(val)?;
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
        JsonValue::Null => JS_NULL,
        JsonValue::Bool(b) => crate::jsvalue::new_bool(b as i32),
        JsonValue::Number(n) => {
            // Check if the f64 can be represented as a short int
            let i = n as i64;
            if (i as f64) == n
                && i >= crate::jsvalue::JS_SHORTINT_MIN as i64
                && i <= crate::jsvalue::JS_SHORTINT_MAX as i64
            {
                return crate::jsvalue::new_short_int(i as i32);
            }
            ctx.new_float64(n).unwrap_or(JS_EXCEPTION)
        }
        JsonValue::String(s) => ctx.new_string_len(s.bytes()).unwrap_or(JS_EXCEPTION),
        JsonValue::Array(arr) => {
            let result = match ctx.alloc_array(arr.len()) {
                Ok(a) => a,
                Err(_) => return JS_EXCEPTION,
            };
            for (i, val) in arr.into_iter().enumerate() {
                let js_val = json_to_jsvalue(ctx, val);
                let _ = crate::property::set_property(
                    ctx,
                    result,
                    crate::jsvalue::new_short_int(i as i32),
                    js_val,
                );
            }
            result
        }
        JsonValue::Object(obj) => {
            let result = match ctx.alloc_object_default() {
                Ok(o) => o,
                Err(_) => return JS_EXCEPTION,
            };
            for (key, val) in obj {
                let key_val = match ctx.intern_string(key.bytes()) {
                    Ok(k) => k,
                    Err(_) => continue,
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
    use crate::stdlib::MQUICKJS_STDLIB_IMAGE;
    use crate::jsvalue::{value_from_ptr, JS_SHORTINT_MAX, JSW};

    fn new_context() -> JSContext {
        JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 64 * 1024,
            prepare_compilation: true,
            finalizers: &[],
        })
        .expect("context init")
    }

    fn new_context_no_compile() -> JSContext {
        JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 64 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init")
    }

    #[test]
    fn eval_json_null() {
        let mut ctx = new_context();
        let result = js_eval(&mut ctx, b"null", JS_EVAL_JSON);
        assert!(js_is_null(result));
    }

    #[test]
    fn eval_json_number() {
        let mut ctx = new_context();
        let result = js_eval(&mut ctx, b"42", JS_EVAL_JSON);
        assert!(js_is_number(result));
        assert_eq!(js_to_number(&mut ctx, result), 42.0);
    }

    #[test]
    fn eval_json_string() {
        let mut ctx = new_context();
        let result = js_eval(&mut ctx, b"\"hello\"", JS_EVAL_JSON);
        assert!(js_is_string(result));
    }

    #[test]
    fn eval_json_array() {
        let mut ctx = new_context();
        let result = js_eval(&mut ctx, b"[1, 2, 3]", JS_EVAL_JSON);
        assert!(js_is_object(result));
    }

    #[test]
    fn eval_json_object() {
        let mut ctx = new_context();
        let result = js_eval(&mut ctx, b"{\"a\": 1}", JS_EVAL_JSON);
        assert!(js_is_object(result));
    }

    #[test]
    fn new_object_and_property() {
        let mut ctx = new_context();
        let obj = js_new_object(&mut ctx);
        assert!(js_is_object(obj));

        let val = crate::jsvalue::new_short_int(42);
        js_set_property_str(&mut ctx, obj, "x", val);

        let result = js_get_property_str(&mut ctx, obj, "x");
        assert_eq!(js_to_number(&mut ctx, result), 42.0);
    }

    #[test]
    fn new_array() {
        let mut ctx = new_context();
        let arr = js_new_array(&mut ctx, 3);
        assert!(js_is_object(arr));
    }

    #[test]
    fn new_string() {
        let mut ctx = new_context();
        let s = js_new_string(&mut ctx, "hello");
        assert!(js_is_string(s));
    }

    #[test]
    fn global_object_exists() {
        let ctx = new_context();
        let global = js_get_global_object(&ctx);
        assert!(js_is_object(global));
    }

    #[test]
    fn new_numbers_roundtrip() {
        let mut ctx = new_context();
        let val = js_new_int32(&mut ctx, -5);
        assert_eq!(js_to_int32(&mut ctx, val), -5);

        let val = js_new_uint32(&mut ctx, 42);
        assert_eq!(js_to_uint32(&mut ctx, val), 42);

        let big = JS_SHORTINT_MAX as i64 + 1;
        let val = js_new_int64(&mut ctx, big);
        assert_eq!(js_to_number(&mut ctx, val), big as f64);

        let val = js_new_float64(&mut ctx, 1.5);
        assert_eq!(js_to_number(&mut ctx, val), 1.5);
    }

    #[test]
    fn user_object_opaque_roundtrip() {
        let mut ctx = new_context();
        let proto = ctx.class_proto()[JSObjectClass::Object as usize];
        let obj = ctx
            .alloc_object(JSObjectClass::Object, proto, size_of::<ObjectUserData>())
            .expect("user object");
        let obj_ptr = value_to_ptr::<Object>(obj).expect("object ptr");
        unsafe {
            // SAFETY: obj_ptr points at a valid object header word.
            let header_word = ptr::read_unaligned(obj_ptr.as_ptr().cast::<JSWord>());
            let header = ObjectHeader::from_word(header_word);
            let patched = ObjectHeader::new(JSObjectClass::User as u8, header.extra_size(), header.gc_mark());
            ptr::write_unaligned(obj_ptr.as_ptr().cast::<JSWord>(), patched.header().word());
        }
        assert!(js_is_object(obj));
        assert_eq!(js_get_class_id(obj), Some(JSObjectClass::User as u8));

        let mut value = 7u8;
        let ptr = core::ptr::addr_of_mut!(value).cast::<c_void>();
        assert!(js_set_opaque(obj, ptr));
        assert_eq!(js_get_opaque(obj), Some(ptr));
    }

    #[test]
    fn is_error_detects_error_object() {
        let mut ctx = new_context();
        let _ = js_throw_type_error(&mut ctx, "boom");
        let err = ctx.current_exception();
        assert!(js_is_error(err));
        assert!(!js_is_error(JS_NULL));
    }

    #[test]
    fn load_bytecode_registers_rom_table() {
        let mut ctx = new_context_no_compile();
        assert_eq!(ctx.n_rom_atom_tables(), 1);

        let header_bytes = size_of::<BytecodeHeader>();
        let data_bytes = crate::containers::value_array_alloc_size(0);
        let total_bytes = header_bytes + data_bytes;
        let word_bytes = JSW as usize;
        let word_len = (total_bytes + word_bytes - 1) / word_bytes;
        let mut storage = vec![0 as JSWord; word_len];
        let base = storage.as_mut_ptr().cast::<u8>();
        let data_ptr = unsafe { base.add(header_bytes) };
        let unique_strings = value_from_ptr(NonNull::new(data_ptr).expect("bytecode table"));
        let header = BytecodeHeader {
            magic: crate::stdlib::stdlib_def::JS_BYTECODE_MAGIC,
            version: crate::stdlib::stdlib_def::JS_BYTECODE_VERSION,
            base_addr: data_ptr as usize,
            unique_strings,
            main_func: JS_NULL,
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
        assert_eq!(main_func, JS_NULL);
        assert_eq!(ctx.n_rom_atom_tables(), 2);
    }

}
