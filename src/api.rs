//! Public API entry points for JavaScript evaluation and execution.
//!
//! This module provides the main entry points for evaluating JavaScript code:
//! - `js_eval` - Parse and execute JavaScript source code
//! - `js_parse` - Parse JavaScript source code without execution
//! - `js_call` - Call a JavaScript function with arguments
//! - `js_run` - Execute parsed bytecode

use crate::context::{BacktraceLocation, JSContext};
pub use crate::exception::JsFormatArg;
use crate::conversion;
use crate::enums::JSObjectClass;
use crate::interpreter::{call, call_with_this, create_closure};
use crate::jsvalue::{JSValue, JS_EXCEPTION, JS_NULL, JS_UNDEFINED};
use crate::parser::entry::{parse_source, ParseError, ParseOutput};
use crate::parser::json::JsonValue;
use crate::parser::regexp::RegExpBytecode;

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
    match call(ctx, func, &[]) {
        Ok(val) => val,
        Err(_) => JS_EXCEPTION,
    }
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
    let key = match ctx.intern_string(name.as_bytes()) {
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
    let key = match ctx.intern_string(name.as_bytes()) {
        Ok(k) => k,
        Err(_) => return JS_EXCEPTION,
    };
    match crate::property::set_property(ctx, obj, key, val) {
        Ok(()) => JS_UNDEFINED,
        Err(_) => JS_EXCEPTION,
    }
}

fn handle_api_error(ctx: &mut JSContext, err: ApiError) -> JSValue {
    match err {
        ApiError::Parse(err) => {
            let val = ctx.throw_syntax_error(err.message());
            let location = BacktraceLocation {
                filename: "<input>",
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

/// Create a new JavaScript object.
pub fn js_new_object(ctx: &mut JSContext) -> JSValue {
    ctx.alloc_object_default().unwrap_or(JS_EXCEPTION)
}

/// Create a new JavaScript array.
pub fn js_new_array(ctx: &mut JSContext, len: usize) -> JSValue {
    ctx.alloc_array(len).unwrap_or(JS_EXCEPTION)
}

/// Create a new JavaScript string.
pub fn js_new_string(ctx: &mut JSContext, s: &str) -> JSValue {
    ctx.new_string(s).unwrap_or(JS_EXCEPTION)
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
    conversion::to_number(ctx, val).unwrap_or(f64::NAN)
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

/// Check if a value is an exception marker.
pub fn js_is_exception(val: JSValue) -> bool {
    crate::jsvalue::is_exception(val)
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

    fn new_context() -> JSContext {
        JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 64 * 1024,
            prepare_compilation: true,
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

}
