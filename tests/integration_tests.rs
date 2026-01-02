//! Integration tests for mquickjs JavaScript evaluation.
//!
//! These tests validate the public API and built-in functions by evaluating
//! JavaScript code and checking the results.

use mquickjs::api::{
    js_eval, js_get_global_object, js_get_property_str, js_is_exception, js_is_number,
    js_is_object, js_is_string, js_to_number,
};
use std::ffi::c_void;
use std::fs;
use std::path::Path;
use std::slice;
use mquickjs::capi_defs::{JS_EVAL_JSON, JS_EVAL_RETVAL};
use mquickjs::context::{ContextConfig, JSContext};
use mquickjs::jsvalue::JSValue;
use mquickjs::stdlib::MQUICKJS_STDLIB_IMAGE;

fn new_context() -> JSContext {
    JSContext::new(ContextConfig {
        image: &MQUICKJS_STDLIB_IMAGE,
        memory_size: 256 * 1024,
        prepare_compilation: false,
        finalizers: &[],
    })
    .expect("context init")
}

// ---------------------------------------------------------------------------
// JSON Tests
// ---------------------------------------------------------------------------

#[test]
fn test_json_parse_basic() {
    let mut ctx = new_context();
    
    // Test JSON.parse with simple values
    let result = js_eval(&mut ctx, b"null", JS_EVAL_JSON);
    assert!(!js_is_exception(result));
    
    let result = js_eval(&mut ctx, b"true", JS_EVAL_JSON);
    assert!(!js_is_exception(result));
    assert!(result.is_bool());
    assert_eq!(result.get_special_value(), 1);
    
    let result = js_eval(&mut ctx, b"42", JS_EVAL_JSON);
    assert!(js_is_number(result));
    assert_eq!(js_to_number(&mut ctx, result), 42.0);
    
    let result = js_eval(&mut ctx, b"\"hello\"", JS_EVAL_JSON);
    assert!(js_is_string(result));
}

#[test]
fn test_json_parse_object() {
    let mut ctx = new_context();
    
    let result = js_eval(&mut ctx, b"{\"x\":1,\"y\":2}", JS_EVAL_JSON);
    assert!(!js_is_exception(result));
    assert!(js_is_object(result));
    
    // Access properties
    let x = js_get_property_str(&mut ctx, result, "x");
    assert!(js_is_number(x));
    assert_eq!(js_to_number(&mut ctx, x), 1.0);
    
    let y = js_get_property_str(&mut ctx, result, "y");
    assert!(js_is_number(y));
    assert_eq!(js_to_number(&mut ctx, y), 2.0);
}

#[test]
fn test_json_parse_array() {
    let mut ctx = new_context();
    
    let result = js_eval(&mut ctx, b"[1, 2, 3]", JS_EVAL_JSON);
    assert!(!js_is_exception(result));
    assert!(js_is_object(result));
}

#[test]
fn test_json_stringify_replacer_function() {
    let mut ctx = new_context();
    assert_js_true(
        &mut ctx,
        br#"var obj = JSON.parse('{"a":1,"b":2}'); var out = JSON.stringify(obj, function(){ if (arguments[0] === 'b') return 3; return arguments[1]; }); out === '{"a":1,"b":3}' || out === '{"b":3,"a":1}'"#,
    );
}

#[test]
fn test_json_stringify_replacer_array() {
    let mut ctx = new_context();
    assert_js_true(
        &mut ctx,
        br#"var obj = JSON.parse('{"a":1,"b":2,"c":3}'); var repl = JSON.parse('["b","a","b"]'); JSON.stringify(obj, repl) === '{"b":2,"a":1}'"#,
    );
}

#[test]
fn test_json_stringify_space_number() {
    let mut ctx = new_context();
    assert_js_true(
        &mut ctx,
        br#"var obj = JSON.parse('{"a":1,"b":{"c":2}}'); JSON.stringify(obj, null, 2) === "{\n  \"a\": 1,\n  \"b\": {\n    \"c\": 2\n  }\n}""#,
    );
}

#[test]
fn test_json_stringify_space_string() {
    let mut ctx = new_context();
    assert_js_true(
        &mut ctx,
        br#"var obj = JSON.parse('{"a":1}'); JSON.stringify(obj, null, "--") === "{\n--\"a\": 1\n}""#,
    );
}

#[test]
fn test_json_stringify_top_level_undefined() {
    let mut ctx = new_context();
    assert_js_true(&mut ctx, br#"JSON.stringify(undefined) === undefined"#);
}

// ---------------------------------------------------------------------------
// Object Tests
// ---------------------------------------------------------------------------

#[test]
fn test_global_object_exists() {
    let ctx = new_context();
    let global = js_get_global_object(&ctx);
    assert!(js_is_object(global));
}

#[test]
fn test_global_eval_builtin() {
    let mut ctx = new_context();

    assert_js_true(&mut ctx, br#"var g_eval = (1, eval); g_eval('1+1;') === 2"#);
    assert_js_true(&mut ctx, br#"var g_eval = (1, eval); g_eval('var z=2; z;') === 2"#);

    let z = js_eval(&mut ctx, b"z", JS_EVAL_RETVAL);
    assert!(!js_is_exception(z));
    assert_eq!(js_to_number(&mut ctx, z), 2.0);

    assert_js_true(&mut ctx, br#"var g_eval = (1, eval); g_eval('if (1) 2; else 3;') === 2"#);
    assert_js_true(&mut ctx, br#"var g_eval = (1, eval); g_eval('if (0) 2; else 3;') === 3"#);
    assert_js_true(&mut ctx, br#"var g_eval = (1, eval); z = 2; g_eval('z') === 2"#);
    assert_js_true(&mut ctx, br#"var g_eval = (1, eval); g_eval('z = 3'); z === 3"#);
    assert_js_true(&mut ctx, br#"var g_eval = (1, eval); g_eval(1) === 1"#);
}

// ---------------------------------------------------------------------------
// Utility to evaluate JS and check for no exceptions
// (Kept for future use when bytecode evaluation is stable)
// ---------------------------------------------------------------------------

#[allow(dead_code)]
fn eval_js(ctx: &mut JSContext, code: &[u8]) -> mquickjs::jsvalue::JSValue {
    let result = js_eval(ctx, code, 0);
    if result == JSValue::JS_EXCEPTION {
        panic!("JS evaluation failed for: {}", String::from_utf8_lossy(code));
    }
    result
}

#[allow(dead_code)]
fn eval_js_bool(ctx: &mut JSContext, code: &[u8]) -> bool {
    let result = eval_js(ctx, code);
    if result.is_bool() {
        result.get_special_value() != 0
    } else if js_is_number(result) {
        js_to_number(ctx, result) != 0.0
    } else {
        false
    }
}

#[allow(dead_code)]
fn eval_js_number(ctx: &mut JSContext, code: &[u8]) -> f64 {
    let result = eval_js(ctx, code);
    js_to_number(ctx, result)
}

fn assert_js_true(ctx: &mut JSContext, code: &[u8]) {
    let result = js_eval(ctx, code, JS_EVAL_RETVAL);
    assert!(!js_is_exception(result));
    assert!(result.is_bool());
    assert_eq!(result.get_special_value(), 1);
}

unsafe extern "C" fn test_write_func(opaque: *mut c_void, buf: *const c_void, buf_len: usize) {
    if opaque.is_null() || buf.is_null() {
        return;
    }
    let out = unsafe {
        // SAFETY: opaque is set to a valid Vec<u8> for the duration of the test.
        &mut *(opaque as *mut Vec<u8>)
    };
    let bytes = unsafe {
        // SAFETY: buf points at buf_len bytes per JSWriteFunc contract.
        slice::from_raw_parts(buf.cast::<u8>(), buf_len)
    };
    out.extend_from_slice(bytes);
}

// Note: More comprehensive tests can be added here once the bytecode
// evaluation is fully working. The tests above validate the JSON parsing
// and basic API functionality.

// ---------------------------------------------------------------------------
// TypedArray Tests (pending full bytecode evaluation)
// ---------------------------------------------------------------------------

// TypedArray JavaScript evaluation tests.

#[test]
fn test_typed_array_js_eval() {
    let mut ctx = new_context();
    // Simple test: create a Uint8Array and check its length
    let result = js_eval(
        &mut ctx,
        b"var a = new Uint8Array(4); a.length;",
        JS_EVAL_RETVAL,
    );
    assert!(!js_is_exception(result), "TypedArray creation failed");
    assert!(js_is_number(result));
    assert_eq!(js_to_number(&mut ctx, result), 4.0);
}

// ---------------------------------------------------------------------------
// mquickjs-c test script coverage
// ---------------------------------------------------------------------------

#[cfg(not(miri))]
#[test]
fn test_mandelbrot_js() {
    let mut output: Vec<u8> = Vec::new();
    let mut ctx = JSContext::new(ContextConfig {
        image: &MQUICKJS_STDLIB_IMAGE,
        memory_size: 4 * 1024 * 1024,
        prepare_compilation: false,
        finalizers: &[],
    })
    .expect("context init");
    ctx.set_opaque(&mut output as *mut _ as *mut c_void);
    ctx.set_log_func(Some(test_write_func));

    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("mquickjs-c/tests/mandelbrot.js");
    let code =
        fs::read(&path).expect("mandelbrot.js (run `git submodule update --init mquickjs-c`)");
    let source = String::from_utf8(code).expect("mandelbrot.js utf8");
    let mut script = Vec::with_capacity(source.len() + 128);
    script.extend_from_slice(
        b"var console = (typeof console === \"undefined\") ? { log: print } : console;\n",
    );
    script.extend_from_slice(source.as_bytes());
    let result = js_eval(&mut ctx, &script, 0);
    assert!(!js_is_exception(result));
    assert!(!output.is_empty());

    let line_count = output.iter().filter(|&&b| b == b'\n').count();
    assert_eq!(line_count, 25);
    assert!(output.windows(2).any(|w| w == [0x1b, b'[']));
    assert!(output.windows(3).any(|w| w == [0xE2, 0x96, 0x80]));

    ctx.set_log_func(None);
    ctx.set_opaque(core::ptr::null_mut());
}

#[test]
fn test_typed_array_element_access_js() {
    let mut ctx = new_context();
    
    // Test element assignment and read
    let result = js_eval(
        &mut ctx,
        b"var a = new Uint8Array(4); a[0] = 42; a[0];",
        JS_EVAL_RETVAL,
    );
    assert!(!js_is_exception(result), "TypedArray element access failed");
    assert!(js_is_number(result));
    assert_eq!(js_to_number(&mut ctx, result), 42.0);
}

#[test]
fn test_typed_array_int8_overflow_js() {
    let mut ctx = new_context();
    
    // Test Int8Array overflow behavior (255 wraps to -1)
    let result = js_eval(
        &mut ctx,
        b"var a = new Int8Array(1); a[0] = 255; a[0];",
        JS_EVAL_RETVAL,
    );
    assert!(!js_is_exception(result), "Int8Array overflow test failed");
    assert!(js_is_number(result));
    assert_eq!(js_to_number(&mut ctx, result), -1.0);
}

#[test]
fn test_array_buffer_js() {
    let mut ctx = new_context();
    
    // Test ArrayBuffer creation
    let result = js_eval(
        &mut ctx,
        b"var buf = new ArrayBuffer(16); buf.byteLength;",
        JS_EVAL_RETVAL,
    );
    assert!(!js_is_exception(result), "ArrayBuffer creation failed");
    assert!(js_is_number(result));
    assert_eq!(js_to_number(&mut ctx, result), 16.0);
}

#[test]
fn test_typed_array_from_array_buffer_js() {
    let mut ctx = new_context();
    
    // Test creating TypedArray view on ArrayBuffer with offset
    let result = js_eval(
        &mut ctx,
        b"var buf = new ArrayBuffer(16); var a = new Uint32Array(buf, 12, 1); a[0] = 42; a[0];",
        JS_EVAL_RETVAL,
    );
    assert!(!js_is_exception(result), "TypedArray from ArrayBuffer failed");
    assert!(js_is_number(result));
    assert_eq!(js_to_number(&mut ctx, result), 42.0);
}
