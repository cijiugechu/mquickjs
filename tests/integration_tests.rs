//! Integration tests for mquickjs JavaScript evaluation.
//!
//! These tests validate the public API and built-in functions by evaluating
//! JavaScript code and checking the results.

use mquickjs::api::{
    js_eval, js_get_global_object, js_get_property_str, js_is_exception, js_is_number,
    js_is_object, js_is_string, js_to_number,
};
use mquickjs::capi_defs::JS_EVAL_JSON;
use mquickjs::context::{ContextConfig, JSContext};
use mquickjs::jsvalue::{is_bool, value_get_special_value, JS_EXCEPTION};
use mquickjs::stdlib::MQUICKJS_STDLIB_IMAGE;

fn new_context() -> JSContext {
    JSContext::new(ContextConfig {
        image: &MQUICKJS_STDLIB_IMAGE,
        memory_size: 256 * 1024,
        prepare_compilation: true,
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
    assert!(is_bool(result));
    assert_eq!(value_get_special_value(result), 1);
    
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

// ---------------------------------------------------------------------------
// Object Tests
// ---------------------------------------------------------------------------

#[test]
fn test_global_object_exists() {
    let ctx = new_context();
    let global = js_get_global_object(&ctx);
    assert!(js_is_object(global));
}

// ---------------------------------------------------------------------------
// Utility to evaluate JS and check for no exceptions
// (Kept for future use when bytecode evaluation is stable)
// ---------------------------------------------------------------------------

#[allow(dead_code)]
fn eval_js(ctx: &mut JSContext, code: &[u8]) -> mquickjs::jsvalue::JSValue {
    let result = js_eval(ctx, code, 0);
    if result == JS_EXCEPTION {
        panic!("JS evaluation failed for: {}", String::from_utf8_lossy(code));
    }
    result
}

#[allow(dead_code)]
fn eval_js_bool(ctx: &mut JSContext, code: &[u8]) -> bool {
    let result = eval_js(ctx, code);
    if is_bool(result) {
        value_get_special_value(result) != 0
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

// Note: More comprehensive tests can be added here once the bytecode
// evaluation is fully working. The tests above validate the JSON parsing
// and basic API functionality.

// ---------------------------------------------------------------------------
// TypedArray Tests (pending full bytecode evaluation)
// ---------------------------------------------------------------------------

// TypedArray JavaScript evaluation tests - IGNORED until bytecode evaluation is stable.
// The TypedArray builtins are implemented, but the full bytecode evaluation path 
// (particularly `new` operator with constructors) needs more work.
// See ROADMAP.md item: "Run complete mquickjs-c/tests/*.js test suite once bytecode evaluation is stable"

#[test]
#[ignore = "Pending bytecode evaluation stability - TypedArray builtins are implemented"]
fn test_typed_array_js_eval() {
    let mut ctx = new_context();
    
    // Simple test: create a Uint8Array and check its length
    let result = js_eval(&mut ctx, b"var a = new Uint8Array(4); a.length;", 0);
    assert!(!js_is_exception(result), "TypedArray creation failed");
    assert!(js_is_number(result));
    assert_eq!(js_to_number(&mut ctx, result), 4.0);
}

#[test]
#[ignore = "Pending bytecode evaluation stability"]
fn test_typed_array_element_access_js() {
    let mut ctx = new_context();
    
    // Test element assignment and read
    let result = js_eval(&mut ctx, b"var a = new Uint8Array(4); a[0] = 42; a[0];", 0);
    assert!(!js_is_exception(result), "TypedArray element access failed");
    assert!(js_is_number(result));
    assert_eq!(js_to_number(&mut ctx, result), 42.0);
}

#[test]
#[ignore = "Pending bytecode evaluation stability"]
fn test_typed_array_int8_overflow_js() {
    let mut ctx = new_context();
    
    // Test Int8Array overflow behavior (255 wraps to -1)
    let result = js_eval(&mut ctx, b"var a = new Int8Array(1); a[0] = 255; a[0];", 0);
    assert!(!js_is_exception(result), "Int8Array overflow test failed");
    assert!(js_is_number(result));
    assert_eq!(js_to_number(&mut ctx, result), -1.0);
}

#[test]
#[ignore = "Pending bytecode evaluation stability"]
fn test_array_buffer_js() {
    let mut ctx = new_context();
    
    // Test ArrayBuffer creation
    let result = js_eval(&mut ctx, b"var buf = new ArrayBuffer(16); buf.byteLength;", 0);
    assert!(!js_is_exception(result), "ArrayBuffer creation failed");
    assert!(js_is_number(result));
    assert_eq!(js_to_number(&mut ctx, result), 16.0);
}

#[test]
#[ignore = "Pending bytecode evaluation stability"]
fn test_typed_array_from_array_buffer_js() {
    let mut ctx = new_context();
    
    // Test creating TypedArray view on ArrayBuffer with offset
    let result = js_eval(
        &mut ctx,
        b"var buf = new ArrayBuffer(16); var a = new Uint32Array(buf, 12, 1); a[0] = 42; a[0];",
        0,
    );
    assert!(!js_is_exception(result), "TypedArray from ArrayBuffer failed");
    assert!(js_is_number(result));
    assert_eq!(js_to_number(&mut ctx, result), 42.0);
}

