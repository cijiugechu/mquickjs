use crate::context::{ContextError, JSContext};
use crate::cutils::float64_as_uint64;
use crate::dtoa::{js_atod, js_dtoa, AtodFlags, JS_ATOD_ACCEPT_BIN_OCT, JS_DTOA_FORMAT_FREE};
use crate::enums::JSObjectClass;
use crate::interpreter::{call_with_this, InterpreterError};
use crate::jsvalue::{
    is_bool, is_int, is_null, is_ptr, is_undefined, new_short_int, value_get_int,
    value_get_special_tag, value_get_special_value, value_to_ptr, JSValue, JSWord,
    JS_SHORTINT_MAX, JS_TAG_SHORT_FUNC, JS_TAG_STRING_CHAR,
};
#[cfg(target_pointer_width = "64")]
use crate::jsvalue::{is_short_float, short_float_to_f64};
use crate::memblock::{MbHeader, MTag};
use crate::object::{Object, ObjectHeader, PrimitiveValue};
use crate::property::{get_property, PropertyError};
use crate::string::runtime::string_view;
use core::mem::size_of;
use core::ptr;

#[derive(Copy, Clone, Debug)]
pub enum ToPrimitiveHint {
    String,
    Number,
    None,
}

#[derive(Debug)]
pub enum ConversionError {
    Context(ContextError),
    Property(PropertyError),
    Interpreter(InterpreterError),
    TypeError(&'static str),
}

impl From<ContextError> for ConversionError {
    fn from(err: ContextError) -> Self {
        ConversionError::Context(err)
    }
}

impl From<PropertyError> for ConversionError {
    fn from(err: PropertyError) -> Self {
        match err {
            PropertyError::Interpreter(err) => ConversionError::Interpreter(err),
            err => ConversionError::Property(err),
        }
    }
}

impl From<InterpreterError> for ConversionError {
    fn from(err: InterpreterError) -> Self {
        ConversionError::Interpreter(err)
    }
}

pub(crate) fn is_primitive(val: JSValue) -> bool {
    if !is_ptr(val) {
        return value_get_special_tag(val) != JS_TAG_SHORT_FUNC;
    }
    match mtag_from_value(val) {
        Some(MTag::Object) => false,
        Some(_) => true,
        None => true,
    }
}

pub(crate) fn is_object(val: JSValue) -> bool {
    matches!(mtag_from_value(val), Some(MTag::Object))
}

pub(crate) fn is_string(val: JSValue) -> bool {
    if !is_ptr(val) {
        return value_get_special_tag(val) == JS_TAG_STRING_CHAR;
    }
    matches!(mtag_from_value(val), Some(MTag::String))
}

pub(crate) fn is_number(val: JSValue) -> bool {
    if is_int(val) {
        return true;
    }
    #[cfg(target_pointer_width = "64")]
    if is_short_float(val) {
        return true;
    }
    matches!(mtag_from_value(val), Some(MTag::Float64))
}

pub(crate) fn is_function(val: JSValue) -> bool {
    if !is_ptr(val) {
        return value_get_special_tag(val) == JS_TAG_SHORT_FUNC;
    }
    let header = match object_header(val) {
        Some(header) => header,
        None => return false,
    };
    matches!(
        header.class_id(),
        class if class == JSObjectClass::Closure as u8 || class == JSObjectClass::CFunction as u8
    )
}

pub(crate) fn to_number(ctx: &mut JSContext, mut val: JSValue) -> Result<f64, ConversionError> {
    loop {
        if is_int(val) {
            return Ok(f64::from(value_get_int(val)));
        }
        #[cfg(target_pointer_width = "64")]
        if is_short_float(val) {
            return Ok(short_float_to_f64(val));
        }
        if is_ptr(val) {
            match mtag_from_value(val) {
                Some(MTag::String) => return Ok(to_number_from_string(val)),
                Some(MTag::Float64) => {
                    if let Some(num) = read_float64(val) {
                        return Ok(num);
                    }
                    return Ok(f64::NAN);
                }
                Some(MTag::Object) => {
                    val = to_primitive(ctx, val, ToPrimitiveHint::Number)?;
                    continue;
                }
                _ => return Ok(f64::NAN),
            }
        } else {
            match value_get_special_tag(val) {
                tag if tag == crate::jsvalue::JS_TAG_NULL => {
                    return Ok(f64::from(value_get_special_value(val)));
                }
                tag if tag == crate::jsvalue::JS_TAG_BOOL => {
                    return Ok(f64::from(value_get_special_value(val)));
                }
                tag if tag == crate::jsvalue::JS_TAG_UNDEFINED => return Ok(f64::NAN),
                tag if tag == JS_TAG_STRING_CHAR => return Ok(to_number_from_string(val)),
                _ => return Ok(f64::NAN),
            }
        }
    }
}

pub(crate) fn to_uint32(ctx: &mut JSContext, val: JSValue) -> Result<u32, ConversionError> {
    Ok(to_int32_internal(ctx, val, false)? as u32)
}

pub(crate) fn to_int32(ctx: &mut JSContext, val: JSValue) -> Result<i32, ConversionError> {
    to_int32_internal(ctx, val, false)
}

pub(crate) fn to_string(ctx: &mut JSContext, mut val: JSValue) -> Result<JSValue, ConversionError> {
    loop {
        if is_int(val) {
            let buf = value_get_int(val).to_string();
            return Ok(ctx.new_string(&buf)?);
        }
        #[cfg(target_pointer_width = "64")]
        if is_short_float(val) {
            return number_to_string(ctx, short_float_to_f64(val));
        }
        if is_ptr(val) {
            match mtag_from_value(val) {
                Some(MTag::Object) => {
                    val = to_primitive(ctx, val, ToPrimitiveHint::String)?;
                    continue;
                }
                Some(MTag::String) => return Ok(val),
                Some(MTag::Float64) => {
                    let num = read_float64(val).unwrap_or(f64::NAN);
                    return number_to_string(ctx, num);
                }
                Some(tag) => {
                    let buf = format!("[mtag {}]", tag as u8);
                    return Ok(ctx.new_string(&buf)?);
                }
                None => return Ok(ctx.new_string("?")?),
            }
        } else {
            match value_get_special_tag(val) {
                tag if tag == crate::jsvalue::JS_TAG_NULL => return Ok(ctx.new_string("null")?),
                tag if tag == crate::jsvalue::JS_TAG_UNDEFINED => {
                    return Ok(ctx.new_string("undefined")?)
                }
                tag if tag == crate::jsvalue::JS_TAG_BOOL => {
                    let literal = if value_get_special_value(val) != 0 {
                        "true"
                    } else {
                        "false"
                    };
                    return Ok(ctx.new_string(literal)?);
                }
                tag if tag == JS_TAG_STRING_CHAR => return Ok(val),
                tag if tag == JS_TAG_SHORT_FUNC => {
                    val = to_primitive(ctx, val, ToPrimitiveHint::String)?;
                }
                _ => return Ok(ctx.new_string("?")?),
            }
        }
    }
}

pub(crate) fn to_property_key(ctx: &mut JSContext, val: JSValue) -> Result<JSValue, ConversionError> {
    if is_int(val) {
        return Ok(val);
    }
    let str_val = to_string(ctx, val)?;
    if let Some(int_val) = parse_num_string(str_val) {
        return Ok(new_short_int(int_val));
    }
    Ok(ctx.atom_tables_mut().make_unique_string(str_val))
}

fn alloc_primitive_object(
    ctx: &mut JSContext,
    class_id: JSObjectClass,
    val: JSValue,
) -> Result<JSValue, ConversionError> {
    let proto = ctx.class_proto()[class_id as usize];
    let obj = ctx.alloc_object(class_id, proto, size_of::<PrimitiveValue>())?;
    let obj_ptr = value_to_ptr::<Object>(obj).ok_or(ConversionError::TypeError("object"))?;
    unsafe {
        // SAFETY: payload region holds a PrimitiveValue.
        let payload = Object::payload_ptr(obj_ptr.as_ptr());
        let primitive = core::ptr::addr_of_mut!((*payload).primitive);
        ptr::write_unaligned(PrimitiveValue::value_ptr(primitive), val);
    }
    Ok(obj)
}

#[allow(dead_code)]
pub(crate) fn to_object(ctx: &mut JSContext, val: JSValue) -> Result<JSValue, ConversionError> {
    if is_object(val) {
        return Ok(val);
    }
    if !is_ptr(val) && value_get_special_tag(val) == JS_TAG_SHORT_FUNC {
        return Ok(val);
    }
    if is_null(val) || is_undefined(val) {
        return Err(ConversionError::TypeError("null or undefined"));
    }
    if is_number(val) {
        return alloc_primitive_object(ctx, JSObjectClass::Number, val);
    }
    if is_bool(val) {
        return alloc_primitive_object(ctx, JSObjectClass::Boolean, val);
    }
    if is_string(val) {
        return alloc_primitive_object(ctx, JSObjectClass::String, val);
    }
    Err(ConversionError::TypeError("not an object"))
}

pub(crate) fn to_primitive(
    ctx: &mut JSContext,
    val: JSValue,
    hint: ToPrimitiveHint,
) -> Result<JSValue, ConversionError> {
    if is_primitive(val) {
        return Ok(val);
    }
    if let Some(primitive) = object_primitive_value(val) {
        return Ok(primitive);
    }
    let hint_index = match hint {
        ToPrimitiveHint::String => 0,
        ToPrimitiveHint::Number | ToPrimitiveHint::None => 1,
    };
    for i in 0..2 {
        let use_to_string = (i ^ hint_index) == 0;
        let atom: &[u8] = if use_to_string { b"toString" } else { b"valueOf" };
        let prop = ctx.intern_string(atom)?;
        let method = get_property_for_value(ctx, val, prop)?;
        if is_function(method) {
            let ret = call_with_this(ctx, method, val, &[])?;
            if !is_object(ret) {
                return Ok(ret);
            }
        }
    }
    Err(ConversionError::TypeError("toPrimitive"))
}

pub(crate) fn concat_strings(
    ctx: &mut JSContext,
    left: JSValue,
    right: JSValue,
) -> Result<JSValue, ConversionError> {
    let mut left_scratch = [0u8; 5];
    let mut right_scratch = [0u8; 5];
    let left_view = string_view(left, &mut left_scratch)
        .ok_or(ConversionError::TypeError("string"))?;
    let right_view = string_view(right, &mut right_scratch)
        .ok_or(ConversionError::TypeError("string"))?;
    let mut bytes = Vec::with_capacity(left_view.bytes().len() + right_view.bytes().len());
    bytes.extend_from_slice(left_view.bytes());
    bytes.extend_from_slice(right_view.bytes());
    Ok(ctx.new_string_len(&bytes)?)
}

fn get_property_for_value(
    ctx: &mut JSContext,
    val: JSValue,
    prop: JSValue,
) -> Result<JSValue, ConversionError> {
    if is_object(val) {
        return Ok(get_property(ctx, val, prop)?);
    }
    let proto = if is_number(val) {
        ctx.class_proto()[JSObjectClass::Number as usize]
    } else if is_bool(val) {
        ctx.class_proto()[JSObjectClass::Boolean as usize]
    } else if is_string(val) {
        ctx.class_proto()[JSObjectClass::String as usize]
    } else if value_get_special_tag(val) == JS_TAG_SHORT_FUNC {
        ctx.class_proto()[JSObjectClass::Closure as usize]
    } else if is_null(val) || is_undefined(val) {
        return Err(ConversionError::TypeError("null or undefined"));
    } else {
        return Err(ConversionError::TypeError("not an object"));
    };
    Ok(get_property(ctx, proto, prop)?)
}

fn mtag_from_value(val: JSValue) -> Option<MTag> {
    let ptr = value_to_ptr::<u8>(val)?;
    let header_word = unsafe {
        // SAFETY: ptr points to a readable memblock header.
        ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
    };
    Some(MbHeader::from_word(header_word).tag())
}

fn object_header(val: JSValue) -> Option<ObjectHeader> {
    let ptr = value_to_ptr::<Object>(val)?;
    let header_word = unsafe {
        // SAFETY: ptr points to a readable object header.
        ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
    };
    let header = ObjectHeader::from_word(header_word);
    if header.tag() != MTag::Object {
        return None;
    }
    Some(header)
}

fn object_primitive_value(val: JSValue) -> Option<JSValue> {
    let obj_ptr = value_to_ptr::<Object>(val)?;
    let header_word = unsafe {
        // SAFETY: obj_ptr points at a readable object header.
        ptr::read_unaligned(obj_ptr.as_ptr().cast::<JSWord>())
    };
    let header = ObjectHeader::from_word(header_word);
    if header.tag() != MTag::Object || header.extra_size() < 1 {
        return None;
    }
    match header.class_id() {
        c if c == JSObjectClass::Number as u8
            || c == JSObjectClass::Boolean as u8
            || c == JSObjectClass::String as u8 =>
        unsafe {
            // SAFETY: payload starts with PrimitiveValue for boxed primitives.
            let payload = Object::payload_ptr(obj_ptr.as_ptr());
            let primitive = core::ptr::addr_of_mut!((*payload).primitive);
            Some(ptr::read_unaligned(PrimitiveValue::value_ptr(primitive)))
        },
        _ => None,
    }
}

fn read_float64(val: JSValue) -> Option<f64> {
    let ptr = value_to_ptr::<u8>(val)?;
    let header_word = unsafe {
        // SAFETY: ptr points to a readable memblock header.
        ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
    };
    let header = MbHeader::from_word(header_word);
    if header.tag() != MTag::Float64 {
        return None;
    }
    let payload = unsafe {
        // SAFETY: payload follows the float64 header.
        ptr::read_unaligned(ptr.as_ptr().add(size_of::<JSWord>()) as *const f64)
    };
    Some(payload)
}

fn number_to_string(ctx: &mut JSContext, num: f64) -> Result<JSValue, ConversionError> {
    let out = js_dtoa(num, 10, 0, JS_DTOA_FORMAT_FREE).map_err(|_| {
        ConversionError::TypeError("number to string")
    })?;
    Ok(ctx.new_string(&out)?)
}

fn parse_num_string(val: JSValue) -> Option<i32> {
    let mut scratch = [0u8; 5];
    let view = string_view(val, &mut scratch)?;
    let bytes = view.bytes();
    if bytes.is_empty() || bytes.len() > 11 || !view.is_ascii() {
        return None;
    }
    let mut idx = 0usize;
    let mut is_neg = false;
    if bytes[0] == b'-' {
        if bytes.len() == 1 {
            return None;
        }
        is_neg = true;
        idx = 1;
    }
    let first = bytes.get(idx).copied()?;
    if !first.is_ascii_digit() {
        return None;
    }
    if first == b'0' {
        if idx + 1 != bytes.len() || is_neg {
            return None;
        }
        return Some(0);
    }
    let mut n: i64 = (first - b'0') as i64;
    let max = JS_SHORTINT_MAX as i64 + if is_neg { 1 } else { 0 };
    for &b in &bytes[idx + 1..] {
        if !b.is_ascii_digit() {
            return None;
        }
        n = n * 10 + (b - b'0') as i64;
        if n > max {
            return None;
        }
    }
    if is_neg {
        n = -n;
    }
    i32::try_from(n).ok()
}

fn to_number_from_string(val: JSValue) -> f64 {
    let mut scratch = [0u8; 5];
    let Some(view) = string_view(val, &mut scratch) else {
        return f64::NAN;
    };
    parse_number_bytes(view.bytes(), 0, JS_ATOD_ACCEPT_BIN_OCT, true)
}

fn parse_number_bytes(bytes: &[u8], radix: u32, flags: AtodFlags, to_string: bool) -> f64 {
    let start = skip_spaces(bytes, 0);
    if start >= bytes.len() {
        return if to_string { 0.0 } else { f64::NAN };
    }
    let input = match core::str::from_utf8(&bytes[start..]) {
        Ok(s) => s,
        Err(_) => return f64::NAN,
    };
    let parsed = match js_atod(input, radix, flags) {
        Ok(parsed) => parsed,
        Err(_) => return f64::NAN,
    };
    let next = start.saturating_add(parsed.next);
    if to_string {
        let rest = skip_spaces(bytes, next);
        if rest < bytes.len() {
            return f64::NAN;
        }
    }
    parsed.value
}

fn skip_spaces(bytes: &[u8], mut idx: usize) -> usize {
    while idx < bytes.len() {
        let c = bytes[idx];
        let is_space = matches!(c, b'\t' | b'\n' | b'\x0b' | b'\x0c' | b'\r' | b' ');
        if !is_space {
            break;
        }
        idx += 1;
    }
    idx
}

fn to_int32_internal(
    ctx: &mut JSContext,
    val: JSValue,
    sat_flag: bool,
) -> Result<i32, ConversionError> {
    let d = to_number(ctx, val)?;
    let u = float64_as_uint64(d);
    let e = ((u >> 52) & 0x7ff) as i32;
    if e <= (1023 + 30) {
        return Ok(d as i32);
    }
    if !sat_flag {
        if e <= (1023 + 30 + 53) {
            let mut v = (u & ((1u64 << 52) - 1)) | (1u64 << 52);
            v <<= (e - 1023) - 52 + 32;
            let mut ret = (v >> 32) as i32;
            if (u >> 63) != 0 {
                ret = -ret;
            }
            return Ok(ret);
        }
        return Ok(0);
    }
    if e == 2047 && (u & ((1u64 << 52) - 1)) != 0 {
        return Ok(0);
    }
    if (u >> 63) != 0 {
        return Ok(0x8000_0000u32 as i32);
    }
    Ok(0x7fff_ffff)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::{ContextConfig, JSContext};
    use crate::function_bytecode::{FunctionBytecodeFields, FunctionBytecodeHeader};
    use crate::jsvalue::{is_int, value_get_int, JS_NULL, JS_UNDEFINED};
    use crate::opcode::{OP_PUSH_1, OP_RETURN};
    use crate::property::define_property_value;
    use crate::stdlib::MQUICKJS_STDLIB_IMAGE;

    fn new_context() -> JSContext {
        JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 32 * 1024,
            prepare_compilation: false,
        })
        .expect("context init")
    }

    fn make_const_func(ctx: &mut JSContext, value_op: u8) -> JSValue {
        let bytecode = vec![value_op, OP_RETURN.0 as u8];
        let byte_code_val = ctx.alloc_byte_array(&bytecode).expect("bytecode");
        let header = FunctionBytecodeHeader::new(false, false, false, 0, false);
        let fields = FunctionBytecodeFields {
            func_name: JS_NULL,
            byte_code: byte_code_val,
            cpool: JS_NULL,
            vars: JS_NULL,
            ext_vars: JS_NULL,
            stack_size: 2,
            ext_vars_len: 0,
            filename: JS_NULL,
            pc2line: JS_NULL,
            source_pos: 0,
        };
        let func = ctx.alloc_function_bytecode(header, fields).expect("func");
        ctx.alloc_closure(func, 0).expect("closure")
    }

    #[test]
    fn to_number_parses_strings() {
        let mut ctx = new_context();
        let val = ctx.new_string("  123 ").expect("string");
        let num = to_number(&mut ctx, val).expect("number");
        assert_eq!(num, 123.0);

        let empty = ctx.new_string("   ").expect("string");
        let num = to_number(&mut ctx, empty).expect("number");
        assert_eq!(num, 0.0);

        let undef = to_number(&mut ctx, JS_UNDEFINED).expect("number");
        assert!(undef.is_nan());
    }

    #[test]
    fn to_property_key_parses_short_ints() {
        let mut ctx = new_context();
        let numeric = ctx.new_string("42").expect("string");
        let key = to_property_key(&mut ctx, numeric).expect("key");
        assert!(is_int(key));
        assert_eq!(value_get_int(key), 42);

        let neg_zero = ctx.new_string("-0").expect("string");
        let key = to_property_key(&mut ctx, neg_zero).expect("key");
        assert!(!is_int(key));
    }

    #[test]
    fn to_primitive_uses_value_of() {
        let mut ctx = new_context();
        let obj = ctx
            .alloc_object(JSObjectClass::Object, ctx.class_proto()[JSObjectClass::Object as usize], 0)
            .expect("object");
        let value_of = ctx.intern_string(b"valueOf").expect("atom");
        let func = make_const_func(&mut ctx, OP_PUSH_1.0 as u8);
        define_property_value(&mut ctx, obj, value_of, func).expect("define");
        let prim = to_primitive(&mut ctx, obj, ToPrimitiveHint::Number).expect("primitive");
        assert!(is_int(prim));
        assert_eq!(value_get_int(prim), 1);
    }
}
