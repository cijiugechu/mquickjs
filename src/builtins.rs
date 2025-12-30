#![allow(non_snake_case)]

use crate::context::JSContext;
use crate::conversion;
use crate::cutils::{float64_as_uint64, get_u16, uint64_as_float64};
use crate::dtoa::{
    js_atod, js_dtoa, AtodFlags, DtoaFlags, JS_ATOD_ACCEPT_BIN_OCT, JS_ATOD_INT_ONLY,
    JS_DTOA_EXP_DISABLED, JS_DTOA_EXP_ENABLED, JS_DTOA_FORMAT_FIXED, JS_DTOA_FORMAT_FRAC,
    JS_DTOA_FORMAT_FREE,
};
use crate::enums::JSObjectClass;
use crate::js_libm;
use crate::jsvalue::{
    is_bool, is_int, is_null, is_ptr, is_undefined, new_bool, new_short_int, value_get_int,
    value_get_special_tag, value_get_special_value, value_to_ptr, JSValue, JSWord, JS_EXCEPTION,
    JS_NULL, JS_TAG_SHORT_FUNC, JS_TAG_STRING_CHAR, JS_UNDEFINED,
};
#[cfg(target_pointer_width = "64")]
use crate::jsvalue::{is_short_float, short_float_to_f64};
use crate::memblock::{MbHeader, MTag};
use crate::object::{Object, ObjectHeader, PrimitiveValue, RegExp};
use crate::parser::regexp_flags::{LRE_FLAG_GLOBAL, LRE_FLAG_STICKY};
use crate::property::{
    define_property_getset, define_property_value, find_own_property_exposed, get_property,
    has_property, object_keys,
};
use crate::regexp::{regexp_exec, RegExpError, RegExpExecMode};
use crate::string::runtime::string_view;
use core::mem::size_of;
use core::ptr::{self, NonNull};
use core::slice;
use std::time::{SystemTime, UNIX_EPOCH};


fn alloc_number(ctx: &mut JSContext, value: f64) -> JSValue {
    ctx.new_float64(value).unwrap_or(JS_EXCEPTION)
}

fn alloc_string(ctx: &mut JSContext, value: &str) -> JSValue {
    ctx.new_string(value).unwrap_or(JS_EXCEPTION)
}

fn read_float64(val: JSValue) -> Option<f64> {
    let ptr = value_to_ptr::<u8>(val)?;
    let header_word = unsafe {
        // SAFETY: ptr points at a readable memblock header.
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

fn js_to_bool(val: JSValue) -> bool {
    if is_int(val) {
        return value_get_int(val) != 0;
    }
    #[cfg(target_pointer_width = "64")]
    if is_short_float(val) {
        let num = short_float_to_f64(val);
        return !num.is_nan() && num != 0.0;
    }
    if is_bool(val) {
        return value_get_special_value(val) != 0;
    }
    if is_null(val) || is_undefined(val) {
        return false;
    }
    if value_get_special_tag(val) == JS_TAG_SHORT_FUNC {
        return true;
    }
    let mut scratch = [0u8; 5];
    if let Some(view) = string_view(val, &mut scratch) {
        return !view.bytes().is_empty();
    }
    if let Some(num) = read_float64(val) {
        return !num.is_nan() && num != 0.0;
    }
    if is_ptr(val) {
        return true;
    }
    false
}

fn to_number_from_string(val: JSValue, flags: AtodFlags, to_string: bool) -> f64 {
    let mut scratch = [0u8; 5];
    let Some(view) = string_view(val, &mut scratch) else {
        return f64::NAN;
    };
    parse_number_bytes(view.bytes(), 0, flags, to_string)
}

fn to_number(val: JSValue) -> f64 {
    if is_int(val) {
        return f64::from(value_get_int(val));
    }
    #[cfg(target_pointer_width = "64")]
    if is_short_float(val) {
        return short_float_to_f64(val);
    }
    if value_get_special_tag(val) == JS_TAG_STRING_CHAR {
        return to_number_from_string(val, JS_ATOD_ACCEPT_BIN_OCT, true);
    }
    if is_ptr(val) {
        if let Some(value) = read_float64(val) {
            return value;
        }
        let mut scratch = [0u8; 5];
        if string_view(val, &mut scratch).is_some() {
            return to_number_from_string(val, JS_ATOD_ACCEPT_BIN_OCT, true);
        }
    }
    if is_bool(val) || is_null(val) {
        return f64::from(value_get_special_value(val));
    }
    if is_undefined(val) {
        return f64::NAN;
    }
    f64::NAN
}

fn to_int32_internal(val: JSValue, sat_flag: bool) -> i32 {
    let d = to_number(val);
    let u = float64_as_uint64(d);
    let e = ((u >> 52) & 0x7ff) as i32;
    if e <= (1023 + 30) {
        d as i32
    } else if !sat_flag {
        if e <= (1023 + 30 + 53) {
            let mut v = (u & (((1u64) << 52) - 1)) | ((1u64) << 52);
            v <<= (e - 1023) - 52 + 32;
            let mut ret = (v >> 32) as i32;
            if (u >> 63) != 0 {
                ret = -ret;
            }
            ret
        } else {
            0
        }
    } else if e == 2047 && (u & (((1u64) << 52) - 1)) != 0 {
        0
    } else if (u >> 63) != 0 {
        0x8000_0000u32 as i32
    } else {
        0x7fff_ffff
    }
}

fn to_int32(val: JSValue) -> i32 {
    to_int32_internal(val, false)
}

fn to_uint32(val: JSValue) -> u32 {
    to_int32_internal(val, false) as u32
}

fn to_int32_sat(val: JSValue) -> i32 {
    to_int32_internal(val, true)
}

fn to_string_value(ctx: &mut JSContext, val: JSValue) -> JSValue {
    let mut scratch = [0u8; 5];
    if string_view(val, &mut scratch).is_some() {
        return val;
    }
    let mut is_number = is_int(val) || read_float64(val).is_some();
    #[cfg(target_pointer_width = "64")]
    {
        if is_short_float(val) {
            is_number = true;
        }
    }
    if is_number {
        let d = to_number(val);
        let out = match js_dtoa(d, 10, 0, JS_DTOA_FORMAT_FREE) {
            Ok(out) => out,
            Err(_) => return JS_EXCEPTION,
        };
        return alloc_string(ctx, &out);
    }
    if is_bool(val) {
        if value_get_special_value(val) != 0 {
            return alloc_string(ctx, "true");
        }
        return alloc_string(ctx, "false");
    }
    if is_null(val) {
        return alloc_string(ctx, "null");
    }
    if is_undefined(val) {
        return alloc_string(ctx, "undefined");
    }
    JS_EXCEPTION
}

fn dtoa_to_string(ctx: &mut JSContext, d: f64, radix: u32, n_digits: usize, flags: DtoaFlags) -> JSValue {
    let out = match js_dtoa(d, radix, n_digits, flags) {
        Ok(out) => out,
        Err(_) => return JS_EXCEPTION,
    };
    alloc_string(ctx, &out)
}

fn js_fmin(a: f64, b: f64) -> f64 {
    if a == 0.0 && b == 0.0 {
        let bits = float64_as_uint64(a) | float64_as_uint64(b);
        return uint64_as_float64(bits);
    }
    if a <= b {
        a
    } else {
        b
    }
}

fn js_fmax(a: f64, b: f64) -> f64 {
    if a == 0.0 && b == 0.0 {
        let bits = float64_as_uint64(a) & float64_as_uint64(b);
        return uint64_as_float64(bits);
    }
    if a >= b {
        a
    } else {
        b
    }
}

pub fn js_number_constructor(ctx: &mut JSContext, _this_val: JSValue, args: &[JSValue]) -> JSValue {
    if args.is_empty() {
        return new_short_int(0);
    }
    let d = to_number(args[0]);
    alloc_number(ctx, d)
}

pub fn js_number_toString(ctx: &mut JSContext, this_val: JSValue, args: &[JSValue]) -> JSValue {
    if !is_number_value(this_val) {
        return JS_EXCEPTION;
    }
    let d = to_number(this_val);
    let radix = if args.is_empty() || is_undefined(args[0]) {
        10
    } else {
        to_int32_sat(args[0])
    };
    if !(2..=36).contains(&radix) {
        return JS_EXCEPTION;
    }
    let mut flags = JS_DTOA_FORMAT_FREE;
    if radix != 10 {
        flags |= JS_DTOA_EXP_DISABLED;
    }
    dtoa_to_string(ctx, d, radix as u32, 0, flags)
}

pub fn js_number_toFixed(ctx: &mut JSContext, this_val: JSValue, args: &[JSValue]) -> JSValue {
    if !is_number_value(this_val) {
        return JS_EXCEPTION;
    }
    let d = to_number(this_val);
    let f = if args.is_empty() { 0 } else { to_int32_sat(args[0]) };
    if !(0..=100).contains(&f) {
        return JS_EXCEPTION;
    }
    let flags = if d.abs() >= 1e21 {
        JS_DTOA_FORMAT_FREE
    } else {
        JS_DTOA_FORMAT_FRAC
    };
    dtoa_to_string(ctx, d, 10, f as usize, flags)
}

pub fn js_number_toExponential(
    ctx: &mut JSContext,
    this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    if !is_number_value(this_val) {
        return JS_EXCEPTION;
    }
    let d = to_number(this_val);
    let mut f = if args.is_empty() { 0 } else { to_int32_sat(args[0]) };
    let flags = if args.is_empty() || !d.is_finite() {
        f = 0;
        JS_DTOA_FORMAT_FREE
    } else {
        if !(0..=100).contains(&f) {
            return JS_EXCEPTION;
        }
        f += 1;
        JS_DTOA_FORMAT_FIXED
    };
    dtoa_to_string(ctx, d, 10, f as usize, flags | JS_DTOA_EXP_ENABLED)
}

pub fn js_number_toPrecision(
    ctx: &mut JSContext,
    this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    if !is_number_value(this_val) {
        return JS_EXCEPTION;
    }
    let d = to_number(this_val);
    let (p, flags) = if args.is_empty() || is_undefined(args[0]) {
        (0, JS_DTOA_FORMAT_FREE)
    } else {
        let p = to_int32_sat(args[0]);
        if !d.is_finite() {
            (p, JS_DTOA_FORMAT_FREE)
        } else {
            if !(1..=100).contains(&p) {
                return JS_EXCEPTION;
            }
            (p, JS_DTOA_FORMAT_FIXED)
        }
    };
    dtoa_to_string(ctx, d, 10, p as usize, flags)
}

pub fn js_number_parseInt(ctx: &mut JSContext, _this_val: JSValue, args: &[JSValue]) -> JSValue {
    let input = args.first().copied().unwrap_or(JS_UNDEFINED);
    let radix_val = args.get(1).copied().unwrap_or(JS_UNDEFINED);
    let str_val = to_string_value(ctx, input);
    if str_val == JS_EXCEPTION {
        return JS_EXCEPTION;
    }
    let radix = to_int32(radix_val);
    let value = if radix != 0 && !(2..=36).contains(&radix) {
        f64::NAN
    } else {
        let mut scratch = [0u8; 5];
        let Some(view) = string_view(str_val, &mut scratch) else {
            return JS_EXCEPTION;
        };
        parse_number_bytes(view.bytes(), radix as u32, JS_ATOD_INT_ONLY, false)
    };
    alloc_number(ctx, value)
}

pub fn js_number_parseFloat(ctx: &mut JSContext, _this_val: JSValue, args: &[JSValue]) -> JSValue {
    let input = args.first().copied().unwrap_or(JS_UNDEFINED);
    let str_val = to_string_value(ctx, input);
    if str_val == JS_EXCEPTION {
        return JS_EXCEPTION;
    }
    let mut scratch = [0u8; 5];
    let Some(view) = string_view(str_val, &mut scratch) else {
        return JS_EXCEPTION;
    };
    let value = parse_number_bytes(view.bytes(), 10, AtodFlags::empty(), false);
    alloc_number(ctx, value)
}

pub fn js_boolean_constructor(
    _ctx: &mut JSContext,
    _this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    let val = args.first().copied().unwrap_or(JS_UNDEFINED);
    new_bool(js_to_bool(val) as i32)
}

pub fn js_math_min_max(
    ctx: &mut JSContext,
    _this_val: JSValue,
    args: &[JSValue],
    magic: i32,
) -> JSValue {
    let is_max = magic != 0;
    if args.is_empty() {
        return alloc_number(ctx, if is_max { f64::NEG_INFINITY } else { f64::INFINITY });
    }
    if args.iter().all(|val| is_int(*val)) {
        let mut r = value_get_int(args[0]);
        for val in &args[1..] {
            let a = value_get_int(*val);
            if is_max {
                r = r.max(a);
            } else {
                r = r.min(a);
            }
        }
        return new_short_int(r);
    }
    let mut r = to_number(args[0]);
    for val in &args[1..] {
        let a = to_number(*val);
        if !r.is_nan() {
            if a.is_nan() {
                r = a;
            } else if is_max {
                r = js_fmax(r, a);
            } else {
                r = js_fmin(r, a);
            }
        }
    }
    alloc_number(ctx, r)
}

pub fn js_math_sign(a: f64) -> f64 {
    if a.is_nan() || a == 0.0 {
        return a;
    }
    if a < 0.0 {
        -1.0
    } else {
        1.0
    }
}

pub fn js_math_fround(a: f64) -> f64 {
    (a as f32) as f64
}

pub fn js_math_imul(ctx: &mut JSContext, _this_val: JSValue, args: &[JSValue]) -> JSValue {
    let a = to_int32(args.first().copied().unwrap_or(JS_UNDEFINED));
    let b = to_int32(args.get(1).copied().unwrap_or(JS_UNDEFINED));
    let out = (a as u32).wrapping_mul(b as u32) as i32;
    alloc_number(ctx, out as f64)
}

pub fn js_math_clz32(_ctx: &mut JSContext, _this_val: JSValue, args: &[JSValue]) -> JSValue {
    let a = to_uint32(args.first().copied().unwrap_or(JS_UNDEFINED));
    let out = if a == 0 { 32 } else { a.leading_zeros() as i32 };
    new_short_int(out)
}

pub fn js_math_atan2(ctx: &mut JSContext, _this_val: JSValue, args: &[JSValue]) -> JSValue {
    let y = to_number(args.first().copied().unwrap_or(JS_UNDEFINED));
    let x = to_number(args.get(1).copied().unwrap_or(JS_UNDEFINED));
    alloc_number(ctx, js_libm::js_atan2(y, x))
}

pub fn js_math_pow(ctx: &mut JSContext, _this_val: JSValue, args: &[JSValue]) -> JSValue {
    let x = to_number(args.first().copied().unwrap_or(JS_UNDEFINED));
    let y = to_number(args.get(1).copied().unwrap_or(JS_UNDEFINED));
    alloc_number(ctx, js_libm::js_pow(x, y))
}

pub fn js_math_random(ctx: &mut JSContext, _this_val: JSValue, _args: &[JSValue]) -> JSValue {
    let v = ctx.next_random_u64();
    let d = uint64_as_float64(((0x3ffu64) << 52) | (v >> 12)) - 1.0;
    alloc_number(ctx, d)
}

pub fn js_date_now(ctx: &mut JSContext, _this_val: JSValue, _args: &[JSValue]) -> JSValue {
    let now = SystemTime::now();
    let Ok(duration) = now.duration_since(UNIX_EPOCH) else {
        return JS_EXCEPTION;
    };
    let ms = duration.as_secs() as f64 * 1000.0 + f64::from(duration.subsec_millis());
    alloc_number(ctx, ms)
}

pub fn js_date_constructor(ctx: &mut JSContext, _this_val: JSValue, _args: &[JSValue]) -> JSValue {
    ctx.throw_type_error("only Date.now() is supported")
}

pub fn js_global_eval(ctx: &mut JSContext, _this_val: JSValue, args: &[JSValue]) -> JSValue {
    let val = args.first().copied().unwrap_or(JS_UNDEFINED);
    if !val.is_string() {
        return val;
    }
    let mut scratch = [0u8; 5];
    let Some(view) = string_view(val, &mut scratch) else {
        return JS_EXCEPTION;
    };
    crate::api::js_eval(ctx, view.bytes(), crate::capi_defs::JS_EVAL_RETVAL)
}

pub fn js_global_isNaN(_ctx: &mut JSContext, _this_val: JSValue, args: &[JSValue]) -> JSValue {
    let val = args.first().copied().unwrap_or(JS_UNDEFINED);
    let num = to_number(val);
    new_bool(num.is_nan() as i32)
}

pub fn js_global_isFinite(_ctx: &mut JSContext, _this_val: JSValue, args: &[JSValue]) -> JSValue {
    let val = args.first().copied().unwrap_or(JS_UNDEFINED);
    let num = to_number(val);
    new_bool(num.is_finite() as i32)
}

fn is_number_value(val: JSValue) -> bool {
    if is_int(val) {
        return true;
    }
    #[cfg(target_pointer_width = "64")]
    if is_short_float(val) {
        return true;
    }
    if is_ptr(val) {
        let ptr = match value_to_ptr::<u8>(val) {
            Some(ptr) => ptr,
            None => return false,
        };
        let header_word = unsafe {
            // SAFETY: ptr points at a readable memblock header.
            ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
        };
        let header = MbHeader::from_word(header_word);
        if header.tag() == MTag::Float64 {
            return true;
        }
        if header.tag() == MTag::Object {
            let obj_header = ObjectHeader::from_word(header_word);
            return obj_header.class_id() == JSObjectClass::Number as u8
                && obj_header.extra_size() >= 1;
        }
    }
    false
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

// ---------------------------------------------------------------------------
// Object builtins
// ---------------------------------------------------------------------------

pub fn js_object_constructor(
    ctx: &mut JSContext,
    _this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    if args.is_empty() {
        ctx.alloc_object_default().unwrap_or(JS_EXCEPTION)
    } else {
        args[0]
    }
}

pub fn js_object_hasOwnProperty(
    ctx: &mut JSContext,
    this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    if !this_val.is_object() {
        return JS_EXCEPTION;
    }
    let prop = match conversion::to_property_key(ctx, args.first().copied().unwrap_or(JS_UNDEFINED))
    {
        Ok(p) => p,
        Err(_) => return JS_EXCEPTION,
    };
    match find_own_property_exposed(ctx, this_val, prop) {
        Ok(found) => new_bool(found as i32),
        Err(_) => JS_EXCEPTION,
    }
}

pub fn js_object_toString(
    _ctx: &mut JSContext,
    this_val: JSValue,
    _args: &[JSValue],
) -> JSValue {
    object_to_string_internal(_ctx, this_val)
}

fn object_to_string_internal(ctx: &mut JSContext, val: JSValue) -> JSValue {
    let type_name = if is_int(val) {
        "Number"
    } else {
        #[cfg(target_pointer_width = "64")]
        if is_short_float(val) {
            return alloc_string(ctx, "[object Number]");
        }
        if !is_ptr(val) {
            match value_get_special_tag(val) {
                tag if tag == crate::jsvalue::JS_TAG_NULL => "Null",
                tag if tag == crate::jsvalue::JS_TAG_UNDEFINED => "Undefined",
                tag if tag == JS_TAG_SHORT_FUNC => "Function",
                tag if tag == crate::jsvalue::JS_TAG_BOOL => "Boolean",
                tag if tag == JS_TAG_STRING_CHAR => "String",
                _ => "Object",
            }
        } else {
            let ptr = match value_to_ptr::<u8>(val) {
                Some(p) => p,
                None => return alloc_string(ctx, "[object Object]"),
            };
            let header_word = unsafe { ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>()) };
            let header = MbHeader::from_word(header_word);
            match header.tag() {
                MTag::Object => {
                    let obj_header = ObjectHeader::from_word(header_word);
                    match obj_header.class_id() {
                        c if c == JSObjectClass::Array as u8 => "Array",
                        c if c == JSObjectClass::Error as u8 => "Error",
                        c if c == JSObjectClass::Closure as u8 => "Function",
                        c if c == JSObjectClass::CFunction as u8 => "Function",
                        c if c == JSObjectClass::Number as u8 => "Number",
                        c if c == JSObjectClass::Boolean as u8 => "Boolean",
                        c if c == JSObjectClass::String as u8 => "String",
                        c if c == JSObjectClass::RegExp as u8 => "RegExp",
                        c if c == JSObjectClass::Date as u8 => "Date",
                        _ => "Object",
                    }
                }
                MTag::String => "String",
                MTag::Float64 => "Number",
                _ => "Object",
            }
        }
    };
    let buf = format!("[object {}]", type_name);
    alloc_string(ctx, &buf)
}

pub fn js_object_defineProperty(
    ctx: &mut JSContext,
    _this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    let obj = args.first().copied().unwrap_or(JS_UNDEFINED);
    let prop_arg = args.get(1).copied().unwrap_or(JS_UNDEFINED);
    let desc = args.get(2).copied().unwrap_or(JS_UNDEFINED);

    if !obj.is_object() {
        return JS_EXCEPTION;
    }
    let prop = match conversion::to_property_key(ctx, prop_arg) {
        Ok(p) => p,
        Err(_) => return JS_EXCEPTION,
    };

    // Check for value, get, set properties in descriptor.
    let value_key = match ctx.intern_string(b"value") {
        Ok(k) => k,
        Err(_) => return JS_EXCEPTION,
    };
    let get_key = match ctx.intern_string(b"get") {
        Ok(k) => k,
        Err(_) => return JS_EXCEPTION,
    };
    let set_key = match ctx.intern_string(b"set") {
        Ok(k) => k,
        Err(_) => return JS_EXCEPTION,
    };

    let has_value = has_property(ctx, desc, value_key).unwrap_or(false);
    let has_get = has_property(ctx, desc, get_key).unwrap_or(false);
    let has_set = has_property(ctx, desc, set_key).unwrap_or(false);

    if !has_value && !has_get && !has_set {
        return JS_EXCEPTION;
    }

    if has_value {
        let val = match get_property(ctx, desc, value_key) {
            Ok(v) => v,
            Err(_) => return JS_EXCEPTION,
        };
        if define_property_value(ctx, obj, prop, val).is_err() {
            return JS_EXCEPTION;
        }
    } else {
        let getter = if has_get {
            match get_property(ctx, desc, get_key) {
                Ok(v) => v,
                Err(_) => return JS_EXCEPTION,
            }
        } else {
            JS_UNDEFINED
        };
        let setter = if has_set {
            match get_property(ctx, desc, set_key) {
                Ok(v) => v,
                Err(_) => return JS_EXCEPTION,
            }
        } else {
            JS_UNDEFINED
        };
        if define_property_getset(ctx, obj, prop, getter, setter).is_err() {
            return JS_EXCEPTION;
        }
    }
    obj
}

pub fn js_object_getPrototypeOf(
    _ctx: &mut JSContext,
    _this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    let obj = args.first().copied().unwrap_or(JS_UNDEFINED);
    if !obj.is_object() {
        return JS_EXCEPTION;
    }
    let obj_ptr = match value_to_ptr::<Object>(obj) {
        Some(p) => p,
        None => return JS_EXCEPTION,
    };
    unsafe { ptr::read_unaligned(Object::proto_ptr(obj_ptr.as_ptr())) }
}

pub fn js_object_setPrototypeOf(
    ctx: &mut JSContext,
    _this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    let obj = args.first().copied().unwrap_or(JS_UNDEFINED);
    let proto = args.get(1).copied().unwrap_or(JS_UNDEFINED);

    if !obj.is_object() {
        return JS_EXCEPTION;
    }
    if proto != JS_NULL && !proto.is_object() {
        return JS_EXCEPTION;
    }

    if set_prototype_internal(ctx, obj, proto).is_err() {
        return JS_EXCEPTION;
    }
    obj
}

fn set_prototype_internal(
    _ctx: &mut JSContext,
    obj: JSValue,
    proto: JSValue,
) -> Result<(), ()> {
    let obj_ptr = value_to_ptr::<Object>(obj).ok_or(())?;
    let current_proto = unsafe { ptr::read_unaligned(Object::proto_ptr(obj_ptr.as_ptr())) };

    if current_proto == proto {
        return Ok(());
    }

    // Check for circular prototype chain.
    if proto != JS_NULL {
        let mut check = proto;
        loop {
            let check_ptr = value_to_ptr::<Object>(check).ok_or(())?;
            if check_ptr == obj_ptr {
                return Err(());
            }
            let next = unsafe { ptr::read_unaligned(Object::proto_ptr(check_ptr.as_ptr())) };
            if next == JS_NULL {
                break;
            }
            check = next;
        }
    }

    unsafe {
        ptr::write_unaligned(Object::proto_ptr(obj_ptr.as_ptr()), proto);
    }
    Ok(())
}

pub fn js_object_create(
    ctx: &mut JSContext,
    _this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    let proto = args.first().copied().unwrap_or(JS_UNDEFINED);
    if proto != JS_NULL && !proto.is_object() {
        return JS_EXCEPTION;
    }
    if args.len() >= 2 && !is_undefined(args[1]) {
        // Additional properties not supported.
        return JS_EXCEPTION;
    }
    ctx.alloc_object_with_proto(proto).unwrap_or(JS_EXCEPTION)
}

pub fn js_object_keys(
    ctx: &mut JSContext,
    _this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    let obj = args.first().copied().unwrap_or(JS_UNDEFINED);
    if !obj.is_object() {
        return JS_EXCEPTION;
    }
    match object_keys(ctx, obj) {
        Ok(arr) => arr,
        Err(_) => JS_EXCEPTION,
    }
}

// ---------------------------------------------------------------------------
// Function builtins
// ---------------------------------------------------------------------------

pub fn js_function_call(
    ctx: &mut JSContext,
    this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    // Function.prototype.call(thisArg, ...args)
    // this_val is the function to call
    if !this_val.is_function() {
        return JS_EXCEPTION;
    }
    let this_arg = args.first().copied().unwrap_or(JS_UNDEFINED);
    let call_args = if args.len() > 1 { &args[1..] } else { &[] };
    match crate::interpreter::call_with_this(ctx, this_val, this_arg, call_args) {
        Ok(result) => result,
        Err(_) => JS_EXCEPTION,
    }
}

pub fn js_function_apply(
    ctx: &mut JSContext,
    this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    // Function.prototype.apply(thisArg, argsArray)
    if !this_val.is_function() {
        return JS_EXCEPTION;
    }
    let this_arg = args.first().copied().unwrap_or(JS_UNDEFINED);
    let args_array = args.get(1).copied().unwrap_or(JS_UNDEFINED);

    // Extract arguments from the array
    let call_args = if args_array.is_object() {
        match extract_array_elements(ctx, args_array) {
            Ok(elements) => elements,
            Err(_) => return JS_EXCEPTION,
        }
    } else {
        Vec::new()
    };

    match crate::interpreter::call_with_this(ctx, this_val, this_arg, &call_args) {
        Ok(result) => result,
        Err(_) => JS_EXCEPTION,
    }
}

fn extract_array_elements(ctx: &JSContext, arr: JSValue) -> Result<Vec<JSValue>, ()> {
    let obj_ptr = value_to_ptr::<Object>(arr).ok_or(())?;
    let header_word = unsafe { ptr::read_unaligned(obj_ptr.as_ptr().cast::<JSWord>()) };
    let header = ObjectHeader::from_word(header_word);
    if header.tag() != MTag::Object || header.class_id() != JSObjectClass::Array as u8 {
        return Err(());
    }
    let data = unsafe {
        let payload = Object::payload_ptr(obj_ptr.as_ptr());
        ptr::read_unaligned(core::ptr::addr_of!((*payload).array))
    };
    let len = data.len() as usize;
    let tab = data.tab();
    if tab == JS_NULL || len == 0 {
        return Ok(Vec::new());
    }

    // Read elements from the value array
    let tab_ptr = value_to_ptr::<u8>(tab).ok_or(())?;
    let header_word = unsafe { ptr::read_unaligned(tab_ptr.as_ptr().cast::<JSWord>()) };
    let header = crate::containers::ValueArrayHeader::from(MbHeader::from_word(header_word));
    let arr_ptr = unsafe { tab_ptr.as_ptr().add(size_of::<JSWord>()) as *const JSValue };
    let mut elements = Vec::with_capacity(len.min(header.size() as usize));
    for i in 0..len.min(header.size() as usize) {
        let val = unsafe { ptr::read_unaligned(arr_ptr.add(i)) };
        // Resolve ROM pointers if needed
        let resolved = if ctx.is_rom_ptr(NonNull::new(arr_ptr as *mut u8).unwrap()) {
            ctx.rom_value_from_word(crate::jsvalue::raw_bits(val))
        } else {
            val
        };
        elements.push(resolved);
    }
    Ok(elements)
}

pub fn js_function_bind(
    ctx: &mut JSContext,
    this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    // Function.prototype.bind(thisArg, ...args)
    // Creates a bound function
    if !this_val.is_function() {
        return JS_EXCEPTION;
    }

    // Allocate a value array to store: [func, thisArg, ...boundArgs]
    let bound_args_count = if args.is_empty() { 0 } else { args.len() - 1 };
    let total_size = 2 + bound_args_count;
    let arr_ptr = match ctx.alloc_value_array(total_size) {
        Ok(ptr) => ptr,
        Err(_) => return JS_EXCEPTION,
    };

    // Store func and thisArg and bound args
    unsafe {
        let arr = arr_ptr.as_ptr().add(size_of::<JSWord>()) as *mut JSValue;
        ptr::write_unaligned(arr, this_val);
        ptr::write_unaligned(arr.add(1), args.first().copied().unwrap_or(JS_UNDEFINED));
        for (i, arg) in args.iter().skip(1).enumerate() {
            ptr::write_unaligned(arr.add(2 + i), *arg);
        }
    }

    let params = crate::jsvalue::value_from_ptr(arr_ptr);
    // JS_CFUNCTION_bound = 0
    ctx.new_cfunction_params(0, params).unwrap_or(JS_EXCEPTION)
}

pub fn js_function_bound(
    ctx: &mut JSContext,
    _this_val: JSValue,
    args: &[JSValue],
    params: JSValue,
) -> JSValue {
    // This is called when a bound function is invoked
    let params_ptr = match value_to_ptr::<u8>(params) {
        Some(p) => p,
        None => return JS_EXCEPTION,
    };

    // Read bound parameters: [func, thisArg, ...boundArgs]
    let header_word = unsafe { ptr::read_unaligned(params_ptr.as_ptr().cast::<JSWord>()) };
    let header = crate::containers::ValueArrayHeader::from(MbHeader::from_word(header_word));
    let size = header.size() as usize;
    if size < 2 {
        return JS_EXCEPTION;
    }

    let arr = unsafe { params_ptr.as_ptr().add(size_of::<JSWord>()) as *const JSValue };
    let func = unsafe { ptr::read_unaligned(arr) };
    let this_arg = unsafe { ptr::read_unaligned(arr.add(1)) };

    // Combine bound args with call args
    let bound_args_count = size - 2;
    let mut call_args = Vec::with_capacity(bound_args_count + args.len());
    for i in 0..bound_args_count {
        call_args.push(unsafe { ptr::read_unaligned(arr.add(2 + i)) });
    }
    call_args.extend_from_slice(args);

    match crate::interpreter::call_with_this(ctx, func, this_arg, &call_args) {
        Ok(result) => result,
        Err(_) => JS_EXCEPTION,
    }
}

pub fn js_function_toString(
    ctx: &mut JSContext,
    this_val: JSValue,
    _args: &[JSValue],
) -> JSValue {
    // Get function name
    let name = match get_function_name(ctx, this_val) {
        Ok(n) => n,
        Err(_) => return JS_EXCEPTION,
    };
    let name_str = {
        let mut scratch = [0u8; 5];
        if let Some(view) = string_view(name, &mut scratch) {
            core::str::from_utf8(view.bytes())
                .unwrap_or("anonymous")
                .to_string()
        } else {
            "anonymous".to_string()
        }
    };
    let result = format!("function {}() {{\n    [native code]\n}}", name_str);
    alloc_string(ctx, &result)
}

fn get_function_name(ctx: &mut JSContext, func: JSValue) -> Result<JSValue, ()> {
    if !is_ptr(func) {
        if value_get_special_tag(func) != JS_TAG_SHORT_FUNC {
            return Err(());
        }
        let idx = value_get_special_value(func);
        if idx < 0 {
            return Err(());
        }
        let def = ctx.c_function(idx as usize).ok_or(())?;
        return ctx.intern_string(def.name_str.as_bytes()).map_err(|_| ());
    }
    let obj_ptr = value_to_ptr::<Object>(func).ok_or(())?;
    let header_word = unsafe { ptr::read_unaligned(obj_ptr.as_ptr().cast::<JSWord>()) };
    let header = ObjectHeader::from_word(header_word);
    if header.tag() != MTag::Object {
        return Err(());
    }
    match header.class_id() {
        c if c == JSObjectClass::Closure as u8 => {
            let func_bytecode = unsafe {
                let payload = Object::payload_ptr(obj_ptr.as_ptr());
                let closure = core::ptr::addr_of!((*payload).closure);
                ptr::read_unaligned(crate::closure_data::ClosureData::func_bytecode_ptr(
                    closure as *mut _,
                ))
            };
            let bytecode_ptr =
                value_to_ptr::<crate::function_bytecode::FunctionBytecode>(func_bytecode)
                    .ok_or(())?;
            let bytecode = unsafe { bytecode_ptr.as_ref() };
            let name = bytecode.func_name();
            if name == JS_NULL {
                Ok(ctx.intern_string(b"").map_err(|_| ())?)
            } else {
                Ok(name)
            }
        }
        c if c == JSObjectClass::CFunction as u8 => {
            let data = unsafe {
                let payload = Object::payload_ptr(obj_ptr.as_ptr());
                ptr::read_unaligned(core::ptr::addr_of!((*payload).cfunc))
            };
            let idx = data.idx();
            let def = ctx.c_function(idx as usize).ok_or(())?;
            Ok(ctx.intern_string(def.name_str.as_bytes()).map_err(|_| ())?)
        }
        _ => Err(()),
    }
}

pub fn js_function_get_prototype(
    ctx: &mut JSContext,
    this_val: JSValue,
    _args: &[JSValue],
) -> JSValue {
    if !is_ptr(this_val) {
        if value_get_special_tag(this_val) != JS_TAG_SHORT_FUNC {
            return JS_EXCEPTION;
        }
        return JS_UNDEFINED;
    }
    let obj_ptr = match value_to_ptr::<Object>(this_val) {
        Some(p) => p,
        None => return JS_EXCEPTION,
    };
    let header_word = unsafe { ptr::read_unaligned(obj_ptr.as_ptr().cast::<JSWord>()) };
    let header = ObjectHeader::from_word(header_word);
    if header.tag() != MTag::Object {
        return JS_EXCEPTION;
    }
    match header.class_id() {
        c if c == JSObjectClass::Closure as u8 => {
            let obj = match ctx.alloc_object_default() {
                Ok(o) => o,
                Err(_) => return JS_EXCEPTION,
            };
            let ctor_key = match ctx.intern_string(b"constructor") {
                Ok(k) => k,
                Err(_) => return JS_EXCEPTION,
            };
            if define_property_value(ctx, obj, ctor_key, this_val).is_err() {
                return JS_EXCEPTION;
            }
            let proto_key = match ctx.intern_string(b"prototype") {
                Ok(k) => k,
                Err(_) => return JS_EXCEPTION,
            };
            if define_property_value(ctx, this_val, proto_key, obj).is_err() {
                return JS_EXCEPTION;
            }
            obj
        }
        c if c == JSObjectClass::CFunction as u8 => JS_UNDEFINED,
        _ => JS_EXCEPTION,
    }
}

pub fn js_function_set_prototype(
    ctx: &mut JSContext,
    this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    if !this_val.is_function() {
        return JS_EXCEPTION;
    }
    let proto_key = match ctx.intern_string(b"prototype") {
        Ok(k) => k,
        Err(_) => return JS_EXCEPTION,
    };
    let proto = args.first().copied().unwrap_or(JS_UNDEFINED);
    if define_property_value(ctx, this_val, proto_key, proto).is_err() {
        return JS_EXCEPTION;
    }
    JS_UNDEFINED
}

pub fn js_function_get_length_name(
    ctx: &mut JSContext,
    this_val: JSValue,
    _args: &[JSValue],
    magic: i32,
) -> JSValue {
    // magic: 0 = length, 1 = name
    let is_name = magic != 0;

    if !is_ptr(this_val) {
        if value_get_special_tag(this_val) != JS_TAG_SHORT_FUNC {
            return JS_EXCEPTION;
        }
        let idx = value_get_special_value(this_val);
        if idx < 0 {
            return JS_EXCEPTION;
        }
        let def = match ctx.c_function(idx as usize) {
            Some(d) => d,
            None => return JS_EXCEPTION,
        };
        if is_name {
            ctx.intern_string(def.name_str.as_bytes())
                .unwrap_or(JS_EXCEPTION)
        } else {
            new_short_int(def.arg_count as i32)
        }
    } else {
        let obj_ptr = match value_to_ptr::<Object>(this_val) {
            Some(p) => p,
            None => return JS_EXCEPTION,
        };
        let header_word = unsafe { ptr::read_unaligned(obj_ptr.as_ptr().cast::<JSWord>()) };
        let header = ObjectHeader::from_word(header_word);
        if header.tag() != MTag::Object {
            return JS_EXCEPTION;
        }
        match header.class_id() {
            c if c == JSObjectClass::Closure as u8 => {
                let func_bytecode = unsafe {
                    let payload = Object::payload_ptr(obj_ptr.as_ptr());
                    let closure = core::ptr::addr_of!((*payload).closure);
                    ptr::read_unaligned(crate::closure_data::ClosureData::func_bytecode_ptr(
                        closure as *mut _,
                    ))
                };
                let bytecode_ptr = match value_to_ptr::<crate::function_bytecode::FunctionBytecode>(
                    func_bytecode,
                ) {
                    Some(p) => p,
                    None => return JS_EXCEPTION,
                };
                let bytecode = unsafe { bytecode_ptr.as_ref() };
                if is_name {
                    let name = bytecode.func_name();
                    if name == JS_NULL {
                        ctx.intern_string(b"").unwrap_or(JS_EXCEPTION)
                    } else {
                        name
                    }
                } else {
                    new_short_int(bytecode.arg_count() as i32)
                }
            }
            c if c == JSObjectClass::CFunction as u8 => {
                let data = unsafe {
                    let payload = Object::payload_ptr(obj_ptr.as_ptr());
                    ptr::read_unaligned(core::ptr::addr_of!((*payload).cfunc))
                };
                let idx = data.idx();
                let def = match ctx.c_function(idx as usize) {
                    Some(d) => d,
                    None => return JS_EXCEPTION,
                };
                if is_name {
                    ctx.intern_string(def.name_str.as_bytes())
                        .unwrap_or(JS_EXCEPTION)
                } else {
                    new_short_int(def.arg_count as i32)
                }
            }
            _ => JS_EXCEPTION,
        }
    }
}

pub fn js_function_constructor(
    ctx: &mut JSContext,
    _this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    // Function(arg1, arg2, ..., body) constructor
    // For now, return an exception since dynamic function compilation
    // requires the parser and JS_Eval which will be implemented in phase 5
    let _ = (ctx, args);
    JS_EXCEPTION
}

// ---------------------------------------------------------------------------
// String builtins
// ---------------------------------------------------------------------------

pub fn js_string_constructor(
    ctx: &mut JSContext,
    _this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    if args.is_empty() {
        ctx.intern_string(b"").unwrap_or(JS_EXCEPTION)
    } else {
        match conversion::to_string(ctx, args[0]) {
            Ok(s) => s,
            Err(_) => JS_EXCEPTION,
        }
    }
}

pub fn js_string_get_length(
    ctx: &mut JSContext,
    this_val: JSValue,
    _args: &[JSValue],
) -> JSValue {
    let s = match to_string_check_object(ctx, this_val) {
        Ok(s) => s,
        Err(_) => return JS_EXCEPTION,
    };
    let len = ctx.string_len(s);
    new_short_int(len as i32)
}

pub fn js_string_set_length(
    _ctx: &mut JSContext,
    _this_val: JSValue,
    _args: &[JSValue],
) -> JSValue {
    JS_UNDEFINED // string length is read-only
}

pub fn js_string_charAt(
    ctx: &mut JSContext,
    this_val: JSValue,
    args: &[JSValue],
    magic: i32,
) -> JSValue {
    let s = match to_string_check_object(ctx, this_val) {
        Ok(s) => s,
        Err(_) => return JS_EXCEPTION,
    };
    let idx = match to_int32_with_ctx(ctx, args.first().copied().unwrap_or(JS_UNDEFINED)) {
        Ok(i) => i,
        Err(_) => return JS_EXCEPTION,
    };
    if idx < 0 {
        return char_at_undefined(ctx, magic);
    }
    let c = ctx.string_getcp(s, idx as u32, magic == MAGIC_CODEPOINT_AT);
    if c < 0 {
        return char_at_undefined(ctx, magic);
    }
    match magic {
        MAGIC_CHAR_CODE_AT | MAGIC_CODEPOINT_AT => new_short_int(c),
        _ => ctx.new_string_char(c as u32).unwrap_or(JS_EXCEPTION),
    }
}

const MAGIC_CHAR_AT: i32 = 0;
const MAGIC_CHAR_CODE_AT: i32 = 1;
const MAGIC_CODEPOINT_AT: i32 = 2;

fn char_at_undefined(ctx: &mut JSContext, magic: i32) -> JSValue {
    match magic {
        MAGIC_CHAR_CODE_AT => ctx.new_float64(f64::NAN).unwrap_or(JS_EXCEPTION),
        MAGIC_CHAR_AT => ctx.intern_string(b"").unwrap_or(JS_EXCEPTION),
        _ => JS_UNDEFINED,
    }
}

pub fn js_string_slice(
    ctx: &mut JSContext,
    this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    let s = match to_string_check_object(ctx, this_val) {
        Ok(s) => s,
        Err(_) => return JS_EXCEPTION,
    };
    let len = ctx.string_len(s) as i32;
    let start = match to_int32_clamp_neg(ctx, args.first().copied().unwrap_or(JS_UNDEFINED), len) {
        Ok(i) => i,
        Err(_) => return JS_EXCEPTION,
    };
    let end = if args.len() > 1 && !is_undefined(args[1]) {
        match to_int32_clamp_neg(ctx, args[1], len) {
            Ok(i) => i,
            Err(_) => return JS_EXCEPTION,
        }
    } else {
        len
    };
    let end = end.max(start);
    ctx.sub_string(s, start as u32, end as u32)
        .unwrap_or(JS_EXCEPTION)
}

pub fn js_string_substring(
    ctx: &mut JSContext,
    this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    let s = match to_string_check_object(ctx, this_val) {
        Ok(s) => s,
        Err(_) => return JS_EXCEPTION,
    };
    let len = ctx.string_len(s) as i32;
    let a = match to_int32_clamp(ctx, args.first().copied().unwrap_or(JS_UNDEFINED), 0, len) {
        Ok(i) => i,
        Err(_) => return JS_EXCEPTION,
    };
    let b = if args.len() > 1 && !is_undefined(args[1]) {
        match to_int32_clamp(ctx, args[1], 0, len) {
            Ok(i) => i,
            Err(_) => return JS_EXCEPTION,
        }
    } else {
        len
    };
    let (start, end) = if a < b { (a, b) } else { (b, a) };
    ctx.sub_string(s, start as u32, end as u32)
        .unwrap_or(JS_EXCEPTION)
}

pub fn js_string_concat(
    ctx: &mut JSContext,
    this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    let mut result = match to_string_check_object(ctx, this_val) {
        Ok(s) => {
            let mut scratch = [0u8; 5];
            string_view(s, &mut scratch)
                .map(|v| v.bytes().to_vec())
                .unwrap_or_default()
        }
        Err(_) => return JS_EXCEPTION,
    };
    for arg in args {
        let s = match conversion::to_string(ctx, *arg) {
            Ok(s) => s,
            Err(_) => return JS_EXCEPTION,
        };
        let mut scratch = [0u8; 5];
        if let Some(view) = string_view(s, &mut scratch) {
            result.extend_from_slice(view.bytes());
        }
    }
    ctx.new_string_len(&result).unwrap_or(JS_EXCEPTION)
}

pub fn js_string_indexOf(
    ctx: &mut JSContext,
    this_val: JSValue,
    args: &[JSValue],
    magic: i32,
) -> JSValue {
    let last_index_of = magic != 0;
    let s = match to_string_check_object(ctx, this_val) {
        Ok(s) => s,
        Err(_) => return JS_EXCEPTION,
    };
    let needle = match conversion::to_string(ctx, args.first().copied().unwrap_or(JS_UNDEFINED)) {
        Ok(n) => n,
        Err(_) => return JS_EXCEPTION,
    };
    let len = ctx.string_len(s) as i32;
    let v_len = ctx.string_len(needle) as i32;

    let pos = if last_index_of {
        let default_pos = len - v_len;
        if args.len() > 1 {
            match conversion::to_number(ctx, args[1]) {
                Ok(d) if !d.is_nan() => {
                    let p = d as i32;
                    p.clamp(0, default_pos.max(0))
                }
                _ => default_pos.max(0),
            }
        } else {
            default_pos.max(0)
        }
    } else {
        let default_pos = 0;
        if args.len() > 1 {
            match to_int32_clamp(ctx, args[1], 0, len) {
                Ok(p) => p,
                Err(_) => return JS_EXCEPTION,
            }
        } else {
            default_pos
        }
    };

    let result = string_index_of(ctx, s, needle, pos, len, v_len, last_index_of);
    new_short_int(result)
}

fn string_index_of(
    ctx: &mut JSContext,
    haystack: JSValue,
    needle: JSValue,
    start_pos: i32,
    len: i32,
    v_len: i32,
    reverse: bool,
) -> i32 {
    if v_len > len {
        return -1;
    }
    if reverse {
        for i in (0..=start_pos.min(len - v_len)).rev() {
            if string_compare_at(ctx, haystack, needle, i, v_len) {
                return i;
            }
        }
    } else {
        for i in start_pos..=(len - v_len) {
            if string_compare_at(ctx, haystack, needle, i, v_len) {
                return i;
            }
        }
    }
    -1
}

fn string_compare_at(
    ctx: &mut JSContext,
    haystack: JSValue,
    needle: JSValue,
    offset: i32,
    needle_len: i32,
) -> bool {
    for j in 0..needle_len {
        let c1 = ctx.string_getc(haystack, (offset + j) as u32);
        let c2 = ctx.string_getc(needle, j as u32);
        if c1 != c2 {
            return false;
        }
    }
    true
}

fn is_regexp_object(val: JSValue) -> bool {
    if !is_ptr(val) {
        return false;
    }
    let Some(obj_ptr) = value_to_ptr::<Object>(val) else {
        return false;
    };
    let header_word = unsafe {
        // SAFETY: obj_ptr points to a readable object header.
        ptr::read_unaligned(obj_ptr.as_ptr().cast::<JSWord>())
    };
    let header = ObjectHeader::from_word(header_word);
    header.tag() == MTag::Object && header.class_id() == JSObjectClass::RegExp as u8
}

fn regexp_object_ptr(val: JSValue) -> Result<NonNull<Object>, RegExpError> {
    let ptr = value_to_ptr::<Object>(val).ok_or(RegExpError::TypeError("not a regular expression"))?;
    let header_word = unsafe {
        // SAFETY: ptr points to a readable object header.
        ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
    };
    let header = ObjectHeader::from_word(header_word);
    if header.tag() != MTag::Object || header.class_id() != JSObjectClass::RegExp as u8 {
        return Err(RegExpError::TypeError("not a regular expression"));
    }
    Ok(ptr)
}

fn regexp_payload_ptr(obj_ptr: NonNull<Object>) -> *mut RegExp {
    unsafe {
        // SAFETY: obj_ptr points to a RegExp object payload.
        let payload = Object::payload_ptr(obj_ptr.as_ptr());
        ptr::addr_of_mut!((*payload).regexp)
    }
}

fn regexp_flags(val: JSValue) -> Result<u32, RegExpError> {
    let obj_ptr = regexp_object_ptr(val)?;
    let re_ptr = regexp_payload_ptr(obj_ptr);
    let byte_code = unsafe {
        // SAFETY: re_ptr points to a valid RegExp with a byte_code slot.
        ptr::read_unaligned(RegExp::byte_code_ptr(re_ptr))
    };
    let byte_ptr = value_to_ptr::<u8>(byte_code).ok_or(RegExpError::InvalidValue("byte array"))?;
    let header_word = unsafe {
        // SAFETY: byte_ptr points to a readable memblock header.
        ptr::read_unaligned(byte_ptr.as_ptr().cast::<JSWord>())
    };
    let header = MbHeader::from_word(header_word);
    if header.tag() != MTag::ByteArray {
        return Err(RegExpError::InvalidValue("byte array"));
    }
    let size = ByteArrayHeader::from(header).size() as usize;
    if size < 2 {
        return Err(RegExpError::InvalidBytecode("header"));
    }
    let payload = unsafe {
        // SAFETY: payload follows the byte array header and spans `size` bytes.
        byte_ptr.as_ptr().add(size_of::<JSWord>())
    };
    let bytes = unsafe {
        // SAFETY: payload covers `size` bytes of byte array storage.
        slice::from_raw_parts(payload, size)
    };
    Ok(get_u16(&bytes[0..2]) as u32)
}

fn regexp_last_index(val: JSValue) -> Result<i32, RegExpError> {
    let obj_ptr = regexp_object_ptr(val)?;
    let re_ptr = regexp_payload_ptr(obj_ptr);
    Ok(unsafe {
        // SAFETY: re_ptr points to a valid RegExp with a last_index slot.
        ptr::read_unaligned(RegExp::last_index_ptr(re_ptr))
    })
}

fn regexp_set_last_index(val: JSValue, last_index: i32) -> Result<(), RegExpError> {
    let obj_ptr = regexp_object_ptr(val)?;
    let re_ptr = regexp_payload_ptr(obj_ptr);
    unsafe {
        // SAFETY: re_ptr points to a valid RegExp with a last_index slot.
        ptr::write_unaligned(RegExp::last_index_ptr(re_ptr), last_index);
    }
    Ok(())
}

fn regexp_source(val: JSValue) -> Result<JSValue, RegExpError> {
    let obj_ptr = regexp_object_ptr(val)?;
    let re_ptr = regexp_payload_ptr(obj_ptr);
    Ok(unsafe {
        // SAFETY: re_ptr points to a valid RegExp with a source slot.
        ptr::read_unaligned(RegExp::source_ptr(re_ptr))
    })
}

fn handle_regexp_error(ctx: &mut JSContext, err: RegExpError) -> JSValue {
    match err {
        RegExpError::TypeError(msg) => ctx.throw_type_error(msg),
        _ => JS_EXCEPTION,
    }
}

// Flag character order for regexp flags string: g, i, m, s, u, y
const RE_FLAG_CHARS: [u8; 6] = [b'g', b'i', b'm', b's', b'u', b'y'];

fn regexp_flags_str(re_flags: u32) -> Vec<u8> {
    let mut buf = Vec::with_capacity(6);
    for (i, &ch) in RE_FLAG_CHARS.iter().enumerate() {
        if (re_flags >> i) & 1 != 0 {
            buf.push(ch);
        }
    }
    buf
}

/// RegExp constructor: new RegExp(pattern, flags)
pub fn js_regexp_constructor(
    ctx: &mut JSContext,
    _this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    use crate::conversion::to_string;
    use crate::parser::regexp::compile_regexp;
    use crate::parser::regexp_flags::parse_regexp_flags;

    // Convert pattern to string
    let pattern_arg = args.first().copied().unwrap_or(JS_UNDEFINED);
    let pattern = match to_string(ctx, pattern_arg) {
        Ok(s) => s,
        Err(_) => return JS_EXCEPTION,
    };

    // Get pattern bytes
    let mut scratch = [0u8; 5];
    let pattern_bytes = match string_view(pattern, &mut scratch) {
        Some(view) => view.bytes().to_vec(),
        None => return ctx.throw_type_error("invalid pattern"),
    };

    // Parse flags if provided
    let flags_arg = args.get(1).copied().unwrap_or(JS_UNDEFINED);
    let re_flags = if is_undefined(flags_arg) {
        0u32
    } else {
        let flags_str = match to_string(ctx, flags_arg) {
            Ok(s) => s,
            Err(_) => return JS_EXCEPTION,
        };
        let mut scratch2 = [0u8; 5];
        let flags_bytes = match string_view(flags_str, &mut scratch2) {
            Some(view) => view.bytes().to_vec(),
            None => return ctx.throw_type_error("invalid flags"),
        };
        let mut parsed_flags = 0u32;
        let consumed = parse_regexp_flags(&mut parsed_flags, &flags_bytes);
        if consumed != flags_bytes.len() {
            return ctx.throw_syntax_error("invalid regexp flags");
        }
        parsed_flags
    };

    // Compile the regexp
    let bytecode = match compile_regexp(&pattern_bytes, re_flags) {
        Ok(bc) => bc,
        Err(err) => return ctx.throw_syntax_error(err.message()),
    };

    // Allocate bytecode array
    let bytecode_val = match ctx.alloc_byte_array(bytecode.bytes()) {
        Ok(val) => val,
        Err(_) => return JS_EXCEPTION,
    };

    // Create the RegExp object
    match ctx.alloc_regexp(pattern, bytecode_val, 0) {
        Ok(obj) => obj,
        Err(_) => JS_EXCEPTION,
    }
}

/// RegExp.prototype.lastIndex getter
pub fn js_regexp_get_lastIndex(
    _ctx: &mut JSContext,
    this_val: JSValue,
    _args: &[JSValue],
) -> JSValue {
    match regexp_last_index(this_val) {
        Ok(idx) => new_short_int(idx),
        Err(_) => JS_EXCEPTION,
    }
}

/// RegExp.prototype.lastIndex setter
pub fn js_regexp_set_lastIndex(
    ctx: &mut JSContext,
    this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    let arg = args.first().copied().unwrap_or(JS_UNDEFINED);
    let last_index = match to_number(arg) {
        v if v.is_nan() => 0,
        v => v as i32,
    };
    if let Err(err) = regexp_set_last_index(this_val, last_index) {
        return handle_regexp_error(ctx, err);
    }
    JS_UNDEFINED
}

/// RegExp.prototype.source getter
pub fn js_regexp_get_source(
    ctx: &mut JSContext,
    this_val: JSValue,
    _args: &[JSValue],
) -> JSValue {
    match regexp_source(this_val) {
        Ok(source) => source,
        Err(err) => handle_regexp_error(ctx, err),
    }
}

/// RegExp.prototype.flags getter
pub fn js_regexp_get_flags(
    ctx: &mut JSContext,
    this_val: JSValue,
    _args: &[JSValue],
) -> JSValue {
    let flags = match regexp_flags(this_val) {
        Ok(f) => f,
        Err(err) => return handle_regexp_error(ctx, err),
    };
    let flags_str = regexp_flags_str(flags);
    ctx.new_string_len(&flags_str).unwrap_or(JS_EXCEPTION)
}

/// RegExp.prototype.exec (magic=0) and RegExp.prototype.test (magic=1)
pub fn js_regexp_exec(
    ctx: &mut JSContext,
    this_val: JSValue,
    args: &[JSValue],
    magic: i32,
) -> JSValue {
    let arg = args.first().copied().unwrap_or(JS_UNDEFINED);
    let mode = if magic == 1 {
        RegExpExecMode::Test
    } else {
        RegExpExecMode::Exec
    };
    match regexp_exec(ctx, this_val, arg, mode) {
        Ok(val) => val,
        Err(err) => handle_regexp_error(ctx, err),
    }
}

pub fn js_string_toLowerCase(
    ctx: &mut JSContext,
    this_val: JSValue,
    _args: &[JSValue],
    magic: i32,
) -> JSValue {
    let to_lower = magic == 0;
    let s = match to_string_check_object(ctx, this_val) {
        Ok(s) => s,
        Err(_) => return JS_EXCEPTION,
    };
    let len = ctx.string_len(s);
    let mut result = Vec::with_capacity(len as usize);
    for i in 0..len {
        let c = ctx.string_getc(s, i) as u32;
        let converted = if to_lower {
            if (c as u8).is_ascii_uppercase() && c < 128 {
                c + (b'a' - b'A') as u32
            } else {
                c
            }
        } else if (c as u8).is_ascii_lowercase() && c < 128 {
            c - (b'a' - b'A') as u32
        } else {
            c
        };
        let mut buf = [0u8; 4];
        let len = crate::cutils::unicode_to_utf8(&mut buf, converted);
        result.extend_from_slice(&buf[..len]);
    }
    ctx.new_string_len(&result).unwrap_or(JS_EXCEPTION)
}

pub fn js_string_trim(
    ctx: &mut JSContext,
    this_val: JSValue,
    _args: &[JSValue],
    magic: i32,
) -> JSValue {
    let s = match to_string_check_object(ctx, this_val) {
        Ok(s) => s,
        Err(_) => return JS_EXCEPTION,
    };
    let len = ctx.string_len(s) as i32;
    let mut start = 0i32;
    let mut end = len;

    // magic & 1 = trim start, magic & 2 = trim end
    if magic & 1 != 0 {
        while start < len && is_unicode_space(ctx.string_getc(s, start as u32) as u32) {
            start += 1;
        }
    }
    if magic & 2 != 0 {
        while end > start && is_unicode_space(ctx.string_getc(s, (end - 1) as u32) as u32) {
            end -= 1;
        }
    }
    ctx.sub_string(s, start as u32, end as u32)
        .unwrap_or(JS_EXCEPTION)
}

fn is_unicode_space(c: u32) -> bool {
    if c < 128 {
        (0x0009..=0x000D).contains(&c) || c == 0x0020
    } else {
        c == 0x00A0
            || c == 0x1680
            || (0x2000..=0x200A).contains(&c)
            || (0x2028..=0x2029).contains(&c)
            || c == 0x202F
            || c == 0x205F
            || c == 0x3000
            || c == 0xFEFF
    }
}

pub fn js_string_fromCharCode(
    ctx: &mut JSContext,
    _this_val: JSValue,
    args: &[JSValue],
    magic: i32,
) -> JSValue {
    let is_from_codepoint = magic != 0;
    let mut result = Vec::new();
    for arg in args {
        let c = match conversion::to_int32(ctx, *arg) {
            Ok(c) => c,
            Err(_) => return JS_EXCEPTION,
        };
        let codepoint = if is_from_codepoint {
            if !(0..=0x10ffff).contains(&c) {
                return JS_EXCEPTION; // RangeError
            }
            c as u32
        } else {
            (c & 0xffff) as u32
        };
        let mut buf = [0u8; 4];
        let len = crate::cutils::unicode_to_utf8(&mut buf, codepoint);
        result.extend_from_slice(&buf[..len]);
    }
    ctx.new_string_len(&result).unwrap_or(JS_EXCEPTION)
}

pub fn js_string_split(
    ctx: &mut JSContext,
    this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    let s = match to_string_check_object(ctx, this_val) {
        Ok(s) => s,
        Err(_) => return JS_EXCEPTION,
    };

    let limit = if args.len() > 1 && !is_undefined(args[1]) {
        match conversion::to_uint32(ctx, args[1]) {
            Ok(l) => l,
            Err(_) => return JS_EXCEPTION,
        }
    } else {
        u32::MAX
    };

    if limit == 0 {
        return ctx.alloc_array(0).unwrap_or(JS_EXCEPTION);
    }

    let sep = args.first().copied().unwrap_or(JS_UNDEFINED);

    // If separator is undefined, return array with original string
    if is_undefined(sep) {
        let arr = match ctx.alloc_array(1) {
            Ok(a) => a,
            Err(_) => return JS_EXCEPTION,
        };
        let _ = crate::property::set_property(ctx, arr, new_short_int(0), s);
        return arr;
    }

    let input_len = ctx.string_len(s) as i32;
    let arr = match ctx.alloc_array(0) {
        Ok(a) => a,
        Err(_) => return JS_EXCEPTION,
    };

    if is_regexp_object(sep) {
        let re_flags = match regexp_flags(sep) {
            Ok(flags) => flags,
            Err(err) => return handle_regexp_error(ctx, err),
        };
        let index_key = match ctx.intern_string(b"index") {
            Ok(key) => key,
            Err(_) => return JS_EXCEPTION,
        };

        let mut count = 0u32;
        let mut p = 0i32;

        if input_len == 0 {
            if let Err(err) = regexp_set_last_index(sep, 0) {
                return handle_regexp_error(ctx, err);
            }
            let z = match regexp_exec(ctx, sep, s, RegExpExecMode::ForceGlobal) {
                Ok(val) => val,
                Err(err) => return handle_regexp_error(ctx, err),
            };
            if z != JS_NULL {
                return arr;
            }
            let _ = crate::property::set_property(ctx, arr, new_short_int(0), s);
            let _ = ctx.array_set_length(arr, 1);
            return arr;
        }

        let mut q = 0i32;
        while q < input_len {
            if let Err(err) = regexp_set_last_index(sep, q) {
                return handle_regexp_error(ctx, err);
            }
            let z = match regexp_exec(ctx, sep, s, RegExpExecMode::ForceGlobal) {
                Ok(val) => val,
                Err(err) => return handle_regexp_error(ctx, err),
            };
            if z == JS_NULL {
                if (re_flags & LRE_FLAG_STICKY) == 0 {
                    break;
                }
                let c = ctx.string_getcp(s, q as u32, true);
                let advance = if c >= 0x10000 { 2 } else { 1 };
                q += advance;
                continue;
            }

            if (re_flags & LRE_FLAG_STICKY) == 0 {
                let idx_val = match get_property(ctx, z, index_key) {
                    Ok(v) => v,
                    Err(_) => return JS_EXCEPTION,
                };
                let idx = match conversion::to_int32(ctx, idx_val) {
                    Ok(v) => v,
                    Err(_) => return JS_EXCEPTION,
                };
                q = idx;
            }

            let mut e = match regexp_last_index(sep) {
                Ok(v) => v,
                Err(err) => return handle_regexp_error(ctx, err),
            };
            if e > input_len {
                e = input_len;
            }

            if e == p {
                let c = ctx.string_getcp(s, q as u32, true);
                let advance = if c >= 0x10000 { 2 } else { 1 };
                q += advance;
                continue;
            }

            let sub = match ctx.sub_string(s, p as u32, q as u32) {
                Ok(sub) => sub,
                Err(_) => return JS_EXCEPTION,
            };
            let _ = crate::property::set_property(ctx, arr, new_short_int(count as i32), sub);
            count += 1;
            let _ = ctx.array_set_length(arr, count);
            if count >= limit {
                return arr;
            }

            let captures_len = match get_array_info(z) {
                Some((_, len)) => len,
                None => return JS_EXCEPTION,
            };
            for i in 1..captures_len {
                let capture = match get_property(ctx, z, new_short_int(i as i32)) {
                    Ok(val) => val,
                    Err(_) => return JS_EXCEPTION,
                };
                let _ = crate::property::set_property(
                    ctx,
                    arr,
                    new_short_int(count as i32),
                    capture,
                );
                count += 1;
                let _ = ctx.array_set_length(arr, count);
                if count >= limit {
                    return arr;
                }
            }

            q = e;
            p = e;
        }

        let sub = match ctx.sub_string(s, p as u32, input_len as u32) {
            Ok(sub) => sub,
            Err(_) => return JS_EXCEPTION,
        };
        let _ = crate::property::set_property(ctx, arr, new_short_int(count as i32), sub);
        let _ = ctx.array_set_length(arr, count + 1);
        return arr;
    }

    let sep_str = match conversion::to_string(ctx, sep) {
        Ok(s) => s,
        Err(_) => return JS_EXCEPTION,
    };
    let sep_len = ctx.string_len(sep_str) as i32;

    // Empty string with non-empty separator
    if input_len == 0 {
        if sep_len != 0 {
            let _ = crate::property::set_property(ctx, arr, new_short_int(0), s);
            let _ = ctx.array_set_length(arr, 1);
        }
        return arr;
    }

    let mut count = 0u32;
    let mut p = 0i32;
    let mut q = 0i32;

    while q <= input_len - sep_len - if sep_len == 0 { -1 } else { 0 } {
        if sep_len == 0 {
            q += 1;
        }

        // Find next occurrence
        let e = string_index_of(ctx, s, sep_str, q, input_len, sep_len, false);
        if e < 0 {
            break;
        }

        // Add substring from p to e
        let sub = match ctx.sub_string(s, p as u32, e as u32) {
            Ok(sub) => sub,
            Err(_) => return JS_EXCEPTION,
        };
        let _ = crate::property::set_property(ctx, arr, new_short_int(count as i32), sub);
        count += 1;
        let _ = ctx.array_set_length(arr, count);

        if count >= limit {
            return arr;
        }

        p = e + sep_len;
        q = p;
    }

    // Add tail
    let sub = match ctx.sub_string(s, p as u32, input_len as u32) {
        Ok(sub) => sub,
        Err(_) => return JS_EXCEPTION,
    };
    let _ = crate::property::set_property(ctx, arr, new_short_int(count as i32), sub);
    let _ = ctx.array_set_length(arr, count + 1);

    arr
}

pub fn js_string_replace(
    ctx: &mut JSContext,
    this_val: JSValue,
    args: &[JSValue],
    magic: i32,
) -> JSValue {
    let is_replace_all = magic != 0;

    let s = match to_string_check_object(ctx, this_val) {
        Ok(s) => s,
        Err(_) => return JS_EXCEPTION,
    };

    let search_arg = args.first().copied().unwrap_or(JS_UNDEFINED);
    let replace_arg = args.get(1).copied().unwrap_or(JS_UNDEFINED);
    let is_regexp = is_regexp_object(search_arg);

    // Functional replace not supported
    if replace_arg.is_function() {
        return JS_EXCEPTION;
    }

    let replace = match conversion::to_string(ctx, replace_arg) {
        Ok(s) => s,
        Err(_) => return JS_EXCEPTION,
    };

    let input_len = ctx.string_len(s) as i32;
    let mut result = Vec::new();
    let mut end_of_last_match = 0i32;

    if is_regexp {
        let re_flags = match regexp_flags(search_arg) {
            Ok(flags) => flags,
            Err(err) => return handle_regexp_error(ctx, err),
        };
        let index_key = match ctx.intern_string(b"index") {
            Ok(key) => key,
            Err(_) => return JS_EXCEPTION,
        };

        let mut last_index = if (re_flags & (LRE_FLAG_GLOBAL | LRE_FLAG_STICKY)) == 0 {
            0
        } else {
            match regexp_last_index(search_arg) {
                Ok(idx) => idx.max(0),
                Err(err) => return handle_regexp_error(ctx, err),
            }
        };
        if (re_flags & LRE_FLAG_GLOBAL) != 0 {
            if let Err(err) = regexp_set_last_index(search_arg, 0) {
                return handle_regexp_error(ctx, err);
            }
            last_index = 0;
        }

        loop {
            if let Err(err) = regexp_set_last_index(search_arg, last_index) {
                return handle_regexp_error(ctx, err);
            }
            let exec_val = match regexp_exec(ctx, search_arg, s, RegExpExecMode::Exec) {
                Ok(val) => val,
                Err(err) => return handle_regexp_error(ctx, err),
            };
            if exec_val == JS_NULL {
                break;
            }

            let index_val = match get_property(ctx, exec_val, index_key) {
                Ok(val) => val,
                Err(_) => return JS_EXCEPTION,
            };
            let start = match conversion::to_int32(ctx, index_val) {
                Ok(val) => val,
                Err(_) => return JS_EXCEPTION,
            };
            let match_val = match get_property(ctx, exec_val, new_short_int(0)) {
                Ok(val) => val,
                Err(_) => return JS_EXCEPTION,
            };
            let match_len = ctx.string_len(match_val) as i32;
            let end = start + match_len;

            append_utf16_range(ctx, &mut result, s, end_of_last_match, start);

            let captures_len = match get_array_info(exec_val) {
                Some((_, len)) => len,
                None => return JS_EXCEPTION,
            };
            append_replacement(
                ctx,
                &mut result,
                s,
                replace,
                start,
                end,
                Some((exec_val, captures_len)),
            );

            end_of_last_match = end;

            if (re_flags & LRE_FLAG_GLOBAL) == 0 {
                if (re_flags & LRE_FLAG_STICKY) != 0 {
                    let _ = regexp_set_last_index(search_arg, end);
                }
                break;
            }

            let mut next_index = end;
            if end == start {
                let c = ctx.string_getcp(s, end as u32, true);
                let advance = if c >= 0x10000 { 2 } else { 1 };
                next_index = end + advance;
            }
            last_index = next_index;
        }

        append_utf16_range(ctx, &mut result, s, end_of_last_match, input_len);
        return ctx.new_string_len(&result).unwrap_or(JS_EXCEPTION);
    }

    let search = match conversion::to_string(ctx, search_arg) {
        Ok(s) => s,
        Err(_) => return JS_EXCEPTION,
    };
    let search_len = ctx.string_len(search) as i32;

    let mut is_first = true;
    loop {
        let pos = if search_len == 0 {
            if is_first {
                0
            } else if end_of_last_match >= input_len {
                -1
            } else {
                end_of_last_match + 1
            }
        } else {
            string_index_of(ctx, s, search, end_of_last_match, input_len, search_len, false)
        };

        if pos < 0 {
            if is_first {
                // No match found, return original string
                return s;
            }
            break;
        }

        // Append substring before match
        append_utf16_range(ctx, &mut result, s, end_of_last_match, pos);

        // Process replacement string
        append_replacement(
            ctx,
            &mut result,
            s,
            replace,
            pos,
            pos + search_len,
            None,
        );

        end_of_last_match = pos + search_len;
        is_first = false;

        if !is_replace_all {
            break;
        }
    }

    // Append tail
    append_utf16_range(ctx, &mut result, s, end_of_last_match, input_len);

    ctx.new_string_len(&result).unwrap_or(JS_EXCEPTION)
}

pub fn js_string_match(
    ctx: &mut JSContext,
    this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    let regexp = args.first().copied().unwrap_or(JS_UNDEFINED);
    let flags = match regexp_flags(regexp) {
        Ok(flags) => flags,
        Err(err) => return handle_regexp_error(ctx, err),
    };

    if (flags & LRE_FLAG_GLOBAL) == 0 {
        return match regexp_exec(ctx, regexp, this_val, RegExpExecMode::Exec) {
            Ok(val) => val,
            Err(err) => handle_regexp_error(ctx, err),
        };
    }

    if let Err(err) = regexp_set_last_index(regexp, 0) {
        return handle_regexp_error(ctx, err);
    }

    let mut result = JS_NULL;
    let mut count = 0u32;
    loop {
        let exec_val = match regexp_exec(ctx, regexp, this_val, RegExpExecMode::Exec) {
            Ok(val) => val,
            Err(err) => return handle_regexp_error(ctx, err),
        };
        if exec_val == JS_NULL {
            break;
        }
        if result == JS_NULL {
            result = ctx.alloc_array(1).unwrap_or(JS_EXCEPTION);
            if result == JS_EXCEPTION {
                return JS_EXCEPTION;
            }
        }
        let capture = match get_property(ctx, exec_val, new_short_int(0)) {
            Ok(val) => val,
            Err(_) => return JS_EXCEPTION,
        };
        if crate::property::set_property(ctx, result, new_short_int(count as i32), capture).is_err()
        {
            return JS_EXCEPTION;
        }
        count += 1;
        let _ = ctx.array_set_length(result, count);
    }

    result
}

pub fn js_string_search(
    ctx: &mut JSContext,
    this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    let regexp = args.first().copied().unwrap_or(JS_UNDEFINED);
    match regexp_exec(ctx, regexp, this_val, RegExpExecMode::Search) {
        Ok(val) => val,
        Err(err) => handle_regexp_error(ctx, err),
    }
}

fn append_utf16_range(
    ctx: &mut JSContext,
    result: &mut Vec<u8>,
    input: JSValue,
    start: i32,
    end: i32,
) {
    for i in start..end {
        let c = ctx.string_getc(input, i as u32) as u32;
        let mut buf = [0u8; 4];
        let len = crate::cutils::unicode_to_utf8(&mut buf, c);
        result.extend_from_slice(&buf[..len]);
    }
}

fn append_string_bytes(result: &mut Vec<u8>, val: JSValue) -> bool {
    let mut scratch = [0u8; 5];
    let Some(view) = string_view(val, &mut scratch) else {
        return false;
    };
    result.extend_from_slice(view.bytes());
    true
}

fn append_replacement(
    ctx: &mut JSContext,
    result: &mut Vec<u8>,
    input: JSValue,
    replace: JSValue,
    match_start: i32,
    match_end: i32,
    captures: Option<(JSValue, u32)>,
) {
    let replace_len = ctx.string_len(replace) as i32;
    let input_len = ctx.string_len(input) as i32;
    let mut i = 0;

    while i < replace_len {
        let c = ctx.string_getc(replace, i as u32);
        i += 1;

        if c != b'$' as i32 {
            let mut buf = [0u8; 4];
            let len = crate::cutils::unicode_to_utf8(&mut buf, c as u32);
            result.extend_from_slice(&buf[..len]);
            continue;
        }

        if i >= replace_len {
            result.push(b'$');
            continue;
        }

        let c2 = ctx.string_getc(replace, i as u32);
        match c2 as u8 {
            b'$' => {
                result.push(b'$');
                i += 1;
            }
            b'&' => {
                append_utf16_range(ctx, result, input, match_start, match_end);
                i += 1;
            }
            b'`' => {
                append_utf16_range(ctx, result, input, 0, match_start);
                i += 1;
            }
            b'\'' => {
                append_utf16_range(ctx, result, input, match_end, input_len);
                i += 1;
            }
            b'0'..=b'9' => {
                let mut k = (c2 as u8 - b'0') as u32;
                let mut j = i + 1;
                let mut second_digit = None;
                if j < replace_len {
                    let c3 = ctx.string_getc(replace, j as u32);
                    if (c3 as u8).is_ascii_digit() {
                        k = k * 10 + (c3 as u8 - b'0') as u32;
                        second_digit = Some(c3 as u8);
                        j += 1;
                    }
                }
                let mut replaced = false;
                if let Some((captures_val, captures_len)) = captures
                    && k >= 1 && k < captures_len {
                        if let Ok(cap) = get_property(ctx, captures_val, new_short_int(k as i32))
                            && !is_undefined(cap) {
                                let _ = append_string_bytes(result, cap);
                            }
                        replaced = true;
                    }
                if replaced {
                    i = j;
                } else {
                    result.push(b'$');
                    result.push(c2 as u8);
                    if let Some(digit) = second_digit {
                        result.push(digit);
                    }
                    i = j;
                }
            }
            _ => {
                result.push(b'$');
            }
        }
    }
}

// Helper to convert this_val to string, handling objects
fn to_string_check_object(ctx: &mut JSContext, val: JSValue) -> Result<JSValue, ()> {
    if val.is_string() {
        return Ok(val);
    }
    conversion::to_string(ctx, val).map_err(|_| ())
}

fn to_int32_with_ctx(ctx: &mut JSContext, val: JSValue) -> Result<i32, ()> {
    if is_int(val) {
        return Ok(value_get_int(val));
    }
    let n = conversion::to_number(ctx, val).map_err(|_| ())?;
    if n.is_nan() {
        return Ok(0);
    }
    if n.is_infinite() {
        return Ok(if n > 0.0 { i32::MAX } else { i32::MIN });
    }
    Ok(n as i32)
}

fn to_int32_clamp(ctx: &mut JSContext, val: JSValue, min: i32, max: i32) -> Result<i32, ()> {
    let n = to_int32_with_ctx(ctx, val)?;
    Ok(n.clamp(min, max))
}

fn to_int32_clamp_neg(ctx: &mut JSContext, val: JSValue, len: i32) -> Result<i32, ()> {
    let n = to_int32_with_ctx(ctx, val)?;
    if n < 0 {
        Ok((len + n).max(0))
    } else {
        Ok(n.min(len))
    }
}

// ---------------------------------------------------------------------------
// Array builtins
// ---------------------------------------------------------------------------

pub fn js_array_constructor(
    ctx: &mut JSContext,
    _this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    let (len, has_init) = if args.len() == 1 && args[0].is_number() {
        let len = match conversion::to_int32(ctx, args[0]) {
            Ok(l) => l,
            Err(_) => return JS_EXCEPTION,
        };
        if len < 0 {
            return JS_EXCEPTION; // RangeError
        }
        (len as usize, false)
    } else {
        (args.len(), true)
    };

    let arr = match ctx.alloc_array(len) {
        Ok(a) => a,
        Err(_) => return JS_EXCEPTION,
    };

    if has_init && !args.is_empty() {
        for (i, arg) in args.iter().enumerate() {
            if crate::property::set_property(ctx, arr, new_short_int(i as i32), *arg).is_err() {
                return JS_EXCEPTION;
            }
        }
    }
    arr
}

pub fn js_array_get_length(
    ctx: &mut JSContext,
    this_val: JSValue,
    _args: &[JSValue],
) -> JSValue {
    match get_array_info(this_val) {
        Some((_, len)) => new_short_int(len as i32),
        None => {
            let _ = ctx;
            JS_EXCEPTION
        }
    }
}

pub fn js_array_set_length(
    ctx: &mut JSContext,
    this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    if get_array_info(this_val).is_none() {
        return JS_EXCEPTION;
    }
    let new_len = match conversion::to_int32(ctx, args.first().copied().unwrap_or(JS_UNDEFINED)) {
        Ok(l) if l >= 0 => l as u32,
        _ => return JS_EXCEPTION,
    };
    if ctx.array_set_length(this_val, new_len).is_err() {
        return JS_EXCEPTION;
    }
    JS_UNDEFINED
}

pub fn js_array_push(
    ctx: &mut JSContext,
    this_val: JSValue,
    args: &[JSValue],
    magic: i32,
) -> JSValue {
    let is_unshift = magic != 0;
    let (tab, len) = match get_array_info(this_val) {
        Some(info) => info,
        None => return JS_EXCEPTION,
    };

    let new_len = len as usize + args.len();
    if new_len > crate::jsvalue::JS_SHORTINT_MAX as usize {
        return JS_EXCEPTION;
    }

    // Resize array
    if ctx.array_resize(this_val, new_len).is_err() {
        return JS_EXCEPTION;
    }

    // Get updated array info
    let (_, _) = match get_array_info(this_val) {
        Some(info) => info,
        None => return JS_EXCEPTION,
    };

    // If unshift, move existing elements first
    if is_unshift && !args.is_empty() && tab != JS_NULL && len > 0 {
        // Move elements to make room at the beginning
        for i in (0..len).rev() {
            let val = match crate::property::get_property(ctx, this_val, new_short_int(i as i32)) {
                Ok(v) => v,
                Err(_) => JS_UNDEFINED,
            };
            if crate::property::set_property(
                ctx,
                this_val,
                new_short_int((i + args.len() as u32) as i32),
                val,
            )
            .is_err()
            {
                return JS_EXCEPTION;
            }
        }
    }

    // Insert new elements
    let start = if is_unshift { 0 } else { len as usize };
    for (i, arg) in args.iter().enumerate() {
        if crate::property::set_property(ctx, this_val, new_short_int((start + i) as i32), *arg)
            .is_err()
        {
            return JS_EXCEPTION;
        }
    }

    new_short_int(new_len as i32)
}

pub fn js_array_pop(
    ctx: &mut JSContext,
    this_val: JSValue,
    _args: &[JSValue],
) -> JSValue {
    let (_, len) = match get_array_info(this_val) {
        Some(info) => info,
        None => return JS_EXCEPTION,
    };

    if len == 0 {
        return JS_UNDEFINED;
    }

    let ret = match crate::property::get_property(ctx, this_val, new_short_int((len - 1) as i32)) {
        Ok(v) => v,
        Err(_) => JS_UNDEFINED,
    };

    if ctx.array_set_length(this_val, len - 1).is_err() {
        return JS_EXCEPTION;
    }

    ret
}

pub fn js_array_shift(
    ctx: &mut JSContext,
    this_val: JSValue,
    _args: &[JSValue],
) -> JSValue {
    let (_, len) = match get_array_info(this_val) {
        Some(info) => info,
        None => return JS_EXCEPTION,
    };

    if len == 0 {
        return JS_UNDEFINED;
    }

    // Get first element
    let ret = match crate::property::get_property(ctx, this_val, new_short_int(0)) {
        Ok(v) => v,
        Err(_) => JS_UNDEFINED,
    };

    // Shift elements down
    for i in 1..len {
        let val = match crate::property::get_property(ctx, this_val, new_short_int(i as i32)) {
            Ok(v) => v,
            Err(_) => JS_UNDEFINED,
        };
        if crate::property::set_property(ctx, this_val, new_short_int((i - 1) as i32), val).is_err()
        {
            return JS_EXCEPTION;
        }
    }

    if ctx.array_set_length(this_val, len - 1).is_err() {
        return JS_EXCEPTION;
    }

    ret
}

pub fn js_array_join(
    ctx: &mut JSContext,
    this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    if !this_val.is_object() {
        return JS_EXCEPTION;
    }

    let len = match get_array_info(this_val) {
        Some((_, l)) => l,
        None => {
            // Try to get length property for generic objects
            let length_key = match ctx.intern_string(b"length") {
                Ok(k) => k,
                Err(_) => return JS_EXCEPTION,
            };
            match crate::property::get_property(ctx, this_val, length_key) {
                Ok(v) => match conversion::to_uint32(ctx, v) {
                    Ok(l) => l,
                    Err(_) => return JS_EXCEPTION,
                },
                Err(_) => 0,
            }
        }
    };

    let sep = if !args.is_empty() && !is_undefined(args[0]) {
        match conversion::to_string(ctx, args[0]) {
            Ok(s) => s,
            Err(_) => return JS_EXCEPTION,
        }
    } else {
        match ctx.new_string_char(b',' as u32) {
            Ok(s) => s,
            Err(_) => return JS_EXCEPTION,
        }
    };

    let mut result = Vec::new();
    let mut sep_scratch = [0u8; 5];
    let sep_bytes = string_view(sep, &mut sep_scratch)
        .map(|v| v.bytes().to_vec())
        .unwrap_or_else(|| b",".to_vec());

    for i in 0..len {
        if i > 0 {
            result.extend_from_slice(&sep_bytes);
        }
        let val = match crate::property::get_property(ctx, this_val, new_short_int(i as i32)) {
            Ok(v) => v,
            Err(_) => JS_UNDEFINED,
        };
        if !is_undefined(val) && !is_null(val) {
            let s = match conversion::to_string(ctx, val) {
                Ok(s) => s,
                Err(_) => continue,
            };
            let mut scratch = [0u8; 5];
            if let Some(view) = string_view(s, &mut scratch) {
                result.extend_from_slice(view.bytes());
            }
        }
    }

    ctx.new_string_len(&result).unwrap_or(JS_EXCEPTION)
}

pub fn js_array_toString(
    ctx: &mut JSContext,
    this_val: JSValue,
    _args: &[JSValue],
) -> JSValue {
    js_array_join(ctx, this_val, &[])
}

pub fn js_array_isArray(
    _ctx: &mut JSContext,
    _this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    let val = args.first().copied().unwrap_or(JS_UNDEFINED);
    let is_array = get_array_info(val).is_some();
    new_bool(is_array as i32)
}

pub fn js_array_reverse(
    ctx: &mut JSContext,
    this_val: JSValue,
    _args: &[JSValue],
) -> JSValue {
    let (_, len) = match get_array_info(this_val) {
        Some(info) => info,
        None => return JS_EXCEPTION,
    };

    let mut i = 0u32;
    let mut j = len.saturating_sub(1);
    while i < j {
        let val_i = crate::property::get_property(ctx, this_val, new_short_int(i as i32))
            .unwrap_or(JS_UNDEFINED);
        let val_j = crate::property::get_property(ctx, this_val, new_short_int(j as i32))
            .unwrap_or(JS_UNDEFINED);
        let _ = crate::property::set_property(ctx, this_val, new_short_int(i as i32), val_j);
        let _ = crate::property::set_property(ctx, this_val, new_short_int(j as i32), val_i);
        i += 1;
        j = j.saturating_sub(1);
    }

    this_val
}

pub fn js_array_concat(
    ctx: &mut JSContext,
    this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    let (_, this_len) = match get_array_info(this_val) {
        Some(info) => info,
        None => return JS_EXCEPTION,
    };

    // Calculate total length
    let mut total_len: usize = this_len as usize;
    for arg in args {
        if let Some((_, len)) = get_array_info(*arg) {
            total_len += len as usize;
        } else {
            total_len += 1;
        }
    }

    if total_len > crate::jsvalue::JS_SHORTINT_MAX as usize {
        return JS_EXCEPTION;
    }

    let result = match ctx.alloc_array(total_len) {
        Ok(a) => a,
        Err(_) => return JS_EXCEPTION,
    };

    let mut pos = 0usize;

    // Copy this array elements
    for i in 0..this_len {
        let val = crate::property::get_property(ctx, this_val, new_short_int(i as i32))
            .unwrap_or(JS_UNDEFINED);
        let _ = crate::property::set_property(ctx, result, new_short_int(pos as i32), val);
        pos += 1;
    }

    // Copy argument arrays or values
    for arg in args {
        if let Some((_, len)) = get_array_info(*arg) {
            for i in 0..len {
                let val = crate::property::get_property(ctx, *arg, new_short_int(i as i32))
                    .unwrap_or(JS_UNDEFINED);
                let _ = crate::property::set_property(ctx, result, new_short_int(pos as i32), val);
                pos += 1;
            }
        } else {
            let _ = crate::property::set_property(ctx, result, new_short_int(pos as i32), *arg);
            pos += 1;
        }
    }

    result
}

pub fn js_array_indexOf(
    ctx: &mut JSContext,
    this_val: JSValue,
    args: &[JSValue],
    magic: i32,
) -> JSValue {
    let is_last_index_of = magic != 0;
    let (_, len) = match get_array_info(this_val) {
        Some(info) => info,
        None => return JS_EXCEPTION,
    };

    if len == 0 {
        return new_short_int(-1);
    }

    let search_element = args.first().copied().unwrap_or(JS_UNDEFINED);
    let start = if args.len() > 1 {
        match to_int32_clamp(ctx, args[1], 0, len as i32) {
            Ok(n) => n,
            Err(_) => return JS_EXCEPTION,
        }
    } else if is_last_index_of {
        len as i32 - 1
    } else {
        0
    };

    if is_last_index_of {
        let start = start.min(len as i32 - 1);
        for i in (0..=start).rev() {
            let val = crate::property::get_property(ctx, this_val, new_short_int(i))
                .unwrap_or(JS_UNDEFINED);
            if strict_eq(val, search_element) {
                return new_short_int(i);
            }
        }
    } else {
        for i in start..len as i32 {
            let val = crate::property::get_property(ctx, this_val, new_short_int(i))
                .unwrap_or(JS_UNDEFINED);
            if strict_eq(val, search_element) {
                return new_short_int(i);
            }
        }
    }

    new_short_int(-1)
}

fn strict_eq(a: JSValue, b: JSValue) -> bool {
    // Simple strict equality check
    crate::jsvalue::raw_bits(a) == crate::jsvalue::raw_bits(b)
}

pub fn js_array_slice(
    ctx: &mut JSContext,
    this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    let (_, len) = match get_array_info(this_val) {
        Some(info) => info,
        None => return JS_EXCEPTION,
    };

    let start = match to_int32_clamp_neg(ctx, args.first().copied().unwrap_or(JS_UNDEFINED), len as i32) {
        Ok(n) => n as u32,
        Err(_) => return JS_EXCEPTION,
    };

    let end = if args.len() > 1 && !is_undefined(args[1]) {
        match to_int32_clamp_neg(ctx, args[1], len as i32) {
            Ok(n) => n as u32,
            Err(_) => return JS_EXCEPTION,
        }
    } else {
        len
    };

    let end = end.min(len);
    let slice_len = end.saturating_sub(start);

    let result = match ctx.alloc_array(slice_len as usize) {
        Ok(a) => a,
        Err(_) => return JS_EXCEPTION,
    };

    for i in start..end {
        let val = crate::property::get_property(ctx, this_val, new_short_int(i as i32))
            .unwrap_or(JS_UNDEFINED);
        let _ = crate::property::set_property(ctx, result, new_short_int((i - start) as i32), val);
    }

    result
}

pub fn js_array_splice(
    ctx: &mut JSContext,
    this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    let (_, len) = match get_array_info(this_val) {
        Some(info) => info,
        None => return JS_EXCEPTION,
    };

    let len_i32 = len as i32;
    let start = match to_int32_clamp_neg(ctx, args.first().copied().unwrap_or(JS_UNDEFINED), len_i32) {
        Ok(n) => n as u32,
        Err(_) => return JS_EXCEPTION,
    };

    let (del_count, item_count) = if args.is_empty() {
        (0u32, 0usize)
    } else if args.len() == 1 {
        ((len - start), 0usize)
    } else {
        let del = match to_int32_clamp(ctx, args[1], 0, (len - start) as i32) {
            Ok(n) => n as u32,
            Err(_) => return JS_EXCEPTION,
        };
        (del, args.len().saturating_sub(2))
    };

    let new_len = (len as usize + item_count).saturating_sub(del_count as usize);

    // Create result array with deleted elements
    let result = match ctx.alloc_array(del_count as usize) {
        Ok(a) => a,
        Err(_) => return JS_EXCEPTION,
    };

    // Copy deleted elements to result
    for i in 0..del_count {
        let val = crate::property::get_property(ctx, this_val, new_short_int((start + i) as i32))
            .unwrap_or(JS_UNDEFINED);
        let _ = crate::property::set_property(ctx, result, new_short_int(i as i32), val);
    }

    // Shift elements if needed
    if item_count != del_count as usize {
        if del_count as usize > item_count {
            // Shift left
            let shift = del_count as usize - item_count;
            for i in (start + del_count) as usize..len as usize {
                let val = crate::property::get_property(ctx, this_val, new_short_int(i as i32))
                    .unwrap_or(JS_UNDEFINED);
                let _ = crate::property::set_property(
                    ctx,
                    this_val,
                    new_short_int((i - shift) as i32),
                    val,
                );
            }
        } else {
            // Shift right
            let shift = item_count - del_count as usize;
            for i in ((start + del_count) as usize..len as usize).rev() {
                let val = crate::property::get_property(ctx, this_val, new_short_int(i as i32))
                    .unwrap_or(JS_UNDEFINED);
                let _ = crate::property::set_property(
                    ctx,
                    this_val,
                    new_short_int((i + shift) as i32),
                    val,
                );
            }
        }
    }

    // Insert new elements
    for (i, arg) in args.iter().skip(2).enumerate() {
        let _ = crate::property::set_property(
            ctx,
            this_val,
            new_short_int((start as usize + i) as i32),
            *arg,
        );
    }

    // Update array length
    if ctx.array_set_length(this_val, new_len as u32).is_err() {
        return JS_EXCEPTION;
    }

    result
}

// Magic values for array iterator methods
const JS_SPECIAL_EVERY: i32 = 0;
const JS_SPECIAL_SOME: i32 = 1;
const JS_SPECIAL_FOR_EACH: i32 = 2;
const JS_SPECIAL_MAP: i32 = 3;
const JS_SPECIAL_FILTER: i32 = 4;

pub fn js_array_every(
    ctx: &mut JSContext,
    this_val: JSValue,
    args: &[JSValue],
    magic: i32,
) -> JSValue {
    let (_, len) = match get_array_info(this_val) {
        Some(info) => info,
        None => return JS_EXCEPTION,
    };

    let func = args.first().copied().unwrap_or(JS_UNDEFINED);
    let this_arg = args.get(1).copied().unwrap_or(JS_UNDEFINED);

    if !func.is_function() {
        return JS_EXCEPTION; // TypeError: not a function
    }

    // Initialize return value based on magic
    let ret = match magic {
        JS_SPECIAL_EVERY => new_bool(1),
        JS_SPECIAL_SOME => new_bool(0),
        JS_SPECIAL_MAP => match ctx.alloc_array(len as usize) {
            Ok(a) => a,
            Err(_) => return JS_EXCEPTION,
        },
        JS_SPECIAL_FILTER => match ctx.alloc_array(0) {
            Ok(a) => a,
            Err(_) => return JS_EXCEPTION,
        },
        _ => JS_UNDEFINED, // forEach
    };

    let mut n = 0u32; // for filter

    for k in 0..len {
        // Get current element
        let val = match crate::property::get_property(ctx, this_val, new_short_int(k as i32)) {
            Ok(v) => v,
            Err(_) => JS_UNDEFINED,
        };

        // Call callback: func(val, k, this_val)
        let call_args = [val, new_short_int(k as i32), this_val];
        let res = match crate::interpreter::call_with_this(ctx, func, this_arg, &call_args) {
            Ok(r) => r,
            Err(_) => return JS_EXCEPTION,
        };

        // Process result based on magic
        match magic {
            JS_SPECIAL_EVERY => {
                if !to_bool(res) {
                    return new_bool(0);
                }
            }
            JS_SPECIAL_SOME => {
                if to_bool(res) {
                    return new_bool(1);
                }
            }
            JS_SPECIAL_MAP => {
                let _ = crate::property::set_property(ctx, ret, new_short_int(k as i32), res);
            }
            JS_SPECIAL_FILTER => {
                if to_bool(res) {
                    let _ = crate::property::set_property(ctx, ret, new_short_int(n as i32), val);
                    n += 1;
                    // Update array length for filter
                    let _ = ctx.array_set_length(ret, n);
                }
            }
            _ => {} // forEach: do nothing with result
        }
    }

    ret
}

// Magic values for reduce methods
const JS_SPECIAL_REDUCE: i32 = 0;
const JS_SPECIAL_REDUCE_RIGHT: i32 = 1;

pub fn js_array_reduce(
    ctx: &mut JSContext,
    this_val: JSValue,
    args: &[JSValue],
    magic: i32,
) -> JSValue {
    let (_, len) = match get_array_info(this_val) {
        Some(info) => info,
        None => return JS_EXCEPTION,
    };

    let func = args.first().copied().unwrap_or(JS_UNDEFINED);

    if !func.is_function() {
        return JS_EXCEPTION; // TypeError: not a function
    }

    if len == 0 && args.len() <= 1 {
        return JS_EXCEPTION; // TypeError: empty array with no initial value
    }

    let is_right = magic == JS_SPECIAL_REDUCE_RIGHT;

    let (mut acc, start_k) = if args.len() > 1 {
        (args[1], 0u32)
    } else {
        let first_idx = if is_right { len - 1 } else { 0 };
        let val = crate::property::get_property(ctx, this_val, new_short_int(first_idx as i32))
            .unwrap_or(JS_UNDEFINED);
        (val, 1)
    };

    for k in start_k..len {
        let idx = if is_right {
            len - k - 1
        } else {
            k
        };

        let val = match crate::property::get_property(ctx, this_val, new_short_int(idx as i32)) {
            Ok(v) => v,
            Err(_) => JS_UNDEFINED,
        };

        // Call callback: func(acc, val, idx, this_val)
        let call_args = [acc, val, new_short_int(idx as i32), this_val];
        acc = match crate::interpreter::call_with_this(ctx, func, JS_UNDEFINED, &call_args) {
            Ok(r) => r,
            Err(_) => return JS_EXCEPTION,
        };
    }

    acc
}

pub fn js_array_sort(
    ctx: &mut JSContext,
    this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    let (_, len) = match get_array_info(this_val) {
        Some(info) => info,
        None => return JS_EXCEPTION,
    };

    if len <= 1 {
        return this_val;
    }

    let compare_fn = args.first().copied().filter(|v| v.is_function());

    // Build array of (value, original_index) pairs for stable sort
    let mut pairs: Vec<(JSValue, u32)> = Vec::with_capacity(len as usize);
    for i in 0..len {
        let val = crate::property::get_property(ctx, this_val, new_short_int(i as i32))
            .unwrap_or(JS_UNDEFINED);
        pairs.push((val, i));
    }

    // Sort using a simple stable sort
    // Note: For production, this should use heapsort like C version for stability guarantees
    let mut exception = false;
    pairs.sort_by(|a, b| {
        if exception {
            return core::cmp::Ordering::Equal;
        }

        let cmp: i32 = if let Some(func) = compare_fn {
            // Call custom comparator
            let call_args = [a.0, b.0];
            match crate::interpreter::call_with_this(ctx, func, JS_UNDEFINED, &call_args) {
                Ok(res) => {
                    let d = to_number(res);
                    if d.is_nan() {
                        0
                    } else if d > 0.0 {
                        1
                    } else if d < 0.0 {
                        -1
                    } else {
                        0
                    }
                }
                Err(_) => {
                    exception = true;
                    0
                }
            }
        } else {
            // Default: convert to string and compare
            let str_a = match conversion::to_string(ctx, a.0) {
                Ok(s) => s,
                Err(_) => {
                    exception = true;
                    return core::cmp::Ordering::Equal;
                }
            };
            let str_b = match conversion::to_string(ctx, b.0) {
                Ok(s) => s,
                Err(_) => {
                    exception = true;
                    return core::cmp::Ordering::Equal;
                }
            };

            let mut scratch_a = [0u8; 5];
            let mut scratch_b = [0u8; 5];
            let view_a = string_view(str_a, &mut scratch_a);
            let view_b = string_view(str_b, &mut scratch_b);

            match (view_a, view_b) {
                (Some(va), Some(vb)) => match va.bytes().cmp(vb.bytes()) {
                    core::cmp::Ordering::Less => -1,
                    core::cmp::Ordering::Equal => 0,
                    core::cmp::Ordering::Greater => 1,
                },
                _ => 0,
            }
        };

        // Make stable by comparing original indices if values are equal
        if cmp == 0 {
            a.1.cmp(&b.1)
        } else if cmp < 0 {
            core::cmp::Ordering::Less
        } else {
            core::cmp::Ordering::Greater
        }
    });

    if exception {
        return JS_EXCEPTION;
    }

    // Write sorted values back
    for (i, (val, _)) in pairs.iter().enumerate() {
        let _ = crate::property::set_property(ctx, this_val, new_short_int(i as i32), *val);
    }

    this_val
}

fn to_bool(val: JSValue) -> bool {
    if is_int(val) {
        return value_get_int(val) != 0;
    }
    if is_bool(val) {
        return value_get_special_value(val) != 0;
    }
    if is_undefined(val) || is_null(val) {
        return false;
    }
    // Objects/strings/numbers are truthy (simplified)
    true
}

// Helper functions for array operations

fn get_array_info(val: JSValue) -> Option<(JSValue, u32)> {
    if !is_ptr(val) {
        return None;
    }
    let obj_ptr = value_to_ptr::<Object>(val)?;
    let header_word = unsafe { ptr::read_unaligned(obj_ptr.as_ptr().cast::<JSWord>()) };
    let header = ObjectHeader::from_word(header_word);
    if header.tag() != MTag::Object || header.class_id() != JSObjectClass::Array as u8 {
        return None;
    }
    let data = unsafe {
        let payload = Object::payload_ptr(obj_ptr.as_ptr());
        ptr::read_unaligned(core::ptr::addr_of!((*payload).array))
    };
    Some((data.tab(), data.len()))
}

// ---------------------------------------------------------------------------
// Error builtins
// ---------------------------------------------------------------------------

pub fn js_error_constructor(
    ctx: &mut JSContext,
    _this_val: JSValue,
    args: &[JSValue],
    magic: i32,
) -> JSValue {
    // magic is the class ID (Error, TypeError, ReferenceError, etc.)
    let class_id = if magic >= JSObjectClass::Error as i32
        && magic <= JSObjectClass::InternalError as i32
    {
        magic as u8
    } else {
        JSObjectClass::Error as u8
    };

    let proto = ctx.class_proto()[class_id as usize];
    let obj = match ctx.alloc_error(proto) {
        Ok(o) => o,
        Err(_) => return JS_EXCEPTION,
    };

    // Set message
    let message = if !args.is_empty() && !is_undefined(args[0]) {
        match conversion::to_string(ctx, args[0]) {
            Ok(s) => s,
            Err(_) => return JS_EXCEPTION,
        }
    } else {
        match ctx.intern_string(b"") {
            Ok(s) => s,
            Err(_) => return JS_EXCEPTION,
        }
    };

    if ctx.set_error_message(obj, message).is_err() {
        return JS_EXCEPTION;
    }

    let _ = ctx.build_backtrace(obj, None, 1);

    obj
}

pub fn js_error_toString(
    ctx: &mut JSContext,
    this_val: JSValue,
    _args: &[JSValue],
) -> JSValue {
    if !is_error(this_val) {
        return ctx.throw_type_error("not an Error object");
    }

    // Get name property
    let name_key = match ctx.intern_string(b"name") {
        Ok(k) => k,
        Err(_) => return JS_EXCEPTION,
    };
    let name = match crate::property::get_property(ctx, this_val, name_key) {
        Ok(v) if !is_undefined(v) => match conversion::to_string(ctx, v) {
            Ok(s) => s,
            Err(_) => return JS_EXCEPTION,
        },
        _ => match ctx.intern_string(b"Error") {
            Ok(s) => s,
            Err(_) => return JS_EXCEPTION,
        },
    };

    // Get message
    let message = match ctx.get_error_message(this_val) {
        Ok(m) => m,
        Err(_) => JS_NULL,
    };

    let mut name_scratch = [0u8; 5];
    let name_str = string_view(name, &mut name_scratch)
        .map(|v| core::str::from_utf8(v.bytes()).unwrap_or("Error"))
        .unwrap_or("Error");

    let mut msg_scratch = [0u8; 5];
    let msg_str = string_view(message, &mut msg_scratch)
        .map(|v| core::str::from_utf8(v.bytes()).unwrap_or(""))
        .unwrap_or("");

    let result = if msg_str.is_empty() {
        name_str.to_string()
    } else {
        format!("{}: {}", name_str, msg_str)
    };

    alloc_string(ctx, &result)
}

pub fn js_error_get_message(
    ctx: &mut JSContext,
    this_val: JSValue,
    _args: &[JSValue],
    magic: i32,
) -> JSValue {
    if !is_error(this_val) {
        return ctx.throw_type_error("not an Error object");
    }
    if magic == 0 {
        ctx.get_error_message(this_val).unwrap_or(JS_NULL)
    } else {
        ctx.get_error_stack(this_val).unwrap_or(JS_NULL)
    }
}

// ---------------------------------------------------------------------------
// JSON builtins
// ---------------------------------------------------------------------------

pub fn js_json_parse(
    ctx: &mut JSContext,
    _this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    let input = args.first().copied().unwrap_or(JS_UNDEFINED);

    // Convert to string
    let str_val = match conversion::to_string(ctx, input) {
        Ok(s) => s,
        Err(_) => return JS_EXCEPTION,
    };

    // Get string bytes
    let mut scratch = [0u8; 5];
    let Some(view) = string_view(str_val, &mut scratch) else {
        return JS_EXCEPTION;
    };

    // Use the API's JSON evaluation
    crate::api::js_eval(ctx, view.bytes(), crate::capi_defs::JS_EVAL_JSON)
}

pub fn js_json_stringify(
    ctx: &mut JSContext,
    _this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    let val = args.first().copied().unwrap_or(JS_UNDEFINED);
    let replacer = args.get(1).copied().unwrap_or(JS_UNDEFINED);
    let space = args.get(2).copied().unwrap_or(JS_UNDEFINED);
    let mut state = match JsonStringifyState::new(ctx, replacer, space) {
        Ok(state) => state,
        Err(_) => return JS_EXCEPTION,
    };

    let output = match state.stringify(val) {
        Ok(output) => output,
        Err(_) => return JS_EXCEPTION,
    };

    match output {
        Some(bytes) => ctx.new_string_len(&bytes).unwrap_or(JS_EXCEPTION),
        None => JS_UNDEFINED,
    }
}

enum JsonReplacer {
    Function(JSValue),
    PropertyList(Vec<JSValue>),
}

struct JsonStringifyState<'a> {
    ctx: &'a mut JSContext,
    gap: Vec<u8>,
    indent: Vec<u8>,
    replacer: Option<JsonReplacer>,
    stack: Vec<JSValue>,
}

impl<'a> JsonStringifyState<'a> {
    fn new(ctx: &'a mut JSContext, replacer: JSValue, space: JSValue) -> Result<Self, ()> {
        let replacer = build_json_replacer(ctx, replacer)?;
        let gap = build_json_gap(ctx, space)?;
        Ok(Self {
            ctx,
            gap,
            indent: Vec::new(),
            replacer,
            stack: Vec::new(),
        })
    }

    fn stringify(&mut self, value: JSValue) -> Result<Option<Vec<u8>>, ()> {
        let holder = self.ctx.alloc_object_default().map_err(|_| ())?;
        let empty_key = self.ctx.intern_string(b"").map_err(|_| ())?;
        if crate::property::set_property(self.ctx, holder, empty_key, value).is_err() {
            return Err(());
        }
        let mut out = Vec::new();
        let wrote = self.str(empty_key, holder, &mut out)?;
        if wrote {
            Ok(Some(out))
        } else {
            Ok(None)
        }
    }

    fn str(&mut self, key: JSValue, holder: JSValue, out: &mut Vec<u8>) -> Result<bool, ()> {
        let prop_key = conversion::to_property_key(self.ctx, key).map_err(|_| ())?;
        let mut value = get_property(self.ctx, holder, prop_key).map_err(|_| ())?;
        if value == JS_EXCEPTION {
            return Err(());
        }

        if value.is_object() {
            let to_json_key = self.ctx.intern_string(b"toJSON").map_err(|_| ())?;
            let to_json = get_property(self.ctx, value, to_json_key).map_err(|_| ())?;
            if to_json.is_function() {
                value = crate::interpreter::call_with_this(self.ctx, to_json, value, &[key])
                    .map_err(|_| ())?;
                if value == JS_EXCEPTION {
                    return Err(());
                }
            }
        }

        if let Some(func) = match &self.replacer {
            Some(JsonReplacer::Function(func)) => Some(*func),
            _ => None,
        } {
            value = crate::interpreter::call_with_this(self.ctx, func, holder, &[key, value])
                .map_err(|_| ())?;
            if value == JS_EXCEPTION {
                return Err(());
            }
        }

        if let Some(primitive) = boxed_primitive_value(value) {
            value = primitive;
        }

        if is_undefined(value) || value.is_function() {
            return Ok(false);
        }

        if is_null(value) {
            out.extend_from_slice(b"null");
            return Ok(true);
        }

        if is_bool(value) {
            if value_get_special_value(value) != 0 {
                out.extend_from_slice(b"true");
            } else {
                out.extend_from_slice(b"false");
            }
            return Ok(true);
        }

        if value.is_number() {
            let num = number_to_f64(value).ok_or(())?;
            stringify_number(num, out)?;
            return Ok(true);
        }

        if value.is_string() {
            stringify_string(self.ctx, value, out)?;
            return Ok(true);
        }

        if value.is_object() {
            if get_array_info(value).is_some() {
                self.serialize_array(value, out)?;
            } else {
                self.serialize_object(value, out)?;
            }
            return Ok(true);
        }

        out.extend_from_slice(b"null");
        Ok(true)
    }

    fn serialize_array(&mut self, value: JSValue, out: &mut Vec<u8>) -> Result<(), ()> {
        self.check_circular(value)?;
        self.stack.push(value);

        let stepback = self.indent.clone();
        self.indent.extend_from_slice(&self.gap);
        out.push(b'[');

        let len = get_array_info(value).map(|(_, len)| len).unwrap_or(0);
        if len > 0 {
            if !self.gap.is_empty() {
                out.push(b'\n');
            }
            for i in 0..len {
                if i > 0 {
                    out.push(b',');
                    if !self.gap.is_empty() {
                        out.push(b'\n');
                    }
                }
                if !self.gap.is_empty() {
                    out.extend_from_slice(&self.indent);
                }
                let key = index_key_string(self.ctx, i)?;
                let wrote = self.str(key, value, out)?;
                if !wrote {
                    out.extend_from_slice(b"null");
                }
            }
            if !self.gap.is_empty() {
                out.push(b'\n');
                out.extend_from_slice(&stepback);
            }
        }

        out.push(b']');
        self.indent = stepback;
        self.stack.pop();
        Ok(())
    }

    fn serialize_object(&mut self, value: JSValue, out: &mut Vec<u8>) -> Result<(), ()> {
        self.check_circular(value)?;
        self.stack.push(value);

        let stepback = self.indent.clone();
        self.indent.extend_from_slice(&self.gap);
        out.push(b'{');

        let keys = match &self.replacer {
            Some(JsonReplacer::PropertyList(list)) => list.clone(),
            _ => collect_object_keys(self.ctx, value)?,
        };

        let mut first = true;
        for key in keys {
            let key_str = key_to_string_value(self.ctx, key)?;
            let mut prop_buf = Vec::new();
            if !self.str(key_str, value, &mut prop_buf)? {
                continue;
            }
            if first {
                if !self.gap.is_empty() {
                    out.push(b'\n');
                }
                first = false;
            } else {
                out.push(b',');
                if !self.gap.is_empty() {
                    out.push(b'\n');
                }
            }
            if !self.gap.is_empty() {
                out.extend_from_slice(&self.indent);
            }
            stringify_string(self.ctx, key_str, out)?;
            if self.gap.is_empty() {
                out.push(b':');
            } else {
                out.extend_from_slice(b": ");
            }
            out.extend_from_slice(&prop_buf);
        }

        if !first && !self.gap.is_empty() {
            out.push(b'\n');
            out.extend_from_slice(&stepback);
        }
        out.push(b'}');

        self.indent = stepback;
        self.stack.pop();
        Ok(())
    }

    fn check_circular(&mut self, value: JSValue) -> Result<(), ()> {
        for &entry in &self.stack {
            if crate::jsvalue::raw_bits(entry) == crate::jsvalue::raw_bits(value) {
                let _ = self.ctx.throw_type_error("circular reference");
                return Err(());
            }
        }
        Ok(())
    }
}

fn build_json_gap(ctx: &mut JSContext, space: JSValue) -> Result<Vec<u8>, ()> {
    let mut space_val = space;
    if let Some(primitive) = boxed_primitive_value(space_val) {
        space_val = primitive;
    }

    if space_val.is_number() {
        let n = conversion::to_number(ctx, space_val).map_err(|_| ())?;
        let mut count = if n.is_nan() || n <= 0.0 {
            0
        } else if n.is_infinite() {
            10
        } else {
            n.trunc().min(10.0) as usize
        };
        if count > 10 {
            count = 10;
        }
        return Ok(vec![b' '; count]);
    }

    if space_val.is_string() {
        let len = ctx.string_len(space_val);
        let keep = core::cmp::min(10, len) as usize;
        let mut out = Vec::new();
        for i in 0..keep as u32 {
            let c = ctx.string_getc(space_val, i) as u32;
            let mut buf = [0u8; 4];
            let n = crate::cutils::unicode_to_utf8(&mut buf, c);
            out.extend_from_slice(&buf[..n]);
        }
        return Ok(out);
    }

    Ok(Vec::new())
}

fn build_json_replacer(ctx: &mut JSContext, replacer: JSValue) -> Result<Option<JsonReplacer>, ()> {
    if replacer.is_function() {
        return Ok(Some(JsonReplacer::Function(replacer)));
    }
    let Some((_, len)) = get_array_info(replacer) else {
        return Ok(None);
    };
    let mut property_list = Vec::new();
    for i in 0..len {
        let elem = get_property(ctx, replacer, new_short_int(i as i32)).map_err(|_| ())?;
        let mut key_val = elem;
        if let Some(primitive) = boxed_primitive_value(key_val) {
            key_val = primitive;
        }
        if !key_val.is_string() && !key_val.is_number() {
            continue;
        }
        let prop_key = conversion::to_property_key(ctx, key_val).map_err(|_| ())?;
        if !property_list
            .iter()
            .any(|existing| crate::jsvalue::raw_bits(*existing) == crate::jsvalue::raw_bits(prop_key))
        {
            property_list.push(prop_key);
        }
    }
    Ok(Some(JsonReplacer::PropertyList(property_list)))
}

fn collect_object_keys(ctx: &mut JSContext, obj: JSValue) -> Result<Vec<JSValue>, ()> {
    let keys = object_keys(ctx, obj).map_err(|_| ())?;
    let (_, key_count) = get_array_info(keys).unwrap_or((JS_NULL, 0));
    let mut out = Vec::with_capacity(key_count as usize);
    for i in 0..key_count {
        let key = get_property(ctx, keys, new_short_int(i as i32)).map_err(|_| ())?;
        out.push(key);
    }
    Ok(out)
}

fn key_to_string_value(ctx: &mut JSContext, key: JSValue) -> Result<JSValue, ()> {
    let mut scratch = [0u8; 5];
    if string_view(key, &mut scratch).is_some() {
        return Ok(key);
    }
    conversion::to_string(ctx, key).map_err(|_| ())
}

fn index_key_string(ctx: &mut JSContext, index: u32) -> Result<JSValue, ()> {
    ctx.new_string(&index.to_string()).map_err(|_| ())
}

fn number_to_f64(val: JSValue) -> Option<f64> {
    if is_int(val) {
        return Some(f64::from(value_get_int(val)));
    }
    #[cfg(target_pointer_width = "64")]
    if is_short_float(val) {
        return Some(short_float_to_f64(val));
    }
    read_float64(val)
}

fn boxed_primitive_value(val: JSValue) -> Option<JSValue> {
    let obj_ptr = value_to_ptr::<Object>(val)?;
    let header_word = unsafe { ptr::read_unaligned(obj_ptr.as_ptr().cast::<JSWord>()) };
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

fn stringify_number(d: f64, result: &mut Vec<u8>) -> Result<(), ()> {
    if !d.is_finite() {
        result.extend_from_slice(b"null");
    } else {
        let s = match crate::dtoa::js_dtoa(d, 10, 0, crate::dtoa::JS_DTOA_FORMAT_FREE) {
            Ok(s) => s,
            Err(_) => return Err(()),
        };
        result.extend_from_slice(s.as_bytes());
    }
    Ok(())
}

fn stringify_string(ctx: &mut JSContext, val: JSValue, result: &mut Vec<u8>) -> Result<(), ()> {
    result.push(b'"');
    let len = ctx.string_len(val);
    for i in 0..len {
        let c = ctx.string_getc(val, i) as u32;
        match c {
            0x22 => result.extend_from_slice(b"\\\""), // "
            0x5C => result.extend_from_slice(b"\\\\"), // \
            0x0A => result.extend_from_slice(b"\\n"),  // newline
            0x0D => result.extend_from_slice(b"\\r"),  // carriage return
            0x09 => result.extend_from_slice(b"\\t"),  // tab
            0x08 => result.extend_from_slice(b"\\b"),  // backspace
            0x0C => result.extend_from_slice(b"\\f"),  // form feed
            c if c < 0x20 => {
                // Other control characters
                result.extend_from_slice(b"\\u00");
                result.push(b"0123456789abcdef"[(c >> 4) as usize]);
                result.push(b"0123456789abcdef"[(c & 0xf) as usize]);
            }
            c => {
                let mut buf = [0u8; 4];
                let n = crate::cutils::unicode_to_utf8(&mut buf, c);
                result.extend_from_slice(&buf[..n]);
            }
        }
    }
    result.push(b'"');
    Ok(())
}

// ---------------------------------------------------------------------------
// TypedArray / ArrayBuffer builtins
// ---------------------------------------------------------------------------

use crate::array_buffer::ArrayBuffer;
use crate::containers::ByteArrayHeader;
use crate::typed_array::TypedArray;

// Size log2 for each TypedArray type (indexed by class_id - Uint8CArray)
// Uint8C, Int8, Uint8, Int16, Uint16, Int32, Uint32, Float32, Float64
const TYPED_ARRAY_SIZE_LOG2: [u8; 9] = [0, 0, 0, 1, 1, 2, 2, 2, 3];

fn to_index(ctx: &mut JSContext, val: JSValue) -> Result<u64, ()> {
    if is_undefined(val) {
        return Ok(0);
    }
    let n = conversion::to_number(ctx, val).map_err(|_| ())?;
    if n < 0.0 || n.is_nan() || n.is_infinite() || n > (u32::MAX as f64) {
        return Err(());
    }
    Ok(n as u64)
}

fn get_array_buffer(val: JSValue) -> Option<(NonNull<Object>, ArrayBuffer)> {
    let obj_ptr = value_to_ptr::<Object>(val)?;
    let header_word = unsafe { ptr::read_unaligned(obj_ptr.as_ptr().cast::<JSWord>()) };
    let header = ObjectHeader::from_word(header_word);
    if header.tag() != MTag::Object || header.class_id() != JSObjectClass::ArrayBuffer as u8 {
        return None;
    }
    let data = unsafe {
        let payload = Object::payload_ptr(obj_ptr.as_ptr());
        ptr::read_unaligned(core::ptr::addr_of!((*payload).array_buffer))
    };
    Some((obj_ptr, data))
}

fn get_byte_array_size(byte_buffer: JSValue) -> Option<u32> {
    let ptr = value_to_ptr::<u8>(byte_buffer)?;
    let header_word = unsafe { ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>()) };
    let header = MbHeader::from_word(header_word);
    if header.tag() != MTag::ByteArray {
        return None;
    }
    Some(ByteArrayHeader::from(header).size() as u32)
}

fn get_typed_array(val: JSValue) -> Option<(NonNull<Object>, u8, TypedArray)> {
    let obj_ptr = value_to_ptr::<Object>(val)?;
    let header_word = unsafe { ptr::read_unaligned(obj_ptr.as_ptr().cast::<JSWord>()) };
    let header = ObjectHeader::from_word(header_word);
    if header.tag() != MTag::Object {
        return None;
    }
    let class_id = header.class_id();
    if !(JSObjectClass::Uint8CArray as u8..=JSObjectClass::Float64Array as u8).contains(&class_id) {
        return None;
    }
    let data = unsafe {
        let payload = Object::payload_ptr(obj_ptr.as_ptr());
        ptr::read_unaligned(core::ptr::addr_of!((*payload).typed_array))
    };
    Some((obj_ptr, class_id, data))
}

pub fn js_array_buffer_constructor(
    ctx: &mut JSContext,
    _this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    let len = match to_index(ctx, args.first().copied().unwrap_or(JS_UNDEFINED)) {
        Ok(l) => l as usize,
        Err(_) => return JS_EXCEPTION, // RangeError: invalid array buffer length
    };
    ctx.alloc_array_buffer(len).unwrap_or(JS_EXCEPTION)
}

pub fn js_array_buffer_get_byteLength(
    _ctx: &mut JSContext,
    this_val: JSValue,
    _args: &[JSValue],
) -> JSValue {
    let Some((_, data)) = get_array_buffer(this_val) else {
        return JS_EXCEPTION; // TypeError: expected an ArrayBuffer
    };
    let Some(size) = get_byte_array_size(data.byte_buffer()) else {
        return JS_EXCEPTION;
    };
    new_short_int(size as i32)
}

pub fn js_typed_array_base_constructor(
    _ctx: &mut JSContext,
    _this_val: JSValue,
    _args: &[JSValue],
) -> JSValue {
    // TypedArray base constructor cannot be called directly
    JS_EXCEPTION
}

fn typed_array_constructor_from_array(
    ctx: &mut JSContext,
    source: JSValue,
    class_id: JSObjectClass,
) -> JSValue {
    // Get length from source array/typed array
    let len = if let Some((_, len)) = get_array_info(source) {
        len
    } else if let Some((_, _, ta)) = get_typed_array(source) {
        ta.len()
    } else {
        return JS_EXCEPTION;
    };

    // Create new TypedArray with that length
    let size_log2 = TYPED_ARRAY_SIZE_LOG2[(class_id as u8 - JSObjectClass::Uint8CArray as u8) as usize];
    let byte_len = (len as usize) << size_log2;
    let buffer = match ctx.alloc_array_buffer(byte_len) {
        Ok(b) => b,
        Err(_) => return JS_EXCEPTION,
    };

    let typed_array = match ctx.alloc_typed_array(class_id, buffer, 0, len) {
        Ok(ta) => ta,
        Err(_) => return JS_EXCEPTION,
    };

    // Copy elements from source
    for i in 0..len {
        let elem = match get_property(ctx, source, new_short_int(i as i32)) {
            Ok(v) => v,
            Err(_) => return JS_EXCEPTION,
        };
        if crate::property::set_property(ctx, typed_array, new_short_int(i as i32), elem).is_err() {
            return JS_EXCEPTION;
        }
    }

    typed_array
}

pub fn js_typed_array_constructor(
    ctx: &mut JSContext,
    _this_val: JSValue,
    args: &[JSValue],
    magic: i32,
) -> JSValue {
    let class_id = match u8::try_from(magic) {
        Ok(id) if (JSObjectClass::Uint8CArray as u8..=JSObjectClass::Float64Array as u8).contains(&id) => {
            // Safety: we've verified the range
            unsafe { core::mem::transmute::<u8, JSObjectClass>(id) }
        }
        _ => return JS_EXCEPTION,
    };

    let size_log2 = TYPED_ARRAY_SIZE_LOG2[(class_id as u8 - JSObjectClass::Uint8CArray as u8) as usize];
    let arg0 = args.first().copied().unwrap_or(JS_UNDEFINED);

    // Case 1: Not an object - treat as length
    if !arg0.is_object() {
        let len = match to_index(ctx, arg0) {
            Ok(l) => l,
            Err(_) => return JS_EXCEPTION,
        };
        let byte_len = (len as usize) << size_log2;
        let buffer = match ctx.alloc_array_buffer(byte_len) {
            Ok(b) => b,
            Err(_) => return JS_EXCEPTION,
        };
        return ctx.alloc_typed_array(class_id, buffer, 0, len as u32).unwrap_or(JS_EXCEPTION);
    }

    // Case 2: ArrayBuffer argument - create view
    if let Some((_, ab_data)) = get_array_buffer(arg0) {
        let byte_length = match get_byte_array_size(ab_data.byte_buffer()) {
            Some(s) => s as u64,
            None => return JS_EXCEPTION,
        };

        let offset = match to_index(ctx, args.get(1).copied().unwrap_or(JS_UNDEFINED)) {
            Ok(o) => o,
            Err(_) => return JS_EXCEPTION,
        };

        // Check offset alignment and bounds
        if (offset & ((1 << size_log2) - 1)) != 0 || offset > byte_length {
            return JS_EXCEPTION; // RangeError: invalid offset
        }

        let len = if args.get(2).copied().unwrap_or(JS_UNDEFINED) == JS_UNDEFINED {
            // If length not specified, use remaining buffer
            if (byte_length & ((1 << size_log2) - 1)) != 0 {
                return JS_EXCEPTION; // RangeError: invalid length
            }
            (byte_length - offset) >> size_log2
        } else {
            let l = match to_index(ctx, args[2]) {
                Ok(l) => l,
                Err(_) => return JS_EXCEPTION,
            };
            if offset + (l << size_log2) > byte_length {
                return JS_EXCEPTION; // RangeError: invalid length
            }
            l
        };

        // Offset is in elements for TypedArray struct
        let offset_elements = (offset >> size_log2) as u32;
        return ctx.alloc_typed_array(class_id, arg0, offset_elements, len as u32).unwrap_or(JS_EXCEPTION);
    }

    // Case 3: Array or TypedArray argument - copy elements
    if get_array_info(arg0).is_some() || get_typed_array(arg0).is_some() {
        return typed_array_constructor_from_array(ctx, arg0, class_id);
    }

    JS_EXCEPTION
}

pub fn js_typed_array_get_length(
    _ctx: &mut JSContext,
    this_val: JSValue,
    _args: &[JSValue],
    magic: i32,
) -> JSValue {
    let Some((_, class_id, ta)) = get_typed_array(this_val) else {
        return JS_EXCEPTION; // TypeError: not a TypedArray
    };
    let size_log2 = TYPED_ARRAY_SIZE_LOG2[(class_id - JSObjectClass::Uint8CArray as u8) as usize];
    
    match magic {
        0 => new_short_int(ta.len() as i32),                           // length
        1 => new_short_int((ta.len() << size_log2) as i32),            // byteLength
        2 => new_short_int((ta.offset() << size_log2) as i32),         // byteOffset
        3 => ta.buffer(),                                               // buffer
        _ => JS_EXCEPTION,
    }
}

pub fn js_typed_array_subarray(
    ctx: &mut JSContext,
    this_val: JSValue,
    args: &[JSValue],
) -> JSValue {
    let Some((_, class_id, ta)) = get_typed_array(this_val) else {
        return JS_EXCEPTION; // TypeError: not a TypedArray
    };
    
    let len = ta.len() as i32;
    
    // Get start index with negative wrapping
    let start = match to_int32_clamp_neg(ctx, args.first().copied().unwrap_or(JS_UNDEFINED), len) {
        Ok(s) => s,
        Err(_) => return JS_EXCEPTION,
    };
    
    // Get end index with negative wrapping
    let end = if args.get(1).copied().unwrap_or(JS_UNDEFINED) == JS_UNDEFINED {
        len
    } else {
        match to_int32_clamp_neg(ctx, args[1], len) {
            Ok(e) => e,
            Err(_) => return JS_EXCEPTION,
        }
    };
    
    // Calculate count (can't be negative)
    let count = (end - start).max(0) as u32;
    
    // New offset = old offset + start
    let new_offset = ta.offset() + start as u32;
    
    // Create new TypedArray with same buffer
    let class = unsafe { core::mem::transmute::<u8, JSObjectClass>(class_id) };
    ctx.alloc_typed_array(class, ta.buffer(), new_offset, count).unwrap_or(JS_EXCEPTION)
}

fn is_error(val: JSValue) -> bool {
    if !is_ptr(val) {
        return false;
    }
    let obj_ptr = match value_to_ptr::<Object>(val) {
        Some(p) => p,
        None => return false,
    };
    let header_word = unsafe { ptr::read_unaligned(obj_ptr.as_ptr().cast::<JSWord>()) };
    let header = ObjectHeader::from_word(header_word);
    if header.tag() != MTag::Object {
        return false;
    }
    let class_id = header.class_id();
    class_id == JSObjectClass::Error as u8
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::{ContextConfig, JSContext};
    use crate::jsvalue::{
        is_bool, new_short_int, value_get_special_value, JSWord, JS_EXCEPTION, JS_FALSE,
        JS_TRUE, JS_UNDEFINED,
    };
    use crate::object::{Object, ObjectHeader};
    use crate::parser::regexp::compile_regexp;
    use crate::parser::regexp_flags::LRE_FLAG_GLOBAL;
    use crate::stdlib::MQUICKJS_STDLIB_IMAGE;
    use core::ptr;

    fn new_context() -> JSContext {
        JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 32 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init")
    }

    fn string_from_value(val: JSValue) -> String {
        let mut scratch = [0u8; 5];
        let view = string_view(val, &mut scratch).expect("string view");
        core::str::from_utf8(view.bytes())
            .expect("utf8")
            .to_string()
    }

    fn object_class_id(val: JSValue) -> u8 {
        let obj_ptr = value_to_ptr::<Object>(val).expect("object ptr");
        let header_word = unsafe { ptr::read_unaligned(obj_ptr.as_ptr().cast::<JSWord>()) };
        ObjectHeader::from_word(header_word).class_id()
    }

    fn make_regexp(ctx: &mut JSContext, pattern: &[u8], flags: u32) -> JSValue {
        let source = ctx.new_string_len(pattern).expect("pattern string");
        let bytecode = compile_regexp(pattern, flags).expect("regexp compile");
        let bytecode_val = ctx
            .alloc_byte_array(bytecode.bytes())
            .expect("bytecode array");
        crate::regexp::new_regexp_object(ctx, source, bytecode_val).expect("regexp object")
    }

    fn assert_string_bytes(val: JSValue, expected: &[u8]) {
        let mut scratch = [0u8; 5];
        let view = string_view(val, &mut scratch).expect("string view");
        assert_eq!(view.bytes(), expected);
    }

    #[test]
    fn number_formatting_matches_c() {
        let mut ctx = new_context();
        let value = ctx.new_float64(25.0).expect("number");
        let exp = js_number_toExponential(&mut ctx, value, &[]);
        assert_eq!(string_from_value(exp), "2.5e+1");

        let exp0 = js_number_toExponential(&mut ctx, value, &[new_short_int(0)]);
        assert_eq!(string_from_value(exp0), "3e+1");

        let neg = ctx.new_float64(-25.0).expect("number");
        let exp_neg = js_number_toExponential(&mut ctx, neg, &[new_short_int(0)]);
        assert_eq!(string_from_value(exp_neg), "-3e+1");

        let prec_val = ctx.new_float64(2.5).unwrap();
        let prec = js_number_toPrecision(&mut ctx, prec_val, &[new_short_int(1)]);
        assert_eq!(string_from_value(prec), "3");

        let fixed_val = ctx.new_float64(1.125).unwrap();
        let fixed = js_number_toFixed(&mut ctx, fixed_val, &[new_short_int(2)]);
        assert_eq!(string_from_value(fixed), "1.13");

        let fixed_neg_val = ctx.new_float64(-1e-10).unwrap();
        let fixed_neg = js_number_toFixed(&mut ctx, fixed_neg_val, &[new_short_int(0)]);
        assert_eq!(string_from_value(fixed_neg), "-0");
    }

    #[test]
    fn number_parse_int_float_matches_c() {
        let mut ctx = new_context();
        let val = ctx.new_string("  123r").unwrap();
        let out = js_number_parseInt(&mut ctx, JS_UNDEFINED, &[val]);
        assert_eq!(to_number(out), 123.0);

        let hex = ctx.new_string("0x123").unwrap();
        let out = js_number_parseInt(&mut ctx, JS_UNDEFINED, &[hex]);
        assert_eq!(to_number(out), 0x123 as f64);

        let oct = ctx.new_string("0o123").unwrap();
        let out = js_number_parseInt(&mut ctx, JS_UNDEFINED, &[oct]);
        assert_eq!(to_number(out), 0.0);

        let hex_float = ctx.new_string("0x1234").unwrap();
        let out = js_number_parseFloat(&mut ctx, JS_UNDEFINED, &[hex_float]);
        assert_eq!(to_number(out), 0.0);

        let inf = ctx.new_string("Infinity").unwrap();
        let out = js_number_parseFloat(&mut ctx, JS_UNDEFINED, &[inf]);
        assert!(to_number(out).is_infinite());
    }

    #[test]
    fn boolean_constructor_matches_c() {
        let mut ctx = new_context();

        let out = js_boolean_constructor(&mut ctx, JS_UNDEFINED, &[]);
        assert!(is_bool(out));
        assert_eq!(value_get_special_value(out), 0);

        let out = js_boolean_constructor(&mut ctx, JS_UNDEFINED, &[JS_TRUE]);
        assert_eq!(value_get_special_value(out), 1);

        let out = js_boolean_constructor(&mut ctx, JS_UNDEFINED, &[JS_FALSE]);
        assert_eq!(value_get_special_value(out), 0);

        let out = js_boolean_constructor(&mut ctx, JS_UNDEFINED, &[new_short_int(0)]);
        assert_eq!(value_get_special_value(out), 0);

        let empty = ctx.new_string("").unwrap();
        let out = js_boolean_constructor(&mut ctx, JS_UNDEFINED, &[empty]);
        assert_eq!(value_get_special_value(out), 0);

        let nonempty = ctx.new_string("0").unwrap();
        let out = js_boolean_constructor(&mut ctx, JS_UNDEFINED, &[nonempty]);
        assert_eq!(value_get_special_value(out), 1);

        let nan = ctx.new_float64(f64::NAN).unwrap();
        let out = js_boolean_constructor(&mut ctx, JS_UNDEFINED, &[nan]);
        assert_eq!(value_get_special_value(out), 0);

        let obj = ctx.alloc_object_default().unwrap();
        let out = js_boolean_constructor(&mut ctx, JS_UNDEFINED, &[obj]);
        assert_eq!(value_get_special_value(out), 1);
    }

    #[test]
    fn math_helpers_match_c() {
        let mut ctx = new_context();
        let min = js_math_min_max(&mut ctx, JS_UNDEFINED, &[new_short_int(3), new_short_int(1)], 0);
        assert_eq!(value_get_int(min), 1);

        let max = js_math_min_max(&mut ctx, JS_UNDEFINED, &[new_short_int(3), new_short_int(1)], 1);
        assert_eq!(value_get_int(max), 3);

        let imul = js_math_imul(
            &mut ctx,
            JS_UNDEFINED,
            &[new_short_int(0x12345678), new_short_int(123)],
        );
        assert_eq!(to_number(imul), -1088058456.0);

        let clz = js_math_clz32(&mut ctx, JS_UNDEFINED, &[new_short_int(1)]);
        assert_eq!(value_get_int(clz), 31);
    }

    #[test]
    fn string_regexp_match_and_search() {
        let mut ctx = new_context();
        let input = ctx.new_string("abbbc").expect("input string");
        let re = make_regexp(&mut ctx, b"b+", 0);
        let match_val = js_string_match(&mut ctx, input, &[re]);
        let first = get_property(&mut ctx, match_val, new_short_int(0)).expect("match[0]");
        assert_string_bytes(first, b"bbb");

        let input_global = ctx.new_string("abcaaad").expect("input string");
        let re_global = make_regexp(&mut ctx, b"a+", LRE_FLAG_GLOBAL);
        let matches = js_string_match(&mut ctx, input_global, &[re_global]);
        let len = get_array_info(matches).expect("match array").1;
        assert_eq!(len, 2);
        let m0 = get_property(&mut ctx, matches, new_short_int(0)).expect("match[0]");
        let m1 = get_property(&mut ctx, matches, new_short_int(1)).expect("match[1]");
        assert_string_bytes(m0, b"a");
        assert_string_bytes(m1, b"aaa");

        let input_search = ctx.new_string("abc").expect("input string");
        let re_b = make_regexp(&mut ctx, b"b", 0);
        let search = js_string_search(&mut ctx, input_search, &[re_b]);
        assert_eq!(value_get_int(search), 1);

        let re_d = make_regexp(&mut ctx, b"d", 0);
        let search = js_string_search(&mut ctx, input_search, &[re_d]);
        assert_eq!(value_get_int(search), -1);
    }

    #[test]
    fn string_regexp_replace() {
        let mut ctx = new_context();
        let input = ctx.new_string("abbbbcbbd").expect("input string");
        let re = make_regexp(&mut ctx, b"b+", 0);

        let mut euro = [0u8; 4];
        let euro_len = crate::cutils::unicode_to_utf8(&mut euro, 0x20ac);
        let mut replace_bytes = Vec::new();
        replace_bytes.extend_from_slice(&euro[..euro_len]);
        replace_bytes.extend_from_slice(b"$&");
        let replace = ctx.new_string_len(&replace_bytes).expect("replace string");

        let out = js_string_replace(&mut ctx, input, &[re, replace], 0);
        let mut expected = Vec::new();
        expected.extend_from_slice(b"a");
        expected.extend_from_slice(&euro[..euro_len]);
        expected.extend_from_slice(b"bbbbcbbd");
        assert_string_bytes(out, &expected);

        let input_global = ctx.new_string("abbbbcbbd").expect("input string");
        let re_global = make_regexp(&mut ctx, b"b+", LRE_FLAG_GLOBAL);
        let out = js_string_replace(&mut ctx, input_global, &[re_global, replace], 0);
        let mut expected_global = Vec::new();
        expected_global.extend_from_slice(b"a");
        expected_global.extend_from_slice(&euro[..euro_len]);
        expected_global.extend_from_slice(b"bbbbc");
        expected_global.extend_from_slice(&euro[..euro_len]);
        expected_global.extend_from_slice(b"bbd");
        assert_string_bytes(out, &expected_global);

        let input_caps = ctx.new_string("abbbbccccd").expect("input string");
        let re_caps = make_regexp(&mut ctx, b"(b+)(c+)", LRE_FLAG_GLOBAL);
        let replace_caps = ctx.new_string("_$1_$2_").expect("replace string");
        let out = js_string_replace(&mut ctx, input_caps, &[re_caps, replace_caps], 0);
        assert_string_bytes(out, b"a_bbbb_cccc_d");

        let input_ctx = ctx.new_string("abbbbcd").expect("input string");
        let re_ctx = make_regexp(&mut ctx, b"b+", LRE_FLAG_GLOBAL);
        let replace_ctx = ctx.new_string("_$`_$&_$'_").expect("replace string");
        let out = js_string_replace(&mut ctx, input_ctx, &[re_ctx, replace_ctx], 0);
        assert_string_bytes(out, b"a_a_bbbb_cd_cd");
    }

    #[test]
    fn string_regexp_split() {
        let mut ctx = new_context();
        let input = ctx.new_string("abc").expect("input string");
        let re = make_regexp(&mut ctx, b"b", 0);
        let out = js_string_split(&mut ctx, input, &[re]);
        if !is_ptr(out) {
            panic!(
                "split returned non-pointer tag {}",
                value_get_special_tag(out)
            );
        }
        let len = match get_array_info(out) {
            Some((_, len)) => len,
            None => {
                let ptr = value_to_ptr::<u8>(out).expect("split ptr");
                let header_word =
                    unsafe { ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>()) };
                let header = MbHeader::from_word(header_word);
                let class_id = if header.tag() == MTag::Object {
                    ObjectHeader::from_word(header_word).class_id()
                } else {
                    0
                };
                panic!("split returned tag {:?} class {}", header.tag(), class_id);
            }
        };
        assert_eq!(len, 2);
        let a0 = get_property(&mut ctx, out, new_short_int(0)).expect("split[0]");
        let a1 = get_property(&mut ctx, out, new_short_int(1)).expect("split[1]");
        assert_string_bytes(a0, b"a");
        assert_string_bytes(a1, b"c");

        let input_empty = ctx.new_string("ab").expect("input string");
        let re_empty = make_regexp(&mut ctx, b"a*", LRE_FLAG_GLOBAL);
        let out = js_string_split(&mut ctx, input_empty, &[re_empty]);
        let len = get_array_info(out).expect("split array a*").1;
        assert_eq!(len, 2);
        let b0 = get_property(&mut ctx, out, new_short_int(0)).expect("split[0]");
        let b1 = get_property(&mut ctx, out, new_short_int(1)).expect("split[1]");
        assert_string_bytes(b0, b"");
        assert_string_bytes(b1, b"b");

        let input_lazy = ctx.new_string("ab").expect("input string");
        let re_lazy = make_regexp(&mut ctx, b"a*?", LRE_FLAG_GLOBAL);
        let out = js_string_split(&mut ctx, input_lazy, &[re_lazy]);
        let len = get_array_info(out).expect("split array a*?").1;
        assert_eq!(len, 2);
        let c0 = get_property(&mut ctx, out, new_short_int(0)).expect("split[0]");
        let c1 = get_property(&mut ctx, out, new_short_int(1)).expect("split[1]");
        assert_string_bytes(c0, b"a");
        assert_string_bytes(c1, b"b");

        let input_tags =
            ctx.new_string("A<B>bold</B>and<CODE>coded</CODE>")
                .expect("input string");
        let tag_pattern = b"<(\\/)?([^<>]+)>";
        let re_tags = make_regexp(&mut ctx, tag_pattern, 0);
        let out = js_string_split(&mut ctx, input_tags, &[re_tags]);
        if out == JS_EXCEPTION {
            panic!("split tags returned exception");
        }
        if !is_ptr(out) {
            panic!(
                "split tags returned non-pointer tag {}",
                value_get_special_tag(out)
            );
        }
        let len = get_array_info(out).expect("split array tags").1;
        assert_eq!(len, 13);
        let d0 = get_property(&mut ctx, out, new_short_int(0)).expect("split[0]");
        let d1 = get_property(&mut ctx, out, new_short_int(1)).expect("split[1]");
        let d2 = get_property(&mut ctx, out, new_short_int(2)).expect("split[2]");
        let d3 = get_property(&mut ctx, out, new_short_int(3)).expect("split[3]");
        let d4 = get_property(&mut ctx, out, new_short_int(4)).expect("split[4]");
        let d5 = get_property(&mut ctx, out, new_short_int(5)).expect("split[5]");
        let d6 = get_property(&mut ctx, out, new_short_int(6)).expect("split[6]");
        let d7 = get_property(&mut ctx, out, new_short_int(7)).expect("split[7]");
        let d8 = get_property(&mut ctx, out, new_short_int(8)).expect("split[8]");
        let d9 = get_property(&mut ctx, out, new_short_int(9)).expect("split[9]");
        let d10 = get_property(&mut ctx, out, new_short_int(10)).expect("split[10]");
        let d11 = get_property(&mut ctx, out, new_short_int(11)).expect("split[11]");
        let d12 = get_property(&mut ctx, out, new_short_int(12)).expect("split[12]");
        assert_string_bytes(d0, b"A");
        assert_string_bytes(d1, b"");
        assert_string_bytes(d2, b"B");
        assert_string_bytes(d3, b"bold");
        assert_string_bytes(d4, b"/");
        assert_string_bytes(d5, b"B");
        assert_string_bytes(d6, b"and");
        assert_string_bytes(d7, b"");
        assert_string_bytes(d8, b"CODE");
        assert_string_bytes(d9, b"coded");
        assert_string_bytes(d10, b"/");
        assert_string_bytes(d11, b"CODE");
        assert_string_bytes(d12, b"");
    }

    #[test]
    fn global_nan_finite_checks() {
        let mut ctx = new_context();
        let nan = js_global_isNaN(&mut ctx, JS_UNDEFINED, &[JS_UNDEFINED]);
        assert!(value_get_special_value(nan) != 0);
        let finite = js_global_isFinite(&mut ctx, JS_UNDEFINED, &[new_short_int(1)]);
        assert!(value_get_special_value(finite) != 0);
        let inf = ctx.new_string("Infinity").unwrap();
        let not_finite = js_global_isFinite(&mut ctx, JS_UNDEFINED, &[inf]);
        assert_eq!(value_get_special_value(not_finite), 0);
    }

    #[test]
    fn error_constructor_sets_message_and_stack() {
        let mut ctx = new_context();
        let msg = ctx.new_string("boom").expect("msg");
        let err = js_error_constructor(&mut ctx, JS_UNDEFINED, &[msg], JSObjectClass::TypeError as i32);
        assert!(is_error(err));
        assert_eq!(object_class_id(err), JSObjectClass::Error as u8);
        let message = ctx.get_error_message(err).expect("message");
        assert_eq!(string_from_value(message), "boom");
        let stack = ctx.get_error_stack(err).expect("stack");
        assert_eq!(string_from_value(stack), "");
    }

    #[test]
    fn build_backtrace_records_location_only() {
        let mut ctx = new_context();
        let proto = ctx.class_proto()[JSObjectClass::Error as usize];
        let err = ctx.alloc_error(proto).expect("error");
        let msg = ctx.new_string("nope").expect("msg");
        ctx.set_error_message(err, msg).expect("message");
        let location = crate::context::BacktraceLocation {
            filename: "test.js",
            line: 3,
            column: 5,
        };
        ctx.build_backtrace(err, Some(location), 0).expect("backtrace");
        let stack = ctx.get_error_stack(err).expect("stack");
        assert_eq!(string_from_value(stack), "    at test.js:3:5\n");
    }

    #[cfg(all(test, not(miri)))]
    // MIRI: unsupported operation: `clock_gettime` with `REALTIME` clocks not available when isolation is enabled
    fn date_now_returns_number() {
        let mut ctx = new_context();
        let val = js_date_now(&mut ctx, JS_UNDEFINED, &[]);
        assert!(to_number(val) >= 0.0);
    }

    #[test]
    fn date_constructor_throws_type_error() {
        let mut ctx = new_context();
        let val = js_date_constructor(&mut ctx, JS_UNDEFINED, &[]);
        assert_eq!(val, JS_EXCEPTION);
        let err = ctx.take_current_exception();
        assert!(is_error(err));
        let message = ctx.get_error_message(err).expect("message");
        assert_eq!(string_from_value(message), "only Date.now() is supported");
    }

    #[test]
    fn math_sign_preserves_zero() {
        let minus_zero = -0.0f64;
        let out = js_math_sign(minus_zero);
        assert_eq!(float64_as_uint64(out), float64_as_uint64(minus_zero));
    }

    #[test]
    fn math_fround_rounds() {
        let out = js_math_fround(0.1);
        assert_eq!(out, 0.10000000149011612);
    }

    #[test]
    fn array_buffer_constructor_creates_buffer() {
        let mut ctx = new_context();
        let ab = js_array_buffer_constructor(&mut ctx, JS_UNDEFINED, &[new_short_int(16)]);
        assert_ne!(ab, JS_EXCEPTION);
        
        // Check byteLength
        let len = js_array_buffer_get_byteLength(&mut ctx, ab, &[]);
        assert_eq!(value_get_int(len), 16);
    }

    #[test]
    fn typed_array_constructor_from_length() {
        use crate::enums::JSObjectClass;
        let mut ctx = new_context();
        
        // Create Uint8Array with length 4
        let ta = js_typed_array_constructor(
            &mut ctx,
            JS_UNDEFINED,
            &[new_short_int(4)],
            JSObjectClass::Uint8Array as i32,
        );
        assert_ne!(ta, JS_EXCEPTION);
        
        // Check length
        let len = js_typed_array_get_length(&mut ctx, ta, &[], 0);
        assert_eq!(value_get_int(len), 4);
        
        // Check byteLength
        let byte_len = js_typed_array_get_length(&mut ctx, ta, &[], 1);
        assert_eq!(value_get_int(byte_len), 4);
    }

    #[test]
    fn typed_array_element_access() {
        use crate::enums::JSObjectClass;
        use crate::property::{get_property, set_property};
        
        let mut ctx = new_context();
        
        // Create Uint8Array with length 4
        let ta = js_typed_array_constructor(
            &mut ctx,
            JS_UNDEFINED,
            &[new_short_int(4)],
            JSObjectClass::Uint8Array as i32,
        );
        assert_ne!(ta, JS_EXCEPTION);
        
        // Set elements
        set_property(&mut ctx, ta, new_short_int(0), new_short_int(10)).unwrap();
        set_property(&mut ctx, ta, new_short_int(1), new_short_int(20)).unwrap();
        set_property(&mut ctx, ta, new_short_int(2), new_short_int(30)).unwrap();
        set_property(&mut ctx, ta, new_short_int(3), new_short_int(40)).unwrap();
        
        // Read elements back
        let v0 = get_property(&mut ctx, ta, new_short_int(0)).unwrap();
        assert_eq!(value_get_int(v0), 10);
        
        let v1 = get_property(&mut ctx, ta, new_short_int(1)).unwrap();
        assert_eq!(value_get_int(v1), 20);
        
        let v2 = get_property(&mut ctx, ta, new_short_int(2)).unwrap();
        assert_eq!(value_get_int(v2), 30);
        
        let v3 = get_property(&mut ctx, ta, new_short_int(3)).unwrap();
        assert_eq!(value_get_int(v3), 40);
    }

    #[test]
    fn typed_array_uint8_overflow() {
        use crate::enums::JSObjectClass;
        use crate::property::{get_property, set_property};
        
        let mut ctx = new_context();
        
        // Create Uint8Array
        let ta = js_typed_array_constructor(
            &mut ctx,
            JS_UNDEFINED,
            &[new_short_int(1)],
            JSObjectClass::Uint8Array as i32,
        );
        assert_ne!(ta, JS_EXCEPTION);
        
        // Set -1, should wrap to 255
        set_property(&mut ctx, ta, new_short_int(0), new_short_int(-1)).unwrap();
        let v = get_property(&mut ctx, ta, new_short_int(0)).unwrap();
        assert_eq!(value_get_int(v), 255);
    }

    #[test]
    fn typed_array_int8_signed() {
        use crate::enums::JSObjectClass;
        use crate::property::{get_property, set_property};
        
        let mut ctx = new_context();
        
        // Create Int8Array
        let ta = js_typed_array_constructor(
            &mut ctx,
            JS_UNDEFINED,
            &[new_short_int(1)],
            JSObjectClass::Int8Array as i32,
        );
        assert_ne!(ta, JS_EXCEPTION);
        
        // Set 255, should wrap to -1
        set_property(&mut ctx, ta, new_short_int(0), new_short_int(255)).unwrap();
        let v = get_property(&mut ctx, ta, new_short_int(0)).unwrap();
        assert_eq!(value_get_int(v), -1);
    }

    #[test]
    fn typed_array_uint8_clamped() {
        use crate::enums::JSObjectClass;
        use crate::property::{get_property, set_property};
        
        let mut ctx = new_context();
        
        // Create Uint8ClampedArray
        let ta = js_typed_array_constructor(
            &mut ctx,
            JS_UNDEFINED,
            &[new_short_int(2)],
            JSObjectClass::Uint8CArray as i32,
        );
        assert_ne!(ta, JS_EXCEPTION);
        
        // Set -100, should clamp to 0
        set_property(&mut ctx, ta, new_short_int(0), new_short_int(-100)).unwrap();
        let v0 = get_property(&mut ctx, ta, new_short_int(0)).unwrap();
        assert_eq!(value_get_int(v0), 0);
        
        // Set 1000, should clamp to 255
        set_property(&mut ctx, ta, new_short_int(1), new_short_int(1000)).unwrap();
        let v1 = get_property(&mut ctx, ta, new_short_int(1)).unwrap();
        assert_eq!(value_get_int(v1), 255);
    }

    #[test]
    fn typed_array_int32() {
        use crate::enums::JSObjectClass;
        use crate::property::{get_property, set_property};
        
        let mut ctx = new_context();
        
        // Create Int32Array
        let ta = js_typed_array_constructor(
            &mut ctx,
            JS_UNDEFINED,
            &[new_short_int(1)],
            JSObjectClass::Int32Array as i32,
        );
        assert_ne!(ta, JS_EXCEPTION);
        
        // Check byteLength (4 bytes per element)
        let byte_len = js_typed_array_get_length(&mut ctx, ta, &[], 1);
        assert_eq!(value_get_int(byte_len), 4);
        
        // Set and read
        set_property(&mut ctx, ta, new_short_int(0), new_short_int(-12345)).unwrap();
        let v = get_property(&mut ctx, ta, new_short_int(0)).unwrap();
        assert_eq!(value_get_int(v), -12345);
    }

    #[test]
    fn typed_array_subarray() {
        use crate::enums::JSObjectClass;
        use crate::property::{get_property, set_property};
        
        let mut ctx = new_context();
        
        // Create Uint8Array with 4 elements
        let ta = js_typed_array_constructor(
            &mut ctx,
            JS_UNDEFINED,
            &[new_short_int(4)],
            JSObjectClass::Uint8Array as i32,
        );
        assert_ne!(ta, JS_EXCEPTION);
        
        // Set values [1, 2, 3, 4]
        set_property(&mut ctx, ta, new_short_int(0), new_short_int(1)).unwrap();
        set_property(&mut ctx, ta, new_short_int(1), new_short_int(2)).unwrap();
        set_property(&mut ctx, ta, new_short_int(2), new_short_int(3)).unwrap();
        set_property(&mut ctx, ta, new_short_int(3), new_short_int(4)).unwrap();
        
        // Create subarray [1, 3) -> should be [2, 3]
        let sub = js_typed_array_subarray(&mut ctx, ta, &[new_short_int(1), new_short_int(3)]);
        assert_ne!(sub, JS_EXCEPTION);
        
        // Check length is 2
        let len = js_typed_array_get_length(&mut ctx, sub, &[], 0);
        assert_eq!(value_get_int(len), 2);
        
        // Check values are [2, 3]
        let v0 = get_property(&mut ctx, sub, new_short_int(0)).unwrap();
        assert_eq!(value_get_int(v0), 2);
        
        let v1 = get_property(&mut ctx, sub, new_short_int(1)).unwrap();
        assert_eq!(value_get_int(v1), 3);
    }

    #[test]
    fn typed_array_from_array_buffer() {
        use crate::enums::JSObjectClass;
        use crate::property::{get_property, set_property};
        
        let mut ctx = new_context();
        
        // Create ArrayBuffer with 16 bytes
        let ab = js_array_buffer_constructor(&mut ctx, JS_UNDEFINED, &[new_short_int(16)]);
        assert_ne!(ab, JS_EXCEPTION);
        
        // Create Uint32Array view from byte 12, length 1
        let ta = js_typed_array_constructor(
            &mut ctx,
            JS_UNDEFINED,
            &[ab, new_short_int(12), new_short_int(1)],
            JSObjectClass::Uint32Array as i32,
        );
        assert_ne!(ta, JS_EXCEPTION);
        
        // Check length is 1
        let len = js_typed_array_get_length(&mut ctx, ta, &[], 0);
        assert_eq!(value_get_int(len), 1);
        
        // Check byteOffset is 12
        let offset = js_typed_array_get_length(&mut ctx, ta, &[], 2);
        assert_eq!(value_get_int(offset), 12);
        
        // Set a value and check we can read it back
        set_property(&mut ctx, ta, new_short_int(0), new_short_int(42)).unwrap();
        let v = get_property(&mut ctx, ta, new_short_int(0)).unwrap();
        assert_eq!(value_get_int(v), 42);
    }

    #[test]
    fn regexp_constructor_creates_working_regexp() {
        let mut ctx = new_context();
        
        let pattern = ctx.new_string("a+").unwrap();
        let flags = ctx.new_string("g").unwrap();
        let re = js_regexp_constructor(&mut ctx, JS_UNDEFINED, &[pattern, flags]);
        assert_ne!(re, JS_EXCEPTION);
        
        // Check that it's a RegExp object by testing source and flags
        let source = js_regexp_get_source(&mut ctx, re, &[]);
        assert_string_bytes(source, b"a+");
        
        let flags_str = js_regexp_get_flags(&mut ctx, re, &[]);
        assert_string_bytes(flags_str, b"g");
    }

    #[test]
    fn regexp_lastindex_getter_setter() {
        let mut ctx = new_context();
        
        let pattern = ctx.new_string("x").unwrap();
        let flags = ctx.new_string("g").unwrap();
        let re = js_regexp_constructor(&mut ctx, JS_UNDEFINED, &[pattern, flags]);
        assert_ne!(re, JS_EXCEPTION);
        
        // Initial lastIndex should be 0
        let idx = js_regexp_get_lastIndex(&mut ctx, re, &[]);
        assert!(is_int(idx));
        assert_eq!(value_get_int(idx), 0);
        
        // Set lastIndex to 5
        let result = js_regexp_set_lastIndex(&mut ctx, re, &[new_short_int(5)]);
        assert_eq!(result, JS_UNDEFINED);
        
        // Read back should be 5
        let idx2 = js_regexp_get_lastIndex(&mut ctx, re, &[]);
        assert_eq!(value_get_int(idx2), 5);
    }

    #[test]
    fn regexp_flags_str_output() {
        // Test the flag string generation
        assert_eq!(regexp_flags_str(0), b"");
        assert_eq!(regexp_flags_str(1), b"g");  // global
        assert_eq!(regexp_flags_str(2), b"i");  // ignoreCase
        assert_eq!(regexp_flags_str(3), b"gi"); // global + ignoreCase
        assert_eq!(regexp_flags_str(4), b"m");  // multiline
        assert_eq!(regexp_flags_str(7), b"gim"); // g + i + m
        assert_eq!(regexp_flags_str(0x3f), b"gimsuy"); // all flags
    }

    #[test]
    fn regexp_exec_returns_match_array() {
        use crate::property::get_property;
        
        let mut ctx = new_context();
        
        let pattern = ctx.new_string("a").unwrap();
        let re = js_regexp_constructor(&mut ctx, JS_UNDEFINED, &[pattern]);
        assert_ne!(re, JS_EXCEPTION);
        
        let input = ctx.new_string("cat").unwrap();
        let result = js_regexp_exec(&mut ctx, re, &[input], 0); // magic=0 for exec
        assert_ne!(result, JS_EXCEPTION);
        assert_ne!(result, JS_NULL);
        
        // Check the match at index 0
        let match_val = get_property(&mut ctx, result, new_short_int(0)).unwrap();
        assert_string_bytes(match_val, b"a");
        
        // Check the index property
        let index_key = ctx.intern_string(b"index").unwrap();
        let index = get_property(&mut ctx, result, index_key).unwrap();
        assert!(is_int(index));
        assert_eq!(value_get_int(index), 1); // 'a' is at position 1 in "cat"
    }

    #[test]
    fn regexp_test_returns_boolean() {
        let mut ctx = new_context();
        
        let pattern = ctx.new_string("x").unwrap();
        let re = js_regexp_constructor(&mut ctx, JS_UNDEFINED, &[pattern]);
        assert_ne!(re, JS_EXCEPTION);
        
        // Test match
        let input_match = ctx.new_string("fox").unwrap();
        let result_match = js_regexp_exec(&mut ctx, re, &[input_match], 1); // magic=1 for test
        assert_eq!(result_match, JS_TRUE);
        
        // Test no match
        let input_no_match = ctx.new_string("cat").unwrap();
        let result_no_match = js_regexp_exec(&mut ctx, re, &[input_no_match], 1);
        assert_eq!(result_no_match, JS_FALSE);
    }

    #[test]
    fn regexp_global_updates_lastindex() {
        let mut ctx = new_context();
        
        let pattern = ctx.new_string("a").unwrap();
        let flags = ctx.new_string("g").unwrap();
        let re = js_regexp_constructor(&mut ctx, JS_UNDEFINED, &[pattern, flags]);
        assert_ne!(re, JS_EXCEPTION);
        
        let input = ctx.new_string("banana").unwrap();
        
        // First exec should find 'a' at index 1
        let _ = js_regexp_exec(&mut ctx, re, &[input], 0);
        let idx1 = js_regexp_get_lastIndex(&mut ctx, re, &[]);
        assert_eq!(value_get_int(idx1), 2);
        
        // Second exec should find 'a' at index 3
        let _ = js_regexp_exec(&mut ctx, re, &[input], 0);
        let idx2 = js_regexp_get_lastIndex(&mut ctx, re, &[]);
        assert_eq!(value_get_int(idx2), 4);
    }
}