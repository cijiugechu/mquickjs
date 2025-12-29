#![allow(non_snake_case)]

use crate::context::JSContext;
use crate::conversion;
use crate::cutils::{float64_as_uint64, uint64_as_float64};
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
use crate::object::{Object, ObjectHeader};
use crate::property::{
    define_property_getset, define_property_value, find_own_property_exposed, get_property,
    has_property, object_keys,
};
use crate::string::runtime::string_view;
use core::mem::size_of;
use core::ptr::{self, NonNull};
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
        return header.tag() == MTag::Float64;
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
    if !conversion::is_object(this_val) {
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

    if !conversion::is_object(obj) {
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
    if !conversion::is_object(obj) {
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

    if !conversion::is_object(obj) {
        return JS_EXCEPTION;
    }
    if proto != JS_NULL && !conversion::is_object(proto) {
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
    if proto != JS_NULL && !conversion::is_object(proto) {
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
    if !conversion::is_object(obj) {
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
    if !conversion::is_function(this_val) {
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
    if !conversion::is_function(this_val) {
        return JS_EXCEPTION;
    }
    let this_arg = args.first().copied().unwrap_or(JS_UNDEFINED);
    let args_array = args.get(1).copied().unwrap_or(JS_UNDEFINED);

    // Extract arguments from the array
    let call_args = if conversion::is_object(args_array) {
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
    if !conversion::is_function(this_val) {
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
            // Check if prototype property exists, otherwise create one
            let proto_key = match ctx.intern_string(b"prototype") {
                Ok(k) => k,
                Err(_) => return JS_EXCEPTION,
            };
            match get_property(ctx, this_val, proto_key) {
                Ok(val) if !is_undefined(val) => val,
                _ => {
                    // Create a new prototype object
                    let obj = match ctx.alloc_object_default() {
                        Ok(o) => o,
                        Err(_) => return JS_EXCEPTION,
                    };
                    // Set constructor property
                    let ctor_key = match ctx.intern_string(b"constructor") {
                        Ok(k) => k,
                        Err(_) => return JS_EXCEPTION,
                    };
                    if define_property_value(ctx, obj, ctor_key, this_val).is_err() {
                        return JS_EXCEPTION;
                    }
                    // Set prototype property on the function
                    if define_property_value(ctx, this_val, proto_key, obj).is_err() {
                        return JS_EXCEPTION;
                    }
                    obj
                }
            }
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
    if !conversion::is_function(this_val) {
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
    if !conversion::is_string(this_val) {
        return JS_EXCEPTION;
    }
    let len = ctx.string_len(this_val);
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
            if (b'A'..=b'Z').contains(&(c as u8)) && c < 128 {
                c + (b'a' - b'A') as u32
            } else {
                c
            }
        } else if (b'a'..=b'z').contains(&(c as u8)) && c < 128 {
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
            if c < 0 || c > 0x10ffff {
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

// Helper to convert this_val to string, handling objects
fn to_string_check_object(ctx: &mut JSContext, val: JSValue) -> Result<JSValue, ()> {
    if conversion::is_string(val) {
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
    let (len, has_init) = if args.len() == 1 && conversion::is_number(args[0]) {
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
    if !conversion::is_object(this_val) {
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
    let slice_len = if end > start { end - start } else { 0 };

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
    let obj = match ctx.alloc_error(proto, class_id) {
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

    // TODO: build_backtrace(ctx, obj)

    obj
}

pub fn js_error_toString(
    ctx: &mut JSContext,
    this_val: JSValue,
    _args: &[JSValue],
) -> JSValue {
    if !is_error(this_val) {
        return JS_EXCEPTION;
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
        return JS_EXCEPTION;
    }
    if magic == 0 {
        ctx.get_error_message(this_val).unwrap_or(JS_NULL)
    } else {
        ctx.get_error_stack(this_val).unwrap_or(JS_NULL)
    }
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
    (JSObjectClass::Error as u8..=JSObjectClass::InternalError as u8).contains(&class_id)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::{ContextConfig, JSContext};
    use crate::stdlib::MQUICKJS_STDLIB_IMAGE;

    fn new_context() -> JSContext {
        JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 32 * 1024,
            prepare_compilation: false,
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

    #[cfg(all(test, not(miri)))]
    // MIRI: unsupported operation: `clock_gettime` with `REALTIME` clocks not available when isolation is enabled
    fn date_now_returns_number() {
        let mut ctx = new_context();
        let val = js_date_now(&mut ctx, JS_UNDEFINED, &[]);
        assert!(to_number(val) >= 0.0);
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
}
