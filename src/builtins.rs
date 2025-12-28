#![allow(non_snake_case)]

use crate::context::JSContext;
use crate::cutils::{float64_as_uint64, uint64_as_float64};
use crate::dtoa::{
    js_atod, js_dtoa, AtodFlags, DtoaFlags, JS_ATOD_ACCEPT_BIN_OCT, JS_ATOD_INT_ONLY,
    JS_DTOA_EXP_DISABLED, JS_DTOA_EXP_ENABLED, JS_DTOA_FORMAT_FIXED, JS_DTOA_FORMAT_FRAC,
    JS_DTOA_FORMAT_FREE,
};
use crate::js_libm;
use crate::jsvalue::{
    is_bool, is_int, is_null, is_ptr, is_undefined, new_bool, new_short_int, value_get_int,
    value_get_special_tag, value_get_special_value, value_to_ptr, JSValue, JSWord, JS_EXCEPTION,
    JS_TAG_STRING_CHAR, JS_UNDEFINED,
};
#[cfg(target_pointer_width = "64")]
use crate::jsvalue::{is_short_float, short_float_to_f64};
use crate::memblock::{MbHeader, MTag};
use crate::string::runtime::string_view;
use core::mem::size_of;
use core::ptr;
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
