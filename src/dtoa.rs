//! Floating-point formatting and parsing helpers backed by third-party crates.
//!
//! Invariants carried over from the C implementation:
//! - `radix` is `2..=36`.
//! - `radix != 10` only supports the free format (no fixed or frac output).
//! - `JS_DTOA_FORMAT_FIXED` uses significant digits (`1..=JS_DTOA_MAX_DIGITS`).
//! - `JS_DTOA_FORMAT_FRAC` uses fractional digits (`0..=100` for JS usage).
//! - `JS_DTOA_MINUS_ZERO` controls whether `-0` preserves its sign.
//! - `JS_ATOD_INT_ONLY` forbids dots and exponents.
//! - `_` separators are allowed only when explicitly enabled and only between digits.

use bitflags::bitflags;
use core::num::{NonZeroU8, NonZeroUsize};

use lexical_core::write_float_options::RoundMode;
use lexical_core::{NumberFormatBuilder, ParseFloatOptions, WriteFloatOptions};

pub const JS_DTOA_MAX_DIGITS: usize = 101;

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct DtoaFlags: u32 {
        const FORMAT_FIXED = 1 << 0;
        const FORMAT_FRAC = 2;
        const EXP_ENABLED = 1 << 2;
        const EXP_DISABLED = 2 << 2;
        const MINUS_ZERO = 1 << 4;
    }
}

pub const JS_DTOA_FORMAT_FREE: DtoaFlags = DtoaFlags::empty();
pub const JS_DTOA_FORMAT_FIXED: DtoaFlags = DtoaFlags::FORMAT_FIXED;
pub const JS_DTOA_FORMAT_FRAC: DtoaFlags = DtoaFlags::FORMAT_FRAC;
const JS_DTOA_FORMAT_MASK: u32 = DtoaFlags::FORMAT_FIXED.bits() | DtoaFlags::FORMAT_FRAC.bits();

pub const JS_DTOA_EXP_AUTO: DtoaFlags = DtoaFlags::empty();
pub const JS_DTOA_EXP_ENABLED: DtoaFlags = DtoaFlags::EXP_ENABLED;
pub const JS_DTOA_EXP_DISABLED: DtoaFlags = DtoaFlags::EXP_DISABLED;
const JS_DTOA_EXP_MASK: u32 = DtoaFlags::EXP_ENABLED.bits() | DtoaFlags::EXP_DISABLED.bits();

pub const JS_DTOA_MINUS_ZERO: DtoaFlags = DtoaFlags::MINUS_ZERO;

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct AtodFlags: u32 {
        const INT_ONLY = 1 << 0;
        const ACCEPT_BIN_OCT = 1 << 1;
        const ACCEPT_LEGACY_OCTAL = 1 << 2;
        const ACCEPT_UNDERSCORES = 1 << 3;
    }
}

pub const JS_ATOD_INT_ONLY: AtodFlags = AtodFlags::INT_ONLY;
pub const JS_ATOD_ACCEPT_BIN_OCT: AtodFlags = AtodFlags::ACCEPT_BIN_OCT;
pub const JS_ATOD_ACCEPT_LEGACY_OCTAL: AtodFlags = AtodFlags::ACCEPT_LEGACY_OCTAL;
pub const JS_ATOD_ACCEPT_UNDERSCORES: AtodFlags = AtodFlags::ACCEPT_UNDERSCORES;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DtoaError {
    InvalidRadix,
    InvalidDigits,
    UnsupportedFormat,
    UnsupportedFlags,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AtodError {
    InvalidRadix,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct AtodResult {
    pub value: f64,
    pub next: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DtoaFormat {
    Free,
    Fixed,
    Frac,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DtoaExp {
    Auto,
    Enabled,
    Disabled,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExpKind {
    None,
    Decimal,
    Binary,
    Radix,
}

const DECIMAL_FORMAT: u128 = lexical_core::format::STANDARD;
const DECIMAL_NO_EXP_FORMAT: u128 = NumberFormatBuilder::new()
    .mantissa_radix(10)
    .exponent_base(NonZeroU8::new(10))
    .exponent_radix(NonZeroU8::new(10))
    .no_exponent_notation(true)
    .build_strict();
const DECIMAL_FORCE_EXP_FORMAT: u128 = NumberFormatBuilder::new()
    .mantissa_radix(10)
    .exponent_base(NonZeroU8::new(10))
    .exponent_radix(NonZeroU8::new(10))
    .required_exponent_notation(true)
    .build_strict();

const FREE_OPTIONS: WriteFloatOptions = WriteFloatOptions::builder()
    .trim_floats(true)
    .build_strict();

pub fn js_dtoa(
    d: f64,
    radix: u32,
    n_digits: usize,
    flags: DtoaFlags,
) -> Result<String, DtoaError> {
    let radix = normalize_radix(radix)?;
    let (format, exp, minus_zero) = decode_dtoa_flags(flags)?;

    if radix != 10 {
        if format != DtoaFormat::Free {
            return Err(DtoaError::UnsupportedFormat);
        }
        if exp != DtoaExp::Disabled {
            return Err(DtoaError::UnsupportedFlags);
        }
    }

    match format {
        DtoaFormat::Free => format_free(d, radix, exp, minus_zero),
        DtoaFormat::Fixed => format_fixed(d, radix, n_digits, exp, minus_zero),
        DtoaFormat::Frac => format_frac(d, radix, n_digits, minus_zero),
    }
}

pub fn js_atod(input: &str, radix: u32, flags: AtodFlags) -> Result<AtodResult, AtodError> {
    let mut radix = if radix == 0 {
        0
    } else if (2..=36).contains(&radix) {
        radix
    } else {
        return Err(AtodError::InvalidRadix);
    };
    let bytes = input.as_bytes();
    let len = bytes.len();
    if len == 0 {
        return Ok(AtodResult { value: f64::NAN, next: 0 });
    }

    let mut index = 0;
    let mut is_neg = false;
    match bytes[index] {
        b'+' => index += 1,
        b'-' => {
            is_neg = true;
            index += 1;
        }
        _ => {}
    }

    let start = index;
    if index < len && bytes[index] == b'0' {
        if index + 1 < len && (bytes[index + 1] == b'x' || bytes[index + 1] == b'X') {
            if radix == 0 || radix == 16 {
                index += 2;
                radix = 16;
            }
        } else if index + 1 < len
            && (bytes[index + 1] == b'o' || bytes[index + 1] == b'O')
            && radix == 0
            && flags.contains(JS_ATOD_ACCEPT_BIN_OCT)
        {
            index += 2;
            radix = 8;
        } else if index + 1 < len
            && (bytes[index + 1] == b'b' || bytes[index + 1] == b'B')
            && radix == 0
            && flags.contains(JS_ATOD_ACCEPT_BIN_OCT)
        {
            index += 2;
            radix = 2;
        } else if index + 1 < len
            && bytes[index + 1].is_ascii_digit()
            && radix == 0
            && flags.contains(JS_ATOD_ACCEPT_LEGACY_OCTAL)
        {
            let mut i = index + 1;
            while i < len && bytes[i] >= b'0' && bytes[i] <= b'7' {
                i += 1;
            }
            if i < len && (bytes[i] == b'8' || bytes[i] == b'9') {
                radix = 0;
            } else {
                index += 1;
                radix = 8;
            }
        }

        if radix != 0 && (index >= len || !is_digit_in_radix(bytes[index], radix)) {
            return Ok(AtodResult {
                value: f64::NAN,
                next: index,
            });
        }
    } else if !flags.contains(JS_ATOD_INT_ONLY)
        && bytes[start..].starts_with(b"Infinity")
    {
        let value = if is_neg { f64::NEG_INFINITY } else { f64::INFINITY };
        return Ok(AtodResult {
            value,
            next: start + "Infinity".len(),
        });
    }

    if radix == 0 {
        radix = 10;
    }

    let scan = scan_number(&bytes[index..], radix, flags);
    if let Some(error_pos) = scan.error_pos {
        return Ok(AtodResult {
            value: f64::NAN,
            next: index + error_pos,
        });
    }
    if !scan.saw_digit {
        return Ok(AtodResult {
            value: f64::NAN,
            next: index,
        });
    }

    let options = build_parse_options(scan.exp_kind, radix);
    let parsed = match scan.exp_kind {
        ExpKind::None | ExpKind::Decimal => parse_decimal_or_radix(radix, &scan.cleaned, &options),
        ExpKind::Radix => parse_radix_exp(radix, &scan.cleaned, &options),
        ExpKind::Binary => parse_binary_exp(radix, &scan.cleaned, &options),
    };

    let mut value = parsed.unwrap_or(f64::NAN);
    if is_neg {
        value = -value;
    }
    Ok(AtodResult {
        value,
        next: index + scan.end,
    })
}

fn normalize_radix(radix: u32) -> Result<u32, DtoaError> {
    if (2..=36).contains(&radix) {
        Ok(radix)
    } else {
        Err(DtoaError::InvalidRadix)
    }
}

fn decode_dtoa_flags(flags: DtoaFlags) -> Result<(DtoaFormat, DtoaExp, bool), DtoaError> {
    let format_bits = flags.bits() & JS_DTOA_FORMAT_MASK;
    let format = match format_bits {
        0 => DtoaFormat::Free,
        bits if bits == JS_DTOA_FORMAT_FIXED.bits() => DtoaFormat::Fixed,
        bits if bits == JS_DTOA_FORMAT_FRAC.bits() => DtoaFormat::Frac,
        _ => return Err(DtoaError::UnsupportedFlags),
    };
    let exp_bits = flags.bits() & JS_DTOA_EXP_MASK;
    let exp = match exp_bits {
        0 => DtoaExp::Auto,
        bits if bits == JS_DTOA_EXP_ENABLED.bits() => DtoaExp::Enabled,
        bits if bits == JS_DTOA_EXP_DISABLED.bits() => DtoaExp::Disabled,
        _ => return Err(DtoaError::UnsupportedFlags),
    };
    let minus_zero = flags.contains(JS_DTOA_MINUS_ZERO);
    Ok((format, exp, minus_zero))
}

fn format_free(d: f64, radix: u32, exp: DtoaExp, minus_zero: bool) -> Result<String, DtoaError> {
    if d.is_nan() {
        return Ok("NaN".to_string());
    }
    if d.is_infinite() {
        return Ok(if d.is_sign_negative() {
            "-Infinity".to_string()
        } else {
            "Infinity".to_string()
        });
    }

    let is_neg_zero = is_negative_zero(d);
    let needs_minus_zero = is_neg_zero && minus_zero;
    let value = if is_neg_zero { 0.0 } else { d };

    if radix == 10 {
        if exp == DtoaExp::Disabled {
            let mut out = write_decimal::<DECIMAL_NO_EXP_FORMAT>(value, &FREE_OPTIONS);
            if needs_minus_zero {
                out.insert(0, '-');
            }
            return Ok(out);
        }
        let mut buffer = ryu_js::Buffer::new();
        let mut out = buffer.format(value).to_string();
        let mut negative = false;
        if let Some(rest) = out.strip_prefix('-') {
            negative = true;
            out = rest.to_string();
        }
        if needs_minus_zero {
            negative = true;
        }
        if exp == DtoaExp::Enabled {
            if out.contains('e') || out.contains('E') {
                ensure_exponent_sign(&mut out, b'e');
            } else {
                out = to_exponential_from_decimal(&out);
            }
        }
        if negative {
            out.insert(0, '-');
        }
        return Ok(out);
    }

    let mut out = write_radix_no_exp(radix, value, &FREE_OPTIONS)?;
    if needs_minus_zero {
        out.insert(0, '-');
    }
    Ok(out)
}

fn to_exponential_from_decimal(input: &str) -> String {
    let mut digits = Vec::with_capacity(input.len());
    let mut decimal_pos = input.len();
    let mut saw_dot = false;
    for &b in input.as_bytes() {
        if b == b'.' {
            decimal_pos = digits.len();
            saw_dot = true;
            continue;
        }
        digits.push(b);
    }
    if !saw_dot {
        decimal_pos = digits.len();
    }
    let mut first_non_zero = None;
    for (i, &b) in digits.iter().enumerate() {
        if b != b'0' {
            first_non_zero = Some(i);
            break;
        }
    }
    let Some(start) = first_non_zero else {
        return "0e+0".to_string();
    };
    let mut end = digits.len();
    while end > start && digits[end - 1] == b'0' {
        end -= 1;
    }
    let exponent = decimal_pos as i32 - start as i32 - 1;
    let mut out = String::with_capacity(end - start + 8);
    out.push(digits[start] as char);
    if end - start > 1 {
        out.push('.');
        for &b in &digits[start + 1..end] {
            out.push(b as char);
        }
    }
    out.push('e');
    if exponent >= 0 {
        out.push('+');
    } else {
        out.push('-');
    }
    out.push_str(&exponent.abs().to_string());
    out
}

fn format_fixed(
    d: f64,
    radix: u32,
    n_digits: usize,
    exp: DtoaExp,
    minus_zero: bool,
) -> Result<String, DtoaError> {
    if radix != 10 {
        return Err(DtoaError::UnsupportedFormat);
    }
    if !(1..=JS_DTOA_MAX_DIGITS).contains(&n_digits) {
        return Err(DtoaError::InvalidDigits);
    }

    if d.is_nan() {
        return Ok("NaN".to_string());
    }
    if d.is_infinite() {
        return Ok(if d.is_sign_negative() {
            "-Infinity".to_string()
        } else {
            "Infinity".to_string()
        });
    }

    let is_neg_zero = is_negative_zero(d);
    let needs_minus_zero = is_neg_zero && minus_zero;
    let value = if is_neg_zero { 0.0 } else { d };

    let negative = d.is_sign_negative() && !is_neg_zero;
    let mut exp10 = 0i32;
    let mut digits = if value == 0.0 {
        vec![b'0'; n_digits]
    } else {
        let digits_plus = NonZeroUsize::new(n_digits + 1).ok_or(DtoaError::InvalidDigits)?;
        let options = WriteFloatOptions::builder()
            .min_significant_digits(Some(digits_plus))
            .max_significant_digits(Some(digits_plus))
            .round_mode(RoundMode::Truncate)
            .build_strict();
        let raw = write_decimal::<DECIMAL_FORCE_EXP_FORMAT>(value, &options);
        let (parsed_digits, parsed_exp) = parse_decimal_digits(&raw)?;
        exp10 = parsed_exp;
        parsed_digits
    };

    if digits.len() < n_digits + 1 {
        digits.extend(core::iter::repeat_n(b'0', n_digits + 1 - digits.len()));
    }

    let round_digit = digits[n_digits];
    digits.truncate(n_digits);
    if round_digit >= b'5' && round_up_digits(&mut digits) {
        exp10 += 1;
    }

    let use_exponent = match exp {
        DtoaExp::Enabled => true,
        DtoaExp::Disabled => false,
        DtoaExp::Auto => should_use_exponent_for_precision(exp10, n_digits),
    };

    let mut out = if use_exponent {
        format_exponent(&digits, exp10)
    } else {
        format_decimal(&digits, exp10)
    };

    if negative || needs_minus_zero {
        out.insert(0, '-');
    }
    Ok(out)
}

fn format_frac(
    d: f64,
    radix: u32,
    n_digits: usize,
    minus_zero: bool,
) -> Result<String, DtoaError> {
    if radix != 10 {
        return Err(DtoaError::UnsupportedFormat);
    }
    if n_digits > 100 {
        return Err(DtoaError::InvalidDigits);
    }

    if d.is_nan() {
        return Ok("NaN".to_string());
    }
    if d.is_infinite() {
        return Ok(if d.is_sign_negative() {
            "-Infinity".to_string()
        } else {
            "Infinity".to_string()
        });
    }

    let is_neg_zero = is_negative_zero(d);
    let needs_minus_zero = is_neg_zero && minus_zero;
    let value = if is_neg_zero { 0.0 } else { d };

    let mut buffer = ryu_js::Buffer::new();
    let mut out = buffer.format_to_fixed(value, n_digits as u8).to_string();
    if needs_minus_zero {
        out.insert(0, '-');
    }
    Ok(out)
}

fn is_negative_zero(d: f64) -> bool {
    d == 0.0 && d.is_sign_negative()
}

fn should_use_exponent_for_precision(exp10: i32, n_digits: usize) -> bool {
    exp10 < -6 || exp10 >= n_digits as i32
}

fn parse_decimal_digits(input: &str) -> Result<(Vec<u8>, i32), DtoaError> {
    let mut slice = input;
    if let Some(rest) = slice.strip_prefix('-') {
        slice = rest;
    }

    let mut mantissa = slice;
    let mut exp_part = "";
    if let Some(idx) = slice.find(|c| ['e', 'E'].contains(&c)) {
        mantissa = &slice[..idx];
        exp_part = &slice[idx + 1..];
    }

    let mut digits = Vec::with_capacity(mantissa.len());
    let mut digits_before_dot = 0usize;
    let mut saw_dot = false;
    for &b in mantissa.as_bytes() {
        if b == b'.' {
            saw_dot = true;
            continue;
        }
        if !saw_dot {
            digits_before_dot += 1;
        }
        digits.push(b);
    }

    let exp_from_e = if exp_part.is_empty() {
        0
    } else {
        let mut sign = 1i32;
        let mut chars = exp_part.chars();
        if let Some(first) = chars.next() {
            let mut rest = exp_part;
            if first == '-' {
                sign = -1;
                rest = &exp_part[1..];
            } else if first == '+' {
                rest = &exp_part[1..];
            }
            let mut value = 0i32;
            for b in rest.bytes() {
                if !b.is_ascii_digit() {
                    return Err(DtoaError::UnsupportedFormat);
                }
                value = value
                    .checked_mul(10)
                    .and_then(|v| v.checked_add((b - b'0') as i32))
                    .ok_or(DtoaError::InvalidDigits)?;
            }
            sign * value
        } else {
            return Err(DtoaError::UnsupportedFormat);
        }
    };

    let mut leading_zeros = 0usize;
    while leading_zeros < digits.len() && digits[leading_zeros] == b'0' {
        leading_zeros += 1;
    }
    if leading_zeros == digits.len() {
        return Ok((vec![b'0'], 0));
    }

    let exp10 = exp_from_e + digits_before_dot as i32 - 1 - leading_zeros as i32;
    let digits = digits[leading_zeros..].to_vec();
    Ok((digits, exp10))
}

fn round_up_digits(digits: &mut [u8]) -> bool {
    for idx in (0..digits.len()).rev() {
        if digits[idx] < b'9' {
            digits[idx] += 1;
            return false;
        }
        digits[idx] = b'0';
    }
    true
}

fn format_exponent(digits: &[u8], exp10: i32) -> String {
    let mut out = String::with_capacity(digits.len() + 8);
    out.push(digits[0] as char);
    if digits.len() > 1 {
        out.push('.');
        for &b in &digits[1..] {
            out.push(b as char);
        }
    }
    out.push('e');
    if exp10 >= 0 {
        out.push('+');
    } else {
        out.push('-');
    }
    out.push_str(&exp10.abs().to_string());
    out
}

fn format_decimal(digits: &[u8], exp10: i32) -> String {
    if exp10 >= 0 {
        let int_len = exp10 as usize + 1;
        let mut out = String::with_capacity(digits.len() + int_len);
        if int_len >= digits.len() {
            for &b in digits {
                out.push(b as char);
            }
            for _ in 0..(int_len - digits.len()) {
                out.push('0');
            }
            return out;
        }
        for &b in &digits[..int_len] {
            out.push(b as char);
        }
        out.push('.');
        for &b in &digits[int_len..] {
            out.push(b as char);
        }
        out
    } else {
        let mut out = String::with_capacity(digits.len() + (-exp10 as usize) + 3);
        out.push('0');
        out.push('.');
        for _ in 0..(-exp10 - 1) {
            out.push('0');
        }
        for &b in digits {
            out.push(b as char);
        }
        out
    }
}

fn write_decimal<const FORMAT: u128>(value: f64, options: &WriteFloatOptions) -> String {
    let mut out = write_with_options::<FORMAT>(value, options);
    ensure_exponent_sign(&mut out, b'e');
    out
}

fn write_with_options<const FORMAT: u128>(value: f64, options: &WriteFloatOptions) -> String {
    let size = options.buffer_size_const::<f64, FORMAT>();
    let mut buffer = vec![0u8; size];
    let bytes = lexical_core::write_with_options::<_, FORMAT>(value, &mut buffer, options);
    core::str::from_utf8(bytes)
        .expect("lexical-core writes valid UTF-8")
        .to_string()
}

macro_rules! write_with_radix_literal {
    ($value:expr, $options:expr, $radix:literal) => {{
        const FORMAT: u128 = NumberFormatBuilder::new()
            .mantissa_radix($radix)
            .no_exponent_notation(true)
            .build_strict();
        write_with_options::<FORMAT>($value, $options)
    }};
}

fn write_radix_no_exp(
    radix: u32,
    value: f64,
    options: &WriteFloatOptions,
) -> Result<String, DtoaError> {
    let out = match radix {
        2 => write_with_radix_literal!(value, options, 2),
        3 => write_with_radix_literal!(value, options, 3),
        4 => write_with_radix_literal!(value, options, 4),
        5 => write_with_radix_literal!(value, options, 5),
        6 => write_with_radix_literal!(value, options, 6),
        7 => write_with_radix_literal!(value, options, 7),
        8 => write_with_radix_literal!(value, options, 8),
        9 => write_with_radix_literal!(value, options, 9),
        10 => write_with_radix_literal!(value, options, 10),
        11 => write_with_radix_literal!(value, options, 11),
        12 => write_with_radix_literal!(value, options, 12),
        13 => write_with_radix_literal!(value, options, 13),
        14 => write_with_radix_literal!(value, options, 14),
        15 => write_with_radix_literal!(value, options, 15),
        16 => write_with_radix_literal!(value, options, 16),
        17 => write_with_radix_literal!(value, options, 17),
        18 => write_with_radix_literal!(value, options, 18),
        19 => write_with_radix_literal!(value, options, 19),
        20 => write_with_radix_literal!(value, options, 20),
        21 => write_with_radix_literal!(value, options, 21),
        22 => write_with_radix_literal!(value, options, 22),
        23 => write_with_radix_literal!(value, options, 23),
        24 => write_with_radix_literal!(value, options, 24),
        25 => write_with_radix_literal!(value, options, 25),
        26 => write_with_radix_literal!(value, options, 26),
        27 => write_with_radix_literal!(value, options, 27),
        28 => write_with_radix_literal!(value, options, 28),
        29 => write_with_radix_literal!(value, options, 29),
        30 => write_with_radix_literal!(value, options, 30),
        31 => write_with_radix_literal!(value, options, 31),
        32 => write_with_radix_literal!(value, options, 32),
        33 => write_with_radix_literal!(value, options, 33),
        34 => write_with_radix_literal!(value, options, 34),
        35 => write_with_radix_literal!(value, options, 35),
        36 => write_with_radix_literal!(value, options, 36),
        _ => return Err(DtoaError::InvalidRadix),
    };
    let mut out = out;
    if radix > 10 {
        out.make_ascii_lowercase();
    }
    Ok(out)
}

fn ensure_exponent_sign(out: &mut String, exponent: u8) {
    let bytes = out.as_bytes();
    let mut idx = None;
    for (i, &b) in bytes.iter().enumerate() {
        if b == exponent || b == exponent.to_ascii_uppercase() {
            idx = Some(i);
            break;
        }
    }
    let Some(exp_index) = idx else { return };
    let sign_index = exp_index + 1;
    if sign_index >= out.len() {
        return;
    }
    let sign = out.as_bytes()[sign_index];
    if sign != b'+' && sign != b'-' {
        out.insert(sign_index, '+');
    }
}

fn build_parse_options(exp_kind: ExpKind, radix: u32) -> ParseFloatOptions {
    let exponent = match exp_kind {
        ExpKind::Binary => b'p',
        ExpKind::Radix => b'@',
        ExpKind::Decimal => b'e',
        ExpKind::None => {
            if radix == 10 {
                b'e'
            } else {
                b'@'
            }
        }
    };
    ParseFloatOptions::builder()
        .exponent(exponent)
        .nan_string(None)
        .inf_string(None)
        .build_strict()
}

fn parse_decimal_or_radix(
    radix: u32,
    bytes: &[u8],
    options: &ParseFloatOptions,
) -> Result<f64, lexical_core::Error> {
    if radix == 10 {
        if bytes.iter().any(|&b| b == b'e' || b == b'E') {
            return lexical_core::parse_with_options::<f64, DECIMAL_FORMAT>(bytes, options);
        }
        return lexical_core::parse_with_options::<f64, DECIMAL_NO_EXP_FORMAT>(bytes, options);
    }
    parse_radix_no_exp(radix, bytes, options)
}

fn parse_radix_no_exp(
    radix: u32,
    bytes: &[u8],
    options: &ParseFloatOptions,
) -> Result<f64, lexical_core::Error> {
    macro_rules! parse_with_radix_literal {
        ($radix:literal) => {{
            const FORMAT: u128 = NumberFormatBuilder::new()
                .mantissa_radix($radix)
                .no_exponent_notation(true)
                .build_strict();
            lexical_core::parse_with_options::<f64, FORMAT>(bytes, options)
        }};
    }
    match radix {
        2 => parse_with_radix_literal!(2),
        3 => parse_with_radix_literal!(3),
        4 => parse_with_radix_literal!(4),
        5 => parse_with_radix_literal!(5),
        6 => parse_with_radix_literal!(6),
        7 => parse_with_radix_literal!(7),
        8 => parse_with_radix_literal!(8),
        9 => parse_with_radix_literal!(9),
        10 => parse_with_radix_literal!(10),
        11 => parse_with_radix_literal!(11),
        12 => parse_with_radix_literal!(12),
        13 => parse_with_radix_literal!(13),
        14 => parse_with_radix_literal!(14),
        15 => parse_with_radix_literal!(15),
        16 => parse_with_radix_literal!(16),
        17 => parse_with_radix_literal!(17),
        18 => parse_with_radix_literal!(18),
        19 => parse_with_radix_literal!(19),
        20 => parse_with_radix_literal!(20),
        21 => parse_with_radix_literal!(21),
        22 => parse_with_radix_literal!(22),
        23 => parse_with_radix_literal!(23),
        24 => parse_with_radix_literal!(24),
        25 => parse_with_radix_literal!(25),
        26 => parse_with_radix_literal!(26),
        27 => parse_with_radix_literal!(27),
        28 => parse_with_radix_literal!(28),
        29 => parse_with_radix_literal!(29),
        30 => parse_with_radix_literal!(30),
        31 => parse_with_radix_literal!(31),
        32 => parse_with_radix_literal!(32),
        33 => parse_with_radix_literal!(33),
        34 => parse_with_radix_literal!(34),
        35 => parse_with_radix_literal!(35),
        36 => parse_with_radix_literal!(36),
        _ => Err(lexical_core::Error::InvalidRadix),
    }
}

fn parse_radix_exp(
    radix: u32,
    bytes: &[u8],
    options: &ParseFloatOptions,
) -> Result<f64, lexical_core::Error> {
    macro_rules! parse_with_radix_exp_literal {
        ($radix:literal) => {{
            const FORMAT: u128 = NumberFormatBuilder::new()
                .mantissa_radix($radix)
                .exponent_base(NonZeroU8::new($radix))
                .exponent_radix(NonZeroU8::new(10))
                .build_strict();
            lexical_core::parse_with_options::<f64, FORMAT>(bytes, options)
        }};
    }
    match radix {
        2 => parse_with_radix_exp_literal!(2),
        3 => parse_with_radix_exp_literal!(3),
        4 => parse_with_radix_exp_literal!(4),
        5 => parse_with_radix_exp_literal!(5),
        6 => parse_with_radix_exp_literal!(6),
        7 => parse_with_radix_exp_literal!(7),
        8 => parse_with_radix_exp_literal!(8),
        9 => parse_with_radix_exp_literal!(9),
        10 => parse_with_radix_exp_literal!(10),
        11 => parse_with_radix_exp_literal!(11),
        12 => parse_with_radix_exp_literal!(12),
        13 => parse_with_radix_exp_literal!(13),
        14 => parse_with_radix_exp_literal!(14),
        15 => parse_with_radix_exp_literal!(15),
        16 => parse_with_radix_exp_literal!(16),
        17 => parse_with_radix_exp_literal!(17),
        18 => parse_with_radix_exp_literal!(18),
        19 => parse_with_radix_exp_literal!(19),
        20 => parse_with_radix_exp_literal!(20),
        21 => parse_with_radix_exp_literal!(21),
        22 => parse_with_radix_exp_literal!(22),
        23 => parse_with_radix_exp_literal!(23),
        24 => parse_with_radix_exp_literal!(24),
        25 => parse_with_radix_exp_literal!(25),
        26 => parse_with_radix_exp_literal!(26),
        27 => parse_with_radix_exp_literal!(27),
        28 => parse_with_radix_exp_literal!(28),
        29 => parse_with_radix_exp_literal!(29),
        30 => parse_with_radix_exp_literal!(30),
        31 => parse_with_radix_exp_literal!(31),
        32 => parse_with_radix_exp_literal!(32),
        33 => parse_with_radix_exp_literal!(33),
        34 => parse_with_radix_exp_literal!(34),
        35 => parse_with_radix_exp_literal!(35),
        36 => parse_with_radix_exp_literal!(36),
        _ => Err(lexical_core::Error::InvalidRadix),
    }
}

fn parse_binary_exp(
    radix: u32,
    bytes: &[u8],
    options: &ParseFloatOptions,
) -> Result<f64, lexical_core::Error> {
    macro_rules! parse_with_radix_exp_base2_literal {
        ($radix:literal) => {{
            const FORMAT: u128 = NumberFormatBuilder::new()
                .mantissa_radix($radix)
                .exponent_base(NonZeroU8::new(2))
                .exponent_radix(NonZeroU8::new(10))
                .build_strict();
            lexical_core::parse_with_options::<f64, FORMAT>(bytes, options)
        }};
    }
    match radix {
        2 => parse_with_radix_exp_base2_literal!(2),
        4 => parse_with_radix_exp_base2_literal!(4),
        8 => parse_with_radix_exp_base2_literal!(8),
        16 => parse_with_radix_exp_base2_literal!(16),
        _ => Err(lexical_core::Error::InvalidRadix),
    }
}

struct ScanResult {
    end: usize,
    cleaned: Vec<u8>,
    exp_kind: ExpKind,
    saw_digit: bool,
    error_pos: Option<usize>,
}

fn scan_number(bytes: &[u8], radix: u32, flags: AtodFlags) -> ScanResult {
    let allow_underscore = flags.contains(JS_ATOD_ACCEPT_UNDERSCORES);
    let int_only = flags.contains(JS_ATOD_INT_ONLY);
    let mut out = Vec::with_capacity(bytes.len());
    let mut i = 0;
    let mut saw_digit = false;
    let mut saw_dot = false;
    let mut in_exp = false;
    let mut exp_kind = ExpKind::None;
    let mut error_pos = None;

    while i < bytes.len() {
        let c = bytes[i];
        if !in_exp {
            if c == b'.'
                && !int_only
                && !saw_dot
                && (i > 0 || next_is_digit(bytes, i + 1, radix))
            {
                out.push(c);
                saw_dot = true;
                i += 1;
                continue;
            }
            if c == b'_' && allow_underscore {
                if i > 0 && next_is_digit(bytes, i + 1, radix) {
                    i += 1;
                    continue;
                }
                break;
            }
            if is_digit_in_radix(c, radix) {
                out.push(c);
                saw_digit = true;
                i += 1;
                continue;
            }
            if !int_only
                && saw_digit
                && let Some(kind) = exp_kind_for_char(c, radix)
            {
                exp_kind = kind;
                in_exp = true;
                out.push(normalize_exp_char(c));
                i += 1;
                if i < bytes.len() && (bytes[i] == b'+' || bytes[i] == b'-') {
                    out.push(bytes[i]);
                    i += 1;
                }
                if i >= bytes.len() || !bytes[i].is_ascii_digit() {
                    error_pos = Some(i);
                    break;
                }
                continue;
            }
            break;
        } else {
            if c == b'_' && allow_underscore {
                if next_is_decimal_digit(bytes, i + 1) {
                    i += 1;
                    continue;
                }
                break;
            }
            if c.is_ascii_digit() {
                out.push(c);
                i += 1;
                continue;
            }
            break;
        }
    }

    ScanResult {
        end: i,
        cleaned: out,
        exp_kind,
        saw_digit,
        error_pos,
    }
}

fn exp_kind_for_char(c: u8, radix: u32) -> Option<ExpKind> {
    if radix == 10 {
        if c == b'e' || c == b'E' {
            return Some(ExpKind::Decimal);
        }
        return None;
    }
    if c == b'@' {
        return Some(ExpKind::Radix);
    }
    if matches!(radix, 2 | 4 | 8 | 16) && (c == b'p' || c == b'P') {
        return Some(ExpKind::Binary);
    }
    None
}

fn normalize_exp_char(c: u8) -> u8 {
    if c == b'E' {
        b'e'
    } else if c == b'P' {
        b'p'
    } else {
        c
    }
}

fn next_is_digit(bytes: &[u8], idx: usize, radix: u32) -> bool {
    if idx >= bytes.len() {
        return false;
    }
    is_digit_in_radix(bytes[idx], radix)
}

fn next_is_decimal_digit(bytes: &[u8], idx: usize) -> bool {
    if idx >= bytes.len() {
        return false;
    }
    bytes[idx].is_ascii_digit()
}

fn is_digit_in_radix(c: u8, radix: u32) -> bool {
    match digit_value(c) {
        Some(value) => value < radix,
        None => false,
    }
}

fn digit_value(c: u8) -> Option<u32> {
    match c {
        b'0'..=b'9' => Some((c - b'0') as u32),
        b'a'..=b'z' => Some((c - b'a' + 10) as u32),
        b'A'..=b'Z' => Some((c - b'A' + 10) as u32),
        _ => None,
    }
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    #[test]
    fn format_free_decimal() {
        let s = js_dtoa(1.234, 10, 0, JS_DTOA_FORMAT_FREE | JS_DTOA_EXP_AUTO).unwrap();
        assert_eq!(s, "1.234");
        let s = js_dtoa(1e21, 10, 0, JS_DTOA_FORMAT_FREE | JS_DTOA_EXP_AUTO).unwrap();
        assert_eq!(s, "1e+21");
        let s = js_dtoa(1e-7, 10, 0, JS_DTOA_FORMAT_FREE | JS_DTOA_EXP_AUTO).unwrap();
        assert_eq!(s, "1e-7");
    }

    #[test]
    fn format_free_exp_enabled() {
        let s = js_dtoa(25.0, 10, 0, JS_DTOA_FORMAT_FREE | JS_DTOA_EXP_ENABLED).unwrap();
        assert_eq!(s, "2.5e+1");
        let s = js_dtoa(0.00123, 10, 0, JS_DTOA_FORMAT_FREE | JS_DTOA_EXP_ENABLED).unwrap();
        assert_eq!(s, "1.23e-3");
    }

    #[test]
    fn format_minus_zero() {
        let s = js_dtoa(-0.0, 10, 0, JS_DTOA_FORMAT_FREE | JS_DTOA_EXP_AUTO).unwrap();
        assert_eq!(s, "0");
        let s = js_dtoa(
            -0.0,
            10,
            0,
            JS_DTOA_FORMAT_FREE | JS_DTOA_EXP_AUTO | JS_DTOA_MINUS_ZERO,
        )
        .unwrap();
        assert_eq!(s, "-0");
        let s = js_dtoa(
            -0.0,
            10,
            2,
            JS_DTOA_FORMAT_FRAC | JS_DTOA_EXP_AUTO | JS_DTOA_MINUS_ZERO,
        )
        .unwrap();
        assert_eq!(s, "-0.00");
    }

    #[test]
    fn format_to_fixed() {
        let s = js_dtoa(1.25, 10, 1, JS_DTOA_FORMAT_FRAC | JS_DTOA_EXP_AUTO).unwrap();
        assert_eq!(s, "1.3");
        let s = js_dtoa(1.0, 10, 2, JS_DTOA_FORMAT_FRAC | JS_DTOA_EXP_AUTO).unwrap();
        assert_eq!(s, "1.00");
    }

    #[test]
    fn format_to_exponential() {
        let s = js_dtoa(
            1.234,
            10,
            3,
            JS_DTOA_FORMAT_FIXED | JS_DTOA_EXP_ENABLED,
        )
        .unwrap();
        assert_eq!(s, "1.23e+0");
    }

    #[test]
    fn format_to_precision_auto_exp() {
        let s = js_dtoa(
            1234.0,
            10,
            3,
            JS_DTOA_FORMAT_FIXED | JS_DTOA_EXP_AUTO,
        )
        .unwrap();
        assert_eq!(s, "1.23e+3");
        let s = js_dtoa(
            12.34,
            10,
            3,
            JS_DTOA_FORMAT_FIXED | JS_DTOA_EXP_AUTO,
        )
        .unwrap();
        assert_eq!(s, "12.3");
    }

    #[test]
    fn format_hex_radix_free() {
        let s = js_dtoa(10.0, 16, 0, JS_DTOA_FORMAT_FREE | JS_DTOA_EXP_DISABLED).unwrap();
        assert_eq!(s, "a");
    }

    #[test]
    fn parse_decimal_with_underscores() {
        let r = js_atod("1_234.5", 10, JS_ATOD_ACCEPT_UNDERSCORES).unwrap();
        assert_eq!(r.value, 1234.5);
        assert_eq!(r.next, 7);
    }

    #[test]
    fn parse_prefixes() {
        let r = js_atod("0x10", 0, JS_ATOD_ACCEPT_BIN_OCT).unwrap();
        assert_eq!(r.value, 16.0);
        let r = js_atod("0b10", 0, JS_ATOD_ACCEPT_BIN_OCT).unwrap();
        assert_eq!(r.value, 2.0);
        let r = js_atod("0o10", 0, JS_ATOD_ACCEPT_BIN_OCT).unwrap();
        assert_eq!(r.value, 8.0);
    }

    #[test]
    fn parse_legacy_octal() {
        let r = js_atod("077", 0, JS_ATOD_ACCEPT_LEGACY_OCTAL).unwrap();
        assert_eq!(r.value, 63.0);
        assert_eq!(r.next, 3);
        let r = js_atod("08", 0, JS_ATOD_ACCEPT_LEGACY_OCTAL).unwrap();
        assert_eq!(r.value, 8.0);
        assert_eq!(r.next, 2);
    }

    #[test]
    fn parse_int_only_stops_at_dot() {
        let r = js_atod("123.4", 10, JS_ATOD_INT_ONLY).unwrap();
        assert_eq!(r.value, 123.0);
        assert_eq!(r.next, 3);
    }

    #[test]
    fn parse_infinity() {
        let r = js_atod("Infinity", 0, AtodFlags::empty()).unwrap();
        assert!(r.value.is_infinite());
        assert_eq!(r.next, 8);
        let r = js_atod("-Infinity", 0, AtodFlags::empty()).unwrap();
        assert_eq!(r.value, f64::NEG_INFINITY);
        assert_eq!(r.next, 9);
    }

    #[test]
    fn parse_missing_exponent_digits() {
        let r = js_atod("1e", 10, AtodFlags::empty()).unwrap();
        assert!(r.value.is_nan());
        assert_eq!(r.next, 2);
    }
}
