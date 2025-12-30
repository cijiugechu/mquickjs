use crate::jsvalue::JSValue;
use crate::string::runtime::string_view;

#[derive(Copy, Clone, Debug)]
pub enum JsFormatArg<'a> {
    Int(i64),
    UInt(u64),
    Str(&'a [u8]),
    Char(u8),
    Pointer(usize),
    Value(JSValue),
}

impl<'a> JsFormatArg<'a> {
    pub fn pointer(ptr: *const core::ffi::c_void) -> Self {
        JsFormatArg::Pointer(ptr as usize)
    }

    pub fn char(value: u8) -> Self {
        JsFormatArg::Char(value)
    }
}

impl<'a> From<&'a str> for JsFormatArg<'a> {
    fn from(value: &'a str) -> Self {
        JsFormatArg::Str(value.as_bytes())
    }
}

impl<'a> From<&'a [u8]> for JsFormatArg<'a> {
    fn from(value: &'a [u8]) -> Self {
        JsFormatArg::Str(value)
    }
}

impl<'a> From<i32> for JsFormatArg<'a> {
    fn from(value: i32) -> Self {
        JsFormatArg::Int(value as i64)
    }
}

impl<'a> From<i64> for JsFormatArg<'a> {
    fn from(value: i64) -> Self {
        JsFormatArg::Int(value)
    }
}

impl<'a> From<u32> for JsFormatArg<'a> {
    fn from(value: u32) -> Self {
        JsFormatArg::UInt(value as u64)
    }
}

impl<'a> From<u64> for JsFormatArg<'a> {
    fn from(value: u64) -> Self {
        JsFormatArg::UInt(value)
    }
}

impl<'a> From<usize> for JsFormatArg<'a> {
    fn from(value: usize) -> Self {
        JsFormatArg::UInt(value as u64)
    }
}

impl<'a> From<JSValue> for JsFormatArg<'a> {
    fn from(value: JSValue) -> Self {
        JsFormatArg::Value(value)
    }
}

pub(crate) fn format_js_message(
    fmt: &str,
    args: &[JsFormatArg<'_>],
    max_len: usize,
) -> Vec<u8> {
    let bytes = fmt.as_bytes();
    let mut out = Vec::new();
    let mut idx = 0usize;
    let mut arg_index = 0usize;

    while idx < bytes.len() {
        if bytes[idx] != b'%' {
            push_bytes(&mut out, &bytes[idx..idx + 1], max_len);
            if out.len() >= max_len {
                break;
            }
            idx += 1;
            continue;
        }
        idx += 1;
        if idx >= bytes.len() {
            break;
        }
        if bytes[idx] == b'%' {
            push_bytes(&mut out, b"%", max_len);
            idx += 1;
            continue;
        }

        let mut zero_pad = false;
        let mut left_adj = false;
        loop {
            match bytes[idx] {
                b'0' => {
                    zero_pad = true;
                    idx += 1;
                }
                b'-' => {
                    left_adj = true;
                    idx += 1;
                }
                b'#' | b'+' | b' ' => {
                    idx += 1;
                }
                _ => break,
            }
            if idx >= bytes.len() {
                break;
            }
        }

        let mut width = 0usize;
        if idx < bytes.len() && bytes[idx] == b'*' {
            idx += 1;
        } else {
            while idx < bytes.len() && bytes[idx].is_ascii_digit() {
                width = width.saturating_mul(10);
                width = width.saturating_add((bytes[idx] - b'0') as usize);
                idx += 1;
            }
        }

        if idx < bytes.len() && bytes[idx] == b'.' {
            idx += 1;
            if idx < bytes.len() && bytes[idx] == b'*' {
                idx += 1;
            } else {
                while idx < bytes.len() && bytes[idx].is_ascii_digit() {
                    idx += 1;
                }
            }
        }

        while idx < bytes.len() {
            match bytes[idx] {
                b'l' | b'z' | b't' => idx += 1,
                _ => break,
            }
        }

        if idx >= bytes.len() {
            break;
        }
        let spec = bytes[idx];
        idx += 1;

        let arg = if spec != b'%' {
            let arg = args.get(arg_index).copied().unwrap_or(JsFormatArg::Int(0));
            arg_index = arg_index.saturating_add(1);
            arg
        } else {
            JsFormatArg::Int(0)
        };

        let piece = match spec {
            b'%' => b"%".to_vec(),
            b'c' => {
                zero_pad = false;
                vec![arg_as_char(arg)]
            }
            b's' => {
                zero_pad = false;
                arg_as_bytes(arg)
            }
            b'd' => arg_as_i64(arg).to_string().into_bytes(),
            b'u' => arg_as_u64(arg).to_string().into_bytes(),
            b'x' => format!("{:x}", arg_as_u64(arg)).into_bytes(),
            b'p' => format!("0x{:x}", arg_as_usize(arg)).into_bytes(),
            b'o' => {
                zero_pad = false;
                format_js_value(arg_as_value(arg))
            }
            _ => Vec::new(),
        };

        write_padded(&mut out, &piece, width, left_adj, zero_pad, max_len);
        if out.len() >= max_len {
            break;
        }
    }

    out
}

fn write_padded(
    out: &mut Vec<u8>,
    piece: &[u8],
    width: usize,
    left_adj: bool,
    zero_pad: bool,
    max_len: usize,
) {
    let len = piece.len();
    if width <= len {
        push_bytes(out, piece, max_len);
        return;
    }
    let pad_len = width - len;
    let pad_char = if zero_pad && !left_adj { b'0' } else { b' ' };
    if !left_adj {
        push_pad(out, pad_char, pad_len, max_len);
    }
    push_bytes(out, piece, max_len);
    if left_adj {
        push_pad(out, b' ', pad_len, max_len);
    }
}

fn push_bytes(out: &mut Vec<u8>, bytes: &[u8], max_len: usize) {
    let available = max_len.saturating_sub(out.len());
    if available == 0 {
        return;
    }
    let count = bytes.len().min(available);
    out.extend_from_slice(&bytes[..count]);
}

fn push_pad(out: &mut Vec<u8>, pad: u8, count: usize, max_len: usize) {
    if count == 0 || out.len() >= max_len {
        return;
    }
    let available = max_len.saturating_sub(out.len());
    let count = count.min(available);
    out.extend(core::iter::repeat_n(pad, count));
}

fn arg_as_char(arg: JsFormatArg<'_>) -> u8 {
    match arg {
        JsFormatArg::Char(val) => val,
        JsFormatArg::Int(val) => val as u8,
        JsFormatArg::UInt(val) => val as u8,
        _ => 0,
    }
}

fn arg_as_bytes(arg: JsFormatArg<'_>) -> Vec<u8> {
    match arg {
        JsFormatArg::Str(bytes) => bytes.to_vec(),
        _ => b"null".to_vec(),
    }
}

fn arg_as_i64(arg: JsFormatArg<'_>) -> i64 {
    match arg {
        JsFormatArg::Int(val) => val,
        JsFormatArg::UInt(val) => val as i64,
        JsFormatArg::Char(val) => val as i64,
        _ => 0,
    }
}

fn arg_as_u64(arg: JsFormatArg<'_>) -> u64 {
    match arg {
        JsFormatArg::UInt(val) => val,
        JsFormatArg::Int(val) => val as u64,
        JsFormatArg::Char(val) => val as u64,
        _ => 0,
    }
}

fn arg_as_usize(arg: JsFormatArg<'_>) -> usize {
    match arg {
        JsFormatArg::Pointer(val) => val,
        JsFormatArg::UInt(val) => val as usize,
        JsFormatArg::Int(val) => val as usize,
        _ => 0,
    }
}

fn arg_as_value(arg: JsFormatArg<'_>) -> JSValue {
    match arg {
        JsFormatArg::Value(val) => val,
        _ => JSValue::JS_UNDEFINED,
    }
}

fn format_js_value(val: JSValue) -> Vec<u8> {
    let mut scratch = [0u8; 5];
    if let Some(view) = string_view(val, &mut scratch) {
        return view.bytes().to_vec();
    }
    if val.is_int() {
        return val.get_int().to_string().into_bytes();
    }
    #[cfg(target_pointer_width = "64")]
    if val.is_short_float() {
        return b"[short_float]".to_vec();
    }
    if !val.is_ptr() {
        let tag = val.get_special_tag();
        return match tag {
            JSValue::JS_TAG_NULL => b"null".to_vec(),
            JSValue::JS_TAG_UNDEFINED => b"undefined".to_vec(),
            JSValue::JS_TAG_UNINITIALIZED => b"uninitialized".to_vec(),
            JSValue::JS_TAG_BOOL => {
                if val.get_special_value() != 0 {
                    b"true".to_vec()
                } else {
                    b"false".to_vec()
                }
            }
            _ => b"[tag]".to_vec(),
        };
    }
    b"[mtag]".to_vec()
}
