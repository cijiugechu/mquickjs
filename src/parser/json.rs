use crate::containers::JS_STRING_LEN_MAX;
use crate::cutils::{unicode_from_utf8, unicode_to_utf8, utf8_get, UTF8_CHAR_LEN_MAX};
use crate::dtoa::{js_atod, AtodFlags};
use crate::parser::lexer::js_parse_escape;

const ERR_INVALID_ESCAPE: &str = "invalid escape sequence";
const ERR_UNEXPECTED_END_OF_STRING: &str = "unexpected end of string";
const ERR_INVALID_UTF8: &str = "invalid UTF-8 sequence";
const ERR_INVALID_NUMBER: &str = "invalid number literal";
const ERR_UNEXPECTED_CHARACTER: &str = "unexpected character";
const ERR_EXPECTING_CLOSE_BRACKET: &str = "expecting ']'";
const ERR_EXPECTING_CLOSE_BRACE: &str = "expecting '}'";
const ERR_EXPECTING_COLON: &str = "expecting ':'";
const ERR_EXPECTING_DOUBLE_QUOTE: &str = "expecting '\"'";
const ERR_NO_MEM: &str = "not enough memory";
const NUL_BYTE: [u8; 1] = [0];

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct JsonString {
    bytes: Vec<u8>,
    is_ascii: bool,
}

impl JsonString {
    fn new(bytes: Vec<u8>, is_ascii: bool) -> Self {
        Self { bytes, is_ascii }
    }

    pub fn bytes(&self) -> &[u8] {
        &self.bytes
    }

    pub fn is_ascii(&self) -> bool {
        self.is_ascii
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum JsonValue {
    Null,
    Bool(bool),
    Number(f64),
    String(JsonString),
    Array(Vec<JsonValue>),
    Object(Vec<(JsonString, JsonValue)>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JsonError {
    message: &'static str,
    position: usize,
}

impl JsonError {
    fn new(message: &'static str, position: usize) -> Self {
        Self { message, position }
    }

    pub fn message(&self) -> &'static str {
        self.message
    }

    pub fn position(&self) -> usize {
        self.position
    }
}

pub fn parse_json(source: &[u8]) -> Result<JsonValue, JsonError> {
    JsonParser::new(source).parse()
}

struct JsonParser<'a> {
    source: &'a [u8],
    buf_len: usize,
    pos: usize,
}

impl<'a> JsonParser<'a> {
    fn new(source: &'a [u8]) -> Self {
        let buf_len = if source.last() == Some(&0) {
            source.len().saturating_sub(1)
        } else {
            source.len()
        };
        Self {
            source,
            buf_len,
            pos: 0,
        }
    }

    fn parse(mut self) -> Result<JsonValue, JsonError> {
        let value = self.parse_value()?;
        self.pos = self.skip_spaces(self.pos);
        if self.pos != self.buf_len {
            return Err(JsonError::new(ERR_UNEXPECTED_CHARACTER, self.pos));
        }
        Ok(value)
    }

    fn parse_value(&mut self) -> Result<JsonValue, JsonError> {
        let mut stack: Vec<Frame> = Vec::new();
        let mut mode = Mode::Value;

        loop {
            match mode {
                Mode::Value => {
                    self.pos = self.skip_spaces(self.pos);
                    let c = self.byte_at(self.pos);
                    let value = match c {
                        b'-' | b'0'..=b'9' => {
                            let (num, new_pos) = self.parse_number(self.pos)?;
                            self.pos = new_pos;
                            JsonValue::Number(num)
                        }
                        b'"' => {
                            let (string, new_pos) = self.parse_string(self.pos + 1)?;
                            self.pos = new_pos;
                            JsonValue::String(string)
                        }
                        b't' => {
                            if self.match_literal(self.pos, b"true") {
                                self.pos += 4;
                                JsonValue::Bool(true)
                            } else {
                                return Err(JsonError::new(ERR_UNEXPECTED_CHARACTER, self.pos));
                            }
                        }
                        b'f' => {
                            if self.match_literal(self.pos, b"false") {
                                self.pos += 5;
                                JsonValue::Bool(false)
                            } else {
                                return Err(JsonError::new(ERR_UNEXPECTED_CHARACTER, self.pos));
                            }
                        }
                        b'n' => {
                            if self.match_literal(self.pos, b"null") {
                                self.pos += 4;
                                JsonValue::Null
                            } else {
                                return Err(JsonError::new(ERR_UNEXPECTED_CHARACTER, self.pos));
                            }
                        }
                        b'[' => {
                            self.pos += 1;
                            self.pos = self.skip_spaces(self.pos);
                            if self.byte_at(self.pos) == b']' {
                                self.pos += 1;
                                JsonValue::Array(Vec::new())
                            } else {
                                stack.push(Frame::Array { elements: Vec::new() });
                                continue;
                            }
                        }
                        b'{' => {
                            self.pos += 1;
                            self.pos = self.skip_spaces(self.pos);
                            if self.byte_at(self.pos) == b'}' {
                                self.pos += 1;
                                JsonValue::Object(Vec::new())
                            } else {
                                stack.push(Frame::Object {
                                    entries: Vec::new(),
                                    pending_key: None,
                                });
                                mode = Mode::Key;
                                continue;
                            }
                        }
                        _ => return Err(JsonError::new(ERR_UNEXPECTED_CHARACTER, self.pos)),
                    };

                    if let Some(result) = self.finish_value(value, &mut stack, &mut mode)? {
                        return Ok(result);
                    }
                }
                Mode::Key => {
                    self.pos = self.skip_spaces(self.pos);
                    if self.byte_at(self.pos) != b'"' {
                        return Err(JsonError::new(ERR_EXPECTING_DOUBLE_QUOTE, self.pos));
                    }
                    let (key, new_pos) = self.parse_string(self.pos + 1)?;
                    self.pos = new_pos;
                    self.pos = self.skip_spaces(self.pos);
                    if self.byte_at(self.pos) != b':' {
                        return Err(JsonError::new(ERR_EXPECTING_COLON, self.pos));
                    }
                    self.pos += 1;
                    match stack.last_mut() {
                        Some(Frame::Object { pending_key, .. }) => {
                            *pending_key = Some(key);
                        }
                        _ => return Err(JsonError::new(ERR_UNEXPECTED_CHARACTER, self.pos)),
                    }
                    mode = Mode::Value;
                }
            }
        }
    }

    fn finish_value(
        &mut self,
        mut current: JsonValue,
        stack: &mut Vec<Frame>,
        mode: &mut Mode,
    ) -> Result<Option<JsonValue>, JsonError> {
        loop {
            let Some(frame) = stack.pop() else {
                return Ok(Some(current));
            };
            match frame {
                Frame::Array { mut elements } => {
                    elements.push(current);
                    self.pos = self.skip_spaces(self.pos);
                    match self.byte_at(self.pos) {
                        b',' => {
                            self.pos += 1;
                            stack.push(Frame::Array { elements });
                            *mode = Mode::Value;
                            return Ok(None);
                        }
                        b']' => {
                            self.pos += 1;
                            current = JsonValue::Array(elements);
                        }
                        _ => return Err(JsonError::new(ERR_EXPECTING_CLOSE_BRACKET, self.pos)),
                    }
                }
                Frame::Object {
                    mut entries,
                    pending_key,
                } => {
                    let Some(key) = pending_key else {
                        return Err(JsonError::new(ERR_UNEXPECTED_CHARACTER, self.pos));
                    };
                    entries.push((key, current));
                    self.pos = self.skip_spaces(self.pos);
                    match self.byte_at(self.pos) {
                        b',' => {
                            self.pos += 1;
                            stack.push(Frame::Object {
                                entries,
                                pending_key: None,
                            });
                            *mode = Mode::Key;
                            return Ok(None);
                        }
                        b'}' => {
                            self.pos += 1;
                            current = JsonValue::Object(entries);
                        }
                        _ => return Err(JsonError::new(ERR_EXPECTING_CLOSE_BRACE, self.pos)),
                    }
                }
            }
        }
    }

    fn parse_string(&self, mut pos: usize) -> Result<(JsonString, usize), JsonError> {
        let mut buf = StringBuffer::with_capacity(16);
        loop {
            let c = self.byte_at(pos);
            if c == 0 || c == b'\n' || c == b'\r' {
                return Err(JsonError::new(ERR_UNEXPECTED_END_OF_STRING, pos));
            }
            pos += 1;
            if c == b'"' {
                break;
            }
            if c == b'\\' {
                if self.byte_at(pos) == b'\n' {
                    pos += 1;
                    continue;
                }
                let mut escape_len = 0usize;
                let escaped = js_parse_escape(self.slice_from(pos), &mut escape_len);
                if escaped == -1 {
                    return Err(JsonError::new(ERR_INVALID_ESCAPE, pos));
                }
                if escaped == -2 {
                    continue;
                }
                pos += escape_len;
                if !buf.push_codepoint(escaped as u32) {
                    return Err(JsonError::new(ERR_NO_MEM, pos));
                }
                continue;
            }
            if c >= 0x80 {
                pos -= 1;
                let mut clen = 0usize;
                let codepoint =
                    unicode_from_utf8(self.slice_from(pos), UTF8_CHAR_LEN_MAX, &mut clen);
                if codepoint == -1 {
                    return Err(JsonError::new(ERR_INVALID_UTF8, pos));
                }
                pos += clen;
                if !buf.push_codepoint(codepoint as u32) {
                    return Err(JsonError::new(ERR_NO_MEM, pos));
                }
                continue;
            }
            if !buf.push_codepoint(c as u32) {
                return Err(JsonError::new(ERR_NO_MEM, pos));
            }
        }
        let (bytes, is_ascii) = buf.finish();
        Ok((JsonString::new(bytes, is_ascii), pos))
    }

    fn parse_number(&self, pos: usize) -> Result<(f64, usize), JsonError> {
        let slice = self.ascii_slice_from(pos);
        let input = match std::str::from_utf8(slice) {
            Ok(s) => s,
            Err(_) => return Err(JsonError::new(ERR_INVALID_NUMBER, pos)),
        };
        let parsed = js_atod(input, 10, AtodFlags::empty())
            .map_err(|_| JsonError::new(ERR_INVALID_NUMBER, pos))?;
        if parsed.value.is_nan() {
            return Err(JsonError::new(ERR_INVALID_NUMBER, pos));
        }
        Ok((parsed.value, pos + parsed.next))
    }

    fn match_literal(&self, pos: usize, literal: &[u8]) -> bool {
        self.source
            .get(pos..pos + literal.len())
            .is_some_and(|bytes| bytes == literal)
    }

    fn skip_spaces(&self, mut pos: usize) -> usize {
        while pos < self.buf_len {
            let c = self.source[pos];
            if !((0x09..=0x0d).contains(&c) || c == 0x20) {
                break;
            }
            pos += 1;
        }
        pos
    }

    fn byte_at(&self, pos: usize) -> u8 {
        if pos >= self.buf_len {
            0
        } else {
            self.source.get(pos).copied().unwrap_or(0)
        }
    }

    fn slice_from(&self, pos: usize) -> &[u8] {
        if pos < self.source.len() {
            &self.source[pos..]
        } else {
            &NUL_BYTE
        }
    }

    fn ascii_slice_from(&self, pos: usize) -> &[u8] {
        if pos >= self.buf_len {
            return &[];
        }
        let mut end = pos;
        while end < self.buf_len {
            let c = self.source[end];
            if c == 0 || c >= 0x80 {
                break;
            }
            end += 1;
        }
        &self.source[pos..end]
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Mode {
    Value,
    Key,
}

enum Frame {
    Array { elements: Vec<JsonValue> },
    Object {
        entries: Vec<(JsonString, JsonValue)>,
        pending_key: Option<JsonString>,
    },
}

struct StringBuffer {
    buf: Vec<u8>,
    is_ascii: bool,
}

impl StringBuffer {
    fn with_capacity(cap: usize) -> Self {
        Self {
            buf: Vec::with_capacity(cap),
            is_ascii: true,
        }
    }

    fn push_codepoint(&mut self, c: u32) -> bool {
        let mut tmp = [0u8; UTF8_CHAR_LEN_MAX];
        let len = unicode_to_utf8(&mut tmp, c);
        if len == 0 {
            return false;
        }

        if len >= 3
            && self.buf.len() >= 3
            && is_utf8_left_surrogate(&self.buf[self.buf.len() - 3..])
            && is_utf8_right_surrogate(&tmp[..3])
        {
            let mut clen = 0usize;
            let left = utf8_get(&self.buf[self.buf.len() - 3..], &mut clen);
            let right = utf8_get(&tmp[..3], &mut clen);
            if left >= 0 && right >= 0 {
                let combined =
                    0x10000 + (((left as u32) & 0x3ff) << 10) + ((right as u32) & 0x3ff);
                let mut merged = [0u8; UTF8_CHAR_LEN_MAX];
                let merged_len = unicode_to_utf8(&mut merged, combined);
                if merged_len == 0 {
                    return false;
                }
                let new_len = self.buf.len() - 3 + merged_len;
                if new_len > JS_STRING_LEN_MAX as usize {
                    return false;
                }
                self.buf.truncate(self.buf.len() - 3);
                self.buf.extend_from_slice(&merged[..merged_len]);
                self.is_ascii = false;
                return true;
            }
        }

        if self.buf.len() + len > JS_STRING_LEN_MAX as usize {
            return false;
        }
        self.buf.extend_from_slice(&tmp[..len]);
        if c >= 0x80 {
            self.is_ascii = false;
        }
        true
    }

    fn finish(self) -> (Vec<u8>, bool) {
        (self.buf, self.is_ascii)
    }
}

fn is_utf8_left_surrogate(p: &[u8]) -> bool {
    p.len() >= 3 && p[0] == 0xed && (0xa0..=0xaf).contains(&p[1]) && p[2] >= 0x80
}

fn is_utf8_right_surrogate(p: &[u8]) -> bool {
    p.len() >= 3 && p[0] == 0xed && (0xb0..=0xbf).contains(&p[1]) && p[2] >= 0x80
}

#[cfg(test)]
mod tests {
    use super::*;

    fn find_value<'a>(entries: &'a [(JsonString, JsonValue)], key: &[u8]) -> Option<&'a JsonValue> {
        entries
            .iter()
            .find(|(name, _)| name.bytes() == key)
            .map(|(_, value)| value)
    }

    fn assert_number(value: &JsonValue, expected: f64) {
        match value {
            JsonValue::Number(actual) => assert_eq!(*actual, expected),
            other => panic!("expected number, got {other:?}"),
        }
    }

    fn assert_bool(value: &JsonValue, expected: bool) {
        match value {
            JsonValue::Bool(actual) => assert_eq!(*actual, expected),
            other => panic!("expected bool, got {other:?}"),
        }
    }

    #[test]
    fn parse_json_basic_object() {
        let input = br#"{"x":1,"y":true,"z":null,"a":[1,2,false],"1234":"str"}"#;
        let value = parse_json(input).expect("parse json");
        let JsonValue::Object(entries) = value else {
            panic!("expected object");
        };
        assert_number(find_value(&entries, b"x").expect("x"), 1.0);
        assert_bool(find_value(&entries, b"y").expect("y"), true);
        assert!(matches!(
            find_value(&entries, b"z").expect("z"),
            JsonValue::Null
        ));

        let array = find_value(&entries, b"a").expect("a");
        let JsonValue::Array(elements) = array else {
            panic!("expected array");
        };
        assert_eq!(elements.len(), 3);
        assert_number(&elements[0], 1.0);
        assert_number(&elements[1], 2.0);
        assert_bool(&elements[2], false);

        let str_value = find_value(&entries, b"1234").expect("1234");
        let JsonValue::String(name) = str_value else {
            panic!("expected string");
        };
        assert_eq!(name.bytes(), b"str");
    }

    #[test]
    fn parse_json_deep_array_stack() {
        let depth = 100;
        let mut input = vec![b'['; depth];
        input.extend(vec![b']'; depth]);
        let mut value = parse_json(&input).expect("parse json");
        for idx in 0..depth {
            match value {
                JsonValue::Array(mut elements) => {
                    if idx + 1 == depth {
                        assert!(elements.is_empty());
                        break;
                    }
                    assert_eq!(elements.len(), 1);
                    value = elements.pop().expect("nested value");
                }
                other => panic!("expected array at depth {idx}, got {other:?}"),
            }
        }
    }

    #[test]
    fn parse_json_string_escapes() {
        let value = parse_json(br#""\x41\uD83D\uDE00""#).expect("parse json");
        let JsonValue::String(string) = value else {
            panic!("expected string");
        };
        assert_eq!(string.bytes(), b"A\xF0\x9F\x98\x80");
    }

    #[test]
    fn parse_json_invalid_escape_reports_position() {
        let err = parse_json(br#""\xZ1""#).expect_err("parse error");
        assert_eq!(err.message(), ERR_INVALID_ESCAPE);
        assert_eq!(err.position(), 2);
    }
}
