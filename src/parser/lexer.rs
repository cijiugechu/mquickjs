use crate::cutils::{from_hex, unicode_from_utf8, unicode_to_utf8, utf8_get, UTF8_CHAR_LEN_MAX};
use crate::dtoa::{js_atod, JS_ATOD_ACCEPT_BIN_OCT, JS_ATOD_ACCEPT_UNDERSCORES};
use crate::jsvalue::{
    value_from_ptr, value_get_special_tag, value_get_special_value, value_make_special,
    value_to_ptr, JSValue, JS_NULL, JS_TAG_STRING_CHAR,
};
use crate::string::js_string::{is_ascii_bytes, JSString};
use std::pin::Pin;
use std::ptr::NonNull;

use super::regexp_flags::parse_regexp_flags;
use super::tokens::*;
use super::types::{ParsePos, SourcePos, Token, TokenExtra};

const ERR_INVALID_ESCAPE: &str = "invalid escape sequence";
const ERR_UNEXPECTED_END_OF_STRING: &str = "unexpected end of string";
const ERR_INVALID_UTF8: &str = "invalid UTF-8 sequence";
const ERR_UNEXPECTED_LINE_TERMINATOR_IN_REGEXP: &str = "unexpected line terminator in regexp";
const ERR_INVALID_REGEXP_FLAGS: &str = "invalid regular expression flags";
const ERR_INVALID_NUMBER: &str = "invalid number literal";
const ERR_UNEXPECTED_END_OF_COMMENT: &str = "unexpected end of comment";
const ERR_UNEXPECTED_CHARACTER: &str = "unexpected character";
const ERR_NO_MEM: &str = "not enough memory";
const ERR_TOO_MANY_NESTED_BLOCKS: &str = "too many nested blocks";
const ERR_EXPECTING_CLOSE_PAREN: &str = "expecting ')'";
const ERR_EXPECTING_CLOSE_BRACKET: &str = "expecting ']'";
const ERR_EXPECTING_CLOSE_BRACE: &str = "expecting '}'";
const ERR_EXPECTING_DELIMITER: &str = "expecting closing delimiter";
const NUL_BYTE: [u8; 1] = [0];

pub(crate) const SKIP_HAS_ARGUMENTS: u32 = 1 << 0;
pub(crate) const SKIP_HAS_FUNC_NAME: u32 = 1 << 1;
pub(crate) const SKIP_HAS_SEMI: u32 = 1 << 2;

const TOK_PAREN_OPEN: i32 = b'(' as i32;
const TOK_PAREN_CLOSE: i32 = b')' as i32;
const TOK_BRACKET_OPEN: i32 = b'[' as i32;
const TOK_BRACKET_CLOSE: i32 = b']' as i32;
const TOK_BRACE_OPEN: i32 = b'{' as i32;
const TOK_BRACE_CLOSE: i32 = b'}' as i32;
const TOK_SEMI: i32 = b';' as i32;

fn is_num(c: u8) -> bool {
    c.is_ascii_digit()
}

// C: is_ident_first in mquickjs.c.
pub fn is_ident_first(c: u8) -> bool {
    c.is_ascii_alphabetic() || c == b'_' || c == b'$'
}

// C: is_ident_next in mquickjs.c.
pub fn is_ident_next(c: u8) -> bool {
    is_ident_first(c) || is_num(c)
}

// C: js_parse_escape in mquickjs.c. Returns escape value or -1/-2 on error.
pub fn js_parse_escape(buf: &[u8], plen: &mut usize) -> i32 {
    if buf.is_empty() {
        return -1;
    }
    let mut idx = 0usize;
    let c = buf[idx];
    idx += 1;
    let out = match c {
        b'b' => b'\x08' as i32,
        b'f' => b'\x0c' as i32,
        b'n' => b'\n' as i32,
        b'r' => b'\r' as i32,
        b't' => b'\t' as i32,
        b'v' => b'\x0b' as i32,
        b'\'' | b'\"' | b'\\' => c as i32,
        b'x' => {
            let h0 = match buf.get(idx) {
                Some(&b) => from_hex(b),
                None => return -1,
            };
            idx += 1;
            if h0 < 0 {
                return -1;
            }
            let h1 = match buf.get(idx) {
                Some(&b) => from_hex(b),
                None => return -1,
            };
            idx += 1;
            if h1 < 0 {
                return -1;
            }
            (h0 << 4) | h1
        }
        b'u' => {
            if buf.get(idx) == Some(&b'{') {
                idx += 1;
                let mut val: u32 = 0;
                loop {
                    let h = match buf.get(idx) {
                        Some(&b) => from_hex(b),
                        None => return -1,
                    };
                    if h < 0 {
                        return -1;
                    }
                    val = (val << 4) | (h as u32);
                    if val > 0x10ffff {
                        return -1;
                    }
                    idx += 1;
                    match buf.get(idx) {
                        Some(b'}') => {
                            idx += 1;
                            break;
                        }
                        Some(_) => {}
                        None => return -1,
                    }
                }
                val as i32
            } else {
                let mut val: u32 = 0;
                for _ in 0..4 {
                    let h = match buf.get(idx) {
                        Some(&b) => from_hex(b),
                        None => return -1,
                    };
                    if h < 0 {
                        return -1;
                    }
                    val = (val << 4) | (h as u32);
                    idx += 1;
                }
                val as i32
            }
        }
        b'0' => {
            if buf.get(idx).is_some_and(|b| is_num(*b)) {
                return -1;
            }
            0
        }
        _ => -2,
    };
    *plen = idx;
    out
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    message: &'static str,
    position: usize,
}

impl ParseError {
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

pub struct ParseState<'a> {
    source: &'a [u8],
    buf_len: usize,
    buf_pos: usize,
    got_lf: bool,
    token: Token,
    // Pinned to keep stable addresses for JSValue pointer tagging.
    strings: Vec<Pin<Box<JSString>>>,
    interned: Vec<usize>,
}

impl<'a> ParseState<'a> {
    pub fn new(source: &'a [u8]) -> Self {
        let buf_len = if source.last() == Some(&0) {
            source.len().saturating_sub(1)
        } else {
            source.len()
        };
        Self {
            source,
            buf_len,
            buf_pos: 0,
            got_lf: false,
            token: Token::new(b' ' as i32, 0, TokenExtra::None, JS_NULL),
            strings: Vec::new(),
            interned: Vec::new(),
        }
    }

    pub fn token(&self) -> Token {
        self.token
    }

    pub fn got_lf(&self) -> bool {
        self.got_lf
    }

    pub fn is_label(&self) -> bool {
        self.token.val() == TOK_IDENT && self.byte_at(self.buf_pos) == b':'
    }

    pub fn buf_pos(&self) -> usize {
        self.buf_pos
    }

    pub fn get_pos(&self) -> ParsePos {
        ParsePos::new(
            self.got_lf,
            is_regexp_allowed(self.token.val()),
            self.token.source_pos(),
        )
    }

    pub fn seek_token(&mut self, pos: ParsePos) -> Result<(), ParseError> {
        self.buf_pos = pos.source_pos() as usize;
        self.got_lf = pos.got_lf();
        let prev_val = if pos.regexp_allowed() {
            b' ' as i32
        } else {
            b')' as i32
        };
        self.token = Token::new(prev_val, pos.source_pos(), TokenExtra::None, JS_NULL);
        self.next_token()
    }

    pub fn skip_parens(&mut self, func_name: Option<JSValue>) -> Result<u32, ParseError> {
        let mut state = [0i32; 128];
        let mut level = 0usize;
        let mut bits = 0u32;

        state[level] = 0;
        level += 1;
        loop {
            match self.token.val() {
                TOK_PAREN_OPEN => {
                    if level >= state.len() {
                        return Err(ParseError::new(
                            ERR_TOO_MANY_NESTED_BLOCKS,
                            self.token.source_pos() as usize,
                        ));
                    }
                    state[level] = TOK_PAREN_CLOSE;
                    level += 1;
                }
                TOK_BRACKET_OPEN => {
                    if level >= state.len() {
                        return Err(ParseError::new(
                            ERR_TOO_MANY_NESTED_BLOCKS,
                            self.token.source_pos() as usize,
                        ));
                    }
                    state[level] = TOK_BRACKET_CLOSE;
                    level += 1;
                }
                TOK_BRACE_OPEN => {
                    if level >= state.len() {
                        return Err(ParseError::new(
                            ERR_TOO_MANY_NESTED_BLOCKS,
                            self.token.source_pos() as usize,
                        ));
                    }
                    state[level] = TOK_BRACE_CLOSE;
                    level += 1;
                }
                TOK_PAREN_CLOSE | TOK_BRACKET_CLOSE | TOK_BRACE_CLOSE => {
                    level -= 1;
                    let expected = state[level];
                    if self.token.val() != expected {
                        return Err(ParseError::new(
                            expecting_close_error(expected),
                            self.token.source_pos() as usize,
                        ));
                    }
                }
                TOK_EOF => {
                    let expected = state[level.saturating_sub(1)];
                    return Err(ParseError::new(
                        expecting_close_error(expected),
                        self.token.source_pos() as usize,
                    ));
                }
                TOK_IDENT => {
                    if value_matches_bytes(self.token.value(), b"arguments") {
                        bits |= SKIP_HAS_ARGUMENTS;
                    }
                    if let Some(name) = func_name
                        && self.token.value() == name
                    {
                        bits |= SKIP_HAS_FUNC_NAME;
                    }
                }
                TOK_SEMI => {
                    if level == 2 {
                        bits |= SKIP_HAS_SEMI;
                    }
                }
                _ => {}
            }
            self.next_token()?;
            if level <= 1 {
                break;
            }
        }
        Ok(bits)
    }

    pub fn skip_parens_token(&mut self) -> Result<u32, ParseError> {
        let pos = self.get_pos();
        let bits = self.skip_parens(None)?;
        self.seek_token(pos)?;
        Ok(bits)
    }

    pub fn next_token(&mut self) -> Result<(), ParseError> {
        let prev_val = self.token.val();
        self.got_lf = false;
        let mut pos = self.buf_pos;
        loop {
            let source_pos = pos as SourcePos;
            let c = self.byte_at(pos);
            match c {
                0 => {
                    self.token = Token::new(TOK_EOF, source_pos, TokenExtra::None, JS_NULL);
                    self.buf_pos = pos;
                    return Ok(());
                }
                b'\"' | b'\'' => {
                    pos += 1;
                    let (value, new_pos) = self.parse_string(pos, c)?;
                    pos = new_pos;
                    self.token = Token::new(TOK_STRING, source_pos, TokenExtra::None, value);
                    self.buf_pos = pos;
                    return Ok(());
                }
                b'\n' => {
                    self.got_lf = true;
                    pos += 1;
                }
                b' ' | b'\t' | b'\x0c' | b'\x0b' | b'\r' => {
                    pos += 1;
                }
                b'/' => {
                    let next = self.byte_at(pos + 1);
                    if next == b'*' {
                        pos += 2;
                        loop {
                            let c0 = self.byte_at(pos);
                            if c0 == 0 {
                                return Err(ParseError::new(ERR_UNEXPECTED_END_OF_COMMENT, pos));
                            }
                            if c0 == b'*' && self.byte_at(pos + 1) == b'/' {
                                pos += 2;
                                break;
                            }
                            pos += 1;
                        }
                    } else if next == b'/' {
                        pos += 2;
                        loop {
                            let c0 = self.byte_at(pos);
                            if c0 == 0 || c0 == b'\n' {
                                break;
                            }
                            pos += 1;
                        }
                    } else if is_regexp_allowed(prev_val) {
                        pos += 1;
                        let (value, extra, new_pos) = self.parse_regexp_token(pos)?;
                        pos = new_pos;
                        self.token = Token::new(TOK_REGEXP, source_pos, extra, value);
                        self.buf_pos = pos;
                        return Ok(());
                    } else if next == b'=' {
                        pos += 2;
                        self.token = Token::new(TOK_DIV_ASSIGN, source_pos, TokenExtra::None, JS_NULL);
                        self.buf_pos = pos;
                        return Ok(());
                    } else {
                        pos += 1;
                        self.token = Token::new(b'/' as i32, source_pos, TokenExtra::None, JS_NULL);
                        self.buf_pos = pos;
                        return Ok(());
                    }
                }
                b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'$' => {
                    pos += 1;
                    let (token, new_pos) = self.parse_ident(pos, c, source_pos)?;
                    pos = new_pos;
                    self.token = token;
                    self.buf_pos = pos;
                    return Ok(());
                }
                b'.' => {
                    if is_num(self.byte_at(pos + 1)) {
                        let (value, new_pos) = self.parse_number(pos, source_pos as usize)?;
                        pos = new_pos;
                        self.token = Token::new(TOK_NUMBER, source_pos, TokenExtra::Number(value), JS_NULL);
                        self.buf_pos = pos;
                        return Ok(());
                    }
                    self.token = Token::new(b'.' as i32, source_pos, TokenExtra::None, JS_NULL);
                    self.buf_pos = pos + 1;
                    return Ok(());
                }
                b'0' => {
                    if is_num(self.byte_at(pos + 1)) {
                        return Err(ParseError::new(ERR_INVALID_NUMBER, source_pos as usize));
                    }
                    let (value, new_pos) = self.parse_number(pos, source_pos as usize)?;
                    pos = new_pos;
                    self.token = Token::new(TOK_NUMBER, source_pos, TokenExtra::Number(value), JS_NULL);
                    self.buf_pos = pos;
                    return Ok(());
                }
                b'1'..=b'9' => {
                    let (value, new_pos) = self.parse_number(pos, source_pos as usize)?;
                    pos = new_pos;
                    self.token = Token::new(TOK_NUMBER, source_pos, TokenExtra::Number(value), JS_NULL);
                    self.buf_pos = pos;
                    return Ok(());
                }
                b'*' => {
                    if self.byte_at(pos + 1) == b'=' {
                        pos += 2;
                        self.token = Token::new(TOK_MUL_ASSIGN, source_pos, TokenExtra::None, JS_NULL);
                    } else if self.byte_at(pos + 1) == b'*' {
                        if self.byte_at(pos + 2) == b'=' {
                            pos += 3;
                            self.token = Token::new(TOK_POW_ASSIGN, source_pos, TokenExtra::None, JS_NULL);
                        } else {
                            pos += 2;
                            self.token = Token::new(TOK_POW, source_pos, TokenExtra::None, JS_NULL);
                        }
                    } else {
                        pos += 1;
                        self.token = Token::new(b'*' as i32, source_pos, TokenExtra::None, JS_NULL);
                    }
                    self.buf_pos = pos;
                    return Ok(());
                }
                b'%' => {
                    if self.byte_at(pos + 1) == b'=' {
                        pos += 2;
                        self.token = Token::new(TOK_MOD_ASSIGN, source_pos, TokenExtra::None, JS_NULL);
                    } else {
                        pos += 1;
                        self.token = Token::new(b'%' as i32, source_pos, TokenExtra::None, JS_NULL);
                    }
                    self.buf_pos = pos;
                    return Ok(());
                }
                b'+' => {
                    if self.byte_at(pos + 1) == b'=' {
                        pos += 2;
                        self.token = Token::new(TOK_PLUS_ASSIGN, source_pos, TokenExtra::None, JS_NULL);
                    } else if self.byte_at(pos + 1) == b'+' {
                        pos += 2;
                        self.token = Token::new(TOK_INC, source_pos, TokenExtra::None, JS_NULL);
                    } else {
                        pos += 1;
                        self.token = Token::new(b'+' as i32, source_pos, TokenExtra::None, JS_NULL);
                    }
                    self.buf_pos = pos;
                    return Ok(());
                }
                b'-' => {
                    if self.byte_at(pos + 1) == b'=' {
                        pos += 2;
                        self.token = Token::new(TOK_MINUS_ASSIGN, source_pos, TokenExtra::None, JS_NULL);
                    } else if self.byte_at(pos + 1) == b'-' {
                        pos += 2;
                        self.token = Token::new(TOK_DEC, source_pos, TokenExtra::None, JS_NULL);
                    } else {
                        pos += 1;
                        self.token = Token::new(b'-' as i32, source_pos, TokenExtra::None, JS_NULL);
                    }
                    self.buf_pos = pos;
                    return Ok(());
                }
                b'<' => {
                    if self.byte_at(pos + 1) == b'=' {
                        pos += 2;
                        self.token = Token::new(TOK_LTE, source_pos, TokenExtra::None, JS_NULL);
                    } else if self.byte_at(pos + 1) == b'<' {
                        if self.byte_at(pos + 2) == b'=' {
                            pos += 3;
                            self.token = Token::new(TOK_SHL_ASSIGN, source_pos, TokenExtra::None, JS_NULL);
                        } else {
                            pos += 2;
                            self.token = Token::new(TOK_SHL, source_pos, TokenExtra::None, JS_NULL);
                        }
                    } else {
                        pos += 1;
                        self.token = Token::new(b'<' as i32, source_pos, TokenExtra::None, JS_NULL);
                    }
                    self.buf_pos = pos;
                    return Ok(());
                }
                b'>' => {
                    if self.byte_at(pos + 1) == b'=' {
                        pos += 2;
                        self.token = Token::new(TOK_GTE, source_pos, TokenExtra::None, JS_NULL);
                    } else if self.byte_at(pos + 1) == b'>' {
                        if self.byte_at(pos + 2) == b'>' {
                            if self.byte_at(pos + 3) == b'=' {
                                pos += 4;
                                self.token = Token::new(TOK_SHR_ASSIGN, source_pos, TokenExtra::None, JS_NULL);
                            } else {
                                pos += 3;
                                self.token = Token::new(TOK_SHR, source_pos, TokenExtra::None, JS_NULL);
                            }
                        } else if self.byte_at(pos + 2) == b'=' {
                            pos += 3;
                            self.token = Token::new(TOK_SAR_ASSIGN, source_pos, TokenExtra::None, JS_NULL);
                        } else {
                            pos += 2;
                            self.token = Token::new(TOK_SAR, source_pos, TokenExtra::None, JS_NULL);
                        }
                    } else {
                        pos += 1;
                        self.token = Token::new(b'>' as i32, source_pos, TokenExtra::None, JS_NULL);
                    }
                    self.buf_pos = pos;
                    return Ok(());
                }
                b'=' => {
                    if self.byte_at(pos + 1) == b'=' {
                        if self.byte_at(pos + 2) == b'=' {
                            pos += 3;
                            self.token = Token::new(TOK_STRICT_EQ, source_pos, TokenExtra::None, JS_NULL);
                        } else {
                            pos += 2;
                            self.token = Token::new(TOK_EQ, source_pos, TokenExtra::None, JS_NULL);
                        }
                    } else {
                        pos += 1;
                        self.token = Token::new(b'=' as i32, source_pos, TokenExtra::None, JS_NULL);
                    }
                    self.buf_pos = pos;
                    return Ok(());
                }
                b'!' => {
                    if self.byte_at(pos + 1) == b'=' {
                        if self.byte_at(pos + 2) == b'=' {
                            pos += 3;
                            self.token = Token::new(TOK_STRICT_NEQ, source_pos, TokenExtra::None, JS_NULL);
                        } else {
                            pos += 2;
                            self.token = Token::new(TOK_NEQ, source_pos, TokenExtra::None, JS_NULL);
                        }
                    } else {
                        pos += 1;
                        self.token = Token::new(b'!' as i32, source_pos, TokenExtra::None, JS_NULL);
                    }
                    self.buf_pos = pos;
                    return Ok(());
                }
                b'&' => {
                    if self.byte_at(pos + 1) == b'=' {
                        pos += 2;
                        self.token = Token::new(TOK_AND_ASSIGN, source_pos, TokenExtra::None, JS_NULL);
                    } else if self.byte_at(pos + 1) == b'&' {
                        pos += 2;
                        self.token = Token::new(TOK_LAND, source_pos, TokenExtra::None, JS_NULL);
                    } else {
                        pos += 1;
                        self.token = Token::new(b'&' as i32, source_pos, TokenExtra::None, JS_NULL);
                    }
                    self.buf_pos = pos;
                    return Ok(());
                }
                b'^' => {
                    if self.byte_at(pos + 1) == b'=' {
                        pos += 2;
                        self.token = Token::new(TOK_XOR_ASSIGN, source_pos, TokenExtra::None, JS_NULL);
                    } else {
                        pos += 1;
                        self.token = Token::new(b'^' as i32, source_pos, TokenExtra::None, JS_NULL);
                    }
                    self.buf_pos = pos;
                    return Ok(());
                }
                b'|' => {
                    if self.byte_at(pos + 1) == b'=' {
                        pos += 2;
                        self.token = Token::new(TOK_OR_ASSIGN, source_pos, TokenExtra::None, JS_NULL);
                    } else if self.byte_at(pos + 1) == b'|' {
                        pos += 2;
                        self.token = Token::new(TOK_LOR, source_pos, TokenExtra::None, JS_NULL);
                    } else {
                        pos += 1;
                        self.token = Token::new(b'|' as i32, source_pos, TokenExtra::None, JS_NULL);
                    }
                    self.buf_pos = pos;
                    return Ok(());
                }
                _ => {
                    if c >= 128 {
                        return Err(ParseError::new(ERR_UNEXPECTED_CHARACTER, source_pos as usize));
                    }
                    pos += 1;
                    self.token = Token::new(c as i32, source_pos, TokenExtra::None, JS_NULL);
                    self.buf_pos = pos;
                    return Ok(());
                }
            }
        }
    }

    fn parse_string(&mut self, mut pos: usize, sep: u8) -> Result<(JSValue, usize), ParseError> {
        let mut buf = StringBuffer::with_capacity(16);
        loop {
            let c = self.byte_at(pos);
            if c == 0 || c == b'\n' || c == b'\r' {
                return Err(ParseError::new(ERR_UNEXPECTED_END_OF_STRING, pos));
            }
            pos += 1;
            if c == sep {
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
                    return Err(ParseError::new(ERR_INVALID_ESCAPE, pos));
                }
                if escaped == -2 {
                    continue;
                }
                pos += escape_len;
                if !buf.push_codepoint(escaped as u32) {
                    return Err(ParseError::new(ERR_NO_MEM, pos));
                }
                continue;
            }
            if c >= 0x80 {
                pos -= 1;
                let mut clen = 0usize;
                let codepoint =
                    unicode_from_utf8(self.slice_from(pos), UTF8_CHAR_LEN_MAX, &mut clen);
                if codepoint == -1 {
                    return Err(ParseError::new(ERR_INVALID_UTF8, pos));
                }
                pos += clen;
                if !buf.push_codepoint(codepoint as u32) {
                    return Err(ParseError::new(ERR_NO_MEM, pos));
                }
                continue;
            }
            if !buf.push_codepoint(c as u32) {
                return Err(ParseError::new(ERR_NO_MEM, pos));
            }
        }
        let (bytes, is_ascii) = buf.finish();
        let value = self.value_from_bytes(bytes, is_ascii, false, pos)?;
        Ok((value, pos))
    }

    fn parse_ident(
        &mut self,
        mut pos: usize,
        first: u8,
        source_pos: SourcePos,
    ) -> Result<(Token, usize), ParseError> {
        let mut bytes = Vec::with_capacity(16);
        bytes.push(first);
        while pos < self.buf_len {
            let c = self.source[pos];
            if !is_ident_next(c) {
                break;
            }
            pos += 1;
            bytes.push(c);
        }
        let value = self.intern_bytes(&bytes, source_pos as usize)?;
        let val = keyword_token(&bytes).unwrap_or(TOK_IDENT);
        Ok((Token::new(val, source_pos, TokenExtra::None, value), pos))
    }

    fn parse_regexp_token(
        &mut self,
        mut pos: usize,
    ) -> Result<(JSValue, TokenExtra, usize), ParseError> {
        let start_pos = pos;
        let mut in_class = false;
        loop {
            let mut clen = 0usize;
            let c = unicode_from_utf8(self.slice_from(pos), UTF8_CHAR_LEN_MAX, &mut clen);
            if c == -1 {
                return Err(ParseError::new(ERR_INVALID_UTF8, pos));
            }
            pos += clen;
            if c == 0 || c == b'\n' as i32 || c == b'\r' as i32 {
                return Err(ParseError::new(
                    ERR_UNEXPECTED_LINE_TERMINATOR_IN_REGEXP,
                    pos,
                ));
            }
            if c == b'/' as i32 {
                if !in_class {
                    break;
                }
            } else if c == b'[' as i32 {
                in_class = true;
            } else if c == b']' as i32 {
                in_class = false;
            } else if c == b'\\' as i32 {
                let mut clen = 0usize;
                let esc = unicode_from_utf8(self.slice_from(pos), UTF8_CHAR_LEN_MAX, &mut clen);
                if esc == -1 {
                    return Err(ParseError::new(ERR_INVALID_UTF8, pos));
                }
                if esc == 0 || esc == b'\n' as i32 || esc == b'\r' as i32 {
                    return Err(ParseError::new(
                        ERR_UNEXPECTED_LINE_TERMINATOR_IN_REGEXP,
                        pos,
                    ));
                }
                pos += clen;
            }
        }
        let end_pos = pos - 1;
        let mut flags = 0u32;
        let flags_len = parse_regexp_flags(&mut flags, self.slice_from(pos));
        pos += flags_len;
        if is_ident_next(self.byte_at(pos)) {
            return Err(ParseError::new(ERR_INVALID_REGEXP_FLAGS, pos));
        }
        let bytes = self.source[start_pos..end_pos].to_vec();
        let is_ascii = is_ascii_bytes(&bytes);
        let value = self.value_from_bytes(bytes, is_ascii, false, start_pos)?;
        let extra = TokenExtra::RegExp {
            flags,
            end_pos: end_pos as u32,
        };
        Ok((value, extra, pos))
    }

    fn parse_number(&self, start: usize, err_pos: usize) -> Result<(f64, usize), ParseError> {
        let slice = self.ascii_slice_from(start);
        let input = match std::str::from_utf8(slice) {
            Ok(s) => s,
            Err(_) => return Err(ParseError::new(ERR_INVALID_NUMBER, err_pos)),
        };
        let parsed = js_atod(input, 0, JS_ATOD_ACCEPT_BIN_OCT | JS_ATOD_ACCEPT_UNDERSCORES)
            .map_err(|_| ParseError::new(ERR_INVALID_NUMBER, err_pos))?;
        if parsed.value.is_nan() {
            return Err(ParseError::new(ERR_INVALID_NUMBER, err_pos));
        }
        Ok((parsed.value, start + parsed.next))
    }

    fn intern_bytes(&mut self, bytes: &[u8], err_pos: usize) -> Result<JSValue, ParseError> {
        if let Some(codepoint) = single_codepoint(bytes) {
            return Ok(value_make_special(JS_TAG_STRING_CHAR, codepoint));
        }
        let insert_at = match self.interned.binary_search_by(|&index| {
            compare_string_bytes(bytes, self.strings[index].as_ref().get_ref().buf())
        }) {
            Ok(pos) => {
                let index = self.interned[pos];
                let ptr = NonNull::from(self.strings[index].as_ref().get_ref());
                return Ok(value_from_ptr(ptr));
            }
            Err(pos) => pos,
        };
        let is_ascii = is_ascii_bytes(bytes);
        let string = JSString::new(bytes.to_vec(), true, is_ascii, false)
            .ok_or_else(|| ParseError::new(ERR_NO_MEM, err_pos))?;
        let boxed = Box::pin(string);
        let index = self.strings.len();
        self.strings.push(boxed);
        // Derive the tagged pointer only after the Box is stored to avoid Miri retag UB.
        let ptr = NonNull::from(self.strings[index].as_ref().get_ref());
        let value = value_from_ptr(ptr);
        self.interned.insert(insert_at, index);
        Ok(value)
    }

    pub(crate) fn value_from_bytes(
        &mut self,
        bytes: Vec<u8>,
        is_ascii: bool,
        is_unique: bool,
        err_pos: usize,
    ) -> Result<JSValue, ParseError> {
        if bytes.is_empty() {
            return self.alloc_string(bytes, is_ascii, is_unique, err_pos);
        }
        if let Some(codepoint) = single_codepoint(&bytes) {
            return Ok(value_make_special(JS_TAG_STRING_CHAR, codepoint));
        }
        self.alloc_string(bytes, is_ascii, is_unique, err_pos)
    }

    pub(crate) fn intern_identifier(
        &mut self,
        bytes: &[u8],
        err_pos: usize,
    ) -> Result<JSValue, ParseError> {
        self.intern_bytes(bytes, err_pos)
    }

    fn alloc_string(
        &mut self,
        bytes: Vec<u8>,
        is_ascii: bool,
        is_unique: bool,
        err_pos: usize,
    ) -> Result<JSValue, ParseError> {
        let string = JSString::new(bytes, is_unique, is_ascii, false)
            .ok_or_else(|| ParseError::new(ERR_NO_MEM, err_pos))?;
        let boxed = Box::pin(string);
        let index = self.strings.len();
        self.strings.push(boxed);
        // Obtain the pointer from the stored Box to keep its provenance valid.
        let ptr = NonNull::from(self.strings[index].as_ref().get_ref());
        Ok(value_from_ptr(ptr))
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

fn keyword_token(bytes: &[u8]) -> Option<i32> {
    match bytes {
        b"null" => Some(TOK_NULL),
        b"false" => Some(TOK_FALSE),
        b"true" => Some(TOK_TRUE),
        b"if" => Some(TOK_IF),
        b"else" => Some(TOK_ELSE),
        b"return" => Some(TOK_RETURN),
        b"var" => Some(TOK_VAR),
        b"this" => Some(TOK_THIS),
        b"delete" => Some(TOK_DELETE),
        b"void" => Some(TOK_VOID),
        b"typeof" => Some(TOK_TYPEOF),
        b"new" => Some(TOK_NEW),
        b"in" => Some(TOK_IN),
        b"instanceof" => Some(TOK_INSTANCEOF),
        b"do" => Some(TOK_DO),
        b"while" => Some(TOK_WHILE),
        b"for" => Some(TOK_FOR),
        b"break" => Some(TOK_BREAK),
        b"continue" => Some(TOK_CONTINUE),
        b"switch" => Some(TOK_SWITCH),
        b"case" => Some(TOK_CASE),
        b"default" => Some(TOK_DEFAULT),
        b"throw" => Some(TOK_THROW),
        b"try" => Some(TOK_TRY),
        b"catch" => Some(TOK_CATCH),
        b"finally" => Some(TOK_FINALLY),
        b"function" => Some(TOK_FUNCTION),
        b"debugger" => Some(TOK_DEBUGGER),
        b"with" => Some(TOK_WITH),
        b"class" => Some(TOK_CLASS),
        b"const" => Some(TOK_CONST),
        b"enum" => Some(TOK_ENUM),
        b"export" => Some(TOK_EXPORT),
        b"extends" => Some(TOK_EXTENDS),
        b"import" => Some(TOK_IMPORT),
        b"super" => Some(TOK_SUPER),
        b"implements" => Some(TOK_IMPLEMENTS),
        b"interface" => Some(TOK_INTERFACE),
        b"let" => Some(TOK_LET),
        b"package" => Some(TOK_PACKAGE),
        b"private" => Some(TOK_PRIVATE),
        b"protected" => Some(TOK_PROTECTED),
        b"public" => Some(TOK_PUBLIC),
        b"static" => Some(TOK_STATIC),
        b"yield" => Some(TOK_YIELD),
        _ => None,
    }
}

fn single_codepoint(bytes: &[u8]) -> Option<u32> {
    let mut clen = 0usize;
    let c = utf8_get(bytes, &mut clen);
    if c >= 0 && clen == bytes.len() {
        Some(c as u32)
    } else {
        None
    }
}

fn compare_string_bytes(a: &[u8], b: &[u8]) -> core::cmp::Ordering {
    let len = a.len().min(b.len());
    let mut idx = 0usize;
    while idx < len {
        if a[idx] != b[idx] {
            break;
        }
        idx += 1;
    }
    if idx == len {
        return a.len().cmp(&b.len());
    }
    let c1 = string_get_cp(a, idx);
    let c2 = string_get_cp(b, idx);
    if c1 < 0 || c2 < 0 {
        return a[idx].cmp(&b[idx]);
    }
    if (c1 < 0x10000 && c2 < 0x10000) || (c1 >= 0x10000 && c2 >= 0x10000) {
        return c1.cmp(&c2);
    }
    if c1 < 0x10000 {
        let c2 = 0xd800 + ((c2 - 0x10000) >> 10);
        if c1 <= c2 {
            core::cmp::Ordering::Less
        } else {
            core::cmp::Ordering::Greater
        }
    } else {
        let c1 = 0xd800 + ((c1 - 0x10000) >> 10);
        if c1 < c2 {
            core::cmp::Ordering::Less
        } else {
            core::cmp::Ordering::Greater
        }
    }
}

fn string_get_cp(buf: &[u8], mut idx: usize) -> i32 {
    while idx > 0 && (buf[idx] & 0xc0) == 0x80 {
        idx -= 1;
    }
    let mut clen = 0usize;
    utf8_get(&buf[idx..], &mut clen)
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
                if new_len > crate::containers::JS_STRING_LEN_MAX as usize {
                    return false;
                }
                self.buf.truncate(self.buf.len() - 3);
                self.buf.extend_from_slice(&merged[..merged_len]);
                self.is_ascii = false;
                return true;
            }
        }

        if self.buf.len() + len > crate::containers::JS_STRING_LEN_MAX as usize {
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

fn expecting_close_error(expected: i32) -> &'static str {
    match expected {
        TOK_PAREN_CLOSE => ERR_EXPECTING_CLOSE_PAREN,
        TOK_BRACKET_CLOSE => ERR_EXPECTING_CLOSE_BRACKET,
        TOK_BRACE_CLOSE => ERR_EXPECTING_CLOSE_BRACE,
        _ => ERR_EXPECTING_DELIMITER,
    }
}

pub(crate) fn value_matches_bytes(value: JSValue, bytes: &[u8]) -> bool {
    if value_get_special_tag(value) == JS_TAG_STRING_CHAR {
        if bytes.len() != 1 {
            return false;
        }
        return value_get_special_value(value) as u32 == bytes[0] as u32;
    }
    let Some(ptr) = value_to_ptr::<JSString>(value) else {
        return false;
    };
    // SAFETY: ParseState owns all JSString allocations used in tokens.
    unsafe { ptr.as_ref().buf() == bytes }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cutils::UTF8_CHAR_LEN_MAX;
    use crate::jsvalue::{value_get_special_tag, value_get_special_value, value_to_ptr};
    use crate::parser::regexp_flags::{LRE_FLAG_GLOBAL, LRE_FLAG_IGNORECASE};
    use crate::parser::types::ParsePos;

    fn string_bytes(val: JSValue) -> Vec<u8> {
        if value_get_special_tag(val) == JS_TAG_STRING_CHAR {
            let c = value_get_special_value(val) as u32;
            let mut buf = [0u8; UTF8_CHAR_LEN_MAX];
            let len = unicode_to_utf8(&mut buf, c);
            return buf[..len].to_vec();
        }
        let ptr = value_to_ptr::<JSString>(val).expect("string pointer");
        // SAFETY: ParseState owns the JSString backing this JSValue in these tests.
        unsafe { ptr.as_ref().buf().to_vec() }
    }

    #[test]
    fn ident_classification_matches_c() {
        assert!(is_ident_first(b'a'));
        assert!(is_ident_first(b'Z'));
        assert!(is_ident_first(b'_'));
        assert!(is_ident_first(b'$'));
        assert!(!is_ident_first(b'0'));
        assert!(is_ident_next(b'0'));
        assert!(!is_ident_next(b'-'));
    }

    #[test]
    fn parse_escape_simple_codes() {
        let mut len = 0;
        assert_eq!(js_parse_escape(b"n", &mut len), b'\n' as i32);
        assert_eq!(len, 1);
        assert_eq!(js_parse_escape(b"'", &mut len), b'\'' as i32);
        assert_eq!(len, 1);
        assert_eq!(js_parse_escape(b"\\", &mut len), b'\\' as i32);
        assert_eq!(len, 1);
    }

    #[test]
    fn parse_escape_hex_and_unicode() {
        let mut len = 0;
        assert_eq!(js_parse_escape(b"x41", &mut len), 0x41);
        assert_eq!(len, 3);
        assert_eq!(js_parse_escape(b"u0041", &mut len), 0x41);
        assert_eq!(len, 5);
        assert_eq!(js_parse_escape(b"u{1f600}", &mut len), 0x1f600);
        assert_eq!(len, 8);
    }

    #[test]
    fn parse_escape_zero_rules() {
        let mut len = 0;
        assert_eq!(js_parse_escape(b"0", &mut len), 0);
        assert_eq!(len, 1);
        len = 9;
        assert_eq!(js_parse_escape(b"09", &mut len), -1);
        assert_eq!(len, 9);
    }

    #[test]
    fn parse_escape_invalid_sequences() {
        let mut len = 7;
        assert_eq!(js_parse_escape(b"q", &mut len), -2);
        assert_eq!(len, 1);
        len = 7;
        assert_eq!(js_parse_escape(b"xZ1", &mut len), -1);
        assert_eq!(len, 7);
        len = 7;
        assert_eq!(js_parse_escape(b"u{110000}", &mut len), -1);
        assert_eq!(len, 7);
    }

    #[test]
    fn parse_identifier_keywords() {
        let mut state = ParseState::new(b"if yield foo");
        state.next_token().unwrap();
        assert_eq!(state.token().val(), TOK_IF);
        state.next_token().unwrap();
        assert_eq!(state.token().val(), TOK_YIELD);
        state.next_token().unwrap();
        assert_eq!(state.token().val(), TOK_IDENT);
    }

    #[test]
    fn parse_identifier_interns() {
        let mut state = ParseState::new(b"foo foo");
        state.next_token().unwrap();
        let first = state.token().value();
        state.next_token().unwrap();
        let second = state.token().value();
        assert_eq!(first, second);
    }

    #[test]
    fn parse_string_surrogate_pair_merges() {
        let mut state = ParseState::new(br#""\uD83D\uDE00""#);
        state.next_token().unwrap();
        let token = state.token();
        assert_eq!(token.val(), TOK_STRING);
        assert_eq!(value_get_special_tag(token.value()), JS_TAG_STRING_CHAR);
        assert_eq!(value_get_special_value(token.value()), 0x1f600);
    }

    #[test]
    fn parse_string_invalid_escape_errors() {
        let mut state = ParseState::new(br#""\xZ1""#);
        let err = state.next_token().unwrap_err();
        assert_eq!(err.message(), ERR_INVALID_ESCAPE);
    }

    #[test]
    fn parse_regexp_token_basic() {
        let mut state = ParseState::new(b"/ab+c/gi");
        state.next_token().unwrap();
        let token = state.token();
        assert_eq!(token.val(), TOK_REGEXP);
        match token.extra() {
            TokenExtra::RegExp { flags, end_pos } => {
                assert_eq!(flags, LRE_FLAG_GLOBAL | LRE_FLAG_IGNORECASE);
                assert_eq!(end_pos, 5);
            }
            _ => panic!("expected regexp token"),
        }
        assert_eq!(string_bytes(token.value()), b"ab+c".to_vec());
    }

    #[test]
    fn parse_regexp_invalid_flags_errors() {
        let mut state = ParseState::new(b"/a/gg");
        let err = state.next_token().unwrap_err();
        assert_eq!(err.message(), ERR_INVALID_REGEXP_FLAGS);
    }

    #[test]
    fn next_token_skips_line_comment_and_sets_got_lf() {
        let mut state = ParseState::new(b"//x\n1");
        state.next_token().unwrap();
        assert_eq!(state.token().val(), TOK_NUMBER);
        assert!(state.got_lf());
    }

    #[test]
    fn block_comment_does_not_set_got_lf() {
        let mut state = ParseState::new(b"/*x\n*/1");
        state.next_token().unwrap();
        assert_eq!(state.token().val(), TOK_NUMBER);
        assert!(!state.got_lf());
    }

    #[test]
    fn regexp_disallowed_after_ident() {
        let mut state = ParseState::new(b"a / b");
        state.next_token().unwrap();
        assert_eq!(state.token().val(), TOK_IDENT);
        state.next_token().unwrap();
        assert_eq!(state.token().val(), b'/' as i32);
    }

    #[test]
    fn number_leading_zero_is_invalid() {
        let mut state = ParseState::new(b"012");
        let err = state.next_token().unwrap_err();
        assert_eq!(err.message(), ERR_INVALID_NUMBER);
    }

    #[test]
    fn parse_number_hex_literal() {
        let mut state = ParseState::new(b"0x10");
        state.next_token().unwrap();
        let token = state.token();
        assert_eq!(token.val(), TOK_NUMBER);
        match token.extra() {
            TokenExtra::Number(value) => assert_eq!(value, 16.0),
            _ => panic!("expected number token"),
        }
    }

    #[test]
    fn skip_parens_sets_bits_and_advances_token() {
        let mut state = ParseState::new(b"(arguments; foo) bar");
        state.next_token().unwrap();
        let bits = state.skip_parens(None).unwrap();
        assert_eq!(bits & SKIP_HAS_ARGUMENTS, SKIP_HAS_ARGUMENTS);
        assert_eq!(bits & SKIP_HAS_SEMI, SKIP_HAS_SEMI);
        assert_eq!(bits & SKIP_HAS_FUNC_NAME, 0);
        let token = state.token();
        assert_eq!(token.val(), TOK_IDENT);
        assert_eq!(string_bytes(token.value()), b"bar".to_vec());
    }

    #[test]
    fn skip_parens_detects_func_name() {
        let mut state = ParseState::new(b"foo (foo)");
        state.next_token().unwrap();
        let func_name = state.token().value();
        state.next_token().unwrap();
        let bits = state.skip_parens(Some(func_name)).unwrap();
        assert_eq!(bits & SKIP_HAS_FUNC_NAME, SKIP_HAS_FUNC_NAME);
        assert_eq!(state.token().val(), TOK_EOF);
    }

    #[test]
    fn skip_parens_token_restores_state() {
        let mut state = ParseState::new(b"(a + b) c");
        state.next_token().unwrap();
        let pos = state.get_pos();
        let bits = state.skip_parens_token().unwrap();
        assert_eq!(bits, 0);
        assert_eq!(state.token().val(), b'(' as i32);
        assert_eq!(state.get_pos(), pos);
    }

    #[test]
    fn parse_pos_roundtrip_restores_token() {
        let mut state = ParseState::new(b"foo + bar");
        state.next_token().unwrap();
        let first = state.token();
        let pos = state.get_pos();
        assert_eq!(pos.regexp_allowed(), is_regexp_allowed(first.val()));
        state.next_token().unwrap();
        state.next_token().unwrap();
        state.seek_token(pos).unwrap();
        let token = state.token();
        assert_eq!(token.val(), first.val());
        assert_eq!(token.value(), first.value());
        assert_eq!(token.source_pos(), first.source_pos());
    }

    #[test]
    fn seek_token_regexp_disambiguates_slash() {
        let mut state = ParseState::new(b"/a/");
        let pos = ParsePos::new(false, true, 0);
        state.seek_token(pos).unwrap();
        assert_eq!(state.token().val(), TOK_REGEXP);

        let mut state = ParseState::new(b"/a/");
        let pos = ParsePos::new(false, false, 0);
        state.seek_token(pos).unwrap();
        assert_eq!(state.token().val(), b'/' as i32);
    }
}
