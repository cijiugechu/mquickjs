use crate::context::JSContext;
use crate::cutils::{from_hex, unicode_from_utf8, unicode_to_utf8, utf8_get, UTF8_CHAR_LEN_MAX};
use crate::dtoa::{js_atod, JS_ATOD_ACCEPT_BIN_OCT, JS_ATOD_ACCEPT_UNDERSCORES};
use crate::jsvalue::JSValue;
use crate::string::js_string::is_ascii_bytes;
use crate::string::runtime::string_view;
use std::ptr::NonNull;

use super::parse_state::JSParseState;
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
    ctx: NonNull<JSContext>,
    source: &'a [u8],
    buf_len: usize,
    buf_pos: usize,
    got_lf: bool,
    token: Token,
    parse_state: Option<NonNull<JSParseState>>,
}

impl<'a> ParseState<'a> {
    pub fn new(source: &'a [u8], ctx: &mut JSContext) -> Self {
        let buf_len = if source.last() == Some(&0) {
            source.len().saturating_sub(1)
        } else {
            source.len()
        };
        Self {
            ctx: NonNull::from(ctx),
            source,
            buf_len,
            buf_pos: 0,
            got_lf: false,
            token: Token::new(b' ' as i32, 0, TokenExtra::None, JSValue::JS_NULL),
            parse_state: None,
        }
    }

    pub(crate) fn set_parse_state(&mut self, state: Option<NonNull<JSParseState>>) {
        self.parse_state = state;
        self.sync_parse_state_token();
    }

    pub fn token(&self) -> Token {
        self.token
    }

    pub(crate) fn ctx_mut(&mut self) -> &mut JSContext {
        unsafe { self.ctx.as_mut() }
    }

    fn set_token(&mut self, token: Token) {
        self.token = token;
        self.sync_parse_state_token();
    }

    fn sync_parse_state_token(&mut self) {
        let Some(state) = self.parse_state else {
            return;
        };
        unsafe {
            // SAFETY: parse_state pointer is valid while attached to the parser.
            JSParseState::set_token_raw(state.as_ptr(), self.token);
        }
    }

    pub fn source(&self) -> &[u8] {
        self.source
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

    pub fn buf_len(&self) -> usize {
        self.buf_len
    }

    pub fn reset_to_pos(&mut self, pos: usize) {
        self.buf_pos = pos.min(self.buf_len);
        self.got_lf = false;
        self.set_token(Token::new(
            b' ' as i32,
            self.buf_pos as SourcePos,
            TokenExtra::None,
            JSValue::JS_NULL,
        ));
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
        self.set_token(Token::new(
            prev_val,
            pos.source_pos(),
            TokenExtra::None,
            JSValue::JS_NULL,
        ));
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
                    self.set_token(Token::new(TOK_EOF, source_pos, TokenExtra::None, JSValue::JS_NULL));
                    self.buf_pos = pos;
                    return Ok(());
                }
                b'\"' | b'\'' => {
                    pos += 1;
                    let (value, new_pos) = self.parse_string(pos, c)?;
                    pos = new_pos;
                    self.set_token(Token::new(TOK_STRING, source_pos, TokenExtra::None, value));
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
                        let comment_start = pos;
                        pos += 2;
                        loop {
                            let c0 = self.byte_at(pos);
                            if c0 == 0 {
                                return Err(ParseError::new(ERR_UNEXPECTED_END_OF_COMMENT, comment_start));
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
                        self.set_token(Token::new(TOK_REGEXP, source_pos, extra, value));
                        self.buf_pos = pos;
                        return Ok(());
                    } else if next == b'=' {
                        pos += 2;
                        self.set_token(Token::new(TOK_DIV_ASSIGN, source_pos, TokenExtra::None, JSValue::JS_NULL));
                        self.buf_pos = pos;
                        return Ok(());
                    } else {
                        pos += 1;
                        self.set_token(Token::new(b'/' as i32, source_pos, TokenExtra::None, JSValue::JS_NULL));
                        self.buf_pos = pos;
                        return Ok(());
                    }
                }
                b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'$' => {
                    pos += 1;
                    let (token, new_pos) = self.parse_ident(pos, c, source_pos)?;
                    pos = new_pos;
                    self.set_token(token);
                    self.buf_pos = pos;
                    return Ok(());
                }
                b'.' => {
                    if is_num(self.byte_at(pos + 1)) {
                        let (value, new_pos) = self.parse_number(pos, source_pos as usize)?;
                        pos = new_pos;
                        self.set_token(Token::new(TOK_NUMBER, source_pos, TokenExtra::Number(value), JSValue::JS_NULL));
                        self.buf_pos = pos;
                        return Ok(());
                    }
                    self.set_token(Token::new(b'.' as i32, source_pos, TokenExtra::None, JSValue::JS_NULL));
                    self.buf_pos = pos + 1;
                    return Ok(());
                }
                b'0' => {
                    if is_num(self.byte_at(pos + 1)) {
                        return Err(ParseError::new(ERR_INVALID_NUMBER, source_pos as usize));
                    }
                    let (value, new_pos) = self.parse_number(pos, source_pos as usize)?;
                    pos = new_pos;
                    self.set_token(Token::new(TOK_NUMBER, source_pos, TokenExtra::Number(value), JSValue::JS_NULL));
                    self.buf_pos = pos;
                    return Ok(());
                }
                b'1'..=b'9' => {
                    let (value, new_pos) = self.parse_number(pos, source_pos as usize)?;
                    pos = new_pos;
                    self.set_token(Token::new(TOK_NUMBER, source_pos, TokenExtra::Number(value), JSValue::JS_NULL));
                    self.buf_pos = pos;
                    return Ok(());
                }
                b'*' => {
                    if self.byte_at(pos + 1) == b'=' {
                        pos += 2;
                        self.set_token(Token::new(TOK_MUL_ASSIGN, source_pos, TokenExtra::None, JSValue::JS_NULL));
                    } else if self.byte_at(pos + 1) == b'*' {
                        if self.byte_at(pos + 2) == b'=' {
                            pos += 3;
                            self.set_token(Token::new(TOK_POW_ASSIGN, source_pos, TokenExtra::None, JSValue::JS_NULL));
                        } else {
                            pos += 2;
                            self.set_token(Token::new(TOK_POW, source_pos, TokenExtra::None, JSValue::JS_NULL));
                        }
                    } else {
                        pos += 1;
                        self.set_token(Token::new(b'*' as i32, source_pos, TokenExtra::None, JSValue::JS_NULL));
                    }
                    self.buf_pos = pos;
                    return Ok(());
                }
                b'%' => {
                    if self.byte_at(pos + 1) == b'=' {
                        pos += 2;
                        self.set_token(Token::new(TOK_MOD_ASSIGN, source_pos, TokenExtra::None, JSValue::JS_NULL));
                    } else {
                        pos += 1;
                        self.set_token(Token::new(b'%' as i32, source_pos, TokenExtra::None, JSValue::JS_NULL));
                    }
                    self.buf_pos = pos;
                    return Ok(());
                }
                b'+' => {
                    if self.byte_at(pos + 1) == b'=' {
                        pos += 2;
                        self.set_token(Token::new(TOK_PLUS_ASSIGN, source_pos, TokenExtra::None, JSValue::JS_NULL));
                    } else if self.byte_at(pos + 1) == b'+' {
                        pos += 2;
                        self.set_token(Token::new(TOK_INC, source_pos, TokenExtra::None, JSValue::JS_NULL));
                    } else {
                        pos += 1;
                        self.set_token(Token::new(b'+' as i32, source_pos, TokenExtra::None, JSValue::JS_NULL));
                    }
                    self.buf_pos = pos;
                    return Ok(());
                }
                b'-' => {
                    if self.byte_at(pos + 1) == b'=' {
                        pos += 2;
                        self.set_token(Token::new(TOK_MINUS_ASSIGN, source_pos, TokenExtra::None, JSValue::JS_NULL));
                    } else if self.byte_at(pos + 1) == b'-' {
                        pos += 2;
                        self.set_token(Token::new(TOK_DEC, source_pos, TokenExtra::None, JSValue::JS_NULL));
                    } else {
                        pos += 1;
                        self.set_token(Token::new(b'-' as i32, source_pos, TokenExtra::None, JSValue::JS_NULL));
                    }
                    self.buf_pos = pos;
                    return Ok(());
                }
                b'<' => {
                    if self.byte_at(pos + 1) == b'=' {
                        pos += 2;
                        self.set_token(Token::new(TOK_LTE, source_pos, TokenExtra::None, JSValue::JS_NULL));
                    } else if self.byte_at(pos + 1) == b'<' {
                        if self.byte_at(pos + 2) == b'=' {
                            pos += 3;
                            self.set_token(Token::new(TOK_SHL_ASSIGN, source_pos, TokenExtra::None, JSValue::JS_NULL));
                        } else {
                            pos += 2;
                            self.set_token(Token::new(TOK_SHL, source_pos, TokenExtra::None, JSValue::JS_NULL));
                        }
                    } else {
                        pos += 1;
                        self.set_token(Token::new(b'<' as i32, source_pos, TokenExtra::None, JSValue::JS_NULL));
                    }
                    self.buf_pos = pos;
                    return Ok(());
                }
                b'>' => {
                    if self.byte_at(pos + 1) == b'=' {
                        pos += 2;
                        self.set_token(Token::new(TOK_GTE, source_pos, TokenExtra::None, JSValue::JS_NULL));
                    } else if self.byte_at(pos + 1) == b'>' {
                        if self.byte_at(pos + 2) == b'>' {
                            if self.byte_at(pos + 3) == b'=' {
                                pos += 4;
                                self.set_token(Token::new(TOK_SHR_ASSIGN, source_pos, TokenExtra::None, JSValue::JS_NULL));
                            } else {
                                pos += 3;
                                self.set_token(Token::new(TOK_SHR, source_pos, TokenExtra::None, JSValue::JS_NULL));
                            }
                        } else if self.byte_at(pos + 2) == b'=' {
                            pos += 3;
                            self.set_token(Token::new(TOK_SAR_ASSIGN, source_pos, TokenExtra::None, JSValue::JS_NULL));
                        } else {
                            pos += 2;
                            self.set_token(Token::new(TOK_SAR, source_pos, TokenExtra::None, JSValue::JS_NULL));
                        }
                    } else {
                        pos += 1;
                        self.set_token(Token::new(b'>' as i32, source_pos, TokenExtra::None, JSValue::JS_NULL));
                    }
                    self.buf_pos = pos;
                    return Ok(());
                }
                b'=' => {
                    if self.byte_at(pos + 1) == b'=' {
                        if self.byte_at(pos + 2) == b'=' {
                            pos += 3;
                            self.set_token(Token::new(TOK_STRICT_EQ, source_pos, TokenExtra::None, JSValue::JS_NULL));
                        } else {
                            pos += 2;
                            self.set_token(Token::new(TOK_EQ, source_pos, TokenExtra::None, JSValue::JS_NULL));
                        }
                    } else {
                        pos += 1;
                        self.set_token(Token::new(b'=' as i32, source_pos, TokenExtra::None, JSValue::JS_NULL));
                    }
                    self.buf_pos = pos;
                    return Ok(());
                }
                b'!' => {
                    if self.byte_at(pos + 1) == b'=' {
                        if self.byte_at(pos + 2) == b'=' {
                            pos += 3;
                            self.set_token(Token::new(TOK_STRICT_NEQ, source_pos, TokenExtra::None, JSValue::JS_NULL));
                        } else {
                            pos += 2;
                            self.set_token(Token::new(TOK_NEQ, source_pos, TokenExtra::None, JSValue::JS_NULL));
                        }
                    } else {
                        pos += 1;
                        self.set_token(Token::new(b'!' as i32, source_pos, TokenExtra::None, JSValue::JS_NULL));
                    }
                    self.buf_pos = pos;
                    return Ok(());
                }
                b'&' => {
                    if self.byte_at(pos + 1) == b'=' {
                        pos += 2;
                        self.set_token(Token::new(TOK_AND_ASSIGN, source_pos, TokenExtra::None, JSValue::JS_NULL));
                    } else if self.byte_at(pos + 1) == b'&' {
                        pos += 2;
                        self.set_token(Token::new(TOK_LAND, source_pos, TokenExtra::None, JSValue::JS_NULL));
                    } else {
                        pos += 1;
                        self.set_token(Token::new(b'&' as i32, source_pos, TokenExtra::None, JSValue::JS_NULL));
                    }
                    self.buf_pos = pos;
                    return Ok(());
                }
                b'^' => {
                    if self.byte_at(pos + 1) == b'=' {
                        pos += 2;
                        self.set_token(Token::new(TOK_XOR_ASSIGN, source_pos, TokenExtra::None, JSValue::JS_NULL));
                    } else {
                        pos += 1;
                        self.set_token(Token::new(b'^' as i32, source_pos, TokenExtra::None, JSValue::JS_NULL));
                    }
                    self.buf_pos = pos;
                    return Ok(());
                }
                b'|' => {
                    if self.byte_at(pos + 1) == b'=' {
                        pos += 2;
                        self.set_token(Token::new(TOK_OR_ASSIGN, source_pos, TokenExtra::None, JSValue::JS_NULL));
                    } else if self.byte_at(pos + 1) == b'|' {
                        pos += 2;
                        self.set_token(Token::new(TOK_LOR, source_pos, TokenExtra::None, JSValue::JS_NULL));
                    } else {
                        pos += 1;
                        self.set_token(Token::new(b'|' as i32, source_pos, TokenExtra::None, JSValue::JS_NULL));
                    }
                    self.buf_pos = pos;
                    return Ok(());
                }
                _ => {
                    if c >= 128 {
                        return Err(ParseError::new(ERR_UNEXPECTED_CHARACTER, source_pos as usize));
                    }
                    pos += 1;
                    self.set_token(Token::new(c as i32, source_pos, TokenExtra::None, JSValue::JS_NULL));
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
        let ctx = self.ctx_mut();
        ctx.intern_string(bytes)
            .map_err(|_| ParseError::new(ERR_NO_MEM, err_pos))
    }

    pub(crate) fn value_from_bytes(
        &mut self,
        bytes: Vec<u8>,
        _is_ascii: bool,
        is_unique: bool,
        err_pos: usize,
    ) -> Result<JSValue, ParseError> {
        let ctx = self.ctx_mut();
        let value = ctx
            .new_string_len(&bytes)
            .map_err(|_| ParseError::new(ERR_NO_MEM, err_pos))?;
        if !is_unique {
            return Ok(value);
        }
        Ok(ctx.atom_tables_mut().make_unique_string(value))
    }

    pub(crate) fn intern_identifier(
        &mut self,
        bytes: &[u8],
        err_pos: usize,
    ) -> Result<JSValue, ParseError> {
        self.intern_bytes(bytes, err_pos)
    }

    pub(crate) fn atomize_value(&mut self, value: JSValue) -> JSValue {
        self.ctx_mut().atom_tables_mut().make_unique_string(value)
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
    let mut scratch = [0u8; 5];
    let Some(view) = string_view(value, &mut scratch) else {
        return false;
    };
    view.bytes() == bytes
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::{ContextConfig, JSContext};
    use crate::parser::regexp_flags::{LRE_FLAG_GLOBAL, LRE_FLAG_IGNORECASE};
    use crate::parser::types::ParsePos;
    use crate::stdlib::MQUICKJS_STDLIB_IMAGE;

    fn string_bytes(val: JSValue) -> Vec<u8> {
        let mut scratch = [0u8; 5];
        let view = string_view(val, &mut scratch).expect("string view");
        view.bytes().to_vec()
    }

    fn new_state<'a>(input: &'a [u8], ctx: &mut JSContext) -> ParseState<'a> {
        ParseState::new(input, ctx)
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
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let mut state = new_state(b"if yield foo", &mut ctx);
        state.next_token().unwrap();
        assert_eq!(state.token().val(), TOK_IF);
        state.next_token().unwrap();
        assert_eq!(state.token().val(), TOK_YIELD);
        state.next_token().unwrap();
        assert_eq!(state.token().val(), TOK_IDENT);
    }

    #[test]
    fn parse_identifier_interns() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let mut state = new_state(b"foo foo", &mut ctx);
        state.next_token().unwrap();
        let first = state.token().value();
        state.next_token().unwrap();
        let second = state.token().value();
        assert_eq!(first, second);
    }

    #[test]
    fn parse_string_surrogate_pair_merges() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let mut state = new_state(br#""\uD83D\uDE00""#, &mut ctx);
        state.next_token().unwrap();
        let token = state.token();
        assert_eq!(token.val(), TOK_STRING);
        assert_eq!(token.value().get_special_tag(), JSValue::JS_TAG_STRING_CHAR);
        assert_eq!(token.value().get_special_value(), 0x1f600);
    }

    #[test]
    fn parse_string_invalid_escape_errors() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let mut state = new_state(br#""\xZ1""#, &mut ctx);
        let err = state.next_token().unwrap_err();
        assert_eq!(err.message(), ERR_INVALID_ESCAPE);
    }

    #[test]
    fn parse_regexp_token_basic() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let mut state = new_state(b"/ab+c/gi", &mut ctx);
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
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let mut state = new_state(b"/a/gg", &mut ctx);
        let err = state.next_token().unwrap_err();
        assert_eq!(err.message(), ERR_INVALID_REGEXP_FLAGS);
    }

    #[test]
    fn next_token_skips_line_comment_and_sets_got_lf() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let mut state = new_state(b"//x\n1", &mut ctx);
        state.next_token().unwrap();
        assert_eq!(state.token().val(), TOK_NUMBER);
        assert!(state.got_lf());
    }

    #[test]
    fn block_comment_does_not_set_got_lf() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let mut state = new_state(b"/*x\n*/1", &mut ctx);
        state.next_token().unwrap();
        assert_eq!(state.token().val(), TOK_NUMBER);
        assert!(!state.got_lf());
    }

    #[test]
    fn regexp_disallowed_after_ident() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let mut state = new_state(b"a / b", &mut ctx);
        state.next_token().unwrap();
        assert_eq!(state.token().val(), TOK_IDENT);
        state.next_token().unwrap();
        assert_eq!(state.token().val(), b'/' as i32);
    }

    #[test]
    fn number_leading_zero_is_invalid() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let mut state = new_state(b"012", &mut ctx);
        let err = state.next_token().unwrap_err();
        assert_eq!(err.message(), ERR_INVALID_NUMBER);
    }

    #[test]
    fn parse_number_hex_literal() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let mut state = new_state(b"0x10", &mut ctx);
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
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let mut state = new_state(b"(arguments; foo) bar", &mut ctx);
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
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let mut state = new_state(b"foo (foo)", &mut ctx);
        state.next_token().unwrap();
        let func_name = state.token().value();
        state.next_token().unwrap();
        let bits = state.skip_parens(Some(func_name)).unwrap();
        assert_eq!(bits & SKIP_HAS_FUNC_NAME, SKIP_HAS_FUNC_NAME);
        assert_eq!(state.token().val(), TOK_EOF);
    }

    #[test]
    fn skip_parens_token_restores_state() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let mut state = new_state(b"(a + b) c", &mut ctx);
        state.next_token().unwrap();
        let pos = state.get_pos();
        let bits = state.skip_parens_token().unwrap();
        assert_eq!(bits, 0);
        assert_eq!(state.token().val(), b'(' as i32);
        assert_eq!(state.get_pos(), pos);
    }

    #[test]
    fn parse_pos_roundtrip_restores_token() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let mut state = new_state(b"foo + bar", &mut ctx);
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
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let mut state = new_state(b"/a/", &mut ctx);
        let pos = ParsePos::new(false, true, 0);
        state.seek_token(pos).unwrap();
        assert_eq!(state.token().val(), TOK_REGEXP);

        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let mut state = new_state(b"/a/", &mut ctx);
        let pos = ParsePos::new(false, false, 0);
        state.seek_token(pos).unwrap();
        assert_eq!(state.token().val(), b'/' as i32);
    }
}
