use crate::cutils::{
    get_u16, get_u32, put_u16, put_u32, unicode_from_utf8, unicode_to_utf8, UTF8_CHAR_LEN_MAX,
};
use crate::jsvalue::JSValue;
use crate::opcode::{
    RegExpOpCode, REOP_ANY, REOP_BACK_REFERENCE, REOP_BACK_REFERENCE_I, REOP_CHAR1, REOP_CHAR4,
    REOP_CHECK_ADVANCE, REOP_DOT, REOP_GOTO, REOP_LINE_END, REOP_LINE_END_M, REOP_LINE_START,
    REOP_LINE_START_M, REOP_LOOKAHEAD, REOP_LOOKAHEAD_MATCH, REOP_LOOP,
    REOP_LOOP_CHECK_ADV_SPLIT_GOTO_FIRST, REOP_LOOP_CHECK_ADV_SPLIT_NEXT_FIRST,
    REOP_LOOP_SPLIT_GOTO_FIRST, REOP_LOOP_SPLIT_NEXT_FIRST, REOP_MATCH,
    REOP_NEGATIVE_LOOKAHEAD_MATCH, REOP_NOT_SPACE,
    REOP_NOT_WORD_BOUNDARY, REOP_RANGE, REOP_RANGE8, REOP_SAVE_END, REOP_SAVE_RESET,
    REOP_SAVE_START, REOP_SET_CHAR_POS, REOP_SET_I32, REOP_SPACE, REOP_SPLIT_GOTO_FIRST,
    REOP_SPLIT_NEXT_FIRST, REOP_WORD_BOUNDARY, RE_OPCODES,
};
use crate::parser::lexer::js_parse_escape;
use crate::parser::regexp_flags::{
    LRE_FLAG_DOTALL, LRE_FLAG_IGNORECASE, LRE_FLAG_MULTILINE, LRE_FLAG_STICKY, LRE_FLAG_UNICODE,
};
use core::cmp::{max, min};

const RE_HEADER_FLAGS: usize = 0;
const RE_HEADER_CAPTURE_COUNT: usize = 2;
const RE_HEADER_REGISTER_COUNT: usize = 3;
const RE_HEADER_LEN: usize = 4;

const CAPTURE_COUNT_MAX: u8 = 255;
const REGISTER_COUNT_MAX: u8 = 255;
const CLASS_RANGE_BASE: u32 = 0x4000_0000;

const CHAR_RANGE_DIGIT: u32 = 0;
const CHAR_RANGE_NOT_DIGIT: u32 = 1;
const CHAR_RANGE_SPACE: u32 = 2;
const CHAR_RANGE_NOT_SPACE: u32 = 3;
const CHAR_RANGE_WORD: u32 = 4;
const CHAR_RANGE_NOT_WORD: u32 = 5;

const ERR_EXPECTING_CLOSE_PAREN: &str = "expecting ')'";
const ERR_EXPECTING_CLOSE_BRACE: &str = "expecting '}'";
const ERR_INVALID_ESCAPE_IN_REGEXP: &str = "invalid escape sequence in regular expression";
const ERR_UNEXPECTED_END: &str = "unexpected end";
const ERR_MALFORMED_UNICODE_CHAR: &str = "malformed unicode char";
const ERR_RANGE_TOO_BIG: &str = "range too big";
const ERR_INVALID_CLASS_RANGE: &str = "invalid class range";
const ERR_INVALID_REPETITION_COUNT: &str = "invalid repetition count";
const ERR_NOTHING_TO_REPEAT: &str = "nothing to repeat";
const ERR_SYNTAX_ERROR: &str = "syntax error";
const ERR_INVALID_DECIMAL_ESCAPE: &str = "invalid decimal escape in regular expression";
const ERR_INVALID_GROUP: &str = "invalid group";
const ERR_TOO_MANY_CAPTURES: &str = "too many captures";
const ERR_BACK_REFERENCE_OUT_OF_RANGE: &str = "back reference is out of range";
const ERR_TOO_MANY_REGEXP_REGISTERS: &str = "too many regexp registers";
const ERR_EXTRANEOUS_CHARACTERS: &str = "extraneous characters at the end";

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RegExpError {
    message: &'static str,
    position: usize,
}

impl RegExpError {
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RegExpBytecode {
    buf: Vec<u8>,
}

impl RegExpBytecode {
    pub fn bytes(&self) -> &[u8] {
        &self.buf
    }

    pub fn flags(&self) -> u16 {
        get_u16(&self.buf[RE_HEADER_FLAGS..RE_HEADER_FLAGS + 2]) as u16
    }

    pub fn capture_count(&self) -> u8 {
        self.buf[RE_HEADER_CAPTURE_COUNT]
    }

    pub fn register_count(&self) -> u8 {
        self.buf[RE_HEADER_REGISTER_COUNT]
    }

    pub fn bytecode(&self) -> &[u8] {
        &self.buf[RE_HEADER_LEN..]
    }
}

pub fn compile_regexp(source: &[u8], flags: u32) -> Result<RegExpBytecode, RegExpError> {
    let mut parser = RegExpParser::new(source, flags);
    parser.compile()
}

struct BytecodeBuilder {
    buf: Vec<u8>,
}

impl BytecodeBuilder {
    fn new() -> Self {
        Self { buf: Vec::new() }
    }

    fn len(&self) -> usize {
        self.buf.len()
    }

    fn truncate(&mut self, len: usize) {
        self.buf.truncate(len);
    }

    fn emit_u8(&mut self, val: u8) {
        self.buf.push(val);
    }

    fn emit_u16(&mut self, val: u16) {
        let mut tmp = [0u8; 2];
        put_u16(&mut tmp, val);
        self.buf.extend_from_slice(&tmp);
    }

    fn emit_u32(&mut self, val: u32) {
        let mut tmp = [0u8; 4];
        put_u32(&mut tmp, val);
        self.buf.extend_from_slice(&tmp);
    }

    fn insert(&mut self, pos: usize, len: usize) {
        let old_len = self.buf.len();
        self.buf.resize(old_len + len, 0);
        self.buf.copy_within(pos..old_len, pos + len);
        for slot in &mut self.buf[pos..pos + len] {
            *slot = 0;
        }
    }

    fn write_u8(&mut self, pos: usize, val: u8) {
        self.buf[pos] = val;
    }

    fn write_u32(&mut self, pos: usize, val: u32) {
        put_u32(&mut self.buf[pos..pos + 4], val);
    }

    fn read_u8(&self, pos: usize) -> u8 {
        self.buf[pos]
    }
}

struct RegExpParser<'a> {
    source: &'a [u8],
    buf_len: usize,
    buf_pos: usize,
    flags: u32,
    multi_line: bool,
    dotall: bool,
    ignore_case: bool,
    is_unicode: bool,
    capture_count: u8,
    bytecode: BytecodeBuilder,
}

impl<'a> RegExpParser<'a> {
    fn new(source: &'a [u8], flags: u32) -> Self {
        let buf_len = if source.last() == Some(&0) {
            source.len().saturating_sub(1)
        } else {
            source.len()
        };
        Self {
            source,
            buf_len,
            buf_pos: 0,
            flags,
            multi_line: (flags & LRE_FLAG_MULTILINE) != 0,
            dotall: (flags & LRE_FLAG_DOTALL) != 0,
            ignore_case: (flags & LRE_FLAG_IGNORECASE) != 0,
            is_unicode: (flags & LRE_FLAG_UNICODE) != 0,
            capture_count: 1,
            bytecode: BytecodeBuilder::new(),
        }
    }

    fn compile(&mut self) -> Result<RegExpBytecode, RegExpError> {
        self.bytecode.emit_u16(self.flags as u16);
        self.bytecode.emit_u8(0);
        self.bytecode.emit_u8(0);

        if (self.flags & LRE_FLAG_STICKY) == 0 {
            self.re_emit_op_u32(opcode_u8(REOP_SPLIT_GOTO_FIRST), 1 + 5);
            self.re_emit_op(opcode_u8(REOP_ANY));
            self.re_emit_op_u32(opcode_u8(REOP_GOTO), (-(5i32 + 1 + 5)) as u32);
        }
        self.re_emit_op_u8(opcode_u8(REOP_SAVE_START), 0);

        self.parse_disjunction()?;

        self.re_emit_op_u8(opcode_u8(REOP_SAVE_END), 0);
        self.re_emit_op(opcode_u8(REOP_MATCH));

        if self.buf_pos != self.buf_len {
            return Err(self.error(ERR_EXTRANEOUS_CHARACTERS));
        }

        if self.bytecode.len() < RE_HEADER_LEN {
            return Err(self.error(ERR_EXTRANEOUS_CHARACTERS));
        }
        self.bytecode.buf[RE_HEADER_CAPTURE_COUNT] = self.capture_count;

        let reg_count = self.compute_register_count()?;
        self.bytecode.buf[RE_HEADER_REGISTER_COUNT] = reg_count;

        let buf = core::mem::take(&mut self.bytecode.buf);
        Ok(RegExpBytecode { buf })
    }

    fn error(&self, message: &'static str) -> RegExpError {
        RegExpError::new(message, self.buf_pos)
    }

    fn byte_at(&self, pos: usize) -> u8 {
        self.source.get(pos).copied().unwrap_or(0)
    }

    fn re_emit_op(&mut self, op: u8) {
        self.bytecode.emit_u8(op);
    }

    fn re_emit_op_u8(&mut self, op: u8, val: u32) {
        self.bytecode.emit_u8(op);
        self.bytecode.emit_u8(val as u8);
    }

    fn re_emit_op_u16(&mut self, op: u8, val: u32) {
        self.bytecode.emit_u8(op);
        self.bytecode.emit_u16(val as u16);
    }

    fn re_emit_op_u32(&mut self, op: u8, val: u32) -> usize {
        self.bytecode.emit_u8(op);
        let pos = self.bytecode.len();
        self.bytecode.emit_u32(val);
        pos
    }

    fn re_emit_goto(&mut self, op: u8, val: usize) -> usize {
        self.bytecode.emit_u8(op);
        let pos = self.bytecode.len();
        let rel = val as i32 - (pos as i32 + 4);
        self.bytecode.emit_u32(rel as u32);
        pos
    }

    fn re_emit_goto_u8(&mut self, op: u8, arg: u32, val: usize) -> usize {
        self.bytecode.emit_u8(op);
        self.bytecode.emit_u8(arg as u8);
        let pos = self.bytecode.len();
        let rel = val as i32 - (pos as i32 + 4);
        self.bytecode.emit_u32(rel as u32);
        pos
    }

    fn re_emit_goto_u8_u32(&mut self, op: u8, arg0: u32, arg1: u32, val: usize) -> usize {
        self.bytecode.emit_u8(op);
        self.bytecode.emit_u8(arg0 as u8);
        self.bytecode.emit_u32(arg1);
        let pos = self.bytecode.len();
        let rel = val as i32 - (pos as i32 + 4);
        self.bytecode.emit_u32(rel as u32);
        pos
    }

    fn re_emit_char(&mut self, c: u32) {
        let mut buf = [0u8; 4];
        let n = unicode_to_utf8(&mut buf, c);
        self.re_emit_op(opcode_u8(RegExpOpCode::from_u16(
            REOP_CHAR1.as_u16() + n as u16 - 1,
        )));
        for b in &buf[..n] {
            self.bytecode.emit_u8(*b);
        }
    }

    fn parse_expect(&mut self, c: u8) -> Result<(), RegExpError> {
        if self.byte_at(self.buf_pos) != c {
            let message = match c {
                b')' => ERR_EXPECTING_CLOSE_PAREN,
                b'}' => ERR_EXPECTING_CLOSE_BRACE,
                _ => ERR_SYNTAX_ERROR,
            };
            return Err(self.error(message));
        }
        self.buf_pos += 1;
        Ok(())
    }

    fn parse_digits(&self, pos: &mut usize) -> i32 {
        let mut v: u64 = 0;
        while let Some(&c) = self.source.get(*pos) {
            if !is_digit(c) {
                break;
            }
            v = v.saturating_mul(10).saturating_add((c - b'0') as u64);
            if v >= JSValue::JS_SHORTINT_MAX as u64 {
                v = JSValue::JS_SHORTINT_MAX as u64;
            }
            *pos += 1;
        }
        v as i32
    }

    fn need_check_adv_and_capture_init(
        &self,
        bc_buf: &[u8],
    ) -> (bool, bool) {
        let mut need_check_adv = true;
        let mut need_capture_init = false;
        let mut pos = 0usize;
        while pos < bc_buf.len() {
            let opcode = bc_buf[pos];
            let mut len = opcode_size(opcode);
            match opcode {
                x if x == opcode_u8(REOP_RANGE8) => {
                    let val = bc_buf[pos + 1] as usize;
                    len += val * 2;
                    need_check_adv = false;
                }
                x if x == opcode_u8(REOP_RANGE) => {
                    let val = get_u16(&bc_buf[pos + 1..pos + 3]) as usize;
                    len += val * 8;
                    need_check_adv = false;
                }
                x if x >= opcode_u8(REOP_CHAR1) && x <= opcode_u8(REOP_CHAR4) => {
                    need_check_adv = false;
                }
                x if x == opcode_u8(REOP_DOT)
                    || x == opcode_u8(REOP_ANY)
                    || x == opcode_u8(REOP_SPACE)
                    || x == opcode_u8(REOP_NOT_SPACE) =>
                {
                    need_check_adv = false;
                }
                x if x == opcode_u8(REOP_LINE_START)
                    || x == opcode_u8(REOP_LINE_START_M)
                    || x == opcode_u8(REOP_LINE_END)
                    || x == opcode_u8(REOP_LINE_END_M)
                    || x == opcode_u8(REOP_SET_I32)
                    || x == opcode_u8(REOP_SET_CHAR_POS)
                    || x == opcode_u8(REOP_WORD_BOUNDARY)
                    || x == opcode_u8(REOP_NOT_WORD_BOUNDARY) => {}
                x if x == opcode_u8(REOP_SAVE_START)
                    || x == opcode_u8(REOP_SAVE_END)
                    || x == opcode_u8(REOP_SAVE_RESET) => {}
                _ => {
                    need_capture_init = true;
                    break;
                }
            }
            pos += len;
        }
        (need_check_adv, need_capture_init)
    }

    fn get_class_atom(&mut self, in_class: bool) -> Result<u32, RegExpError> {
        let mut p = self.buf_pos;
        let mut c = self.byte_at(p);
        let out = match c {
            b'\\' => {
                p += 1;
                c = self.byte_at(p);
                p += 1;
                match c {
                    b'd' => CLASS_RANGE_BASE + CHAR_RANGE_DIGIT,
                    b'D' => CLASS_RANGE_BASE + CHAR_RANGE_NOT_DIGIT,
                    b's' => CLASS_RANGE_BASE + CHAR_RANGE_SPACE,
                    b'S' => CLASS_RANGE_BASE + CHAR_RANGE_NOT_SPACE,
                    b'w' => CLASS_RANGE_BASE + CHAR_RANGE_WORD,
                    b'W' => CLASS_RANGE_BASE + CHAR_RANGE_NOT_WORD,
                    b'c' => {
                        let next = self.byte_at(p);
                        if next.is_ascii_lowercase()
                            || next.is_ascii_uppercase()
                            || ((next.is_ascii_digit() || next == b'_') && in_class && !self.is_unicode)
                        {
                            p += 1;
                            (next & 0x1f) as u32
                        } else if self.is_unicode {
                            self.buf_pos = p;
                            return Err(self.error(ERR_INVALID_ESCAPE_IN_REGEXP));
                        } else {
                            p -= 1;
                            b'\\' as u32
                        }
                    }
                    b'-' => {
                        if !in_class && self.is_unicode {
                            self.buf_pos = p - 1;
                            return Err(self.error(ERR_INVALID_ESCAPE_IN_REGEXP));
                        }
                        b'-' as u32
                    }
                    b'^' | b'$' | b'\\' | b'.' | b'*' | b'+' | b'?' | b'(' | b')' | b'['
                    | b']' | b'{' | b'}' | b'|' | b'/' => c as u32,
                    _ => {
                        p -= 1;
                        let slice = &self.source[p..];
                        let mut len = 0usize;
                        let ret = js_parse_escape(slice, &mut len);
                        if ret < 0 {
                            if self.is_unicode {
                                self.buf_pos = p;
                                return Err(self.error(ERR_INVALID_ESCAPE_IN_REGEXP));
                            }
                            let (ch, consumed) = self.get_class_atom_fallback(p)?;
                            p += consumed;
                            ch
                        } else {
                            p += len;
                            ret as u32
                        }
                    }
                }
            }
            0 | b'/' => {
                if p >= self.buf_len {
                    return Err(self.error(ERR_UNEXPECTED_END));
                }
                let (ch, consumed) = self.get_class_atom_fallback(p)?;
                p += consumed;
                ch
            }
            _ => {
                let (ch, consumed) = self.get_class_atom_fallback(p)?;
                p += consumed;
                ch
            }
        };
        self.buf_pos = p;
        Ok(out)
    }

    fn get_class_atom_fallback(&self, pos: usize) -> Result<(u32, usize), RegExpError> {
        let mut len = 0usize;
        let ret = unicode_from_utf8(&self.source[pos..], UTF8_CHAR_LEN_MAX, &mut len);
        if ret < 0 {
            return Err(RegExpError::new(ERR_MALFORMED_UNICODE_CHAR, pos));
        }
        Ok((ret as u32, len))
    }

    fn emit_range_base(&mut self, c: u32) {
        let invert = (c & 1) != 0;
        if invert {
            self.bytecode.emit_u32(0);
        }
        match c & !1 {
            CHAR_RANGE_DIGIT => {
                self.bytecode.emit_u32(0x30);
                self.bytecode.emit_u32(0x39 + 1);
            }
            CHAR_RANGE_SPACE => {
                for val in CHAR_RANGE_S_TABLE {
                    self.bytecode.emit_u32(*val);
                }
            }
            CHAR_RANGE_WORD => {
                for val in CHAR_RANGE_W_TABLE {
                    self.bytecode.emit_u32(*val);
                }
            }
            _ => {}
        }
        if invert {
            self.bytecode.emit_u32(0x110000);
        }
    }

    fn range_optimize(&mut self, range_start: usize, invert: bool) -> Result<(), RegExpError> {
        let mut intervals = Vec::new();
        let mut pos = range_start;
        while pos + 8 <= self.bytecode.len() {
            let start = get_u32(&self.bytecode.buf[pos..pos + 4]);
            let end = get_u32(&self.bytecode.buf[pos + 4..pos + 8]);
            intervals.push((start, end));
            pos += 8;
        }
        intervals.sort_by_key(|(start, _)| *start);
        intervals = compress_intervals(&intervals);

        if invert {
            let mut inverted = Vec::with_capacity(intervals.len() + 1);
            let mut prev_end = 0u32;
            for (start, end) in &intervals {
                inverted.push((prev_end, *start));
                prev_end = *end;
            }
            inverted.push((prev_end, 0x110000));
            intervals = compress_intervals(&inverted);
        }

        if intervals.len() > u16::MAX as usize - 1 {
            return Err(self.error(ERR_RANGE_TOO_BIG));
        }

        if intervals.is_empty() {
            self.bytecode.truncate(range_start - 3);
            self.re_emit_op_u16(opcode_u8(REOP_RANGE), 0);
            return Ok(());
        }

        if intervals.len() < 16 {
            let (last_start, last_end) = intervals[intervals.len() - 1];
            if last_end < 254 || (last_end == 0x110000 && last_start < 254) {
                self.bytecode.truncate(range_start - 3);
                self.re_emit_op_u8(opcode_u8(REOP_RANGE8), intervals.len() as u32);
                for (start, end) in &intervals {
                    let mut s = *start;
                    let mut e = *end;
                    if s == 0x110000 {
                        s = 0xff;
                    }
                    if e == 0x110000 {
                        e = 0xff;
                    }
                    self.bytecode.emit_u8(s as u8);
                    self.bytecode.emit_u8(e as u8);
                }
                return Ok(());
            }
        }

        self.bytecode.truncate(range_start - 3);
        self.re_emit_op_u16(opcode_u8(REOP_RANGE), intervals.len() as u32);
        for (start, end) in &intervals {
            self.bytecode.emit_u32(*start);
            self.bytecode.emit_u32(*end);
        }
        Ok(())
    }

    fn add_interval_intersect(
        &mut self,
        start: u32,
        end: u32,
        start1: u32,
        end1: u32,
        offset: i32,
    ) {
        let start = max(start, start1);
        let end = min(end, end1);
        if start < end {
            self.bytecode.emit_u32(start);
            self.bytecode.emit_u32(end);
            if offset != 0 {
                debug_assert!(start as i32 + offset >= 0);
                debug_assert!(end as i32 + offset >= 0);
                self.bytecode.emit_u32((start as i32 + offset) as u32);
                self.bytecode.emit_u32((end as i32 + offset) as u32);
            }
        }
    }

    fn parse_char_class(&mut self) -> Result<(), RegExpError> {
        self.buf_pos += 1;
        let mut invert = false;
        if self.byte_at(self.buf_pos) == b'^' {
            self.buf_pos += 1;
            invert = true;
        }

        self.re_emit_op_u16(opcode_u8(REOP_RANGE), 0);
        let range_start = self.bytecode.len();

        loop {
            if self.byte_at(self.buf_pos) == b']' {
                break;
            }
            let c1 = self.get_class_atom(true)?;
            if self.byte_at(self.buf_pos) == b'-' && self.byte_at(self.buf_pos + 1) != b']' {
                self.buf_pos += 1;
                if c1 >= CLASS_RANGE_BASE {
                    return Err(self.error(ERR_INVALID_CLASS_RANGE));
                }
                let c2 = self.get_class_atom(true)?;
                if c2 >= CLASS_RANGE_BASE || c2 < c1 {
                    return Err(self.error(ERR_INVALID_CLASS_RANGE));
                }
                self.bytecode.emit_u32(c1);
                self.bytecode.emit_u32(c2 + 1);
            } else if c1 >= CLASS_RANGE_BASE {
                self.emit_range_base(c1 - CLASS_RANGE_BASE);
            } else {
                let c2 = c1 + 1;
                if self.ignore_case {
                    self.add_interval_intersect(c1, c2, 0, b'A' as u32, 0);
                    self.add_interval_intersect(c1, c2, (b'Z' + 1) as u32, b'a' as u32, 0);
                    self.add_interval_intersect(c1, c2, (b'z' + 1) as u32, i32::MAX as u32, 0);
                    self.add_interval_intersect(c1, c2, b'A' as u32, (b'Z' + 1) as u32, 32);
                    self.add_interval_intersect(c1, c2, b'a' as u32, (b'z' + 1) as u32, -32);
                } else {
                    self.bytecode.emit_u32(c1);
                    self.bytecode.emit_u32(c2);
                }
            }
        }
        self.buf_pos += 1;
        self.range_optimize(range_start, invert)
    }

    fn parse_quantifier(
        &mut self,
        last_atom_start: usize,
        last_capture_count: u8,
    ) -> Result<(), RegExpError> {
        let mut last_atom_start = last_atom_start;
        let mut p = self.buf_pos;
        let c = self.byte_at(p);
        let (quant_min, quant_max) = match c {
            b'*' => {
                p += 1;
                (0, JSValue::JS_SHORTINT_MAX)
            }
            b'+' => {
                p += 1;
                (1, JSValue::JS_SHORTINT_MAX)
            }
            b'?' => {
                p += 1;
                (0, 1)
            }
            b'{' => {
                if !is_digit(self.byte_at(p + 1)) {
                    return Err(self.error(ERR_INVALID_REPETITION_COUNT));
                }
                p += 1;
                let mut tmp_pos = p;
                let min = self.parse_digits(&mut tmp_pos);
                let mut max = min;
                if self.byte_at(tmp_pos) == b',' {
                    tmp_pos += 1;
                    if is_digit(self.byte_at(tmp_pos)) {
                        max = self.parse_digits(&mut tmp_pos);
                        if max < min {
                            return Err(self.error(ERR_INVALID_REPETITION_COUNT));
                        }
                    } else {
                        max = JSValue::JS_SHORTINT_MAX;
                    }
                }
                self.buf_pos = tmp_pos;
                self.parse_expect(b'}')?;
                p = self.buf_pos;
                (min, max)
            }
            _ => return Ok(()),
        };

        let mut greedy = true;
        if self.byte_at(p) == b'?' {
            p += 1;
            greedy = false;
        }
        self.buf_pos = p;

        let (need_check_adv, need_capture_init) = {
            let start = last_atom_start;
            let end = self.bytecode.len();
            let slice = &self.bytecode.buf[start..end];
            self.need_check_adv_and_capture_init(slice)
        };

        if need_capture_init && last_capture_count != self.capture_count {
            self.bytecode.insert(last_atom_start, 3);
            self.bytecode.write_u8(last_atom_start, opcode_u8(REOP_SAVE_RESET));
            self.bytecode.write_u8(last_atom_start + 1, last_capture_count);
            self.bytecode.write_u8(last_atom_start + 2, self.capture_count - 1);
        }

        let len = self.bytecode.len() - last_atom_start;
        let greedy_offset = if greedy { 1 } else { 0 };
        if quant_min == 0 {
            if !need_capture_init && last_capture_count != self.capture_count {
                self.bytecode.insert(last_atom_start, 3);
                self.bytecode.write_u8(last_atom_start, opcode_u8(REOP_SAVE_RESET));
                self.bytecode.write_u8(last_atom_start + 1, last_capture_count);
                self.bytecode.write_u8(last_atom_start + 2, self.capture_count - 1);
                last_atom_start += 3;
            }
            if quant_max == 0 {
                self.bytecode.truncate(last_atom_start);
            } else if quant_max == 1 || quant_max == JSValue::JS_SHORTINT_MAX {
                let has_goto = quant_max == JSValue::JS_SHORTINT_MAX;
                let extra = if need_check_adv { 2 } else { 0 };
                self.bytecode.insert(last_atom_start, 5 + extra);
                self.bytecode.write_u8(
                    last_atom_start,
                    opcode_u8(RegExpOpCode::from_u16(
                        REOP_SPLIT_GOTO_FIRST.as_u16() + greedy_offset as u16,
                    )),
                );
                let jump_len = len + 5 * if has_goto { 1 } else { 0 } + extra * 2;
                self.bytecode
                    .write_u32(last_atom_start + 1, jump_len as u32);
                let pos = last_atom_start + 1 + 4;
                if need_check_adv {
                    self.bytecode.write_u8(pos, opcode_u8(REOP_SET_CHAR_POS));
                    self.bytecode.write_u8(pos + 1, 0);
                    self.re_emit_op_u8(opcode_u8(REOP_CHECK_ADVANCE), 0);
                }
                if has_goto {
                    let target = last_atom_start;
                    self.re_emit_goto(opcode_u8(REOP_GOTO), target);
                }
            } else {
                let extra = if need_check_adv { 2 } else { 0 };
                self.bytecode.insert(last_atom_start, 11 + extra);
                let mut pos = last_atom_start;
                self.bytecode.write_u8(
                    pos,
                    opcode_u8(RegExpOpCode::from_u16(
                        REOP_SPLIT_GOTO_FIRST.as_u16() + greedy_offset as u16,
                    )),
                );
                self.bytecode.write_u32(pos + 1, (6 + extra + len + 10) as u32);
                pos += 5;
                self.bytecode.write_u8(pos, opcode_u8(REOP_SET_I32));
                self.bytecode.write_u8(pos + 1, 0);
                self.bytecode.write_u32(pos + 2, quant_max as u32);
                pos += 6;
                let loop_start = pos;
                if need_check_adv {
                    self.bytecode.write_u8(pos, opcode_u8(REOP_SET_CHAR_POS));
                    self.bytecode.write_u8(pos + 1, 0);
                }
                self.re_emit_goto_u8_u32(
                    opcode_u8(RegExpOpCode::from_u16(
                        (if need_check_adv {
                            REOP_LOOP_CHECK_ADV_SPLIT_NEXT_FIRST.as_u16()
                        } else {
                            REOP_LOOP_SPLIT_NEXT_FIRST.as_u16()
                        }) - greedy_offset as u16,
                    )),
                    0,
                    quant_max as u32,
                    loop_start,
                );
            }
        } else if quant_min == 1 && quant_max == JSValue::JS_SHORTINT_MAX && !need_check_adv {
            self.re_emit_goto(
                opcode_u8(RegExpOpCode::from_u16(
                    REOP_SPLIT_NEXT_FIRST.as_u16() - greedy_offset as u16,
                )),
                last_atom_start,
            );
        } else {
            let mut need_check_adv = need_check_adv;
            if quant_min == quant_max {
                need_check_adv = false;
            }
            let extra = if need_check_adv { 2 } else { 0 };
            self.bytecode.insert(last_atom_start, 6 + extra);
            let mut pos = last_atom_start;
            self.bytecode.write_u8(pos, opcode_u8(REOP_SET_I32));
            self.bytecode.write_u8(pos + 1, 0);
            self.bytecode.write_u32(pos + 2, quant_max as u32);
            pos += 6;
            let loop_start = pos;
            if need_check_adv {
                self.bytecode.write_u8(pos, opcode_u8(REOP_SET_CHAR_POS));
                self.bytecode.write_u8(pos + 1, 0);
            }
            if quant_min == quant_max {
                self.re_emit_goto_u8(opcode_u8(REOP_LOOP), 0, loop_start);
            } else {
                self.re_emit_goto_u8_u32(
                    opcode_u8(RegExpOpCode::from_u16(
                        (if need_check_adv {
                            REOP_LOOP_CHECK_ADV_SPLIT_NEXT_FIRST.as_u16()
                        } else {
                            REOP_LOOP_SPLIT_NEXT_FIRST.as_u16()
                        }) - greedy_offset as u16,
                    )),
                    0,
                    (quant_max - quant_min) as u32,
                    loop_start,
                );
            }
        }
        Ok(())
    }

    fn step_alternative(&mut self, alt: &mut AlternativeFrame) -> Result<StepAction, RegExpError> {
        let action = match alt.state {
            AlternativeState::Init => {
                alt.last_term_start = None;
                alt.state = AlternativeState::Running;
                StepAction::None
            }
            AlternativeState::Running => {
                if self.buf_pos >= self.buf_len {
                    return Ok(StepAction::Pop);
                }
                let term_start = self.bytecode.len();
                alt.term_start = term_start;
                alt.last_atom_start = None;
                alt.last_capture_count = 0;

                match self.byte_at(self.buf_pos) {
                    b'|' | b')' => {
                        return Ok(StepAction::Pop);
                    }
                    b'^' => {
                        self.buf_pos += 1;
                        self.re_emit_op(if self.multi_line {
                            opcode_u8(REOP_LINE_START_M)
                        } else {
                            opcode_u8(REOP_LINE_START)
                        });
                    }
                    b'$' => {
                        self.buf_pos += 1;
                        self.re_emit_op(if self.multi_line {
                            opcode_u8(REOP_LINE_END_M)
                        } else {
                            opcode_u8(REOP_LINE_END)
                        });
                    }
                    b'.' => {
                        self.buf_pos += 1;
                        alt.last_atom_start = Some(self.bytecode.len());
                        alt.last_capture_count = self.capture_count;
                        self.re_emit_op(if self.dotall {
                            opcode_u8(REOP_ANY)
                        } else {
                            opcode_u8(REOP_DOT)
                        });
                    }
                    b'{' => {
                        if !self.is_unicode && !is_digit(self.byte_at(self.buf_pos + 1)) {
                            let c = self.get_class_atom(false)?;
                            self.emit_atom_char(alt, c)?;
                        } else {
                            return Err(self.error(ERR_NOTHING_TO_REPEAT));
                        }
                    }
                    b'*' | b'+' | b'?' => {
                        return Err(self.error(ERR_NOTHING_TO_REPEAT));
                    }
                    b'(' => {
                        if self.byte_at(self.buf_pos + 1) == b'?' {
                            let c = self.byte_at(self.buf_pos + 2);
                            if c == b':' {
                                self.buf_pos += 3;
                                alt.last_atom_start = Some(self.bytecode.len());
                                alt.last_capture_count = self.capture_count;
                                alt.state = AlternativeState::AfterNonCapture;
                                return Ok(StepAction::Push(Frame::Disjunction(
                                    DisjunctionFrame::new(self.bytecode.len()),
                                )));
                            } else if c == b'=' || c == b'!' {
                                let is_neg = c == b'!';
                                self.buf_pos += 3;
                                let pos = self.re_emit_op_u32(
                                    opcode_u8(RegExpOpCode::from_u16(
                                        REOP_LOOKAHEAD.as_u16() + is_neg as u16,
                                    )),
                                    0,
                                );
                                alt.last_atom_start = Some(self.bytecode.len());
                                alt.last_capture_count = self.capture_count;
                                alt.is_neg = is_neg;
                                alt.lookahead_pos = pos;
                                alt.state = AlternativeState::AfterLookahead;
                                return Ok(StepAction::Push(Frame::Disjunction(
                                    DisjunctionFrame::new(self.bytecode.len()),
                                )));
                            } else {
                                return Err(self.error(ERR_INVALID_GROUP));
                            }
                        } else {
                            self.buf_pos += 1;
                            if self.capture_count == CAPTURE_COUNT_MAX {
                                return Err(self.error(ERR_TOO_MANY_CAPTURES));
                            }
                            alt.last_atom_start = Some(self.bytecode.len());
                            alt.last_capture_count = self.capture_count;
                            alt.capture_index = self.capture_count;
                            self.capture_count += 1;
                            self.re_emit_op_u8(
                                opcode_u8(REOP_SAVE_START),
                                alt.capture_index as u32,
                            );
                            alt.state = AlternativeState::AfterCapture;
                            return Ok(StepAction::Push(Frame::Disjunction(
                                DisjunctionFrame::new(self.bytecode.len()),
                            )));
                        }
                    }
                    b'\\' => match self.byte_at(self.buf_pos + 1) {
                        b'b' | b'B' => {
                            let op = if self.byte_at(self.buf_pos + 1) == b'b' {
                                opcode_u8(REOP_WORD_BOUNDARY)
                            } else {
                                opcode_u8(REOP_NOT_WORD_BOUNDARY)
                            };
                            self.re_emit_op(op);
                            self.buf_pos += 2;
                        }
                        b'0' => {
                            self.buf_pos += 2;
                            if is_digit(self.byte_at(self.buf_pos)) {
                                return Err(self.error(ERR_INVALID_DECIMAL_ESCAPE));
                            }
                            self.emit_atom_char(alt, 0)?;
                        }
                        b'1'..=b'9' => {
                            let mut p = self.buf_pos + 1;
                            let c = self.parse_digits(&mut p);
                            self.buf_pos = p;
                            if c > CAPTURE_COUNT_MAX as i32 {
                                return Err(self.error(ERR_BACK_REFERENCE_OUT_OF_RANGE));
                            }
                            alt.last_atom_start = Some(self.bytecode.len());
                            alt.last_capture_count = self.capture_count;
                            let base = if self.ignore_case {
                                opcode_u8(REOP_BACK_REFERENCE_I)
                            } else {
                                opcode_u8(REOP_BACK_REFERENCE)
                            };
                            self.re_emit_op_u8(base, c as u32);
                        }
                        _ => {
                            let c = self.get_class_atom(false)?;
                            self.emit_atom_char(alt, c)?;
                        }
                    },
                    b'[' => {
                        alt.last_atom_start = Some(self.bytecode.len());
                        alt.last_capture_count = self.capture_count;
                        self.parse_char_class()?;
                    }
                    b']' | b'}' => {
                        if self.is_unicode {
                            return Err(self.error(ERR_SYNTAX_ERROR));
                        }
                        let c = self.get_class_atom(false)?;
                        self.emit_atom_char(alt, c)?;
                    }
                    _ => {
                        let c = self.get_class_atom(false)?;
                        self.emit_atom_char(alt, c)?;
                    }
                }

                self.finish_atom(alt)?;
                StepAction::None
            }
            AlternativeState::AfterNonCapture => {
                self.parse_expect(b')')?;
                alt.state = AlternativeState::Running;
                self.finish_atom(alt)?;
                StepAction::None
            }
            AlternativeState::AfterLookahead => {
                self.parse_expect(b')')?;
                let op = if alt.is_neg {
                    opcode_u8(REOP_NEGATIVE_LOOKAHEAD_MATCH)
                } else {
                    opcode_u8(REOP_LOOKAHEAD_MATCH)
                };
                self.re_emit_op(op);
                let patch = self.bytecode.len() - (alt.lookahead_pos + 4);
                self.bytecode.write_u32(alt.lookahead_pos, patch as u32);
                alt.state = AlternativeState::Running;
                self.finish_atom(alt)?;
                StepAction::None
            }
            AlternativeState::AfterCapture => {
                let capture_index = alt.capture_index;
                self.re_emit_op_u8(opcode_u8(REOP_SAVE_END), capture_index as u32);
                self.parse_expect(b')')?;
                alt.state = AlternativeState::Running;
                self.finish_atom(alt)?;
                StepAction::None
            }
        };
        Ok(action)
    }

    fn finish_atom(&mut self, alt: &mut AlternativeFrame) -> Result<(), RegExpError> {
        if let Some(last_atom_start) = alt.last_atom_start {
            self.parse_quantifier(last_atom_start, alt.last_capture_count)?;
        }
        if let Some(last_term_start) = alt.last_term_start {
            let term_start = alt.term_start;
            let end = self.bytecode.len();
            let n1 = re_is_char(&self.bytecode.buf, last_term_start, term_start);
            let n2 = re_is_char(&self.bytecode.buf, term_start, end);
            if n1 > 0 && n2 > 0 && n1 + n2 <= 4 {
                self.bytecode.write_u8(
                    last_term_start,
                    opcode_u8(RegExpOpCode::from_u16(
                        REOP_CHAR1.as_u16() + (n1 + n2 - 1) as u16,
                    )),
                );
                self.bytecode
                    .buf
                    .copy_within((term_start + 1)..end, term_start);
                self.bytecode.buf.truncate(end - 1);
                return Ok(());
            }
        }
        alt.last_term_start = Some(alt.term_start);
        Ok(())
    }

    fn emit_atom_char(&mut self, alt: &mut AlternativeFrame, c: u32) -> Result<(), RegExpError> {
        alt.last_atom_start = Some(self.bytecode.len());
        alt.last_capture_count = self.capture_count;
        if c >= CLASS_RANGE_BASE {
            let c = c - CLASS_RANGE_BASE;
            if c == CHAR_RANGE_SPACE || c == CHAR_RANGE_NOT_SPACE {
                let op = if c == CHAR_RANGE_NOT_SPACE {
                    opcode_u8(REOP_NOT_SPACE)
                } else {
                    opcode_u8(REOP_SPACE)
                };
                self.re_emit_op(op);
            } else {
                self.re_emit_op_u16(opcode_u8(REOP_RANGE), 0);
                let range_start = self.bytecode.len();
                self.emit_range_base(c);
                self.range_optimize(range_start, false)?;
            }
        } else if self.ignore_case && ((c >= b'A' as u32 && c <= b'Z' as u32)
            || (c >= b'a' as u32 && c <= b'z' as u32))
        {
            let mut c = c;
            if c >= b'a' as u32 {
                c -= 32;
            }
            self.re_emit_op_u8(opcode_u8(REOP_RANGE8), 2);
            self.bytecode.emit_u8(c as u8);
            self.bytecode.emit_u8((c + 1) as u8);
            self.bytecode.emit_u8((c + 32) as u8);
            self.bytecode.emit_u8((c + 33) as u8);
        } else {
            self.re_emit_char(c);
        }
        Ok(())
    }

    fn step_disjunction(&mut self, frame: &mut DisjunctionFrame) -> Result<StepAction, RegExpError> {
        let action = match frame.state {
            DisjunctionState::Init => {
                frame.start = self.bytecode.len();
                frame.state = DisjunctionState::AfterFirstAlt;
                StepAction::Push(Frame::Alternative(AlternativeFrame::new()))
            }
            DisjunctionState::AfterFirstAlt => {
                if self.byte_at(self.buf_pos) == b'|' {
                    self.buf_pos += 1;
                    let len = self.bytecode.len() - frame.start;
                    self.bytecode.insert(frame.start, 5);
                    self.bytecode
                        .write_u8(frame.start, opcode_u8(REOP_SPLIT_NEXT_FIRST));
                    self.bytecode
                        .write_u32(frame.start + 1, (len + 5) as u32);
                    let pos = self.re_emit_op_u32(opcode_u8(REOP_GOTO), 0);
                    frame.pos = pos;
                    frame.state = DisjunctionState::AfterLoopAlt;
                    StepAction::Push(Frame::Alternative(AlternativeFrame::new()))
                } else {
                    StepAction::Pop
                }
            }
            DisjunctionState::AfterLoopAlt => {
                let len = self.bytecode.len() - (frame.pos + 4);
                self.bytecode.write_u32(frame.pos, len as u32);
                frame.state = DisjunctionState::AfterFirstAlt;
                StepAction::None
            }
        };
        Ok(action)
    }

    fn parse_disjunction(&mut self) -> Result<(), RegExpError> {
        let mut frames = vec![Frame::Disjunction(DisjunctionFrame::new(self.bytecode.len()))];
        while let Some(frame) = frames.last_mut() {
            let action = match frame {
                Frame::Disjunction(disj) => self.step_disjunction(disj)?,
                Frame::Alternative(alt) => self.step_alternative(alt)?,
            };
            match action {
                StepAction::None => {}
                StepAction::Pop => {
                    frames.pop();
                }
                StepAction::Push(frame) => {
                    frames.push(frame);
                }
            }
        }
        Ok(())
    }

    fn compute_register_count(&mut self) -> Result<u8, RegExpError> {
        let mut stack_size = 0i32;
        let mut stack_size_max = 0i32;
        let mut pos = RE_HEADER_LEN;
        while pos < self.bytecode.len() {
            let opcode = self.bytecode.read_u8(pos);
            let mut len = opcode_size(opcode);
            match opcode {
                x if x == opcode_u8(REOP_SET_I32) || x == opcode_u8(REOP_SET_CHAR_POS) => {
                    self.bytecode.buf[pos + 1] = stack_size as u8;
                    stack_size += 1;
                    if stack_size > stack_size_max {
                        if stack_size > REGISTER_COUNT_MAX as i32 {
                            return Err(self.error(ERR_TOO_MANY_REGEXP_REGISTERS));
                        }
                        stack_size_max = stack_size;
                    }
                }
                x if x == opcode_u8(REOP_CHECK_ADVANCE)
                    || x == opcode_u8(REOP_LOOP)
                    || x == opcode_u8(REOP_LOOP_SPLIT_GOTO_FIRST)
                    || x == opcode_u8(REOP_LOOP_SPLIT_NEXT_FIRST) =>
                {
                    debug_assert!(stack_size > 0);
                    stack_size -= 1;
                    self.bytecode.buf[pos + 1] = stack_size as u8;
                }
                x if x == opcode_u8(REOP_LOOP_CHECK_ADV_SPLIT_GOTO_FIRST)
                    || x == opcode_u8(REOP_LOOP_CHECK_ADV_SPLIT_NEXT_FIRST) =>
                {
                    debug_assert!(stack_size >= 2);
                    stack_size -= 2;
                    self.bytecode.buf[pos + 1] = stack_size as u8;
                }
                x if x == opcode_u8(REOP_RANGE8) => {
                    let val = self.bytecode.buf[pos + 1] as usize;
                    len += val * 2;
                }
                x if x == opcode_u8(REOP_RANGE) => {
                    let val = get_u16(&self.bytecode.buf[pos + 1..pos + 3]) as usize;
                    len += val * 8;
                }
                x if x == opcode_u8(REOP_BACK_REFERENCE)
                    || x == opcode_u8(REOP_BACK_REFERENCE_I) =>
                {
                    if self.bytecode.buf[pos + 1] >= self.capture_count {
                        return Err(self.error(ERR_BACK_REFERENCE_OUT_OF_RANGE));
                    }
                }
                _ => {}
            }
            pos += len;
        }
        Ok(stack_size_max as u8)
    }
}

#[derive(Copy, Clone, Debug)]
enum Frame {
    Disjunction(DisjunctionFrame),
    Alternative(AlternativeFrame),
}

#[derive(Copy, Clone, Debug)]
enum StepAction {
    None,
    Pop,
    Push(Frame),
}

#[derive(Copy, Clone, Debug)]
struct DisjunctionFrame {
    state: DisjunctionState,
    start: usize,
    pos: usize,
}

impl DisjunctionFrame {
    fn new(start: usize) -> Self {
        Self {
            state: DisjunctionState::Init,
            start,
            pos: 0,
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum DisjunctionState {
    Init,
    AfterFirstAlt,
    AfterLoopAlt,
}

#[derive(Copy, Clone, Debug)]
struct AlternativeFrame {
    state: AlternativeState,
    last_term_start: Option<usize>,
    last_atom_start: Option<usize>,
    last_capture_count: u8,
    term_start: usize,
    capture_index: u8,
    lookahead_pos: usize,
    is_neg: bool,
}

impl AlternativeFrame {
    fn new() -> Self {
        Self {
            state: AlternativeState::Init,
            last_term_start: None,
            last_atom_start: None,
            last_capture_count: 0,
            term_start: 0,
            capture_index: 0,
            lookahead_pos: 0,
            is_neg: false,
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum AlternativeState {
    Init,
    Running,
    AfterNonCapture,
    AfterLookahead,
    AfterCapture,
}

fn opcode_u8(op: RegExpOpCode) -> u8 {
    op.as_u8()
}

fn opcode_size(opcode: u8) -> usize {
    RE_OPCODES
        .get(opcode as usize)
        .map(|info| info.size as usize)
        .unwrap_or(1)
}

fn is_digit(c: u8) -> bool {
    c.is_ascii_digit()
}

fn re_is_char(buf: &[u8], start: usize, end: usize) -> usize {
    if start >= end || start >= buf.len() {
        return 0;
    }
    let op = buf[start];
    if op < opcode_u8(REOP_CHAR1) || op > opcode_u8(REOP_CHAR4) {
        return 0;
    }
    let n = (op - opcode_u8(REOP_CHAR1) + 1) as usize;
    if end - start != n + 1 {
        return 0;
    }
    n
}

fn compress_intervals(intervals: &[(u32, u32)]) -> Vec<(u32, u32)> {
    let mut out = Vec::with_capacity(intervals.len());
    for (start, end) in intervals.iter().copied() {
        if start == end {
            continue;
        }
        match out.last_mut() {
            Some((_, last_end)) if *last_end >= start => {
                *last_end = max(*last_end, end);
            }
            _ => out.push((start, end)),
        }
    }
    out
}

const CHAR_RANGE_S_TABLE: &[u32] = &[
    0x0009,
    0x000D + 1,
    0x0020,
    0x0020 + 1,
    0x00A0,
    0x00A0 + 1,
    0x1680,
    0x1680 + 1,
    0x2000,
    0x200A + 1,
    0x2028,
    0x2029 + 1,
    0x202F,
    0x202F + 1,
    0x205F,
    0x205F + 1,
    0x3000,
    0x3000 + 1,
    0xFEFF,
    0xFEFF + 1,
];

const CHAR_RANGE_W_TABLE: &[u32] = &[
    0x0030,
    0x0039 + 1,
    0x0041,
    0x005A + 1,
    0x005F,
    0x005F + 1,
    0x0061,
    0x007A + 1,
];

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::regexp_flags::LRE_FLAG_UNICODE;

    fn opcodes_from(bytecode: &[u8]) -> Vec<u8> {
        let mut ops = Vec::new();
        let mut pos = 0usize;
        while pos < bytecode.len() {
            let op = bytecode[pos];
            ops.push(op);
            let mut len = opcode_size(op);
            if op == opcode_u8(REOP_RANGE8) {
                let n = bytecode[pos + 1] as usize;
                len += n * 2;
            } else if op == opcode_u8(REOP_RANGE) {
                let n = get_u16(&bytecode[pos + 1..pos + 3]) as usize;
                len += n * 8;
            }
            pos += len;
        }
        ops
    }

    #[test]
    fn compile_simple_literal_includes_non_sticky_prefix() {
        let compiled = compile_regexp(b"a", 0).unwrap();
        assert_eq!(compiled.flags(), 0);
        assert_eq!(compiled.capture_count(), 1);
        assert_eq!(compiled.register_count(), 0);
        let ops = opcodes_from(compiled.bytecode());
        assert_eq!(ops[0], opcode_u8(REOP_SPLIT_GOTO_FIRST));
        assert_eq!(ops[1], opcode_u8(REOP_ANY));
        assert_eq!(ops[2], opcode_u8(REOP_GOTO));
        assert_eq!(ops[3], opcode_u8(REOP_SAVE_START));
        assert_eq!(ops[4], opcode_u8(REOP_CHAR1));
        assert_eq!(ops[5], opcode_u8(REOP_SAVE_END));
        assert_eq!(ops[6], opcode_u8(REOP_MATCH));
    }

    #[test]
    fn compile_sticky_skips_prefix() {
        let compiled = compile_regexp(b"a", LRE_FLAG_STICKY).unwrap();
        let ops = opcodes_from(compiled.bytecode());
        assert_eq!(ops[0], opcode_u8(REOP_SAVE_START));
    }

    #[test]
    fn compile_capture_count_and_backref_error() {
        let compiled = compile_regexp(b"(a)\\1", 0).unwrap();
        assert_eq!(compiled.capture_count(), 2);
        let err = compile_regexp(b"\\1", 0).unwrap_err();
        assert_eq!(err.message(), ERR_BACK_REFERENCE_OUT_OF_RANGE);
    }

    #[test]
    fn compile_invalid_class_range_errors() {
        let err = compile_regexp(b"[z-a]", 0).unwrap_err();
        assert_eq!(err.message(), ERR_INVALID_CLASS_RANGE);
    }

    #[test]
    fn compile_unicode_invalid_escape_errors() {
        let err = compile_regexp(b"\\c0", LRE_FLAG_UNICODE).unwrap_err();
        assert_eq!(err.message(), ERR_INVALID_ESCAPE_IN_REGEXP);
    }

    #[test]
    fn compile_quantifier_without_atom_errors() {
        let err = compile_regexp(b"*", 0).unwrap_err();
        assert_eq!(err.message(), ERR_NOTHING_TO_REPEAT);
    }

    #[test]
    fn compile_extraneous_characters_errors() {
        let err = compile_regexp(b"a)", 0).unwrap_err();
        assert_eq!(err.message(), ERR_EXTRANEOUS_CHARACTERS);
    }
}
