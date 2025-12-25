use crate::cutils::{get_u32, put_u16, put_u32};
use crate::jsvalue::{JS_SHORTINT_MAX, JS_SHORTINT_MIN, JS_TAG_INT};
use crate::opcode::{
    OpCode, OpCodeFormat, OPCODES, OP_GOTO, OP_INVALID, OP_PUSH_0, OP_PUSH_I16, OP_PUSH_I8,
    OP_PUSH_MINUS1, OP_PUSH_VALUE, OP_RET, OP_RETURN, OP_RETURN_UNDEF, OP_THROW, OP_GET_LOC,
    OP_PUT_LOC, OP_GET_ARG, OP_PUT_ARG, OP_GET_LOC0, OP_PUT_LOC0, OP_GET_ARG0, OP_PUT_ARG0,
    OP_GET_LOC8, OP_PUT_LOC8,
};

use super::pc2line::Pc2LineEmitter;
use super::types::SourcePos;

const LABEL_RESOLVED_FLAG: i32 = 1 << 29;
const LABEL_OFFSET_MASK: i32 = (1 << 29) - 1;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Label(i32);

impl Label {
    pub const fn none() -> Self {
        Self(-1)
    }

    pub const fn new() -> Self {
        Self(LABEL_OFFSET_MASK)
    }

    pub const fn is_none(self) -> bool {
        self.0 < 0
    }

    fn is_resolved(self) -> bool {
        (self.0 & LABEL_RESOLVED_FLAG) != 0
    }

    fn offset(self) -> i32 {
        self.0 & LABEL_OFFSET_MASK
    }
}

impl Default for Label {
    fn default() -> Self {
        Self::new()
    }
}

// C: bytecode emission helpers from mquickjs.c.
pub struct BytecodeEmitter<'a> {
    source: &'a [u8],
    byte_code: Vec<u8>,
    pc2line: Pc2LineEmitter,
    last_opcode_pos: Option<usize>,
    last_pc2line_pos: u32,
    last_pc2line_source_pos: SourcePos,
}

impl<'a> BytecodeEmitter<'a> {
    pub fn new(source: &'a [u8], source_pos: SourcePos, has_column: bool) -> Self {
        Self {
            source,
            byte_code: Vec::new(),
            pc2line: Pc2LineEmitter::new(source_pos, has_column),
            last_opcode_pos: None,
            last_pc2line_pos: 0,
            last_pc2line_source_pos: source_pos,
        }
    }

    pub fn byte_code(&self) -> &[u8] {
        &self.byte_code
    }

    pub fn pc2line_bytes(&self) -> &[u8] {
        self.pc2line.bytes()
    }

    pub fn pc2line_source_pos(&self) -> SourcePos {
        self.pc2line.source_pos()
    }

    pub fn emit_u8(&mut self, val: u8) {
        self.byte_code.push(val);
    }

    pub fn emit_u16(&mut self, val: u16) {
        let start = self.byte_code.len();
        self.byte_code.resize(start + 2, 0);
        put_u16(&mut self.byte_code[start..start + 2], val);
    }

    pub fn emit_u32(&mut self, val: u32) {
        let start = self.byte_code.len();
        self.byte_code.resize(start + 4, 0);
        put_u32(&mut self.byte_code[start..start + 4], val);
    }

    pub fn emit_op_pos(&mut self, op: OpCode, source_pos: SourcePos) {
        self.last_opcode_pos = Some(self.byte_code.len());
        self.last_pc2line_pos = self.pc2line.bit_len();
        self.last_pc2line_source_pos = self.pc2line.source_pos();
        self.pc2line.emit_pc2line(self.source, source_pos);
        self.emit_u8(op.0 as u8);
    }

    pub fn emit_op(&mut self, op: OpCode) {
        let source_pos = self.pc2line.source_pos();
        self.emit_op_pos(op, source_pos);
    }

    pub fn emit_op_param(&mut self, op: OpCode, param: u32, source_pos: SourcePos) {
        self.emit_op_pos(op, source_pos);
        let info = &OPCODES[op.as_usize()];
        match info.fmt {
            OpCodeFormat::npop => {
                debug_assert!(param <= u16::MAX as u32);
                self.emit_u16(param as u16);
            }
            OpCodeFormat::none => {}
            _ => unreachable!("unexpected opcode format for emit_op_param"),
        }
    }

    pub fn emit_insert(&mut self, pos: usize, len: usize) {
        let old_len = self.byte_code.len();
        self.byte_code.resize(old_len + len, 0);
        self.byte_code.copy_within(pos..old_len, pos + len);
        for slot in &mut self.byte_code[pos..pos + len] {
            *slot = 0;
        }
    }

    pub fn get_prev_opcode(&self) -> OpCode {
        match self.last_opcode_pos {
            Some(pos) => OpCode(self.byte_code[pos] as u16),
            None => OP_INVALID,
        }
    }

    pub fn is_live_code(&self) -> bool {
        !matches!(
            self.get_prev_opcode(),
            OP_RETURN | OP_RETURN_UNDEF | OP_THROW | OP_GOTO | OP_RET
        )
    }

    pub fn remove_last_op(&mut self) {
        if let Some(pos) = self.last_opcode_pos {
            self.byte_code.truncate(pos);
            self.pc2line
                .restore_state(self.last_pc2line_pos, self.last_pc2line_source_pos);
        }
        self.last_opcode_pos = None;
    }

    pub fn emit_push_short_int(&mut self, val: i32) {
        if val == -1 {
            self.emit_op(OP_PUSH_MINUS1);
            return;
        }
        if (0..=7).contains(&val) {
            self.emit_op(OpCode(OP_PUSH_0.0 + val as u16));
            return;
        }
        if val == val as i8 as i32 {
            self.emit_op(OP_PUSH_I8);
            self.emit_u8(val as u8);
            return;
        }
        if val == val as i16 as i32 {
            self.emit_op(OP_PUSH_I16);
            self.emit_u16(val as u16);
            return;
        }
        self.emit_op(OP_PUSH_VALUE);
        self.emit_u32(encode_short_int(val));
    }

    pub fn emit_var(&mut self, opcode: OpCode, var_idx: u32, source_pos: SourcePos) {
        if opcode == OP_GET_LOC {
            if var_idx < 4 {
                self.emit_op_pos(OpCode(OP_GET_LOC0.0 + var_idx as u16), source_pos);
                return;
            }
            if var_idx < 256 {
                self.emit_op_pos(OP_GET_LOC8, source_pos);
                self.emit_u8(var_idx as u8);
                return;
            }
        } else if opcode == OP_PUT_LOC {
            if var_idx < 4 {
                self.emit_op_pos(OpCode(OP_PUT_LOC0.0 + var_idx as u16), source_pos);
                return;
            }
            if var_idx < 256 {
                self.emit_op_pos(OP_PUT_LOC8, source_pos);
                self.emit_u8(var_idx as u8);
                return;
            }
        } else if opcode == OP_GET_ARG {
            if var_idx < 4 {
                self.emit_op_pos(OpCode(OP_GET_ARG0.0 + var_idx as u16), source_pos);
                return;
            }
        } else if opcode == OP_PUT_ARG && var_idx < 4 {
            self.emit_op_pos(OpCode(OP_PUT_ARG0.0 + var_idx as u16), source_pos);
            return;
        }

        self.emit_op_pos(opcode, source_pos);
        self.emit_u16(var_idx as u16);
    }

    pub fn emit_label_pos(&mut self, label: &mut Label, pos: usize) {
        debug_assert!(!label.is_resolved());
        let mut cur = label.offset();
        while cur != LABEL_OFFSET_MASK {
            let idx = cur as usize;
            let next = get_u32(&self.byte_code[idx..idx + 4]) as i32;
            let rel = (pos as i32) - cur;
            put_u32(&mut self.byte_code[idx..idx + 4], rel as u32);
            cur = next;
        }
        let resolved = (pos as i32) | LABEL_RESOLVED_FLAG;
        *label = Label(resolved);
    }

    pub fn emit_label(&mut self, label: &mut Label) {
        let pos = self.byte_code.len();
        self.emit_label_pos(label, pos);
        self.last_opcode_pos = None;
    }

    pub fn emit_goto(&mut self, opcode: OpCode, label: &mut Label) {
        debug_assert!(opcode == OP_GOTO || opcode == OP_RET);
        self.emit_op(opcode);
        let label_val = label.0;
        if (label_val & LABEL_RESOLVED_FLAG) != 0 {
            let target = label_val & LABEL_OFFSET_MASK;
            let base = self.byte_code.len() as i32;
            self.emit_u32((target - base) as u32);
        } else {
            self.emit_u32(label_val as u32);
            let pos = self.byte_code.len() as i32 - 4;
            *label = Label(pos);
        }
    }
}

fn encode_short_int(val: i32) -> u32 {
    debug_assert!((JS_SHORTINT_MIN..=JS_SHORTINT_MAX).contains(&val));
    ((val as u32) << 1) | (JS_TAG_INT as u32)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cutils::get_u16;
    use crate::opcode::{OP_CALL, OP_NOP};

    #[test]
    fn emit_goto_patches_label() {
        let source = b"abc";
        let mut emitter = BytecodeEmitter::new(source, 0, false);
        let mut label = Label::new();

        emitter.emit_goto(OP_GOTO, &mut label);
        assert_eq!(emitter.byte_code[0], OP_GOTO.0 as u8);
        assert_eq!(get_u32(&emitter.byte_code[1..5]), LABEL_OFFSET_MASK as u32);
        assert!(!label.is_resolved());

        emitter.emit_op(OP_NOP);
        emitter.emit_label(&mut label);
        assert!(label.is_resolved());
        assert_eq!(get_u32(&emitter.byte_code[1..5]), 5);

        emitter.emit_goto(OP_GOTO, &mut label);
        let start = emitter.byte_code.len() - 4;
        assert_eq!(get_u32(&emitter.byte_code[start..start + 4]), 0xffff_ffff);
    }

    #[test]
    fn emit_op_param_encodes_npop() {
        let source = b"call";
        let mut emitter = BytecodeEmitter::new(source, 0, false);
        emitter.emit_op_param(OP_CALL, 7, 0);
        assert_eq!(emitter.byte_code[0], OP_CALL.0 as u8);
        assert_eq!(get_u16(&emitter.byte_code[1..3]), 7);
    }

    #[test]
    fn remove_last_op_restores_pc2line_state() {
        let source = b"a\nb";
        let mut emitter = BytecodeEmitter::new(source, 0, false);
        emitter.emit_op_pos(OP_NOP, 0);
        let first_bits = emitter.pc2line.bit_len();
        emitter.emit_op_pos(OP_NOP, 2);
        assert!(emitter.pc2line.bit_len() > first_bits);
        emitter.remove_last_op();
        assert_eq!(emitter.byte_code.len(), 1);
        assert_eq!(emitter.pc2line.bit_len(), first_bits);
        assert_eq!(emitter.get_prev_opcode(), OP_INVALID);
    }
}
