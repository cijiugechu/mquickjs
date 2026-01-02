use crate::cutils::{get_u16, get_u8};
use crate::opcode::{
    OpCode, OPCODES, OP_DUP, OP_DUP2, OP_GET_ARG, OP_GET_ARG0, OP_GET_ARG1, OP_GET_ARG2,
    OP_GET_ARG3, OP_GET_ARRAY_EL, OP_GET_FIELD, OP_GET_FIELD2, OP_GET_LENGTH, OP_GET_LENGTH2,
    OP_GET_LOC, OP_GET_LOC0, OP_GET_LOC1, OP_GET_LOC2, OP_GET_LOC3, OP_GET_LOC8, OP_GET_VAR_REF,
    OP_INSERT2, OP_INSERT3, OP_INVALID, OP_PERM3, OP_PERM4, OP_PUT_ARG, OP_PUT_ARRAY_EL,
    OP_PUT_FIELD, OP_PUT_LOC, OP_PUT_VAR_REF, OP_PUT_VAR_REF_NOCHECK, OP_ROT3L, OP_SWAP,
};

use super::emit::BytecodeEmitter;
use super::types::SourcePos;

const ERR_INVALID_LVALUE: &str = "invalid lvalue";

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum PutLValue {
    KeepTop,
    NoKeepTop,
    KeepSecond,
    NoKeepBottom,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct LValue {
    opcode: OpCode,
    var_idx: i32,
    source_pos: SourcePos,
}

impl LValue {
    pub const fn new(opcode: OpCode, var_idx: i32, source_pos: SourcePos) -> Self {
        Self {
            opcode,
            var_idx,
            source_pos,
        }
    }

    pub const fn opcode(self) -> OpCode {
        self.opcode
    }

    pub const fn var_idx(self) -> i32 {
        self.var_idx
    }

    pub const fn source_pos(self) -> SourcePos {
        self.source_pos
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LValueError {
    message: &'static str,
    position: usize,
}

impl LValueError {
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

fn opcode_size(opcode: OpCode) -> Option<usize> {
    let idx = opcode.as_usize();
    if idx >= OPCODES.len() {
        return None;
    }
    Some(OPCODES[idx].size as usize)
}

fn to_u16(var_idx: i32, pos: SourcePos) -> Result<u16, LValueError> {
    if !(0..=u16::MAX as i32).contains(&var_idx) {
        return Err(LValueError::new(ERR_INVALID_LVALUE, pos as usize));
    }
    Ok(var_idx as u16)
}

fn to_u32(var_idx: i32, pos: SourcePos) -> Result<u32, LValueError> {
    if var_idx < 0 {
        return Err(LValueError::new(ERR_INVALID_LVALUE, pos as usize));
    }
    Ok(var_idx as u32)
}

pub fn get_lvalue(
    emitter: &mut BytecodeEmitter,
    keep: bool,
) -> Result<LValue, LValueError> {
    let opcode = emitter.get_prev_opcode();
    if opcode == OP_INVALID {
        return Err(LValueError::new(
            ERR_INVALID_LVALUE,
            emitter.pc2line_source_pos() as usize,
        ));
    }
    let size = opcode_size(opcode).ok_or_else(|| {
        LValueError::new(ERR_INVALID_LVALUE, emitter.pc2line_source_pos() as usize)
    })?;
    if size == 0 || emitter.byte_code().len() < size {
        return Err(LValueError::new(
            ERR_INVALID_LVALUE,
            emitter.pc2line_source_pos() as usize,
        ));
    }
    let source_pos = emitter.pc2line_source_pos();
    let byte_code = emitter.byte_code();
    let op_pos = byte_code.len() - size;

    let (opcode, var_idx) = match opcode {
        OP_GET_LOC0 | OP_GET_LOC1 | OP_GET_LOC2 | OP_GET_LOC3 => {
            let idx = (opcode.as_u16() - OP_GET_LOC0.as_u16()) as i32;
            (OP_GET_LOC, idx)
        }
        OP_GET_ARG0 | OP_GET_ARG1 | OP_GET_ARG2 | OP_GET_ARG3 => {
            let idx = (opcode.as_u16() - OP_GET_ARG0.as_u16()) as i32;
            (OP_GET_ARG, idx)
        }
        OP_GET_LOC8 => {
            let idx = get_u8(&byte_code[op_pos + 1..]) as i32;
            (OP_GET_LOC, idx)
        }
        OP_GET_LOC | OP_GET_ARG | OP_GET_VAR_REF | OP_GET_FIELD => {
            let idx = get_u16(&byte_code[op_pos + 1..]) as i32;
            (opcode, idx)
        }
        OP_GET_ARRAY_EL | OP_GET_LENGTH => (opcode, -1),
        _ => {
            return Err(LValueError::new(
                ERR_INVALID_LVALUE,
                source_pos as usize,
            ))
        }
    };

    emitter.remove_last_op();

    if keep {
        match opcode {
            OP_GET_LOC | OP_GET_ARG | OP_GET_VAR_REF => {
                let idx = to_u32(var_idx, source_pos)?;
                emitter.emit_var(opcode, idx, source_pos);
            }
            OP_GET_FIELD => {
                let idx = to_u16(var_idx, source_pos)?;
                emitter.emit_op_pos(OP_GET_FIELD2, source_pos);
                emitter.emit_u16(idx);
            }
            OP_GET_LENGTH => {
                emitter.emit_op_pos(OP_GET_LENGTH2, source_pos);
            }
            OP_GET_ARRAY_EL => {
                emitter.emit_op(OP_DUP2);
                emitter.emit_op_pos(OP_GET_ARRAY_EL, source_pos);
            }
            _ => {
                return Err(LValueError::new(
                    ERR_INVALID_LVALUE,
                    source_pos as usize,
                ))
            }
        }
    }

    Ok(LValue::new(opcode, var_idx, source_pos))
}

pub fn put_lvalue(
    emitter: &mut BytecodeEmitter,
    lvalue: LValue,
    special: PutLValue,
    is_repl: bool,
    length_atom_index: u16,
) -> Result<(), LValueError> {
    let opcode = lvalue.opcode();
    let source_pos = lvalue.source_pos();
    match opcode {
        OP_GET_LOC | OP_GET_ARG | OP_GET_VAR_REF => {
            if matches!(special, PutLValue::KeepTop) {
                emitter.emit_op(OP_DUP);
            }
            let next_opcode = match opcode {
                OP_GET_LOC => OP_PUT_LOC,
                OP_GET_ARG => OP_PUT_ARG,
                OP_GET_VAR_REF => {
                    if is_repl {
                        OP_PUT_VAR_REF_NOCHECK
                    } else {
                        OP_PUT_VAR_REF
                    }
                }
                _ => {
                    return Err(LValueError::new(
                        ERR_INVALID_LVALUE,
                        source_pos as usize,
                    ))
                }
            };
            let idx = to_u32(lvalue.var_idx(), source_pos)?;
            emitter.emit_var(next_opcode, idx, source_pos);
        }
        OP_GET_FIELD | OP_GET_LENGTH => {
            match special {
                PutLValue::KeepTop => emitter.emit_op(OP_INSERT2),
                PutLValue::NoKeepTop => {}
                PutLValue::NoKeepBottom => emitter.emit_op(OP_SWAP),
                PutLValue::KeepSecond => emitter.emit_op(OP_PERM3),
            }
            emitter.emit_op_pos(OP_PUT_FIELD, source_pos);
            let idx = if opcode == OP_GET_LENGTH {
                length_atom_index
            } else {
                to_u16(lvalue.var_idx(), source_pos)?
            };
            emitter.emit_u16(idx);
        }
        OP_GET_ARRAY_EL => {
            match special {
                PutLValue::KeepTop => emitter.emit_op(OP_INSERT3),
                PutLValue::NoKeepTop => {}
                PutLValue::NoKeepBottom => emitter.emit_op(OP_ROT3L),
                PutLValue::KeepSecond => emitter.emit_op(OP_PERM4),
            }
            emitter.emit_op_pos(OP_PUT_ARRAY_EL, source_pos);
        }
        _ => {
            return Err(LValueError::new(
                ERR_INVALID_LVALUE,
                source_pos as usize,
            ))
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cutils::get_u16;
    use crate::opcode::{OP_ADD, OP_DUP2, OP_GET_ARRAY_EL, OP_GET_LENGTH, OP_GET_LOC0, OP_GET_LOC8, OP_GET_VAR_REF};

    #[test]
    fn get_lvalue_loc0_without_keep() {
        let source = b"0123456789";
        let mut emitter = BytecodeEmitter::new(source, 0, false);
        emitter.emit_op_pos(OP_GET_LOC0, 4);
        let lvalue = get_lvalue(&mut emitter, false).unwrap();
        assert_eq!(lvalue.opcode(), OP_GET_LOC);
        assert_eq!(lvalue.var_idx(), 0);
        assert_eq!(lvalue.source_pos(), 4);
        assert!(emitter.byte_code().is_empty());
    }

    #[test]
    fn get_lvalue_loc8_with_keep() {
        let source = b"0123456789";
        let mut emitter = BytecodeEmitter::new(source, 0, false);
        emitter.emit_op_pos(OP_GET_LOC8, 2);
        emitter.emit_u8(200);
        let lvalue = get_lvalue(&mut emitter, true).unwrap();
        assert_eq!(lvalue.opcode(), OP_GET_LOC);
        assert_eq!(lvalue.var_idx(), 200);
        assert_eq!(emitter.byte_code()[0], OP_GET_LOC8.as_u8());
        assert_eq!(emitter.byte_code()[1], 200u8);
    }

    #[test]
    fn get_lvalue_field_with_keep() {
        let source = b"0123456789";
        let mut emitter = BytecodeEmitter::new(source, 0, false);
        emitter.emit_op_pos(OP_GET_FIELD, 1);
        emitter.emit_u16(0x1234);
        let lvalue = get_lvalue(&mut emitter, true).unwrap();
        assert_eq!(lvalue.opcode(), OP_GET_FIELD);
        assert_eq!(lvalue.var_idx(), 0x1234);
        assert_eq!(emitter.byte_code()[0], OP_GET_FIELD2.as_u8());
        assert_eq!(get_u16(&emitter.byte_code()[1..3]), 0x1234);
    }

    #[test]
    fn get_lvalue_array_el_with_keep() {
        let source = b"0123456789";
        let mut emitter = BytecodeEmitter::new(source, 0, false);
        emitter.emit_op_pos(OP_GET_ARRAY_EL, 3);
        let lvalue = get_lvalue(&mut emitter, true).unwrap();
        assert_eq!(lvalue.opcode(), OP_GET_ARRAY_EL);
        assert_eq!(lvalue.var_idx(), -1);
        assert_eq!(emitter.byte_code()[0], OP_DUP2.as_u8());
        assert_eq!(emitter.byte_code()[1], OP_GET_ARRAY_EL.as_u8());
    }

    #[test]
    fn get_lvalue_rejects_invalid_op() {
        let source = b"0123456789";
        let mut emitter = BytecodeEmitter::new(source, 0, false);
        emitter.emit_op_pos(OP_ADD, 5);
        let err = get_lvalue(&mut emitter, false).unwrap_err();
        assert_eq!(err.message(), ERR_INVALID_LVALUE);
        assert_eq!(err.position(), 5);
    }

    #[test]
    fn put_lvalue_local_keep_top() {
        let source = b"0123456789";
        let mut emitter = BytecodeEmitter::new(source, 0, false);
        let lvalue = LValue::new(OP_GET_LOC, 2, 1);
        put_lvalue(&mut emitter, lvalue, PutLValue::KeepTop, false, 0).unwrap();
        assert_eq!(emitter.byte_code()[0], OP_DUP.as_u8());
        assert_eq!(emitter.byte_code()[1], crate::opcode::OP_PUT_LOC2.as_u8());
    }

    #[test]
    fn put_lvalue_var_ref_repl_uses_nocheck() {
        let source = b"0123456789";
        let mut emitter = BytecodeEmitter::new(source, 0, false);
        let lvalue = LValue::new(OP_GET_VAR_REF, 7, 2);
        put_lvalue(&mut emitter, lvalue, PutLValue::KeepTop, true, 0).unwrap();
        assert_eq!(emitter.byte_code()[0], OP_DUP.as_u8());
        assert_eq!(emitter.byte_code()[1], OP_PUT_VAR_REF_NOCHECK.as_u8());
        assert_eq!(get_u16(&emitter.byte_code()[2..4]), 7);
    }

    #[test]
    fn put_lvalue_field_keep_second() {
        let source = b"0123456789";
        let mut emitter = BytecodeEmitter::new(source, 0, false);
        let lvalue = LValue::new(OP_GET_FIELD, 0x2222, 3);
        put_lvalue(&mut emitter, lvalue, PutLValue::KeepSecond, false, 0).unwrap();
        assert_eq!(emitter.byte_code()[0], OP_PERM3.as_u8());
        assert_eq!(emitter.byte_code()[1], OP_PUT_FIELD.as_u8());
        assert_eq!(get_u16(&emitter.byte_code()[2..4]), 0x2222);
    }

    #[test]
    fn put_lvalue_length_uses_length_atom_index() {
        let source = b"0123456789";
        let mut emitter = BytecodeEmitter::new(source, 0, false);
        let lvalue = LValue::new(OP_GET_LENGTH, -1, 4);
        put_lvalue(&mut emitter, lvalue, PutLValue::NoKeepTop, false, 0x55aa).unwrap();
        assert_eq!(emitter.byte_code()[0], OP_PUT_FIELD.as_u8());
        assert_eq!(get_u16(&emitter.byte_code()[1..3]), 0x55aa);
    }

    #[test]
    fn put_lvalue_array_el_no_keep_bottom() {
        let source = b"0123456789";
        let mut emitter = BytecodeEmitter::new(source, 0, false);
        let lvalue = LValue::new(OP_GET_ARRAY_EL, -1, 6);
        put_lvalue(
            &mut emitter,
            lvalue,
            PutLValue::NoKeepBottom,
            false,
            0,
        )
        .unwrap();
        assert_eq!(emitter.byte_code()[0], OP_ROT3L.as_u8());
        assert_eq!(emitter.byte_code()[1], OP_PUT_ARRAY_EL.as_u8());
    }

    #[test]
    fn put_lvalue_rejects_invalid_op() {
        let source = b"0123456789";
        let mut emitter = BytecodeEmitter::new(source, 0, false);
        let lvalue = LValue::new(OP_ADD, 0, 1);
        let err = put_lvalue(&mut emitter, lvalue, PutLValue::NoKeepTop, false, 0).unwrap_err();
        assert_eq!(err.message(), ERR_INVALID_LVALUE);
        assert_eq!(err.position(), 1);
    }
}
