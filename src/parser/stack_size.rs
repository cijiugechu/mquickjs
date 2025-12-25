use core::fmt;

use crate::cutils::{get_u16, get_u32};
use crate::opcode::{
    OpCode, OpCodeFormat, OPCODES, OP_GOSUB, OP_GOTO, OP_IF_FALSE, OP_IF_TRUE, OP_INVALID, OP_RET,
    OP_RETURN, OP_RETURN_UNDEF, OP_THROW,
};

pub const JS_MAX_FUNC_STACK_SIZE: i32 = 65535;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StackSizeErrorKind {
    InvalidOpcode,
    BufferOverflow,
    StackUnderflow,
    StackOverflow,
    InconsistentStackSize { expected: u8, actual: u8 },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StackSizeError {
    kind: StackSizeErrorKind,
    pc: u32,
}

impl StackSizeError {
    fn new(kind: StackSizeErrorKind, pc: u32) -> Self {
        Self { kind, pc }
    }

    pub fn kind(&self) -> &StackSizeErrorKind {
        &self.kind
    }

    pub fn pc(&self) -> u32 {
        self.pc
    }

    pub fn message(&self) -> String {
        match self.kind {
            StackSizeErrorKind::InvalidOpcode => {
                format!("invalid opcode (pc={})", self.pc)
            }
            StackSizeErrorKind::BufferOverflow => {
                format!("bytecode buffer overflow (pc={})", self.pc)
            }
            StackSizeErrorKind::StackUnderflow => {
                format!("stack underflow (pc={})", self.pc)
            }
            StackSizeErrorKind::StackOverflow => {
                format!("stack overflow (pc={})", self.pc)
            }
            StackSizeErrorKind::InconsistentStackSize { expected, actual } => format!(
                "unconsistent stack size: {} {} (pc={})",
                expected as i32 - 1,
                actual as i32 - 1,
                self.pc
            ),
        }
    }
}

impl fmt::Display for StackSizeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.message())
    }
}

fn short_stack_len(stack_len: i32) -> u8 {
    debug_assert!(stack_len >= 0);
    ((stack_len as u32 % 255) + 1) as u8
}

fn compute_stack_size_push(
    worklist: &mut Vec<(u32, i32)>,
    explore_tab: &mut [u8],
    byte_len: u32,
    pos: u32,
    stack_len: i32,
) -> Result<(), StackSizeError> {
    if pos >= byte_len {
        return Err(StackSizeError::new(StackSizeErrorKind::BufferOverflow, pos));
    }
    let short = short_stack_len(stack_len);
    let slot = &mut explore_tab[pos as usize];
    if *slot != 0 {
        if *slot != short {
            return Err(StackSizeError::new(
                StackSizeErrorKind::InconsistentStackSize {
                    expected: *slot,
                    actual: short,
                },
                pos,
            ));
        }
        return Ok(());
    }
    *slot = short;
    worklist.push((pos, stack_len));
    Ok(())
}

fn add_pos(base: u32, offset: u32, pc: u32) -> Result<u32, StackSizeError> {
    base.checked_add(offset)
        .ok_or_else(|| StackSizeError::new(StackSizeErrorKind::BufferOverflow, pc))
}

// C: `compute_stack_size` and `compute_stack_size_push` in mquickjs.c.
pub fn compute_stack_size(byte_code: &[u8]) -> Result<u16, StackSizeError> {
    let byte_len = byte_code.len() as u32;
    let mut explore_tab = vec![0u8; byte_code.len()];
    let mut worklist: Vec<(u32, i32)> = Vec::new();
    let mut max_stack = 0i32;

    compute_stack_size_push(&mut worklist, &mut explore_tab, byte_len, 0, 0)?;

    while let Some((mut pos, mut stack_len)) = worklist.pop() {
        let pc = pos;
        if pos >= byte_len {
            return Err(StackSizeError::new(StackSizeErrorKind::BufferOverflow, pc));
        }
        let op = byte_code[pos as usize];
        pos += 1;
        if op == OP_INVALID.0 as u8 || (op as usize) >= OPCODES.len() {
            return Err(StackSizeError::new(StackSizeErrorKind::InvalidOpcode, pc));
        }
        let info = &OPCODES[op as usize];
        let op_len = info.size as u32;
        if pos + op_len - 1 > byte_len {
            return Err(StackSizeError::new(StackSizeErrorKind::BufferOverflow, pc));
        }
        let mut n_pop = info.n_pop as i32;
        if info.fmt == OpCodeFormat::npop {
            let offset = pos as usize;
            n_pop += get_u16(&byte_code[offset..offset + 2]) as i32;
        }
        if stack_len < n_pop {
            return Err(StackSizeError::new(StackSizeErrorKind::StackUnderflow, pc));
        }
        stack_len += info.n_push as i32 - n_pop;
        if stack_len > max_stack {
            if stack_len > JS_MAX_FUNC_STACK_SIZE {
                return Err(StackSizeError::new(StackSizeErrorKind::StackOverflow, pc));
            }
            max_stack = stack_len;
        }
        let opcode = OpCode(op as u16);
        match opcode {
            OP_RETURN | OP_RETURN_UNDEF | OP_THROW | OP_RET => continue,
            OP_GOTO => {
                let offset = get_u32(&byte_code[pos as usize..pos as usize + 4]);
                pos = add_pos(pos, offset, pc)?;
            }
            OP_IF_TRUE | OP_IF_FALSE => {
                let offset = get_u32(&byte_code[pos as usize..pos as usize + 4]);
                let target = add_pos(pos, offset, pc)?;
                compute_stack_size_push(
                    &mut worklist,
                    &mut explore_tab,
                    byte_len,
                    target,
                    stack_len,
                )?;
                pos += op_len - 1;
            }
            OP_GOSUB => {
                let offset = get_u32(&byte_code[pos as usize..pos as usize + 4]);
                let target = add_pos(pos, offset, pc)?;
                compute_stack_size_push(
                    &mut worklist,
                    &mut explore_tab,
                    byte_len,
                    target,
                    stack_len + 1,
                )?;
                pos += op_len - 1;
            }
            _ => {
                pos += op_len - 1;
            }
        }
        compute_stack_size_push(&mut worklist, &mut explore_tab, byte_len, pos, stack_len)?;
    }

    Ok(max_stack as u16)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cutils::{put_u16, put_u32};
    use crate::opcode::{OP_CALL, OP_PUSH_0, OP_PUSH_1, OP_PUSH_TRUE};

    #[test]
    fn compute_stack_size_linear_sequence() {
        let byte_code = [OP_PUSH_0.0 as u8, OP_RETURN.0 as u8];
        let size = compute_stack_size(&byte_code).unwrap();
        assert_eq!(size, 1);
    }

    #[test]
    fn compute_stack_size_handles_npop_call() {
        let mut byte_code = vec![
            OP_PUSH_0.0 as u8,
            OP_PUSH_0.0 as u8,
            OP_PUSH_0.0 as u8,
            OP_CALL.0 as u8,
            0,
            0,
            OP_RETURN.0 as u8,
        ];
        put_u16(&mut byte_code[4..6], 2);
        let size = compute_stack_size(&byte_code).unwrap();
        assert_eq!(size, 3);
    }

    #[test]
    fn compute_stack_size_handles_branch_and_goto() {
        let mut byte_code = vec![
            OP_PUSH_TRUE.0 as u8,
            OP_IF_TRUE.0 as u8,
            0,
            0,
            0,
            0,
            OP_PUSH_0.0 as u8,
            OP_GOTO.0 as u8,
            0,
            0,
            0,
            0,
            OP_PUSH_1.0 as u8,
            OP_RETURN.0 as u8,
        ];
        let if_base = 2u32;
        let if_target = 12u32;
        put_u32(&mut byte_code[2..6], if_target - if_base);
        let goto_base = 8u32;
        let goto_target = 13u32;
        put_u32(&mut byte_code[8..12], goto_target - goto_base);
        let size = compute_stack_size(&byte_code).unwrap();
        assert_eq!(size, 1);
    }

    #[test]
    fn compute_stack_size_detects_inconsistent_stack() {
        let mut byte_code = vec![
            OP_PUSH_TRUE.0 as u8,
            OP_IF_TRUE.0 as u8,
            0,
            0,
            0,
            0,
            OP_PUSH_0.0 as u8,
            OP_GOTO.0 as u8,
            0,
            0,
            0,
            0,
            OP_RETURN.0 as u8,
        ];
        let if_base = 2u32;
        let target = 12u32;
        put_u32(&mut byte_code[2..6], target - if_base);
        let goto_base = 8u32;
        put_u32(&mut byte_code[8..12], target - goto_base);
        let err = compute_stack_size(&byte_code).unwrap_err();
        assert_eq!(err.pc(), 12);
        assert!(
            matches!(
                err.kind(),
                StackSizeErrorKind::InconsistentStackSize { .. }
            ),
            "unexpected error: {err:?}"
        );
    }

    #[test]
    fn compute_stack_size_reports_underflow() {
        let byte_code = [OP_RETURN.0 as u8];
        let err = compute_stack_size(&byte_code).unwrap_err();
        assert_eq!(err.pc(), 0);
        assert_eq!(err.kind(), &StackSizeErrorKind::StackUnderflow);
    }
}
