use crate::jsvalue::{is_null, JSValue};
use crate::opcode::{
    OP_DROP, OP_GOSUB, OP_GOTO, OP_NIP, OP_RETURN, OP_RETURN_UNDEF, OP_UNDEFINED,
};

use super::emit::{BytecodeEmitter, Label};
use super::types::SourcePos;

const ERR_BREAK_OUTSIDE_LOOP: &str = "break must be inside loop or switch";
const ERR_CONTINUE_OUTSIDE_LOOP: &str = "continue must be inside loop";
const ERR_LABEL_NOT_FOUND: &str = "break/continue label not found";

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BreakEntry {
    label_name: JSValue,
    label_break: Label,
    label_cont: Label,
    label_finally: Label,
    drop_count: u32,
}

impl BreakEntry {
    pub fn new(label_name: JSValue, label_break: Label, label_cont: Label, drop_count: u32) -> Self {
        Self {
            label_name,
            label_break,
            label_cont,
            label_finally: Label::none(),
            drop_count,
        }
    }

    pub fn label_name(&self) -> JSValue {
        self.label_name
    }

    pub fn label_break(&self) -> Label {
        self.label_break
    }

    pub fn label_break_mut(&mut self) -> &mut Label {
        &mut self.label_break
    }

    pub fn label_cont(&self) -> Label {
        self.label_cont
    }

    pub fn label_cont_mut(&mut self) -> &mut Label {
        &mut self.label_cont
    }

    pub fn label_finally(&self) -> Label {
        self.label_finally
    }

    pub fn label_finally_mut(&mut self) -> &mut Label {
        &mut self.label_finally
    }

    pub fn drop_count(&self) -> u32 {
        self.drop_count
    }

    pub fn set_drop_count(&mut self, drop_count: u32) {
        self.drop_count = drop_count;
    }
}

#[derive(Debug, Default)]
pub struct BreakStack {
    entries: Vec<BreakEntry>,
}

impl BreakStack {
    pub fn new() -> Self {
        Self { entries: Vec::new() }
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn push(
        &mut self,
        label_name: JSValue,
        label_break: Label,
        label_cont: Label,
        drop_count: u32,
    ) -> usize {
        let entry = BreakEntry::new(label_name, label_break, label_cont, drop_count);
        self.entries.push(entry);
        self.entries.len() - 1
    }

    pub fn pop(&mut self) -> Option<BreakEntry> {
        self.entries.pop()
    }

    pub fn top(&self) -> Option<&BreakEntry> {
        self.entries.last()
    }

    pub fn top_mut(&mut self) -> Option<&mut BreakEntry> {
        self.entries.last_mut()
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &BreakEntry> {
        self.entries.iter()
    }

    pub fn entry_mut(&mut self, idx: usize) -> Option<&mut BreakEntry> {
        self.entries.get_mut(idx)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ControlFlowError {
    message: &'static str,
    position: usize,
}

impl ControlFlowError {
    fn new(message: &'static str, position: SourcePos) -> Self {
        Self {
            message,
            position: position as usize,
        }
    }

    pub fn message(&self) -> &'static str {
        self.message
    }

    pub fn position(&self) -> usize {
        self.position
    }
}

fn is_labelled_stmt(entry: &BreakEntry) -> bool {
    entry.label_cont().is_none() && entry.drop_count() == 0
}

pub fn emit_return(
    emitter: &mut BytecodeEmitter,
    break_stack: &mut BreakStack,
    mut has_val: bool,
    source_pos: SourcePos,
) {
    let mut drop_count: u32 = 0;
    for entry in break_stack.entries.iter_mut().rev() {
        drop_count = drop_count.saturating_add(entry.drop_count());
        if !entry.label_finally().is_none() {
            if !has_val {
                emitter.emit_op(OP_UNDEFINED);
                has_val = true;
            }
            for _ in 0..drop_count {
                emitter.emit_op(OP_NIP);
            }
            drop_count = 0;
            emitter.emit_goto(OP_GOSUB, entry.label_finally_mut());
        }
    }
    let op = if has_val { OP_RETURN } else { OP_RETURN_UNDEF };
    emitter.emit_op_pos(op, source_pos);
}

pub fn emit_break(
    emitter: &mut BytecodeEmitter,
    break_stack: &mut BreakStack,
    label_name: JSValue,
    is_cont: bool,
    source_pos: SourcePos,
) -> Result<(), ControlFlowError> {
    for entry in break_stack.entries.iter_mut().rev() {
        let is_match = (is_null(label_name) && !is_labelled_stmt(entry))
            || entry.label_name() == label_name;
        if is_match {
            let target = if is_cont {
                entry.label_cont_mut()
            } else {
                entry.label_break_mut()
            };
            if !target.is_none() {
                emitter.emit_goto(OP_GOTO, target);
                return Ok(());
            }
        }

        for _ in 0..entry.drop_count() {
            emitter.emit_op(OP_DROP);
        }
        if !entry.label_finally().is_none() {
            emitter.emit_op(OP_UNDEFINED);
            emitter.emit_goto(OP_GOSUB, entry.label_finally_mut());
            emitter.emit_op(OP_DROP);
        }
    }

    if is_null(label_name) {
        let message = if is_cont {
            ERR_CONTINUE_OUTSIDE_LOOP
        } else {
            ERR_BREAK_OUTSIDE_LOOP
        };
        return Err(ControlFlowError::new(message, source_pos));
    }
    Err(ControlFlowError::new(ERR_LABEL_NOT_FOUND, source_pos))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jsvalue::{new_short_int, JS_NULL};
    use crate::opcode::{
        OP_DROP, OP_GOSUB, OP_GOTO, OP_NIP, OP_RETURN, OP_RETURN_UNDEF, OP_UNDEFINED,
    };

    #[test]
    fn emit_return_without_finally_uses_return_undef() {
        let mut stack = BreakStack::new();
        stack.push(JS_NULL, Label::new(), Label::new(), 2);
        let source = b"ret";
        let mut emitter = BytecodeEmitter::new(source, 0, false);
        emit_return(&mut emitter, &mut stack, false, 3);
        assert_eq!(emitter.byte_code(), &[OP_RETURN_UNDEF.0 as u8]);
    }

    #[test]
    fn emit_return_with_finally_inserts_undefined_and_nip() {
        let mut stack = BreakStack::new();
        let outer = stack.push(JS_NULL, Label::new(), Label::new(), 2);
        *stack.entry_mut(outer).unwrap().label_finally_mut() = Label::new();
        stack.push(JS_NULL, Label::new(), Label::new(), 1);

        let mut emitter = BytecodeEmitter::new(b"0123456789", 0, false);
        emit_return(&mut emitter, &mut stack, false, 5);
        let code = emitter.byte_code();
        assert_eq!(code.len(), 10);
        assert_eq!(code[0], OP_UNDEFINED.0 as u8);
        assert_eq!(code[1], OP_NIP.0 as u8);
        assert_eq!(code[2], OP_NIP.0 as u8);
        assert_eq!(code[3], OP_NIP.0 as u8);
        assert_eq!(code[4], OP_GOSUB.0 as u8);
        assert_eq!(code[9], OP_RETURN.0 as u8);
    }

    #[test]
    fn emit_break_continues_after_drop() {
        let mut stack = BreakStack::new();
        stack.push(JS_NULL, Label::new(), Label::new(), 0);
        stack.push(new_short_int(7), Label::new(), Label::none(), 1);

        let mut emitter = BytecodeEmitter::new(b"x", 0, false);
        emit_break(&mut emitter, &mut stack, JS_NULL, true, 0).unwrap();
        let code = emitter.byte_code();
        assert_eq!(code.len(), 6);
        assert_eq!(code[0], OP_DROP.0 as u8);
        assert_eq!(code[1], OP_GOTO.0 as u8);
    }

    #[test]
    fn emit_break_runs_finally_before_goto() {
        let mut stack = BreakStack::new();
        stack.push(JS_NULL, Label::new(), Label::new(), 0);
        let top = stack.push(JS_NULL, Label::new(), Label::none(), 0);
        *stack.entry_mut(top).unwrap().label_finally_mut() = Label::new();

        let mut emitter = BytecodeEmitter::new(b"x", 0, false);
        emit_break(&mut emitter, &mut stack, JS_NULL, true, 0).unwrap();
        let code = emitter.byte_code();
        assert_eq!(code.len(), 12);
        assert_eq!(code[0], OP_UNDEFINED.0 as u8);
        assert_eq!(code[1], OP_GOSUB.0 as u8);
        assert_eq!(code[6], OP_DROP.0 as u8);
        assert_eq!(code[7], OP_GOTO.0 as u8);
    }

    #[test]
    fn emit_break_errors_without_target() {
        let mut stack = BreakStack::new();
        let mut emitter = BytecodeEmitter::new(b"x", 0, false);
        let err = emit_break(&mut emitter, &mut stack, JS_NULL, false, 9).unwrap_err();
        assert_eq!(err.message(), ERR_BREAK_OUTSIDE_LOOP);
        assert_eq!(err.position(), 9);
    }

    #[test]
    fn emit_break_errors_for_missing_label() {
        let mut stack = BreakStack::new();
        stack.push(new_short_int(1), Label::new(), Label::new(), 0);
        let mut emitter = BytecodeEmitter::new(b"x", 0, false);
        let err = emit_break(&mut emitter, &mut stack, new_short_int(2), false, 4).unwrap_err();
        assert_eq!(err.message(), ERR_LABEL_NOT_FOUND);
        assert_eq!(err.position(), 4);
    }
}
