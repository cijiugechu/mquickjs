use crate::jsvalue::JSValue;
use super::parse_state::{PARSE_STATE_INIT, PARSE_STATE_RET};

pub const JS_STACK_SLACK: usize = 16;

const ERR_STACK_OVERFLOW: &str = "stack overflow";

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseStackError {
    message: &'static str,
}

impl ParseStackError {
    fn new(message: &'static str) -> Self {
        Self { message }
    }

    pub fn message(&self) -> &'static str {
        self.message
    }
}

// C: parse stack helpers from mquickjs.c (stack grows downward).
pub struct ParseStack {
    stack: Vec<JSValue>,
    sp: usize,
    stack_bottom: usize,
}

impl ParseStack {
    pub fn new(stack: Vec<JSValue>) -> Self {
        let sp = stack.len();
        Self {
            stack,
            sp,
            stack_bottom: sp,
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self::new(vec![JSValue::JS_NULL; capacity])
    }

    pub fn sp(&self) -> usize {
        self.sp
    }

    pub fn stack_bottom(&self) -> usize {
        self.stack_bottom
    }

    pub fn depth(&self) -> usize {
        self.stack.len().saturating_sub(self.sp)
    }

    fn grow(&mut self, needed: usize) -> Result<(), ParseStackError> {
        let len = self.stack.len();
        let required = needed.saturating_sub(self.sp);
        let new_len = len.saturating_mul(2).max(len + required + JS_STACK_SLACK);
        if new_len <= len {
            return Err(ParseStackError::new(ERR_STACK_OVERFLOW));
        }
        let extra = new_len - len;
        let mut new_stack = vec![JSValue::JS_NULL; new_len];
        new_stack[extra..].copy_from_slice(&self.stack);
        self.stack = new_stack;
        self.sp = self.sp.saturating_add(extra);
        self.stack_bottom = self.stack_bottom.saturating_add(extra);
        Ok(())
    }

    pub fn ensure_capacity(&mut self, len: usize) -> Result<(), ParseStackError> {
        let needed = len.saturating_add(JS_STACK_SLACK);
        if self.sp < needed {
            self.grow(needed)?;
        }
        self.stack_bottom = self.sp - needed;
        Ok(())
    }

    fn stack_alloc(&mut self, val: JSValue) -> Result<JSValue, ParseStackError> {
        self.ensure_capacity(1)?;
        Ok(val)
    }

    pub fn push(&mut self, val: JSValue) -> Result<(), ParseStackError> {
        let val = if self.sp <= self.stack_bottom {
            self.stack_alloc(val)?
        } else {
            val
        };
        debug_assert!(self.sp > 0);
        self.sp -= 1;
        self.stack[self.sp] = val;
        Ok(())
    }

    pub fn push_int(&mut self, val: i32) -> Result<(), ParseStackError> {
        self.push(JSValue::new_short_int(val))
    }

    pub fn pop(&mut self) -> JSValue {
        debug_assert!(self.sp < self.stack.len());
        let val = self.stack[self.sp];
        self.sp += 1;
        if self.sp >= JS_STACK_SLACK {
            let new_bottom = self.sp - JS_STACK_SLACK;
            if new_bottom > self.stack_bottom {
                self.stack_bottom = new_bottom;
            }
        }
        val
    }

    pub fn pop_int(&mut self) -> i32 {
        let val = self.pop();
        debug_assert!(val.is_int());
        val.get_int()
    }
}

pub type ParseFunc<S> = fn(&mut S, u8, i32) -> i32;

pub fn parse_call<S>(
    state: &mut S,
    stack: &mut ParseStack,
    func_table: &[ParseFunc<S>],
    mut func_idx: u8,
    mut param: i32,
) -> Result<(), ParseStackError> {
    let stack_depth = stack.depth();
    let mut parse_state = PARSE_STATE_INIT;
    loop {
        debug_assert!((func_idx as usize) < func_table.len());
        let ret = func_table[func_idx as usize](state, parse_state, param);
        parse_state = (ret & 0xff) as u8;
        if parse_state == PARSE_STATE_RET {
            if stack.depth() == stack_depth {
                break;
            }
            let saved = stack.pop_int();
            parse_state = (saved & 0xff) as u8;
            func_idx = ((saved >> 8) & 0xff) as u8;
            param = -1;
        } else {
            let saved = (parse_state as i32) | ((func_idx as i32) << 8);
            stack.push_int(saved)?;
            parse_state = PARSE_STATE_INIT;
            func_idx = ((ret >> 8) & 0xff) as u8;
            param = ret >> 16;
        }
    }
    Ok(())
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    #[test]
    fn parse_stack_grows_and_shrinks_bottom() {
        let mut stack = ParseStack::with_capacity(64);
        assert_eq!(stack.sp(), 64);
        assert_eq!(stack.stack_bottom(), 64);

        stack.push(JSValue::JS_NULL).unwrap();
        assert_eq!(stack.sp(), 63);
        assert_eq!(stack.stack_bottom(), 64 - (1 + JS_STACK_SLACK));

        stack.pop();
        assert_eq!(stack.sp(), 64);
        assert_eq!(stack.stack_bottom(), 64 - JS_STACK_SLACK);
    }

    #[test]
    fn parse_stack_grows_on_overflow() {
        let mut stack = ParseStack::with_capacity(8);
        for i in 0..64 {
            stack.push_int(i).unwrap();
        }
        for i in (0..64).rev() {
            assert_eq!(stack.pop_int(), i);
        }
    }

    #[derive(Default)]
    struct DummyState {
        log: Vec<&'static str>,
    }

    fn encode_call(state: u8, func_idx: u8, param: i32) -> i32 {
        (state as i32) | ((func_idx as i32) << 8) | (param << 16)
    }

    fn parse_func_a(state: &mut DummyState, parse_state: u8, param: i32) -> i32 {
        match parse_state {
            PARSE_STATE_INIT => {
                state.log.push("a:init");
                encode_call(0, 1, 123)
            }
            0 => {
                state.log.push("a:ret");
                assert_eq!(param, -1);
                PARSE_STATE_RET as i32
            }
            _ => unreachable!("unexpected state"),
        }
    }

    fn parse_func_b(state: &mut DummyState, parse_state: u8, param: i32) -> i32 {
        match parse_state {
            PARSE_STATE_INIT => {
                state.log.push("b:init");
                assert_eq!(param, 123);
                PARSE_STATE_RET as i32
            }
            _ => unreachable!("unexpected state"),
        }
    }

    #[test]
    fn parse_call_returns_to_caller() {
        let mut stack = ParseStack::with_capacity(64);
        let mut state = DummyState::default();
        let funcs: [ParseFunc<DummyState>; 2] = [parse_func_a, parse_func_b];

        parse_call(&mut state, &mut stack, &funcs, 0, -1).unwrap();
        assert_eq!(state.log, ["a:init", "b:init", "a:ret"]);
    }
}
