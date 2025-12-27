use crate::context::{ContextError, JSContext};
use crate::containers::{ByteArrayHeader, ValueArrayHeader};
use crate::enums::JSObjectClass;
use crate::function_bytecode::FunctionBytecode;
use crate::heap::JS_STACK_SLACK;
use crate::js_libm::js_fmod;
use crate::jsvalue::{
    from_bits, is_bool, is_int, is_null, is_ptr, is_short_float, is_undefined, new_bool,
    new_short_int, short_float_to_f64, value_get_int, value_get_special_value, value_to_ptr,
    JSValue, JSWord, JSW, JS_NULL, JS_UNDEFINED,
};
use crate::memblock::{MbHeader, MTag};
use crate::object::{Object, ObjectHeader};
use core::mem::size_of;
use core::ptr::{self, NonNull};

const FRAME_OFFSET_ARG0: isize = 4;
const FRAME_OFFSET_FUNC_OBJ: isize = 3;
const FRAME_OFFSET_THIS_OBJ: isize = 2;
const FRAME_OFFSET_CALL_FLAGS: isize = 1;
const FRAME_OFFSET_SAVED_FP: isize = 0;
const FRAME_OFFSET_VAR0: isize = -3;

#[derive(Debug)]
pub enum InterpreterError {
    Context(ContextError),
    InvalidBytecode(&'static str),
    InvalidValue(&'static str),
    NotAFunction,
    StackOverflow,
    TypeError(&'static str),
    UnsupportedOpcode(u8),
}

impl From<ContextError> for InterpreterError {
    fn from(err: ContextError) -> Self {
        InterpreterError::Context(err)
    }
}

struct ByteArrayRaw {
    buf: NonNull<u8>,
    len: usize,
}

impl ByteArrayRaw {
    unsafe fn from_value(val: JSValue) -> Result<Self, InterpreterError> {
        let ptr = value_to_ptr::<u8>(val).ok_or(InterpreterError::InvalidValue("byte array ptr"))?;
        let header_word = unsafe {
            // SAFETY: ptr points at a readable memblock header.
            ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
        };
        let header = MbHeader::from_word(header_word);
        if header.tag() != MTag::ByteArray {
            return Err(InterpreterError::InvalidValue("byte array tag"));
        }
        let size = ByteArrayHeader::from(header).size() as usize;
        let buf = unsafe {
            // SAFETY: payload follows the byte array header.
            NonNull::new_unchecked(ptr.as_ptr().add(size_of::<JSWord>()))
        };
        Ok(Self { buf, len: size })
    }

    fn buf(&self) -> NonNull<u8> {
        self.buf
    }

    fn len(&self) -> usize {
        self.len
    }
}

struct ValueArrayRaw {
    arr: NonNull<JSValue>,
    len: usize,
}

impl ValueArrayRaw {
    unsafe fn from_value(val: JSValue) -> Result<Self, InterpreterError> {
        let ptr = value_to_ptr::<u8>(val).ok_or(InterpreterError::InvalidValue("value array ptr"))?;
        let header_word = unsafe {
            // SAFETY: ptr points at a readable memblock header.
            ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
        };
        let header = MbHeader::from_word(header_word);
        if header.tag() != MTag::ValueArray {
            return Err(InterpreterError::InvalidValue("value array tag"));
        }
        let size = ValueArrayHeader::from(header).size() as usize;
        let arr = unsafe {
            // SAFETY: payload follows the value array header.
            NonNull::new_unchecked(ptr.as_ptr().add(size_of::<JSWord>()) as *mut JSValue)
        };
        Ok(Self { arr, len: size })
    }

    unsafe fn read(&self, index: usize) -> Result<JSValue, InterpreterError> {
        if index >= self.len {
            return Err(InterpreterError::InvalidBytecode("const index out of bounds"));
        }
        unsafe {
            // SAFETY: index is within the value array bounds.
            Ok(ptr::read_unaligned(self.arr.as_ptr().add(index)))
        }
    }
}

fn object_ptr(val: JSValue) -> Result<NonNull<Object>, InterpreterError> {
    let ptr = value_to_ptr::<Object>(val).ok_or(InterpreterError::InvalidValue("object ptr"))?;
    let header_word = unsafe {
        // SAFETY: ptr points at a readable object header word.
        ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
    };
    let header = MbHeader::from_word(header_word);
    if header.tag() != MTag::Object {
        return Err(InterpreterError::InvalidValue("object tag"));
    }
    Ok(ptr)
}

fn function_bytecode_ptr(val: JSValue) -> Result<NonNull<FunctionBytecode>, InterpreterError> {
    let ptr = value_to_ptr::<FunctionBytecode>(val)
        .ok_or(InterpreterError::InvalidValue("function bytecode ptr"))?;
    let header_word = unsafe {
        // SAFETY: ptr points at a readable function bytecode header word.
        ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
    };
    let header = MbHeader::from_word(header_word);
    if header.tag() != MTag::FunctionBytecode {
        return Err(InterpreterError::InvalidValue("function bytecode tag"));
    }
    Ok(ptr)
}

fn value_to_f64(val: JSValue) -> Result<f64, InterpreterError> {
    if is_int(val) {
        return Ok(f64::from(value_get_int(val)));
    }
    #[cfg(target_pointer_width = "64")]
    {
        if is_short_float(val) {
            return Ok(short_float_to_f64(val));
        }
    }
    if is_ptr(val) {
        let ptr = value_to_ptr::<u8>(val).ok_or(InterpreterError::TypeError("float ptr"))?;
        let header_word = unsafe {
            // SAFETY: ptr points at a readable memblock header.
            ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
        };
        let header = MbHeader::from_word(header_word);
        if header.tag() != MTag::Float64 {
            return Err(InterpreterError::TypeError("float tag"));
        }
        let payload = unsafe {
            // SAFETY: payload follows the float64 header.
            ptr::read_unaligned(ptr.as_ptr().add(size_of::<JSWord>()) as *const f64)
        };
        return Ok(payload);
    }
    Err(InterpreterError::TypeError("number"))
}

fn to_bool(val: JSValue) -> Result<bool, InterpreterError> {
    if is_bool(val) {
        return Ok(value_get_special_value(val) != 0);
    }
    if is_null(val) || is_undefined(val) {
        return Ok(false);
    }
    if is_int(val) {
        return Ok(value_get_int(val) != 0);
    }
    if let Ok(num) = value_to_f64(val) {
        return Ok(num != 0.0 && !num.is_nan());
    }
    Ok(true)
}

fn encode_stack_ptr(stack_top: *mut JSValue, sp: *mut JSValue) -> Result<JSValue, InterpreterError> {
    let offset = (stack_top as isize - sp as isize) / (JSW as isize);
    let offset = i32::try_from(offset).map_err(|_| InterpreterError::StackOverflow)?;
    Ok(new_short_int(offset))
}

unsafe fn push(ctx: &mut JSContext, sp: &mut *mut JSValue, val: JSValue) {
    *sp = sp.wrapping_sub(1);
    unsafe {
        // SAFETY: caller ensures `sp` points at writable stack storage.
        ptr::write_unaligned(*sp, val);
    }
    let sp_ptr = NonNull::new(*sp).expect("stack pointer");
    ctx.set_sp(sp_ptr);
}

unsafe fn pop(ctx: &mut JSContext, sp: &mut *mut JSValue) -> JSValue {
    let val = unsafe {
        // SAFETY: caller ensures `sp` points at readable stack storage.
        ptr::read_unaligned(*sp)
    };
    *sp = sp.wrapping_add(1);
    let sp_ptr = NonNull::new(*sp).expect("stack pointer");
    ctx.set_sp(sp_ptr);
    val
}

fn binary_float_op<F>(
    ctx: &mut JSContext,
    left: JSValue,
    right: JSValue,
    op: F,
) -> Result<JSValue, InterpreterError>
where
    F: FnOnce(f64, f64) -> f64,
{
    let lhs = value_to_f64(left)?;
    let rhs = value_to_f64(right)?;
    Ok(ctx.new_float64(op(lhs, rhs))?)
}

pub fn call(ctx: &mut JSContext, func: JSValue, args: &[JSValue]) -> Result<JSValue, InterpreterError> {
    let this_obj = ctx.global_obj();
    call_with_this(ctx, func, this_obj, args)
}

pub fn call_with_this(
    ctx: &mut JSContext,
    func: JSValue,
    this_obj: JSValue,
    args: &[JSValue],
) -> Result<JSValue, InterpreterError> {
    let obj_ptr = object_ptr(func)?;
    let header_word = unsafe {
        // SAFETY: obj_ptr points at a readable object header word.
        ptr::read_unaligned(obj_ptr.as_ptr().cast::<JSWord>())
    };
    let header = ObjectHeader::from_word(header_word);
    if header.class_id() != JSObjectClass::Closure as u8 {
        return Err(InterpreterError::NotAFunction);
    }
    let func_bytecode = unsafe {
        // SAFETY: obj_ptr points at a valid closure payload.
        let payload = Object::payload_ptr(obj_ptr.as_ptr());
        let closure = core::ptr::addr_of_mut!((*payload).closure);
        ptr::read_unaligned(crate::closure_data::ClosureData::func_bytecode_ptr(closure))
    };
    let func_ptr = function_bytecode_ptr(func_bytecode)?;
    let b = unsafe {
        // SAFETY: func_ptr points at a readable function bytecode.
        func_ptr.as_ref()
    };
    let byte_code_val = b.byte_code();
    if byte_code_val == JS_NULL {
        return Err(InterpreterError::InvalidBytecode("missing bytecode"));
    }
    let byte_code = unsafe { ByteArrayRaw::from_value(byte_code_val)? };
    let mut pc = 0usize;
    let byte_slice = unsafe {
        // SAFETY: byte_code.buf is valid for byte_code.len bytes.
        core::slice::from_raw_parts(byte_code.buf().as_ptr(), byte_code.len())
    };
    let prev_sp = ctx.sp();
    let prev_fp = ctx.fp();

    let arg_count = b.arg_count() as usize;
    let max_args = args.len().max(arg_count);
    let n_vars = if b.vars() != JS_NULL {
        let vars = unsafe { ValueArrayRaw::from_value(b.vars())? };
        vars.len.saturating_sub(arg_count)
    } else {
        0
    };
    let slots_below = n_vars + 2;
    let slots_above = FRAME_OFFSET_ARG0 as usize + max_args;
    let total_slots = slots_below + slots_above;
    let stack_top = ctx.stack_top().as_ptr() as *mut JSValue;
    let new_sp = unsafe { stack_top.sub(total_slots) };
    let heap_limit = ctx.heap_free_ptr().as_ptr() as usize
        + (JS_STACK_SLACK as usize * JSW as usize);
    if (new_sp as usize) < heap_limit {
        return Err(InterpreterError::StackOverflow);
    }
    let mut sp = new_sp;
    let fp = unsafe { sp.add(slots_below) };
    let saved_fp = encode_stack_ptr(stack_top, prev_fp.as_ptr())?;

    unsafe {
        // SAFETY: new_sp..stack_top covers total_slots writable slots.
        for idx in 0..n_vars {
            ptr::write_unaligned(sp.add(idx), JS_UNDEFINED);
        }
        ptr::write_unaligned(sp.add(n_vars), JS_NULL);
        ptr::write_unaligned(sp.add(n_vars + 1), new_short_int(0));
        ptr::write_unaligned(fp.offset(FRAME_OFFSET_SAVED_FP), saved_fp);
        ptr::write_unaligned(fp.offset(FRAME_OFFSET_CALL_FLAGS), new_short_int(args.len() as i32));
        ptr::write_unaligned(fp.offset(FRAME_OFFSET_THIS_OBJ), this_obj);
        ptr::write_unaligned(fp.offset(FRAME_OFFSET_FUNC_OBJ), func);
        for idx in 0..max_args {
            let arg = args.get(idx).copied().unwrap_or(JS_UNDEFINED);
            ptr::write_unaligned(fp.add(FRAME_OFFSET_ARG0 as usize + idx), arg);
        }
    }

    ctx.set_sp(NonNull::new(sp).expect("stack pointer"));
    ctx.set_fp(NonNull::new(fp).expect("frame pointer"));

    macro_rules! try_or_break {
        ($expr:expr) => {
            match $expr {
                Ok(val) => val,
                Err(err) => break Err(err.into()),
            }
        };
    }

    let result = loop {
        if pc >= byte_slice.len() {
            break Err(InterpreterError::InvalidBytecode("pc out of bounds"));
        }
        let opcode = byte_slice[pc];
        pc += 1;
        match opcode {
            op if op == crate::opcode::OP_RETURN.0 as u8 => unsafe {
                let val = ptr::read_unaligned(sp);
                break Ok(val);
            },
            op if op == crate::opcode::OP_RETURN_UNDEF.0 as u8 => break Ok(JS_UNDEFINED),
            op if op == crate::opcode::OP_DROP.0 as u8 => unsafe {
                let _ = pop(ctx, &mut sp);
            },
            op if op == crate::opcode::OP_DUP.0 as u8 => unsafe {
                let val = ptr::read_unaligned(sp);
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_SWAP.0 as u8 => unsafe {
                let a = ptr::read_unaligned(sp);
                let b = ptr::read_unaligned(sp.add(1));
                ptr::write_unaligned(sp, b);
                ptr::write_unaligned(sp.add(1), a);
            },
            op if op == crate::opcode::OP_PUSH_MINUS1.0 as u8 => unsafe {
                push(ctx, &mut sp, new_short_int(-1));
            },
            op if op == crate::opcode::OP_PUSH_0.0 as u8 => unsafe {
                push(ctx, &mut sp, new_short_int(0));
            },
            op if op == crate::opcode::OP_PUSH_1.0 as u8 => unsafe {
                push(ctx, &mut sp, new_short_int(1));
            },
            op if op == crate::opcode::OP_PUSH_2.0 as u8 => unsafe {
                push(ctx, &mut sp, new_short_int(2));
            },
            op if op == crate::opcode::OP_PUSH_3.0 as u8 => unsafe {
                push(ctx, &mut sp, new_short_int(3));
            },
            op if op == crate::opcode::OP_PUSH_4.0 as u8 => unsafe {
                push(ctx, &mut sp, new_short_int(4));
            },
            op if op == crate::opcode::OP_PUSH_5.0 as u8 => unsafe {
                push(ctx, &mut sp, new_short_int(5));
            },
            op if op == crate::opcode::OP_PUSH_6.0 as u8 => unsafe {
                push(ctx, &mut sp, new_short_int(6));
            },
            op if op == crate::opcode::OP_PUSH_7.0 as u8 => unsafe {
                push(ctx, &mut sp, new_short_int(7));
            },
            op if op == crate::opcode::OP_PUSH_I8.0 as u8 => unsafe {
                let imm = match byte_slice.get(pc).copied() {
                    Some(val) => val as i8,
                    None => break Err(InterpreterError::InvalidBytecode("push_i8")),
                };
                pc += 1;
                push(ctx, &mut sp, new_short_int(i32::from(imm)));
            },
            op if op == crate::opcode::OP_PUSH_I16.0 as u8 => unsafe {
                if pc + 1 >= byte_slice.len() {
                    break Err(InterpreterError::InvalidBytecode("push_i16"));
                }
                let imm = i16::from_le_bytes([byte_slice[pc], byte_slice[pc + 1]]);
                pc += 2;
                push(ctx, &mut sp, new_short_int(i32::from(imm)));
            },
            op if op == crate::opcode::OP_PUSH_CONST.0 as u8 => unsafe {
                if pc + 1 >= byte_slice.len() {
                    break Err(InterpreterError::InvalidBytecode("push_const"));
                }
                let idx = u16::from_le_bytes([byte_slice[pc], byte_slice[pc + 1]]) as usize;
                pc += 2;
                let cpool = try_or_break!(ValueArrayRaw::from_value(b.cpool()));
                let val = try_or_break!(cpool.read(idx));
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_PUSH_CONST8.0 as u8 => unsafe {
                let idx = match byte_slice.get(pc).copied() {
                    Some(val) => val as usize,
                    None => break Err(InterpreterError::InvalidBytecode("push_const8")),
                };
                pc += 1;
                let cpool = try_or_break!(ValueArrayRaw::from_value(b.cpool()));
                let val = try_or_break!(cpool.read(idx));
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_PUSH_VALUE.0 as u8 => unsafe {
                if pc + 3 >= byte_slice.len() {
                    break Err(InterpreterError::InvalidBytecode("push_value"));
                }
                let raw = u32::from_le_bytes([
                    byte_slice[pc],
                    byte_slice[pc + 1],
                    byte_slice[pc + 2],
                    byte_slice[pc + 3],
                ]);
                pc += 4;
                push(ctx, &mut sp, from_bits(raw as JSWord));
            },
            op if op == crate::opcode::OP_UNDEFINED.0 as u8 => unsafe {
                push(ctx, &mut sp, JS_UNDEFINED);
            },
            op if op == crate::opcode::OP_NULL.0 as u8 => unsafe {
                push(ctx, &mut sp, JS_NULL);
            },
            op if op == crate::opcode::OP_PUSH_FALSE.0 as u8 => unsafe {
                push(ctx, &mut sp, new_bool(0));
            },
            op if op == crate::opcode::OP_PUSH_TRUE.0 as u8 => unsafe {
                push(ctx, &mut sp, new_bool(1));
            },
            op if op == crate::opcode::OP_GET_LOC.0 as u8 => unsafe {
                if pc + 1 >= byte_slice.len() {
                    break Err(InterpreterError::InvalidBytecode("get_loc"));
                }
                let idx = u16::from_le_bytes([byte_slice[pc], byte_slice[pc + 1]]) as isize;
                pc += 2;
                let val = ptr::read_unaligned(fp.offset(FRAME_OFFSET_VAR0 - idx));
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_PUT_LOC.0 as u8 => unsafe {
                if pc + 1 >= byte_slice.len() {
                    break Err(InterpreterError::InvalidBytecode("put_loc"));
                }
                let idx = u16::from_le_bytes([byte_slice[pc], byte_slice[pc + 1]]) as isize;
                pc += 2;
                let val = pop(ctx, &mut sp);
                ptr::write_unaligned(fp.offset(FRAME_OFFSET_VAR0 - idx), val);
            },
            op if op == crate::opcode::OP_ADD.0 as u8 => unsafe {
                let rhs = pop(ctx, &mut sp);
                let lhs = pop(ctx, &mut sp);
                let res = try_or_break!(binary_float_op(ctx, lhs, rhs, |a, b| a + b));
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_SUB.0 as u8 => unsafe {
                let rhs = pop(ctx, &mut sp);
                let lhs = pop(ctx, &mut sp);
                let res = try_or_break!(binary_float_op(ctx, lhs, rhs, |a, b| a - b));
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_MUL.0 as u8 => unsafe {
                let rhs = pop(ctx, &mut sp);
                let lhs = pop(ctx, &mut sp);
                let res = try_or_break!(binary_float_op(ctx, lhs, rhs, |a, b| a * b));
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_DIV.0 as u8 => unsafe {
                let rhs = pop(ctx, &mut sp);
                let lhs = pop(ctx, &mut sp);
                let res = try_or_break!(binary_float_op(ctx, lhs, rhs, |a, b| a / b));
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_MOD.0 as u8 => unsafe {
                let rhs = pop(ctx, &mut sp);
                let lhs = pop(ctx, &mut sp);
                let res = try_or_break!(binary_float_op(ctx, lhs, rhs, js_fmod));
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_NEG.0 as u8 => unsafe {
                let val = pop(ctx, &mut sp);
                let num = try_or_break!(value_to_f64(val));
                let res = try_or_break!(ctx.new_float64(-num));
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_PLUS.0 as u8 => unsafe {
                let val = pop(ctx, &mut sp);
                let num = try_or_break!(value_to_f64(val));
                let res = try_or_break!(ctx.new_float64(num));
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_LNOT.0 as u8 => unsafe {
                let val = pop(ctx, &mut sp);
                let res = new_bool(!try_or_break!(to_bool(val)) as i32);
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_GET_LOC0.0 as u8 => unsafe {
                let val = ptr::read_unaligned(fp.offset(FRAME_OFFSET_VAR0));
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_GET_LOC1.0 as u8 => unsafe {
                let val = ptr::read_unaligned(fp.offset(FRAME_OFFSET_VAR0 - 1));
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_GET_LOC2.0 as u8 => unsafe {
                let val = ptr::read_unaligned(fp.offset(FRAME_OFFSET_VAR0 - 2));
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_GET_LOC3.0 as u8 => unsafe {
                let val = ptr::read_unaligned(fp.offset(FRAME_OFFSET_VAR0 - 3));
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_PUT_LOC0.0 as u8 => unsafe {
                let val = pop(ctx, &mut sp);
                ptr::write_unaligned(fp.offset(FRAME_OFFSET_VAR0), val);
            },
            op if op == crate::opcode::OP_PUT_LOC1.0 as u8 => unsafe {
                let val = pop(ctx, &mut sp);
                ptr::write_unaligned(fp.offset(FRAME_OFFSET_VAR0 - 1), val);
            },
            op if op == crate::opcode::OP_PUT_LOC2.0 as u8 => unsafe {
                let val = pop(ctx, &mut sp);
                ptr::write_unaligned(fp.offset(FRAME_OFFSET_VAR0 - 2), val);
            },
            op if op == crate::opcode::OP_PUT_LOC3.0 as u8 => unsafe {
                let val = pop(ctx, &mut sp);
                ptr::write_unaligned(fp.offset(FRAME_OFFSET_VAR0 - 3), val);
            },
            op if op == crate::opcode::OP_GET_LOC8.0 as u8 => unsafe {
                let idx = match byte_slice.get(pc).copied() {
                    Some(val) => val as isize,
                    None => break Err(InterpreterError::InvalidBytecode("get_loc8")),
                };
                pc += 1;
                let val = ptr::read_unaligned(fp.offset(FRAME_OFFSET_VAR0 - idx));
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_PUT_LOC8.0 as u8 => unsafe {
                let idx = match byte_slice.get(pc).copied() {
                    Some(val) => val as isize,
                    None => break Err(InterpreterError::InvalidBytecode("put_loc8")),
                };
                pc += 1;
                let val = pop(ctx, &mut sp);
                ptr::write_unaligned(fp.offset(FRAME_OFFSET_VAR0 - idx), val);
            },
            op if op == crate::opcode::OP_GET_ARG.0 as u8 => unsafe {
                if pc + 1 >= byte_slice.len() {
                    break Err(InterpreterError::InvalidBytecode("get_arg"));
                }
                let idx = u16::from_le_bytes([byte_slice[pc], byte_slice[pc + 1]]) as usize;
                pc += 2;
                let val = ptr::read_unaligned(fp.add(FRAME_OFFSET_ARG0 as usize + idx));
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_PUT_ARG.0 as u8 => unsafe {
                if pc + 1 >= byte_slice.len() {
                    break Err(InterpreterError::InvalidBytecode("put_arg"));
                }
                let idx = u16::from_le_bytes([byte_slice[pc], byte_slice[pc + 1]]) as usize;
                pc += 2;
                let val = pop(ctx, &mut sp);
                ptr::write_unaligned(fp.add(FRAME_OFFSET_ARG0 as usize + idx), val);
            },
            op if op == crate::opcode::OP_GET_ARG0.0 as u8 => unsafe {
                let val = ptr::read_unaligned(fp.add(FRAME_OFFSET_ARG0 as usize));
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_GET_ARG1.0 as u8 => unsafe {
                let val = ptr::read_unaligned(fp.add(FRAME_OFFSET_ARG0 as usize + 1));
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_GET_ARG2.0 as u8 => unsafe {
                let val = ptr::read_unaligned(fp.add(FRAME_OFFSET_ARG0 as usize + 2));
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_GET_ARG3.0 as u8 => unsafe {
                let val = ptr::read_unaligned(fp.add(FRAME_OFFSET_ARG0 as usize + 3));
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_PUT_ARG0.0 as u8 => unsafe {
                let val = pop(ctx, &mut sp);
                ptr::write_unaligned(fp.add(FRAME_OFFSET_ARG0 as usize), val);
            },
            op if op == crate::opcode::OP_PUT_ARG1.0 as u8 => unsafe {
                let val = pop(ctx, &mut sp);
                ptr::write_unaligned(fp.add(FRAME_OFFSET_ARG0 as usize + 1), val);
            },
            op if op == crate::opcode::OP_PUT_ARG2.0 as u8 => unsafe {
                let val = pop(ctx, &mut sp);
                ptr::write_unaligned(fp.add(FRAME_OFFSET_ARG0 as usize + 2), val);
            },
            op if op == crate::opcode::OP_PUT_ARG3.0 as u8 => unsafe {
                let val = pop(ctx, &mut sp);
                ptr::write_unaligned(fp.add(FRAME_OFFSET_ARG0 as usize + 3), val);
            },
            _ => break Err(InterpreterError::UnsupportedOpcode(opcode)),
        }
    };

    ctx.set_sp(prev_sp);
    ctx.set_fp(prev_fp);
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::{ContextConfig, JSContext};
    use crate::function_bytecode::{FunctionBytecodeFields, FunctionBytecodeHeader};
    use crate::jsvalue::{is_int, value_get_int, JS_NULL};
    use crate::opcode::{OP_ADD, OP_PUSH_1, OP_PUSH_2, OP_RETURN};
    use crate::stdlib::MQUICKJS_STDLIB_IMAGE;

    fn new_context() -> JSContext {
        JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
        })
        .expect("context init")
    }

    #[test]
    fn call_simple_add() {
        let mut ctx = new_context();
        let bytecode = vec![
            OP_PUSH_1.0 as u8,
            OP_PUSH_2.0 as u8,
            OP_ADD.0 as u8,
            OP_RETURN.0 as u8,
        ];
        let byte_code_val = ctx.alloc_byte_array(&bytecode).expect("bytecode");
        let header = FunctionBytecodeHeader::new(false, false, false, 0, false);
        let fields = FunctionBytecodeFields {
            func_name: JS_NULL,
            byte_code: byte_code_val,
            cpool: JS_NULL,
            vars: JS_NULL,
            ext_vars: JS_NULL,
            stack_size: 2,
            ext_vars_len: 0,
            filename: JS_NULL,
            pc2line: JS_NULL,
            source_pos: 0,
        };
        let func = ctx
            .alloc_function_bytecode(header, fields)
            .expect("func");
        let closure = ctx.alloc_closure(func, 0).expect("closure");
        let result = call(&mut ctx, closure, &[]).expect("call");
        assert!(is_int(result));
        assert_eq!(value_get_int(result), 3);
    }
}
