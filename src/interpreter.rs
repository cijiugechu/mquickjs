use crate::context::{ContextError, JSContext};
use crate::cfunction_data::{CFunctionData, CFUNC_PARAMS_OFFSET};
use crate::containers::{ByteArrayHeader, ValueArrayHeader};
use crate::enums::JSObjectClass;
use crate::function_bytecode::FunctionBytecode;
use crate::heap::JS_STACK_SLACK;
use crate::js_libm::js_fmod;
use crate::jsvalue::{
    from_bits, is_bool, is_int, is_null, is_ptr, is_short_float, is_undefined, new_bool,
    new_short_int, short_float_to_f64, value_get_int, value_get_special_tag,
    value_get_special_value, value_make_special, value_to_ptr, JSValue, JSWord, JSW, JS_NULL,
    JS_TAG_CATCH_OFFSET, JS_TAG_SHORT_FUNC, JS_UNDEFINED,
};
use crate::memblock::{MbHeader, MTag};
use crate::object::{Object, ObjectHeader};
use crate::stdlib::cfunc::{BuiltinCFunction, CFunctionDef};
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
    InvalidCFunctionIndex(u32),
    MissingCFunction(&'static str),
    NotAFunction,
    StackOverflow,
    Thrown(JSValue),
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

fn short_func_index(val: JSValue) -> Option<u32> {
    if value_get_special_tag(val) != JS_TAG_SHORT_FUNC {
        return None;
    }
    let idx = value_get_special_value(val);
    if idx < 0 {
        return None;
    }
    Some(idx as u32)
}

fn cfunction_data(
    obj_ptr: NonNull<Object>,
    header: ObjectHeader,
) -> Result<(u32, JSValue), InterpreterError> {
    let payload = unsafe { Object::payload_ptr(obj_ptr.as_ptr()).cast::<u8>() };
    let idx = unsafe {
        // SAFETY: payload begins with the cfunction index.
        ptr::read_unaligned(payload.cast::<u32>())
    };
    let extra_bytes = header.extra_size() as usize * JSW as usize;
    let params = if extra_bytes >= size_of::<CFunctionData>() {
        let params_ptr = unsafe {
            // SAFETY: params are present when extra_bytes covers CFunctionData.
            payload.add(CFUNC_PARAMS_OFFSET)
        };
        unsafe { ptr::read_unaligned(params_ptr.cast::<JSValue>()) }
    } else {
        JS_UNDEFINED
    };
    Ok((idx, params))
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

fn read_i32(byte_slice: &[u8], pc: usize, opname: &'static str) -> Result<i32, InterpreterError> {
    if pc + 3 >= byte_slice.len() {
        return Err(InterpreterError::InvalidBytecode(opname));
    }
    Ok(i32::from_le_bytes([
        byte_slice[pc],
        byte_slice[pc + 1],
        byte_slice[pc + 2],
        byte_slice[pc + 3],
    ]))
}

fn pc_with_offset(
    pc: usize,
    diff: i32,
    byte_len: usize,
    opname: &'static str,
) -> Result<usize, InterpreterError> {
    let target = (pc as i64).wrapping_add(diff as i64);
    if target < 0 || target >= byte_len as i64 {
        return Err(InterpreterError::InvalidBytecode(opname));
    }
    Ok(target as usize)
}

fn handle_exception(
    ctx: &mut JSContext,
    byte_len: usize,
    fp: *mut JSValue,
    n_vars: usize,
    sp: &mut *mut JSValue,
    exception: JSValue,
) -> Result<Option<usize>, InterpreterError> {
    ctx.set_current_exception(exception);
    let stack_top = unsafe { fp.offset(FRAME_OFFSET_VAR0 + 1 - n_vars as isize) };
    if ctx.current_exception_is_uncatchable() {
        *sp = stack_top;
        let sp_ptr = NonNull::new(*sp).expect("stack pointer");
        ctx.set_sp(sp_ptr);
        return Ok(None);
    }
    let mut cursor = *sp;
    while cursor < stack_top {
        let val = unsafe {
            // SAFETY: cursor is within the current stack slice.
            ptr::read_unaligned(cursor)
        };
        cursor = unsafe { cursor.add(1) };
        if value_get_special_tag(val) == JS_TAG_CATCH_OFFSET {
            let offset = value_get_special_value(val);
            if offset < 0 {
                return Err(InterpreterError::InvalidBytecode("catch"));
            }
            let offset = offset as usize;
            if offset >= byte_len {
                return Err(InterpreterError::InvalidBytecode("catch"));
            }
            let exception = ctx.take_current_exception();
            let new_sp = unsafe { cursor.sub(1) };
            unsafe {
                // SAFETY: new_sp points to the catch marker slot.
                ptr::write_unaligned(new_sp, exception);
            }
            *sp = new_sp;
            let sp_ptr = NonNull::new(*sp).expect("stack pointer");
            ctx.set_sp(sp_ptr);
            return Ok(Some(offset));
        }
    }
    *sp = cursor;
    let sp_ptr = NonNull::new(*sp).expect("stack pointer");
    ctx.set_sp(sp_ptr);
    Ok(None)
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

fn call_cfunction_def(
    ctx: &mut JSContext,
    def: &CFunctionDef,
    this_val: JSValue,
    args: &[JSValue],
    params: JSValue,
) -> Result<JSValue, InterpreterError> {
    match def.func {
        BuiltinCFunction::Generic(func) => Ok(func(ctx, this_val, args)),
        BuiltinCFunction::GenericMagic(func) => Ok(func(ctx, this_val, args, def.magic as i32)),
        BuiltinCFunction::Constructor(func) => Ok(func(ctx, this_val, args)),
        BuiltinCFunction::ConstructorMagic(func) => Ok(func(ctx, this_val, args, def.magic as i32)),
        BuiltinCFunction::GenericParams(func) => Ok(func(ctx, this_val, args, params)),
        BuiltinCFunction::FF(func) => {
            let arg = args.first().copied().unwrap_or(JS_UNDEFINED);
            let num = value_to_f64(arg)?;
            let out = func(num);
            Ok(ctx.new_float64(out)?)
        }
        BuiltinCFunction::Missing(_) => Err(InterpreterError::MissingCFunction(def.func_name)),
    }
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
    if let Some(idx) = short_func_index(func) {
        let def = *ctx
            .c_function(idx as usize)
            .ok_or(InterpreterError::InvalidCFunctionIndex(idx))?;
        return call_cfunction_def(ctx, &def, this_obj, args, JS_UNDEFINED);
    }
    if !is_ptr(func) {
        return Err(InterpreterError::NotAFunction);
    }
    let obj_ptr = match object_ptr(func) {
        Ok(ptr) => ptr,
        Err(_) => return Err(InterpreterError::NotAFunction),
    };
    let header_word = unsafe {
        // SAFETY: obj_ptr points at a readable object header word.
        ptr::read_unaligned(obj_ptr.as_ptr().cast::<JSWord>())
    };
    let header = ObjectHeader::from_word(header_word);
    match header.class_id() {
        class_id if class_id == JSObjectClass::CFunction as u8 => {
            let (idx, params) = cfunction_data(obj_ptr, header)?;
            let def = *ctx
                .c_function(idx as usize)
                .ok_or(InterpreterError::InvalidCFunctionIndex(idx))?;
            return call_cfunction_def(ctx, &def, this_obj, args, params);
        }
        class_id if class_id != JSObjectClass::Closure as u8 => {
            return Err(InterpreterError::NotAFunction);
        }
        _ => {}
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
            op if op == crate::opcode::OP_THROW.0 as u8 => unsafe {
                let val = pop(ctx, &mut sp);
                let handled = try_or_break!(handle_exception(
                    ctx,
                    byte_slice.len(),
                    fp,
                    n_vars,
                    &mut sp,
                    val
                ));
                if let Some(target) = handled {
                    pc = target;
                } else {
                    break Err(InterpreterError::Thrown(val));
                }
            },
            op if op == crate::opcode::OP_CATCH.0 as u8 => unsafe {
                let diff = try_or_break!(read_i32(byte_slice, pc, "catch"));
                let target = try_or_break!(pc_with_offset(pc, diff, byte_slice.len(), "catch"));
                let offset = u32::try_from(target)
                    .map_err(|_| InterpreterError::InvalidBytecode("catch"))?;
                push(ctx, &mut sp, value_make_special(JS_TAG_CATCH_OFFSET, offset));
                pc += 4;
            },
            op if op == crate::opcode::OP_GOSUB.0 as u8 => unsafe {
                let diff = try_or_break!(read_i32(byte_slice, pc, "gosub"));
                let return_pc = pc + 4;
                let return_pc =
                    i32::try_from(return_pc).map_err(|_| InterpreterError::InvalidBytecode("gosub"))?;
                push(ctx, &mut sp, new_short_int(return_pc));
                pc = try_or_break!(pc_with_offset(pc, diff, byte_slice.len(), "gosub"));
            },
            op if op == crate::opcode::OP_RET.0 as u8 => unsafe {
                let val = ptr::read_unaligned(sp);
                if !is_int(val) {
                    break Err(InterpreterError::InvalidBytecode("ret"));
                }
                let pos = value_get_int(val);
                if pos < 0 || (pos as usize) >= byte_slice.len() {
                    break Err(InterpreterError::InvalidBytecode("ret"));
                }
                let _ = pop(ctx, &mut sp);
                pc = pos as usize;
            },
            op if op == crate::opcode::OP_GOTO.0 as u8 => {
                let diff = try_or_break!(read_i32(byte_slice, pc, "goto"));
                pc = try_or_break!(pc_with_offset(pc, diff, byte_slice.len(), "goto"));
            }
            op if op == crate::opcode::OP_IF_FALSE.0 as u8 => unsafe {
                let diff = try_or_break!(read_i32(byte_slice, pc, "if_false"));
                let target = try_or_break!(pc_with_offset(pc, diff, byte_slice.len(), "if_false"));
                pc += 4;
                let cond = try_or_break!(to_bool(pop(ctx, &mut sp)));
                if !cond {
                    pc = target;
                }
            },
            op if op == crate::opcode::OP_IF_TRUE.0 as u8 => unsafe {
                let diff = try_or_break!(read_i32(byte_slice, pc, "if_true"));
                let target = try_or_break!(pc_with_offset(pc, diff, byte_slice.len(), "if_true"));
                pc += 4;
                let cond = try_or_break!(to_bool(pop(ctx, &mut sp)));
                if cond {
                    pc = target;
                }
            },
            op if op == crate::opcode::OP_DROP.0 as u8 => unsafe {
                let _ = pop(ctx, &mut sp);
            },
            op if op == crate::opcode::OP_NIP.0 as u8 => unsafe {
                let val = ptr::read_unaligned(sp);
                ptr::write_unaligned(sp.add(1), val);
                let _ = pop(ctx, &mut sp);
            },
            op if op == crate::opcode::OP_DUP.0 as u8 => unsafe {
                let val = ptr::read_unaligned(sp);
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_DUP2.0 as u8 => unsafe {
                sp = sp.sub(2);
                let a = ptr::read_unaligned(sp.add(2));
                let b = ptr::read_unaligned(sp.add(3));
                ptr::write_unaligned(sp, a);
                ptr::write_unaligned(sp.add(1), b);
                ctx.set_sp(NonNull::new(sp).expect("stack pointer"));
            },
            op if op == crate::opcode::OP_INSERT2.0 as u8 => unsafe {
                let top = ptr::read_unaligned(sp);
                let next = ptr::read_unaligned(sp.add(1));
                ptr::write_unaligned(sp.sub(1), top);
                ptr::write_unaligned(sp, next);
                ptr::write_unaligned(sp.add(1), top);
                sp = sp.sub(1);
                ctx.set_sp(NonNull::new(sp).expect("stack pointer"));
            },
            op if op == crate::opcode::OP_INSERT3.0 as u8 => unsafe {
                let top = ptr::read_unaligned(sp);
                let next = ptr::read_unaligned(sp.add(1));
                let next2 = ptr::read_unaligned(sp.add(2));
                ptr::write_unaligned(sp.sub(1), top);
                ptr::write_unaligned(sp, next);
                ptr::write_unaligned(sp.add(1), next2);
                ptr::write_unaligned(sp.add(2), top);
                sp = sp.sub(1);
                ctx.set_sp(NonNull::new(sp).expect("stack pointer"));
            },
            op if op == crate::opcode::OP_PERM3.0 as u8 => unsafe {
                let tmp = ptr::read_unaligned(sp.add(1));
                let val = ptr::read_unaligned(sp.add(2));
                ptr::write_unaligned(sp.add(1), val);
                ptr::write_unaligned(sp.add(2), tmp);
            },
            op if op == crate::opcode::OP_PERM4.0 as u8 => unsafe {
                let tmp = ptr::read_unaligned(sp.add(1));
                let val = ptr::read_unaligned(sp.add(2));
                let val2 = ptr::read_unaligned(sp.add(3));
                ptr::write_unaligned(sp.add(1), val);
                ptr::write_unaligned(sp.add(2), val2);
                ptr::write_unaligned(sp.add(3), tmp);
            },
            op if op == crate::opcode::OP_SWAP.0 as u8 => unsafe {
                let a = ptr::read_unaligned(sp);
                let b = ptr::read_unaligned(sp.add(1));
                ptr::write_unaligned(sp, b);
                ptr::write_unaligned(sp.add(1), a);
            },
            op if op == crate::opcode::OP_ROT3L.0 as u8 => unsafe {
                let tmp = ptr::read_unaligned(sp.add(2));
                let mid = ptr::read_unaligned(sp.add(1));
                let top = ptr::read_unaligned(sp);
                ptr::write_unaligned(sp.add(2), mid);
                ptr::write_unaligned(sp.add(1), top);
                ptr::write_unaligned(sp, tmp);
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
    use crate::opcode::{
        OP_ADD, OP_CATCH, OP_DUP2, OP_GOSUB, OP_GOTO, OP_IF_FALSE, OP_IF_TRUE, OP_INSERT2,
        OP_INSERT3, OP_NIP, OP_PERM3, OP_PERM4, OP_PUSH_1, OP_PUSH_2, OP_PUSH_3, OP_PUSH_4,
        OP_PUSH_TRUE, OP_RET, OP_RETURN, OP_ROT3L, OP_SUB, OP_THROW,
    };
    use crate::stdlib::MQUICKJS_STDLIB_IMAGE;

    fn new_context() -> JSContext {
        JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
        })
        .expect("context init")
    }

    fn emit_i32(buf: &mut Vec<u8>, value: i32) {
        buf.extend_from_slice(&value.to_le_bytes());
    }

    fn call_bytecode(ctx: &mut JSContext, bytecode: Vec<u8>) -> JSValue {
        let byte_code_val = ctx.alloc_byte_array(&bytecode).expect("bytecode");
        let header = FunctionBytecodeHeader::new(false, false, false, 0, false);
        let fields = FunctionBytecodeFields {
            func_name: JS_NULL,
            byte_code: byte_code_val,
            cpool: JS_NULL,
            vars: JS_NULL,
            ext_vars: JS_NULL,
            stack_size: 4,
            ext_vars_len: 0,
            filename: JS_NULL,
            pc2line: JS_NULL,
            source_pos: 0,
        };
        let func = ctx
            .alloc_function_bytecode(header, fields)
            .expect("func");
        let closure = ctx.alloc_closure(func, 0).expect("closure");
        call(ctx, closure, &[]).expect("call")
    }

    #[test]
    fn call_simple_add() {
        let mut ctx = new_context();
        let result = call_bytecode(
            &mut ctx,
            vec![OP_PUSH_1.0 as u8, OP_PUSH_2.0 as u8, OP_ADD.0 as u8, OP_RETURN.0 as u8],
        );
        assert!(is_int(result));
        assert_eq!(value_get_int(result), 3);
    }

    #[test]
    fn control_flow_if_false_goto() {
        let mut ctx = new_context();
        let mut bytecode = Vec::new();
        bytecode.push(OP_PUSH_TRUE.0 as u8);
        bytecode.push(OP_IF_FALSE.0 as u8);
        emit_i32(&mut bytecode, 10);
        bytecode.push(OP_PUSH_1.0 as u8);
        bytecode.push(OP_GOTO.0 as u8);
        emit_i32(&mut bytecode, 5);
        bytecode.push(OP_PUSH_2.0 as u8);
        bytecode.push(OP_RETURN.0 as u8);
        let result = call_bytecode(&mut ctx, bytecode);
        assert!(is_int(result));
        assert_eq!(value_get_int(result), 1);
    }

    #[test]
    fn control_flow_if_true() {
        let mut ctx = new_context();
        let mut bytecode = Vec::new();
        bytecode.push(OP_PUSH_TRUE.0 as u8);
        bytecode.push(OP_IF_TRUE.0 as u8);
        emit_i32(&mut bytecode, 6);
        bytecode.push(OP_PUSH_1.0 as u8);
        bytecode.push(OP_RETURN.0 as u8);
        bytecode.push(OP_PUSH_2.0 as u8);
        bytecode.push(OP_RETURN.0 as u8);
        let result = call_bytecode(&mut ctx, bytecode);
        assert!(is_int(result));
        assert_eq!(value_get_int(result), 2);
    }

    #[test]
    fn control_flow_gosub_ret() {
        let mut ctx = new_context();
        let mut bytecode = Vec::new();
        bytecode.push(OP_PUSH_1.0 as u8);
        bytecode.push(OP_GOSUB.0 as u8);
        emit_i32(&mut bytecode, 7);
        bytecode.push(OP_PUSH_2.0 as u8);
        bytecode.push(OP_ADD.0 as u8);
        bytecode.push(OP_RETURN.0 as u8);
        bytecode.push(OP_RET.0 as u8);
        let result = call_bytecode(&mut ctx, bytecode);
        assert!(is_int(result));
        assert_eq!(value_get_int(result), 3);
    }

    #[test]
    fn exception_flow_catch_throw() {
        let mut ctx = new_context();
        let mut bytecode = Vec::new();
        bytecode.push(OP_CATCH.0 as u8);
        emit_i32(&mut bytecode, 8);
        bytecode.push(OP_PUSH_1.0 as u8);
        bytecode.push(OP_THROW.0 as u8);
        bytecode.push(OP_PUSH_2.0 as u8);
        bytecode.push(OP_RETURN.0 as u8);
        bytecode.push(OP_RETURN.0 as u8);
        let result = call_bytecode(&mut ctx, bytecode);
        assert!(is_int(result));
        assert_eq!(value_get_int(result), 1);
    }

    #[test]
    fn stack_ops_shuffle() {
        let mut ctx = new_context();
        let result = call_bytecode(
            &mut ctx,
            vec![OP_PUSH_1.0 as u8, OP_PUSH_2.0 as u8, OP_NIP.0 as u8, OP_RETURN.0 as u8],
        );
        assert!(is_int(result));
        assert_eq!(value_get_int(result), 2);

        let mut ctx = new_context();
        let result = call_bytecode(
            &mut ctx,
            vec![
                OP_PUSH_1.0 as u8,
                OP_PUSH_2.0 as u8,
                OP_DUP2.0 as u8,
                OP_ADD.0 as u8,
                OP_ADD.0 as u8,
                OP_ADD.0 as u8,
                OP_RETURN.0 as u8,
            ],
        );
        assert!(is_int(result));
        assert_eq!(value_get_int(result), 6);

        let mut ctx = new_context();
        let result = call_bytecode(
            &mut ctx,
            vec![
                OP_PUSH_1.0 as u8,
                OP_PUSH_2.0 as u8,
                OP_INSERT2.0 as u8,
                OP_ADD.0 as u8,
                OP_ADD.0 as u8,
                OP_RETURN.0 as u8,
            ],
        );
        assert!(is_int(result));
        assert_eq!(value_get_int(result), 5);

        let mut ctx = new_context();
        let result = call_bytecode(
            &mut ctx,
            vec![
                OP_PUSH_1.0 as u8,
                OP_PUSH_2.0 as u8,
                OP_PUSH_3.0 as u8,
                OP_INSERT3.0 as u8,
                OP_ADD.0 as u8,
                OP_ADD.0 as u8,
                OP_ADD.0 as u8,
                OP_RETURN.0 as u8,
            ],
        );
        assert!(is_int(result));
        assert_eq!(value_get_int(result), 9);

        let mut ctx = new_context();
        let result = call_bytecode(
            &mut ctx,
            vec![
                OP_PUSH_1.0 as u8,
                OP_PUSH_2.0 as u8,
                OP_PUSH_3.0 as u8,
                OP_PERM3.0 as u8,
                OP_SUB.0 as u8,
                OP_RETURN.0 as u8,
            ],
        );
        assert!(is_int(result));
        assert_eq!(value_get_int(result), -2);

        let mut ctx = new_context();
        let result = call_bytecode(
            &mut ctx,
            vec![
                OP_PUSH_1.0 as u8,
                OP_PUSH_2.0 as u8,
                OP_PUSH_3.0 as u8,
                OP_ROT3L.0 as u8,
                OP_SUB.0 as u8,
                OP_RETURN.0 as u8,
            ],
        );
        assert!(is_int(result));
        assert_eq!(value_get_int(result), 2);

        let mut ctx = new_context();
        let result = call_bytecode(
            &mut ctx,
            vec![
                OP_PUSH_1.0 as u8,
                OP_PUSH_2.0 as u8,
                OP_PUSH_3.0 as u8,
                OP_PUSH_4.0 as u8,
                OP_PERM4.0 as u8,
                OP_SUB.0 as u8,
                OP_RETURN.0 as u8,
            ],
        );
        assert!(is_int(result));
        assert_eq!(value_get_int(result), -2);
    }
}
