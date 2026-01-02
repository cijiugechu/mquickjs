use crate::array_data::ArrayData;
use crate::atom::{string_compare, string_eq};
use crate::context::{ContextError, JSContext};
use crate::conversion::{self, ConversionError, ToPrimitiveHint};
use crate::cfunction_data::{CFunctionData, CFUNC_PARAMS_OFFSET};
use crate::containers::{ByteArrayHeader, ValueArrayHeader, VarRefHeader};
use crate::enums::{JSObjectClass, JSPropType, JSVarRefKind};
use crate::function_bytecode::FunctionBytecode;
use crate::heap::JS_STACK_SLACK;
use crate::js_libm::{js_fmod, js_pow};
use crate::jsvalue::{JSValue, JSWord};
use crate::memblock::{MbHeader, MTag, JS_MTAG_BITS};
use crate::object::{Object, ObjectHeader};
use crate::property::{define_property_varref, get_own_property_raw, object_keys, PropertyError};
use crate::heap::set_free_block;
use crate::stdlib::cfunc::{BuiltinCFunction, CFunctionDef};
use core::mem::size_of;
use core::ptr::{self, NonNull};

const FRAME_OFFSET_ARG0: isize = 4;
const FRAME_OFFSET_FUNC_OBJ: isize = 3;
const FRAME_OFFSET_THIS_OBJ: isize = 2;
const FRAME_OFFSET_CALL_FLAGS: isize = 1;
const FRAME_OFFSET_SAVED_FP: isize = 0;
const FRAME_OFFSET_CUR_PC: isize = -1;
const FRAME_OFFSET_FIRST_VARREF: isize = -2;
const FRAME_OFFSET_VAR0: isize = -3;

const FRAME_CF_ARGC_MASK: i32 = 0xffff;
const FRAME_CF_CTOR: i32 = 1 << 16;
const FRAME_CF_POP_RET: i32 = 1 << 17;
const FRAME_CF_PC_ADD1: i32 = 1 << 18;

#[derive(Debug)]
pub enum InterpreterError {
    Context(ContextError),
    InvalidBytecode(&'static str),
    InvalidValue(&'static str),
    InvalidCFunctionIndex(u32),
    MissingCFunction(&'static str),
    NotAFunction,
    ReferenceError(&'static str),
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

impl From<ConversionError> for InterpreterError {
    fn from(err: ConversionError) -> Self {
        match err {
            ConversionError::Context(err) => InterpreterError::Context(err),
            ConversionError::Property(_) => InterpreterError::TypeError("property"),
            ConversionError::Interpreter(err) => err,
            ConversionError::TypeError(msg) => InterpreterError::TypeError(msg),
        }
    }
}

struct ByteArrayRaw {
    buf: NonNull<u8>,
    len: usize,
}

impl ByteArrayRaw {
    unsafe fn from_value(val: JSValue) -> Result<Self, InterpreterError> {
        let ptr = val.to_ptr::<u8>().ok_or(InterpreterError::InvalidValue("byte array ptr"))?;
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
        let ptr = val.to_ptr::<u8>().ok_or(InterpreterError::InvalidValue("value array ptr"))?;
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

    unsafe fn write(&self, index: usize, val: JSValue) -> Result<(), InterpreterError> {
        if index >= self.len {
            return Err(InterpreterError::InvalidBytecode("value array index out of bounds"));
        }
        unsafe {
            // SAFETY: index is within the value array bounds.
            ptr::write_unaligned(self.arr.as_ptr().add(index), val);
        }
        Ok(())
    }
}

fn object_ptr(val: JSValue) -> Result<NonNull<Object>, InterpreterError> {
    let ptr = val.to_ptr::<Object>().ok_or(InterpreterError::InvalidValue("object ptr"))?;
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

fn object_header(ptr: NonNull<Object>) -> ObjectHeader {
    let header_word = unsafe {
        // SAFETY: ptr points at a readable object header word.
        ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
    };
    ObjectHeader::from_word(header_word)
}

fn object_proto(ptr: NonNull<Object>) -> JSValue {
    unsafe {
        // SAFETY: ptr points at a readable object proto slot.
        ptr::read_unaligned(Object::proto_ptr(ptr.as_ptr()))
    }
}

fn object_props(ptr: NonNull<Object>) -> JSValue {
    unsafe {
        // SAFETY: ptr points at a readable object props slot.
        ptr::read_unaligned(Object::props_ptr(ptr.as_ptr()))
    }
}

fn set_object_proto(ptr: NonNull<Object>, proto: JSValue) {
    unsafe {
        // SAFETY: ptr points at a writable object proto slot.
        ptr::write_unaligned(Object::proto_ptr(ptr.as_ptr()), proto);
    }
}

fn array_data(ptr: NonNull<Object>) -> ArrayData {
    unsafe {
        // SAFETY: ptr points at a valid object payload.
        let payload = Object::payload_ptr(ptr.as_ptr());
        let array_ptr = ptr::addr_of!((*payload).array);
        ptr::read_unaligned(array_ptr)
    }
}

fn for_of_start(ctx: &mut JSContext, mut val: JSValue, is_for_in: bool) -> Result<JSValue, InterpreterError> {
    if is_for_in {
        val = object_keys(ctx, val).map_err(map_property_error)?;
    }
    if !val.is_object() {
        return Err(InterpreterError::TypeError("unsupported type in for...of"));
    }
    let obj_ptr = object_ptr(val)?;
    let header = object_header(obj_ptr);
    if header.class_id() != JSObjectClass::Array as u8 {
        return Err(InterpreterError::TypeError("unsupported type in for...of"));
    }
    let iter_ptr = ctx.alloc_value_array(2)?;
    unsafe {
        // SAFETY: iter_ptr points to a freshly allocated value array.
        let arr = iter_ptr.as_ptr().add(size_of::<JSWord>()) as *mut JSValue;
        ptr::write_unaligned(arr, val);
        ptr::write_unaligned(arr.add(1), JSValue::new_short_int(0));
    }
    Ok(JSValue::from_ptr(iter_ptr))
}

fn for_of_next(
    _ctx: &mut JSContext,
    iter_val: JSValue,
    done_slot: *mut JSValue,
    value_slot: *mut JSValue,
) -> Result<(), InterpreterError> {
    let iter = unsafe {
        // SAFETY: iter_val must be a value array from for_of_start.
        ValueArrayRaw::from_value(iter_val)?
    };
    let target = unsafe { iter.read(0)? };
    let idx_val = unsafe { iter.read(1)? };
    if !idx_val.is_int() {
        return Err(InterpreterError::InvalidValue("for_of index"));
    }
    let idx = idx_val.get_int();
    if idx < 0 {
        return Err(InterpreterError::InvalidValue("for_of index"));
    }
    let obj_ptr = object_ptr(target)?;
    let header = object_header(obj_ptr);
    if header.class_id() != JSObjectClass::Array as u8 {
        return Err(InterpreterError::TypeError("unsupported type in for...of"));
    }
    let data = array_data(obj_ptr);
    if (idx as u32) >= data.len() {
        unsafe {
            // SAFETY: caller provided writable stack slots.
            ptr::write_unaligned(done_slot, JSValue::new_bool(1));
            ptr::write_unaligned(value_slot, JSValue::JS_UNDEFINED);
        }
        return Ok(());
    }
    let tab = data.tab();
    let value = if tab == JSValue::JS_NULL {
        JSValue::JS_UNDEFINED
    } else {
        let arr = unsafe {
            // SAFETY: tab is the backing value array for an ArrayData payload.
            ValueArrayRaw::from_value(tab)?
        };
        unsafe { arr.read(idx as usize)? }
    };
    unsafe {
        // SAFETY: caller provided writable stack slots.
        ptr::write_unaligned(done_slot, JSValue::new_bool(0));
        ptr::write_unaligned(value_slot, value);
    }
    unsafe {
        // SAFETY: iterator state array has length 2.
        iter.write(1, JSValue::new_short_int(idx + 1))?;
    }
    Ok(())
}

fn set_prototype_internal(obj: JSValue, proto: JSValue) -> Result<(), InterpreterError> {
    let obj_ptr = object_ptr(obj)?;
    if object_proto(obj_ptr) == proto {
        return Ok(());
    }
    if proto != JSValue::JS_NULL {
        let mut current = proto;
        loop {
            let current_ptr = object_ptr(current)?;
            if current_ptr == obj_ptr {
                return Err(InterpreterError::TypeError("circular prototype chain"));
            }
            let next = object_proto(current_ptr);
            if next == JSValue::JS_NULL {
                break;
            }
            current = next;
        }
    }
    set_object_proto(obj_ptr, proto);
    Ok(())
}

fn function_bytecode_ptr(val: JSValue) -> Result<NonNull<FunctionBytecode>, InterpreterError> {
    let ptr = val.to_ptr::<FunctionBytecode>()
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
    if val.get_special_tag() != JSValue::JS_TAG_SHORT_FUNC {
        return None;
    }
    let idx = val.get_special_value();
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
    let extra_bytes = header.extra_size() as usize * JSValue::JSW as usize;
    let params = if extra_bytes >= size_of::<CFunctionData>() {
        let params_ptr = unsafe {
            // SAFETY: params are present when extra_bytes covers CFunctionData.
            payload.add(CFUNC_PARAMS_OFFSET)
        };
        unsafe { ptr::read_unaligned(params_ptr.cast::<JSValue>()) }
    } else {
        JSValue::JS_UNDEFINED
    };
    Ok((idx, params))
}

fn value_to_f64(val: JSValue) -> Result<f64, InterpreterError> {
    if val.is_int() {
        return Ok(f64::from(val.get_int()));
    }
    #[cfg(target_pointer_width = "64")]
    {
        if val.is_short_float() {
            return Ok(val.short_float_to_f64());
        }
    }
    if val.is_ptr() {
        let ptr = val.to_ptr::<u8>().ok_or(InterpreterError::TypeError("float ptr"))?;
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
    if val.is_bool() {
        return Ok(val.get_special_value() != 0);
    }
    if val.is_null() || val.is_undefined() {
        return Ok(false);
    }
    if val.is_int() {
        return Ok(val.get_int() != 0);
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
        if val.get_special_tag() == JSValue::JS_TAG_CATCH_OFFSET {
            let offset = val.get_special_value();
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
    let offset = (stack_top as isize - sp as isize) / (JSValue::JSW as isize);
    let offset = i32::try_from(offset).map_err(|_| InterpreterError::StackOverflow)?;
    Ok(JSValue::new_short_int(offset))
}

fn decode_stack_ptr(stack_top: *mut JSValue, val: JSValue) -> Result<*mut JSValue, InterpreterError> {
    if !val.is_int() {
        return Err(InterpreterError::InvalidValue("saved fp"));
    }
    let offset = val.get_int() as isize;
    if offset < 0 {
        return Err(InterpreterError::InvalidValue("saved fp"));
    }
    Ok(unsafe { stack_top.offset(-offset) })
}

fn call_argc(call_flags: i32) -> usize {
    (call_flags & FRAME_CF_ARGC_MASK) as usize
}

fn stack_check(ctx: &JSContext, sp: *mut JSValue, slots: usize) -> Result<(), InterpreterError> {
    let new_sp = unsafe { sp.sub(slots) };
    let heap_limit = ctx.heap_free_ptr().as_ptr() as usize + (JS_STACK_SLACK as usize * JSValue::JSW as usize);
    if (new_sp as usize) < heap_limit {
        return Err(InterpreterError::StackOverflow);
    }
    Ok(())
}

unsafe fn reverse_vals(sp: *mut JSValue, n: usize) {
    for i in 0..n / 2 {
        let left = unsafe { ptr::read_unaligned(sp.add(i)) };
        let right = unsafe { ptr::read_unaligned(sp.add(n - 1 - i)) };
        unsafe {
            ptr::write_unaligned(sp.add(i), right);
            ptr::write_unaligned(sp.add(n - 1 - i), left);
        }
    }
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

fn add_slow(ctx: &mut JSContext, left: JSValue, right: JSValue) -> Result<JSValue, InterpreterError> {
    let left = conversion::to_primitive(ctx, left, ToPrimitiveHint::None)?;
    let right = conversion::to_primitive(ctx, right, ToPrimitiveHint::None)?;
    if left.is_string() || right.is_string() {
        let left = conversion::to_string(ctx, left)?;
        let right = conversion::to_string(ctx, right)?;
        return Ok(conversion::concat_strings(ctx, left, right)?);
    }
    let d1 = conversion::to_number(ctx, left)?;
    let d2 = conversion::to_number(ctx, right)?;
    Ok(ctx.new_float64(d1 + d2)?)
}

fn binary_arith_slow(
    ctx: &mut JSContext,
    opcode: u8,
    left: JSValue,
    right: JSValue,
) -> Result<JSValue, InterpreterError> {
    let d1 = conversion::to_number(ctx, left)?;
    let d2 = conversion::to_number(ctx, right)?;
    let res = match opcode {
        op if op == crate::opcode::OP_SUB.as_u8() => d1 - d2,
        op if op == crate::opcode::OP_MUL.as_u8() => d1 * d2,
        op if op == crate::opcode::OP_DIV.as_u8() => d1 / d2,
        op if op == crate::opcode::OP_MOD.as_u8() => js_fmod(d1, d2),
        op if op == crate::opcode::OP_POW.as_u8() => js_pow(d1, d2),
        _ => return Err(InterpreterError::InvalidBytecode("binary arith slow")),
    };
    Ok(ctx.new_float64(res)?)
}

fn get_length_value(ctx: &mut JSContext, obj: JSValue) -> Result<JSValue, InterpreterError> {
    if obj.is_string() {
        let len = ctx.string_len(obj);
        return Ok(JSValue::new_short_int(len as i32));
    }
    if obj.is_object() {
        let obj_ptr = object_ptr(obj)?;
        let header = object_header(obj_ptr);
        if header.class_id() == JSObjectClass::Array as u8 {
            let proto = object_proto(obj_ptr);
            let props = object_props(obj_ptr);
            if proto == ctx.class_proto()[JSObjectClass::Array as usize]
                && props == ctx.empty_props()
            {
                let data = array_data(obj_ptr);
                return Ok(JSValue::new_short_int(data.len() as i32));
            }
        }
    }
    let length_key = ctx.intern_string(b"length")?;
    let val = crate::property::get_property(ctx, obj, length_key).map_err(ConversionError::from)?;
    Ok(val)
}

fn unary_arith_slow(
    ctx: &mut JSContext,
    opcode: u8,
    val: JSValue,
) -> Result<JSValue, InterpreterError> {
    let mut d = conversion::to_number(ctx, val)?;
    match opcode {
        op if op == crate::opcode::OP_INC.as_u8() => d += 1.0,
        op if op == crate::opcode::OP_DEC.as_u8() => d -= 1.0,
        op if op == crate::opcode::OP_PLUS.as_u8() => {}
        op if op == crate::opcode::OP_NEG.as_u8() => d = -d,
        _ => return Err(InterpreterError::InvalidBytecode("unary arith slow")),
    }
    Ok(ctx.new_float64(d)?)
}

fn post_inc_slow(
    ctx: &mut JSContext,
    opcode: u8,
    val: JSValue,
) -> Result<(JSValue, JSValue), InterpreterError> {
    let d = conversion::to_number(ctx, val)?;
    let old_val = ctx.new_float64(d)?;
    let delta = if opcode == crate::opcode::OP_POST_INC.as_u8() {
        1.0
    } else {
        -1.0
    };
    let new_val = ctx.new_float64(d + delta)?;
    Ok((old_val, new_val))
}

fn binary_logic_slow(
    ctx: &mut JSContext,
    opcode: u8,
    left: JSValue,
    right: JSValue,
) -> Result<JSValue, InterpreterError> {
    let v1 = conversion::to_uint32(ctx, left)?;
    let v2 = conversion::to_uint32(ctx, right)?;
    let res = match opcode {
        op if op == crate::opcode::OP_SHL.as_u8() => {
            let r = (v1 as i32) << (v2 & 0x1f);
            return Ok(ctx.new_float64(r as f64)?);
        }
        op if op == crate::opcode::OP_SAR.as_u8() => {
            let r = (v1 as i32) >> (v2 & 0x1f);
            return Ok(ctx.new_float64(r as f64)?);
        }
        op if op == crate::opcode::OP_SHR.as_u8() => {
            let r = v1 >> (v2 & 0x1f);
            return Ok(ctx.new_float64(r as f64)?);
        }
        op if op == crate::opcode::OP_AND.as_u8() => v1 & v2,
        op if op == crate::opcode::OP_OR.as_u8() => v1 | v2,
        op if op == crate::opcode::OP_XOR.as_u8() => v1 ^ v2,
        _ => return Err(InterpreterError::InvalidBytecode("binary logic slow")),
    };
    Ok(ctx.new_float64((res as i32) as f64)?)
}

fn not_slow(ctx: &mut JSContext, val: JSValue) -> Result<JSValue, InterpreterError> {
    let r = conversion::to_uint32(ctx, val)?;
    Ok(ctx.new_float64((!r as i32) as f64)?)
}

fn relational_slow(
    ctx: &mut JSContext,
    opcode: u8,
    left: JSValue,
    right: JSValue,
) -> Result<JSValue, InterpreterError> {
    let left = conversion::to_primitive(ctx, left, ToPrimitiveHint::Number)?;
    let right = conversion::to_primitive(ctx, right, ToPrimitiveHint::Number)?;
    let res = if left.is_string() && right.is_string() {
        let cmp = string_compare(left, right);
        match opcode {
            op if op == crate::opcode::OP_LT.as_u8() => cmp < 0,
            op if op == crate::opcode::OP_LTE.as_u8() => cmp <= 0,
            op if op == crate::opcode::OP_GT.as_u8() => cmp > 0,
            op if op == crate::opcode::OP_GTE.as_u8() => cmp >= 0,
            _ => return Err(InterpreterError::InvalidBytecode("relational slow")),
        }
    } else {
        let d1 = conversion::to_number(ctx, left)?;
        let d2 = conversion::to_number(ctx, right)?;
        match opcode {
            op if op == crate::opcode::OP_LT.as_u8() => d1 < d2,
            op if op == crate::opcode::OP_LTE.as_u8() => d1 <= d2,
            op if op == crate::opcode::OP_GT.as_u8() => d1 > d2,
            op if op == crate::opcode::OP_GTE.as_u8() => d1 >= d2,
            _ => return Err(InterpreterError::InvalidBytecode("relational slow")),
        }
    };
    Ok(JSValue::new_bool(res as i32))
}

fn strict_eq(ctx: &mut JSContext, left: JSValue, right: JSValue) -> Result<bool, InterpreterError> {
    if left.is_number() {
        if !right.is_number() {
            return Ok(false);
        }
        let d1 = conversion::to_number(ctx, left)?;
        let d2 = conversion::to_number(ctx, right)?;
        return Ok(d1 == d2);
    }
    if left.is_string() {
        if !right.is_string() {
            return Ok(false);
        }
        return Ok(string_eq(left, right));
    }
    Ok(left == right)
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum EqTag {
    Number,
    String,
    Object,
    Bool,
    Null,
    Undefined,
    Other,
}

fn eq_tag(val: JSValue) -> EqTag {
    if val.is_int() {
        return EqTag::Number;
    }
    #[cfg(target_pointer_width = "64")]
    if val.is_short_float() {
        return EqTag::Number;
    }
    if val.is_ptr() {
        return match mtag_from_value(val) {
            Some(MTag::Float64) => EqTag::Number,
            Some(MTag::String) => EqTag::String,
            _ => EqTag::Object,
        };
    }
    match val.get_special_tag() {
        tag if tag == JSValue::JS_TAG_STRING_CHAR => EqTag::String,
        tag if tag == JSValue::JS_TAG_SHORT_FUNC => EqTag::Object,
        tag if tag == JSValue::JS_TAG_BOOL => EqTag::Bool,
        tag if tag == JSValue::JS_TAG_NULL => EqTag::Null,
        tag if tag == JSValue::JS_TAG_UNDEFINED => EqTag::Undefined,
        _ => EqTag::Other,
    }
}

fn eq_slow(
    ctx: &mut JSContext,
    mut left: JSValue,
    mut right: JSValue,
    is_neq: bool,
) -> Result<JSValue, InterpreterError> {
    loop {
        let tag1 = eq_tag(left);
        let tag2 = eq_tag(right);
        let res = if tag1 == tag2 {
            strict_eq(ctx, left, right)?
        } else if (tag1 == EqTag::Null && tag2 == EqTag::Undefined)
            || (tag2 == EqTag::Null && tag1 == EqTag::Undefined)
        {
            true
        } else if (tag1 == EqTag::String && tag2 == EqTag::Number)
            || (tag2 == EqTag::String && tag1 == EqTag::Number)
        {
            let d1 = conversion::to_number(ctx, left)?;
            let d2 = conversion::to_number(ctx, right)?;
            d1 == d2
        } else if tag1 == EqTag::Bool {
            left = JSValue::new_short_int(left.get_special_value());
            continue;
        } else if tag2 == EqTag::Bool {
            right = JSValue::new_short_int(right.get_special_value());
            continue;
        } else if tag1 == EqTag::Object
            && (tag2 == EqTag::Number || tag2 == EqTag::String)
        {
            left = conversion::to_primitive(ctx, left, ToPrimitiveHint::None)?;
            continue;
        } else if tag2 == EqTag::Object
            && (tag1 == EqTag::Number || tag1 == EqTag::String)
        {
            right = conversion::to_primitive(ctx, right, ToPrimitiveHint::None)?;
            continue;
        } else {
            false
        };
        return Ok(JSValue::new_bool((res ^ is_neq) as i32));
    }
}

fn typeof_value(ctx: &mut JSContext, val: JSValue) -> Result<JSValue, InterpreterError> {
    let name = if val.is_number() {
        "number"
    } else if val.is_string() {
        "string"
    } else if val.is_bool() {
        "boolean"
    } else if val.is_function() {
        "function"
    } else if val.is_object() || val.is_null() {
        "object"
    } else {
        "undefined"
    };
    Ok(ctx.intern_string(name.as_bytes())?)
}

fn is_function_object(val: JSValue) -> bool {
    let ptr = match object_ptr(val) {
        Ok(ptr) => ptr,
        Err(_) => return false,
    };
    let header_word = unsafe {
        // SAFETY: ptr points at a readable object header.
        ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
    };
    let header = ObjectHeader::from_word(header_word);
    matches!(
        header.class_id(),
        class if class == JSObjectClass::Closure as u8 || class == JSObjectClass::CFunction as u8
    )
}

fn object_proto_value(ptr: NonNull<Object>) -> JSValue {
    unsafe {
        // SAFETY: ptr points at a readable object proto.
        ptr::read_unaligned(Object::proto_ptr(ptr.as_ptr()))
    }
}

fn instanceof_check(obj: JSValue, proto: JSValue) -> Result<bool, InterpreterError> {
    if !obj.is_object() {
        return Ok(false);
    }
    let mut current = obj;
    loop {
        let obj_ptr = object_ptr(current)?;
        let next_proto = object_proto_value(obj_ptr);
        if next_proto == JSValue::JS_NULL {
            return Ok(false);
        }
        if next_proto == proto {
            return Ok(true);
        }
        current = next_proto;
    }
}

fn mtag_from_value(val: JSValue) -> Option<MTag> {
    let ptr = val.to_ptr::<u8>()?;
    let header_word = unsafe {
        // SAFETY: ptr points at a readable memblock header.
        ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
    };
    Some(MbHeader::from_word(header_word).tag())
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
            let arg = args.first().copied().unwrap_or(JSValue::JS_UNDEFINED);
            let num = conversion::to_number(ctx, arg)?;
            let out = func(num);
            Ok(ctx.new_float64(out)?)
        }
        BuiltinCFunction::Missing(_) => Err(InterpreterError::MissingCFunction(def.func_name)),
    }
}

fn map_property_error(err: PropertyError) -> InterpreterError {
    match err {
        PropertyError::OutOfMemory => InterpreterError::Context(ContextError::OutOfMemory),
        PropertyError::Interpreter(err) => err,
        _ => InterpreterError::TypeError("property"),
    }
}

fn take_exception_value(ctx: &mut JSContext) -> JSValue {
    let thrown = ctx.take_current_exception();
    if thrown == JSValue::JS_NULL {
        JSValue::JS_EXCEPTION
    } else {
        thrown
    }
}

fn throw_type_error_value(ctx: &mut JSContext, message: &str) -> JSValue {
    let _ = ctx.throw_type_error(message);
    take_exception_value(ctx)
}

fn throw_reference_error_value(ctx: &mut JSContext, message: &str) -> JSValue {
    let _ = ctx.throw_reference_error(message);
    take_exception_value(ctx)
}

fn property_error_to_exception(ctx: &mut JSContext, err: &PropertyError) -> Option<JSValue> {
    match err {
        PropertyError::OutOfMemory => {
            let _ = ctx.throw_out_of_memory();
            Some(take_exception_value(ctx))
        }
        PropertyError::Unsupported(_) | PropertyError::NotObject => {
            Some(throw_type_error_value(ctx, "property"))
        }
        PropertyError::Interpreter(interpreter_err) => match interpreter_err {
            InterpreterError::Thrown(val) => Some(*val),
            InterpreterError::TypeError(msg) => Some(throw_type_error_value(ctx, msg)),
            InterpreterError::ReferenceError(msg) => Some(throw_reference_error_value(ctx, msg)),
            InterpreterError::NotAFunction => Some(throw_type_error_value(ctx, "not a function")),
            InterpreterError::Context(ContextError::OutOfMemory) => {
                let _ = ctx.throw_out_of_memory();
                Some(take_exception_value(ctx))
            }
            _ => None,
        },
        _ => None,
    }
}

fn var_ref_ptr(val: JSValue) -> Result<NonNull<u8>, InterpreterError> {
    let ptr = val.to_ptr::<u8>().ok_or(InterpreterError::InvalidValue("varref ptr"))?;
    let header_word = unsafe {
        // SAFETY: ptr points at a readable memblock header.
        ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
    };
    if MbHeader::from_word(header_word).tag() != MTag::VarRef {
        return Err(InterpreterError::InvalidValue("varref tag"));
    }
    Ok(ptr)
}

unsafe fn var_ref_value_ptr(ptr: NonNull<u8>) -> *mut JSValue {
    unsafe { ptr.as_ptr().add(size_of::<JSWord>()) as *mut JSValue }
}

unsafe fn var_ref_next(ptr: NonNull<u8>) -> JSValue {
    unsafe { ptr::read_unaligned(var_ref_value_ptr(ptr)) }
}

unsafe fn var_ref_pvalue_ptr(ptr: NonNull<u8>) -> *mut *mut JSValue {
    unsafe { ptr.as_ptr().add(size_of::<JSWord>() + size_of::<JSValue>()) as *mut *mut JSValue }
}

unsafe fn var_ref_pvalue(ptr: NonNull<u8>) -> *mut JSValue {
    unsafe { ptr::read_unaligned(var_ref_pvalue_ptr(ptr)) }
}

fn var_ref_is_detached(ptr: NonNull<u8>) -> bool {
    let header_word = unsafe {
        // SAFETY: ptr points at a readable memblock header.
        ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
    };
    let header = VarRefHeader::from(MbHeader::from_word(header_word));
    header.is_detached()
}

fn var_ref_get(val: JSValue) -> Result<JSValue, InterpreterError> {
    let ptr = var_ref_ptr(val)?;
    if var_ref_is_detached(ptr) {
        let value = unsafe {
            // SAFETY: detached varref stores the value at the payload slot.
            ptr::read_unaligned(var_ref_value_ptr(ptr))
        };
        return Ok(value);
    }
    let pvalue = unsafe { var_ref_pvalue(ptr) };
    if pvalue.is_null() {
        return Err(InterpreterError::InvalidValue("varref pvalue"));
    }
    let value = unsafe {
        // SAFETY: pvalue points at a live stack slot.
        ptr::read_unaligned(pvalue)
    };
    Ok(value)
}

fn var_ref_set(val: JSValue, new_value: JSValue) -> Result<(), InterpreterError> {
    let ptr = var_ref_ptr(val)?;
    if var_ref_is_detached(ptr) {
        unsafe {
            // SAFETY: detached varref stores the value at the payload slot.
            ptr::write_unaligned(var_ref_value_ptr(ptr), new_value);
        }
        return Ok(());
    }
    let pvalue = unsafe { var_ref_pvalue(ptr) };
    if pvalue.is_null() {
        return Err(InterpreterError::InvalidValue("varref pvalue"));
    }
    unsafe {
        // SAFETY: pvalue points at a writable stack slot.
        ptr::write_unaligned(pvalue, new_value);
    }
    Ok(())
}

fn alloc_var_ref_attached(
    ctx: &mut JSContext,
    next: JSValue,
    pvalue: *mut JSValue,
) -> Result<JSValue, InterpreterError> {
    let size = size_of::<JSWord>() + size_of::<JSValue>() + size_of::<*mut JSValue>();
    let ptr = ctx.alloc_mblock(size, MTag::VarRef)?;
    let header = VarRefHeader::new(false, false);
    unsafe {
        // SAFETY: ptr points at a writable var ref allocation.
        ptr::write_unaligned(ptr.as_ptr().cast::<JSWord>(), MbHeader::from(header).word());
        ptr::write_unaligned(var_ref_value_ptr(ptr), next);
        ptr::write_unaligned(var_ref_pvalue_ptr(ptr), pvalue);
    }
    Ok(JSValue::from_ptr(ptr))
}

fn get_var_ref(
    ctx: &mut JSContext,
    first_var_ref: *mut JSValue,
    pvalue: *mut JSValue,
) -> Result<JSValue, InterpreterError> {
    let mut val = unsafe {
        // SAFETY: caller ensures first_var_ref is a readable slot.
        ptr::read_unaligned(first_var_ref)
    };
    while val != JSValue::JS_NULL {
        let ptr = var_ref_ptr(val)?;
        if var_ref_is_detached(ptr) {
            return Err(InterpreterError::InvalidValue("detached varref in list"));
        }
        let cur_pvalue = unsafe { var_ref_pvalue(ptr) };
        if cur_pvalue == pvalue {
            return Ok(val);
        }
        val = unsafe { var_ref_next(ptr) };
    }
    let new_ref = alloc_var_ref_attached(ctx, val, pvalue)?;
    unsafe {
        // SAFETY: caller ensures first_var_ref is writable.
        ptr::write_unaligned(first_var_ref, new_ref);
    }
    Ok(new_ref)
}

fn detach_var_refs(first_var_ref: JSValue) -> Result<(), InterpreterError> {
    let mut current = first_var_ref;
    while current != JSValue::JS_NULL {
        let ptr = var_ref_ptr(current)?;
        if var_ref_is_detached(ptr) {
            return Err(InterpreterError::InvalidValue("varref already detached"));
        }
        let next = unsafe { var_ref_next(ptr) };
        let pvalue = unsafe { var_ref_pvalue(ptr) };
        if pvalue.is_null() {
            return Err(InterpreterError::InvalidValue("varref pvalue"));
        }
        let value = unsafe {
            // SAFETY: pvalue points at a readable stack slot.
            ptr::read_unaligned(pvalue)
        };
        unsafe {
            // SAFETY: payload slot is writable.
            ptr::write_unaligned(var_ref_value_ptr(ptr), value);
            let header_word = ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>());
            let detached_word = header_word | ((1 as JSWord) << JS_MTAG_BITS);
            ptr::write_unaligned(ptr.as_ptr().cast::<JSWord>(), detached_word);
            let shrink_ptr = NonNull::new_unchecked(
                ptr.as_ptr()
                    .add(size_of::<JSWord>() + size_of::<JSValue>()),
            );
            set_free_block(shrink_ptr, size_of::<JSValue>());
        }
        current = next;
    }
    Ok(())
}

fn add_global_var(
    ctx: &mut JSContext,
    name: JSValue,
    define_flag: bool,
) -> Result<JSValue, InterpreterError> {
    let global_obj = ctx.global_obj();
    if let Some((value, prop_type)) =
        get_own_property_raw(ctx, global_obj, name).map_err(map_property_error)?
    {
        if prop_type != JSPropType::VarRef {
            return Err(InterpreterError::ReferenceError("global var must be a reference"));
        }
        if define_flag {
            let current = var_ref_get(value)?;
            if current.is_uninitialized() {
                var_ref_set(value, JSValue::JS_UNDEFINED)?;
            }
        }
        return Ok(value);
    }
    let init = if define_flag { JSValue::JS_UNDEFINED } else { JSValue::JS_UNINITIALIZED };
    define_property_varref(ctx, global_obj, name, init).map_err(map_property_error)
}

pub(crate) fn create_closure(
    ctx: &mut JSContext,
    bfunc: JSValue,
    fp: Option<*mut JSValue>,
) -> Result<JSValue, InterpreterError> {
    let func_ptr = function_bytecode_ptr(bfunc)?;
    let b = unsafe {
        // SAFETY: func_ptr points at a readable FunctionBytecode.
        func_ptr.as_ref()
    };
    let ext_len = b.ext_vars_len() as usize;
    let closure = ctx.alloc_closure(bfunc, ext_len)?;
    if ext_len == 0 {
        return Ok(closure);
    }
    let obj_ptr = object_ptr(closure)?;
    unsafe {
        // SAFETY: closure points at a valid closure payload.
        let payload = Object::payload_ptr(obj_ptr.as_ptr());
        let closure_data = core::ptr::addr_of_mut!((*payload).closure);
        let var_refs = crate::closure_data::ClosureData::var_refs_ptr(closure_data);
        for idx in 0..ext_len {
            ptr::write_unaligned(var_refs.add(idx), JSValue::JS_NULL);
        }
    }
    let ext_vars_val = b.ext_vars();
    if ext_vars_val == JSValue::JS_NULL {
        return Err(InterpreterError::InvalidValue("ext vars"));
    }
    let ext_vars = unsafe { ValueArrayRaw::from_value(ext_vars_val)? };
    let first_var_ref_ptr = fp.map(|fp| unsafe { fp.offset(FRAME_OFFSET_FIRST_VARREF) });
    for idx in 0..ext_len {
        let name = unsafe { ext_vars.read(2 * idx)? };
        let decl_val = unsafe { ext_vars.read(2 * idx + 1)? };
        if !decl_val.is_int() {
            return Err(InterpreterError::InvalidValue("ext var decl"));
        }
        let decl = decl_val.get_int();
        let kind = decl >> 16;
        let var_idx = (decl & 0xffff) as usize;
        let var_ref = match kind {
            x if x == JSVarRefKind::Arg as i32 => {
                let fp = fp.ok_or(InterpreterError::InvalidValue("missing frame"))?;
                let pvalue = unsafe { fp.add(FRAME_OFFSET_ARG0 as usize + var_idx) };
                let first_ptr = first_var_ref_ptr.ok_or(InterpreterError::InvalidValue("varref list"))?;
                get_var_ref(ctx, first_ptr, pvalue)?
            }
            x if x == JSVarRefKind::Var as i32 => {
                let fp = fp.ok_or(InterpreterError::InvalidValue("missing frame"))?;
                let pvalue = unsafe { fp.offset(FRAME_OFFSET_VAR0 - var_idx as isize) };
                let first_ptr = first_var_ref_ptr.ok_or(InterpreterError::InvalidValue("varref list"))?;
                get_var_ref(ctx, first_ptr, pvalue)?
            }
            x if x == JSVarRefKind::VarRef as i32 => {
                let fp = fp.ok_or(InterpreterError::InvalidValue("missing frame"))?;
                let func_obj = unsafe {
                    // SAFETY: fp points at a readable frame slot.
                    ptr::read_unaligned(fp.offset(FRAME_OFFSET_FUNC_OBJ))
                };
                let obj_ptr = object_ptr(func_obj)?;
                let header = object_header(obj_ptr);
                if header.class_id() != JSObjectClass::Closure as u8 {
                    return Err(InterpreterError::InvalidValue("varref closure"));
                }
                let extra = header.extra_size() as usize;
                if var_idx >= extra.saturating_sub(1) {
                    return Err(InterpreterError::InvalidBytecode("varref index"));
                }
                unsafe {
                    // SAFETY: closure var_refs contains extra_size - 1 entries.
                    let payload = Object::payload_ptr(obj_ptr.as_ptr());
                    let closure_data = core::ptr::addr_of_mut!((*payload).closure);
                    let var_refs = crate::closure_data::ClosureData::var_refs_ptr(closure_data);
                    ptr::read_unaligned(var_refs.add(var_idx))
                }
            }
            x if x == JSVarRefKind::Global as i32 => add_global_var(ctx, name, var_idx != 0)?,
            _ => return Err(InterpreterError::InvalidValue("ext var kind")),
        };
        unsafe {
            // SAFETY: closure var_refs contains ext_len entries.
            let payload = Object::payload_ptr(obj_ptr.as_ptr());
            let closure_data = core::ptr::addr_of_mut!((*payload).closure);
            let var_refs = crate::closure_data::ClosureData::var_refs_ptr(closure_data);
            ptr::write_unaligned(var_refs.add(idx), var_ref);
        }
    }
    Ok(closure)
}

fn call_constructor_start(
    ctx: &mut JSContext,
    func_slot: *mut JSValue,
) -> Result<JSValue, InterpreterError> {
    let proto_key = ctx.intern_string(b"prototype")?;
    let func = unsafe {
        // SAFETY: func_slot points at a live stack slot for the current frame.
        ptr::read_unaligned(func_slot)
    };
    let proto = match crate::property::get_property(ctx, func, proto_key) {
        Ok(val) => val,
        Err(PropertyError::Unsupported(_)) => JSValue::JS_UNDEFINED,
        Err(err) => return Err(map_property_error(err)),
    };
    let proto = if proto.is_object() {
        proto
    } else {
        ctx.class_proto()[JSObjectClass::Object as usize]
    };
    Ok(ctx.alloc_object(JSObjectClass::Object, proto, 0)?)
}

enum ReturnFlow {
    Done(JSValue),
    Continue { exception: bool },
}

struct FrameState<'a, 'b> {
    stack_top: *mut JSValue,
    initial_fp: *mut JSValue,
    sp: &'a mut *mut JSValue,
    fp: &'a mut *mut JSValue,
    pc: &'a mut usize,
    func_ptr: &'a mut NonNull<FunctionBytecode>,
    byte_slice: &'a mut &'b [u8],
    n_vars: &'a mut usize,
}

fn compute_n_vars(b: &FunctionBytecode) -> Result<usize, InterpreterError> {
    if b.vars() != JSValue::JS_NULL {
        let vars = unsafe { ValueArrayRaw::from_value(b.vars())? };
        Ok(vars.len.saturating_sub(b.arg_count() as usize))
    } else {
        Ok(0)
    }
}

fn return_from_frame(
    ctx: &mut JSContext,
    state: &mut FrameState<'_, '_>,
    mut val: JSValue,
    frame_argc: usize,
    has_varrefs: bool,
) -> Result<ReturnFlow, InterpreterError> {
    if has_varrefs {
        let first_var_ref =
            unsafe { ptr::read_unaligned((*state.fp).offset(FRAME_OFFSET_FIRST_VARREF)) };
        detach_var_refs(first_var_ref)?;
    }
    let call_flags_val =
        unsafe { ptr::read_unaligned((*state.fp).offset(FRAME_OFFSET_CALL_FLAGS)) };
    if !call_flags_val.is_int() {
        return Err(InterpreterError::InvalidValue("call flags"));
    }
    let call_flags = call_flags_val.get_int();
    if (call_flags & FRAME_CF_CTOR) != 0 && !val.is_exception() && !val.is_object() {
        val = unsafe { ptr::read_unaligned((*state.fp).offset(FRAME_OFFSET_THIS_OBJ)) };
    }
    *state.sp = unsafe { (*state.fp).add(FRAME_OFFSET_ARG0 as usize + frame_argc) };
    let saved_fp_val =
        unsafe { ptr::read_unaligned((*state.fp).offset(FRAME_OFFSET_SAVED_FP)) };
    let caller_fp = decode_stack_ptr(state.stack_top, saved_fp_val)?;
    if caller_fp == state.initial_fp {
        if val.is_exception() {
            let thrown = ctx.take_current_exception();
            let thrown = if thrown == JSValue::JS_NULL { JSValue::JS_EXCEPTION } else { thrown };
            return Err(InterpreterError::Thrown(thrown));
        }
        return Ok(ReturnFlow::Done(val));
    }
    *state.fp = caller_fp;
    ctx.set_fp(NonNull::new(*state.fp).expect("frame pointer"));
    ctx.set_sp(NonNull::new(*state.sp).expect("stack pointer"));
    let pc_offset_val =
        unsafe { ptr::read_unaligned((*state.fp).offset(FRAME_OFFSET_CUR_PC)) };
    if !pc_offset_val.is_int() {
        return Err(InterpreterError::InvalidValue("cur pc"));
    }
    let pc_offset = pc_offset_val.get_int();
    if pc_offset < 0 {
        return Err(InterpreterError::InvalidValue("cur pc"));
    }
    let func_obj = unsafe { ptr::read_unaligned((*state.fp).offset(FRAME_OFFSET_FUNC_OBJ)) };
    let func_obj_ptr = object_ptr(func_obj)?;
    let payload = unsafe { Object::payload_ptr(func_obj_ptr.as_ptr()) };
    let closure = unsafe { core::ptr::addr_of_mut!((*payload).closure) };
    let func_val = unsafe {
        ptr::read_unaligned(crate::closure_data::ClosureData::func_bytecode_ptr(closure))
    };
    *state.func_ptr = function_bytecode_ptr(func_val)?;
    let b = unsafe { state.func_ptr.as_ref() };
    let byte_code_val = b.byte_code();
    if byte_code_val == JSValue::JS_NULL {
        return Err(InterpreterError::InvalidBytecode("missing bytecode"));
    }
    let byte_code = unsafe { ByteArrayRaw::from_value(byte_code_val)? };
    if pc_offset as usize >= byte_code.len() {
        return Err(InterpreterError::InvalidBytecode("cur pc"));
    }
    *state.byte_slice =
        unsafe { core::slice::from_raw_parts(byte_code.buf().as_ptr(), byte_code.len()) };
    *state.n_vars = compute_n_vars(b)?;
    *state.pc = pc_offset as usize;
    if val.is_exception() {
        return Ok(ReturnFlow::Continue { exception: true });
    }
    if (call_flags & FRAME_CF_POP_RET) == 0 {
        unsafe {
            push(ctx, state.sp, val);
        }
    }
    if (call_flags & FRAME_CF_PC_ADD1) == 0 {
        *state.pc += 2;
    } else {
        *state.pc += 1;
    }
    Ok(ReturnFlow::Continue { exception: false })
}

fn unwind_exception(
    ctx: &mut JSContext,
    state: &mut FrameState<'_, '_>,
    exception_val: JSValue,
) -> Result<ReturnFlow, InterpreterError> {
    let mut exception_val = exception_val;
    loop {
        let handled = handle_exception(
            ctx,
            state.byte_slice.len(),
            *state.fp,
            *state.n_vars,
            state.sp,
            exception_val,
        )?;
        if let Some(target) = handled {
            *state.pc = target;
            return Ok(ReturnFlow::Continue { exception: false });
        }
        let b = unsafe { state.func_ptr.as_ref() };
        let call_flags_val =
            unsafe { ptr::read_unaligned((*state.fp).offset(FRAME_OFFSET_CALL_FLAGS)) };
        if !call_flags_val.is_int() {
            return Err(InterpreterError::InvalidValue("call flags"));
        }
        let argc = call_argc(call_flags_val.get_int());
        let frame_argc = argc.max(b.arg_count() as usize);
        match return_from_frame(ctx, state, JSValue::JS_EXCEPTION, frame_argc, true)? {
            ReturnFlow::Done(_) => {
                let thrown = ctx.take_current_exception();
                return Err(InterpreterError::Thrown(thrown));
            }
            ReturnFlow::Continue { exception } => {
                if !exception {
                    return Ok(ReturnFlow::Continue { exception: false });
                }
            }
        }
        exception_val = ctx.take_current_exception();
        ctx.set_current_exception(exception_val);
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
    call_with_this_flags(ctx, func, this_obj, args, 0)
}

pub fn call_with_this_flags(
    ctx: &mut JSContext,
    func: JSValue,
    this_obj: JSValue,
    args: &[JSValue],
    call_flags: i32,
) -> Result<JSValue, InterpreterError> {
    let call_flags =
        (call_flags & !FRAME_CF_ARGC_MASK) | ((args.len() as i32) & FRAME_CF_ARGC_MASK);
    if let Some(idx) = short_func_index(func) {
        let def = *ctx
            .c_function(idx as usize)
            .ok_or(InterpreterError::InvalidCFunctionIndex(idx))?;
        let is_ctor = matches!(
            def.func,
            BuiltinCFunction::Constructor(_) | BuiltinCFunction::ConstructorMagic(_)
        );
        if (call_flags & FRAME_CF_CTOR) != 0 && !is_ctor {
            return Err(InterpreterError::TypeError("not a constructor"));
        }
        let mut sp = ctx.sp().as_ptr();
        let prev_sp = ctx.sp();
        unsafe {
            for arg in args.iter().rev() {
                push(ctx, &mut sp, *arg);
            }
            push(ctx, &mut sp, func);
            push(ctx, &mut sp, this_obj);
        }
        let result = call_cfunction_def(ctx, &def, this_obj, args, JSValue::JS_UNDEFINED);
        ctx.set_sp(prev_sp);
        if let Ok(val) = result
            && (call_flags & FRAME_CF_CTOR) != 0
            && !val.is_exception()
            && !val.is_object()
        {
            return Ok(this_obj);
        }
        return result;
    }
    if !func.is_ptr() {
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
            let mut sp = ctx.sp().as_ptr();
            let prev_sp = ctx.sp();
            unsafe {
                for arg in args.iter().rev() {
                    push(ctx, &mut sp, *arg);
                }
                push(ctx, &mut sp, func);
                push(ctx, &mut sp, this_obj);
            }
            let result = call_cfunction_def(ctx, &def, this_obj, args, params);
            ctx.set_sp(prev_sp);
            return result;
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
    let mut func_ptr = function_bytecode_ptr(func_bytecode)?;
    let mut b = unsafe {
        // SAFETY: func_ptr points at a readable function bytecode.
        func_ptr.as_ref()
    };
    let byte_code_val = b.byte_code();
    if byte_code_val == JSValue::JS_NULL {
        return Err(InterpreterError::InvalidBytecode("missing bytecode"));
    }
    let byte_code = unsafe { ByteArrayRaw::from_value(byte_code_val)? };
    let mut pc = 0usize;
    let mut byte_slice = unsafe {
        // SAFETY: byte_code.buf is valid for byte_code.len bytes.
        core::slice::from_raw_parts(byte_code.buf().as_ptr(), byte_code.len())
    };
    let prev_sp = ctx.sp();
    let prev_fp = ctx.fp();
    let initial_fp = prev_fp.as_ptr();

    let arg_count = b.arg_count() as usize;
    let max_args = args.len().max(arg_count);
    let mut n_vars = compute_n_vars(b)?;
    let slots_below = n_vars + 2;
    let slots_above = FRAME_OFFSET_ARG0 as usize + max_args;
    let total_slots = slots_below + slots_above;
    let stack_top = ctx.stack_top().as_ptr() as *mut JSValue;
    let base_sp = prev_sp.as_ptr();
    let new_sp = unsafe { base_sp.sub(total_slots) };
    stack_check(ctx, base_sp, total_slots)?;
    let mut sp = new_sp;
    let mut fp = unsafe { sp.add(slots_below) };
    let saved_fp = encode_stack_ptr(stack_top, prev_fp.as_ptr())?;

    unsafe {
        // SAFETY: new_sp..stack_top covers total_slots writable slots.
        for idx in 0..n_vars {
            ptr::write_unaligned(sp.add(idx), JSValue::JS_UNDEFINED);
        }
        ptr::write_unaligned(sp.add(n_vars), JSValue::JS_NULL);
        ptr::write_unaligned(sp.add(n_vars + 1), JSValue::new_short_int(0));
        ptr::write_unaligned(fp.offset(FRAME_OFFSET_SAVED_FP), saved_fp);
        ptr::write_unaligned(fp.offset(FRAME_OFFSET_CALL_FLAGS), JSValue::new_short_int(call_flags));
        ptr::write_unaligned(fp.offset(FRAME_OFFSET_THIS_OBJ), this_obj);
        ptr::write_unaligned(fp.offset(FRAME_OFFSET_FUNC_OBJ), func);
        for idx in 0..max_args {
            let arg = args.get(idx).copied().unwrap_or(JSValue::JS_UNDEFINED);
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

    macro_rules! handle_call {
        ($call_flags:expr) => {{
            let call_flags = $call_flags;
            let pc_offset =
                i32::try_from(pc).map_err(|_| InterpreterError::InvalidBytecode("call"))?;
            unsafe {
                ptr::write_unaligned(fp.offset(FRAME_OFFSET_CUR_PC), JSValue::new_short_int(pc_offset));
            }
            let saved_fp = try_or_break!(encode_stack_ptr(stack_top, fp));
            unsafe {
                sp = sp.sub(1);
                ptr::write_unaligned(sp, JSValue::new_short_int(call_flags));
                sp = sp.sub(1);
                ptr::write_unaligned(sp, saved_fp);
            }
            ctx.set_sp(NonNull::new(sp).expect("stack pointer"));
            fp = sp;
            ctx.set_fp(NonNull::new(fp).expect("frame pointer"));

            let func_obj = unsafe { ptr::read_unaligned(fp.offset(FRAME_OFFSET_FUNC_OBJ)) };
            let mut cfunc_def: Option<CFunctionDef> = None;
            let mut cfunc_params = JSValue::JS_UNDEFINED;
            let mut closure_ptr: Option<NonNull<Object>> = None;

            if !func_obj.is_ptr() {
                if func_obj.get_special_tag() != JSValue::JS_TAG_SHORT_FUNC {
                    break Err(InterpreterError::NotAFunction);
                }
                let idx = func_obj.get_special_value();
                if idx < 0 {
                    break Err(InterpreterError::InvalidCFunctionIndex(idx as u32));
                }
                let def = *ctx
                    .c_function(idx as usize)
                    .ok_or(InterpreterError::InvalidCFunctionIndex(idx as u32))?;
                cfunc_def = Some(def);
            } else {
                let obj_ptr = match object_ptr(func_obj) {
                    Ok(ptr) => ptr,
                    Err(_) => break Err(InterpreterError::NotAFunction),
                };
                let header = object_header(obj_ptr);
                if header.class_id() == JSObjectClass::CFunction as u8 {
                    let (idx, params) = try_or_break!(cfunction_data(obj_ptr, header));
                    let def = *ctx
                        .c_function(idx as usize)
                        .ok_or(InterpreterError::InvalidCFunctionIndex(idx))?;
                    cfunc_def = Some(def);
                    cfunc_params = params;
                } else if header.class_id() == JSObjectClass::Closure as u8 {
                    closure_ptr = Some(obj_ptr);
                } else {
                    break Err(InterpreterError::NotAFunction);
                }
            }

            if let Some(def) = cfunc_def {
                let is_ctor = matches!(
                    def.func,
                    BuiltinCFunction::Constructor(_) | BuiltinCFunction::ConstructorMagic(_)
                );
                if (call_flags & FRAME_CF_CTOR) != 0 && !is_ctor {
                    break Err(InterpreterError::TypeError("not a constructor"));
                }
                let argc = call_argc(call_flags);
                let pushed_argc = argc;
                let this_val = unsafe { ptr::read_unaligned(fp.offset(FRAME_OFFSET_THIS_OBJ)) };
                let argv_ptr = unsafe { fp.add(FRAME_OFFSET_ARG0 as usize) };
                let argv = unsafe { core::slice::from_raw_parts(argv_ptr, argc) };
                let val = try_or_break!(call_cfunction_def(ctx, &def, this_val, argv, cfunc_params));
                let flow = {
                    let mut frame_state = FrameState {
                        stack_top,
                        initial_fp,
                        sp: &mut sp,
                        fp: &mut fp,
                        pc: &mut pc,
                        func_ptr: &mut func_ptr,
                        byte_slice: &mut byte_slice,
                        n_vars: &mut n_vars,
                    };
                    if val.is_exception() {
                        try_or_break!(return_from_frame(
                            ctx,
                            &mut frame_state,
                            JSValue::JS_EXCEPTION,
                            pushed_argc,
                            false
                        ))
                    } else {
                        try_or_break!(return_from_frame(
                            ctx,
                            &mut frame_state,
                            val,
                            pushed_argc,
                            false
                        ))
                    }
                };
                match flow {
                    ReturnFlow::Done(val) => break Ok(val),
                    ReturnFlow::Continue { exception: false } => {
                        b = unsafe { func_ptr.as_ref() };
                    }
                    ReturnFlow::Continue { exception: true } => {
                        let thrown = ctx.take_current_exception();
                        let thrown = if thrown == JSValue::JS_NULL { JSValue::JS_EXCEPTION } else { thrown };
                        let flow = {
                            let mut frame_state = FrameState {
                                stack_top,
                                initial_fp,
                                sp: &mut sp,
                                fp: &mut fp,
                                pc: &mut pc,
                                func_ptr: &mut func_ptr,
                                byte_slice: &mut byte_slice,
                                n_vars: &mut n_vars,
                            };
                            try_or_break!(unwind_exception(ctx, &mut frame_state, thrown))
                        };
                        if let ReturnFlow::Done(val) = flow {
                            break Ok(val);
                        }
                        b = unsafe { func_ptr.as_ref() };
                    }
                }
            } else {
                let obj_ptr = closure_ptr.expect("closure");
                if (call_flags & FRAME_CF_CTOR) != 0 {
                    let func_slot = unsafe { fp.offset(FRAME_OFFSET_FUNC_OBJ) };
                    let this_val = try_or_break!(call_constructor_start(ctx, func_slot));
                    unsafe {
                        ptr::write_unaligned(fp.offset(FRAME_OFFSET_THIS_OBJ), this_val);
                    }
                }
                let payload = unsafe { Object::payload_ptr(obj_ptr.as_ptr()) };
                let closure = unsafe { core::ptr::addr_of_mut!((*payload).closure) };
                let func_val = unsafe {
                    ptr::read_unaligned(crate::closure_data::ClosureData::func_bytecode_ptr(closure))
                };
                func_ptr = try_or_break!(function_bytecode_ptr(func_val));
                b = unsafe { func_ptr.as_ref() };
                let argc = call_argc(call_flags);
                let arg_count = b.arg_count() as usize;
                let new_n_vars = try_or_break!(compute_n_vars(b));
                let extra = arg_count.saturating_sub(argc);
                let stack_needed = extra + 2 + new_n_vars + b.stack_size() as usize;
                try_or_break!(stack_check(ctx, sp, stack_needed));
                if extra > 0 {
                    unsafe {
                        sp = sp.sub(extra);
                        for i in 0..(FRAME_OFFSET_ARG0 as usize + argc) {
                            let val = ptr::read_unaligned(sp.add(i + extra));
                            ptr::write_unaligned(sp.add(i), val);
                        }
                        for i in 0..extra {
                            ptr::write_unaligned(
                                sp.add(FRAME_OFFSET_ARG0 as usize + argc + i),
                                JSValue::JS_UNDEFINED,
                            );
                        }
                    }
                    ctx.set_sp(NonNull::new(sp).expect("stack pointer"));
                    fp = sp;
                    ctx.set_fp(NonNull::new(fp).expect("frame pointer"));
                }
                unsafe {
                    sp = sp.sub(1);
                    ptr::write_unaligned(sp, JSValue::new_short_int(0));
                    sp = sp.sub(1);
                    ptr::write_unaligned(sp, JSValue::JS_NULL);
                    sp = sp.sub(new_n_vars);
                    for i in 0..new_n_vars {
                        ptr::write_unaligned(sp.add(i), JSValue::JS_UNDEFINED);
                    }
                }
                ctx.set_sp(NonNull::new(sp).expect("stack pointer"));
                let byte_code_val = b.byte_code();
                if byte_code_val == JSValue::JS_NULL {
                    break Err(InterpreterError::InvalidBytecode("missing bytecode"));
                }
                let byte_code = unsafe { ByteArrayRaw::from_value(byte_code_val) };
                let byte_code = try_or_break!(byte_code);
                byte_slice = unsafe {
                    core::slice::from_raw_parts(byte_code.buf().as_ptr(), byte_code.len())
                };
                n_vars = new_n_vars;
                pc = 0;
            }
        }};
    }

    let result = loop {
        if pc >= byte_slice.len() {
            break Err(InterpreterError::InvalidBytecode("pc out of bounds"));
        }
        let opcode = byte_slice[pc];
        pc += 1;
        if let Ok(pc_offset) = i32::try_from(pc) {
            unsafe {
                // SAFETY: fp points at the current frame header.
                ptr::write_unaligned(fp.offset(FRAME_OFFSET_CUR_PC), JSValue::new_short_int(pc_offset));
            }
        }
        match opcode {
            op if op == crate::opcode::OP_RETURN.as_u8() => unsafe {
                let val = ptr::read_unaligned(sp);
                let call_flags_val = ptr::read_unaligned(fp.offset(FRAME_OFFSET_CALL_FLAGS));
                if !call_flags_val.is_int() {
                    break Err(InterpreterError::InvalidValue("call flags"));
                }
                let argc = call_argc(call_flags_val.get_int());
                let frame_argc = argc.max(b.arg_count() as usize);
                let flow = {
                    let mut frame_state = FrameState {
                        stack_top,
                        initial_fp,
                        sp: &mut sp,
                        fp: &mut fp,
                        pc: &mut pc,
                        func_ptr: &mut func_ptr,
                        byte_slice: &mut byte_slice,
                        n_vars: &mut n_vars,
                    };
                    try_or_break!(return_from_frame(
                        ctx,
                        &mut frame_state,
                        val,
                        frame_argc,
                        true
                    ))
                };
                match flow {
                    ReturnFlow::Done(val) => break Ok(val),
                    ReturnFlow::Continue { exception: false } => {
                        b = func_ptr.as_ref();
                    }
                    ReturnFlow::Continue { exception: true } => {
                        let thrown = ctx.take_current_exception();
                        let thrown = if thrown == JSValue::JS_NULL { JSValue::JS_EXCEPTION } else { thrown };
                        let flow = {
                            let mut frame_state = FrameState {
                                stack_top,
                                initial_fp,
                                sp: &mut sp,
                                fp: &mut fp,
                                pc: &mut pc,
                                func_ptr: &mut func_ptr,
                                byte_slice: &mut byte_slice,
                                n_vars: &mut n_vars,
                            };
                            try_or_break!(unwind_exception(ctx, &mut frame_state, thrown))
                        };
                        if let ReturnFlow::Done(val) = flow {
                            break Ok(val);
                        }
                        b = func_ptr.as_ref();
                    }
                }
            },
            op if op == crate::opcode::OP_RETURN_UNDEF.as_u8() => {
                let call_flags_val = unsafe { ptr::read_unaligned(fp.offset(FRAME_OFFSET_CALL_FLAGS)) };
                if !call_flags_val.is_int() {
                    break Err(InterpreterError::InvalidValue("call flags"));
                }
                let argc = call_argc(call_flags_val.get_int());
                let frame_argc = argc.max(b.arg_count() as usize);
                let flow = {
                    let mut frame_state = FrameState {
                        stack_top,
                        initial_fp,
                        sp: &mut sp,
                        fp: &mut fp,
                        pc: &mut pc,
                        func_ptr: &mut func_ptr,
                        byte_slice: &mut byte_slice,
                        n_vars: &mut n_vars,
                    };
                    try_or_break!(return_from_frame(
                        ctx,
                        &mut frame_state,
                        JSValue::JS_UNDEFINED,
                        frame_argc,
                        true
                    ))
                };
                match flow {
                    ReturnFlow::Done(val) => break Ok(val),
                    ReturnFlow::Continue { exception: false } => {
                        b = unsafe { func_ptr.as_ref() };
                    }
                    ReturnFlow::Continue { exception: true } => {
                        let thrown = ctx.take_current_exception();
                        let thrown = if thrown == JSValue::JS_NULL { JSValue::JS_EXCEPTION } else { thrown };
                        let flow = {
                            let mut frame_state = FrameState {
                                stack_top,
                                initial_fp,
                                sp: &mut sp,
                                fp: &mut fp,
                                pc: &mut pc,
                                func_ptr: &mut func_ptr,
                                byte_slice: &mut byte_slice,
                                n_vars: &mut n_vars,
                            };
                            try_or_break!(unwind_exception(ctx, &mut frame_state, thrown))
                        };
                        if let ReturnFlow::Done(val) = flow {
                            break Ok(val);
                        }
                        b = unsafe { func_ptr.as_ref() };
                    }
                }
            },
            op if op == crate::opcode::OP_THROW.as_u8() => unsafe {
                let val = pop(ctx, &mut sp);
                let flow = {
                    let mut frame_state = FrameState {
                        stack_top,
                        initial_fp,
                        sp: &mut sp,
                        fp: &mut fp,
                        pc: &mut pc,
                        func_ptr: &mut func_ptr,
                        byte_slice: &mut byte_slice,
                        n_vars: &mut n_vars,
                    };
                    try_or_break!(unwind_exception(ctx, &mut frame_state, val))
                };
                if let ReturnFlow::Done(val) = flow {
                    break Ok(val);
                }
                b = func_ptr.as_ref();
            },
            op if op == crate::opcode::OP_CATCH.as_u8() => unsafe {
                let diff = try_or_break!(read_i32(byte_slice, pc, "catch"));
                let target = try_or_break!(pc_with_offset(pc, diff, byte_slice.len(), "catch"));
                let offset = u32::try_from(target)
                    .map_err(|_| InterpreterError::InvalidBytecode("catch"))?;
                push(ctx, &mut sp, JSValue::value_make_special(JSValue::JS_TAG_CATCH_OFFSET, offset));
                pc += 4;
            },
            op if op == crate::opcode::OP_GOSUB.as_u8() => unsafe {
                let diff = try_or_break!(read_i32(byte_slice, pc, "gosub"));
                let return_pc = pc + 4;
                let return_pc =
                    i32::try_from(return_pc).map_err(|_| InterpreterError::InvalidBytecode("gosub"))?;
                push(ctx, &mut sp, JSValue::new_short_int(return_pc));
                pc = try_or_break!(pc_with_offset(pc, diff, byte_slice.len(), "gosub"));
            },
            op if op == crate::opcode::OP_RET.as_u8() => unsafe {
                let val = ptr::read_unaligned(sp);
                if !val.is_int() {
                    break Err(InterpreterError::InvalidBytecode("ret"));
                }
                let pos = val.get_int();
                if pos < 0 || (pos as usize) >= byte_slice.len() {
                    break Err(InterpreterError::InvalidBytecode("ret"));
                }
                let _ = pop(ctx, &mut sp);
                pc = pos as usize;
            },
            op if op == crate::opcode::OP_GOTO.as_u8() => {
                let diff = try_or_break!(read_i32(byte_slice, pc, "goto"));
                pc = try_or_break!(pc_with_offset(pc, diff, byte_slice.len(), "goto"));
            }
            op if op == crate::opcode::OP_IF_FALSE.as_u8() => unsafe {
                let diff = try_or_break!(read_i32(byte_slice, pc, "if_false"));
                let target = try_or_break!(pc_with_offset(pc, diff, byte_slice.len(), "if_false"));
                pc += 4;
                let cond = try_or_break!(to_bool(pop(ctx, &mut sp)));
                if !cond {
                    pc = target;
                }
            },
            op if op == crate::opcode::OP_IF_TRUE.as_u8() => unsafe {
                let diff = try_or_break!(read_i32(byte_slice, pc, "if_true"));
                let target = try_or_break!(pc_with_offset(pc, diff, byte_slice.len(), "if_true"));
                pc += 4;
                let cond = try_or_break!(to_bool(pop(ctx, &mut sp)));
                if cond {
                    pc = target;
                }
            },
            op if op == crate::opcode::OP_DROP.as_u8() => unsafe {
                let _ = pop(ctx, &mut sp);
            },
            op if op == crate::opcode::OP_NIP.as_u8() => unsafe {
                let val = ptr::read_unaligned(sp);
                ptr::write_unaligned(sp.add(1), val);
                let _ = pop(ctx, &mut sp);
            },
            op if op == crate::opcode::OP_DUP.as_u8() => unsafe {
                let val = ptr::read_unaligned(sp);
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_DUP2.as_u8() => unsafe {
                sp = sp.sub(2);
                let a = ptr::read_unaligned(sp.add(2));
                let b = ptr::read_unaligned(sp.add(3));
                ptr::write_unaligned(sp, a);
                ptr::write_unaligned(sp.add(1), b);
                ctx.set_sp(NonNull::new(sp).expect("stack pointer"));
            },
            op if op == crate::opcode::OP_INSERT2.as_u8() => unsafe {
                let top = ptr::read_unaligned(sp);
                let next = ptr::read_unaligned(sp.add(1));
                ptr::write_unaligned(sp.sub(1), top);
                ptr::write_unaligned(sp, next);
                ptr::write_unaligned(sp.add(1), top);
                sp = sp.sub(1);
                ctx.set_sp(NonNull::new(sp).expect("stack pointer"));
            },
            op if op == crate::opcode::OP_INSERT3.as_u8() => unsafe {
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
            op if op == crate::opcode::OP_PERM3.as_u8() => unsafe {
                let tmp = ptr::read_unaligned(sp.add(1));
                let val = ptr::read_unaligned(sp.add(2));
                ptr::write_unaligned(sp.add(1), val);
                ptr::write_unaligned(sp.add(2), tmp);
            },
            op if op == crate::opcode::OP_PERM4.as_u8() => unsafe {
                let tmp = ptr::read_unaligned(sp.add(1));
                let val = ptr::read_unaligned(sp.add(2));
                let val2 = ptr::read_unaligned(sp.add(3));
                ptr::write_unaligned(sp.add(1), val);
                ptr::write_unaligned(sp.add(2), val2);
                ptr::write_unaligned(sp.add(3), tmp);
            },
            op if op == crate::opcode::OP_SWAP.as_u8() => unsafe {
                let a = ptr::read_unaligned(sp);
                let b = ptr::read_unaligned(sp.add(1));
                ptr::write_unaligned(sp, b);
                ptr::write_unaligned(sp.add(1), a);
            },
            op if op == crate::opcode::OP_ROT3L.as_u8() => unsafe {
                let tmp = ptr::read_unaligned(sp.add(2));
                let mid = ptr::read_unaligned(sp.add(1));
                let top = ptr::read_unaligned(sp);
                ptr::write_unaligned(sp.add(2), mid);
                ptr::write_unaligned(sp.add(1), top);
                ptr::write_unaligned(sp, tmp);
            },
            op if op == crate::opcode::OP_PUSH_MINUS1.as_u8() => unsafe {
                push(ctx, &mut sp, JSValue::new_short_int(-1));
            },
            op if op == crate::opcode::OP_PUSH_0.as_u8() => unsafe {
                push(ctx, &mut sp, JSValue::new_short_int(0));
            },
            op if op == crate::opcode::OP_PUSH_1.as_u8() => unsafe {
                push(ctx, &mut sp, JSValue::new_short_int(1));
            },
            op if op == crate::opcode::OP_PUSH_2.as_u8() => unsafe {
                push(ctx, &mut sp, JSValue::new_short_int(2));
            },
            op if op == crate::opcode::OP_PUSH_3.as_u8() => unsafe {
                push(ctx, &mut sp, JSValue::new_short_int(3));
            },
            op if op == crate::opcode::OP_PUSH_4.as_u8() => unsafe {
                push(ctx, &mut sp, JSValue::new_short_int(4));
            },
            op if op == crate::opcode::OP_PUSH_5.as_u8() => unsafe {
                push(ctx, &mut sp, JSValue::new_short_int(5));
            },
            op if op == crate::opcode::OP_PUSH_6.as_u8() => unsafe {
                push(ctx, &mut sp, JSValue::new_short_int(6));
            },
            op if op == crate::opcode::OP_PUSH_7.as_u8() => unsafe {
                push(ctx, &mut sp, JSValue::new_short_int(7));
            },
            op if op == crate::opcode::OP_PUSH_I8.as_u8() => unsafe {
                let imm = match byte_slice.get(pc).copied() {
                    Some(val) => val as i8,
                    None => break Err(InterpreterError::InvalidBytecode("push_i8")),
                };
                pc += 1;
                push(ctx, &mut sp, JSValue::new_short_int(i32::from(imm)));
            },
            op if op == crate::opcode::OP_PUSH_I16.as_u8() => unsafe {
                if pc + 1 >= byte_slice.len() {
                    break Err(InterpreterError::InvalidBytecode("push_i16"));
                }
                let imm = i16::from_le_bytes([byte_slice[pc], byte_slice[pc + 1]]);
                pc += 2;
                push(ctx, &mut sp, JSValue::new_short_int(i32::from(imm)));
            },
            op if op == crate::opcode::OP_PUSH_CONST.as_u8() => unsafe {
                if pc + 1 >= byte_slice.len() {
                    break Err(InterpreterError::InvalidBytecode("push_const"));
                }
                let idx = u16::from_le_bytes([byte_slice[pc], byte_slice[pc + 1]]) as usize;
                pc += 2;
                let cpool = try_or_break!(ValueArrayRaw::from_value(b.cpool()));
                let val = try_or_break!(cpool.read(idx));
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_PUSH_CONST8.as_u8() => unsafe {
                let idx = match byte_slice.get(pc).copied() {
                    Some(val) => val as usize,
                    None => break Err(InterpreterError::InvalidBytecode("push_const8")),
                };
                pc += 1;
                let cpool = try_or_break!(ValueArrayRaw::from_value(b.cpool()));
                let val = try_or_break!(cpool.read(idx));
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_PUSH_VALUE.as_u8() => unsafe {
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
                push(ctx, &mut sp, JSValue::from_bits(raw as JSWord));
            },
            op if op == crate::opcode::OP_UNDEFINED.as_u8() => unsafe {
                push(ctx, &mut sp, JSValue::JS_UNDEFINED);
            },
            op if op == crate::opcode::OP_NULL.as_u8() => unsafe {
                push(ctx, &mut sp, JSValue::JS_NULL);
            },
            op if op == crate::opcode::OP_PUSH_FALSE.as_u8() => unsafe {
                push(ctx, &mut sp, JSValue::new_bool(0));
            },
            op if op == crate::opcode::OP_PUSH_TRUE.as_u8() => unsafe {
                push(ctx, &mut sp, JSValue::new_bool(1));
            },
            op if op == crate::opcode::OP_PUSH_THIS.as_u8() => unsafe {
                let val = ptr::read_unaligned(fp.offset(FRAME_OFFSET_THIS_OBJ));
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_OBJECT.as_u8() => unsafe {
                if pc + 1 >= byte_slice.len() {
                    break Err(InterpreterError::InvalidBytecode("object"));
                }
                let count = u16::from_le_bytes([byte_slice[pc], byte_slice[pc + 1]]) as usize;
                pc += 2;
                let obj = try_or_break!(ctx.alloc_object_default());
                push(ctx, &mut sp, obj);
                if count > 0 {
                    try_or_break!(
                        crate::property::prealloc_object_props(ctx, obj, count)
                            .map_err(map_property_error)
                    );
                }
            },
            op if op == crate::opcode::OP_THIS_FUNC.as_u8() => unsafe {
                let val = ptr::read_unaligned(fp.offset(FRAME_OFFSET_FUNC_OBJ));
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_ARGUMENTS.as_u8() => unsafe {
                let call_flags_val = ptr::read_unaligned(fp.offset(FRAME_OFFSET_CALL_FLAGS));
                if !call_flags_val.is_int() {
                    break Err(InterpreterError::InvalidValue("call flags"));
                }
                let argc = call_argc(call_flags_val.get_int());
                let array = try_or_break!(ctx.alloc_array(argc));
                if argc > 0 {
                    let obj_ptr = try_or_break!(object_ptr(array));
                    let data = array_data(obj_ptr);
                    let tab = data.tab();
                    if tab != JSValue::JS_NULL {
                        let arr = try_or_break!(ValueArrayRaw::from_value(tab));
                        let mut write_error = None;
                        for i in 0..argc {
                            let arg = ptr::read_unaligned(fp.add(FRAME_OFFSET_ARG0 as usize + i));
                            if let Err(err) = arr.write(i, arg) {
                                write_error = Some(err);
                                break;
                            }
                        }
                        if let Some(err) = write_error {
                            break Err(err);
                        }
                    }
                }
                push(ctx, &mut sp, array);
            },
            op if op == crate::opcode::OP_NEW_TARGET.as_u8() => unsafe {
                let call_flags_val = ptr::read_unaligned(fp.offset(FRAME_OFFSET_CALL_FLAGS));
                if !call_flags_val.is_int() {
                    break Err(InterpreterError::InvalidValue("call flags"));
                }
                let call_flags = call_flags_val.get_int();
                let val = if (call_flags & FRAME_CF_CTOR) != 0 {
                    ptr::read_unaligned(fp.offset(FRAME_OFFSET_FUNC_OBJ))
                } else {
                    JSValue::JS_UNDEFINED
                };
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_ARRAY_FROM.as_u8() => unsafe {
                if pc + 1 >= byte_slice.len() {
                    break Err(InterpreterError::InvalidBytecode("array_from"));
                }
                let argc = u16::from_le_bytes([byte_slice[pc], byte_slice[pc + 1]]) as usize;
                pc += 2;
                let array = try_or_break!(ctx.alloc_array(argc));
                if argc > 0 {
                    let obj_ptr = try_or_break!(object_ptr(array));
                    let data = array_data(obj_ptr);
                    let tab = data.tab();
                    if tab != JSValue::JS_NULL {
                        let arr = try_or_break!(ValueArrayRaw::from_value(tab));
                        let mut write_error = None;
                        for i in 0..argc {
                            let val = ptr::read_unaligned(sp.add(argc - 1 - i));
                            if let Err(err) = arr.write(i, val) {
                                write_error = Some(err);
                                break;
                            }
                        }
                        if let Some(err) = write_error {
                            break Err(err);
                        }
                    }
                }
                sp = sp.add(argc);
                ctx.set_sp(NonNull::new(sp).expect("stack pointer"));
                push(ctx, &mut sp, array);
            },
            op if op == crate::opcode::OP_FCLOSURE.as_u8() => unsafe {
                if pc + 1 >= byte_slice.len() {
                    break Err(InterpreterError::InvalidBytecode("fclosure"));
                }
                let idx = u16::from_le_bytes([byte_slice[pc], byte_slice[pc + 1]]) as usize;
                pc += 2;
                let cpool = try_or_break!(ValueArrayRaw::from_value(b.cpool()));
                let bfunc = try_or_break!(cpool.read(idx));
                let closure = try_or_break!(create_closure(ctx, bfunc, Some(fp)));
                push(ctx, &mut sp, closure);
            },
            op if op == crate::opcode::OP_CALL_CONSTRUCTOR.as_u8() => {
                if pc + 1 >= byte_slice.len() {
                    break Err(InterpreterError::InvalidBytecode("call_constructor"));
                }
                let call_flags =
                    u16::from_le_bytes([byte_slice[pc], byte_slice[pc + 1]]) as i32 | FRAME_CF_CTOR;
                let argc = call_argc(call_flags);
                unsafe {
                    reverse_vals(sp, argc + 1);
                    push(ctx, &mut sp, JSValue::JS_UNDEFINED);
                }
                handle_call!(call_flags);
            },
            op if op == crate::opcode::OP_CALL.as_u8() => {
                if pc + 1 >= byte_slice.len() {
                    break Err(InterpreterError::InvalidBytecode("call"));
                }
                let call_flags = u16::from_le_bytes([byte_slice[pc], byte_slice[pc + 1]]) as i32;
                let argc = call_argc(call_flags);
                unsafe {
                    reverse_vals(sp, argc + 1);
                    push(ctx, &mut sp, JSValue::JS_UNDEFINED);
                }
                handle_call!(call_flags);
            },
            op if op == crate::opcode::OP_CALL_METHOD.as_u8() => {
                if pc + 1 >= byte_slice.len() {
                    break Err(InterpreterError::InvalidBytecode("call_method"));
                }
                let call_flags = u16::from_le_bytes([byte_slice[pc], byte_slice[pc + 1]]) as i32;
                let argc = call_argc(call_flags);
                unsafe {
                    reverse_vals(sp, argc + 2);
                }
                handle_call!(call_flags);
            },
            op if op == crate::opcode::OP_GET_LOC.as_u8() => unsafe {
                if pc + 1 >= byte_slice.len() {
                    break Err(InterpreterError::InvalidBytecode("get_loc"));
                }
                let idx = u16::from_le_bytes([byte_slice[pc], byte_slice[pc + 1]]) as isize;
                pc += 2;
                let val = ptr::read_unaligned(fp.offset(FRAME_OFFSET_VAR0 - idx));
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_PUT_LOC.as_u8() => unsafe {
                if pc + 1 >= byte_slice.len() {
                    break Err(InterpreterError::InvalidBytecode("put_loc"));
                }
                let idx = u16::from_le_bytes([byte_slice[pc], byte_slice[pc + 1]]) as isize;
                pc += 2;
                let val = pop(ctx, &mut sp);
                ptr::write_unaligned(fp.offset(FRAME_OFFSET_VAR0 - idx), val);
            },
            op if op == crate::opcode::OP_ADD.as_u8() => unsafe {
                let rhs = pop(ctx, &mut sp);
                let lhs = pop(ctx, &mut sp);
                let res = if lhs.is_int() && rhs.is_int() {
                    let sum = lhs.get_int() + rhs.get_int();
                    if (JSValue::JS_SHORTINT_MIN..=JSValue::JS_SHORTINT_MAX).contains(&sum) {
                        Ok(JSValue::new_short_int(sum))
                    } else {
                        add_slow(ctx, lhs, rhs)
                    }
                } else {
                    #[cfg(target_pointer_width = "64")]
                    {
                        if lhs.is_short_float() && rhs.is_short_float() {
                            let sum = lhs.short_float_to_f64() + rhs.short_float_to_f64();
                            Ok(ctx.new_float64(sum)?)
                        } else {
                            add_slow(ctx, lhs, rhs)
                        }
                    }
                    #[cfg(not(target_pointer_width = "64"))]
                    {
                        add_slow(ctx, lhs, rhs)
                    }
                };
                let res = try_or_break!(res);
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_SUB.as_u8() => unsafe {
                let rhs = pop(ctx, &mut sp);
                let lhs = pop(ctx, &mut sp);
                let res = if lhs.is_int() && rhs.is_int() {
                    let diff = lhs.get_int() - rhs.get_int();
                    if (JSValue::JS_SHORTINT_MIN..=JSValue::JS_SHORTINT_MAX).contains(&diff) {
                        Ok(JSValue::new_short_int(diff))
                    } else {
                        binary_arith_slow(ctx, opcode, lhs, rhs)
                    }
                } else {
                    #[cfg(target_pointer_width = "64")]
                    {
                        if lhs.is_short_float() && rhs.is_short_float() {
                            let diff = lhs.short_float_to_f64() - rhs.short_float_to_f64();
                            Ok(ctx.new_float64(diff)?)
                        } else {
                            binary_arith_slow(ctx, opcode, lhs, rhs)
                        }
                    }
                    #[cfg(not(target_pointer_width = "64"))]
                    {
                        binary_arith_slow(ctx, opcode, lhs, rhs)
                    }
                };
                let res = try_or_break!(res);
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_MUL.as_u8() => unsafe {
                let rhs = pop(ctx, &mut sp);
                let lhs = pop(ctx, &mut sp);
                let res = if lhs.is_int() && rhs.is_int() {
                    let v1 = lhs.get_int();
                    let v2 = rhs.get_int();
                    if let Some(prod) = v1.checked_mul(v2) {
                        if (JSValue::JS_SHORTINT_MIN..=JSValue::JS_SHORTINT_MAX).contains(&prod) {
                            if prod == 0 && (v1 | v2) < 0 {
                                Ok(ctx.minus_zero())
                            } else {
                                Ok(JSValue::new_short_int(prod))
                            }
                        } else {
                            binary_arith_slow(ctx, opcode, lhs, rhs)
                        }
                    } else {
                        binary_arith_slow(ctx, opcode, lhs, rhs)
                    }
                } else {
                    #[cfg(target_pointer_width = "64")]
                    {
                        if lhs.is_short_float() && rhs.is_short_float() {
                            let prod = lhs.short_float_to_f64() * rhs.short_float_to_f64();
                            Ok(ctx.new_float64(prod)?)
                        } else {
                            binary_arith_slow(ctx, opcode, lhs, rhs)
                        }
                    }
                    #[cfg(not(target_pointer_width = "64"))]
                    {
                        binary_arith_slow(ctx, opcode, lhs, rhs)
                    }
                };
                let res = try_or_break!(res);
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_DIV.as_u8() => unsafe {
                let rhs = pop(ctx, &mut sp);
                let lhs = pop(ctx, &mut sp);
                let res = if lhs.is_int() && rhs.is_int() {
                    let v1 = lhs.get_int();
                    let v2 = rhs.get_int();
                    Ok(ctx.new_float64((v1 as f64) / (v2 as f64))?)
                } else {
                    binary_arith_slow(ctx, opcode, lhs, rhs)
                };
                let res = try_or_break!(res);
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_MOD.as_u8() => unsafe {
                let rhs = pop(ctx, &mut sp);
                let lhs = pop(ctx, &mut sp);
                let res = if lhs.is_int() && rhs.is_int() {
                    let v1 = lhs.get_int();
                    let v2 = rhs.get_int();
                    if v1 >= 0 && v2 > 0 {
                        Ok(JSValue::new_short_int(v1 % v2))
                    } else {
                        binary_arith_slow(ctx, opcode, lhs, rhs)
                    }
                } else {
                    binary_arith_slow(ctx, opcode, lhs, rhs)
                };
                let res = try_or_break!(res);
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_POW.as_u8() => unsafe {
                let rhs = pop(ctx, &mut sp);
                let lhs = pop(ctx, &mut sp);
                let res = try_or_break!(binary_arith_slow(ctx, opcode, lhs, rhs));
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_NEG.as_u8() => unsafe {
                let val = pop(ctx, &mut sp);
                let res = if val.is_int() {
                    let v1 = val.get_int();
                    if v1 == 0 {
                        Ok(ctx.minus_zero())
                    } else if v1 == JSValue::JS_SHORTINT_MIN {
                        unary_arith_slow(ctx, opcode, val)
                    } else {
                        Ok(JSValue::new_short_int(-v1))
                    }
                } else {
                    unary_arith_slow(ctx, opcode, val)
                };
                let res = try_or_break!(res);
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_PLUS.as_u8() => unsafe {
                let val = pop(ctx, &mut sp);
                let res = if val.is_number() {
                    Ok(val)
                } else {
                    unary_arith_slow(ctx, opcode, val)
                };
                let res = try_or_break!(res);
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_INC.as_u8() => unsafe {
                let val = pop(ctx, &mut sp);
                let res = if val.is_int() {
                    let v1 = val.get_int();
                    if v1 == JSValue::JS_SHORTINT_MAX {
                        unary_arith_slow(ctx, opcode, val)
                    } else {
                        Ok(JSValue::new_short_int(v1 + 1))
                    }
                } else {
                    unary_arith_slow(ctx, opcode, val)
                };
                let res = try_or_break!(res);
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_DEC.as_u8() => unsafe {
                let val = pop(ctx, &mut sp);
                let res = if val.is_int() {
                    let v1 = val.get_int();
                    if v1 == JSValue::JS_SHORTINT_MIN {
                        unary_arith_slow(ctx, opcode, val)
                    } else {
                        Ok(JSValue::new_short_int(v1 - 1))
                    }
                } else {
                    unary_arith_slow(ctx, opcode, val)
                };
                let res = try_or_break!(res);
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_POST_INC.as_u8() => unsafe {
                let val = ptr::read_unaligned(sp);
                let (old_val, new_val) = if val.is_int() {
                    let v1 = val.get_int();
                    if v1 == JSValue::JS_SHORTINT_MAX {
                        try_or_break!(post_inc_slow(ctx, opcode, val))
                    } else {
                        (val, JSValue::new_short_int(v1 + 1))
                    }
                } else {
                    try_or_break!(post_inc_slow(ctx, opcode, val))
                };
                ptr::write_unaligned(sp, old_val);
                push(ctx, &mut sp, new_val);
            },
            op if op == crate::opcode::OP_POST_DEC.as_u8() => unsafe {
                let val = ptr::read_unaligned(sp);
                let (old_val, new_val) = if val.is_int() {
                    let v1 = val.get_int();
                    if v1 == JSValue::JS_SHORTINT_MIN {
                        try_or_break!(post_inc_slow(ctx, opcode, val))
                    } else {
                        (val, JSValue::new_short_int(v1 - 1))
                    }
                } else {
                    try_or_break!(post_inc_slow(ctx, opcode, val))
                };
                ptr::write_unaligned(sp, old_val);
                push(ctx, &mut sp, new_val);
            },
            op if op == crate::opcode::OP_LNOT.as_u8() => unsafe {
                let val = pop(ctx, &mut sp);
                let res = JSValue::new_bool(!try_or_break!(to_bool(val)) as i32);
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_NOT.as_u8() => unsafe {
                let val = pop(ctx, &mut sp);
                let res = if val.is_int() {
                    Ok(JSValue::new_short_int(!val.get_int()))
                } else {
                    not_slow(ctx, val)
                };
                let res = try_or_break!(res);
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_SHL.as_u8() => unsafe {
                let rhs = pop(ctx, &mut sp);
                let lhs = pop(ctx, &mut sp);
                let res = if lhs.is_int() && rhs.is_int() {
                    let shift = (rhs.get_int() & 0x1f) as u32;
                    let r = lhs.get_int().wrapping_shl(shift);
                    if (JSValue::JS_SHORTINT_MIN..=JSValue::JS_SHORTINT_MAX).contains(&r) {
                        Ok(JSValue::new_short_int(r))
                    } else {
                        binary_logic_slow(ctx, opcode, lhs, rhs)
                    }
                } else {
                    binary_logic_slow(ctx, opcode, lhs, rhs)
                };
                let res = try_or_break!(res);
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_SAR.as_u8() => unsafe {
                let rhs = pop(ctx, &mut sp);
                let lhs = pop(ctx, &mut sp);
                let res = if lhs.is_int() && rhs.is_int() {
                    let shift = (rhs.get_int() & 0x1f) as u32;
                    let r = lhs.get_int() >> shift;
                    Ok(JSValue::new_short_int(r))
                } else {
                    binary_logic_slow(ctx, opcode, lhs, rhs)
                };
                let res = try_or_break!(res);
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_SHR.as_u8() => unsafe {
                let rhs = pop(ctx, &mut sp);
                let lhs = pop(ctx, &mut sp);
                let res = if lhs.is_int() && rhs.is_int() {
                    let shift = (rhs.get_int() & 0x1f) as u32;
                    let r = (lhs.get_int() as u32) >> shift;
                    if r <= JSValue::JS_SHORTINT_MAX as u32 {
                        Ok(JSValue::new_short_int(r as i32))
                    } else {
                        binary_logic_slow(ctx, opcode, lhs, rhs)
                    }
                } else {
                    binary_logic_slow(ctx, opcode, lhs, rhs)
                };
                let res = try_or_break!(res);
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_AND.as_u8() => unsafe {
                let rhs = pop(ctx, &mut sp);
                let lhs = pop(ctx, &mut sp);
                let res = if lhs.is_int() && rhs.is_int() {
                    Ok(JSValue::new_short_int(lhs.get_int() & rhs.get_int()))
                } else {
                    binary_logic_slow(ctx, opcode, lhs, rhs)
                };
                let res = try_or_break!(res);
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_XOR.as_u8() => unsafe {
                let rhs = pop(ctx, &mut sp);
                let lhs = pop(ctx, &mut sp);
                let res = if lhs.is_int() && rhs.is_int() {
                    Ok(JSValue::new_short_int(lhs.get_int() ^ rhs.get_int()))
                } else {
                    binary_logic_slow(ctx, opcode, lhs, rhs)
                };
                let res = try_or_break!(res);
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_OR.as_u8() => unsafe {
                let rhs = pop(ctx, &mut sp);
                let lhs = pop(ctx, &mut sp);
                let res = if lhs.is_int() && rhs.is_int() {
                    Ok(JSValue::new_short_int(lhs.get_int() | rhs.get_int()))
                } else {
                    binary_logic_slow(ctx, opcode, lhs, rhs)
                };
                let res = try_or_break!(res);
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_LT.as_u8() => unsafe {
                let rhs = pop(ctx, &mut sp);
                let lhs = pop(ctx, &mut sp);
                let res = if lhs.is_int() && rhs.is_int() {
                    Ok(JSValue::new_bool((lhs.get_int() < rhs.get_int()) as i32))
                } else {
                    relational_slow(ctx, opcode, lhs, rhs)
                };
                let res = try_or_break!(res);
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_LTE.as_u8() => unsafe {
                let rhs = pop(ctx, &mut sp);
                let lhs = pop(ctx, &mut sp);
                let res = if lhs.is_int() && rhs.is_int() {
                    Ok(JSValue::new_bool((lhs.get_int() <= rhs.get_int()) as i32))
                } else {
                    relational_slow(ctx, opcode, lhs, rhs)
                };
                let res = try_or_break!(res);
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_GT.as_u8() => unsafe {
                let rhs = pop(ctx, &mut sp);
                let lhs = pop(ctx, &mut sp);
                let res = if lhs.is_int() && rhs.is_int() {
                    Ok(JSValue::new_bool((lhs.get_int() > rhs.get_int()) as i32))
                } else {
                    relational_slow(ctx, opcode, lhs, rhs)
                };
                let res = try_or_break!(res);
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_GTE.as_u8() => unsafe {
                let rhs = pop(ctx, &mut sp);
                let lhs = pop(ctx, &mut sp);
                let res = if lhs.is_int() && rhs.is_int() {
                    Ok(JSValue::new_bool((lhs.get_int() >= rhs.get_int()) as i32))
                } else {
                    relational_slow(ctx, opcode, lhs, rhs)
                };
                let res = try_or_break!(res);
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_EQ.as_u8() => unsafe {
                let rhs = pop(ctx, &mut sp);
                let lhs = pop(ctx, &mut sp);
                let res = if lhs.is_int() && rhs.is_int() {
                    Ok(JSValue::new_bool((lhs.get_int() == rhs.get_int()) as i32))
                } else {
                    eq_slow(ctx, lhs, rhs, false)
                };
                let res = try_or_break!(res);
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_NEQ.as_u8() => unsafe {
                let rhs = pop(ctx, &mut sp);
                let lhs = pop(ctx, &mut sp);
                let res = if lhs.is_int() && rhs.is_int() {
                    Ok(JSValue::new_bool((lhs.get_int() != rhs.get_int()) as i32))
                } else {
                    eq_slow(ctx, lhs, rhs, true)
                };
                let res = try_or_break!(res);
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_STRICT_EQ.as_u8() => unsafe {
                let rhs = pop(ctx, &mut sp);
                let lhs = pop(ctx, &mut sp);
                let res: Result<JSValue, InterpreterError> = if lhs.is_int() && rhs.is_int() {
                    Ok(JSValue::new_bool((lhs.get_int() == rhs.get_int()) as i32))
                } else {
                    Ok(JSValue::new_bool(strict_eq(ctx, lhs, rhs)? as i32))
                };
                let res = try_or_break!(res);
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_STRICT_NEQ.as_u8() => unsafe {
                let rhs = pop(ctx, &mut sp);
                let lhs = pop(ctx, &mut sp);
                let res: Result<JSValue, InterpreterError> = if lhs.is_int() && rhs.is_int() {
                    Ok(JSValue::new_bool((lhs.get_int() != rhs.get_int()) as i32))
                } else {
                    Ok(JSValue::new_bool((!strict_eq(ctx, lhs, rhs)?) as i32))
                };
                let res = try_or_break!(res);
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_TYPEOF.as_u8() => unsafe {
                let val = pop(ctx, &mut sp);
                let res = try_or_break!(typeof_value(ctx, val));
                push(ctx, &mut sp, res);
            },
            op if op == crate::opcode::OP_IN.as_u8() => unsafe {
                let obj = pop(ctx, &mut sp);
                let prop = pop(ctx, &mut sp);
                if !obj.is_object() {
                    break Err(InterpreterError::TypeError("invalid 'in' operand"));
                }
                let prop = try_or_break!(conversion::to_property_key(ctx, prop));
                let has = try_or_break!(
                    crate::property::has_property(ctx, obj, prop).map_err(ConversionError::from)
                );
                push(ctx, &mut sp, JSValue::new_bool(has as i32));
            },
            op if op == crate::opcode::OP_INSTANCEOF.as_u8() => unsafe {
                let ctor = pop(ctx, &mut sp);
                let obj = pop(ctx, &mut sp);
                if !is_function_object(ctor) {
                    break Err(InterpreterError::TypeError(
                        "invalid 'instanceof' right operand",
                    ));
                }
                let proto_key = try_or_break!(ctx.intern_string(b"prototype"));
                let proto = try_or_break!(
                    crate::property::get_property(ctx, ctor, proto_key).map_err(ConversionError::from)
                );
                let found = try_or_break!(instanceof_check(obj, proto));
                push(ctx, &mut sp, JSValue::new_bool(found as i32));
            },
            op if op == crate::opcode::OP_GET_FIELD.as_u8() => unsafe {
                if pc + 1 >= byte_slice.len() {
                    break Err(InterpreterError::InvalidBytecode("get_field"));
                }
                let idx = u16::from_le_bytes([byte_slice[pc], byte_slice[pc + 1]]) as usize;
                pc += 2;
                let cpool = try_or_break!(ValueArrayRaw::from_value(b.cpool()));
                let prop = try_or_break!(cpool.read(idx));
                let obj = ptr::read_unaligned(sp);
                let val = match crate::property::get_property(ctx, obj, prop) {
                    Ok(val) => val,
                    Err(err) => {
                        if let Some(thrown) = property_error_to_exception(ctx, &err) {
                            let flow = {
                                let mut frame_state = FrameState {
                                    stack_top,
                                    initial_fp,
                                    sp: &mut sp,
                                    fp: &mut fp,
                                    pc: &mut pc,
                                    func_ptr: &mut func_ptr,
                                    byte_slice: &mut byte_slice,
                                    n_vars: &mut n_vars,
                                };
                                try_or_break!(unwind_exception(ctx, &mut frame_state, thrown))
                            };
                            if let ReturnFlow::Done(val) = flow {
                                break Ok(val);
                            }
                            b = func_ptr.as_ref();
                            continue;
                        } else {
                            break Err(ConversionError::from(err).into());
                        }
                    }
                };
                ptr::write_unaligned(sp, val);
            },
            op if op == crate::opcode::OP_GET_FIELD2.as_u8() => unsafe {
                if pc + 1 >= byte_slice.len() {
                    break Err(InterpreterError::InvalidBytecode("get_field2"));
                }
                let idx = u16::from_le_bytes([byte_slice[pc], byte_slice[pc + 1]]) as usize;
                pc += 2;
                let cpool = try_or_break!(ValueArrayRaw::from_value(b.cpool()));
                let prop = try_or_break!(cpool.read(idx));
                let obj = ptr::read_unaligned(sp);
                push(ctx, &mut sp, obj);
                let val = match crate::property::get_property(ctx, obj, prop) {
                    Ok(val) => val,
                    Err(err) => {
                        if let Some(thrown) = property_error_to_exception(ctx, &err) {
                            let flow = {
                                let mut frame_state = FrameState {
                                    stack_top,
                                    initial_fp,
                                    sp: &mut sp,
                                    fp: &mut fp,
                                    pc: &mut pc,
                                    func_ptr: &mut func_ptr,
                                    byte_slice: &mut byte_slice,
                                    n_vars: &mut n_vars,
                                };
                                try_or_break!(unwind_exception(ctx, &mut frame_state, thrown))
                            };
                            if let ReturnFlow::Done(val) = flow {
                                break Ok(val);
                            }
                            b = func_ptr.as_ref();
                            continue;
                        } else {
                            break Err(ConversionError::from(err).into());
                        }
                    }
                };
                ptr::write_unaligned(sp, val);
            },
            op if op == crate::opcode::OP_PUT_FIELD.as_u8() => unsafe {
                if pc + 1 >= byte_slice.len() {
                    break Err(InterpreterError::InvalidBytecode("put_field"));
                }
                let idx = u16::from_le_bytes([byte_slice[pc], byte_slice[pc + 1]]) as usize;
                pc += 2;
                let cpool = try_or_break!(ValueArrayRaw::from_value(b.cpool()));
                let prop = try_or_break!(cpool.read(idx));
                let val = pop(ctx, &mut sp);
                let obj = ptr::read_unaligned(sp);
                match crate::property::set_property(ctx, obj, prop, val) {
                    Ok(()) => {
                        let _ = pop(ctx, &mut sp);
                    }
                    Err(err) => {
                        if let Some(thrown) = property_error_to_exception(ctx, &err) {
                            let flow = {
                                let mut frame_state = FrameState {
                                    stack_top,
                                    initial_fp,
                                    sp: &mut sp,
                                    fp: &mut fp,
                                    pc: &mut pc,
                                    func_ptr: &mut func_ptr,
                                    byte_slice: &mut byte_slice,
                                    n_vars: &mut n_vars,
                                };
                                try_or_break!(unwind_exception(ctx, &mut frame_state, thrown))
                            };
                            if let ReturnFlow::Done(val) = flow {
                                break Ok(val);
                            }
                            b = func_ptr.as_ref();
                        } else {
                            break Err(ConversionError::from(err).into());
                        }
                    }
                }
            },
            op if op == crate::opcode::OP_GET_ARRAY_EL.as_u8() => unsafe {
                let prop = pop(ctx, &mut sp);
                let obj = ptr::read_unaligned(sp);
                let prop = try_or_break!(conversion::to_property_key(ctx, prop));
                let val = match crate::property::get_property(ctx, obj, prop) {
                    Ok(val) => val,
                    Err(err) => {
                        if let Some(thrown) = property_error_to_exception(ctx, &err) {
                            let flow = {
                                let mut frame_state = FrameState {
                                    stack_top,
                                    initial_fp,
                                    sp: &mut sp,
                                    fp: &mut fp,
                                    pc: &mut pc,
                                    func_ptr: &mut func_ptr,
                                    byte_slice: &mut byte_slice,
                                    n_vars: &mut n_vars,
                                };
                                try_or_break!(unwind_exception(ctx, &mut frame_state, thrown))
                            };
                            if let ReturnFlow::Done(val) = flow {
                                break Ok(val);
                            }
                            b = func_ptr.as_ref();
                            continue;
                        } else {
                            break Err(ConversionError::from(err).into());
                        }
                    }
                };
                ptr::write_unaligned(sp, val);
            },
            op if op == crate::opcode::OP_GET_ARRAY_EL2.as_u8() => unsafe {
                let prop = ptr::read_unaligned(sp);
                let obj = ptr::read_unaligned(sp.add(1));
                let prop = try_or_break!(conversion::to_property_key(ctx, prop));
                let val = match crate::property::get_property(ctx, obj, prop) {
                    Ok(val) => val,
                    Err(err) => {
                        if let Some(thrown) = property_error_to_exception(ctx, &err) {
                            let flow = {
                                let mut frame_state = FrameState {
                                    stack_top,
                                    initial_fp,
                                    sp: &mut sp,
                                    fp: &mut fp,
                                    pc: &mut pc,
                                    func_ptr: &mut func_ptr,
                                    byte_slice: &mut byte_slice,
                                    n_vars: &mut n_vars,
                                };
                                try_or_break!(unwind_exception(ctx, &mut frame_state, thrown))
                            };
                            if let ReturnFlow::Done(val) = flow {
                                break Ok(val);
                            }
                            b = func_ptr.as_ref();
                            continue;
                        } else {
                            break Err(ConversionError::from(err).into());
                        }
                    }
                };
                ptr::write_unaligned(sp, val);
            },
            op if op == crate::opcode::OP_PUT_ARRAY_EL.as_u8() => unsafe {
                let val = pop(ctx, &mut sp);
                let prop = pop(ctx, &mut sp);
                let obj = ptr::read_unaligned(sp);
                let prop = try_or_break!(conversion::to_property_key(ctx, prop));
                match crate::property::set_property(ctx, obj, prop, val) {
                    Ok(()) => {
                        let _ = pop(ctx, &mut sp);
                    }
                    Err(err) => {
                        if let Some(thrown) = property_error_to_exception(ctx, &err) {
                            let flow = {
                                let mut frame_state = FrameState {
                                    stack_top,
                                    initial_fp,
                                    sp: &mut sp,
                                    fp: &mut fp,
                                    pc: &mut pc,
                                    func_ptr: &mut func_ptr,
                                    byte_slice: &mut byte_slice,
                                    n_vars: &mut n_vars,
                                };
                                try_or_break!(unwind_exception(ctx, &mut frame_state, thrown))
                            };
                            if let ReturnFlow::Done(val) = flow {
                                break Ok(val);
                            }
                            b = func_ptr.as_ref();
                        } else {
                            break Err(ConversionError::from(err).into());
                        }
                    }
                }
            },
            op if op == crate::opcode::OP_GET_LENGTH.as_u8() => unsafe {
                let obj = ptr::read_unaligned(sp);
                let val = try_or_break!(get_length_value(ctx, obj));
                ptr::write_unaligned(sp, val);
            },
            op if op == crate::opcode::OP_GET_LENGTH2.as_u8() => unsafe {
                let obj = ptr::read_unaligned(sp);
                push(ctx, &mut sp, obj);
                let val = try_or_break!(get_length_value(ctx, obj));
                ptr::write_unaligned(sp, val);
            },
            op if op == crate::opcode::OP_DEFINE_FIELD.as_u8() => unsafe {
                if pc + 1 >= byte_slice.len() {
                    break Err(InterpreterError::InvalidBytecode("define_field"));
                }
                let idx = u16::from_le_bytes([byte_slice[pc], byte_slice[pc + 1]]) as usize;
                pc += 2;
                let cpool = try_or_break!(ValueArrayRaw::from_value(b.cpool()));
                let prop = try_or_break!(cpool.read(idx));
                let val = pop(ctx, &mut sp);
                let obj = ptr::read_unaligned(sp);
                try_or_break!(
                    crate::property::define_property_value(ctx, obj, prop, val)
                        .map_err(ConversionError::from)
                );
            },
            op if op == crate::opcode::OP_DEFINE_GETTER.as_u8() => unsafe {
                if pc + 1 >= byte_slice.len() {
                    break Err(InterpreterError::InvalidBytecode("define_getter"));
                }
                let idx = u16::from_le_bytes([byte_slice[pc], byte_slice[pc + 1]]) as usize;
                pc += 2;
                let cpool = try_or_break!(ValueArrayRaw::from_value(b.cpool()));
                let prop = try_or_break!(cpool.read(idx));
                let val = pop(ctx, &mut sp);
                let obj = ptr::read_unaligned(sp);
                try_or_break!(
                    crate::property::define_property_getset(ctx, obj, prop, val, JSValue::JS_UNDEFINED)
                        .map_err(ConversionError::from)
                );
            },
            op if op == crate::opcode::OP_DEFINE_SETTER.as_u8() => unsafe {
                if pc + 1 >= byte_slice.len() {
                    break Err(InterpreterError::InvalidBytecode("define_setter"));
                }
                let idx = u16::from_le_bytes([byte_slice[pc], byte_slice[pc + 1]]) as usize;
                pc += 2;
                let cpool = try_or_break!(ValueArrayRaw::from_value(b.cpool()));
                let prop = try_or_break!(cpool.read(idx));
                let val = pop(ctx, &mut sp);
                let obj = ptr::read_unaligned(sp);
                try_or_break!(
                    crate::property::define_property_getset(ctx, obj, prop, JSValue::JS_UNDEFINED, val)
                        .map_err(ConversionError::from)
                );
            },
            op if op == crate::opcode::OP_SET_PROTO.as_u8() => unsafe {
                let proto = pop(ctx, &mut sp);
                let obj = ptr::read_unaligned(sp);
                if proto.is_object() || proto.is_null() {
                    try_or_break!(set_prototype_internal(obj, proto));
                }
            },
            op if op == crate::opcode::OP_DELETE.as_u8() => unsafe {
                let prop = pop(ctx, &mut sp);
                let obj = ptr::read_unaligned(sp);
                let prop = try_or_break!(conversion::to_property_key(ctx, prop));
                let deleted = try_or_break!(
                    crate::property::delete_property(ctx, obj, prop).map_err(ConversionError::from)
                );
                ptr::write_unaligned(sp, JSValue::new_bool(deleted as i32));
            },
            op if op == crate::opcode::OP_FOR_IN_START.as_u8()
                || op == crate::opcode::OP_FOR_OF_START.as_u8() =>
            unsafe {
                let val = ptr::read_unaligned(sp);
                let iter = try_or_break!(for_of_start(
                    ctx,
                    val,
                    op == crate::opcode::OP_FOR_IN_START.as_u8()
                ));
                ptr::write_unaligned(sp, iter);
            },
            op if op == crate::opcode::OP_FOR_OF_NEXT.as_u8() => unsafe {
                try_or_break!(stack_check(ctx, sp, 2));
                let iter = ptr::read_unaligned(sp);
                let done_slot = sp.sub(2);
                let value_slot = sp.sub(1);
                try_or_break!(for_of_next(ctx, iter, done_slot, value_slot));
                sp = sp.sub(2);
                ctx.set_sp(NonNull::new(sp).expect("stack pointer"));
            },
            op if op == crate::opcode::OP_REGEXP.as_u8() => unsafe {
                let byte_code = ptr::read_unaligned(sp);
                let source = ptr::read_unaligned(sp.add(1));
                let re_obj = try_or_break!(crate::regexp::new_regexp_object(ctx, source, byte_code));
                ptr::write_unaligned(sp.add(1), re_obj);
                sp = sp.add(1);
                ctx.set_sp(NonNull::new(sp).expect("stack pointer"));
            },
            op if op == crate::opcode::OP_GET_LOC0.as_u8() => unsafe {
                let val = ptr::read_unaligned(fp.offset(FRAME_OFFSET_VAR0));
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_GET_LOC1.as_u8() => unsafe {
                let val = ptr::read_unaligned(fp.offset(FRAME_OFFSET_VAR0 - 1));
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_GET_LOC2.as_u8() => unsafe {
                let val = ptr::read_unaligned(fp.offset(FRAME_OFFSET_VAR0 - 2));
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_GET_LOC3.as_u8() => unsafe {
                let val = ptr::read_unaligned(fp.offset(FRAME_OFFSET_VAR0 - 3));
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_PUT_LOC0.as_u8() => unsafe {
                let val = pop(ctx, &mut sp);
                ptr::write_unaligned(fp.offset(FRAME_OFFSET_VAR0), val);
            },
            op if op == crate::opcode::OP_PUT_LOC1.as_u8() => unsafe {
                let val = pop(ctx, &mut sp);
                ptr::write_unaligned(fp.offset(FRAME_OFFSET_VAR0 - 1), val);
            },
            op if op == crate::opcode::OP_PUT_LOC2.as_u8() => unsafe {
                let val = pop(ctx, &mut sp);
                ptr::write_unaligned(fp.offset(FRAME_OFFSET_VAR0 - 2), val);
            },
            op if op == crate::opcode::OP_PUT_LOC3.as_u8() => unsafe {
                let val = pop(ctx, &mut sp);
                ptr::write_unaligned(fp.offset(FRAME_OFFSET_VAR0 - 3), val);
            },
            op if op == crate::opcode::OP_GET_LOC8.as_u8() => unsafe {
                let idx = match byte_slice.get(pc).copied() {
                    Some(val) => val as isize,
                    None => break Err(InterpreterError::InvalidBytecode("get_loc8")),
                };
                pc += 1;
                let val = ptr::read_unaligned(fp.offset(FRAME_OFFSET_VAR0 - idx));
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_PUT_LOC8.as_u8() => unsafe {
                let idx = match byte_slice.get(pc).copied() {
                    Some(val) => val as isize,
                    None => break Err(InterpreterError::InvalidBytecode("put_loc8")),
                };
                pc += 1;
                let val = pop(ctx, &mut sp);
                ptr::write_unaligned(fp.offset(FRAME_OFFSET_VAR0 - idx), val);
            },
            op if op == crate::opcode::OP_GET_ARG.as_u8() => unsafe {
                if pc + 1 >= byte_slice.len() {
                    break Err(InterpreterError::InvalidBytecode("get_arg"));
                }
                let idx = u16::from_le_bytes([byte_slice[pc], byte_slice[pc + 1]]) as usize;
                pc += 2;
                let val = ptr::read_unaligned(fp.add(FRAME_OFFSET_ARG0 as usize + idx));
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_PUT_ARG.as_u8() => unsafe {
                if pc + 1 >= byte_slice.len() {
                    break Err(InterpreterError::InvalidBytecode("put_arg"));
                }
                let idx = u16::from_le_bytes([byte_slice[pc], byte_slice[pc + 1]]) as usize;
                pc += 2;
                let val = pop(ctx, &mut sp);
                ptr::write_unaligned(fp.add(FRAME_OFFSET_ARG0 as usize + idx), val);
            },
            op if op == crate::opcode::OP_GET_ARG0.as_u8() => unsafe {
                let val = ptr::read_unaligned(fp.add(FRAME_OFFSET_ARG0 as usize));
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_GET_ARG1.as_u8() => unsafe {
                let val = ptr::read_unaligned(fp.add(FRAME_OFFSET_ARG0 as usize + 1));
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_GET_ARG2.as_u8() => unsafe {
                let val = ptr::read_unaligned(fp.add(FRAME_OFFSET_ARG0 as usize + 2));
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_GET_ARG3.as_u8() => unsafe {
                let val = ptr::read_unaligned(fp.add(FRAME_OFFSET_ARG0 as usize + 3));
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_PUT_ARG0.as_u8() => unsafe {
                let val = pop(ctx, &mut sp);
                ptr::write_unaligned(fp.add(FRAME_OFFSET_ARG0 as usize), val);
            },
            op if op == crate::opcode::OP_PUT_ARG1.as_u8() => unsafe {
                let val = pop(ctx, &mut sp);
                ptr::write_unaligned(fp.add(FRAME_OFFSET_ARG0 as usize + 1), val);
            },
            op if op == crate::opcode::OP_PUT_ARG2.as_u8() => unsafe {
                let val = pop(ctx, &mut sp);
                ptr::write_unaligned(fp.add(FRAME_OFFSET_ARG0 as usize + 2), val);
            },
            op if op == crate::opcode::OP_PUT_ARG3.as_u8() => unsafe {
                let val = pop(ctx, &mut sp);
                ptr::write_unaligned(fp.add(FRAME_OFFSET_ARG0 as usize + 3), val);
            },
            op if op == crate::opcode::OP_GET_VAR_REF.as_u8()
                || op == crate::opcode::OP_GET_VAR_REF_NOCHECK.as_u8() =>
            unsafe {
                if pc + 1 >= byte_slice.len() {
                    break Err(InterpreterError::InvalidBytecode("get_var_ref"));
                }
                let idx = u16::from_le_bytes([byte_slice[pc], byte_slice[pc + 1]]) as usize;
                pc += 2;
                let func_obj = ptr::read_unaligned(fp.offset(FRAME_OFFSET_FUNC_OBJ));
                let obj_ptr = try_or_break!(object_ptr(func_obj));
                let header = object_header(obj_ptr);
                if header.class_id() != JSObjectClass::Closure as u8 {
                    break Err(InterpreterError::InvalidValue("varref closure"));
                }
                let extra = header.extra_size() as usize;
                if idx >= extra.saturating_sub(1) {
                    break Err(InterpreterError::InvalidBytecode("varref index"));
                }
                let var_ref = {
                    // SAFETY: closure var_refs contains extra_size - 1 entries.
                    let payload = Object::payload_ptr(obj_ptr.as_ptr());
                    let closure_data = core::ptr::addr_of_mut!((*payload).closure);
                    let var_refs = crate::closure_data::ClosureData::var_refs_ptr(closure_data);
                    ptr::read_unaligned(var_refs.add(idx))
                };
                let val = try_or_break!(var_ref_get(var_ref));
                if val.is_uninitialized() && op == crate::opcode::OP_GET_VAR_REF.as_u8() {
                    let thrown = throw_reference_error_value(ctx, "varref uninitialized");
                    let flow = {
                        let mut frame_state = FrameState {
                            stack_top,
                            initial_fp,
                            sp: &mut sp,
                            fp: &mut fp,
                            pc: &mut pc,
                            func_ptr: &mut func_ptr,
                            byte_slice: &mut byte_slice,
                            n_vars: &mut n_vars,
                        };
                        try_or_break!(unwind_exception(ctx, &mut frame_state, thrown))
                    };
                    if let ReturnFlow::Done(val) = flow {
                        break Ok(val);
                    }
                    b = func_ptr.as_ref();
                    continue;
                }
                push(ctx, &mut sp, val);
            },
            op if op == crate::opcode::OP_PUT_VAR_REF.as_u8()
                || op == crate::opcode::OP_PUT_VAR_REF_NOCHECK.as_u8() =>
            unsafe {
                if pc + 1 >= byte_slice.len() {
                    break Err(InterpreterError::InvalidBytecode("put_var_ref"));
                }
                let idx = u16::from_le_bytes([byte_slice[pc], byte_slice[pc + 1]]) as usize;
                pc += 2;
                let func_obj = ptr::read_unaligned(fp.offset(FRAME_OFFSET_FUNC_OBJ));
                let obj_ptr = try_or_break!(object_ptr(func_obj));
                let header = object_header(obj_ptr);
                if header.class_id() != JSObjectClass::Closure as u8 {
                    break Err(InterpreterError::InvalidValue("varref closure"));
                }
                let extra = header.extra_size() as usize;
                if idx >= extra.saturating_sub(1) {
                    break Err(InterpreterError::InvalidBytecode("varref index"));
                }
                let var_ref = {
                    // SAFETY: closure var_refs contains extra_size - 1 entries.
                    let payload = Object::payload_ptr(obj_ptr.as_ptr());
                    let closure_data = core::ptr::addr_of_mut!((*payload).closure);
                    let var_refs = crate::closure_data::ClosureData::var_refs_ptr(closure_data);
                    ptr::read_unaligned(var_refs.add(idx))
                };
                let current = try_or_break!(var_ref_get(var_ref));
                if current.is_uninitialized() && op == crate::opcode::OP_PUT_VAR_REF.as_u8() {
                    let thrown = throw_reference_error_value(ctx, "varref uninitialized");
                    let flow = {
                        let mut frame_state = FrameState {
                            stack_top,
                            initial_fp,
                            sp: &mut sp,
                            fp: &mut fp,
                            pc: &mut pc,
                            func_ptr: &mut func_ptr,
                            byte_slice: &mut byte_slice,
                            n_vars: &mut n_vars,
                        };
                        try_or_break!(unwind_exception(ctx, &mut frame_state, thrown))
                    };
                    if let ReturnFlow::Done(val) = flow {
                        break Ok(val);
                    }
                    b = func_ptr.as_ref();
                    continue;
                }
                let val = pop(ctx, &mut sp);
                try_or_break!(var_ref_set(var_ref, val));
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
    use crate::api::js_to_cstring;
    use crate::array_data::ArrayData;
    use crate::closure_data::ClosureData;
    use crate::context::{ContextConfig, JSContext};
    use crate::enums::{JSObjectClass, JSVarRefKind};
    use crate::function_bytecode::{FunctionBytecodeFields, FunctionBytecodeHeader};
    use crate::object::Object;
    use crate::opcode::{
        OP_ADD, OP_AND, OP_ARGUMENTS, OP_ARRAY_FROM, OP_CALL, OP_CALL_CONSTRUCTOR, OP_CALL_METHOD,
        OP_CATCH, OP_DEFINE_FIELD, OP_DEFINE_GETTER, OP_DEFINE_SETTER, OP_DELETE, OP_DIV, OP_DUP2,
        OP_EQ, OP_FCLOSURE, OP_FOR_IN_START, OP_FOR_OF_NEXT, OP_FOR_OF_START, OP_GET_ARG0,
        OP_GET_ARG1, OP_GET_ARRAY_EL, OP_GET_ARRAY_EL2, OP_GET_FIELD, OP_GET_FIELD2,
        OP_GET_LENGTH, OP_GET_LENGTH2, OP_GET_VAR_REF, OP_GET_VAR_REF_NOCHECK, OP_GOSUB, OP_GOTO,
        OP_IF_FALSE, OP_IF_TRUE, OP_INSERT2, OP_INSERT3, OP_LT, OP_MUL, OP_NEW_TARGET, OP_NIP,
        OP_OBJECT, OP_PERM3, OP_PERM4, OP_PUSH_0, OP_PUSH_1, OP_PUSH_2, OP_PUSH_3, OP_PUSH_4,
        OP_PUSH_5, OP_PUSH_CONST, OP_PUSH_I8, OP_PUSH_THIS, OP_PUSH_TRUE, OP_PUT_ARRAY_EL,
        OP_PUT_FIELD, OP_PUT_VAR_REF, OP_PUT_VAR_REF_NOCHECK, OP_REGEXP, OP_RET, OP_RETURN,
        OP_ROT3L, OP_SET_PROTO, OP_STRICT_EQ, OP_SUB, OP_SWAP, OP_THIS_FUNC, OP_THROW,
    };
    use crate::property::{debug_property, define_property_value, has_property, DebugProperty};
    use crate::parser::regexp::compile_regexp;
    use crate::string::runtime::string_view;
    use crate::stdlib::MQUICKJS_STDLIB_IMAGE;
    use core::mem::size_of;
    use core::ptr;

    fn emit_i32(buf: &mut Vec<u8>, value: i32) {
        buf.extend_from_slice(&value.to_le_bytes());
    }

    fn emit_u16(buf: &mut Vec<u8>, value: u16) {
        buf.extend_from_slice(&value.to_le_bytes());
    }

    fn call_bytecode(ctx: &mut JSContext, bytecode: Vec<u8>) -> JSValue {
        call_bytecode_with_cpool(ctx, bytecode, Vec::new())
    }

    fn call_bytecode_with_cpool(
        ctx: &mut JSContext,
        bytecode: Vec<u8>,
        cpool: Vec<JSValue>,
    ) -> JSValue {
        let byte_code_val = ctx.alloc_byte_array(&bytecode).expect("bytecode");
        let cpool_val = if cpool.is_empty() {
            JSValue::JS_NULL
        } else {
            let ptr = ctx.alloc_value_array(cpool.len()).expect("cpool");
            unsafe {
                // SAFETY: ptr points to a freshly allocated value array.
                let arr = ptr.as_ptr().add(size_of::<JSWord>()) as *mut JSValue;
                for (idx, val) in cpool.iter().enumerate() {
                    ptr::write_unaligned(arr.add(idx), *val);
                }
            }
            JSValue::from_ptr(ptr)
        };
        let header = FunctionBytecodeHeader::new(false, false, false, 0, false);
        let fields = FunctionBytecodeFields {
            func_name: JSValue::JS_NULL,
            byte_code: byte_code_val,
            cpool: cpool_val,
            vars: JSValue::JS_NULL,
            ext_vars: JSValue::JS_NULL,
            stack_size: 4,
            ext_vars_len: 0,
            filename: JSValue::JS_NULL,
            pc2line: JSValue::JS_NULL,
            source_pos: 0,
        };
        let func = ctx
            .alloc_function_bytecode(header, fields)
            .expect("func");
        let closure = ctx.alloc_closure(func, 0).expect("closure");
        call(ctx, closure, &[]).expect("call")
    }

    #[test]
    fn mul_preserves_negative_zero_for_ints() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");

        let mut bytecode = Vec::new();
        bytecode.push(OP_PUSH_1.as_u8());
        bytecode.push(OP_PUSH_0.as_u8());
        bytecode.push(OP_PUSH_I8.as_u8());
        bytecode.push((-6i8) as u8);
        bytecode.push(OP_MUL.as_u8());
        bytecode.push(OP_DIV.as_u8());
        bytecode.push(OP_RETURN.as_u8());

        let result = call_bytecode(&mut ctx, bytecode);
        let num = crate::conversion::to_number(&mut ctx, result).expect("number");
        assert!(num.is_infinite());
        assert!(num.is_sign_negative());
    }

    fn alloc_value_array(ctx: &mut JSContext, values: &[JSValue]) -> JSValue {
        if values.is_empty() {
            return JSValue::JS_NULL;
        }
        let ptr = ctx.alloc_value_array(values.len()).expect("value array");
        unsafe {
            // SAFETY: ptr points to a freshly allocated value array.
            let arr = ptr.as_ptr().add(size_of::<JSWord>()) as *mut JSValue;
            for (idx, val) in values.iter().enumerate() {
                ptr::write_unaligned(arr.add(idx), *val);
            }
        }
        JSValue::from_ptr(ptr)
    }

    fn make_function_bytecode(
        ctx: &mut JSContext,
        bytecode: Vec<u8>,
        cpool: Vec<JSValue>,
        vars: Vec<JSValue>,
        ext_vars: Vec<JSValue>,
        arg_count: u16,
        stack_size: u16,
    ) -> JSValue {
        assert!(ext_vars.len() % 2 == 0, "ext_vars must be name/decl pairs");
        let byte_code_val = ctx.alloc_byte_array(&bytecode).expect("bytecode");
        let cpool_val = alloc_value_array(ctx, &cpool);
        let vars_val = alloc_value_array(ctx, &vars);
        let ext_vars_val = alloc_value_array(ctx, &ext_vars);
        let ext_vars_len = (ext_vars.len() / 2) as u16;
        let header = FunctionBytecodeHeader::new(false, false, false, arg_count, false);
        let fields = FunctionBytecodeFields {
            func_name: JSValue::JS_NULL,
            byte_code: byte_code_val,
            cpool: cpool_val,
            vars: vars_val,
            ext_vars: ext_vars_val,
            stack_size,
            ext_vars_len,
            filename: JSValue::JS_NULL,
            pc2line: JSValue::JS_NULL,
            source_pos: 0,
        };
        ctx.alloc_function_bytecode(header, fields).expect("func")
    }

    fn string_from_value(val: JSValue) -> String {
        let mut scratch = [0u8; 5];
        let view = string_view(val, &mut scratch).expect("string view");
        core::str::from_utf8(view.bytes())
            .expect("utf8")
            .to_string()
    }

    fn new_array(ctx: &mut JSContext, elements: &[JSValue]) -> JSValue {
        let tab = if elements.is_empty() {
            JSValue::JS_NULL
        } else {
            let ptr = ctx.alloc_value_array(elements.len()).expect("array tab");
            unsafe {
                // SAFETY: ptr points to a freshly allocated value array.
                let arr = ptr.as_ptr().add(size_of::<JSWord>()) as *mut JSValue;
                for (idx, val) in elements.iter().enumerate() {
                    ptr::write_unaligned(arr.add(idx), *val);
                }
            }
            JSValue::from_ptr(ptr)
        };
        let proto = ctx.class_proto()[JSObjectClass::Array as usize];
        let array = ctx
            .alloc_object(JSObjectClass::Array, proto, size_of::<ArrayData>())
            .expect("array");
        let obj_ptr = array.to_ptr::<Object>().expect("array ptr");
        unsafe {
            // SAFETY: payload points at a writable array data slot.
            let payload = Object::payload_ptr(obj_ptr.as_ptr());
            let array_ptr = ptr::addr_of_mut!((*payload).array);
            ptr::write_unaligned(array_ptr, ArrayData::new(tab, elements.len() as u32));
        }
        array
    }

    fn object_proto(obj: JSValue) -> JSValue {
        let obj_ptr = obj.to_ptr::<Object>().expect("object ptr");
        unsafe {
            // SAFETY: obj_ptr points at a readable object proto slot.
            ptr::read_unaligned(Object::proto_ptr(obj_ptr.as_ptr()))
        }
    }

    #[test]
    fn call_simple_add() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let result = call_bytecode(
            &mut ctx,
            vec![OP_PUSH_1.as_u8(), OP_PUSH_2.as_u8(), OP_ADD.as_u8(), OP_RETURN.as_u8()],
        );
        assert!(result.is_int());
        assert_eq!(result.get_int(), 3);
    }

    #[test]
    fn call_add_string_concat() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let str_val = ctx.new_string("hi").expect("string");
        let bytecode = vec![
            OP_PUSH_CONST.as_u8(),
            0,
            0,
            OP_PUSH_1.as_u8(),
            OP_ADD.as_u8(),
            OP_RETURN.as_u8(),
        ];
        let result = call_bytecode_with_cpool(&mut ctx, bytecode, vec![str_val]);
        assert_eq!(string_from_value(result), "hi1");
    }

    #[test]
    fn call_eq_and_strict_eq() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let str_val = ctx.new_string("1").expect("string");
        let bytecode = vec![
            OP_PUSH_CONST.as_u8(),
            0,
            0,
            OP_PUSH_1.as_u8(),
            OP_EQ.as_u8(),
            OP_RETURN.as_u8(),
        ];
        let result = call_bytecode_with_cpool(&mut ctx, bytecode, vec![str_val]);
        assert_eq!(result.get_special_value(), 1);

        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let str_val = ctx.new_string("1").expect("string");
        let bytecode = vec![
            OP_PUSH_CONST.as_u8(),
            0,
            0,
            OP_PUSH_1.as_u8(),
            OP_STRICT_EQ.as_u8(),
            OP_RETURN.as_u8(),
        ];
        let result = call_bytecode_with_cpool(&mut ctx, bytecode, vec![str_val]);
        assert_eq!(result.get_special_value(), 0);
    }

    #[test]
    fn call_relational_string_compare() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let a = ctx.new_string("a").expect("string");
        let b = ctx.new_string("b").expect("string");
        let bytecode = vec![
            OP_PUSH_CONST.as_u8(),
            0,
            0,
            OP_PUSH_CONST.as_u8(),
            1,
            0,
            OP_LT.as_u8(),
            OP_RETURN.as_u8(),
        ];
        let result = call_bytecode_with_cpool(&mut ctx, bytecode, vec![a, b]);
        assert_eq!(result.get_special_value(), 1);
    }

    #[test]
    fn call_bitwise_and() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let result = call_bytecode(
            &mut ctx,
            vec![OP_PUSH_3.as_u8(), OP_PUSH_1.as_u8(), OP_AND.as_u8(), OP_RETURN.as_u8()],
        );
        assert!(result.is_int());
        assert_eq!(result.get_int(), 1);
    }

    #[test]
    fn control_flow_if_false_goto() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let mut bytecode = Vec::new();
        bytecode.push(OP_PUSH_TRUE.as_u8());
        bytecode.push(OP_IF_FALSE.as_u8());
        emit_i32(&mut bytecode, 10);
        bytecode.push(OP_PUSH_1.as_u8());
        bytecode.push(OP_GOTO.as_u8());
        emit_i32(&mut bytecode, 5);
        bytecode.push(OP_PUSH_2.as_u8());
        bytecode.push(OP_RETURN.as_u8());
        let result = call_bytecode(&mut ctx, bytecode);
        assert!(result.is_int());
        assert_eq!(result.get_int(), 1);
    }

    #[test]
    fn control_flow_if_true() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let mut bytecode = Vec::new();
        bytecode.push(OP_PUSH_TRUE.as_u8());
        bytecode.push(OP_IF_TRUE.as_u8());
        emit_i32(&mut bytecode, 6);
        bytecode.push(OP_PUSH_1.as_u8());
        bytecode.push(OP_RETURN.as_u8());
        bytecode.push(OP_PUSH_2.as_u8());
        bytecode.push(OP_RETURN.as_u8());
        let result = call_bytecode(&mut ctx, bytecode);
        assert!(result.is_int());
        assert_eq!(result.get_int(), 2);
    }

    #[test]
    fn control_flow_gosub_ret() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let mut bytecode = Vec::new();
        bytecode.push(OP_PUSH_1.as_u8());
        bytecode.push(OP_GOSUB.as_u8());
        emit_i32(&mut bytecode, 7);
        bytecode.push(OP_PUSH_2.as_u8());
        bytecode.push(OP_ADD.as_u8());
        bytecode.push(OP_RETURN.as_u8());
        bytecode.push(OP_RET.as_u8());
        let result = call_bytecode(&mut ctx, bytecode);
        assert!(result.is_int());
        assert_eq!(result.get_int(), 3);
    }

    #[test]
    fn exception_flow_catch_throw() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let mut bytecode = Vec::new();
        bytecode.push(OP_CATCH.as_u8());
        emit_i32(&mut bytecode, 8);
        bytecode.push(OP_PUSH_1.as_u8());
        bytecode.push(OP_THROW.as_u8());
        bytecode.push(OP_PUSH_2.as_u8());
        bytecode.push(OP_RETURN.as_u8());
        bytecode.push(OP_RETURN.as_u8());
        let result = call_bytecode(&mut ctx, bytecode);
        assert!(result.is_int());
        assert_eq!(result.get_int(), 1);
    }

    #[test]
    fn stack_ops_shuffle() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let result = call_bytecode(
            &mut ctx,
            vec![OP_PUSH_1.as_u8(), OP_PUSH_2.as_u8(), OP_NIP.as_u8(), OP_RETURN.as_u8()],
        );
        assert!(result.is_int());
        assert_eq!(result.get_int(), 2);

        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let result = call_bytecode(
            &mut ctx,
            vec![
                OP_PUSH_1.as_u8(),
                OP_PUSH_2.as_u8(),
                OP_DUP2.as_u8(),
                OP_ADD.as_u8(),
                OP_ADD.as_u8(),
                OP_ADD.as_u8(),
                OP_RETURN.as_u8(),
            ],
        );
        assert!(result.is_int());
        assert_eq!(result.get_int(), 6);

        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let result = call_bytecode(
            &mut ctx,
            vec![
                OP_PUSH_1.as_u8(),
                OP_PUSH_2.as_u8(),
                OP_INSERT2.as_u8(),
                OP_ADD.as_u8(),
                OP_ADD.as_u8(),
                OP_RETURN.as_u8(),
            ],
        );
        assert!(result.is_int());
        assert_eq!(result.get_int(), 5);

        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let result = call_bytecode(
            &mut ctx,
            vec![
                OP_PUSH_1.as_u8(),
                OP_PUSH_2.as_u8(),
                OP_PUSH_3.as_u8(),
                OP_INSERT3.as_u8(),
                OP_ADD.as_u8(),
                OP_ADD.as_u8(),
                OP_ADD.as_u8(),
                OP_RETURN.as_u8(),
            ],
        );
        assert!(result.is_int());
        assert_eq!(result.get_int(), 9);

        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let result = call_bytecode(
            &mut ctx,
            vec![
                OP_PUSH_1.as_u8(),
                OP_PUSH_2.as_u8(),
                OP_PUSH_3.as_u8(),
                OP_PERM3.as_u8(),
                OP_SUB.as_u8(),
                OP_RETURN.as_u8(),
            ],
        );
        assert!(result.is_int());
        assert_eq!(result.get_int(), -2);

        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let result = call_bytecode(
            &mut ctx,
            vec![
                OP_PUSH_1.as_u8(),
                OP_PUSH_2.as_u8(),
                OP_PUSH_3.as_u8(),
                OP_ROT3L.as_u8(),
                OP_SUB.as_u8(),
                OP_RETURN.as_u8(),
            ],
        );
        assert!(result.is_int());
        assert_eq!(result.get_int(), 2);

        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let result = call_bytecode(
            &mut ctx,
            vec![
                OP_PUSH_1.as_u8(),
                OP_PUSH_2.as_u8(),
                OP_PUSH_3.as_u8(),
                OP_PUSH_4.as_u8(),
                OP_PERM4.as_u8(),
                OP_SUB.as_u8(),
                OP_RETURN.as_u8(),
            ],
        );
        assert!(result.is_int());
        assert_eq!(result.get_int(), -2);
    }

    #[test]
    fn get_field_reads_property() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let obj = ctx
            .alloc_object(JSObjectClass::Object, JSValue::JS_NULL, 0)
            .expect("object");
        let key = ctx.intern_string(b"answer").expect("atom");
        define_property_value(&mut ctx, obj, key, JSValue::new_short_int(42)).expect("define");

        let mut bytecode = Vec::new();
        bytecode.push(OP_PUSH_CONST.as_u8());
        emit_u16(&mut bytecode, 0);
        bytecode.push(OP_GET_FIELD.as_u8());
        emit_u16(&mut bytecode, 1);
        bytecode.push(OP_RETURN.as_u8());

        let result = call_bytecode_with_cpool(&mut ctx, bytecode, vec![obj, key]);
        assert!(result.is_int());
        assert_eq!(result.get_int(), 42);
    }

    #[test]
    fn get_field2_keeps_object() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let obj = ctx
            .alloc_object(JSObjectClass::Object, JSValue::JS_NULL, 0)
            .expect("object");
        let key = ctx.intern_string(b"foo").expect("atom");
        define_property_value(&mut ctx, obj, key, JSValue::new_short_int(7)).expect("define");

        let mut bytecode = Vec::new();
        bytecode.push(OP_PUSH_CONST.as_u8());
        emit_u16(&mut bytecode, 0);
        bytecode.push(OP_GET_FIELD2.as_u8());
        emit_u16(&mut bytecode, 1);
        bytecode.push(OP_SWAP.as_u8());
        bytecode.push(OP_RETURN.as_u8());

        let result = call_bytecode_with_cpool(&mut ctx, bytecode, vec![obj, key]);
        assert_eq!(result, obj);
    }

    #[test]
    fn put_field_writes_property() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let obj = ctx
            .alloc_object(JSObjectClass::Object, JSValue::JS_NULL, 0)
            .expect("object");
        let key = ctx.intern_string(b"x").expect("atom");

        let mut bytecode = Vec::new();
        bytecode.push(OP_PUSH_CONST.as_u8());
        emit_u16(&mut bytecode, 0);
        bytecode.push(OP_PUSH_5.as_u8());
        bytecode.push(OP_PUT_FIELD.as_u8());
        emit_u16(&mut bytecode, 1);
        bytecode.push(OP_PUSH_CONST.as_u8());
        emit_u16(&mut bytecode, 0);
        bytecode.push(OP_GET_FIELD.as_u8());
        emit_u16(&mut bytecode, 1);
        bytecode.push(OP_RETURN.as_u8());

        let result = call_bytecode_with_cpool(&mut ctx, bytecode, vec![obj, key]);
        assert!(result.is_int());
        assert_eq!(result.get_int(), 5);
    }

    #[test]
    fn define_field_sets_property() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let obj = ctx
            .alloc_object(JSObjectClass::Object, JSValue::JS_NULL, 0)
            .expect("object");
        let key = ctx.intern_string(b"y").expect("atom");

        let mut bytecode = Vec::new();
        bytecode.push(OP_PUSH_CONST.as_u8());
        emit_u16(&mut bytecode, 0);
        bytecode.push(OP_PUSH_3.as_u8());
        bytecode.push(OP_DEFINE_FIELD.as_u8());
        emit_u16(&mut bytecode, 1);
        bytecode.push(OP_GET_FIELD.as_u8());
        emit_u16(&mut bytecode, 1);
        bytecode.push(OP_RETURN.as_u8());

        let result = call_bytecode_with_cpool(&mut ctx, bytecode, vec![obj, key]);
        assert!(result.is_int());
        assert_eq!(result.get_int(), 3);
    }

    #[test]
    fn object_preallocates_props() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");

        let mut bytecode = Vec::new();
        bytecode.push(OP_OBJECT.as_u8());
        emit_u16(&mut bytecode, 2);
        bytecode.push(OP_RETURN.as_u8());

        let result = call_bytecode(&mut ctx, bytecode);
        let obj_ptr = object_ptr(result).expect("object");
        let props = object_props(obj_ptr);
        assert_ne!(props, ctx.empty_props());
    }

    #[test]
    fn object_zero_count_uses_empty_props() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");

        let mut bytecode = Vec::new();
        bytecode.push(OP_OBJECT.as_u8());
        emit_u16(&mut bytecode, 0);
        bytecode.push(OP_RETURN.as_u8());

        let result = call_bytecode(&mut ctx, bytecode);
        let obj_ptr = object_ptr(result).expect("object");
        let props = object_props(obj_ptr);
        assert_eq!(props, ctx.empty_props());
    }

    #[test]
    fn define_getter_sets_getset_entry() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let obj = ctx
            .alloc_object(JSObjectClass::Object, JSValue::JS_NULL, 0)
            .expect("object");
        let key = ctx.intern_string(b"g").expect("atom");

        let mut bytecode = Vec::new();
        bytecode.push(OP_PUSH_CONST.as_u8());
        emit_u16(&mut bytecode, 0);
        bytecode.push(OP_PUSH_I8.as_u8());
        bytecode.push(11);
        bytecode.push(OP_DEFINE_GETTER.as_u8());
        emit_u16(&mut bytecode, 1);
        bytecode.push(OP_RETURN.as_u8());

        let result = call_bytecode_with_cpool(&mut ctx, bytecode, vec![obj, key]);
        assert_eq!(result, obj);

        let entry = debug_property(&ctx, obj, key)
            .expect("debug property")
            .expect("property");
        match entry {
            DebugProperty::GetSet { getter, setter } => {
                assert!(getter.is_int());
                assert_eq!(getter.get_int(), 11);
                assert_eq!(setter, JSValue::JS_UNDEFINED);
            }
            _ => panic!("expected get/set entry"),
        }
    }

    #[test]
    fn define_setter_sets_getset_entry() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let obj = ctx
            .alloc_object(JSObjectClass::Object, JSValue::JS_NULL, 0)
            .expect("object");
        let key = ctx.intern_string(b"s").expect("atom");

        let mut bytecode = Vec::new();
        bytecode.push(OP_PUSH_CONST.as_u8());
        emit_u16(&mut bytecode, 0);
        bytecode.push(OP_PUSH_I8.as_u8());
        bytecode.push(22);
        bytecode.push(OP_DEFINE_SETTER.as_u8());
        emit_u16(&mut bytecode, 1);
        bytecode.push(OP_RETURN.as_u8());

        let result = call_bytecode_with_cpool(&mut ctx, bytecode, vec![obj, key]);
        assert_eq!(result, obj);

        let entry = debug_property(&ctx, obj, key)
            .expect("debug property")
            .expect("property");
        match entry {
            DebugProperty::GetSet { getter, setter } => {
                assert_eq!(getter, JSValue::JS_UNDEFINED);
                assert!(setter.is_int());
                assert_eq!(setter.get_int(), 22);
            }
            _ => panic!("expected get/set entry"),
        }
    }

    #[test]
    fn get_array_el_reads_element() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let array = new_array(
            &mut ctx,
            &[JSValue::new_short_int(10), JSValue::new_short_int(20)],
        );

        let bytecode = vec![
            OP_PUSH_CONST.as_u8(),
            0,
            0,
            OP_PUSH_1.as_u8(),
            OP_GET_ARRAY_EL.as_u8(),
            OP_RETURN.as_u8(),
        ];

        let result = call_bytecode_with_cpool(&mut ctx, bytecode, vec![array]);
        assert!(result.is_int());
        assert_eq!(result.get_int(), 20);
    }

    #[test]
    fn get_array_el2_keeps_object() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let array = new_array(&mut ctx, &[JSValue::new_short_int(7)]);

        let bytecode = vec![
            OP_PUSH_CONST.as_u8(),
            0,
            0,
            OP_PUSH_0.as_u8(),
            OP_GET_ARRAY_EL2.as_u8(),
            OP_SWAP.as_u8(),
            OP_RETURN.as_u8(),
        ];

        let result = call_bytecode_with_cpool(&mut ctx, bytecode, vec![array]);
        assert_eq!(result, array);
    }

    #[test]
    fn put_array_el_writes_element() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let array = new_array(&mut ctx, &[JSValue::new_short_int(0)]);

        let bytecode = vec![
            OP_PUSH_CONST.as_u8(),
            0,
            0,
            OP_PUSH_0.as_u8(),
            OP_PUSH_I8.as_u8(),
            7,
            OP_PUT_ARRAY_EL.as_u8(),
            OP_PUSH_CONST.as_u8(),
            0,
            0,
            OP_PUSH_0.as_u8(),
            OP_GET_ARRAY_EL.as_u8(),
            OP_RETURN.as_u8(),
        ];

        let result = call_bytecode_with_cpool(&mut ctx, bytecode, vec![array]);
        assert!(result.is_int());
        assert_eq!(result.get_int(), 7);
    }

    #[test]
    fn get_length_array() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let array = new_array(&mut ctx, &[JSValue::new_short_int(1), JSValue::new_short_int(2)]);

        let bytecode = vec![
            OP_PUSH_CONST.as_u8(),
            0,
            0,
            OP_GET_LENGTH.as_u8(),
            OP_RETURN.as_u8(),
        ];

        let result = call_bytecode_with_cpool(&mut ctx, bytecode, vec![array]);
        assert!(result.is_int());
        assert_eq!(result.get_int(), 2);
    }

    #[test]
    fn get_length_string() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let val = ctx.new_string("hi").expect("string");
        let bytecode = vec![
            OP_PUSH_CONST.as_u8(),
            0,
            0,
            OP_GET_LENGTH.as_u8(),
            OP_RETURN.as_u8(),
        ];

        let result = call_bytecode_with_cpool(&mut ctx, bytecode, vec![val]);
        assert!(result.is_int());
        assert_eq!(result.get_int(), 2);
    }

    #[test]
    fn get_length2_keeps_object() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let array = new_array(&mut ctx, &[JSValue::new_short_int(3)]);

        let bytecode = vec![
            OP_PUSH_CONST.as_u8(),
            0,
            0,
            OP_GET_LENGTH2.as_u8(),
            OP_SWAP.as_u8(),
            OP_RETURN.as_u8(),
        ];

        let result = call_bytecode_with_cpool(&mut ctx, bytecode, vec![array]);
        assert_eq!(result, array);
    }

    #[test]
    fn set_proto_sets_object_proto() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let obj = ctx
            .alloc_object(JSObjectClass::Object, JSValue::JS_NULL, 0)
            .expect("object");
        let proto = ctx
            .alloc_object(JSObjectClass::Object, JSValue::JS_NULL, 0)
            .expect("proto");

        let bytecode = vec![
            OP_PUSH_CONST.as_u8(),
            0,
            0,
            OP_PUSH_CONST.as_u8(),
            1,
            0,
            OP_SET_PROTO.as_u8(),
            OP_RETURN.as_u8(),
        ];

        let result = call_bytecode_with_cpool(&mut ctx, bytecode, vec![obj, proto]);
        assert_eq!(result, obj);
        assert_eq!(object_proto(obj), proto);
    }

    #[test]
    fn delete_property_removes_entry() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let obj = ctx
            .alloc_object(JSObjectClass::Object, JSValue::JS_NULL, 0)
            .expect("object");
        let key = ctx.intern_string(b"del").expect("atom");
        define_property_value(&mut ctx, obj, key, JSValue::new_short_int(1)).expect("define");

        let bytecode = vec![
            OP_PUSH_CONST.as_u8(),
            0,
            0,
            OP_PUSH_CONST.as_u8(),
            1,
            0,
            OP_DELETE.as_u8(),
            OP_RETURN.as_u8(),
        ];

        let result = call_bytecode_with_cpool(&mut ctx, bytecode, vec![obj, key]);
        assert_eq!(result.get_special_value(), 1);
        assert!(!has_property(&ctx, obj, key).expect("has"));
    }

    #[test]
    fn array_from_preserves_order() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let mut bytecode = Vec::new();
        bytecode.push(OP_PUSH_1.as_u8());
        bytecode.push(OP_PUSH_2.as_u8());
        bytecode.push(OP_PUSH_3.as_u8());
        bytecode.push(OP_ARRAY_FROM.as_u8());
        emit_u16(&mut bytecode, 3);
        bytecode.push(OP_PUSH_0.as_u8());
        bytecode.push(OP_GET_ARRAY_EL.as_u8());
        bytecode.push(OP_RETURN.as_u8());

        let result = call_bytecode(&mut ctx, bytecode);
        assert!(result.is_int());
        assert_eq!(result.get_int(), 1);
    }

    #[test]
    fn for_of_next_yields_value() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let array = new_array(&mut ctx, &[JSValue::new_short_int(7)]);
        let mut bytecode = Vec::new();
        bytecode.push(OP_PUSH_CONST.as_u8());
        emit_u16(&mut bytecode, 0);
        bytecode.push(OP_FOR_OF_START.as_u8());
        bytecode.push(OP_FOR_OF_NEXT.as_u8());
        bytecode.push(OP_IF_FALSE.as_u8());
        emit_i32(&mut bytecode, 6);
        bytecode.push(OP_PUSH_0.as_u8());
        bytecode.push(OP_RETURN.as_u8());
        bytecode.push(OP_NIP.as_u8());
        bytecode.push(OP_RETURN.as_u8());

        let result = call_bytecode_with_cpool(&mut ctx, bytecode, vec![array]);
        assert!(result.is_int());
        assert_eq!(result.get_int(), 7);
    }

    #[test]
    fn for_in_start_collects_keys() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let obj = ctx
            .alloc_object(JSObjectClass::Object, JSValue::JS_NULL, 0)
            .expect("object");
        let key = ctx.intern_string(b"k").expect("key");
        define_property_value(&mut ctx, obj, key, JSValue::new_short_int(1)).expect("define");

        let mut bytecode = Vec::new();
        bytecode.push(OP_PUSH_CONST.as_u8());
        emit_u16(&mut bytecode, 0);
        bytecode.push(OP_FOR_IN_START.as_u8());
        bytecode.push(OP_FOR_OF_NEXT.as_u8());
        bytecode.push(OP_IF_FALSE.as_u8());
        emit_i32(&mut bytecode, 6);
        bytecode.push(OP_PUSH_0.as_u8());
        bytecode.push(OP_RETURN.as_u8());
        bytecode.push(OP_NIP.as_u8());
        bytecode.push(OP_RETURN.as_u8());

        let result = call_bytecode_with_cpool(&mut ctx, bytecode, vec![obj]);
        let mut scratch = [0u8; 5];
        let view = string_view(result, &mut scratch).expect("key string");
        assert_eq!(view.bytes(), b"k");
    }

    #[test]
    fn regexp_opcode_builds_object() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let source = ctx.new_string("a").expect("source");
        let compiled = compile_regexp(b"a", 0).expect("regexp");
        let bytecode_val = ctx.alloc_byte_array(compiled.bytes()).expect("bytecode");
        let bytecode = vec![
            OP_PUSH_CONST.as_u8(),
            0,
            0,
            OP_PUSH_CONST.as_u8(),
            1,
            0,
            OP_REGEXP.as_u8(),
            OP_RETURN.as_u8(),
        ];
        let result = call_bytecode_with_cpool(&mut ctx, bytecode, vec![source, bytecode_val]);
        let obj_ptr = object_ptr(result).expect("regexp ptr");
        let header = object_header(obj_ptr);
        assert_eq!(header.class_id(), JSObjectClass::RegExp as u8);
        let payload = unsafe { Object::payload_ptr(obj_ptr.as_ptr()) };
        let regexp = unsafe { ptr::read_unaligned(core::ptr::addr_of!((*payload).regexp)) };
        assert_eq!(regexp.source(), source);
        assert_eq!(regexp.byte_code(), bytecode_val);
        assert_eq!(regexp.last_index(), 0);
    }

    #[test]
    fn arguments_len_matches_passed_args() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let bytecode = vec![OP_ARGUMENTS.as_u8(), OP_GET_LENGTH.as_u8(), OP_RETURN.as_u8()];
        let func = make_function_bytecode(
            &mut ctx,
            bytecode,
            Vec::new(),
            Vec::new(),
            Vec::new(),
            2,
            4,
        );
        let closure = ctx.alloc_closure(func, 0).expect("closure");
        let result = call(&mut ctx, closure, &[JSValue::new_short_int(7)]).expect("call");
        assert!(result.is_int());
        assert_eq!(result.get_int(), 1);
    }

    #[test]
    fn this_func_returns_closure() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let bytecode = vec![OP_THIS_FUNC.as_u8(), OP_RETURN.as_u8()];
        let func = make_function_bytecode(
            &mut ctx,
            bytecode,
            Vec::new(),
            Vec::new(),
            Vec::new(),
            0,
            2,
        );
        let closure = ctx.alloc_closure(func, 0).expect("closure");
        let result = call(&mut ctx, closure, &[]).expect("call");
        assert_eq!(result, closure);
    }

    #[test]
    fn push_this_returns_global() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let bytecode = vec![OP_PUSH_THIS.as_u8(), OP_RETURN.as_u8()];
        let func = make_function_bytecode(
            &mut ctx,
            bytecode,
            Vec::new(),
            Vec::new(),
            Vec::new(),
            0,
            2,
        );
        let closure = ctx.alloc_closure(func, 0).expect("closure");
        let result = call(&mut ctx, closure, &[]).expect("call");
        assert_eq!(result, ctx.global_obj());
    }

    #[test]
    fn new_target_tracks_constructor_calls() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let inner_code = vec![OP_NEW_TARGET.as_u8(), OP_RETURN.as_u8()];
        let inner_func = make_function_bytecode(
            &mut ctx,
            inner_code,
            Vec::new(),
            Vec::new(),
            Vec::new(),
            0,
            2,
        );
        let inner_closure = ctx.alloc_closure(inner_func, 0).expect("closure");
        let result = call(&mut ctx, inner_closure, &[]).expect("call");
        assert_eq!(result, JSValue::JS_UNDEFINED);

        let mut outer_code = Vec::new();
        outer_code.push(OP_FCLOSURE.as_u8());
        emit_u16(&mut outer_code, 0);
        outer_code.push(OP_CALL_CONSTRUCTOR.as_u8());
        emit_u16(&mut outer_code, 0);
        outer_code.push(OP_RETURN.as_u8());
        let outer_func = make_function_bytecode(
            &mut ctx,
            outer_code,
            vec![inner_func],
            Vec::new(),
            Vec::new(),
            0,
            2,
        );
        let outer_closure = ctx.alloc_closure(outer_func, 0).expect("closure");
        let result = call(&mut ctx, outer_closure, &[]).expect("call");
        let obj_ptr = object_ptr(result).expect("closure");
        let header = object_header(obj_ptr);
        assert_eq!(header.class_id(), JSObjectClass::Closure as u8);
        let payload = unsafe { Object::payload_ptr(obj_ptr.as_ptr()) };
        let closure = unsafe { core::ptr::addr_of_mut!((*payload).closure) };
        let func_val = unsafe { ptr::read_unaligned(ClosureData::func_bytecode_ptr(closure)) };
        assert_eq!(func_val, inner_func);
    }

    #[test]
    fn call_invokes_closure_with_args() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let callee_code = vec![
            OP_GET_ARG0.as_u8(),
            OP_GET_ARG1.as_u8(),
            OP_ADD.as_u8(),
            OP_RETURN.as_u8(),
        ];
        let callee_func = make_function_bytecode(
            &mut ctx,
            callee_code,
            Vec::new(),
            Vec::new(),
            Vec::new(),
            2,
            4,
        );
        let callee_closure = ctx.alloc_closure(callee_func, 0).expect("closure");

        let mut caller_code = Vec::new();
        caller_code.push(OP_PUSH_CONST.as_u8());
        emit_u16(&mut caller_code, 0);
        caller_code.push(OP_PUSH_1.as_u8());
        caller_code.push(OP_PUSH_2.as_u8());
        caller_code.push(OP_CALL.as_u8());
        emit_u16(&mut caller_code, 2);
        caller_code.push(OP_RETURN.as_u8());
        let caller_func = make_function_bytecode(
            &mut ctx,
            caller_code,
            vec![callee_closure],
            Vec::new(),
            Vec::new(),
            0,
            4,
        );
        let caller_closure = ctx.alloc_closure(caller_func, 0).expect("closure");
        let result = call(&mut ctx, caller_closure, &[]).expect("call");
        assert!(result.is_int());
        assert_eq!(result.get_int(), 3);
    }

    #[test]
    fn call_method_preserves_arg_order() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let callee_code = vec![OP_GET_ARG0.as_u8(), OP_RETURN.as_u8()];
        let callee_func = make_function_bytecode(
            &mut ctx,
            callee_code,
            Vec::new(),
            Vec::new(),
            Vec::new(),
            1,
            2,
        );
        let callee_closure = ctx.alloc_closure(callee_func, 0).expect("closure");
        let this_obj = ctx
            .alloc_object(JSObjectClass::Object, JSValue::JS_NULL, 0)
            .expect("object");

        let mut caller_code = Vec::new();
        caller_code.push(OP_PUSH_CONST.as_u8());
        emit_u16(&mut caller_code, 0);
        caller_code.push(OP_PUSH_CONST.as_u8());
        emit_u16(&mut caller_code, 1);
        caller_code.push(OP_PUSH_5.as_u8());
        caller_code.push(OP_CALL_METHOD.as_u8());
        emit_u16(&mut caller_code, 1);
        caller_code.push(OP_RETURN.as_u8());
        let caller_func = make_function_bytecode(
            &mut ctx,
            caller_code,
            vec![this_obj, callee_closure],
            Vec::new(),
            Vec::new(),
            0,
            4,
        );
        let caller_closure = ctx.alloc_closure(caller_func, 0).expect("closure");
        let result = call(&mut ctx, caller_closure, &[]).expect("call");
        assert!(result.is_int());
        assert_eq!(result.get_int(), 5);
    }

    #[test]
    fn call_constructor_uses_function_prototype() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let callee_code = vec![OP_PUSH_1.as_u8(), OP_RETURN.as_u8()];
        let callee_func = make_function_bytecode(
            &mut ctx,
            callee_code,
            Vec::new(),
            Vec::new(),
            Vec::new(),
            0,
            2,
        );
        let callee_closure = ctx.alloc_closure(callee_func, 0).expect("closure");
        let proto = ctx
            .alloc_object(JSObjectClass::Object, JSValue::JS_NULL, 0)
            .expect("proto");
        let proto_key = ctx.intern_string(b"prototype").expect("atom");
        define_property_value(&mut ctx, callee_closure, proto_key, proto).expect("define");

        let mut caller_code = Vec::new();
        caller_code.push(OP_PUSH_CONST.as_u8());
        emit_u16(&mut caller_code, 0);
        caller_code.push(OP_CALL_CONSTRUCTOR.as_u8());
        emit_u16(&mut caller_code, 0);
        caller_code.push(OP_RETURN.as_u8());
        let caller_func = make_function_bytecode(
            &mut ctx,
            caller_code,
            vec![callee_closure],
            Vec::new(),
            Vec::new(),
            0,
            2,
        );
        let caller_closure = ctx.alloc_closure(caller_func, 0).expect("closure");
        let result = call(&mut ctx, caller_closure, &[]).expect("call");
        assert_eq!(object_proto(result), proto);
    }

    #[test]
    fn fclosure_varref_roundtrip() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let name = ctx.intern_string(b"x").expect("atom");
        let decl = (JSVarRefKind::Arg as i32) << 16;
        let ext_vars = vec![name, JSValue::new_short_int(decl)];

        let mut inner_code = Vec::new();
        inner_code.push(OP_GET_VAR_REF.as_u8());
        emit_u16(&mut inner_code, 0);
        inner_code.push(OP_PUSH_1.as_u8());
        inner_code.push(OP_ADD.as_u8());
        inner_code.push(OP_PUT_VAR_REF.as_u8());
        emit_u16(&mut inner_code, 0);
        inner_code.push(OP_GET_VAR_REF.as_u8());
        emit_u16(&mut inner_code, 0);
        inner_code.push(OP_RETURN.as_u8());
        let inner_func = make_function_bytecode(
            &mut ctx,
            inner_code,
            Vec::new(),
            Vec::new(),
            ext_vars,
            0,
            4,
        );

        let mut outer_code = Vec::new();
        outer_code.push(OP_FCLOSURE.as_u8());
        emit_u16(&mut outer_code, 0);
        outer_code.push(OP_RETURN.as_u8());
        let outer_func = make_function_bytecode(
            &mut ctx,
            outer_code,
            vec![inner_func],
            Vec::new(),
            Vec::new(),
            1,
            2,
        );
        let outer_closure = ctx.alloc_closure(outer_func, 0).expect("closure");
        let inner_closure =
            call(&mut ctx, outer_closure, &[JSValue::new_short_int(10)]).expect("call");

        let result = call(&mut ctx, inner_closure, &[]).expect("call");
        assert!(result.is_int());
        assert_eq!(result.get_int(), 11);

        let result = call(&mut ctx, inner_closure, &[]).expect("call");
        assert!(result.is_int());
        assert_eq!(result.get_int(), 12);
    }

    #[test]
    fn var_ref_nocheck_allows_uninitialized() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let name = ctx.intern_string(b"missing").expect("atom");
        let decl = (JSVarRefKind::Global as i32) << 16;
        let ext_vars = vec![name, JSValue::new_short_int(decl)];

        let mut code = Vec::new();
        code.push(OP_PUSH_1.as_u8());
        code.push(OP_PUT_VAR_REF_NOCHECK.as_u8());
        emit_u16(&mut code, 0);
        code.push(OP_GET_VAR_REF_NOCHECK.as_u8());
        emit_u16(&mut code, 0);
        code.push(OP_RETURN.as_u8());
        let func = make_function_bytecode(
            &mut ctx,
            code,
            Vec::new(),
            Vec::new(),
            ext_vars,
            0,
            4,
        );
        let closure = create_closure(&mut ctx, func, None).expect("closure");
        let result = call(&mut ctx, closure, &[]).expect("call");
        assert!(result.is_int());
        assert_eq!(result.get_int(), 1);
    }

    #[test]
    fn var_ref_checked_errors_on_uninitialized() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let name = ctx.intern_string(b"missing").expect("atom");
        let decl = (JSVarRefKind::Global as i32) << 16;
        let ext_vars = vec![name, JSValue::new_short_int(decl)];

        let mut code = Vec::new();
        code.push(OP_GET_VAR_REF.as_u8());
        emit_u16(&mut code, 0);
        code.push(OP_RETURN.as_u8());
        let func = make_function_bytecode(
            &mut ctx,
            code,
            Vec::new(),
            Vec::new(),
            ext_vars,
            0,
            2,
        );
        let closure = create_closure(&mut ctx, func, None).expect("closure");
        let err = call(&mut ctx, closure, &[]).unwrap_err();
        match err {
            InterpreterError::Thrown(val) => {
                assert!(val.is_error());
                let msg_val = ctx.get_error_message(val).expect("message");
                let msg = js_to_cstring(&mut ctx, msg_val).expect("message string");
                assert_eq!(msg, "varref uninitialized");
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }
}
