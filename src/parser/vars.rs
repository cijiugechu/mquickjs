use core::cmp::min;
use core::ptr::NonNull;

use crate::cutils::{get_u16, put_u16};
use crate::enums::JSVarRefKind;
use crate::function_bytecode::FunctionBytecode;
use crate::jsvalue::{
    new_short_int, value_from_ptr, value_to_ptr, JSValue, JS_NULL, JS_UNDEFINED,
};
use crate::opcode::{
    OpCode, OPCODES, OP_GET_LOC, OP_GET_VAR_REF, OP_GET_VAR_REF_NOCHECK, OP_PUT_ARG, OP_PUT_LOC,
    OP_PUT_VAR_REF_NOCHECK, OP_PUT_VAR_REF,
};

use super::emit::BytecodeEmitter;
use super::parse_state::JSParseState;
use super::types::SourcePos;

pub const JS_MAX_LOCAL_VARS: u32 = u16::MAX as u32;
const CVT_VAR_SIZE_MAX: usize = 16;

const ERR_TOO_MANY_LOCAL_VARS: &str = "too many local variables";
const ERR_TOO_MANY_VAR_REFS: &str = "too many variable references";
const ERR_INVALID_VAR_INDEX: &str = "invalid variable index";
const ERR_INVALID_FUNC_PTR: &str = "invalid function bytecode pointer";
const ERR_INVALID_ARRAY_PTR: &str = "invalid value array pointer";
const ERR_INVALID_BYTECODE_PTR: &str = "invalid bytecode pointer";

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VarError {
    message: &'static str,
}

impl VarError {
    fn new(message: &'static str) -> Self {
        Self { message }
    }

    pub fn message(&self) -> &'static str {
        self.message
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValueArray {
    values: Vec<JSValue>,
}

impl ValueArray {
    pub fn new(size: usize) -> Self {
        Self {
            values: vec![JS_UNDEFINED; size],
        }
    }

    pub fn size(&self) -> usize {
        self.values.len()
    }

    pub fn values(&self) -> &[JSValue] {
        &self.values
    }

    pub fn values_mut(&mut self) -> &mut [JSValue] {
        &mut self.values
    }

    fn ensure_size(&mut self, new_size: usize) {
        let old_size = self.values.len();
        if new_size <= old_size {
            return;
        }
        let grow = old_size + old_size / 2;
        let target = new_size.max(grow);
        self.values.resize(target, JS_UNDEFINED);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ByteArray {
    buf: Vec<u8>,
}

impl ByteArray {
    pub fn new(buf: Vec<u8>) -> Self {
        Self { buf }
    }

    pub fn len(&self) -> usize {
        self.buf.len()
    }

    pub fn is_empty(&self) -> bool {
        self.buf.is_empty()
    }

    pub fn buf_mut(&mut self) -> &mut [u8] {
        &mut self.buf
    }
}

#[derive(Debug, Default)]
pub struct VarAllocator {
    value_arrays: Vec<NonNull<ValueArray>>,
    byte_arrays: Vec<NonNull<ByteArray>>,
}

impl VarAllocator {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn alloc_value_array(&mut self, size: usize) -> JSValue {
        let boxed = Box::new(ValueArray::new(size));
        // SAFETY: Box never yields a null pointer.
        let ptr = unsafe { NonNull::new_unchecked(Box::into_raw(boxed)) };
        self.value_arrays.push(ptr);
        value_from_ptr(ptr)
    }

    pub fn alloc_byte_array(&mut self, buf: Vec<u8>) -> JSValue {
        let boxed = Box::new(ByteArray::new(buf));
        // SAFETY: Box never yields a null pointer.
        let ptr = unsafe { NonNull::new_unchecked(Box::into_raw(boxed)) };
        self.byte_arrays.push(ptr);
        value_from_ptr(ptr)
    }
}

impl Drop for VarAllocator {
    fn drop(&mut self) {
        for ptr in self.value_arrays.drain(..) {
            // SAFETY: pointers were created from Box::into_raw in this allocator.
            unsafe { drop(Box::from_raw(ptr.as_ptr())) };
        }
        for ptr in self.byte_arrays.drain(..) {
            // SAFETY: pointers were created from Box::into_raw in this allocator.
            unsafe { drop(Box::from_raw(ptr.as_ptr())) };
        }
    }
}

fn value_array_ptr(val: JSValue) -> Result<NonNull<ValueArray>, VarError> {
    value_to_ptr::<ValueArray>(val).ok_or_else(|| VarError::new(ERR_INVALID_ARRAY_PTR))
}

fn byte_array_ptr(val: JSValue) -> Result<NonNull<ByteArray>, VarError> {
    value_to_ptr::<ByteArray>(val).ok_or_else(|| VarError::new(ERR_INVALID_BYTECODE_PTR))
}

fn func_from_value(val: JSValue) -> Result<NonNull<FunctionBytecode>, VarError> {
    value_to_ptr::<FunctionBytecode>(val).ok_or_else(|| VarError::new(ERR_INVALID_FUNC_PTR))
}

fn resize_value_array(
    alloc: &mut VarAllocator,
    val: JSValue,
    new_size: usize,
) -> Result<JSValue, VarError> {
    if val == JS_NULL {
        return Ok(alloc.alloc_value_array(new_size));
    }
    let mut array_ptr = value_array_ptr(val)?;
    // SAFETY: caller ensures the JSValue points to a live ValueArray and no other
    // mutable references are active during this borrow.
    let array = unsafe { array_ptr.as_mut() };
    array.ensure_size(new_size);
    Ok(val)
}

fn pack_decl(kind: JSVarRefKind, idx: i32) -> i32 {
    debug_assert!((0..=0xffff).contains(&idx));
    ((kind as i32) << 16) | (idx & 0xffff)
}

/* return the local variable index or -1 if not found */
pub fn find_func_var(func: JSValue, name: JSValue) -> i32 {
    let Some(func_ptr) = value_to_ptr::<FunctionBytecode>(func) else {
        return -1;
    };
    // SAFETY: caller provides a valid function bytecode pointer.
    let func_ref = unsafe { func_ptr.as_ref() };
    if func_ref.vars() == JS_NULL {
        return -1;
    }
    let Some(arr_ptr) = value_to_ptr::<ValueArray>(func_ref.vars()) else {
        return -1;
    };
    // SAFETY: vars points to a live ValueArray.
    let arr = unsafe { arr_ptr.as_ref() };
    for (idx, value) in arr.values().iter().enumerate() {
        if *value == name {
            return idx as i32;
        }
    }
    -1
}

pub fn find_var(state: &JSParseState, name: JSValue) -> i32 {
    let local_len = state.local_vars_len() as usize;
    if local_len == 0 {
        return -1;
    }
    let func_ptr = match value_to_ptr::<FunctionBytecode>(state.cur_func()) {
        Some(ptr) => ptr,
        None => return -1,
    };
    // SAFETY: state.cur_func points to a live FunctionBytecode.
    let func_ref = unsafe { func_ptr.as_ref() };
    let vars_val = func_ref.vars();
    let Some(arr_ptr) = value_to_ptr::<ValueArray>(vars_val) else {
        debug_assert!(local_len == 0);
        return -1;
    };
    // SAFETY: vars points to a live ValueArray.
    let arr = unsafe { arr_ptr.as_ref() };
    debug_assert!(arr.size() >= local_len);
    for (idx, value) in arr.values().iter().take(local_len).enumerate() {
        if *value == name {
            return idx as i32;
        }
    }
    -1
}

pub fn get_ext_var_name(state: &JSParseState, var_idx: usize) -> Option<JSValue> {
    let func_ptr = value_to_ptr::<FunctionBytecode>(state.cur_func())?;
    // SAFETY: state.cur_func points to a live FunctionBytecode.
    let func_ref = unsafe { func_ptr.as_ref() };
    let ext_vars_val = func_ref.ext_vars();
    let arr_ptr = value_to_ptr::<ValueArray>(ext_vars_val)?;
    // SAFETY: ext_vars points to a live ValueArray.
    let arr = unsafe { arr_ptr.as_ref() };
    arr.values().get(2 * var_idx).copied()
}

/* return the external variable index or -1 if not found */
pub fn find_func_ext_var(func: JSValue, name: JSValue) -> i32 {
    let Some(func_ptr) = value_to_ptr::<FunctionBytecode>(func) else {
        return -1;
    };
    // SAFETY: caller provides a valid function bytecode pointer.
    let func_ref = unsafe { func_ptr.as_ref() };
    let ext_len = func_ref.ext_vars_len() as usize;
    if ext_len == 0 {
        return -1;
    }
    let Some(arr_ptr) = value_to_ptr::<ValueArray>(func_ref.ext_vars()) else {
        debug_assert!(ext_len == 0);
        return -1;
    };
    // SAFETY: ext_vars points to a live ValueArray.
    let arr = unsafe { arr_ptr.as_ref() };
    debug_assert!(arr.size() >= ext_len * 2);
    for i in 0..ext_len {
        if arr.values()[2 * i] == name {
            return i as i32;
        }
    }
    -1
}

pub fn find_ext_var(state: &JSParseState, name: JSValue) -> i32 {
    find_func_ext_var(state.cur_func(), name)
}

/* return the external variable index */
pub fn add_func_ext_var(
    alloc: &mut VarAllocator,
    func: JSValue,
    name: JSValue,
    decl: i32,
) -> Result<i32, VarError> {
    let mut func_ptr = func_from_value(func)?;
    // SAFETY: caller provides a valid function bytecode pointer.
    let func_ref = unsafe { func_ptr.as_mut() };
    let ext_len = func_ref.ext_vars_len() as u32;
    if ext_len >= JS_MAX_LOCAL_VARS {
        return Err(VarError::new(ERR_TOO_MANY_VAR_REFS));
    }
    let new_size = ((ext_len + 1).max(2) * 2) as usize;
    let new_ext_vars = resize_value_array(alloc, func_ref.ext_vars(), new_size)?;
    func_ref.set_ext_vars(new_ext_vars);
    let mut arr_ptr = value_array_ptr(new_ext_vars)?;
    // SAFETY: the new_ext_vars value array is owned by the allocator and not aliased here.
    let arr = unsafe { arr_ptr.as_mut() };
    let idx = ext_len as usize;
    debug_assert!(arr.size() >= (idx + 1) * 2);
    arr.values_mut()[2 * idx] = name;
    arr.values_mut()[2 * idx + 1] = new_short_int(decl);
    let new_len = ext_len + 1;
    func_ref.set_ext_vars_len(new_len as u16);
    Ok(new_len as i32 - 1)
}

/* return the external variable index */
pub fn add_ext_var(
    state: &JSParseState,
    alloc: &mut VarAllocator,
    name: JSValue,
    decl: i32,
) -> Result<i32, VarError> {
    add_func_ext_var(alloc, state.cur_func(), name, decl)
}

/* return the local variable index */
pub fn add_var(
    state: &mut JSParseState,
    alloc: &mut VarAllocator,
    name: JSValue,
) -> Result<i32, VarError> {
    let local_len = state.local_vars_len() as u32;
    if local_len >= JS_MAX_LOCAL_VARS {
        return Err(VarError::new(ERR_TOO_MANY_LOCAL_VARS));
    }
    let mut func_ptr = func_from_value(state.cur_func())?;
    // SAFETY: state.cur_func points to a live FunctionBytecode.
    let func_ref = unsafe { func_ptr.as_mut() };
    let new_size = (local_len + 1).max(4) as usize;
    let new_vars = resize_value_array(alloc, func_ref.vars(), new_size)?;
    func_ref.set_vars(new_vars);
    let mut arr_ptr = value_array_ptr(new_vars)?;
    // SAFETY: the new_vars value array is owned by the allocator and not aliased here.
    let arr = unsafe { arr_ptr.as_mut() };
    let idx = local_len as usize;
    debug_assert!(arr.size() > idx);
    arr.values_mut()[idx] = name;
    state.set_local_vars_len((local_len + 1) as u16);
    Ok(local_len as i32)
}

pub fn define_var(
    state: &mut JSParseState,
    alloc: &mut VarAllocator,
    name: JSValue,
) -> Result<(JSVarRefKind, i32), VarError> {
    if state.is_eval() {
        let mut var_idx = find_ext_var(state, name);
        let decl = ((JSVarRefKind::Global as i32) << 16) | 1;
        if var_idx < 0 {
            var_idx = add_ext_var(state, alloc, name, decl)?;
        } else {
            let func_ptr = func_from_value(state.cur_func())?;
            // SAFETY: state.cur_func points to a live FunctionBytecode.
            let func_ref = unsafe { func_ptr.as_ref() };
            let mut arr_ptr = value_array_ptr(func_ref.ext_vars())?;
            // SAFETY: ext_vars points to a live ValueArray and is uniquely borrowed here.
            let arr = unsafe { arr_ptr.as_mut() };
            let idx = var_idx as usize;
            debug_assert!(arr.size() > idx * 2 + 1);
            arr.values_mut()[2 * idx + 1] = new_short_int(decl);
        }
        return Ok((JSVarRefKind::VarRef, var_idx));
    }

    let arg_count = {
        let func_ptr = func_from_value(state.cur_func())?;
        // SAFETY: state.cur_func points to a live FunctionBytecode.
        unsafe { func_ptr.as_ref() }.arg_count() as i32
    };
    let var_idx = find_var(state, name);
    if var_idx >= 0 {
        if var_idx < arg_count {
            Ok((JSVarRefKind::Arg, var_idx))
        } else {
            Ok((JSVarRefKind::Var, var_idx - arg_count))
        }
    } else {
        let var_idx = add_var(state, alloc, name)?;
        Ok((JSVarRefKind::Var, var_idx - arg_count))
    }
}

pub fn put_var(
    emitter: &mut BytecodeEmitter,
    var_kind: JSVarRefKind,
    var_idx: i32,
    source_pos: SourcePos,
) -> Result<(), VarError> {
    let opcode = match var_kind {
        JSVarRefKind::Arg => OP_PUT_ARG,
        JSVarRefKind::Var => OP_PUT_LOC,
        _ => OP_PUT_VAR_REF_NOCHECK,
    };
    let idx = u32::try_from(var_idx).map_err(|_| VarError::new(ERR_INVALID_VAR_INDEX))?;
    emitter.emit_var(opcode, idx, source_pos);
    Ok(())
}

#[derive(Copy, Clone, Debug, Default)]
struct ConvertVarEntry {
    new_var_idx: u16,
    is_local: bool,
}

fn convert_ext_vars_to_local_vars_bytecode(
    byte_code: &mut [u8],
    var_start: u32,
    cvt_tab: &[ConvertVarEntry],
) {
    let var_end = var_start + cvt_tab.len() as u32;
    let mut pos = 0usize;
    while pos < byte_code.len() {
        let op_val = byte_code[pos] as usize;
        debug_assert!(op_val < OPCODES.len());
        if op_val >= OPCODES.len() {
            break;
        }
        let op = OpCode(op_val as u16);
        match op {
            OP_GET_VAR_REF | OP_PUT_VAR_REF | OP_GET_VAR_REF_NOCHECK | OP_PUT_VAR_REF_NOCHECK => {
                let var_idx = get_u16(&byte_code[pos + 1..pos + 3]);
                if (var_start..var_end).contains(&var_idx) {
                    let idx = (var_idx - var_start) as usize;
                    let entry = cvt_tab[idx];
                    put_u16(&mut byte_code[pos + 1..pos + 3], entry.new_var_idx);
                    if entry.is_local {
                        byte_code[pos] = if op == OP_GET_VAR_REF || op == OP_GET_VAR_REF_NOCHECK {
                            OP_GET_LOC.0 as u8
                        } else {
                            OP_PUT_LOC.0 as u8
                        };
                    }
                }
            }
            _ => {}
        }
        let size = OPCODES[op_val].size as usize;
        debug_assert!(size > 0);
        if size == 0 {
            break;
        }
        pos += size;
    }
}

pub fn convert_ext_vars_to_local_vars(state: &mut JSParseState) -> Result<(), VarError> {
    let mut func_ptr = func_from_value(state.cur_func())?;
    let (ext_len, byte_code_val, ext_vars_val, arg_count) = {
        // SAFETY: state.cur_func points to a live FunctionBytecode.
        let func_ref = unsafe { func_ptr.as_ref() };
        (
            func_ref.ext_vars_len() as usize,
            func_ref.byte_code(),
            func_ref.ext_vars(),
            func_ref.arg_count() as i32,
        )
    };
    let local_len = state.local_vars_len() as usize;
    if local_len == 0 || ext_len == 0 {
        return Ok(());
    }
    let mut byte_code_ptr = byte_array_ptr(byte_code_val)?;
    // SAFETY: byte_code_val points to a live ByteArray owned by the allocator.
    let byte_code_arr = unsafe { byte_code_ptr.as_mut() };
    let byte_code_len = state.byte_code_len() as usize;
    debug_assert!(byte_code_len <= byte_code_arr.len());
    let byte_code = byte_code_arr.buf_mut();

    let mut ext_vars_ptr = value_array_ptr(ext_vars_val)?;
    // SAFETY: ext_vars_val points to a live ValueArray owned by the allocator.
    let ext_vars = unsafe { ext_vars_ptr.as_mut() };
    debug_assert!(ext_vars.size() >= ext_len * 2);
    debug_assert!(local_len >= arg_count as usize);

    let mut j = 0usize;
    for i0 in (0..ext_len).step_by(CVT_VAR_SIZE_MAX) {
        let len = min(ext_len - i0, CVT_VAR_SIZE_MAX);
        let mut cvt_tab = [ConvertVarEntry::default(); CVT_VAR_SIZE_MAX];
        for (i, entry) in cvt_tab.iter_mut().enumerate().take(len) {
            let var_name = ext_vars.values()[2 * (i0 + i)];
            let decl = ext_vars.values()[2 * (i0 + i) + 1];
            let var_idx = find_var(state, var_name);
            if var_idx >= arg_count {
                entry.new_var_idx = (var_idx - arg_count) as u16;
                entry.is_local = true;
            } else {
                entry.new_var_idx = j as u16;
                entry.is_local = false;
                ext_vars.values_mut()[2 * j] = var_name;
                ext_vars.values_mut()[2 * j + 1] = decl;
                j += 1;
            }
        }
        if j != i0 + len {
            convert_ext_vars_to_local_vars_bytecode(
                &mut byte_code[..byte_code_len],
                i0 as u32,
                &cvt_tab[..len],
            );
        }
    }
    // SAFETY: state.cur_func points to a live FunctionBytecode.
    unsafe { func_ptr.as_mut() }.set_ext_vars_len(j as u16);
    Ok(())
}

pub fn resolve_var_refs(
    alloc: &mut VarAllocator,
    func: JSValue,
    parent_func: JSValue,
) -> Result<(), VarError> {
    let mut func_ptr = func_from_value(func)?;
    let parent_ptr = func_from_value(parent_func)?;
    let arg_count = {
        // SAFETY: caller provides a valid FunctionBytecode pointer.
        unsafe { parent_ptr.as_ref() }.arg_count() as i32
    };
    // SAFETY: caller provides a valid FunctionBytecode pointer.
    let func_ref = unsafe { func_ptr.as_mut() };
    if func_ref.ext_vars_len() == 0 {
        return Ok(());
    }

    let ext_len = func_ref.ext_vars_len() as usize;
    let mut ext_vars_ptr = value_array_ptr(func_ref.ext_vars())?;
    // SAFETY: ext_vars points to a live ValueArray owned by the allocator.
    let ext_vars = unsafe { ext_vars_ptr.as_mut() };
    debug_assert!(ext_vars.size() >= ext_len * 2);

    for i in 0..ext_len {
        let var_name = ext_vars.values()[2 * i];
        let mut var_idx = find_func_var(parent_func, var_name);
        let decl = if var_idx >= 0 {
            if var_idx < arg_count {
                pack_decl(JSVarRefKind::Arg, var_idx)
            } else {
                pack_decl(JSVarRefKind::Var, var_idx - arg_count)
            }
        } else {
            var_idx = find_func_ext_var(parent_func, var_name);
            if var_idx < 0 {
                var_idx = add_func_ext_var(
                    alloc,
                    parent_func,
                    var_name,
                    (JSVarRefKind::Global as i32) << 16,
                )?;
            }
            pack_decl(JSVarRefKind::VarRef, var_idx)
        };
        ext_vars.values_mut()[2 * i + 1] = new_short_int(decl);
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::function_bytecode::{FunctionBytecodeFields, FunctionBytecodeHeader};
    use crate::jsvalue::{new_short_int, value_get_int};

    struct OwnedFunc {
        ptr: NonNull<FunctionBytecode>,
    }

    impl OwnedFunc {
        fn new(header: FunctionBytecodeHeader, fields: FunctionBytecodeFields) -> Self {
            let boxed = Box::new(FunctionBytecode::from_fields(header, fields));
            // SAFETY: Box never yields a null pointer.
            let ptr = unsafe { NonNull::new_unchecked(Box::into_raw(boxed)) };
            Self { ptr }
        }

        fn value(&self) -> JSValue {
            value_from_ptr(self.ptr)
        }

        fn as_ref(&self) -> &FunctionBytecode {
            // SAFETY: pointer remains valid for the lifetime of OwnedFunc.
            unsafe { self.ptr.as_ref() }
        }

        fn as_mut(&mut self) -> &mut FunctionBytecode {
            // SAFETY: pointer remains valid for the lifetime of OwnedFunc.
            unsafe { self.ptr.as_mut() }
        }
    }

    impl Drop for OwnedFunc {
        fn drop(&mut self) {
            // SAFETY: pointer was created from Box::into_raw in OwnedFunc::new.
            unsafe { drop(Box::from_raw(self.ptr.as_ptr())) };
        }
    }

    struct Harness {
        alloc: VarAllocator,
        state: JSParseState,
        func: OwnedFunc,
    }

    impl Harness {
        fn new(arg_count: u16) -> Self {
            let header = FunctionBytecodeHeader::new(false, false, false, arg_count, false);
            let fields = FunctionBytecodeFields {
                func_name: JS_NULL,
                byte_code: JS_NULL,
                cpool: JS_NULL,
                vars: JS_NULL,
                ext_vars: JS_NULL,
                stack_size: 0,
                ext_vars_len: 0,
                filename: JS_NULL,
                pc2line: JS_NULL,
                source_pos: 0,
            };
            let func = OwnedFunc::new(header, fields);
            let func_val = func.value();
            let ctx = NonNull::dangling();
            let mut state = JSParseState::new(ctx, false);
            state.set_cur_func(func_val);
            Self {
                alloc: VarAllocator::new(),
                state,
                func,
            }
        }
    }

    fn value_array_ref(val: JSValue) -> &'static ValueArray {
        let ptr = value_to_ptr::<ValueArray>(val).expect("value array pointer");
        // SAFETY: the ValueArray is kept alive by the allocator in the test harness.
        unsafe { ptr.as_ref() }
    }

    #[test]
    fn add_var_appends_and_grows_array() {
        let mut harness = Harness::new(0);
        let name = new_short_int(7);
        let idx = add_var(&mut harness.state, &mut harness.alloc, name).unwrap();
        assert_eq!(idx, 0);
        assert_eq!(harness.state.local_vars_len(), 1);
        let vars_val = harness.func.as_ref().vars();
        let arr = value_array_ref(vars_val);
        assert_eq!(arr.size(), 4);
        assert_eq!(arr.values()[0], name);
        assert_eq!(arr.values()[1], JS_UNDEFINED);
    }

    #[test]
    fn add_ext_var_stores_pairs() {
        let mut harness = Harness::new(0);
        let name = new_short_int(11);
        let decl = (JSVarRefKind::Global as i32) << 16;
        let idx = add_ext_var(&harness.state, &mut harness.alloc, name, decl).unwrap();
        assert_eq!(idx, 0);
        let func_ref = harness.func.as_ref();
        assert_eq!(func_ref.ext_vars_len(), 1);
        let arr = value_array_ref(func_ref.ext_vars());
        assert_eq!(arr.size(), 4);
        assert_eq!(arr.values()[0], name);
        assert_eq!(value_get_int(arr.values()[1]), decl);
    }

    #[test]
    fn define_var_distinguishes_args_and_locals() {
        let mut harness = Harness::new(2);
        let vars_val = harness.alloc.alloc_value_array(4);
        {
            let mut arr_ptr = value_array_ptr(vars_val).unwrap();
            // SAFETY: vars_val is owned by the allocator in this test.
            let arr = unsafe { arr_ptr.as_mut() };
            arr.values_mut()[0] = new_short_int(1);
            arr.values_mut()[1] = new_short_int(2);
            arr.values_mut()[2] = new_short_int(3);
        }
        harness.func.as_mut().set_vars(vars_val);
        harness.state.set_local_vars_len(3);

        let (kind, idx) = define_var(&mut harness.state, &mut harness.alloc, new_short_int(2))
            .unwrap();
        assert_eq!(kind, JSVarRefKind::Arg);
        assert_eq!(idx, 1);

        let (kind, idx) = define_var(&mut harness.state, &mut harness.alloc, new_short_int(3))
            .unwrap();
        assert_eq!(kind, JSVarRefKind::Var);
        assert_eq!(idx, 0);

        let (kind, idx) = define_var(&mut harness.state, &mut harness.alloc, new_short_int(4))
            .unwrap();
        assert_eq!(kind, JSVarRefKind::Var);
        assert_eq!(idx, 1);
        assert_eq!(harness.state.local_vars_len(), 4);
    }

    #[test]
    fn define_var_eval_updates_ext_var_decl() {
        let mut harness = Harness::new(0);
        harness.state.set_is_eval(true);
        let name = new_short_int(9);
        let (kind, idx) = define_var(&mut harness.state, &mut harness.alloc, name).unwrap();
        assert_eq!(kind, JSVarRefKind::VarRef);
        assert_eq!(idx, 0);
        let func_ref = harness.func.as_ref();
        let arr = value_array_ref(func_ref.ext_vars());
        let decl = ((JSVarRefKind::Global as i32) << 16) | 1;
        assert_eq!(value_get_int(arr.values()[1]), decl);

        let (kind, idx) = define_var(&mut harness.state, &mut harness.alloc, name).unwrap();
        assert_eq!(kind, JSVarRefKind::VarRef);
        assert_eq!(idx, 0);
        let arr = value_array_ref(func_ref.ext_vars());
        assert_eq!(value_get_int(arr.values()[1]), decl);
    }

    #[test]
    fn resolve_var_refs_links_parent_vars() {
        let mut alloc = VarAllocator::new();
        let header_parent = FunctionBytecodeHeader::new(false, false, false, 1, false);
        let fields_parent = FunctionBytecodeFields {
            func_name: JS_NULL,
            byte_code: JS_NULL,
            cpool: JS_NULL,
            vars: JS_NULL,
            ext_vars: JS_NULL,
            stack_size: 0,
            ext_vars_len: 0,
            filename: JS_NULL,
            pc2line: JS_NULL,
            source_pos: 0,
        };
        let mut parent = OwnedFunc::new(header_parent, fields_parent);
        let parent_val = parent.value();
        let vars_val = alloc.alloc_value_array(2);
        {
            let mut arr_ptr = value_array_ptr(vars_val).unwrap();
            // SAFETY: vars_val is owned by the allocator in this test.
            let arr = unsafe { arr_ptr.as_mut() };
            arr.values_mut()[0] = new_short_int(1);
            arr.values_mut()[1] = new_short_int(2);
        }
        parent.as_mut().set_vars(vars_val);

        let header_child = FunctionBytecodeHeader::new(false, false, false, 0, false);
        let fields_child = FunctionBytecodeFields {
            func_name: JS_NULL,
            byte_code: JS_NULL,
            cpool: JS_NULL,
            vars: JS_NULL,
            ext_vars: JS_NULL,
            stack_size: 0,
            ext_vars_len: 2,
            filename: JS_NULL,
            pc2line: JS_NULL,
            source_pos: 0,
        };
        let mut child = OwnedFunc::new(header_child, fields_child);
        let child_val = child.value();
        let ext_vars_val = alloc.alloc_value_array(4);
        {
            let mut arr_ptr = value_array_ptr(ext_vars_val).unwrap();
            // SAFETY: ext_vars_val is owned by the allocator in this test.
            let arr = unsafe { arr_ptr.as_mut() };
            arr.values_mut()[0] = new_short_int(2);
            arr.values_mut()[1] = new_short_int(0);
            arr.values_mut()[2] = new_short_int(3);
            arr.values_mut()[3] = new_short_int(0);
        }
        child.as_mut().set_ext_vars(ext_vars_val);

        resolve_var_refs(&mut alloc, child_val, parent_val).unwrap();
        let arr = value_array_ref(child.as_ref().ext_vars());
        let decl_local = (JSVarRefKind::Var as i32) << 16;
        assert_eq!(value_get_int(arr.values()[1]), decl_local);
        let decl_ref = (JSVarRefKind::VarRef as i32) << 16;
        assert_eq!(value_get_int(arr.values()[3]), decl_ref);

        let parent_arr = value_array_ref(parent.as_ref().ext_vars());
        assert_eq!(parent.as_ref().ext_vars_len(), 1);
        assert_eq!(parent_arr.values()[0], new_short_int(3));
    }

    #[test]
    fn convert_ext_vars_to_local_vars_patches_bytecode() {
        let mut harness = Harness::new(1);
        let vars_val = harness.alloc.alloc_value_array(2);
        {
            let mut arr_ptr = value_array_ptr(vars_val).unwrap();
            // SAFETY: vars_val is owned by the allocator in this test.
            let arr = unsafe { arr_ptr.as_mut() };
            arr.values_mut()[0] = new_short_int(1);
            arr.values_mut()[1] = new_short_int(2);
        }
        harness.func.as_mut().set_vars(vars_val);
        harness.state.set_local_vars_len(2);

        let ext_vars_val = harness.alloc.alloc_value_array(2);
        {
            let mut arr_ptr = value_array_ptr(ext_vars_val).unwrap();
            // SAFETY: ext_vars_val is owned by the allocator in this test.
            let arr = unsafe { arr_ptr.as_mut() };
            arr.values_mut()[0] = new_short_int(2);
            arr.values_mut()[1] = new_short_int(0);
        }
        harness.func.as_mut().set_ext_vars(ext_vars_val);
        harness.func.as_mut().set_ext_vars_len(1);

        let mut byte_code = vec![OP_GET_VAR_REF.0 as u8, 0, 0];
        let byte_code_val = harness.alloc.alloc_byte_array(byte_code.clone());
        harness.func.as_mut().set_byte_code(byte_code_val);
        harness.state.set_byte_code_len(byte_code.len() as u32);

        convert_ext_vars_to_local_vars(&mut harness.state).unwrap();
        let mut byte_code_ptr = byte_array_ptr(byte_code_val).unwrap();
        // SAFETY: byte_code_val is owned by the allocator in this test.
        let arr = unsafe { byte_code_ptr.as_mut() };
        let len = byte_code.len();
        byte_code.copy_from_slice(&arr.buf_mut()[..len]);
        assert_eq!(byte_code[0], OP_GET_LOC.0 as u8);
        assert_eq!(get_u16(&byte_code[1..3]), 0);
        assert_eq!(harness.func.as_ref().ext_vars_len(), 0);
    }
}
