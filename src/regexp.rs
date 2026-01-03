#![allow(dead_code)]

use crate::context::{ContextError, JSContext};
use crate::conversion::{self, ConversionError};
use crate::cutils::{get_u16, get_u32, utf8_get};
use crate::enums::JSObjectClass;
use crate::heap::JS_STACK_SLACK;
use crate::jsvalue::{JSValue, JSWord};
use crate::memblock::{read_mblock_header, MTag};
use crate::object::{Object, ObjectHeader, RegExp};
use crate::opcode::{
    REOP_ANY, REOP_BACK_REFERENCE, REOP_BACK_REFERENCE_I, REOP_CHAR1, REOP_CHAR2, REOP_CHAR3,
    REOP_CHAR4, REOP_CHECK_ADVANCE, REOP_DOT, REOP_GOTO, REOP_LINE_END, REOP_LINE_END_M,
    REOP_LINE_START, REOP_LINE_START_M, REOP_LOOKAHEAD, REOP_LOOKAHEAD_MATCH, REOP_LOOP,
    REOP_LOOP_CHECK_ADV_SPLIT_GOTO_FIRST, REOP_LOOP_CHECK_ADV_SPLIT_NEXT_FIRST,
    REOP_LOOP_SPLIT_GOTO_FIRST, REOP_LOOP_SPLIT_NEXT_FIRST, REOP_MATCH,
    REOP_NEGATIVE_LOOKAHEAD, REOP_NEGATIVE_LOOKAHEAD_MATCH, REOP_NOT_SPACE,
    REOP_NOT_WORD_BOUNDARY, REOP_RANGE, REOP_RANGE8, REOP_SAVE_END, REOP_SAVE_RESET,
    REOP_SAVE_START, REOP_SET_CHAR_POS, REOP_SET_I32, REOP_SPACE, REOP_SPLIT_GOTO_FIRST,
    REOP_SPLIT_NEXT_FIRST, REOP_WORD_BOUNDARY,
};
use crate::parser::regexp_flags::{LRE_FLAG_GLOBAL, LRE_FLAG_STICKY};
use crate::property::{define_property_value, set_property, PropertyError};
use crate::string::runtime::string_view;
use core::mem::size_of;
use core::ptr::{self, NonNull};
use core::slice;

const RE_HEADER_FLAGS: usize = 0;
const RE_HEADER_CAPTURE_COUNT: usize = 2;
const RE_HEADER_REGISTER_COUNT: usize = 3;
const RE_HEADER_LEN: usize = 4;

const CP_LS: u32 = 0x2028;
const CP_PS: u32 = 0x2029;

const CHAR_RANGE_S_TABLE: &[u32] = &[
    0x0009, 0x000D + 1, 0x0020, 0x0020 + 1, 0x00A0, 0x00A0 + 1, 0x1680, 0x1680 + 1, 0x2000,
    0x200A + 1, 0x2028, 0x2029 + 1, 0x202F, 0x202F + 1, 0x205F, 0x205F + 1, 0x3000,
    0x3000 + 1, 0xFEFF, 0xFEFF + 1,
];

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum RegExpExecMode {
    Exec,
    Test,
    Search,
    ForceGlobal,
}

#[derive(Debug)]
pub enum RegExpError {
    Context(ContextError),
    Property(PropertyError),
    TypeError(&'static str),
    InvalidValue(&'static str),
    InvalidBytecode(&'static str),
    StackOverflow,
}

impl From<ContextError> for RegExpError {
    fn from(err: ContextError) -> Self {
        RegExpError::Context(err)
    }
}

impl From<PropertyError> for RegExpError {
    fn from(err: PropertyError) -> Self {
        RegExpError::Property(err)
    }
}

impl From<ConversionError> for RegExpError {
    fn from(err: ConversionError) -> Self {
        match err {
            ConversionError::Context(err) => RegExpError::Context(err),
            ConversionError::Property(err) => RegExpError::Property(err),
            ConversionError::Interpreter(_) => RegExpError::TypeError("conversion"),
            ConversionError::TypeError(msg) => RegExpError::TypeError(msg),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum ReExecState {
    Split = 0,
    Lookahead = 1,
    NegativeLookahead = 2,
}

struct ByteArrayView {
    ptr: NonNull<u8>,
    len: usize,
}

impl ByteArrayView {
    unsafe fn from_value(val: JSValue) -> Result<Self, RegExpError> {
        let ptr = val.to_ptr::<u8>().ok_or(RegExpError::InvalidValue("byte array"))?;
        let header = unsafe {
            // SAFETY: ptr points to a readable memblock header.
            read_mblock_header(ptr.as_ptr())
        };
        if header.tag() != MTag::ByteArray {
            return Err(RegExpError::InvalidValue("byte array tag"));
        }
        let size = (header.word() >> crate::memblock::JS_MTAG_BITS) as usize;
        let buf = unsafe {
            // SAFETY: byte array payload starts after the header.
            NonNull::new_unchecked(ptr.as_ptr().add(size_of::<JSWord>()))
        };
        Ok(Self { ptr: buf, len: size })
    }

    fn as_slice(&self) -> &[u8] {
        unsafe {
            // SAFETY: pointer covers `len` bytes of byte array storage.
            slice::from_raw_parts(self.ptr.as_ptr(), self.len)
        }
    }

    fn as_u32_slice_mut(&mut self) -> &mut [u32] {
        debug_assert!(self.len.is_multiple_of(size_of::<u32>()));
        let len = self.len / size_of::<u32>();
        unsafe {
            // SAFETY: pointer covers `len` u32 values in byte array storage.
            slice::from_raw_parts_mut(self.ptr.as_ptr() as *mut u32, len)
        }
    }

    fn as_u32_slice(&self) -> &[u32] {
        debug_assert!(self.len.is_multiple_of(size_of::<u32>()));
        let len = self.len / size_of::<u32>();
        unsafe {
            // SAFETY: pointer covers `len` u32 values in byte array storage.
            slice::from_raw_parts(self.ptr.as_ptr() as *const u32, len)
        }
    }
}

pub(crate) fn new_regexp_object(
    ctx: &mut JSContext,
    source: JSValue,
    byte_code: JSValue,
) -> Result<JSValue, ContextError> {
    let proto = ctx.class_proto()[JSObjectClass::RegExp as usize];
    let obj = ctx.alloc_object(JSObjectClass::RegExp, proto, size_of::<RegExp>())?;
    let obj_ptr = obj.to_ptr::<Object>().expect("regexp object pointer");
    unsafe {
        // SAFETY: object payload contains space for RegExp.
        let payload = Object::payload_ptr(obj_ptr.as_ptr());
        let re_ptr = ptr::addr_of_mut!((*payload).regexp);
        ptr::write_unaligned(re_ptr, RegExp::new(source, byte_code, 0));
    }
    Ok(obj)
}

pub(crate) fn regexp_exec(
    ctx: &mut JSContext,
    this_val: JSValue,
    arg: JSValue,
    mode: RegExpExecMode,
) -> Result<JSValue, RegExpError> {
    let saved_sp = ctx.sp();
    let result = (|| -> Result<JSValue, RegExpError> {
        let input = conversion::to_string(ctx, arg)?;
        let obj_ptr = regexp_object_ptr(this_val)?;
        let re_ptr = regexp_payload_ptr(obj_ptr);
        let byte_code = unsafe {
            // SAFETY: re_ptr is valid and byte_code is a JSValue slot.
            ptr::read_unaligned(RegExp::byte_code_ptr(re_ptr))
        };
        let byte_code_view = unsafe { ByteArrayView::from_value(byte_code)? };
        let byte_code_bytes = byte_code_view.as_slice();
        let capture_count = lre_get_capture_count(byte_code_bytes)?;
        let alloc_count = lre_get_alloc_count(byte_code_bytes)?;
        let flags = lre_get_flags(byte_code_bytes)? as u32;

        let mut last_index = unsafe {
            // SAFETY: re_ptr is valid and last_index is an i32 slot.
            ptr::read_unaligned(RegExp::last_index_ptr(re_ptr))
        };
        if last_index < 0 {
            last_index = 0;
        }

        let mut re_flags = flags;
        let force_global = mode == RegExpExecMode::ForceGlobal;
        if force_global {
            re_flags |= LRE_FLAG_GLOBAL;
        }
        if (re_flags & (LRE_FLAG_GLOBAL | LRE_FLAG_STICKY)) == 0 || mode == RegExpExecMode::Search {
            last_index = 0;
        }

        let capture_buf = ctx.alloc_byte_array(&vec![0u8; alloc_count * size_of::<u32>()])?;
        let mut root_sp = saved_sp.as_ptr();
        unsafe {
            root_push(ctx, &mut root_sp, capture_buf)?;
            root_push(ctx, &mut root_sp, input)?;
        }

        let last_index_utf8 = if last_index <= 0 {
            0u32
        } else {
            ctx.string_utf16_to_utf8_pos(input, last_index as u32) / 2
        };

        let matched = {
            let mut scratch = [0u8; 5];
            let view = string_view(input, &mut scratch).ok_or(RegExpError::TypeError("string"))?;
            let input_len = view.bytes().len() as u32;
            if last_index_utf8 > input_len {
                false
            } else {
                let mut capture_view = unsafe { ByteArrayView::from_value(capture_buf)? };
                let capture = capture_view.as_u32_slice_mut();
                for slot in capture.iter_mut().take(capture_count * 2) {
                    *slot = u32::MAX;
                }
                lre_exec(
                    ctx,
                    capture,
                    byte_code_bytes,
                    view.bytes(),
                    last_index_utf8 as usize,
                )?
            }
        };

        if !matched {
            if (re_flags & (LRE_FLAG_GLOBAL | LRE_FLAG_STICKY)) != 0 {
                unsafe {
                    // SAFETY: re_ptr is valid and last_index is an i32 slot.
                    ptr::write_unaligned(RegExp::last_index_ptr(re_ptr), 0);
                }
            }
            return match mode {
                RegExpExecMode::Search => Ok(JSValue::new_short_int(-1)),
                RegExpExecMode::Test => Ok(JSValue::JS_FALSE),
                RegExpExecMode::Exec | RegExpExecMode::ForceGlobal => Ok(JSValue::JS_NULL),
            };
        }

        let capture_view = unsafe { ByteArrayView::from_value(capture_buf)? };
        let capture = capture_view.as_u32_slice();
        if mode == RegExpExecMode::Search {
            let index = ctx.string_utf8_to_utf16_pos(input, capture[0] * 2) as i32;
            return Ok(JSValue::new_short_int(index));
        }

        if (re_flags & (LRE_FLAG_GLOBAL | LRE_FLAG_STICKY)) != 0 {
            let end = ctx.string_utf8_to_utf16_pos(input, capture[1] * 2) as i32;
            unsafe {
                // SAFETY: re_ptr is valid and last_index is an i32 slot.
                ptr::write_unaligned(RegExp::last_index_ptr(re_ptr), end);
            }
        }

        if mode == RegExpExecMode::Test {
            return Ok(JSValue::JS_TRUE);
        }

        let result = ctx.alloc_array(capture_count)?;
        unsafe {
            root_push(ctx, &mut root_sp, result)?;
        }
        let capture_view = unsafe { ByteArrayView::from_value(capture_buf)? };
        let capture = capture_view.as_u32_slice();
        let index_key = ctx.intern_string(b"index")?;
        let input_key = ctx.intern_string(b"input")?;
        let index_val = ctx.string_utf8_to_utf16_pos(input, capture[0] * 2) as i32;
        define_property_value(ctx, result, index_key, JSValue::new_short_int(index_val))?;
        define_property_value(ctx, result, input_key, input)?;

        for i in 0..capture_count {
            let start = capture[2 * i];
            let end = capture[2 * i + 1];
            if start != u32::MAX && end != u32::MAX {
                let val = ctx.sub_string_utf8(input, start * 2, end * 2)?;
                set_property(ctx, result, JSValue::new_short_int(i as i32), val)?;
            }
        }

        Ok(result)
    })();
    ctx.set_sp(saved_sp);
    result
}

fn regexp_object_ptr(obj: JSValue) -> Result<NonNull<Object>, RegExpError> {
    let ptr = obj.to_ptr::<Object>().ok_or(RegExpError::TypeError("not a regular expression"))?;
    let header_word = unsafe {
        // SAFETY: ptr points to a readable object header.
        read_mblock_header(ptr.as_ptr()).word()
    };
    let header = ObjectHeader::from_word(header_word);
    if header.tag() != MTag::Object || header.class_id() != JSObjectClass::RegExp as u8 {
        return Err(RegExpError::TypeError("not a regular expression"));
    }
    Ok(ptr)
}

fn regexp_payload_ptr(obj_ptr: NonNull<Object>) -> *mut RegExp {
    unsafe {
        // SAFETY: obj_ptr points to a RegExp object payload.
        let payload = Object::payload_ptr(obj_ptr.as_ptr());
        ptr::addr_of_mut!((*payload).regexp)
    }
}

fn lre_get_capture_count(bytecode: &[u8]) -> Result<usize, RegExpError> {
    if bytecode.len() < RE_HEADER_LEN {
        return Err(RegExpError::InvalidBytecode("header"));
    }
    Ok(bytecode[RE_HEADER_CAPTURE_COUNT] as usize)
}

fn lre_get_alloc_count(bytecode: &[u8]) -> Result<usize, RegExpError> {
    if bytecode.len() < RE_HEADER_LEN {
        return Err(RegExpError::InvalidBytecode("header"));
    }
    let capture_count = bytecode[RE_HEADER_CAPTURE_COUNT] as usize;
    let register_count = bytecode[RE_HEADER_REGISTER_COUNT] as usize;
    Ok(capture_count * 2 + register_count)
}

fn lre_get_flags(bytecode: &[u8]) -> Result<u16, RegExpError> {
    if bytecode.len() < RE_HEADER_LEN {
        return Err(RegExpError::InvalidBytecode("header"));
    }
    Ok(get_u16(&bytecode[RE_HEADER_FLAGS..RE_HEADER_FLAGS + 2]) as u16)
}

fn check_stack_space(ctx: &JSContext, sp: *mut JSValue, slots: usize) -> Result<(), RegExpError> {
    let new_sp = unsafe { sp.sub(slots) };
    let heap_limit = ctx.heap_free_ptr().as_ptr() as usize + (JS_STACK_SLACK as usize * JSValue::JSW as usize);
    if (new_sp as usize) < heap_limit {
        return Err(RegExpError::StackOverflow);
    }
    Ok(())
}

unsafe fn root_push(
    ctx: &mut JSContext,
    sp: &mut *mut JSValue,
    val: JSValue,
) -> Result<(), RegExpError> {
    check_stack_space(ctx, *sp, 1)?;
    *sp = unsafe { sp.sub(1) };
    unsafe {
        // SAFETY: caller ensures the stack has space for one value.
        ptr::write_unaligned(*sp, val);
    }
    ctx.set_sp(NonNull::new(*sp).expect("stack pointer"));
    Ok(())
}

fn is_line_terminator(c: u32) -> bool {
    c == b'\n' as u32 || c == b'\r' as u32 || c == CP_LS || c == CP_PS
}

fn is_word_char(c: u32) -> bool {
    (b'0' as u32..=b'9' as u32).contains(&c)
        || (b'a' as u32..=b'z' as u32).contains(&c)
        || (b'A' as u32..=b'Z' as u32).contains(&c)
        || c == b'_' as u32
}

fn lre_canonicalize(c: u32) -> u32 {
    if (b'A' as u32..=b'Z' as u32).contains(&c) {
        c - b'A' as u32 + b'a' as u32
    } else {
        c
    }
}

fn is_space_ascii(c: u8) -> bool {
    matches!(c, b'\t' | b'\n' | 0x0b | 0x0c | b'\r' | b' ')
}

fn is_space_non_ascii(c: u32) -> bool {
    let mut idx = 0usize;
    while idx + 1 < CHAR_RANGE_S_TABLE.len() {
        let start = CHAR_RANGE_S_TABLE[idx];
        let end = CHAR_RANGE_S_TABLE[idx + 1];
        if c >= start && c < end {
            return true;
        }
        idx += 2;
    }
    false
}

fn get_char(input: &[u8], pos: &mut usize) -> Option<u32> {
    if *pos >= input.len() {
        return None;
    }
    let mut len = 0usize;
    let c = utf8_get(&input[*pos..], &mut len);
    if c < 0 || len == 0 {
        return None;
    }
    *pos = pos.saturating_add(len);
    Some(c as u32)
}

fn peek_char(input: &[u8], pos: usize) -> Option<u32> {
    if pos >= input.len() {
        return None;
    }
    let mut len = 0usize;
    let c = utf8_get(&input[pos..], &mut len);
    if c < 0 {
        None
    } else {
        Some(c as u32)
    }
}

fn peek_prev_char(input: &[u8], pos: usize) -> Option<u32> {
    if pos == 0 || pos > input.len() {
        return None;
    }
    let mut idx = pos - 1;
    while idx > 0 && (input[idx] & 0xc0) == 0x80 {
        idx -= 1;
    }
    let mut len = 0usize;
    let c = utf8_get(&input[idx..], &mut len);
    if c < 0 {
        None
    } else {
        Some(c as u32)
    }
}

fn encode_stack_ptr(stack_top: *mut JSValue, ptr: *mut JSValue) -> Result<JSValue, RegExpError> {
    let offset = (stack_top as isize - ptr as isize) / (JSValue::JSW as isize);
    let offset = i32::try_from(offset).map_err(|_| RegExpError::StackOverflow)?;
    Ok(JSValue::new_short_int(offset))
}

fn decode_stack_ptr(stack_top: *mut JSValue, val: JSValue) -> Result<*mut JSValue, RegExpError> {
    if !val.is_int() {
        return Err(RegExpError::InvalidValue("stack ptr"));
    }
    let offset = val.get_int();
    if offset < 0 {
        return Err(RegExpError::InvalidValue("stack ptr"));
    }
    Ok(unsafe { stack_top.offset(-(offset as isize)) })
}

fn encode_pc_state(pc: usize, state: ReExecState) -> JSValue {
    let value = ((pc as i32) << 2) | (state as i32);
    JSValue::new_short_int(value)
}

fn decode_pc_state(val: JSValue) -> Result<(usize, ReExecState), RegExpError> {
    if !val.is_int() {
        return Err(RegExpError::InvalidValue("state"));
    }
    let raw = val.get_int();
    if raw < 0 {
        return Err(RegExpError::InvalidValue("state"));
    }
    let state = match (raw & 3) as u8 {
        0 => ReExecState::Split,
        1 => ReExecState::Lookahead,
        2 => ReExecState::NegativeLookahead,
        _ => return Err(RegExpError::InvalidValue("state type")),
    };
    Ok(((raw >> 2) as usize, state))
}

fn lre_exec(
    ctx: &mut JSContext,
    capture: &mut [u32],
    bytecode: &[u8],
    input: &[u8],
    start: usize,
) -> Result<bool, RegExpError> {
    if bytecode.len() < RE_HEADER_LEN {
        return Err(RegExpError::InvalidBytecode("header"));
    }
    let capture_count = lre_get_capture_count(bytecode)?;
    let stack_top = ctx.stack_top().as_ptr() as *mut JSValue;
    let mut sp = ctx.sp().as_ptr();
    let mut bp = sp;
    let initial_sp = sp;
    let mut pc = RE_HEADER_LEN;
    let mut cptr = start;

    let backtrack = |capture: &mut [u32],
                         pc: &mut usize,
                         cptr: &mut usize,
                         sp: &mut *mut JSValue,
                         bp: &mut *mut JSValue|
     -> Result<bool, RegExpError> {
        loop {
            if *bp == initial_sp {
                return Ok(false);
            }
            while *sp < *bp {
                let idx = unsafe { ptr::read_unaligned(*sp) };
                let prev = unsafe { ptr::read_unaligned((*sp).add(1)) };
                if !idx.is_int() || !prev.is_int() {
                    return Err(RegExpError::InvalidValue("capture restore"));
                }
                let idx = idx.get_int();
                if idx < 0 {
                    return Err(RegExpError::InvalidValue("capture index"));
                }
                capture[idx as usize] = prev.get_int() as u32;
                *sp = unsafe { (*sp).add(2) };
            }
            let state_val = unsafe { ptr::read_unaligned(*sp) };
            let cptr_val = unsafe { ptr::read_unaligned((*sp).add(1)) };
            let bp_val = unsafe { ptr::read_unaligned((*sp).add(2)) };
            let (pc_offset, state) = decode_pc_state(state_val)?;
            if !cptr_val.is_int() {
                return Err(RegExpError::InvalidValue("cptr"));
            }
            let cptr_offset = cptr_val.get_int();
            if cptr_offset < 0 {
                return Err(RegExpError::InvalidValue("cptr"));
            }
            *pc = pc_offset;
            *cptr = cptr_offset as usize;
            *bp = decode_stack_ptr(stack_top, bp_val)?;
            *sp = unsafe { (*sp).add(3) };
            if state != ReExecState::Lookahead {
                break;
            }
        }
        Ok(true)
    };

    loop {
        if pc >= bytecode.len() {
            return Err(RegExpError::InvalidBytecode("pc"));
        }
        let opcode = bytecode[pc];
        pc += 1;
        match opcode {
            op if op == REOP_MATCH.as_u8() => return Ok(true),
            op if op == REOP_LOOKAHEAD_MATCH.as_u8() => {
                let sp_start = sp;
                loop {
                    let sp1 = sp;
                    sp = bp;
                    let state_val = unsafe { ptr::read_unaligned(sp) };
                    let cptr_val = unsafe { ptr::read_unaligned(sp.add(1)) };
                    let bp_val = unsafe { ptr::read_unaligned(sp.add(2)) };
                    let (pc_offset, state) = decode_pc_state(state_val)?;
                    if !cptr_val.is_int() {
                        return Err(RegExpError::InvalidValue("cptr"));
                    }
                    let cptr_offset = cptr_val.get_int();
                    if cptr_offset < 0 {
                        return Err(RegExpError::InvalidValue("cptr"));
                    }
                    pc = pc_offset;
                    cptr = cptr_offset as usize;
                    bp = decode_stack_ptr(stack_top, bp_val)?;
                    let next_sp = encode_stack_ptr(stack_top, sp1)?;
                    unsafe {
                        // SAFETY: sp points at a writable state header slot.
                        ptr::write_unaligned(sp.add(2), next_sp);
                    }
                    sp = unsafe { sp.add(3) };
                    if state == ReExecState::Lookahead {
                        break;
                    }
                }
                if sp != initial_sp {
                    let mut sp1 = sp;
                    while sp1 != sp_start {
                        sp1 = unsafe { sp1.sub(3) };
                        let next_sp = decode_stack_ptr(stack_top, unsafe { ptr::read_unaligned(sp1.add(2)) })?;
                        while sp1 != next_sp {
                            sp = unsafe { sp.sub(1) };
                            sp1 = unsafe { sp1.sub(1) };
                            unsafe {
                                // SAFETY: sp1 and sp point to writable stack slots.
                                ptr::write_unaligned(sp, ptr::read_unaligned(sp1));
                            }
                        }
                    }
                }
            }
            op if op == REOP_NEGATIVE_LOOKAHEAD_MATCH.as_u8() => {
                loop {
                    while sp < bp {
                        let idx = unsafe { ptr::read_unaligned(sp) };
                        let prev = unsafe { ptr::read_unaligned(sp.add(1)) };
                        if !idx.is_int() || !prev.is_int() {
                            return Err(RegExpError::InvalidValue("capture restore"));
                        }
                        let idx = idx.get_int();
                        if idx < 0 {
                            return Err(RegExpError::InvalidValue("capture index"));
                        }
                        capture[idx as usize] = prev.get_int() as u32;
                        sp = unsafe { sp.add(2) };
                    }
                    let state_val = unsafe { ptr::read_unaligned(sp) };
                    let cptr_val = unsafe { ptr::read_unaligned(sp.add(1)) };
                    let bp_val = unsafe { ptr::read_unaligned(sp.add(2)) };
                    let (pc_offset, state) = decode_pc_state(state_val)?;
                    if !cptr_val.is_int() {
                        return Err(RegExpError::InvalidValue("cptr"));
                    }
                    let cptr_offset = cptr_val.get_int();
                    if cptr_offset < 0 {
                        return Err(RegExpError::InvalidValue("cptr"));
                    }
                    pc = pc_offset;
                    cptr = cptr_offset as usize;
                    bp = decode_stack_ptr(stack_top, bp_val)?;
                    sp = unsafe { sp.add(3) };
                    if state == ReExecState::NegativeLookahead {
                        break;
                    }
                }
                if !backtrack(capture, &mut pc, &mut cptr, &mut sp, &mut bp)? {
                    return Ok(false);
                }
            }
            op if op == REOP_CHAR1.as_u8() => {
                if cptr + 1 > input.len() {
                    if !backtrack(capture, &mut pc, &mut cptr, &mut sp, &mut bp)? {
                        return Ok(false);
                    }
                    continue;
                }
                if bytecode[pc] != input[cptr] {
                    if !backtrack(capture, &mut pc, &mut cptr, &mut sp, &mut bp)? {
                        return Ok(false);
                    }
                    continue;
                }
                pc += 1;
                cptr += 1;
            }
            op if op == REOP_CHAR2.as_u8() => {
                if cptr + 2 > input.len() {
                    if !backtrack(capture, &mut pc, &mut cptr, &mut sp, &mut bp)? {
                        return Ok(false);
                    }
                    continue;
                }
                if get_u16(&bytecode[pc..pc + 2]) != get_u16(&input[cptr..cptr + 2]) {
                    if !backtrack(capture, &mut pc, &mut cptr, &mut sp, &mut bp)? {
                        return Ok(false);
                    }
                    continue;
                }
                pc += 2;
                cptr += 2;
            }
            op if op == REOP_CHAR3.as_u8() => {
                if cptr + 3 > input.len() {
                    if !backtrack(capture, &mut pc, &mut cptr, &mut sp, &mut bp)? {
                        return Ok(false);
                    }
                    continue;
                }
                if get_u16(&bytecode[pc..pc + 2]) != get_u16(&input[cptr..cptr + 2])
                    || bytecode[pc + 2] != input[cptr + 2]
                {
                    if !backtrack(capture, &mut pc, &mut cptr, &mut sp, &mut bp)? {
                        return Ok(false);
                    }
                    continue;
                }
                pc += 3;
                cptr += 3;
            }
            op if op == REOP_CHAR4.as_u8() => {
                if cptr + 4 > input.len() {
                    if !backtrack(capture, &mut pc, &mut cptr, &mut sp, &mut bp)? {
                        return Ok(false);
                    }
                    continue;
                }
                if get_u32(&bytecode[pc..pc + 4]) != get_u32(&input[cptr..cptr + 4]) {
                    if !backtrack(capture, &mut pc, &mut cptr, &mut sp, &mut bp)? {
                        return Ok(false);
                    }
                    continue;
                }
                pc += 4;
                cptr += 4;
            }
            op if op == REOP_SPLIT_GOTO_FIRST.as_u8() || op == REOP_SPLIT_NEXT_FIRST.as_u8() => {
                if pc + 4 > bytecode.len() {
                    return Err(RegExpError::InvalidBytecode("split"));
                }
                let offset = get_u32(&bytecode[pc..pc + 4]) as i32;
                pc += 4;
                check_stack_space(ctx, sp, 3)?;
                let (pc1, new_pc) = if op == REOP_SPLIT_NEXT_FIRST.as_u8() {
                    ((pc as i32 + offset) as usize, pc)
                } else {
                    (pc, (pc as i32 + offset) as usize)
                };
                let state = encode_pc_state(pc1, ReExecState::Split);
                let cptr_val = JSValue::new_short_int(cptr as i32);
                let bp_val = encode_stack_ptr(stack_top, bp)?;
                unsafe {
                    sp = sp.sub(3);
                    // SAFETY: stack has space for a split state header.
                    ptr::write_unaligned(sp, state);
                    ptr::write_unaligned(sp.add(1), cptr_val);
                    ptr::write_unaligned(sp.add(2), bp_val);
                }
                bp = sp;
                pc = new_pc;
            }
            op if op == REOP_LOOKAHEAD.as_u8() || op == REOP_NEGATIVE_LOOKAHEAD.as_u8() => {
                if pc + 4 > bytecode.len() {
                    return Err(RegExpError::InvalidBytecode("lookahead"));
                }
                let offset = get_u32(&bytecode[pc..pc + 4]) as i32;
                pc += 4;
                check_stack_space(ctx, sp, 3)?;
                let target = (pc as i32 + offset) as usize;
                let state = encode_pc_state(
                    target,
                    if op == REOP_LOOKAHEAD.as_u8() {
                        ReExecState::Lookahead
                    } else {
                        ReExecState::NegativeLookahead
                    },
                );
                let cptr_val = JSValue::new_short_int(cptr as i32);
                let bp_val = encode_stack_ptr(stack_top, bp)?;
                unsafe {
                    sp = sp.sub(3);
                    // SAFETY: stack has space for a lookahead header.
                    ptr::write_unaligned(sp, state);
                    ptr::write_unaligned(sp.add(1), cptr_val);
                    ptr::write_unaligned(sp.add(2), bp_val);
                }
                bp = sp;
            }
            op if op == REOP_GOTO.as_u8() => {
                if pc + 4 > bytecode.len() {
                    return Err(RegExpError::InvalidBytecode("goto"));
                }
                let offset = get_u32(&bytecode[pc..pc + 4]) as i32;
                pc = (pc as i32 + 4 + offset) as usize;
            }
            op if op == REOP_LINE_START.as_u8() || op == REOP_LINE_START_M.as_u8() => {
                if cptr != 0 {
                    if op == REOP_LINE_START.as_u8() {
                        if !backtrack(capture, &mut pc, &mut cptr, &mut sp, &mut bp)? {
                            return Ok(false);
                        }
                        continue;
                    }
                    match peek_prev_char(input, cptr) {
                        Some(c) if is_line_terminator(c) => {}
                        _ => {
                            if !backtrack(capture, &mut pc, &mut cptr, &mut sp, &mut bp)? {
                                return Ok(false);
                            }
                            continue;
                        }
                    }
                }
            }
            op if op == REOP_LINE_END.as_u8() || op == REOP_LINE_END_M.as_u8() => {
                if cptr != input.len() {
                    if op == REOP_LINE_END.as_u8() {
                        if !backtrack(capture, &mut pc, &mut cptr, &mut sp, &mut bp)? {
                            return Ok(false);
                        }
                        continue;
                    }
                    match peek_char(input, cptr) {
                        Some(c) if is_line_terminator(c) => {}
                        _ => {
                            if !backtrack(capture, &mut pc, &mut cptr, &mut sp, &mut bp)? {
                                return Ok(false);
                            }
                            continue;
                        }
                    }
                }
            }
            op if op == REOP_DOT.as_u8() => {
                let Some(c) = get_char(input, &mut cptr) else {
                    if !backtrack(capture, &mut pc, &mut cptr, &mut sp, &mut bp)? {
                        return Ok(false);
                    }
                    continue;
                };
                if is_line_terminator(c) && !backtrack(capture, &mut pc, &mut cptr, &mut sp, &mut bp)? {
                    return Ok(false);
                }
            }
            op if op == REOP_ANY.as_u8() => {
                if get_char(input, &mut cptr).is_none()
                    && !backtrack(capture, &mut pc, &mut cptr, &mut sp, &mut bp)?
                {
                    return Ok(false);
                }
            }
            op if op == REOP_SPACE.as_u8() || op == REOP_NOT_SPACE.as_u8() => {
                if cptr >= input.len() {
                    if !backtrack(capture, &mut pc, &mut cptr, &mut sp, &mut bp)? {
                        return Ok(false);
                    }
                    continue;
                }
                let c0 = input[cptr];
                let mut is_space = if c0 < 0x80 {
                    cptr += 1;
                    is_space_ascii(c0)
                } else {
                    let mut len = 0usize;
                    let c = utf8_get(&input[cptr..], &mut len);
                    if c < 0 {
                        if !backtrack(capture, &mut pc, &mut cptr, &mut sp, &mut bp)? {
                            return Ok(false);
                        }
                        continue;
                    }
                    cptr += len;
                    is_space_non_ascii(c as u32)
                };
                if op == REOP_NOT_SPACE.as_u8() {
                    is_space = !is_space;
                }
                if !is_space && !backtrack(capture, &mut pc, &mut cptr, &mut sp, &mut bp)? {
                    return Ok(false);
                }
            }
            op if op == REOP_SAVE_START.as_u8() || op == REOP_SAVE_END.as_u8() => {
                if pc >= bytecode.len() {
                    return Err(RegExpError::InvalidBytecode("save"));
                }
                let idx = bytecode[pc] as usize;
                pc += 1;
                if idx >= capture_count {
                    return Err(RegExpError::InvalidBytecode("capture index"));
                }
                let capture_idx = 2 * idx + (op - REOP_SAVE_START.as_u8()) as usize;
                save_capture(ctx, &mut sp, capture, capture_idx, cptr as u32)?;
            }
            op if op == REOP_SAVE_RESET.as_u8() => {
                if pc + 1 >= bytecode.len() {
                    return Err(RegExpError::InvalidBytecode("save_reset"));
                }
                let mut idx = bytecode[pc] as usize;
                let end = bytecode[pc + 1] as usize;
                pc += 2;
                if end >= capture_count || idx > end {
                    return Err(RegExpError::InvalidBytecode("save_reset"));
                }
                let count = end - idx + 1;
                check_stack_space(ctx, sp, count * 2)?;
                while idx <= end {
                    save_capture(ctx, &mut sp, capture, 2 * idx, 0)?;
                    save_capture(ctx, &mut sp, capture, 2 * idx + 1, 0)?;
                    idx += 1;
                }
            }
            op if op == REOP_SET_I32.as_u8() => {
                if pc + 5 > bytecode.len() {
                    return Err(RegExpError::InvalidBytecode("set_i32"));
                }
                let reg = bytecode[pc] as usize;
                let val = get_u32(&bytecode[pc + 1..pc + 5]);
                pc += 5;
                save_capture_check(ctx, &mut sp, &mut bp, capture, 2 * capture_count + reg, val)?;
            }
            op if op == REOP_LOOP.as_u8() => {
                if pc + 5 > bytecode.len() {
                    return Err(RegExpError::InvalidBytecode("loop"));
                }
                let reg = bytecode[pc] as usize;
                let offset = get_u32(&bytecode[pc + 1..pc + 5]) as i32;
                pc += 5;
                let idx = 2 * capture_count + reg;
                let val = capture[idx].wrapping_sub(1);
                save_capture_check(ctx, &mut sp, &mut bp, capture, idx, val)?;
                if val != 0 {
                    pc = (pc as i32 + offset) as usize;
                }
            }
            op if op == REOP_LOOP_SPLIT_GOTO_FIRST.as_u8()
                || op == REOP_LOOP_SPLIT_NEXT_FIRST.as_u8()
                || op == REOP_LOOP_CHECK_ADV_SPLIT_GOTO_FIRST.as_u8()
                || op == REOP_LOOP_CHECK_ADV_SPLIT_NEXT_FIRST.as_u8() =>
            {
                if pc + 9 > bytecode.len() {
                    return Err(RegExpError::InvalidBytecode("loop_split"));
                }
                let reg = bytecode[pc] as usize;
                let limit = get_u32(&bytecode[pc + 1..pc + 5]);
                let offset = get_u32(&bytecode[pc + 5..pc + 9]) as i32;
                pc += 9;
                let idx = 2 * capture_count + reg;
                let val = capture[idx].wrapping_sub(1);
                save_capture_check(ctx, &mut sp, &mut bp, capture, idx, val)?;
                if val > limit {
                    pc = (pc as i32 + offset) as usize;
                } else {
                    let check_adv = op == REOP_LOOP_CHECK_ADV_SPLIT_GOTO_FIRST.as_u8()
                        || op == REOP_LOOP_CHECK_ADV_SPLIT_NEXT_FIRST.as_u8();
                    if check_adv
                        && capture[idx + 1] == cptr as u32
                        && val != limit
                    {
                        if !backtrack(capture, &mut pc, &mut cptr, &mut sp, &mut bp)? {
                            return Ok(false);
                        }
                        continue;
                    }
                    if val != 0 {
                        check_stack_space(ctx, sp, 3)?;
                        let (pc1, new_pc) = if op == REOP_LOOP_SPLIT_NEXT_FIRST.as_u8()
                            || op == REOP_LOOP_CHECK_ADV_SPLIT_NEXT_FIRST.as_u8()
                        {
                            ((pc as i32 + offset) as usize, pc)
                        } else {
                            (pc, (pc as i32 + offset) as usize)
                        };
                        let state = encode_pc_state(pc1, ReExecState::Split);
                        let cptr_val = JSValue::new_short_int(cptr as i32);
                        let bp_val = encode_stack_ptr(stack_top, bp)?;
                        unsafe {
                            sp = sp.sub(3);
                            // SAFETY: stack has space for a loop split header.
                            ptr::write_unaligned(sp, state);
                            ptr::write_unaligned(sp.add(1), cptr_val);
                            ptr::write_unaligned(sp.add(2), bp_val);
                        }
                        bp = sp;
                        pc = new_pc;
                    }
                }
            }
            op if op == REOP_SET_CHAR_POS.as_u8() => {
                if pc >= bytecode.len() {
                    return Err(RegExpError::InvalidBytecode("set_char_pos"));
                }
                let reg = bytecode[pc] as usize;
                pc += 1;
                save_capture_check(ctx, &mut sp, &mut bp, capture, 2 * capture_count + reg, cptr as u32)?;
            }
            op if op == REOP_CHECK_ADVANCE.as_u8() => {
                if pc >= bytecode.len() {
                    return Err(RegExpError::InvalidBytecode("check_advance"));
                }
                let reg = bytecode[pc] as usize;
                pc += 1;
                if capture[2 * capture_count + reg] == cptr as u32
                    && !backtrack(capture, &mut pc, &mut cptr, &mut sp, &mut bp)?
                {
                    return Ok(false);
                }
            }
            op if op == REOP_WORD_BOUNDARY.as_u8() || op == REOP_NOT_WORD_BOUNDARY.as_u8() => {
                let before = if cptr == 0 {
                    false
                } else {
                    peek_prev_char(input, cptr).map(is_word_char).unwrap_or(false)
                };
                let after = if cptr >= input.len() {
                    false
                } else {
                    peek_char(input, cptr).map(is_word_char).unwrap_or(false)
                };
                let is_boundary = before ^ after;
                let want_boundary = op == REOP_WORD_BOUNDARY.as_u8();
                if is_boundary != want_boundary
                    && !backtrack(capture, &mut pc, &mut cptr, &mut sp, &mut bp)?
                {
                    return Ok(false);
                }
            }
            op if op == REOP_RANGE8.as_u8() => {
                if pc >= bytecode.len() {
                    return Err(RegExpError::InvalidBytecode("range8"));
                }
                let n = bytecode[pc] as usize;
                pc += 1;
                let Some(c) = get_char(input, &mut cptr) else {
                    if !backtrack(capture, &mut pc, &mut cptr, &mut sp, &mut bp)? {
                        return Ok(false);
                    }
                    continue;
                };
                if pc + 2 * n > bytecode.len() {
                    return Err(RegExpError::InvalidBytecode("range8"));
                }
                let mut matched = false;
                if n > 0 {
                    for i in 0..(n.saturating_sub(1)) {
                        let low = bytecode[pc + 2 * i] as u32;
                        let high = bytecode[pc + 2 * i + 1] as u32;
                        if c >= low && c < high {
                            matched = true;
                            break;
                        }
                    }
                    if !matched {
                        let low = bytecode[pc + 2 * (n - 1)] as u32;
                        let high = bytecode[pc + 2 * (n - 1) + 1] as u32;
                        if c >= low && (c < high || high == 0xff) {
                            matched = true;
                        }
                    }
                }
                if !matched {
                    if !backtrack(capture, &mut pc, &mut cptr, &mut sp, &mut bp)? {
                        return Ok(false);
                    }
                    continue;
                }
                pc += 2 * n;
            }
            op if op == REOP_RANGE.as_u8() => {
                if pc + 2 > bytecode.len() {
                    return Err(RegExpError::InvalidBytecode("range"));
                }
                let n = get_u16(&bytecode[pc..pc + 2]) as usize;
                pc += 2;
                let Some(c) = get_char(input, &mut cptr) else {
                    if !backtrack(capture, &mut pc, &mut cptr, &mut sp, &mut bp)? {
                        return Ok(false);
                    }
                    continue;
                };
                if pc + 8 * n > bytecode.len() {
                    return Err(RegExpError::InvalidBytecode("range"));
                }
                let mut idx_min = 0usize;
                let mut idx_max = n - 1;
                let low0 = get_u32(&bytecode[pc..pc + 4]);
                let high0 = get_u32(&bytecode[pc + idx_max * 8 + 4..pc + idx_max * 8 + 8]);
                if c < low0 || c >= high0 {
                    if !backtrack(capture, &mut pc, &mut cptr, &mut sp, &mut bp)? {
                        return Ok(false);
                    }
                    continue;
                }
                let mut matched = false;
                while idx_min <= idx_max {
                    let idx = (idx_min + idx_max) / 2;
                    let low = get_u32(&bytecode[pc + idx * 8..pc + idx * 8 + 4]);
                    let high = get_u32(&bytecode[pc + idx * 8 + 4..pc + idx * 8 + 8]);
                    if c < low {
                        if idx == 0 {
                            break;
                        }
                        idx_max = idx - 1;
                    } else if c >= high {
                        idx_min = idx + 1;
                    } else {
                        matched = true;
                        break;
                    }
                }
                if !matched {
                    if !backtrack(capture, &mut pc, &mut cptr, &mut sp, &mut bp)? {
                        return Ok(false);
                    }
                    continue;
                }
                pc += 8 * n;
            }
            op if op == REOP_BACK_REFERENCE.as_u8() || op == REOP_BACK_REFERENCE_I.as_u8() => {
                if pc >= bytecode.len() {
                    return Err(RegExpError::InvalidBytecode("back_reference"));
                }
                let idx = bytecode[pc] as usize;
                pc += 1;
                let start = capture[2 * idx];
                let end = capture[2 * idx + 1];
                if start != u32::MAX && end != u32::MAX {
                    let mut cptr1 = start as usize;
                    let end_ptr = end as usize;
                    while cptr1 < end_ptr {
                        let mut c1_pos = cptr1;
                        let Some(c1) = get_char(input, &mut c1_pos) else {
                            if !backtrack(capture, &mut pc, &mut cptr, &mut sp, &mut bp)? {
                                return Ok(false);
                            }
                            continue;
                        };
                        cptr1 = c1_pos;
                        let Some(c2) = get_char(input, &mut cptr) else {
                            if !backtrack(capture, &mut pc, &mut cptr, &mut sp, &mut bp)? {
                                return Ok(false);
                            }
                            continue;
                        };
                        let mut c1 = c1;
                        let mut c2 = c2;
                        if op == REOP_BACK_REFERENCE_I.as_u8() {
                            c1 = lre_canonicalize(c1);
                            c2 = lre_canonicalize(c2);
                        }
                        if c1 != c2 {
                            if !backtrack(capture, &mut pc, &mut cptr, &mut sp, &mut bp)? {
                                return Ok(false);
                            }
                            continue;
                        }
                    }
                }
            }
            op if op == REOP_NOT_WORD_BOUNDARY.as_u8() => {
                let before = if cptr == 0 {
                    false
                } else {
                    peek_prev_char(input, cptr).map(is_word_char).unwrap_or(false)
                };
                let after = if cptr >= input.len() {
                    false
                } else {
                    peek_char(input, cptr).map(is_word_char).unwrap_or(false)
                };
                let is_boundary = before ^ after;
                if is_boundary && !backtrack(capture, &mut pc, &mut cptr, &mut sp, &mut bp)? {
                    return Ok(false);
                }
            }
            _ => {
                return Err(RegExpError::InvalidBytecode("opcode"));
            }
        }
    }
}

fn save_capture(
    ctx: &JSContext,
    sp: &mut *mut JSValue,
    capture: &mut [u32],
    idx: usize,
    value: u32,
) -> Result<(), RegExpError> {
    check_stack_space(ctx, *sp, 2)?;
    let prev = capture[idx] as i32;
    unsafe {
        *sp = (*sp).sub(2);
        // SAFETY: stack has space for capture undo info.
        ptr::write_unaligned(*sp, JSValue::new_short_int(idx as i32));
        ptr::write_unaligned((*sp).add(1), JSValue::new_short_int(prev));
    }
    capture[idx] = value;
    Ok(())
}

fn save_capture_check(
    ctx: &JSContext,
    sp: &mut *mut JSValue,
    bp: &mut *mut JSValue,
    capture: &mut [u32],
    idx: usize,
    value: u32,
) -> Result<(), RegExpError> {
    let mut sp1 = *sp;
    loop {
        if sp1 < *bp {
            let saved_idx = unsafe { ptr::read_unaligned(sp1) };
            if !saved_idx.is_int() {
                return Err(RegExpError::InvalidValue("capture index"));
            }
            if saved_idx.get_int() == idx as i32 {
                break;
            }
            sp1 = unsafe { sp1.add(2) };
        } else {
            check_stack_space(ctx, *sp, 2)?;
            let prev = capture[idx] as i32;
            unsafe {
                *sp = (*sp).sub(2);
                // SAFETY: stack has space for capture undo info.
                ptr::write_unaligned(*sp, JSValue::new_short_int(idx as i32));
                ptr::write_unaligned((*sp).add(1), JSValue::new_short_int(prev));
            }
            break;
        }
    }
    capture[idx] = value;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::{ContextConfig, JSContext};
    use crate::parser::regexp::compile_regexp;
    use crate::property::get_property;
    use crate::stdlib::MQUICKJS_STDLIB_IMAGE;

    #[test]
    fn regexp_exec_match_array() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let source = ctx.new_string("a").expect("source");
        let bytecode = compile_regexp(b"a", 0).expect("regexp");
        let bytecode_val = ctx.alloc_byte_array(bytecode.bytes()).expect("bytecode");
        let re = new_regexp_object(&mut ctx, source, bytecode_val).expect("regexp object");
        let input = ctx.new_string("cat").expect("input");
        let result = regexp_exec(&mut ctx, re, input, RegExpExecMode::Exec).expect("exec");
        let index_key = ctx.intern_string(b"index").expect("index");
        let input_key = ctx.intern_string(b"input").expect("input key");
        let index_val = get_property(&mut ctx, result, index_key).expect("index prop");
        assert!(index_val.is_int());
        assert_eq!(index_val.get_int(), 1);
        let input_val = get_property(&mut ctx, result, input_key).expect("input prop");
        let mut scratch = [0u8; 5];
        let view = string_view(input_val, &mut scratch).expect("input view");
        assert_eq!(view.bytes(), b"cat");
        let match_val = get_property(&mut ctx, result, JSValue::new_short_int(0)).expect("match");
        let view = string_view(match_val, &mut scratch).expect("match view");
        assert_eq!(view.bytes(), b"a");
    }

    #[test]
    fn regexp_exec_global_updates_last_index() {
        let mut ctx = JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init");
        let source = ctx.new_string("a").expect("source");
        let bytecode = compile_regexp(b"a", LRE_FLAG_GLOBAL).expect("regexp");
        let bytecode_val = ctx.alloc_byte_array(bytecode.bytes()).expect("bytecode");
        let re = new_regexp_object(&mut ctx, source, bytecode_val).expect("regexp object");
        let input = ctx.new_string("aba").expect("input");
        let _ = regexp_exec(&mut ctx, re, input, RegExpExecMode::Exec).expect("exec");
        let obj_ptr = regexp_object_ptr(re).expect("regexp object");
        let re_ptr = regexp_payload_ptr(obj_ptr);
        let last_index = unsafe { ptr::read_unaligned(RegExp::last_index_ptr(re_ptr)) };
        assert_eq!(last_index, 1);
    }
}
