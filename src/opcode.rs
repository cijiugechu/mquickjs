//! Opcode definitions ported from `mquickjs-c/mquickjs_opcode.h`.
//!
//! Invariants:
//! - The opcode order matches the C `OP_*` enum order.
//! - Comments like "must come after" are enforced by tests.

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[allow(non_camel_case_types)]
pub enum OpCodeFormat {
    none,
    none_int,
    none_loc,
    none_arg,
    none_var_ref,
    u8,
    i8,
    loc8,
    const8,
    label8,
    u16,
    i16,
    label16,
    npop,
    npopx,
    loc,
    arg,
    var_ref,
    u32,
    i32,
    const16,
    label,
    value,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct OpCodeInfo {
    pub name: &'static str,
    pub size: u8,
    pub n_pop: u8,
    pub n_push: u8,
    pub fmt: OpCodeFormat,
}

macro_rules! define_opcodes {
    ($(
        $op:ident, $name:expr, $size:expr, $n_pop:expr, $n_push:expr, $fmt:expr;
    )+ $(;)?) => {
        #[derive(Debug, Copy, Clone, PartialEq, Eq)]
        #[allow(non_camel_case_types)]
        #[repr(u16)]
        pub enum OpCode {
            $($op,)+
        }

        pub use OpCode::*;

        impl OpCode {
            pub const fn as_u8(self) -> u8 {
                self as u8
            }

            pub const fn as_u16(self) -> u16 {
                self as u16
            }

            pub const fn as_usize(self) -> usize {
                self as usize
            }

            pub fn from_u16(value: u16) -> Self {
                match value {
                    $(v if v == OpCode::$op as u16 => OpCode::$op,)+
                    _ => {
                        debug_assert!(false, "invalid opcode value: {}", value);
                        OpCode::OP_INVALID
                    }
                }
            }

            pub fn from_u8(value: u8) -> Self {
                Self::from_u16(value as u16)
            }
        }

        pub const OP_COUNT: usize = [$(stringify!($op)),+].len();

        pub const OPCODES: [OpCodeInfo; OP_COUNT] = [
            $(OpCodeInfo {
                name: $name,
                size: $size,
                n_pop: $n_pop,
                n_push: $n_push,
                fmt: $fmt,
            },)+
        ];
    };
}

define_opcodes! {
    OP_INVALID, "invalid", 1, 0, 0, OpCodeFormat::none;
    OP_PUSH_VALUE, "push_value", 5, 0, 1, OpCodeFormat::value;
    OP_PUSH_CONST, "push_const", 3, 0, 1, OpCodeFormat::const16;
    OP_FCLOSURE, "fclosure", 3, 0, 1, OpCodeFormat::const16;
    OP_UNDEFINED, "undefined", 1, 0, 1, OpCodeFormat::none;
    OP_NULL, "null", 1, 0, 1, OpCodeFormat::none;
    OP_PUSH_THIS, "push_this", 1, 0, 1, OpCodeFormat::none;
    OP_PUSH_FALSE, "push_false", 1, 0, 1, OpCodeFormat::none;
    OP_PUSH_TRUE, "push_true", 1, 0, 1, OpCodeFormat::none;
    OP_OBJECT, "object", 3, 0, 1, OpCodeFormat::u16;
    OP_THIS_FUNC, "this_func", 1, 0, 1, OpCodeFormat::none;
    OP_ARGUMENTS, "arguments", 1, 0, 1, OpCodeFormat::none;
    OP_NEW_TARGET, "new_target", 1, 0, 1, OpCodeFormat::none;
    OP_DROP, "drop", 1, 1, 0, OpCodeFormat::none;
    OP_NIP, "nip", 1, 2, 1, OpCodeFormat::none;
    OP_DUP, "dup", 1, 1, 2, OpCodeFormat::none;
    OP_DUP1, "dup1", 1, 2, 3, OpCodeFormat::none;
    OP_DUP2, "dup2", 1, 2, 4, OpCodeFormat::none;
    OP_INSERT2, "insert2", 1, 2, 3, OpCodeFormat::none;
    OP_INSERT3, "insert3", 1, 3, 4, OpCodeFormat::none;
    OP_PERM3, "perm3", 1, 3, 3, OpCodeFormat::none;
    OP_PERM4, "perm4", 1, 4, 4, OpCodeFormat::none;
    OP_SWAP, "swap", 1, 2, 2, OpCodeFormat::none;
    OP_ROT3L, "rot3l", 1, 3, 3, OpCodeFormat::none;
    OP_CALL_CONSTRUCTOR, "call_constructor", 3, 1, 1, OpCodeFormat::npop;
    OP_CALL, "call", 3, 1, 1, OpCodeFormat::npop;
    OP_CALL_METHOD, "call_method", 3, 2, 1, OpCodeFormat::npop;
    OP_ARRAY_FROM, "array_from", 3, 0, 1, OpCodeFormat::npop;
    OP_RETURN, "return", 1, 1, 0, OpCodeFormat::none;
    OP_RETURN_UNDEF, "return_undef", 1, 0, 0, OpCodeFormat::none;
    OP_THROW, "throw", 1, 1, 0, OpCodeFormat::none;
    OP_REGEXP, "regexp", 1, 2, 1, OpCodeFormat::none;
    OP_GET_FIELD, "get_field", 3, 1, 1, OpCodeFormat::const16;
    OP_GET_FIELD2, "get_field2", 3, 1, 2, OpCodeFormat::const16;
    OP_PUT_FIELD, "put_field", 3, 2, 0, OpCodeFormat::const16;
    OP_GET_ARRAY_EL, "get_array_el", 1, 2, 1, OpCodeFormat::none;
    OP_GET_ARRAY_EL2, "get_array_el2", 1, 2, 2, OpCodeFormat::none;
    OP_PUT_ARRAY_EL, "put_array_el", 1, 3, 0, OpCodeFormat::none;
    OP_GET_LENGTH, "get_length", 1, 1, 1, OpCodeFormat::none;
    OP_GET_LENGTH2, "get_length2", 1, 1, 2, OpCodeFormat::none;
    OP_DEFINE_FIELD, "define_field", 3, 2, 1, OpCodeFormat::const16;
    OP_DEFINE_GETTER, "define_getter", 3, 2, 1, OpCodeFormat::const16;
    OP_DEFINE_SETTER, "define_setter", 3, 2, 1, OpCodeFormat::const16;
    OP_SET_PROTO, "set_proto", 1, 2, 1, OpCodeFormat::none;
    OP_GET_LOC, "get_loc", 3, 0, 1, OpCodeFormat::loc;
    OP_PUT_LOC, "put_loc", 3, 1, 0, OpCodeFormat::loc;
    OP_GET_ARG, "get_arg", 3, 0, 1, OpCodeFormat::arg;
    OP_PUT_ARG, "put_arg", 3, 1, 0, OpCodeFormat::arg;
    OP_GET_VAR_REF, "get_var_ref", 3, 0, 1, OpCodeFormat::var_ref;
    OP_PUT_VAR_REF, "put_var_ref", 3, 1, 0, OpCodeFormat::var_ref;
    OP_GET_VAR_REF_NOCHECK, "get_var_ref_nocheck", 3, 0, 1, OpCodeFormat::var_ref;
    OP_PUT_VAR_REF_NOCHECK, "put_var_ref_nocheck", 3, 1, 0, OpCodeFormat::var_ref;
    OP_IF_FALSE, "if_false", 5, 1, 0, OpCodeFormat::label;
    OP_IF_TRUE, "if_true", 5, 1, 0, OpCodeFormat::label;
    OP_GOTO, "goto", 5, 0, 0, OpCodeFormat::label;
    OP_CATCH, "catch", 5, 0, 1, OpCodeFormat::label;
    OP_GOSUB, "gosub", 5, 0, 0, OpCodeFormat::label;
    OP_RET, "ret", 1, 1, 0, OpCodeFormat::none;
    OP_FOR_IN_START, "for_in_start", 1, 1, 1, OpCodeFormat::none;
    OP_FOR_OF_START, "for_of_start", 1, 1, 1, OpCodeFormat::none;
    OP_FOR_OF_NEXT, "for_of_next", 1, 1, 3, OpCodeFormat::none;
    OP_NEG, "neg", 1, 1, 1, OpCodeFormat::none;
    OP_PLUS, "plus", 1, 1, 1, OpCodeFormat::none;
    OP_DEC, "dec", 1, 1, 1, OpCodeFormat::none;
    OP_INC, "inc", 1, 1, 1, OpCodeFormat::none;
    OP_POST_DEC, "post_dec", 1, 1, 2, OpCodeFormat::none;
    OP_POST_INC, "post_inc", 1, 1, 2, OpCodeFormat::none;
    OP_NOT, "not", 1, 1, 1, OpCodeFormat::none;
    OP_LNOT, "lnot", 1, 1, 1, OpCodeFormat::none;
    OP_TYPEOF, "typeof", 1, 1, 1, OpCodeFormat::none;
    OP_DELETE, "delete", 1, 2, 1, OpCodeFormat::none;
    OP_MUL, "mul", 1, 2, 1, OpCodeFormat::none;
    OP_DIV, "div", 1, 2, 1, OpCodeFormat::none;
    OP_MOD, "mod", 1, 2, 1, OpCodeFormat::none;
    OP_ADD, "add", 1, 2, 1, OpCodeFormat::none;
    OP_SUB, "sub", 1, 2, 1, OpCodeFormat::none;
    OP_POW, "pow", 1, 2, 1, OpCodeFormat::none;
    OP_SHL, "shl", 1, 2, 1, OpCodeFormat::none;
    OP_SAR, "sar", 1, 2, 1, OpCodeFormat::none;
    OP_SHR, "shr", 1, 2, 1, OpCodeFormat::none;
    OP_LT, "lt", 1, 2, 1, OpCodeFormat::none;
    OP_LTE, "lte", 1, 2, 1, OpCodeFormat::none;
    OP_GT, "gt", 1, 2, 1, OpCodeFormat::none;
    OP_GTE, "gte", 1, 2, 1, OpCodeFormat::none;
    OP_INSTANCEOF, "instanceof", 1, 2, 1, OpCodeFormat::none;
    OP_IN, "in", 1, 2, 1, OpCodeFormat::none;
    OP_EQ, "eq", 1, 2, 1, OpCodeFormat::none;
    OP_NEQ, "neq", 1, 2, 1, OpCodeFormat::none;
    OP_STRICT_EQ, "strict_eq", 1, 2, 1, OpCodeFormat::none;
    OP_STRICT_NEQ, "strict_neq", 1, 2, 1, OpCodeFormat::none;
    OP_AND, "and", 1, 2, 1, OpCodeFormat::none;
    OP_XOR, "xor", 1, 2, 1, OpCodeFormat::none;
    OP_OR, "or", 1, 2, 1, OpCodeFormat::none;
    OP_NOP, "nop", 1, 0, 0, OpCodeFormat::none;
    OP_PUSH_MINUS1, "push_minus1", 1, 0, 1, OpCodeFormat::none_int;
    OP_PUSH_0, "push_0", 1, 0, 1, OpCodeFormat::none_int;
    OP_PUSH_1, "push_1", 1, 0, 1, OpCodeFormat::none_int;
    OP_PUSH_2, "push_2", 1, 0, 1, OpCodeFormat::none_int;
    OP_PUSH_3, "push_3", 1, 0, 1, OpCodeFormat::none_int;
    OP_PUSH_4, "push_4", 1, 0, 1, OpCodeFormat::none_int;
    OP_PUSH_5, "push_5", 1, 0, 1, OpCodeFormat::none_int;
    OP_PUSH_6, "push_6", 1, 0, 1, OpCodeFormat::none_int;
    OP_PUSH_7, "push_7", 1, 0, 1, OpCodeFormat::none_int;
    OP_PUSH_I8, "push_i8", 2, 0, 1, OpCodeFormat::i8;
    OP_PUSH_I16, "push_i16", 3, 0, 1, OpCodeFormat::i16;
    OP_PUSH_CONST8, "push_const8", 2, 0, 1, OpCodeFormat::const8;
    OP_FCLOSURE8, "fclosure8", 2, 0, 1, OpCodeFormat::const8;
    OP_PUSH_EMPTY_STRING, "push_empty_string", 1, 0, 1, OpCodeFormat::none;
    OP_GET_LOC8, "get_loc8", 2, 0, 1, OpCodeFormat::loc8;
    OP_PUT_LOC8, "put_loc8", 2, 1, 0, OpCodeFormat::loc8;
    OP_GET_LOC0, "get_loc0", 1, 0, 1, OpCodeFormat::none_loc;
    OP_GET_LOC1, "get_loc1", 1, 0, 1, OpCodeFormat::none_loc;
    OP_GET_LOC2, "get_loc2", 1, 0, 1, OpCodeFormat::none_loc;
    OP_GET_LOC3, "get_loc3", 1, 0, 1, OpCodeFormat::none_loc;
    OP_PUT_LOC0, "put_loc0", 1, 1, 0, OpCodeFormat::none_loc;
    OP_PUT_LOC1, "put_loc1", 1, 1, 0, OpCodeFormat::none_loc;
    OP_PUT_LOC2, "put_loc2", 1, 1, 0, OpCodeFormat::none_loc;
    OP_PUT_LOC3, "put_loc3", 1, 1, 0, OpCodeFormat::none_loc;
    OP_GET_ARG0, "get_arg0", 1, 0, 1, OpCodeFormat::none_arg;
    OP_GET_ARG1, "get_arg1", 1, 0, 1, OpCodeFormat::none_arg;
    OP_GET_ARG2, "get_arg2", 1, 0, 1, OpCodeFormat::none_arg;
    OP_GET_ARG3, "get_arg3", 1, 0, 1, OpCodeFormat::none_arg;
    OP_PUT_ARG0, "put_arg0", 1, 1, 0, OpCodeFormat::none_arg;
    OP_PUT_ARG1, "put_arg1", 1, 1, 0, OpCodeFormat::none_arg;
    OP_PUT_ARG2, "put_arg2", 1, 1, 0, OpCodeFormat::none_arg;
    OP_PUT_ARG3, "put_arg3", 1, 1, 0, OpCodeFormat::none_arg;
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct RegExpOpCodeInfo {
    pub name: &'static str,
    pub size: u8,
}

macro_rules! define_regexp_opcodes {
    ($(
        $op:ident, $name:expr, $size:expr;
    )+ $(;)?) => {
        #[derive(Debug, Copy, Clone, PartialEq, Eq)]
        #[allow(non_camel_case_types)]
        #[repr(u16)]
        pub enum RegExpOpCode {
            $($op,)+
        }

        pub use RegExpOpCode::*;

        impl RegExpOpCode {
            pub const fn as_u8(self) -> u8 {
                self as u8
            }

            pub const fn as_u16(self) -> u16 {
                self as u16
            }

            pub const fn as_usize(self) -> usize {
                self as usize
            }

            pub fn from_u16(value: u16) -> Self {
                match value {
                    $(v if v == RegExpOpCode::$op as u16 => RegExpOpCode::$op,)+
                    _ => {
                        debug_assert!(false, "invalid regexp opcode value: {}", value);
                        RegExpOpCode::REOP_INVALID
                    }
                }
            }

            pub fn from_u8(value: u8) -> Self {
                Self::from_u16(value as u16)
            }
        }

        pub const REOP_COUNT: usize = [$(stringify!($op)),+].len();

        pub const RE_OPCODES: [RegExpOpCodeInfo; REOP_COUNT] = [
            $(RegExpOpCodeInfo { name: $name, size: $size },)+
        ];
    };
}

define_regexp_opcodes! {
    REOP_INVALID, "invalid", 1;
    REOP_CHAR1, "char1", 2;
    REOP_CHAR2, "char2", 3;
    REOP_CHAR3, "char3", 4;
    REOP_CHAR4, "char4", 5;
    REOP_DOT, "dot", 1;
    REOP_ANY, "any", 1;
    REOP_SPACE, "space", 1;
    REOP_NOT_SPACE, "not_space", 1;
    REOP_LINE_START, "line_start", 1;
    REOP_LINE_START_M, "line_start_m", 1;
    REOP_LINE_END, "line_end", 1;
    REOP_LINE_END_M, "line_end_m", 1;
    REOP_GOTO, "goto", 5;
    REOP_SPLIT_GOTO_FIRST, "split_goto_first", 5;
    REOP_SPLIT_NEXT_FIRST, "split_next_first", 5;
    REOP_MATCH, "match", 1;
    REOP_LOOKAHEAD_MATCH, "lookahead_match", 1;
    REOP_NEGATIVE_LOOKAHEAD_MATCH, "negative_lookahead_match", 1;
    REOP_SAVE_START, "save_start", 2;
    REOP_SAVE_END, "save_end", 2;
    REOP_SAVE_RESET, "save_reset", 3;
    REOP_LOOP, "loop", 6;
    REOP_LOOP_SPLIT_GOTO_FIRST, "loop_split_goto_first", 10;
    REOP_LOOP_SPLIT_NEXT_FIRST, "loop_split_next_first", 10;
    REOP_LOOP_CHECK_ADV_SPLIT_GOTO_FIRST, "loop_check_adv_split_goto_first", 10;
    REOP_LOOP_CHECK_ADV_SPLIT_NEXT_FIRST, "loop_check_adv_split_next_first", 10;
    REOP_SET_I32, "set_i32", 6;
    REOP_WORD_BOUNDARY, "word_boundary", 1;
    REOP_NOT_WORD_BOUNDARY, "not_word_boundary", 1;
    REOP_BACK_REFERENCE, "back_reference", 2;
    REOP_BACK_REFERENCE_I, "back_reference_i", 2;
    REOP_RANGE8, "range8", 2;
    REOP_RANGE, "range", 3;
    REOP_LOOKAHEAD, "lookahead", 5;
    REOP_NEGATIVE_LOOKAHEAD, "negative_lookahead", 5;
    REOP_SET_CHAR_POS, "set_char_pos", 2;
    REOP_CHECK_ADVANCE, "check_advance", 2;
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    #[test]
    fn opcode_metadata_matches_indices() {
        assert_eq!(OPCODES.len(), OP_COUNT);
        assert_eq!(OPCODES[OP_INVALID.as_usize()].name, "invalid");
        assert_eq!(OPCODES[OP_PUSH_VALUE.as_usize()].size, 5);
        assert_eq!(OPCODES[OP_PUSH_VALUE.as_usize()].fmt, OpCodeFormat::value);
    }

    #[test]
    fn opcode_ordering_invariants() {
        assert!(OP_PUT_LOC.as_usize() > OP_GET_LOC.as_usize());
        assert!(OP_PUT_ARG.as_usize() > OP_GET_ARG.as_usize());
        assert!(OP_PUT_VAR_REF.as_usize() > OP_GET_VAR_REF.as_usize());
        assert!(OP_IF_TRUE.as_usize() > OP_IF_FALSE.as_usize());
        assert!(OP_GOTO.as_usize() > OP_IF_TRUE.as_usize());
        assert!(OP_FCLOSURE8.as_usize() > OP_PUSH_CONST8.as_usize());
        assert!(OP_PUT_LOC8.as_usize() > OP_GET_LOC8.as_usize());
        assert!(OP_PUT_LOC0.as_usize() > OP_GET_LOC.as_usize());
        assert!(OP_PUT_ARG0.as_usize() > OP_GET_ARG.as_usize());
        assert!(OP_NOP.as_usize() < OP_PUSH_MINUS1.as_usize());
    }

    #[test]
    fn regexp_opcode_ordering_invariants() {
        assert_eq!(RE_OPCODES.len(), REOP_COUNT);
        assert!(REOP_NOT_SPACE.as_usize() > REOP_SPACE.as_usize());
        assert!(REOP_NEGATIVE_LOOKAHEAD_MATCH.as_usize() > REOP_LOOKAHEAD_MATCH.as_usize());
        assert!(REOP_SAVE_END.as_usize() > REOP_SAVE_START.as_usize());
        assert!(REOP_NEGATIVE_LOOKAHEAD.as_usize() > REOP_LOOKAHEAD.as_usize());
    }
}
