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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct OpCode(pub u16);

impl OpCode {
    pub const fn as_usize(self) -> usize {
        self.0 as usize
    }
}

pub const OP_INVALID: OpCode = OpCode(0);
pub const OP_PUSH_VALUE: OpCode = OpCode(1);
pub const OP_PUSH_CONST: OpCode = OpCode(2);
pub const OP_FCLOSURE: OpCode = OpCode(3);
pub const OP_UNDEFINED: OpCode = OpCode(4);
pub const OP_NULL: OpCode = OpCode(5);
pub const OP_PUSH_THIS: OpCode = OpCode(6);
pub const OP_PUSH_FALSE: OpCode = OpCode(7);
pub const OP_PUSH_TRUE: OpCode = OpCode(8);
pub const OP_OBJECT: OpCode = OpCode(9);
pub const OP_THIS_FUNC: OpCode = OpCode(10);
pub const OP_ARGUMENTS: OpCode = OpCode(11);
pub const OP_NEW_TARGET: OpCode = OpCode(12);
pub const OP_DROP: OpCode = OpCode(13);
pub const OP_NIP: OpCode = OpCode(14);
pub const OP_DUP: OpCode = OpCode(15);
pub const OP_DUP1: OpCode = OpCode(16);
pub const OP_DUP2: OpCode = OpCode(17);
pub const OP_INSERT2: OpCode = OpCode(18);
pub const OP_INSERT3: OpCode = OpCode(19);
pub const OP_PERM3: OpCode = OpCode(20);
pub const OP_PERM4: OpCode = OpCode(21);
pub const OP_SWAP: OpCode = OpCode(22);
pub const OP_ROT3L: OpCode = OpCode(23);
pub const OP_CALL_CONSTRUCTOR: OpCode = OpCode(24);
pub const OP_CALL: OpCode = OpCode(25);
pub const OP_CALL_METHOD: OpCode = OpCode(26);
pub const OP_ARRAY_FROM: OpCode = OpCode(27);
pub const OP_RETURN: OpCode = OpCode(28);
pub const OP_RETURN_UNDEF: OpCode = OpCode(29);
pub const OP_THROW: OpCode = OpCode(30);
pub const OP_REGEXP: OpCode = OpCode(31);
pub const OP_GET_FIELD: OpCode = OpCode(32);
pub const OP_GET_FIELD2: OpCode = OpCode(33);
pub const OP_PUT_FIELD: OpCode = OpCode(34);
pub const OP_GET_ARRAY_EL: OpCode = OpCode(35);
pub const OP_GET_ARRAY_EL2: OpCode = OpCode(36);
pub const OP_PUT_ARRAY_EL: OpCode = OpCode(37);
pub const OP_GET_LENGTH: OpCode = OpCode(38);
pub const OP_GET_LENGTH2: OpCode = OpCode(39);
pub const OP_DEFINE_FIELD: OpCode = OpCode(40);
pub const OP_DEFINE_GETTER: OpCode = OpCode(41);
pub const OP_DEFINE_SETTER: OpCode = OpCode(42);
pub const OP_SET_PROTO: OpCode = OpCode(43);
pub const OP_GET_LOC: OpCode = OpCode(44);
pub const OP_PUT_LOC: OpCode = OpCode(45);
pub const OP_GET_ARG: OpCode = OpCode(46);
pub const OP_PUT_ARG: OpCode = OpCode(47);
pub const OP_GET_VAR_REF: OpCode = OpCode(48);
pub const OP_PUT_VAR_REF: OpCode = OpCode(49);
pub const OP_GET_VAR_REF_NOCHECK: OpCode = OpCode(50);
pub const OP_PUT_VAR_REF_NOCHECK: OpCode = OpCode(51);
pub const OP_IF_FALSE: OpCode = OpCode(52);
pub const OP_IF_TRUE: OpCode = OpCode(53);
pub const OP_GOTO: OpCode = OpCode(54);
pub const OP_CATCH: OpCode = OpCode(55);
pub const OP_GOSUB: OpCode = OpCode(56);
pub const OP_RET: OpCode = OpCode(57);
pub const OP_FOR_IN_START: OpCode = OpCode(58);
pub const OP_FOR_OF_START: OpCode = OpCode(59);
pub const OP_FOR_OF_NEXT: OpCode = OpCode(60);
pub const OP_NEG: OpCode = OpCode(61);
pub const OP_PLUS: OpCode = OpCode(62);
pub const OP_DEC: OpCode = OpCode(63);
pub const OP_INC: OpCode = OpCode(64);
pub const OP_POST_DEC: OpCode = OpCode(65);
pub const OP_POST_INC: OpCode = OpCode(66);
pub const OP_NOT: OpCode = OpCode(67);
pub const OP_LNOT: OpCode = OpCode(68);
pub const OP_TYPEOF: OpCode = OpCode(69);
pub const OP_DELETE: OpCode = OpCode(70);
pub const OP_MUL: OpCode = OpCode(71);
pub const OP_DIV: OpCode = OpCode(72);
pub const OP_MOD: OpCode = OpCode(73);
pub const OP_ADD: OpCode = OpCode(74);
pub const OP_SUB: OpCode = OpCode(75);
pub const OP_POW: OpCode = OpCode(76);
pub const OP_SHL: OpCode = OpCode(77);
pub const OP_SAR: OpCode = OpCode(78);
pub const OP_SHR: OpCode = OpCode(79);
pub const OP_LT: OpCode = OpCode(80);
pub const OP_LTE: OpCode = OpCode(81);
pub const OP_GT: OpCode = OpCode(82);
pub const OP_GTE: OpCode = OpCode(83);
pub const OP_INSTANCEOF: OpCode = OpCode(84);
pub const OP_IN: OpCode = OpCode(85);
pub const OP_EQ: OpCode = OpCode(86);
pub const OP_NEQ: OpCode = OpCode(87);
pub const OP_STRICT_EQ: OpCode = OpCode(88);
pub const OP_STRICT_NEQ: OpCode = OpCode(89);
pub const OP_AND: OpCode = OpCode(90);
pub const OP_XOR: OpCode = OpCode(91);
pub const OP_OR: OpCode = OpCode(92);
pub const OP_NOP: OpCode = OpCode(93);
pub const OP_PUSH_MINUS1: OpCode = OpCode(94);
pub const OP_PUSH_0: OpCode = OpCode(95);
pub const OP_PUSH_1: OpCode = OpCode(96);
pub const OP_PUSH_2: OpCode = OpCode(97);
pub const OP_PUSH_3: OpCode = OpCode(98);
pub const OP_PUSH_4: OpCode = OpCode(99);
pub const OP_PUSH_5: OpCode = OpCode(100);
pub const OP_PUSH_6: OpCode = OpCode(101);
pub const OP_PUSH_7: OpCode = OpCode(102);
pub const OP_PUSH_I8: OpCode = OpCode(103);
pub const OP_PUSH_I16: OpCode = OpCode(104);
pub const OP_PUSH_CONST8: OpCode = OpCode(105);
pub const OP_FCLOSURE8: OpCode = OpCode(106);
pub const OP_PUSH_EMPTY_STRING: OpCode = OpCode(107);
pub const OP_GET_LOC8: OpCode = OpCode(108);
pub const OP_PUT_LOC8: OpCode = OpCode(109);
pub const OP_GET_LOC0: OpCode = OpCode(110);
pub const OP_GET_LOC1: OpCode = OpCode(111);
pub const OP_GET_LOC2: OpCode = OpCode(112);
pub const OP_GET_LOC3: OpCode = OpCode(113);
pub const OP_PUT_LOC0: OpCode = OpCode(114);
pub const OP_PUT_LOC1: OpCode = OpCode(115);
pub const OP_PUT_LOC2: OpCode = OpCode(116);
pub const OP_PUT_LOC3: OpCode = OpCode(117);
pub const OP_GET_ARG0: OpCode = OpCode(118);
pub const OP_GET_ARG1: OpCode = OpCode(119);
pub const OP_GET_ARG2: OpCode = OpCode(120);
pub const OP_GET_ARG3: OpCode = OpCode(121);
pub const OP_PUT_ARG0: OpCode = OpCode(122);
pub const OP_PUT_ARG1: OpCode = OpCode(123);
pub const OP_PUT_ARG2: OpCode = OpCode(124);
pub const OP_PUT_ARG3: OpCode = OpCode(125);

pub const OPCODES: [OpCodeInfo; 126] = [
    OpCodeInfo {
        name: "invalid",
        size: 1,
        n_pop: 0,
        n_push: 0,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "push_value",
        size: 5,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::value,
    },
    OpCodeInfo {
        name: "push_const",
        size: 3,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::const16,
    },
    OpCodeInfo {
        name: "fclosure",
        size: 3,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::const16,
    },
    OpCodeInfo {
        name: "undefined",
        size: 1,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "null",
        size: 1,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "push_this",
        size: 1,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "push_false",
        size: 1,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "push_true",
        size: 1,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "object",
        size: 3,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::u16,
    },
    OpCodeInfo {
        name: "this_func",
        size: 1,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "arguments",
        size: 1,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "new_target",
        size: 1,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "drop",
        size: 1,
        n_pop: 1,
        n_push: 0,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "nip",
        size: 1,
        n_pop: 2,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "dup",
        size: 1,
        n_pop: 1,
        n_push: 2,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "dup1",
        size: 1,
        n_pop: 2,
        n_push: 3,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "dup2",
        size: 1,
        n_pop: 2,
        n_push: 4,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "insert2",
        size: 1,
        n_pop: 2,
        n_push: 3,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "insert3",
        size: 1,
        n_pop: 3,
        n_push: 4,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "perm3",
        size: 1,
        n_pop: 3,
        n_push: 3,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "perm4",
        size: 1,
        n_pop: 4,
        n_push: 4,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "swap",
        size: 1,
        n_pop: 2,
        n_push: 2,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "rot3l",
        size: 1,
        n_pop: 3,
        n_push: 3,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "call_constructor",
        size: 3,
        n_pop: 1,
        n_push: 1,
        fmt: OpCodeFormat::npop,
    },
    OpCodeInfo {
        name: "call",
        size: 3,
        n_pop: 1,
        n_push: 1,
        fmt: OpCodeFormat::npop,
    },
    OpCodeInfo {
        name: "call_method",
        size: 3,
        n_pop: 2,
        n_push: 1,
        fmt: OpCodeFormat::npop,
    },
    OpCodeInfo {
        name: "array_from",
        size: 3,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::npop,
    },
    OpCodeInfo {
        name: "return",
        size: 1,
        n_pop: 1,
        n_push: 0,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "return_undef",
        size: 1,
        n_pop: 0,
        n_push: 0,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "throw",
        size: 1,
        n_pop: 1,
        n_push: 0,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "regexp",
        size: 1,
        n_pop: 2,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "get_field",
        size: 3,
        n_pop: 1,
        n_push: 1,
        fmt: OpCodeFormat::const16,
    },
    OpCodeInfo {
        name: "get_field2",
        size: 3,
        n_pop: 1,
        n_push: 2,
        fmt: OpCodeFormat::const16,
    },
    OpCodeInfo {
        name: "put_field",
        size: 3,
        n_pop: 2,
        n_push: 0,
        fmt: OpCodeFormat::const16,
    },
    OpCodeInfo {
        name: "get_array_el",
        size: 1,
        n_pop: 2,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "get_array_el2",
        size: 1,
        n_pop: 2,
        n_push: 2,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "put_array_el",
        size: 1,
        n_pop: 3,
        n_push: 0,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "get_length",
        size: 1,
        n_pop: 1,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "get_length2",
        size: 1,
        n_pop: 1,
        n_push: 2,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "define_field",
        size: 3,
        n_pop: 2,
        n_push: 1,
        fmt: OpCodeFormat::const16,
    },
    OpCodeInfo {
        name: "define_getter",
        size: 3,
        n_pop: 2,
        n_push: 1,
        fmt: OpCodeFormat::const16,
    },
    OpCodeInfo {
        name: "define_setter",
        size: 3,
        n_pop: 2,
        n_push: 1,
        fmt: OpCodeFormat::const16,
    },
    OpCodeInfo {
        name: "set_proto",
        size: 1,
        n_pop: 2,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "get_loc",
        size: 3,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::loc,
    },
    OpCodeInfo {
        name: "put_loc",
        size: 3,
        n_pop: 1,
        n_push: 0,
        fmt: OpCodeFormat::loc,
    },
    OpCodeInfo {
        name: "get_arg",
        size: 3,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::arg,
    },
    OpCodeInfo {
        name: "put_arg",
        size: 3,
        n_pop: 1,
        n_push: 0,
        fmt: OpCodeFormat::arg,
    },
    OpCodeInfo {
        name: "get_var_ref",
        size: 3,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::var_ref,
    },
    OpCodeInfo {
        name: "put_var_ref",
        size: 3,
        n_pop: 1,
        n_push: 0,
        fmt: OpCodeFormat::var_ref,
    },
    OpCodeInfo {
        name: "get_var_ref_nocheck",
        size: 3,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::var_ref,
    },
    OpCodeInfo {
        name: "put_var_ref_nocheck",
        size: 3,
        n_pop: 1,
        n_push: 0,
        fmt: OpCodeFormat::var_ref,
    },
    OpCodeInfo {
        name: "if_false",
        size: 5,
        n_pop: 1,
        n_push: 0,
        fmt: OpCodeFormat::label,
    },
    OpCodeInfo {
        name: "if_true",
        size: 5,
        n_pop: 1,
        n_push: 0,
        fmt: OpCodeFormat::label,
    },
    OpCodeInfo {
        name: "goto",
        size: 5,
        n_pop: 0,
        n_push: 0,
        fmt: OpCodeFormat::label,
    },
    OpCodeInfo {
        name: "catch",
        size: 5,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::label,
    },
    OpCodeInfo {
        name: "gosub",
        size: 5,
        n_pop: 0,
        n_push: 0,
        fmt: OpCodeFormat::label,
    },
    OpCodeInfo {
        name: "ret",
        size: 1,
        n_pop: 1,
        n_push: 0,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "for_in_start",
        size: 1,
        n_pop: 1,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "for_of_start",
        size: 1,
        n_pop: 1,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "for_of_next",
        size: 1,
        n_pop: 1,
        n_push: 3,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "neg",
        size: 1,
        n_pop: 1,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "plus",
        size: 1,
        n_pop: 1,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "dec",
        size: 1,
        n_pop: 1,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "inc",
        size: 1,
        n_pop: 1,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "post_dec",
        size: 1,
        n_pop: 1,
        n_push: 2,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "post_inc",
        size: 1,
        n_pop: 1,
        n_push: 2,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "not",
        size: 1,
        n_pop: 1,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "lnot",
        size: 1,
        n_pop: 1,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "typeof",
        size: 1,
        n_pop: 1,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "delete",
        size: 1,
        n_pop: 2,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "mul",
        size: 1,
        n_pop: 2,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "div",
        size: 1,
        n_pop: 2,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "mod",
        size: 1,
        n_pop: 2,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "add",
        size: 1,
        n_pop: 2,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "sub",
        size: 1,
        n_pop: 2,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "pow",
        size: 1,
        n_pop: 2,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "shl",
        size: 1,
        n_pop: 2,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "sar",
        size: 1,
        n_pop: 2,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "shr",
        size: 1,
        n_pop: 2,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "lt",
        size: 1,
        n_pop: 2,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "lte",
        size: 1,
        n_pop: 2,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "gt",
        size: 1,
        n_pop: 2,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "gte",
        size: 1,
        n_pop: 2,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "instanceof",
        size: 1,
        n_pop: 2,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "in",
        size: 1,
        n_pop: 2,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "eq",
        size: 1,
        n_pop: 2,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "neq",
        size: 1,
        n_pop: 2,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "strict_eq",
        size: 1,
        n_pop: 2,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "strict_neq",
        size: 1,
        n_pop: 2,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "and",
        size: 1,
        n_pop: 2,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "xor",
        size: 1,
        n_pop: 2,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "or",
        size: 1,
        n_pop: 2,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "nop",
        size: 1,
        n_pop: 0,
        n_push: 0,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "push_minus1",
        size: 1,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::none_int,
    },
    OpCodeInfo {
        name: "push_0",
        size: 1,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::none_int,
    },
    OpCodeInfo {
        name: "push_1",
        size: 1,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::none_int,
    },
    OpCodeInfo {
        name: "push_2",
        size: 1,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::none_int,
    },
    OpCodeInfo {
        name: "push_3",
        size: 1,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::none_int,
    },
    OpCodeInfo {
        name: "push_4",
        size: 1,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::none_int,
    },
    OpCodeInfo {
        name: "push_5",
        size: 1,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::none_int,
    },
    OpCodeInfo {
        name: "push_6",
        size: 1,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::none_int,
    },
    OpCodeInfo {
        name: "push_7",
        size: 1,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::none_int,
    },
    OpCodeInfo {
        name: "push_i8",
        size: 2,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::i8,
    },
    OpCodeInfo {
        name: "push_i16",
        size: 3,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::i16,
    },
    OpCodeInfo {
        name: "push_const8",
        size: 2,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::const8,
    },
    OpCodeInfo {
        name: "fclosure8",
        size: 2,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::const8,
    },
    OpCodeInfo {
        name: "push_empty_string",
        size: 1,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::none,
    },
    OpCodeInfo {
        name: "get_loc8",
        size: 2,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::loc8,
    },
    OpCodeInfo {
        name: "put_loc8",
        size: 2,
        n_pop: 1,
        n_push: 0,
        fmt: OpCodeFormat::loc8,
    },
    OpCodeInfo {
        name: "get_loc0",
        size: 1,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::none_loc,
    },
    OpCodeInfo {
        name: "get_loc1",
        size: 1,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::none_loc,
    },
    OpCodeInfo {
        name: "get_loc2",
        size: 1,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::none_loc,
    },
    OpCodeInfo {
        name: "get_loc3",
        size: 1,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::none_loc,
    },
    OpCodeInfo {
        name: "put_loc0",
        size: 1,
        n_pop: 1,
        n_push: 0,
        fmt: OpCodeFormat::none_loc,
    },
    OpCodeInfo {
        name: "put_loc1",
        size: 1,
        n_pop: 1,
        n_push: 0,
        fmt: OpCodeFormat::none_loc,
    },
    OpCodeInfo {
        name: "put_loc2",
        size: 1,
        n_pop: 1,
        n_push: 0,
        fmt: OpCodeFormat::none_loc,
    },
    OpCodeInfo {
        name: "put_loc3",
        size: 1,
        n_pop: 1,
        n_push: 0,
        fmt: OpCodeFormat::none_loc,
    },
    OpCodeInfo {
        name: "get_arg0",
        size: 1,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::none_arg,
    },
    OpCodeInfo {
        name: "get_arg1",
        size: 1,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::none_arg,
    },
    OpCodeInfo {
        name: "get_arg2",
        size: 1,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::none_arg,
    },
    OpCodeInfo {
        name: "get_arg3",
        size: 1,
        n_pop: 0,
        n_push: 1,
        fmt: OpCodeFormat::none_arg,
    },
    OpCodeInfo {
        name: "put_arg0",
        size: 1,
        n_pop: 1,
        n_push: 0,
        fmt: OpCodeFormat::none_arg,
    },
    OpCodeInfo {
        name: "put_arg1",
        size: 1,
        n_pop: 1,
        n_push: 0,
        fmt: OpCodeFormat::none_arg,
    },
    OpCodeInfo {
        name: "put_arg2",
        size: 1,
        n_pop: 1,
        n_push: 0,
        fmt: OpCodeFormat::none_arg,
    },
    OpCodeInfo {
        name: "put_arg3",
        size: 1,
        n_pop: 1,
        n_push: 0,
        fmt: OpCodeFormat::none_arg,
    },
];

pub const OP_COUNT: usize = OPCODES.len();

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct RegExpOpCodeInfo {
    pub name: &'static str,
    pub size: u8,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct RegExpOpCode(pub u16);

impl RegExpOpCode {
    pub const fn as_usize(self) -> usize {
        self.0 as usize
    }
}

pub const REOP_INVALID: RegExpOpCode = RegExpOpCode(0);
pub const REOP_CHAR1: RegExpOpCode = RegExpOpCode(1);
pub const REOP_CHAR2: RegExpOpCode = RegExpOpCode(2);
pub const REOP_CHAR3: RegExpOpCode = RegExpOpCode(3);
pub const REOP_CHAR4: RegExpOpCode = RegExpOpCode(4);
pub const REOP_DOT: RegExpOpCode = RegExpOpCode(5);
pub const REOP_ANY: RegExpOpCode = RegExpOpCode(6);
pub const REOP_SPACE: RegExpOpCode = RegExpOpCode(7);
pub const REOP_NOT_SPACE: RegExpOpCode = RegExpOpCode(8);
pub const REOP_LINE_START: RegExpOpCode = RegExpOpCode(9);
pub const REOP_LINE_START_M: RegExpOpCode = RegExpOpCode(10);
pub const REOP_LINE_END: RegExpOpCode = RegExpOpCode(11);
pub const REOP_LINE_END_M: RegExpOpCode = RegExpOpCode(12);
pub const REOP_GOTO: RegExpOpCode = RegExpOpCode(13);
pub const REOP_SPLIT_GOTO_FIRST: RegExpOpCode = RegExpOpCode(14);
pub const REOP_SPLIT_NEXT_FIRST: RegExpOpCode = RegExpOpCode(15);
pub const REOP_MATCH: RegExpOpCode = RegExpOpCode(16);
pub const REOP_LOOKAHEAD_MATCH: RegExpOpCode = RegExpOpCode(17);
pub const REOP_NEGATIVE_LOOKAHEAD_MATCH: RegExpOpCode = RegExpOpCode(18);
pub const REOP_SAVE_START: RegExpOpCode = RegExpOpCode(19);
pub const REOP_SAVE_END: RegExpOpCode = RegExpOpCode(20);
pub const REOP_SAVE_RESET: RegExpOpCode = RegExpOpCode(21);
pub const REOP_LOOP: RegExpOpCode = RegExpOpCode(22);
pub const REOP_LOOP_SPLIT_GOTO_FIRST: RegExpOpCode = RegExpOpCode(23);
pub const REOP_LOOP_SPLIT_NEXT_FIRST: RegExpOpCode = RegExpOpCode(24);
pub const REOP_LOOP_CHECK_ADV_SPLIT_GOTO_FIRST: RegExpOpCode = RegExpOpCode(25);
pub const REOP_LOOP_CHECK_ADV_SPLIT_NEXT_FIRST: RegExpOpCode = RegExpOpCode(26);
pub const REOP_SET_I32: RegExpOpCode = RegExpOpCode(27);
pub const REOP_WORD_BOUNDARY: RegExpOpCode = RegExpOpCode(28);
pub const REOP_NOT_WORD_BOUNDARY: RegExpOpCode = RegExpOpCode(29);
pub const REOP_BACK_REFERENCE: RegExpOpCode = RegExpOpCode(30);
pub const REOP_BACK_REFERENCE_I: RegExpOpCode = RegExpOpCode(31);
pub const REOP_RANGE8: RegExpOpCode = RegExpOpCode(32);
pub const REOP_RANGE: RegExpOpCode = RegExpOpCode(33);
pub const REOP_LOOKAHEAD: RegExpOpCode = RegExpOpCode(34);
pub const REOP_NEGATIVE_LOOKAHEAD: RegExpOpCode = RegExpOpCode(35);
pub const REOP_SET_CHAR_POS: RegExpOpCode = RegExpOpCode(36);
pub const REOP_CHECK_ADVANCE: RegExpOpCode = RegExpOpCode(37);

pub const RE_OPCODES: [RegExpOpCodeInfo; 38] = [
    RegExpOpCodeInfo {
        name: "invalid",
        size: 1,
    },
    RegExpOpCodeInfo {
        name: "char1",
        size: 2,
    },
    RegExpOpCodeInfo {
        name: "char2",
        size: 3,
    },
    RegExpOpCodeInfo {
        name: "char3",
        size: 4,
    },
    RegExpOpCodeInfo {
        name: "char4",
        size: 5,
    },
    RegExpOpCodeInfo {
        name: "dot",
        size: 1,
    },
    RegExpOpCodeInfo {
        name: "any",
        size: 1,
    },
    RegExpOpCodeInfo {
        name: "space",
        size: 1,
    },
    RegExpOpCodeInfo {
        name: "not_space",
        size: 1,
    },
    RegExpOpCodeInfo {
        name: "line_start",
        size: 1,
    },
    RegExpOpCodeInfo {
        name: "line_start_m",
        size: 1,
    },
    RegExpOpCodeInfo {
        name: "line_end",
        size: 1,
    },
    RegExpOpCodeInfo {
        name: "line_end_m",
        size: 1,
    },
    RegExpOpCodeInfo {
        name: "goto",
        size: 5,
    },
    RegExpOpCodeInfo {
        name: "split_goto_first",
        size: 5,
    },
    RegExpOpCodeInfo {
        name: "split_next_first",
        size: 5,
    },
    RegExpOpCodeInfo {
        name: "match",
        size: 1,
    },
    RegExpOpCodeInfo {
        name: "lookahead_match",
        size: 1,
    },
    RegExpOpCodeInfo {
        name: "negative_lookahead_match",
        size: 1,
    },
    RegExpOpCodeInfo {
        name: "save_start",
        size: 2,
    },
    RegExpOpCodeInfo {
        name: "save_end",
        size: 2,
    },
    RegExpOpCodeInfo {
        name: "save_reset",
        size: 3,
    },
    RegExpOpCodeInfo {
        name: "loop",
        size: 6,
    },
    RegExpOpCodeInfo {
        name: "loop_split_goto_first",
        size: 10,
    },
    RegExpOpCodeInfo {
        name: "loop_split_next_first",
        size: 10,
    },
    RegExpOpCodeInfo {
        name: "loop_check_adv_split_goto_first",
        size: 10,
    },
    RegExpOpCodeInfo {
        name: "loop_check_adv_split_next_first",
        size: 10,
    },
    RegExpOpCodeInfo {
        name: "set_i32",
        size: 6,
    },
    RegExpOpCodeInfo {
        name: "word_boundary",
        size: 1,
    },
    RegExpOpCodeInfo {
        name: "not_word_boundary",
        size: 1,
    },
    RegExpOpCodeInfo {
        name: "back_reference",
        size: 2,
    },
    RegExpOpCodeInfo {
        name: "back_reference_i",
        size: 2,
    },
    RegExpOpCodeInfo {
        name: "range8",
        size: 2,
    },
    RegExpOpCodeInfo {
        name: "range",
        size: 3,
    },
    RegExpOpCodeInfo {
        name: "lookahead",
        size: 5,
    },
    RegExpOpCodeInfo {
        name: "negative_lookahead",
        size: 5,
    },
    RegExpOpCodeInfo {
        name: "set_char_pos",
        size: 2,
    },
    RegExpOpCodeInfo {
        name: "check_advance",
        size: 2,
    },
];

pub const REOP_COUNT: usize = RE_OPCODES.len();

#[cfg(test)]
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
