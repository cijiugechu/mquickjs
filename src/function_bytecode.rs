use crate::jsvalue::{JSValue, JSWord, JSW};
use crate::memblock::{MbHeader, MTag, JS_MTAG_BITS};

// C: `JSFunctionBytecode` header bitfields in mquickjs.c.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct FunctionBytecodeHeader(JSWord);

impl FunctionBytecodeHeader {
    pub const ARG_COUNT_BITS: u32 = 16;
    pub const HAS_ARGUMENTS_SHIFT: u32 = JS_MTAG_BITS;
    pub const HAS_LOCAL_FUNC_NAME_SHIFT: u32 = JS_MTAG_BITS + 1;
    pub const HAS_COLUMN_SHIFT: u32 = JS_MTAG_BITS + 2;
    pub const ARG_COUNT_SHIFT: u32 = JS_MTAG_BITS + 3;

    pub const ARG_COUNT_MASK: JSWord = ((1 as JSWord) << Self::ARG_COUNT_BITS) - 1;

    pub const fn word_bits() -> u32 {
        (JSW as u32) * 8
    }

    pub const DUMMY_BITS: u32 = Self::word_bits() - (JS_MTAG_BITS + 3 + Self::ARG_COUNT_BITS);

    pub fn new(
        has_arguments: bool,
        has_local_func_name: bool,
        has_column: bool,
        arg_count: u16,
        gc_mark: bool,
    ) -> Self {
        debug_assert!((arg_count as JSWord) <= Self::ARG_COUNT_MASK);
        let mut word = MbHeader::new(MTag::FunctionBytecode, gc_mark).word();
        if has_arguments {
            word |= (1 as JSWord) << Self::HAS_ARGUMENTS_SHIFT;
        }
        if has_local_func_name {
            word |= (1 as JSWord) << Self::HAS_LOCAL_FUNC_NAME_SHIFT;
        }
        if has_column {
            word |= (1 as JSWord) << Self::HAS_COLUMN_SHIFT;
        }
        word |= (arg_count as JSWord) << Self::ARG_COUNT_SHIFT;
        Self(word)
    }

    pub const fn header(self) -> MbHeader {
        MbHeader::from_word(self.0)
    }

    pub const fn gc_mark(self) -> bool {
        MbHeader::from_word(self.0).gc_mark()
    }

    pub fn tag(self) -> MTag {
        MbHeader::from_word(self.0).tag()
    }

    pub const fn has_arguments(self) -> bool {
        ((self.0 >> Self::HAS_ARGUMENTS_SHIFT) & 1) != 0
    }

    pub const fn has_local_func_name(self) -> bool {
        ((self.0 >> Self::HAS_LOCAL_FUNC_NAME_SHIFT) & 1) != 0
    }

    pub const fn has_column(self) -> bool {
        ((self.0 >> Self::HAS_COLUMN_SHIFT) & 1) != 0
    }

    pub const fn arg_count(self) -> u16 {
        ((self.0 >> Self::ARG_COUNT_SHIFT) & Self::ARG_COUNT_MASK) as u16
    }
}

// C: `JSFunctionBytecode` payload fields in mquickjs.c (excluding header).
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct FunctionBytecodeFields {
    pub func_name: JSValue,
    pub byte_code: JSValue,
    pub cpool: JSValue,
    pub vars: JSValue,
    pub ext_vars: JSValue,
    pub stack_size: u16,
    pub ext_vars_len: u16,
    pub filename: JSValue,
    pub pc2line: JSValue,
    pub source_pos: u32,
}

// C: `JSFunctionBytecode` in mquickjs.c.
#[repr(C)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct FunctionBytecode {
    header: FunctionBytecodeHeader,
    func_name: JSValue,
    byte_code: JSValue,
    cpool: JSValue,
    vars: JSValue,
    ext_vars: JSValue,
    stack_size: u16,
    ext_vars_len: u16,
    filename: JSValue,
    pc2line: JSValue,
    source_pos: u32,
}

impl FunctionBytecode {
    pub const fn from_fields(header: FunctionBytecodeHeader, fields: FunctionBytecodeFields) -> Self {
        Self {
            header,
            func_name: fields.func_name,
            byte_code: fields.byte_code,
            cpool: fields.cpool,
            vars: fields.vars,
            ext_vars: fields.ext_vars,
            stack_size: fields.stack_size,
            ext_vars_len: fields.ext_vars_len,
            filename: fields.filename,
            pc2line: fields.pc2line,
            source_pos: fields.source_pos,
        }
    }

    pub const fn header(self) -> FunctionBytecodeHeader {
        self.header
    }

    pub const fn func_name(self) -> JSValue {
        self.func_name
    }

    pub const fn byte_code(self) -> JSValue {
        self.byte_code
    }

    pub const fn cpool(self) -> JSValue {
        self.cpool
    }

    pub const fn vars(self) -> JSValue {
        self.vars
    }

    pub const fn ext_vars(self) -> JSValue {
        self.ext_vars
    }

    pub const fn stack_size(self) -> u16 {
        self.stack_size
    }

    pub const fn ext_vars_len(self) -> u16 {
        self.ext_vars_len
    }

    pub const fn filename(self) -> JSValue {
        self.filename
    }

    pub const fn pc2line(self) -> JSValue {
        self.pc2line
    }

    pub const fn source_pos(self) -> u32 {
        self.source_pos
    }
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    #[test]
    fn function_bytecode_header_roundtrip() {
        let header = FunctionBytecodeHeader::new(true, false, true, 17, true);
        assert_eq!(header.tag(), MTag::FunctionBytecode);
        assert!(header.gc_mark());
        assert!(header.has_arguments());
        assert!(!header.has_local_func_name());
        assert!(header.has_column());
        assert_eq!(header.arg_count(), 17);
    }

    #[test]
    fn function_bytecode_header_max_arg_count() {
        let header = FunctionBytecodeHeader::new(false, true, false, u16::MAX, false);
        assert_eq!(header.arg_count(), u16::MAX);
        assert!(!header.gc_mark());
        assert!(!header.has_arguments());
        assert!(header.has_local_func_name());
        assert!(!header.has_column());
    }

    #[test]
    fn function_bytecode_roundtrip() {
        let header = FunctionBytecodeHeader::new(false, false, false, 0, false);
        let fields = FunctionBytecodeFields {
            func_name: crate::jsvalue::JS_NULL,
            byte_code: crate::jsvalue::JS_UNDEFINED,
            cpool: crate::jsvalue::JS_NULL,
            vars: crate::jsvalue::JS_UNDEFINED,
            ext_vars: crate::jsvalue::JS_NULL,
            stack_size: 12,
            ext_vars_len: 2,
            filename: crate::jsvalue::JS_NULL,
            pc2line: crate::jsvalue::JS_UNDEFINED,
            source_pos: 99,
        };
        let func = FunctionBytecode::from_fields(header, fields);
        assert_eq!(func.header(), header);
        assert_eq!(func.func_name(), crate::jsvalue::JS_NULL);
        assert_eq!(func.byte_code(), crate::jsvalue::JS_UNDEFINED);
        assert_eq!(func.cpool(), crate::jsvalue::JS_NULL);
        assert_eq!(func.vars(), crate::jsvalue::JS_UNDEFINED);
        assert_eq!(func.ext_vars(), crate::jsvalue::JS_NULL);
        assert_eq!(func.stack_size(), 12);
        assert_eq!(func.ext_vars_len(), 2);
        assert_eq!(func.filename(), crate::jsvalue::JS_NULL);
        assert_eq!(func.pc2line(), crate::jsvalue::JS_UNDEFINED);
        assert_eq!(func.source_pos(), 99);
    }
}
