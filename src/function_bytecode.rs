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

    pub const fn has_arguments(self) -> bool {
        self.header.has_arguments()
    }

    pub const fn has_local_func_name(self) -> bool {
        self.header.has_local_func_name()
    }

    pub const fn has_column(self) -> bool {
        self.header.has_column()
    }

    pub const fn arg_count(self) -> u16 {
        self.header.arg_count()
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

    pub fn set_vars(&mut self, vars: JSValue) {
        self.vars = vars;
    }

    pub fn set_stack_size(&mut self, stack_size: u16) {
        self.stack_size = stack_size;
    }

    pub fn set_ext_vars(&mut self, ext_vars: JSValue) {
        self.ext_vars = ext_vars;
    }

    pub fn set_cpool(&mut self, cpool: JSValue) {
        self.cpool = cpool;
    }

    pub fn set_ext_vars_len(&mut self, len: u16) {
        self.ext_vars_len = len;
    }

    pub fn set_byte_code(&mut self, byte_code: JSValue) {
        self.byte_code = byte_code;
    }

    pub fn set_pc2line(&mut self, pc2line: JSValue) {
        self.pc2line = pc2line;
    }

    pub fn set_arg_count(&mut self, arg_count: u16) {
        self.rebuild_header(
            self.header.has_arguments(),
            self.header.has_local_func_name(),
            self.header.has_column(),
            arg_count,
        );
    }

    pub fn set_has_arguments(&mut self, has_arguments: bool) {
        self.rebuild_header(
            has_arguments,
            self.header.has_local_func_name(),
            self.header.has_column(),
            self.header.arg_count(),
        );
    }

    pub fn set_has_local_func_name(&mut self, has_local_func_name: bool) {
        self.rebuild_header(
            self.header.has_arguments(),
            has_local_func_name,
            self.header.has_column(),
            self.header.arg_count(),
        );
    }

    pub fn set_has_column(&mut self, has_column: bool) {
        self.rebuild_header(
            self.header.has_arguments(),
            self.header.has_local_func_name(),
            has_column,
            self.header.arg_count(),
        );
    }

    fn rebuild_header(
        &mut self,
        has_arguments: bool,
        has_local_func_name: bool,
        has_column: bool,
        arg_count: u16,
    ) {
        let header = FunctionBytecodeHeader::new(
            has_arguments,
            has_local_func_name,
            has_column,
            arg_count,
            self.header.gc_mark(),
        );
        self.header = header;
    }

    pub(crate) unsafe fn func_name_ptr(this: *mut Self) -> *mut JSValue {
        // SAFETY: caller guarantees `this` is valid for writes.
        unsafe { core::ptr::addr_of_mut!((*this).func_name) }
    }

    pub(crate) unsafe fn byte_code_ptr(this: *mut Self) -> *mut JSValue {
        // SAFETY: caller guarantees `this` is valid for writes.
        unsafe { core::ptr::addr_of_mut!((*this).byte_code) }
    }

    pub(crate) unsafe fn cpool_ptr(this: *mut Self) -> *mut JSValue {
        // SAFETY: caller guarantees `this` is valid for writes.
        unsafe { core::ptr::addr_of_mut!((*this).cpool) }
    }

    pub(crate) unsafe fn vars_ptr(this: *mut Self) -> *mut JSValue {
        // SAFETY: caller guarantees `this` is valid for writes.
        unsafe { core::ptr::addr_of_mut!((*this).vars) }
    }

    pub(crate) unsafe fn ext_vars_ptr(this: *mut Self) -> *mut JSValue {
        // SAFETY: caller guarantees `this` is valid for writes.
        unsafe { core::ptr::addr_of_mut!((*this).ext_vars) }
    }

    pub(crate) unsafe fn filename_ptr(this: *mut Self) -> *mut JSValue {
        // SAFETY: caller guarantees `this` is valid for writes.
        unsafe { core::ptr::addr_of_mut!((*this).filename) }
    }

    pub(crate) unsafe fn pc2line_ptr(this: *mut Self) -> *mut JSValue {
        // SAFETY: caller guarantees `this` is valid for writes.
        unsafe { core::ptr::addr_of_mut!((*this).pc2line) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn function_bytecode_header_max_arg_count() {
        let header = FunctionBytecodeHeader::new(false, true, false, u16::MAX, false);
        assert_eq!(header.arg_count(), u16::MAX);
        assert!(!header.gc_mark());
        assert!(!header.has_arguments());
        assert!(header.has_local_func_name());
        assert!(!header.has_column());
    }
}
