use crate::jsvalue::{JSValue, JSWord};
use core::ffi::c_void;

// C: `typedef struct JSContext JSContext;` (opaque handle).
#[repr(C)]
pub struct JSContext {
    _private: [u8; 0],
}

// C: `typedef struct { uint8_t buf[5]; } JSCStringBuf;`
#[repr(C)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct JSCStringBuf {
    pub buf: [u8; 5],
}

// C: `typedef JSValue JSCFunction(JSContext *ctx, JSValue *this_val, int argc, JSValue *argv);`
pub type JSCFunction = unsafe extern "C" fn(
    ctx: *mut JSContext,
    this_val: *mut JSValue,
    argc: i32,
    argv: *mut JSValue,
) -> JSValue;

// C: `typedef void (*JSCFinalizer)(JSContext *ctx, void *opaque);`
pub type JSCFinalizer = unsafe extern "C" fn(ctx: *mut JSContext, opaque: *mut c_void);

// C: `typedef void JSWriteFunc(void *opaque, const void *buf, size_t buf_len);`
pub type JSWriteFunc =
    unsafe extern "C" fn(opaque: *mut c_void, buf: *const c_void, buf_len: usize);

// C: `typedef int JSInterruptHandler(JSContext *ctx, void *opaque);`
// Return != 0 if the JS code needs to be interrupted.
pub type JSInterruptHandler = unsafe extern "C" fn(ctx: *mut JSContext, opaque: *mut c_void) -> i32;

// C: `JS_EVAL_*` flags in mquickjs.h.
pub const JS_EVAL_RETVAL: u32 = 1 << 0;
pub const JS_EVAL_REPL: u32 = 1 << 1;
pub const JS_EVAL_STRIP_COL: u32 = 1 << 2;
pub const JS_EVAL_JSON: u32 = 1 << 3;
pub const JS_EVAL_REGEXP: u32 = 1 << 4;
pub const JS_EVAL_REGEXP_FLAGS_SHIFT: u32 = 8;

// C: `typedef enum JSCFunctionDefEnum { ... } JSCFunctionDefEnum;`
#[repr(u8)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum JSCFunctionDefKind {
    Generic = 0,
    GenericMagic = 1,
    Constructor = 2,
    ConstructorMagic = 3,
    GenericParams = 4,
    FF = 5,
}

// C: `typedef union JSCFunctionType { ... } JSCFunctionType;`
#[repr(C)]
#[derive(Copy, Clone)]
pub union JSCFunctionType {
    pub generic: Option<JSCFunction>,
    pub generic_magic: Option<
        unsafe extern "C" fn(
            ctx: *mut JSContext,
            this_val: *mut JSValue,
            argc: i32,
            argv: *mut JSValue,
            magic: i32,
        ) -> JSValue,
    >,
    pub constructor: Option<JSCFunction>,
    pub constructor_magic: Option<
        unsafe extern "C" fn(
            ctx: *mut JSContext,
            this_val: *mut JSValue,
            argc: i32,
            argv: *mut JSValue,
            magic: i32,
        ) -> JSValue,
    >,
    pub generic_params: Option<
        unsafe extern "C" fn(
            ctx: *mut JSContext,
            this_val: *mut JSValue,
            argc: i32,
            argv: *mut JSValue,
            params: JSValue,
        ) -> JSValue,
    >,
    pub f_f: Option<unsafe extern "C" fn(f: f64) -> f64>,
}

// C: `typedef struct JSCFunctionDef { ... } JSCFunctionDef;`
#[repr(C)]
#[derive(Copy, Clone)]
pub struct JSCFunctionDef {
    pub func: JSCFunctionType,
    pub name: JSValue,
    pub def_type: JSCFunctionDefKind,
    pub arg_count: u8,
    pub magic: i16,
}

// C: `typedef struct { ... } JSSTDLibraryDef;`
#[repr(C)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct JSSTDLibraryDef {
    pub stdlib_table: *const JSWord,
    pub c_function_table: *const JSCFunctionDef,
    pub c_finalizer_table: *const JSCFinalizer,
    pub stdlib_table_len: u32,
    pub stdlib_table_align: u32,
    pub sorted_atoms_offset: u32,
    pub global_object_offset: u32,
    pub class_count: u32,
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;
    use core::mem::{align_of, offset_of, size_of};

    #[test]
    fn jsc_string_buf_layout() {
        assert_eq!(size_of::<JSCStringBuf>(), 5);
        assert_eq!(align_of::<JSCStringBuf>(), 1);
    }

    #[test]
    fn jsc_function_def_kind_discriminants() {
        assert_eq!(JSCFunctionDefKind::Generic as u8, 0);
        assert_eq!(JSCFunctionDefKind::GenericMagic as u8, 1);
        assert_eq!(JSCFunctionDefKind::Constructor as u8, 2);
        assert_eq!(JSCFunctionDefKind::ConstructorMagic as u8, 3);
        assert_eq!(JSCFunctionDefKind::GenericParams as u8, 4);
        assert_eq!(JSCFunctionDefKind::FF as u8, 5);
    }

    #[test]
    fn jsc_function_def_layout() {
        #[cfg(target_pointer_width = "64")]
        {
            assert_eq!(size_of::<JSCFunctionType>(), 8);
            assert_eq!(size_of::<JSCFunctionDef>(), 24);
            assert_eq!(align_of::<JSCFunctionDef>(), 8);
        }
        #[cfg(target_pointer_width = "32")]
        {
            assert_eq!(size_of::<JSCFunctionType>(), 4);
            assert_eq!(size_of::<JSCFunctionDef>(), 12);
            assert_eq!(align_of::<JSCFunctionDef>(), 4);
        }
        assert_eq!(offset_of!(JSCFunctionDef, func), 0);
        assert_eq!(offset_of!(JSCFunctionDef, name), size_of::<JSCFunctionType>());
    }

    #[test]
    fn stdlib_def_layout() {
        #[cfg(target_pointer_width = "64")]
        {
            assert_eq!(size_of::<JSSTDLibraryDef>(), 48);
            assert_eq!(align_of::<JSSTDLibraryDef>(), 8);
        }
        #[cfg(target_pointer_width = "32")]
        {
            assert_eq!(size_of::<JSSTDLibraryDef>(), 32);
            assert_eq!(align_of::<JSSTDLibraryDef>(), 4);
        }
        assert_eq!(offset_of!(JSSTDLibraryDef, stdlib_table), 0);
        assert_eq!(offset_of!(JSSTDLibraryDef, c_function_table), size_of::<*const JSWord>());
        assert_eq!(
            offset_of!(JSSTDLibraryDef, c_finalizer_table),
            size_of::<*const JSWord>() + size_of::<*const JSCFunctionDef>()
        );
    }

    #[test]
    fn eval_flag_constants_match_c() {
        assert_eq!(JS_EVAL_RETVAL, 1);
        assert_eq!(JS_EVAL_REPL, 2);
        assert_eq!(JS_EVAL_STRIP_COL, 4);
        assert_eq!(JS_EVAL_JSON, 8);
        assert_eq!(JS_EVAL_REGEXP, 16);
        assert_eq!(JS_EVAL_REGEXP_FLAGS_SHIFT, 8);
    }
}
