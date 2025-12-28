use mquickjs_build::{BuildInput, ClassDef, PropDef};
use std::env;
use std::path::Path;

static JS_OBJECT_PROTO: [PropDef; 3] = [
    PropDef::CFunc {
        name: "hasOwnProperty",
        length: 1,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_object_hasOwnProperty",
    },
    PropDef::CFunc {
        name: "toString",
        length: 0,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_object_toString",
    },
    PropDef::End,
];

static JS_OBJECT: [PropDef; 6] = [
    PropDef::CFunc {
        name: "defineProperty",
        length: 3,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_object_defineProperty",
    },
    PropDef::CFunc {
        name: "getPrototypeOf",
        length: 1,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_object_getPrototypeOf",
    },
    PropDef::CFunc {
        name: "setPrototypeOf",
        length: 2,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_object_setPrototypeOf",
    },
    PropDef::CFunc {
        name: "create",
        length: 2,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_object_create",
    },
    PropDef::CFunc {
        name: "keys",
        length: 1,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_object_keys",
    },
    PropDef::End,
];

static JS_OBJECT_CLASS: ClassDef = ClassDef {
    name: "Object",
    length: 1,
    cproto_name: Some("constructor"),
    func_name: Some("js_object_constructor"),
    class_id: Some("JS_CLASS_OBJECT"),
    class_props: Some(&JS_OBJECT),
    proto_props: Some(&JS_OBJECT_PROTO),
    parent_class: None,
    finalizer_name: None,
};

static JS_FUNCTION_PROTO: [PropDef; 8] = [
    PropDef::CGetSet {
        name: "prototype",
        magic: "0",
        cproto_name: "generic",
        get_func_name: Some("js_function_get_prototype"),
        set_func_name: Some("js_function_set_prototype"),
    },
    PropDef::CFunc {
        name: "call",
        length: 1,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_function_call",
    },
    PropDef::CFunc {
        name: "apply",
        length: 2,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_function_apply",
    },
    PropDef::CFunc {
        name: "bind",
        length: 1,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_function_bind",
    },
    PropDef::CFunc {
        name: "toString",
        length: 0,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_function_toString",
    },
    PropDef::CGetSet {
        name: "length",
        magic: "0",
        cproto_name: "generic_magic",
        get_func_name: Some("js_function_get_length_name"),
        set_func_name: None,
    },
    PropDef::CGetSet {
        name: "name",
        magic: "1",
        cproto_name: "generic_magic",
        get_func_name: Some("js_function_get_length_name"),
        set_func_name: None,
    },
    PropDef::End,
];

static JS_FUNCTION_CLASS: ClassDef = ClassDef {
    name: "Function",
    length: 1,
    cproto_name: Some("constructor"),
    func_name: Some("js_function_constructor"),
    class_id: Some("JS_CLASS_CLOSURE"),
    class_props: None,
    proto_props: Some(&JS_FUNCTION_PROTO),
    parent_class: None,
    finalizer_name: None,
};

static JS_GLOBAL_OBJECT: [PropDef; 4] = [
    PropDef::Class {
        name: "Object",
        class: &JS_OBJECT_CLASS,
    },
    PropDef::Class {
        name: "Function",
        class: &JS_FUNCTION_CLASS,
    },
    PropDef::PropNull { name: "globalThis" },
    PropDef::End,
];

static JS_C_FUNCTION_DECL: [PropDef; 2] = [
    PropDef::CFunc {
        name: "bound",
        length: 0,
        magic: "0",
        cproto_name: "generic_params",
        func_name: "js_function_bound",
    },
    PropDef::End,
];

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=crates/mquickjs-build/src/lib.rs");
    println!("cargo:rerun-if-changed=crates/mquickjs-build/Cargo.toml");
    println!("cargo:rerun-if-env-changed=MQUICKJS_WORD_BYTES");

    let word_bytes = match env::var("MQUICKJS_WORD_BYTES").as_deref() {
        Ok("4") => 4,
        Ok("8") => 8,
        _ => match env::var("CARGO_CFG_TARGET_POINTER_WIDTH").as_deref() {
            Ok("64") => 8,
            _ => 4,
        },
    };

    let input = BuildInput {
        global_object: &JS_GLOBAL_OBJECT,
        c_function_decl: Some(&JS_C_FUNCTION_DECL),
        predefined_atoms: mquickjs_build::DEFAULT_PREDEFINED_ATOMS,
        word_bytes,
        class_count: mquickjs_build::default_class_count(),
    };

    let image = mquickjs_build::build_stdlib(&input).expect("build stdlib image");
    let output = mquickjs_build::render_rust(&image, "MQUICKJS_STDLIB_IMAGE");

    let out_dir = env::var("OUT_DIR").expect("OUT_DIR not set");
    let out_path = Path::new(&out_dir).join("stdlib_image.rs");
    std::fs::write(&out_path, output).expect("write stdlib_image.rs");
}
