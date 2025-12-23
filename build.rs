use mquickjs_build::{BuildInput, PropDef};
use std::env;
use std::path::Path;

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

    let global = [
        PropDef::CFunc {
            name: "noop",
            length: 0,
            magic: "0",
            cproto_name: "generic",
            func_name: "js_noop",
        },
        PropDef::PropString {
            name: "version",
            value: "v0",
        },
        PropDef::PropDouble {
            name: "answer",
            value: 1.5,
        },
        PropDef::End,
    ];

    let input = BuildInput {
        global_object: &global,
        c_function_decl: None,
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
