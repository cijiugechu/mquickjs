pub mod stdlib_def;
pub mod stdlib_image;
pub mod cfunc;

include!(concat!(env!("OUT_DIR"), "/stdlib_image.rs"));

use self::stdlib_def::BuiltinProto;

#[derive(Copy, Clone, Debug)]
pub struct CFuncWithProto<'a> {
    pub meta: &'a CFuncMeta,
    pub proto: BuiltinProto,
}

pub fn cfunc_proto(meta: &CFuncMeta) -> Option<BuiltinProto> {
    BuiltinProto::from_cproto_name(meta.cproto_name)
}

pub fn iter_cfuncs_with_proto(image: &StdlibImage) -> impl Iterator<Item = CFuncWithProto<'_>> {
    image
        .c_functions
        .iter()
        .filter_map(|meta| cfunc_proto(meta).map(|proto| CFuncWithProto { meta, proto }))
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    #[test]
    fn stdlib_cfunc_protos_are_known() {
        for meta in MQUICKJS_STDLIB_IMAGE.c_functions {
            assert!(
                cfunc_proto(meta).is_some(),
                "unknown cproto_name: {}",
                meta.cproto_name
            );
        }
    }
}
