use crate::builtins;
use crate::context::{ContextError, JSContext};
use crate::enums::JSObjectClass;
use crate::js_libm;
use crate::jsvalue::JSValue;
use crate::stdlib::stdlib_def::BuiltinProto;
use crate::stdlib::stdlib_image::StdlibImage;

pub type CFuncGeneric = fn(&mut JSContext, JSValue, &[JSValue]) -> JSValue;
pub type CFuncGenericMagic = fn(&mut JSContext, JSValue, &[JSValue], i32) -> JSValue;
pub type CFuncConstructor = fn(&mut JSContext, JSValue, &[JSValue]) -> JSValue;
pub type CFuncConstructorMagic = fn(&mut JSContext, JSValue, &[JSValue], i32) -> JSValue;
pub type CFuncGenericParams = fn(&mut JSContext, JSValue, &[JSValue], JSValue) -> JSValue;
pub type CFuncFF = fn(f64) -> f64;

#[derive(Copy, Clone, Debug)]
pub enum BuiltinCFunction {
    Generic(CFuncGeneric),
    GenericMagic(CFuncGenericMagic),
    Constructor(CFuncConstructor),
    ConstructorMagic(CFuncConstructorMagic),
    GenericParams(CFuncGenericParams),
    FF(CFuncFF),
    Missing(BuiltinProto),
}

#[derive(Copy, Clone, Debug)]
pub struct CFunctionDef {
    pub name: JSValue,
    pub name_str: &'static str,
    pub proto: BuiltinProto,
    pub arg_count: u8,
    pub magic: i16,
    pub func_name: &'static str,
    pub func: BuiltinCFunction,
}

pub fn build_c_function_table(
    ctx: &mut JSContext,
    image: &StdlibImage,
) -> Result<Vec<CFunctionDef>, ContextError> {
    let mut table = Vec::with_capacity(image.c_functions.len());
    for meta in image.c_functions {
        let proto = BuiltinProto::from_cproto_name(meta.cproto_name).ok_or(
            ContextError::InvalidCFunctionProto {
                name: meta.name,
                proto: meta.cproto_name,
            },
        )?;
        let magic = magic_from_name(meta.magic).ok_or(ContextError::InvalidCFunctionMagic {
            name: meta.name,
            magic: meta.magic,
        })?;
        let name = ctx.intern_string(meta.name.as_bytes())?;
        let func = resolve_builtin_func(meta.func_name, proto);
        table.push(CFunctionDef {
            name,
            name_str: meta.name,
            proto,
            arg_count: meta.arg_count,
            magic,
            func_name: meta.func_name,
            func,
        });
    }
    Ok(table)
}

fn resolve_builtin_func(func_name: &str, proto: BuiltinProto) -> BuiltinCFunction {
    match proto {
        BuiltinProto::Generic => match func_name {
            // Number
            "js_number_toString" => BuiltinCFunction::Generic(builtins::js_number_toString),
            "js_number_toFixed" => BuiltinCFunction::Generic(builtins::js_number_toFixed),
            "js_number_toExponential" => {
                BuiltinCFunction::Generic(builtins::js_number_toExponential)
            }
            "js_number_toPrecision" => BuiltinCFunction::Generic(builtins::js_number_toPrecision),
            "js_number_parseInt" => BuiltinCFunction::Generic(builtins::js_number_parseInt),
            "js_number_parseFloat" => BuiltinCFunction::Generic(builtins::js_number_parseFloat),
            // Math
            "js_math_imul" => BuiltinCFunction::Generic(builtins::js_math_imul),
            "js_math_clz32" => BuiltinCFunction::Generic(builtins::js_math_clz32),
            "js_math_atan2" => BuiltinCFunction::Generic(builtins::js_math_atan2),
            "js_math_pow" => BuiltinCFunction::Generic(builtins::js_math_pow),
            "js_math_random" => BuiltinCFunction::Generic(builtins::js_math_random),
            // Date
            "js_date_now" => BuiltinCFunction::Generic(builtins::js_date_now),
            // Global
            "js_global_isNaN" => BuiltinCFunction::Generic(builtins::js_global_isNaN),
            "js_global_isFinite" => BuiltinCFunction::Generic(builtins::js_global_isFinite),
            // Object
            "js_object_hasOwnProperty" => {
                BuiltinCFunction::Generic(builtins::js_object_hasOwnProperty)
            }
            "js_object_toString" => BuiltinCFunction::Generic(builtins::js_object_toString),
            "js_object_defineProperty" => {
                BuiltinCFunction::Generic(builtins::js_object_defineProperty)
            }
            "js_object_getPrototypeOf" => {
                BuiltinCFunction::Generic(builtins::js_object_getPrototypeOf)
            }
            "js_object_setPrototypeOf" => {
                BuiltinCFunction::Generic(builtins::js_object_setPrototypeOf)
            }
            "js_object_create" => BuiltinCFunction::Generic(builtins::js_object_create),
            "js_object_keys" => BuiltinCFunction::Generic(builtins::js_object_keys),
            // Function
            "js_function_call" => BuiltinCFunction::Generic(builtins::js_function_call),
            "js_function_apply" => BuiltinCFunction::Generic(builtins::js_function_apply),
            "js_function_bind" => BuiltinCFunction::Generic(builtins::js_function_bind),
            "js_function_toString" => BuiltinCFunction::Generic(builtins::js_function_toString),
            "js_function_get_prototype" => {
                BuiltinCFunction::Generic(builtins::js_function_get_prototype)
            }
            "js_function_set_prototype" => {
                BuiltinCFunction::Generic(builtins::js_function_set_prototype)
            }
            // String
            "js_string_get_length" => BuiltinCFunction::Generic(builtins::js_string_get_length),
            "js_string_set_length" => BuiltinCFunction::Generic(builtins::js_string_set_length),
            "js_string_slice" => BuiltinCFunction::Generic(builtins::js_string_slice),
            "js_string_substring" => BuiltinCFunction::Generic(builtins::js_string_substring),
            "js_string_concat" => BuiltinCFunction::Generic(builtins::js_string_concat),
            "js_string_split" => BuiltinCFunction::Generic(builtins::js_string_split),
            // Array
            "js_array_get_length" => BuiltinCFunction::Generic(builtins::js_array_get_length),
            "js_array_set_length" => BuiltinCFunction::Generic(builtins::js_array_set_length),
            "js_array_pop" => BuiltinCFunction::Generic(builtins::js_array_pop),
            "js_array_shift" => BuiltinCFunction::Generic(builtins::js_array_shift),
            "js_array_join" => BuiltinCFunction::Generic(builtins::js_array_join),
            "js_array_toString" => BuiltinCFunction::Generic(builtins::js_array_toString),
            "js_array_isArray" => BuiltinCFunction::Generic(builtins::js_array_isArray),
            "js_array_reverse" => BuiltinCFunction::Generic(builtins::js_array_reverse),
            "js_array_concat" => BuiltinCFunction::Generic(builtins::js_array_concat),
            "js_array_slice" => BuiltinCFunction::Generic(builtins::js_array_slice),
            "js_array_splice" => BuiltinCFunction::Generic(builtins::js_array_splice),
            "js_array_sort" => BuiltinCFunction::Generic(builtins::js_array_sort),
            // Error
            "js_error_toString" => BuiltinCFunction::Generic(builtins::js_error_toString),
            // JSON
            "js_json_parse" => BuiltinCFunction::Generic(builtins::js_json_parse),
            "js_json_stringify" => BuiltinCFunction::Generic(builtins::js_json_stringify),
            // ArrayBuffer / TypedArray
            "js_array_buffer_constructor" => {
                BuiltinCFunction::Generic(builtins::js_array_buffer_constructor)
            }
            "js_array_buffer_get_byteLength" => {
                BuiltinCFunction::Generic(builtins::js_array_buffer_get_byteLength)
            }
            "js_typed_array_base_constructor" => {
                BuiltinCFunction::Generic(builtins::js_typed_array_base_constructor)
            }
            "js_typed_array_subarray" => {
                BuiltinCFunction::Generic(builtins::js_typed_array_subarray)
            }
            _ => BuiltinCFunction::Missing(proto),
        },
        BuiltinProto::GenericMagic => match func_name {
            "js_math_min_max" => BuiltinCFunction::GenericMagic(builtins::js_math_min_max),
            // Function
            "js_function_get_length_name" => {
                BuiltinCFunction::GenericMagic(builtins::js_function_get_length_name)
            }
            // String
            "js_string_charAt" => BuiltinCFunction::GenericMagic(builtins::js_string_charAt),
            "js_string_indexOf" => BuiltinCFunction::GenericMagic(builtins::js_string_indexOf),
            "js_string_toLowerCase" => {
                BuiltinCFunction::GenericMagic(builtins::js_string_toLowerCase)
            }
            "js_string_trim" => BuiltinCFunction::GenericMagic(builtins::js_string_trim),
            "js_string_fromCharCode" => {
                BuiltinCFunction::GenericMagic(builtins::js_string_fromCharCode)
            }
            "js_string_replace" => BuiltinCFunction::GenericMagic(builtins::js_string_replace),
            // Array
            "js_array_push" => BuiltinCFunction::GenericMagic(builtins::js_array_push),
            "js_array_indexOf" => BuiltinCFunction::GenericMagic(builtins::js_array_indexOf),
            "js_array_every" => BuiltinCFunction::GenericMagic(builtins::js_array_every),
            "js_array_reduce" => BuiltinCFunction::GenericMagic(builtins::js_array_reduce),
            // TypedArray
            "js_typed_array_get_length" => {
                BuiltinCFunction::GenericMagic(builtins::js_typed_array_get_length)
            }
            // Error
            "js_error_get_message" => {
                BuiltinCFunction::GenericMagic(builtins::js_error_get_message)
            }
            _ => BuiltinCFunction::Missing(proto),
        },
        BuiltinProto::Constructor => match func_name {
            "js_number_constructor" => BuiltinCFunction::Constructor(builtins::js_number_constructor),
            "js_object_constructor" => BuiltinCFunction::Constructor(builtins::js_object_constructor),
            "js_function_constructor" => {
                BuiltinCFunction::Constructor(builtins::js_function_constructor)
            }
            "js_string_constructor" => {
                BuiltinCFunction::Constructor(builtins::js_string_constructor)
            }
            "js_array_constructor" => BuiltinCFunction::Constructor(builtins::js_array_constructor),
            "js_array_buffer_constructor" => {
                BuiltinCFunction::Constructor(builtins::js_array_buffer_constructor)
            }
            "js_typed_array_base_constructor" => {
                BuiltinCFunction::Constructor(builtins::js_typed_array_base_constructor)
            }
            _ => BuiltinCFunction::Missing(proto),
        },
        BuiltinProto::ConstructorMagic => match func_name {
            "js_error_constructor" => {
                BuiltinCFunction::ConstructorMagic(builtins::js_error_constructor)
            }
            "js_typed_array_constructor" => {
                BuiltinCFunction::ConstructorMagic(builtins::js_typed_array_constructor)
            }
            _ => BuiltinCFunction::Missing(proto),
        },
        BuiltinProto::GenericParams => match func_name {
            "js_function_bound" => BuiltinCFunction::GenericParams(builtins::js_function_bound),
            _ => BuiltinCFunction::Missing(proto),
        },
        BuiltinProto::FF => match func_name {
            "js_math_sign" => BuiltinCFunction::FF(builtins::js_math_sign),
            "js_math_fround" => BuiltinCFunction::FF(builtins::js_math_fround),
            "js_fabs" => BuiltinCFunction::FF(js_libm::js_fabs),
            "js_floor" => BuiltinCFunction::FF(js_libm::js_floor),
            "js_ceil" => BuiltinCFunction::FF(js_libm::js_ceil),
            "js_round_inf" => BuiltinCFunction::FF(js_libm::js_round_inf),
            "js_sqrt" => BuiltinCFunction::FF(js_libm::js_sqrt),
            "js_sin" => BuiltinCFunction::FF(js_libm::js_sin),
            "js_cos" => BuiltinCFunction::FF(js_libm::js_cos),
            "js_tan" => BuiltinCFunction::FF(js_libm::js_tan),
            "js_asin" => BuiltinCFunction::FF(js_libm::js_asin),
            "js_acos" => BuiltinCFunction::FF(js_libm::js_acos),
            "js_atan" => BuiltinCFunction::FF(js_libm::js_atan),
            "js_exp" => BuiltinCFunction::FF(js_libm::js_exp),
            "js_log" => BuiltinCFunction::FF(js_libm::js_log),
            "js_log2" => BuiltinCFunction::FF(js_libm::js_log2),
            "js_log10" => BuiltinCFunction::FF(js_libm::js_log10),
            "js_trunc" => BuiltinCFunction::FF(js_libm::js_trunc),
            _ => BuiltinCFunction::Missing(proto),
        },
    }
}

fn magic_from_name(name: &str) -> Option<i16> {
    if let Ok(value) = name.parse::<i16>() {
        return Some(value);
    }
    match name {
        "magic_internalAt" => Some(0),
        "magic_charAt" => Some(1),
        "magic_charCodeAt" => Some(2),
        "magic_codePointAt" => Some(3),
        "js_special_every" => Some(0),
        "js_special_some" => Some(1),
        "js_special_forEach" => Some(2),
        "js_special_map" => Some(3),
        "js_special_filter" => Some(4),
        "js_special_reduce" => Some(0),
        "js_special_reduceRight" => Some(1),
        _ => class_id_from_name(name).map(|class_id| class_id as i16),
    }
}

fn class_id_from_name(name: &str) -> Option<JSObjectClass> {
    match name {
        "JS_CLASS_OBJECT" => Some(JSObjectClass::Object),
        "JS_CLASS_ARRAY" => Some(JSObjectClass::Array),
        "JS_CLASS_C_FUNCTION" => Some(JSObjectClass::CFunction),
        "JS_CLASS_CLOSURE" => Some(JSObjectClass::Closure),
        "JS_CLASS_NUMBER" => Some(JSObjectClass::Number),
        "JS_CLASS_BOOLEAN" => Some(JSObjectClass::Boolean),
        "JS_CLASS_STRING" => Some(JSObjectClass::String),
        "JS_CLASS_DATE" => Some(JSObjectClass::Date),
        "JS_CLASS_REGEXP" => Some(JSObjectClass::RegExp),
        "JS_CLASS_ERROR" => Some(JSObjectClass::Error),
        "JS_CLASS_EVAL_ERROR" => Some(JSObjectClass::EvalError),
        "JS_CLASS_RANGE_ERROR" => Some(JSObjectClass::RangeError),
        "JS_CLASS_REFERENCE_ERROR" => Some(JSObjectClass::ReferenceError),
        "JS_CLASS_SYNTAX_ERROR" => Some(JSObjectClass::SyntaxError),
        "JS_CLASS_TYPE_ERROR" => Some(JSObjectClass::TypeError),
        "JS_CLASS_URI_ERROR" => Some(JSObjectClass::UriError),
        "JS_CLASS_INTERNAL_ERROR" => Some(JSObjectClass::InternalError),
        "JS_CLASS_ARRAY_BUFFER" => Some(JSObjectClass::ArrayBuffer),
        "JS_CLASS_TYPED_ARRAY" => Some(JSObjectClass::TypedArray),
        "JS_CLASS_UINT8C_ARRAY" => Some(JSObjectClass::Uint8CArray),
        "JS_CLASS_INT8_ARRAY" => Some(JSObjectClass::Int8Array),
        "JS_CLASS_UINT8_ARRAY" => Some(JSObjectClass::Uint8Array),
        "JS_CLASS_INT16_ARRAY" => Some(JSObjectClass::Int16Array),
        "JS_CLASS_UINT16_ARRAY" => Some(JSObjectClass::Uint16Array),
        "JS_CLASS_INT32_ARRAY" => Some(JSObjectClass::Int32Array),
        "JS_CLASS_UINT32_ARRAY" => Some(JSObjectClass::Uint32Array),
        "JS_CLASS_FLOAT32_ARRAY" => Some(JSObjectClass::Float32Array),
        "JS_CLASS_FLOAT64_ARRAY" => Some(JSObjectClass::Float64Array),
        "JS_CLASS_USER" => Some(JSObjectClass::User),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn magic_names_match_c_values() {
        assert_eq!(magic_from_name("magic_internalAt"), Some(0));
        assert_eq!(magic_from_name("magic_charAt"), Some(1));
        assert_eq!(magic_from_name("magic_charCodeAt"), Some(2));
        assert_eq!(magic_from_name("magic_codePointAt"), Some(3));
        assert_eq!(magic_from_name("js_special_every"), Some(0));
        assert_eq!(magic_from_name("js_special_reduceRight"), Some(1));
    }

    #[test]
    fn magic_parses_numeric_strings() {
        assert_eq!(magic_from_name("0"), Some(0));
        assert_eq!(magic_from_name("-1"), Some(-1));
        assert_eq!(magic_from_name("3"), Some(3));
    }

    #[test]
    fn magic_maps_class_ids() {
        assert_eq!(magic_from_name("JS_CLASS_OBJECT"), Some(0));
        assert_eq!(magic_from_name("JS_CLASS_FLOAT64_ARRAY"), Some(27));
        assert_eq!(magic_from_name("JS_CLASS_USER"), Some(28));
    }
}
