use mquickjs_build::{BuildInput, ClassDef, PropDef};
use std::env;
use std::f64::consts;
use std::path::Path;

static JS_OBJECT_PROTO: &[PropDef] = &[
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

static JS_OBJECT: &[PropDef] = &[
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
    class_props: Some(JS_OBJECT),
    proto_props: Some(JS_OBJECT_PROTO),
    parent_class: None,
    finalizer_name: None,
};

static JS_FUNCTION_PROTO: &[PropDef] = &[
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
    proto_props: Some(JS_FUNCTION_PROTO),
    parent_class: None,
    finalizer_name: None,
};

static JS_NUMBER_PROTO: &[PropDef] = &[
    PropDef::CFunc {
        name: "toExponential",
        length: 1,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_number_toExponential",
    },
    PropDef::CFunc {
        name: "toFixed",
        length: 1,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_number_toFixed",
    },
    PropDef::CFunc {
        name: "toPrecision",
        length: 1,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_number_toPrecision",
    },
    PropDef::CFunc {
        name: "toString",
        length: 1,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_number_toString",
    },
    PropDef::End,
];

static JS_NUMBER: &[PropDef] = &[
    PropDef::CFunc {
        name: "parseInt",
        length: 2,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_number_parseInt",
    },
    PropDef::CFunc {
        name: "parseFloat",
        length: 1,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_number_parseFloat",
    },
    PropDef::PropDouble {
        name: "MAX_VALUE",
        value: f64::MAX,
    },
    PropDef::PropDouble {
        name: "MIN_VALUE",
        value: 5e-324,
    },
    PropDef::PropDouble {
        name: "NaN",
        value: f64::NAN,
    },
    PropDef::PropDouble {
        name: "NEGATIVE_INFINITY",
        value: f64::NEG_INFINITY,
    },
    PropDef::PropDouble {
        name: "POSITIVE_INFINITY",
        value: f64::INFINITY,
    },
    PropDef::PropDouble {
        name: "EPSILON",
        value: f64::EPSILON,
    },
    PropDef::PropDouble {
        name: "MAX_SAFE_INTEGER",
        value: 9_007_199_254_740_991.0,
    },
    PropDef::PropDouble {
        name: "MIN_SAFE_INTEGER",
        value: -9_007_199_254_740_991.0,
    },
    PropDef::End,
];

static JS_NUMBER_CLASS: ClassDef = ClassDef {
    name: "Number",
    length: 1,
    cproto_name: Some("constructor"),
    func_name: Some("js_number_constructor"),
    class_id: Some("JS_CLASS_NUMBER"),
    class_props: Some(JS_NUMBER),
    proto_props: Some(JS_NUMBER_PROTO),
    parent_class: None,
    finalizer_name: None,
};

static JS_BOOLEAN_CLASS: ClassDef = ClassDef {
    name: "Boolean",
    length: 1,
    cproto_name: Some("constructor"),
    func_name: Some("js_boolean_constructor"),
    class_id: Some("JS_CLASS_BOOLEAN"),
    class_props: None,
    proto_props: None,
    parent_class: None,
    finalizer_name: None,
};

static JS_STRING_PROTO: &[PropDef] = &[
    PropDef::CGetSet {
        name: "length",
        magic: "0",
        cproto_name: "generic",
        get_func_name: Some("js_string_get_length"),
        set_func_name: Some("js_string_set_length"),
    },
    PropDef::CFunc {
        name: "charAt",
        length: 1,
        magic: "magic_charAt",
        cproto_name: "generic_magic",
        func_name: "js_string_charAt",
    },
    PropDef::CFunc {
        name: "charCodeAt",
        length: 1,
        magic: "magic_charCodeAt",
        cproto_name: "generic_magic",
        func_name: "js_string_charAt",
    },
    PropDef::CFunc {
        name: "codePointAt",
        length: 1,
        magic: "magic_codePointAt",
        cproto_name: "generic_magic",
        func_name: "js_string_charAt",
    },
    PropDef::CFunc {
        name: "slice",
        length: 2,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_string_slice",
    },
    PropDef::CFunc {
        name: "substring",
        length: 2,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_string_substring",
    },
    PropDef::CFunc {
        name: "concat",
        length: 1,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_string_concat",
    },
    PropDef::CFunc {
        name: "indexOf",
        length: 1,
        magic: "0",
        cproto_name: "generic_magic",
        func_name: "js_string_indexOf",
    },
    PropDef::CFunc {
        name: "lastIndexOf",
        length: 1,
        magic: "1",
        cproto_name: "generic_magic",
        func_name: "js_string_indexOf",
    },
    PropDef::CFunc {
        name: "match",
        length: 1,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_string_match",
    },
    PropDef::CFunc {
        name: "replace",
        length: 2,
        magic: "0",
        cproto_name: "generic_magic",
        func_name: "js_string_replace",
    },
    PropDef::CFunc {
        name: "replaceAll",
        length: 2,
        magic: "1",
        cproto_name: "generic_magic",
        func_name: "js_string_replace",
    },
    PropDef::CFunc {
        name: "search",
        length: 1,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_string_search",
    },
    PropDef::CFunc {
        name: "split",
        length: 2,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_string_split",
    },
    PropDef::CFunc {
        name: "toLowerCase",
        length: 0,
        magic: "1",
        cproto_name: "generic_magic",
        func_name: "js_string_toLowerCase",
    },
    PropDef::CFunc {
        name: "toUpperCase",
        length: 0,
        magic: "0",
        cproto_name: "generic_magic",
        func_name: "js_string_toLowerCase",
    },
    PropDef::CFunc {
        name: "trim",
        length: 0,
        magic: "3",
        cproto_name: "generic_magic",
        func_name: "js_string_trim",
    },
    PropDef::CFunc {
        name: "trimEnd",
        length: 0,
        magic: "2",
        cproto_name: "generic_magic",
        func_name: "js_string_trim",
    },
    PropDef::CFunc {
        name: "trimStart",
        length: 0,
        magic: "1",
        cproto_name: "generic_magic",
        func_name: "js_string_trim",
    },
    PropDef::End,
];

static JS_STRING: &[PropDef] = &[
    PropDef::CFunc {
        name: "fromCharCode",
        length: 1,
        magic: "0",
        cproto_name: "generic_magic",
        func_name: "js_string_fromCharCode",
    },
    PropDef::CFunc {
        name: "fromCodePoint",
        length: 1,
        magic: "1",
        cproto_name: "generic_magic",
        func_name: "js_string_fromCharCode",
    },
    PropDef::End,
];

static JS_STRING_CLASS: ClassDef = ClassDef {
    name: "String",
    length: 1,
    cproto_name: Some("constructor"),
    func_name: Some("js_string_constructor"),
    class_id: Some("JS_CLASS_STRING"),
    class_props: Some(JS_STRING),
    proto_props: Some(JS_STRING_PROTO),
    parent_class: None,
    finalizer_name: None,
};

static JS_ARRAY_PROTO: &[PropDef] = &[
    PropDef::CFunc {
        name: "concat",
        length: 1,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_array_concat",
    },
    PropDef::CGetSet {
        name: "length",
        magic: "0",
        cproto_name: "generic",
        get_func_name: Some("js_array_get_length"),
        set_func_name: Some("js_array_set_length"),
    },
    PropDef::CFunc {
        name: "push",
        length: 1,
        magic: "0",
        cproto_name: "generic_magic",
        func_name: "js_array_push",
    },
    PropDef::CFunc {
        name: "pop",
        length: 0,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_array_pop",
    },
    PropDef::CFunc {
        name: "join",
        length: 1,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_array_join",
    },
    PropDef::CFunc {
        name: "toString",
        length: 0,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_array_toString",
    },
    PropDef::CFunc {
        name: "reverse",
        length: 0,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_array_reverse",
    },
    PropDef::CFunc {
        name: "shift",
        length: 0,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_array_shift",
    },
    PropDef::CFunc {
        name: "slice",
        length: 2,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_array_slice",
    },
    PropDef::CFunc {
        name: "splice",
        length: 2,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_array_splice",
    },
    PropDef::CFunc {
        name: "unshift",
        length: 1,
        magic: "1",
        cproto_name: "generic_magic",
        func_name: "js_array_push",
    },
    PropDef::CFunc {
        name: "indexOf",
        length: 1,
        magic: "0",
        cproto_name: "generic_magic",
        func_name: "js_array_indexOf",
    },
    PropDef::CFunc {
        name: "lastIndexOf",
        length: 1,
        magic: "1",
        cproto_name: "generic_magic",
        func_name: "js_array_indexOf",
    },
    PropDef::CFunc {
        name: "every",
        length: 1,
        magic: "js_special_every",
        cproto_name: "generic_magic",
        func_name: "js_array_every",
    },
    PropDef::CFunc {
        name: "some",
        length: 1,
        magic: "js_special_some",
        cproto_name: "generic_magic",
        func_name: "js_array_every",
    },
    PropDef::CFunc {
        name: "forEach",
        length: 1,
        magic: "js_special_forEach",
        cproto_name: "generic_magic",
        func_name: "js_array_every",
    },
    PropDef::CFunc {
        name: "map",
        length: 1,
        magic: "js_special_map",
        cproto_name: "generic_magic",
        func_name: "js_array_every",
    },
    PropDef::CFunc {
        name: "filter",
        length: 1,
        magic: "js_special_filter",
        cproto_name: "generic_magic",
        func_name: "js_array_every",
    },
    PropDef::CFunc {
        name: "reduce",
        length: 1,
        magic: "js_special_reduce",
        cproto_name: "generic_magic",
        func_name: "js_array_reduce",
    },
    PropDef::CFunc {
        name: "reduceRight",
        length: 1,
        magic: "js_special_reduceRight",
        cproto_name: "generic_magic",
        func_name: "js_array_reduce",
    },
    PropDef::CFunc {
        name: "reduce",
        length: 1,
        magic: "js_special_reduce",
        cproto_name: "generic_magic",
        func_name: "js_array_reduce",
    },
    PropDef::CFunc {
        name: "sort",
        length: 1,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_array_sort",
    },
    PropDef::End,
];

static JS_ARRAY: &[PropDef] = &[
    PropDef::CFunc {
        name: "isArray",
        length: 1,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_array_isArray",
    },
    PropDef::End,
];

static JS_ARRAY_CLASS: ClassDef = ClassDef {
    name: "Array",
    length: 1,
    cproto_name: Some("constructor"),
    func_name: Some("js_array_constructor"),
    class_id: Some("JS_CLASS_ARRAY"),
    class_props: Some(JS_ARRAY),
    proto_props: Some(JS_ARRAY_PROTO),
    parent_class: None,
    finalizer_name: None,
};

static JS_ERROR_PROTO: &[PropDef] = &[
    PropDef::CFunc {
        name: "toString",
        length: 0,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_error_toString",
    },
    PropDef::PropString {
        name: "name",
        value: "Error",
    },
    PropDef::CGetSet {
        name: "message",
        magic: "0",
        cproto_name: "generic_magic",
        get_func_name: Some("js_error_get_message"),
        set_func_name: None,
    },
    PropDef::CGetSet {
        name: "stack",
        magic: "1",
        cproto_name: "generic_magic",
        get_func_name: Some("js_error_get_message"),
        set_func_name: None,
    },
    PropDef::End,
];

static JS_ERROR_CLASS: ClassDef = ClassDef {
    name: "Error",
    length: 1,
    cproto_name: Some("constructor_magic"),
    func_name: Some("js_error_constructor"),
    class_id: Some("JS_CLASS_ERROR"),
    class_props: None,
    proto_props: Some(JS_ERROR_PROTO),
    parent_class: None,
    finalizer_name: None,
};

static JS_EVAL_ERROR_PROTO: &[PropDef] = &[
    PropDef::PropString {
        name: "name",
        value: "EvalError",
    },
    PropDef::End,
];

static JS_EVAL_ERROR_CLASS: ClassDef = ClassDef {
    name: "EvalError",
    length: 1,
    cproto_name: Some("constructor_magic"),
    func_name: Some("js_error_constructor"),
    class_id: Some("JS_CLASS_EVAL_ERROR"),
    class_props: None,
    proto_props: Some(JS_EVAL_ERROR_PROTO),
    parent_class: Some(&JS_ERROR_CLASS),
    finalizer_name: None,
};

static JS_RANGE_ERROR_PROTO: &[PropDef] = &[
    PropDef::PropString {
        name: "name",
        value: "RangeError",
    },
    PropDef::End,
];

static JS_RANGE_ERROR_CLASS: ClassDef = ClassDef {
    name: "RangeError",
    length: 1,
    cproto_name: Some("constructor_magic"),
    func_name: Some("js_error_constructor"),
    class_id: Some("JS_CLASS_RANGE_ERROR"),
    class_props: None,
    proto_props: Some(JS_RANGE_ERROR_PROTO),
    parent_class: Some(&JS_ERROR_CLASS),
    finalizer_name: None,
};

static JS_REFERENCE_ERROR_PROTO: &[PropDef] = &[
    PropDef::PropString {
        name: "name",
        value: "ReferenceError",
    },
    PropDef::End,
];

static JS_REFERENCE_ERROR_CLASS: ClassDef = ClassDef {
    name: "ReferenceError",
    length: 1,
    cproto_name: Some("constructor_magic"),
    func_name: Some("js_error_constructor"),
    class_id: Some("JS_CLASS_REFERENCE_ERROR"),
    class_props: None,
    proto_props: Some(JS_REFERENCE_ERROR_PROTO),
    parent_class: Some(&JS_ERROR_CLASS),
    finalizer_name: None,
};

static JS_SYNTAX_ERROR_PROTO: &[PropDef] = &[
    PropDef::PropString {
        name: "name",
        value: "SyntaxError",
    },
    PropDef::End,
];

static JS_SYNTAX_ERROR_CLASS: ClassDef = ClassDef {
    name: "SyntaxError",
    length: 1,
    cproto_name: Some("constructor_magic"),
    func_name: Some("js_error_constructor"),
    class_id: Some("JS_CLASS_SYNTAX_ERROR"),
    class_props: None,
    proto_props: Some(JS_SYNTAX_ERROR_PROTO),
    parent_class: Some(&JS_ERROR_CLASS),
    finalizer_name: None,
};

static JS_TYPE_ERROR_PROTO: &[PropDef] = &[
    PropDef::PropString {
        name: "name",
        value: "TypeError",
    },
    PropDef::End,
];

static JS_TYPE_ERROR_CLASS: ClassDef = ClassDef {
    name: "TypeError",
    length: 1,
    cproto_name: Some("constructor_magic"),
    func_name: Some("js_error_constructor"),
    class_id: Some("JS_CLASS_TYPE_ERROR"),
    class_props: None,
    proto_props: Some(JS_TYPE_ERROR_PROTO),
    parent_class: Some(&JS_ERROR_CLASS),
    finalizer_name: None,
};

static JS_URI_ERROR_PROTO: &[PropDef] = &[
    PropDef::PropString {
        name: "name",
        value: "URIError",
    },
    PropDef::End,
];

static JS_URI_ERROR_CLASS: ClassDef = ClassDef {
    name: "URIError",
    length: 1,
    cproto_name: Some("constructor_magic"),
    func_name: Some("js_error_constructor"),
    class_id: Some("JS_CLASS_URI_ERROR"),
    class_props: None,
    proto_props: Some(JS_URI_ERROR_PROTO),
    parent_class: Some(&JS_ERROR_CLASS),
    finalizer_name: None,
};

static JS_INTERNAL_ERROR_PROTO: &[PropDef] = &[
    PropDef::PropString {
        name: "name",
        value: "InternalError",
    },
    PropDef::End,
];

static JS_INTERNAL_ERROR_CLASS: ClassDef = ClassDef {
    name: "InternalError",
    length: 1,
    cproto_name: Some("constructor_magic"),
    func_name: Some("js_error_constructor"),
    class_id: Some("JS_CLASS_INTERNAL_ERROR"),
    class_props: None,
    proto_props: Some(JS_INTERNAL_ERROR_PROTO),
    parent_class: Some(&JS_ERROR_CLASS),
    finalizer_name: None,
};

static JS_MATH: &[PropDef] = &[
    PropDef::CFunc {
        name: "min",
        length: 2,
        magic: "0",
        cproto_name: "generic_magic",
        func_name: "js_math_min_max",
    },
    PropDef::CFunc {
        name: "max",
        length: 2,
        magic: "1",
        cproto_name: "generic_magic",
        func_name: "js_math_min_max",
    },
    PropDef::CFunc {
        name: "sign",
        length: 1,
        magic: "0",
        cproto_name: "f_f",
        func_name: "js_math_sign",
    },
    PropDef::CFunc {
        name: "abs",
        length: 1,
        magic: "0",
        cproto_name: "f_f",
        func_name: "js_fabs",
    },
    PropDef::CFunc {
        name: "floor",
        length: 1,
        magic: "0",
        cproto_name: "f_f",
        func_name: "js_floor",
    },
    PropDef::CFunc {
        name: "ceil",
        length: 1,
        magic: "0",
        cproto_name: "f_f",
        func_name: "js_ceil",
    },
    PropDef::CFunc {
        name: "round",
        length: 1,
        magic: "0",
        cproto_name: "f_f",
        func_name: "js_round_inf",
    },
    PropDef::CFunc {
        name: "sqrt",
        length: 1,
        magic: "0",
        cproto_name: "f_f",
        func_name: "js_sqrt",
    },
    PropDef::PropDouble {
        name: "E",
        value: consts::E,
    },
    PropDef::PropDouble {
        name: "LN10",
        value: consts::LN_10,
    },
    PropDef::PropDouble {
        name: "LN2",
        value: consts::LN_2,
    },
    PropDef::PropDouble {
        name: "LOG2E",
        value: consts::LOG2_E,
    },
    PropDef::PropDouble {
        name: "LOG10E",
        value: consts::LOG10_E,
    },
    PropDef::PropDouble {
        name: "PI",
        value: consts::PI,
    },
    PropDef::PropDouble {
        name: "SQRT1_2",
        value: consts::FRAC_1_SQRT_2,
    },
    PropDef::PropDouble {
        name: "SQRT2",
        value: consts::SQRT_2,
    },
    PropDef::CFunc {
        name: "sin",
        length: 1,
        magic: "0",
        cproto_name: "f_f",
        func_name: "js_sin",
    },
    PropDef::CFunc {
        name: "cos",
        length: 1,
        magic: "0",
        cproto_name: "f_f",
        func_name: "js_cos",
    },
    PropDef::CFunc {
        name: "tan",
        length: 1,
        magic: "0",
        cproto_name: "f_f",
        func_name: "js_tan",
    },
    PropDef::CFunc {
        name: "asin",
        length: 1,
        magic: "0",
        cproto_name: "f_f",
        func_name: "js_asin",
    },
    PropDef::CFunc {
        name: "acos",
        length: 1,
        magic: "0",
        cproto_name: "f_f",
        func_name: "js_acos",
    },
    PropDef::CFunc {
        name: "atan",
        length: 1,
        magic: "0",
        cproto_name: "f_f",
        func_name: "js_atan",
    },
    PropDef::CFunc {
        name: "atan2",
        length: 2,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_math_atan2",
    },
    PropDef::CFunc {
        name: "exp",
        length: 1,
        magic: "0",
        cproto_name: "f_f",
        func_name: "js_exp",
    },
    PropDef::CFunc {
        name: "log",
        length: 1,
        magic: "0",
        cproto_name: "f_f",
        func_name: "js_log",
    },
    PropDef::CFunc {
        name: "pow",
        length: 2,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_math_pow",
    },
    PropDef::CFunc {
        name: "random",
        length: 0,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_math_random",
    },
    PropDef::CFunc {
        name: "imul",
        length: 2,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_math_imul",
    },
    PropDef::CFunc {
        name: "clz32",
        length: 1,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_math_clz32",
    },
    PropDef::CFunc {
        name: "fround",
        length: 1,
        magic: "0",
        cproto_name: "f_f",
        func_name: "js_math_fround",
    },
    PropDef::CFunc {
        name: "trunc",
        length: 1,
        magic: "0",
        cproto_name: "f_f",
        func_name: "js_trunc",
    },
    PropDef::CFunc {
        name: "log2",
        length: 1,
        magic: "0",
        cproto_name: "f_f",
        func_name: "js_log2",
    },
    PropDef::CFunc {
        name: "log10",
        length: 1,
        magic: "0",
        cproto_name: "f_f",
        func_name: "js_log10",
    },
    PropDef::End,
];

static JS_MATH_OBJ: ClassDef = ClassDef {
    name: "Math",
    length: 0,
    cproto_name: None,
    func_name: None,
    class_id: None,
    class_props: Some(JS_MATH),
    proto_props: None,
    parent_class: None,
    finalizer_name: None,
};

static JS_JSON: &[PropDef] = &[
    PropDef::CFunc {
        name: "parse",
        length: 2,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_json_parse",
    },
    PropDef::CFunc {
        name: "stringify",
        length: 3,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_json_stringify",
    },
    PropDef::End,
];

static JS_JSON_OBJ: ClassDef = ClassDef {
    name: "JSON",
    length: 0,
    cproto_name: None,
    func_name: None,
    class_id: None,
    class_props: Some(JS_JSON),
    proto_props: None,
    parent_class: None,
    finalizer_name: None,
};

static JS_ARRAY_BUFFER_PROTO: &[PropDef] = &[
    PropDef::CGetSet {
        name: "byteLength",
        magic: "0",
        cproto_name: "generic",
        get_func_name: Some("js_array_buffer_get_byteLength"),
        set_func_name: None,
    },
    PropDef::End,
];

static JS_ARRAY_BUFFER_CLASS: ClassDef = ClassDef {
    name: "ArrayBuffer",
    length: 1,
    cproto_name: Some("constructor"),
    func_name: Some("js_array_buffer_constructor"),
    class_id: Some("JS_CLASS_ARRAY_BUFFER"),
    class_props: None,
    proto_props: Some(JS_ARRAY_BUFFER_PROTO),
    parent_class: None,
    finalizer_name: None,
};

static JS_TYPED_ARRAY_BASE_PROTO: &[PropDef] = &[
    PropDef::CGetSet {
        name: "length",
        magic: "0",
        cproto_name: "generic_magic",
        get_func_name: Some("js_typed_array_get_length"),
        set_func_name: None,
    },
    PropDef::CGetSet {
        name: "byteLength",
        magic: "1",
        cproto_name: "generic_magic",
        get_func_name: Some("js_typed_array_get_length"),
        set_func_name: None,
    },
    PropDef::CGetSet {
        name: "byteOffset",
        magic: "2",
        cproto_name: "generic_magic",
        get_func_name: Some("js_typed_array_get_length"),
        set_func_name: None,
    },
    PropDef::CGetSet {
        name: "buffer",
        magic: "3",
        cproto_name: "generic_magic",
        get_func_name: Some("js_typed_array_get_length"),
        set_func_name: None,
    },
    PropDef::CFunc {
        name: "join",
        length: 1,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_array_join",
    },
    PropDef::CFunc {
        name: "toString",
        length: 0,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_array_toString",
    },
    PropDef::CFunc {
        name: "subarray",
        length: 2,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_typed_array_subarray",
    },
    PropDef::End,
];

static JS_TYPED_ARRAY_BASE_CLASS: ClassDef = ClassDef {
    name: "TypedArray",
    length: 0,
    cproto_name: Some("constructor"),
    func_name: Some("js_typed_array_base_constructor"),
    class_id: Some("JS_CLASS_TYPED_ARRAY"),
    class_props: None,
    proto_props: Some(JS_TYPED_ARRAY_BASE_PROTO),
    parent_class: None,
    finalizer_name: None,
};

static JS_UINT8_CLAMPED_ARRAY: &[PropDef] = &[
    PropDef::PropDouble {
        name: "BYTES_PER_ELEMENT",
        value: 1.0,
    },
    PropDef::End,
];

static JS_UINT8_CLAMPED_ARRAY_PROTO: &[PropDef] = &[
    PropDef::PropDouble {
        name: "BYTES_PER_ELEMENT",
        value: 1.0,
    },
    PropDef::End,
];

static JS_UINT8_CLAMPED_ARRAY_CLASS: ClassDef = ClassDef {
    name: "Uint8ClampedArray",
    length: 3,
    cproto_name: Some("constructor_magic"),
    func_name: Some("js_typed_array_constructor"),
    class_id: Some("JS_CLASS_UINT8C_ARRAY"),
    class_props: Some(JS_UINT8_CLAMPED_ARRAY),
    proto_props: Some(JS_UINT8_CLAMPED_ARRAY_PROTO),
    parent_class: Some(&JS_TYPED_ARRAY_BASE_CLASS),
    finalizer_name: None,
};

static JS_INT8_ARRAY: &[PropDef] = &[
    PropDef::PropDouble {
        name: "BYTES_PER_ELEMENT",
        value: 1.0,
    },
    PropDef::End,
];

static JS_INT8_ARRAY_PROTO: &[PropDef] = &[
    PropDef::PropDouble {
        name: "BYTES_PER_ELEMENT",
        value: 1.0,
    },
    PropDef::End,
];

static JS_INT8_ARRAY_CLASS: ClassDef = ClassDef {
    name: "Int8Array",
    length: 3,
    cproto_name: Some("constructor_magic"),
    func_name: Some("js_typed_array_constructor"),
    class_id: Some("JS_CLASS_INT8_ARRAY"),
    class_props: Some(JS_INT8_ARRAY),
    proto_props: Some(JS_INT8_ARRAY_PROTO),
    parent_class: Some(&JS_TYPED_ARRAY_BASE_CLASS),
    finalizer_name: None,
};

static JS_UINT8_ARRAY: &[PropDef] = &[
    PropDef::PropDouble {
        name: "BYTES_PER_ELEMENT",
        value: 1.0,
    },
    PropDef::End,
];

static JS_UINT8_ARRAY_PROTO: &[PropDef] = &[
    PropDef::PropDouble {
        name: "BYTES_PER_ELEMENT",
        value: 1.0,
    },
    PropDef::End,
];

static JS_UINT8_ARRAY_CLASS: ClassDef = ClassDef {
    name: "Uint8Array",
    length: 3,
    cproto_name: Some("constructor_magic"),
    func_name: Some("js_typed_array_constructor"),
    class_id: Some("JS_CLASS_UINT8_ARRAY"),
    class_props: Some(JS_UINT8_ARRAY),
    proto_props: Some(JS_UINT8_ARRAY_PROTO),
    parent_class: Some(&JS_TYPED_ARRAY_BASE_CLASS),
    finalizer_name: None,
};

static JS_INT16_ARRAY: &[PropDef] = &[
    PropDef::PropDouble {
        name: "BYTES_PER_ELEMENT",
        value: 2.0,
    },
    PropDef::End,
];

static JS_INT16_ARRAY_PROTO: &[PropDef] = &[
    PropDef::PropDouble {
        name: "BYTES_PER_ELEMENT",
        value: 2.0,
    },
    PropDef::End,
];

static JS_INT16_ARRAY_CLASS: ClassDef = ClassDef {
    name: "Int16Array",
    length: 3,
    cproto_name: Some("constructor_magic"),
    func_name: Some("js_typed_array_constructor"),
    class_id: Some("JS_CLASS_INT16_ARRAY"),
    class_props: Some(JS_INT16_ARRAY),
    proto_props: Some(JS_INT16_ARRAY_PROTO),
    parent_class: Some(&JS_TYPED_ARRAY_BASE_CLASS),
    finalizer_name: None,
};

static JS_UINT16_ARRAY: &[PropDef] = &[
    PropDef::PropDouble {
        name: "BYTES_PER_ELEMENT",
        value: 2.0,
    },
    PropDef::End,
];

static JS_UINT16_ARRAY_PROTO: &[PropDef] = &[
    PropDef::PropDouble {
        name: "BYTES_PER_ELEMENT",
        value: 2.0,
    },
    PropDef::End,
];

static JS_UINT16_ARRAY_CLASS: ClassDef = ClassDef {
    name: "Uint16Array",
    length: 3,
    cproto_name: Some("constructor_magic"),
    func_name: Some("js_typed_array_constructor"),
    class_id: Some("JS_CLASS_UINT16_ARRAY"),
    class_props: Some(JS_UINT16_ARRAY),
    proto_props: Some(JS_UINT16_ARRAY_PROTO),
    parent_class: Some(&JS_TYPED_ARRAY_BASE_CLASS),
    finalizer_name: None,
};

static JS_INT32_ARRAY: &[PropDef] = &[
    PropDef::PropDouble {
        name: "BYTES_PER_ELEMENT",
        value: 4.0,
    },
    PropDef::End,
];

static JS_INT32_ARRAY_PROTO: &[PropDef] = &[
    PropDef::PropDouble {
        name: "BYTES_PER_ELEMENT",
        value: 4.0,
    },
    PropDef::End,
];

static JS_INT32_ARRAY_CLASS: ClassDef = ClassDef {
    name: "Int32Array",
    length: 3,
    cproto_name: Some("constructor_magic"),
    func_name: Some("js_typed_array_constructor"),
    class_id: Some("JS_CLASS_INT32_ARRAY"),
    class_props: Some(JS_INT32_ARRAY),
    proto_props: Some(JS_INT32_ARRAY_PROTO),
    parent_class: Some(&JS_TYPED_ARRAY_BASE_CLASS),
    finalizer_name: None,
};

static JS_UINT32_ARRAY: &[PropDef] = &[
    PropDef::PropDouble {
        name: "BYTES_PER_ELEMENT",
        value: 4.0,
    },
    PropDef::End,
];

static JS_UINT32_ARRAY_PROTO: &[PropDef] = &[
    PropDef::PropDouble {
        name: "BYTES_PER_ELEMENT",
        value: 4.0,
    },
    PropDef::End,
];

static JS_UINT32_ARRAY_CLASS: ClassDef = ClassDef {
    name: "Uint32Array",
    length: 3,
    cproto_name: Some("constructor_magic"),
    func_name: Some("js_typed_array_constructor"),
    class_id: Some("JS_CLASS_UINT32_ARRAY"),
    class_props: Some(JS_UINT32_ARRAY),
    proto_props: Some(JS_UINT32_ARRAY_PROTO),
    parent_class: Some(&JS_TYPED_ARRAY_BASE_CLASS),
    finalizer_name: None,
};

static JS_FLOAT32_ARRAY: &[PropDef] = &[
    PropDef::PropDouble {
        name: "BYTES_PER_ELEMENT",
        value: 4.0,
    },
    PropDef::End,
];

static JS_FLOAT32_ARRAY_PROTO: &[PropDef] = &[
    PropDef::PropDouble {
        name: "BYTES_PER_ELEMENT",
        value: 4.0,
    },
    PropDef::End,
];

static JS_FLOAT32_ARRAY_CLASS: ClassDef = ClassDef {
    name: "Float32Array",
    length: 3,
    cproto_name: Some("constructor_magic"),
    func_name: Some("js_typed_array_constructor"),
    class_id: Some("JS_CLASS_FLOAT32_ARRAY"),
    class_props: Some(JS_FLOAT32_ARRAY),
    proto_props: Some(JS_FLOAT32_ARRAY_PROTO),
    parent_class: Some(&JS_TYPED_ARRAY_BASE_CLASS),
    finalizer_name: None,
};

static JS_FLOAT64_ARRAY: &[PropDef] = &[
    PropDef::PropDouble {
        name: "BYTES_PER_ELEMENT",
        value: 8.0,
    },
    PropDef::End,
];

static JS_FLOAT64_ARRAY_PROTO: &[PropDef] = &[
    PropDef::PropDouble {
        name: "BYTES_PER_ELEMENT",
        value: 8.0,
    },
    PropDef::End,
];

static JS_FLOAT64_ARRAY_CLASS: ClassDef = ClassDef {
    name: "Float64Array",
    length: 3,
    cproto_name: Some("constructor_magic"),
    func_name: Some("js_typed_array_constructor"),
    class_id: Some("JS_CLASS_FLOAT64_ARRAY"),
    class_props: Some(JS_FLOAT64_ARRAY),
    proto_props: Some(JS_FLOAT64_ARRAY_PROTO),
    parent_class: Some(&JS_TYPED_ARRAY_BASE_CLASS),
    finalizer_name: None,
};

static JS_REGEXP_PROTO: &[PropDef] = &[
    PropDef::CGetSet {
        name: "lastIndex",
        magic: "0",
        cproto_name: "generic",
        get_func_name: Some("js_regexp_get_lastIndex"),
        set_func_name: Some("js_regexp_set_lastIndex"),
    },
    PropDef::CGetSet {
        name: "source",
        magic: "0",
        cproto_name: "generic",
        get_func_name: Some("js_regexp_get_source"),
        set_func_name: None,
    },
    PropDef::CGetSet {
        name: "flags",
        magic: "0",
        cproto_name: "generic",
        get_func_name: Some("js_regexp_get_flags"),
        set_func_name: None,
    },
    PropDef::CFunc {
        name: "exec",
        length: 1,
        magic: "0",
        cproto_name: "generic_magic",
        func_name: "js_regexp_exec",
    },
    PropDef::CFunc {
        name: "test",
        length: 1,
        magic: "1",
        cproto_name: "generic_magic",
        func_name: "js_regexp_exec",
    },
    PropDef::End,
];

static JS_REGEXP_CLASS: ClassDef = ClassDef {
    name: "RegExp",
    length: 2,
    cproto_name: Some("constructor"),
    func_name: Some("js_regexp_constructor"),
    class_id: Some("JS_CLASS_REGEXP"),
    class_props: None,
    proto_props: Some(JS_REGEXP_PROTO),
    parent_class: None,
    finalizer_name: None,
};

static JS_DATE: &[PropDef] = &[
    PropDef::CFunc {
        name: "now",
        length: 0,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_date_now",
    },
    PropDef::End,
];

static JS_DATE_CLASS: ClassDef = ClassDef {
    name: "Date",
    length: 7,
    cproto_name: Some("constructor"),
    func_name: Some("js_date_constructor"),
    class_id: Some("JS_CLASS_DATE"),
    class_props: Some(JS_DATE),
    proto_props: None,
    parent_class: None,
    finalizer_name: None,
};

static JS_CONSOLE: &[PropDef] = &[
    PropDef::CFunc {
        name: "log",
        length: 1,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_print",
    },
    PropDef::End,
];

static JS_CONSOLE_OBJ: ClassDef = ClassDef {
    name: "Console",
    length: 0,
    cproto_name: None,
    func_name: None,
    class_id: None,
    class_props: Some(JS_CONSOLE),
    proto_props: None,
    parent_class: None,
    finalizer_name: None,
};

static JS_PERFORMANCE: &[PropDef] = &[
    PropDef::CFunc {
        name: "now",
        length: 0,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_performance_now",
    },
    PropDef::End,
];

static JS_PERFORMANCE_OBJ: ClassDef = ClassDef {
    name: "Performance",
    length: 0,
    cproto_name: None,
    func_name: None,
    class_id: None,
    class_props: Some(JS_PERFORMANCE),
    proto_props: None,
    parent_class: None,
    finalizer_name: None,
};

static JS_GLOBAL_OBJECT: &[PropDef] = &[
    PropDef::Class {
        name: "Object",
        class: &JS_OBJECT_CLASS,
    },
    PropDef::Class {
        name: "Function",
        class: &JS_FUNCTION_CLASS,
    },
    PropDef::Class {
        name: "Number",
        class: &JS_NUMBER_CLASS,
    },
    PropDef::Class {
        name: "Boolean",
        class: &JS_BOOLEAN_CLASS,
    },
    PropDef::Class {
        name: "String",
        class: &JS_STRING_CLASS,
    },
    PropDef::Class {
        name: "Array",
        class: &JS_ARRAY_CLASS,
    },
    PropDef::Class {
        name: "Math",
        class: &JS_MATH_OBJ,
    },
    PropDef::Class {
        name: "Date",
        class: &JS_DATE_CLASS,
    },
    PropDef::Class {
        name: "JSON",
        class: &JS_JSON_OBJ,
    },
    PropDef::Class {
        name: "RegExp",
        class: &JS_REGEXP_CLASS,
    },
    PropDef::Class {
        name: "Error",
        class: &JS_ERROR_CLASS,
    },
    PropDef::Class {
        name: "EvalError",
        class: &JS_EVAL_ERROR_CLASS,
    },
    PropDef::Class {
        name: "RangeError",
        class: &JS_RANGE_ERROR_CLASS,
    },
    PropDef::Class {
        name: "ReferenceError",
        class: &JS_REFERENCE_ERROR_CLASS,
    },
    PropDef::Class {
        name: "SyntaxError",
        class: &JS_SYNTAX_ERROR_CLASS,
    },
    PropDef::Class {
        name: "TypeError",
        class: &JS_TYPE_ERROR_CLASS,
    },
    PropDef::Class {
        name: "URIError",
        class: &JS_URI_ERROR_CLASS,
    },
    PropDef::Class {
        name: "InternalError",
        class: &JS_INTERNAL_ERROR_CLASS,
    },
    PropDef::Class {
        name: "ArrayBuffer",
        class: &JS_ARRAY_BUFFER_CLASS,
    },
    PropDef::Class {
        name: "Uint8ClampedArray",
        class: &JS_UINT8_CLAMPED_ARRAY_CLASS,
    },
    PropDef::Class {
        name: "Int8Array",
        class: &JS_INT8_ARRAY_CLASS,
    },
    PropDef::Class {
        name: "Uint8Array",
        class: &JS_UINT8_ARRAY_CLASS,
    },
    PropDef::Class {
        name: "Int16Array",
        class: &JS_INT16_ARRAY_CLASS,
    },
    PropDef::Class {
        name: "Uint16Array",
        class: &JS_UINT16_ARRAY_CLASS,
    },
    PropDef::Class {
        name: "Int32Array",
        class: &JS_INT32_ARRAY_CLASS,
    },
    PropDef::Class {
        name: "Uint32Array",
        class: &JS_UINT32_ARRAY_CLASS,
    },
    PropDef::Class {
        name: "Float32Array",
        class: &JS_FLOAT32_ARRAY_CLASS,
    },
    PropDef::Class {
        name: "Float64Array",
        class: &JS_FLOAT64_ARRAY_CLASS,
    },
    PropDef::CFunc {
        name: "parseInt",
        length: 2,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_number_parseInt",
    },
    PropDef::CFunc {
        name: "parseFloat",
        length: 1,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_number_parseFloat",
    },
    PropDef::CFunc {
        name: "eval",
        length: 1,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_global_eval",
    },
    PropDef::CFunc {
        name: "isNaN",
        length: 1,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_global_isNaN",
    },
    PropDef::CFunc {
        name: "isFinite",
        length: 1,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_global_isFinite",
    },
    PropDef::PropDouble {
        name: "Infinity",
        value: f64::INFINITY,
    },
    PropDef::PropDouble {
        name: "NaN",
        value: f64::NAN,
    },
    PropDef::PropUndefined { name: "undefined" },
    PropDef::PropNull { name: "globalThis" },
    PropDef::Class {
        name: "console",
        class: &JS_CONSOLE_OBJ,
    },
    PropDef::Class {
        name: "performance",
        class: &JS_PERFORMANCE_OBJ,
    },
    PropDef::CFunc {
        name: "print",
        length: 1,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_print",
    },
    PropDef::CFunc {
        name: "gc",
        length: 0,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_gc",
    },
    PropDef::CFunc {
        name: "load",
        length: 1,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_load",
    },
    PropDef::CFunc {
        name: "setTimeout",
        length: 2,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_setTimeout",
    },
    PropDef::CFunc {
        name: "clearTimeout",
        length: 1,
        magic: "0",
        cproto_name: "generic",
        func_name: "js_clearTimeout",
    },
    PropDef::End,
];

static JS_C_FUNCTION_DECL: &[PropDef] = &[
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
        global_object: JS_GLOBAL_OBJECT,
        c_function_decl: Some(JS_C_FUNCTION_DECL),
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
