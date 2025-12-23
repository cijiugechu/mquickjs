use std::fmt;
use std::fmt::Write;

pub const DEFAULT_PREDEFINED_ATOMS: &[&str] = &[
    "null",
    "false",
    "true",
    "if",
    "else",
    "return",
    "var",
    "this",
    "delete",
    "void",
    "typeof",
    "new",
    "in",
    "instanceof",
    "do",
    "while",
    "for",
    "break",
    "continue",
    "switch",
    "case",
    "default",
    "throw",
    "try",
    "catch",
    "finally",
    "function",
    "debugger",
    "with",
    "class",
    "const",
    "enum",
    "export",
    "extends",
    "import",
    "super",
    "implements",
    "interface",
    "let",
    "package",
    "private",
    "protected",
    "public",
    "static",
    "yield",
    "",
    "toString",
    "valueOf",
    "number",
    "object",
    "undefined",
    "string",
    "boolean",
    "<ret>",
    "<eval>",
    "eval",
    "arguments",
    "value",
    "get",
    "set",
    "prototype",
    "constructor",
    "length",
    "target",
    "of",
    "NaN",
    "Infinity",
    "-Infinity",
    "name",
    "Error",
    "__proto__",
    "index",
    "input",
];

const ATOM_ALIGN: u32 = 64;
const JS_MTAG_BITS: u32 = 4;
const JS_MTAG_OBJECT: u64 = 1;
const JS_MTAG_FLOAT64: u64 = 2;
const JS_MTAG_STRING: u64 = 3;
const JS_MTAG_VALUE_ARRAY: u64 = 5;

const JS_PROP_NORMAL: u64 = 0;
const JS_PROP_GETSET: u64 = 1;
const JS_PROP_SPECIAL: u64 = 3;

const JS_TAG_SPECIAL_BITS: u32 = 5;
const JS_TAG_SPECIAL: u64 = 3;
const JS_TAG_NULL: u64 = JS_TAG_SPECIAL | (1 << 2);
const JS_TAG_UNDEFINED: u64 = JS_TAG_SPECIAL | (2 << 2);
const JS_TAG_SHORT_FUNC: u64 = JS_TAG_SPECIAL | (4 << 2);
const JS_TAG_STRING_CHAR: u64 = JS_TAG_SPECIAL | (6 << 2);

const JS_SHORTINT_MIN: i64 = -(1 << 30);
const JS_SHORTINT_MAX: i64 = (1 << 30) - 1;

const EMPTY_PROPS: [PropDef<'static>; 1] = [PropDef::End];

const CLASS_ID_TABLE: &[&str] = &[
    "JS_CLASS_OBJECT",
    "JS_CLASS_ARRAY",
    "JS_CLASS_C_FUNCTION",
    "JS_CLASS_CLOSURE",
    "JS_CLASS_NUMBER",
    "JS_CLASS_BOOLEAN",
    "JS_CLASS_STRING",
    "JS_CLASS_DATE",
    "JS_CLASS_REGEXP",
    "JS_CLASS_ERROR",
    "JS_CLASS_EVAL_ERROR",
    "JS_CLASS_RANGE_ERROR",
    "JS_CLASS_REFERENCE_ERROR",
    "JS_CLASS_SYNTAX_ERROR",
    "JS_CLASS_TYPE_ERROR",
    "JS_CLASS_URI_ERROR",
    "JS_CLASS_INTERNAL_ERROR",
    "JS_CLASS_ARRAY_BUFFER",
    "JS_CLASS_TYPED_ARRAY",
    "JS_CLASS_UINT8C_ARRAY",
    "JS_CLASS_INT8_ARRAY",
    "JS_CLASS_UINT8_ARRAY",
    "JS_CLASS_INT16_ARRAY",
    "JS_CLASS_UINT16_ARRAY",
    "JS_CLASS_INT32_ARRAY",
    "JS_CLASS_UINT32_ARRAY",
    "JS_CLASS_FLOAT32_ARRAY",
    "JS_CLASS_FLOAT64_ARRAY",
    "JS_CLASS_USER",
];

pub fn default_class_count() -> u32 {
    CLASS_ID_TABLE.len() as u32 - 1
}

#[derive(Clone, Copy, Debug)]
pub struct ClassDef<'a> {
    pub name: &'a str,
    pub length: i32,
    pub cproto_name: Option<&'a str>,
    pub func_name: Option<&'a str>,
    pub class_id: Option<&'a str>,
    pub class_props: Option<&'a [PropDef<'a>]>,
    pub proto_props: Option<&'a [PropDef<'a>]>,
    pub parent_class: Option<&'a ClassDef<'a>>,
    pub finalizer_name: Option<&'a str>,
}

#[derive(Clone, Copy, Debug)]
#[allow(dead_code)]
pub enum PropDef<'a> {
    CFunc {
        name: &'a str,
        length: u8,
        magic: &'a str,
        cproto_name: &'a str,
        func_name: &'a str,
    },
    CGetSet {
        name: &'a str,
        magic: &'a str,
        cproto_name: &'a str,
        get_func_name: Option<&'a str>,
        set_func_name: Option<&'a str>,
    },
    PropDouble {
        name: &'a str,
        value: f64,
    },
    PropUndefined {
        name: &'a str,
    },
    PropNull {
        name: &'a str,
    },
    PropString {
        name: &'a str,
        value: &'a str,
    },
    Class {
        name: &'a str,
        class: &'a ClassDef<'a>,
    },
    End,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StdlibWord {
    Raw(u64),
    RomOffset(u32),
}

#[derive(Debug)]
pub struct GenCFuncMeta {
    pub name: String,
    pub cproto_name: String,
    pub func_name: String,
    pub arg_count: u8,
    pub magic: String,
}

#[derive(Debug)]
pub struct GenCFinalizerMeta {
    pub class_id: String,
    pub finalizer_name: String,
}

#[derive(Debug)]
pub struct GenStdlibImage {
    pub words: Vec<StdlibWord>,
    pub c_functions: Vec<GenCFuncMeta>,
    pub c_finalizers: Vec<GenCFinalizerMeta>,
    pub stdlib_table_len: u32,
    pub stdlib_table_align: u32,
    pub sorted_atoms_offset: u32,
    pub global_object_offset: u32,
    pub word_bytes: u8,
    pub class_count: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BuildError {
    InvalidWordBytes(u32),
    MissingPropEnd,
    NumericPropertyName(String),
    NonAsciiPropertyName(String),
    UndefinedAtom(String),
    GlobalGetSetForbidden(String),
    InvalidCFunctionDecl(String),
    MissingClassConstructor(String),
    MissingClassId(String),
    UnknownClassId(String),
}

impl fmt::Display for BuildError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BuildError::InvalidWordBytes(word_bytes) => {
                write!(f, "invalid word size: {}", word_bytes)
            }
            BuildError::MissingPropEnd => write!(f, "property list missing End sentinel"),
            BuildError::NumericPropertyName(name) => {
                write!(f, "numeric property names are not supported: {}", name)
            }
            BuildError::NonAsciiPropertyName(name) => {
                write!(f, "non-ASCII property names are not supported: {}", name)
            }
            BuildError::UndefinedAtom(name) => write!(f, "atom '{}' is undefined", name),
            BuildError::GlobalGetSetForbidden(name) => {
                write!(f, "getter/setter forbidden in global object: {}", name)
            }
            BuildError::InvalidCFunctionDecl(name) => {
                write!(f, "only C functions are allowed in c_function_decl: {}", name)
            }
            BuildError::MissingClassConstructor(name) => {
                write!(f, "class '{}' missing constructor metadata", name)
            }
            BuildError::MissingClassId(name) => {
                write!(f, "class '{}' missing class_id", name)
            }
            BuildError::UnknownClassId(name) => {
                write!(f, "unknown class_id '{}'", name)
            }
        }
    }
}

impl std::error::Error for BuildError {}

pub struct BuildInput<'a> {
    pub global_object: &'a [PropDef<'a>],
    pub c_function_decl: Option<&'a [PropDef<'a>]>,
    pub predefined_atoms: &'a [&'a str],
    pub word_bytes: u32,
    pub class_count: u32,
}

pub fn build_stdlib(input: &BuildInput<'_>) -> Result<GenStdlibImage, BuildError> {
    if input.word_bytes != 4 && input.word_bytes != 8 {
        return Err(BuildError::InvalidWordBytes(input.word_bytes));
    }

    let mut ctx = BuildContext::new(input.word_bytes, input.class_count);

    for atom in input.predefined_atoms {
        ctx.add_atom(atom);
    }

    if let Some(cfuncs) = input.c_function_decl {
        let cfuncs = prop_slice(cfuncs)?;
        for entry in cfuncs {
            if let PropDef::CFunc {
                name,
                length,
                magic,
                cproto_name,
                func_name,
            } = entry
            {
                ctx.add_atom(name);
                ctx.add_cfunc(name, *length, magic, cproto_name, func_name);
            } else {
                let name = entry.name().unwrap_or("<unknown>");
                return Err(BuildError::InvalidCFunctionDecl(name.to_string()));
            }
        }
    }

    ctx.define_atoms_props(input.global_object, PropsKind::Global)?;
    ctx.clear_class_entries();
    ctx.emit_atoms()?;

    let global_object_offset = ctx.define_props(
        Some(input.global_object),
        PropsKind::Global,
        None,
    )?;
    ctx.global_object_offset = global_object_offset;

    let stdlib_table_len = ctx.cur_offset();

    let c_finalizers = ctx.finalize_cfinalizers();
    let c_functions = ctx.finalize_cfuncs();

    Ok(GenStdlibImage {
        words: ctx.words,
        c_functions,
        c_finalizers,
        stdlib_table_len,
        stdlib_table_align: ATOM_ALIGN,
        sorted_atoms_offset: ctx.sorted_atoms_offset,
        global_object_offset: ctx.global_object_offset,
        word_bytes: ctx.word_bytes as u8,
        class_count: ctx.class_count,
    })
}

pub fn render_rust(image: &GenStdlibImage, const_name: &str) -> String {
    let mut out = String::new();
    let _ = writeln!(
        out,
        "use crate::stdlib_image::{{CFinalizerMeta, CFuncMeta, StdlibImage, StdlibWord}};"
    );
    out.push('\n');

    let _ = writeln!(
        out,
        "static STDLIB_WORDS: [StdlibWord; {}] = [",
        image.words.len()
    );
    for word in &image.words {
        match word {
            StdlibWord::Raw(value) => {
                let word_hex = format_word(*value, image.word_bytes);
                let _ = writeln!(out, "    StdlibWord::Raw({}),", word_hex);
            }
            StdlibWord::RomOffset(offset) => {
                let _ = writeln!(out, "    StdlibWord::RomOffset({}),", offset);
            }
        }
    }
    out.push_str("];\n\n");

    let _ = writeln!(
        out,
        "static STDLIB_CFUNCS: [CFuncMeta; {}] = [",
        image.c_functions.len()
    );
    for func in &image.c_functions {
        let _ = writeln!(out, "    CFuncMeta {{");
        let _ = writeln!(out, "        name: {:?},", func.name);
        let _ = writeln!(out, "        cproto_name: {:?},", func.cproto_name);
        let _ = writeln!(out, "        func_name: {:?},", func.func_name);
        let _ = writeln!(out, "        arg_count: {},", func.arg_count);
        let _ = writeln!(out, "        magic: {:?},", func.magic);
        let _ = writeln!(out, "    }},");
    }
    out.push_str("];\n\n");

    let _ = writeln!(
        out,
        "static STDLIB_FINALIZERS: [CFinalizerMeta; {}] = [",
        image.c_finalizers.len()
    );
    for entry in &image.c_finalizers {
        let _ = writeln!(out, "    CFinalizerMeta {{");
        let _ = writeln!(out, "        class_id: {:?},", entry.class_id);
        let _ = writeln!(out, "        finalizer_name: {:?},", entry.finalizer_name);
        let _ = writeln!(out, "    }},");
    }
    out.push_str("];\n\n");

    let _ = writeln!(out, "pub const {}: StdlibImage = StdlibImage {{", const_name);
    let _ = writeln!(out, "    words: &STDLIB_WORDS,");
    let _ = writeln!(out, "    c_functions: &STDLIB_CFUNCS,");
    let _ = writeln!(out, "    c_finalizers: &STDLIB_FINALIZERS,");
    let _ = writeln!(out, "    stdlib_table_len: {},", image.stdlib_table_len);
    let _ = writeln!(out, "    stdlib_table_align: {},", image.stdlib_table_align);
    let _ = writeln!(out, "    sorted_atoms_offset: {},", image.sorted_atoms_offset);
    let _ = writeln!(out, "    global_object_offset: {},", image.global_object_offset);
    let _ = writeln!(out, "    word_bytes: {},", image.word_bytes);
    let _ = writeln!(out, "    class_count: {},", image.class_count);
    out.push_str("};\n");

    out
}

#[derive(Clone)]
struct AtomDef {
    name: String,
    offset: u32,
}

struct CFuncDef {
    name: String,
    length: u8,
    magic: String,
    cproto_name: String,
    func_name: String,
}

struct ClassEntry<'a> {
    class: *const ClassDef<'a>,
    class_idx: u32,
    class_id: Option<String>,
    finalizer_name: Option<String>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum PropsKind {
    Global,
    Proto,
    Class,
    Object,
}

struct BuildContext<'a> {
    word_bytes: u32,
    class_count: u32,
    words: Vec<StdlibWord>,
    atom_list: Vec<AtomDef>,
    atom_next_offset: u32,
    cfunc_list: Vec<CFuncDef>,
    class_entries: Vec<ClassEntry<'a>>,
    sorted_atoms_offset: u32,
    global_object_offset: u32,
}

impl<'a> BuildContext<'a> {
    fn new(word_bytes: u32, class_count: u32) -> Self {
        Self {
            word_bytes,
            class_count,
            words: Vec::new(),
            atom_list: Vec::new(),
            atom_next_offset: 0,
            cfunc_list: Vec::new(),
            class_entries: Vec::new(),
            sorted_atoms_offset: 0,
            global_object_offset: 0,
        }
    }

    fn cur_offset(&self) -> u32 {
        self.words.len() as u32
    }

    fn push_raw(&mut self, value: u64) {
        self.words.push(StdlibWord::Raw(value));
    }

    fn push_rom(&mut self, offset: u32) {
        self.words.push(StdlibWord::RomOffset(offset));
    }

    fn add_atom(&mut self, name: &str) -> u32 {
        if let Some(offset) = self.find_atom_offset(name) {
            return offset;
        }
        let offset = self.atom_next_offset;
        self.atom_list.push(AtomDef {
            name: name.to_string(),
            offset,
        });
        let len = name.len() as u32;
        let len_words = (len + self.word_bytes) / self.word_bytes;
        self.atom_next_offset += 1 + len_words;
        offset
    }

    fn find_atom_offset(&self, name: &str) -> Option<u32> {
        self.atom_list
            .iter()
            .find(|atom| atom.name == name)
            .map(|atom| atom.offset)
    }

    fn add_cfunc(
        &mut self,
        name: &str,
        length: u8,
        magic: &str,
        cproto_name: &str,
        func_name: &str,
    ) -> usize {
        for (idx, entry) in self.cfunc_list.iter().enumerate() {
            if entry.name == name
                && entry.length == length
                && entry.magic == magic
                && entry.cproto_name == cproto_name
                && entry.func_name == func_name
            {
                return idx;
            }
        }
        let idx = self.cfunc_list.len();
        self.cfunc_list.push(CFuncDef {
            name: name.to_string(),
            length,
            magic: magic.to_string(),
            cproto_name: cproto_name.to_string(),
            func_name: func_name.to_string(),
        });
        idx
    }

    fn emit_atoms(&mut self) -> Result<(), BuildError> {
        let atoms = self.atom_list.clone();
        let mut sorted_atoms = atoms.clone();
        sorted_atoms.sort_by(|a, b| a.name.cmp(&b.name));

        for atom in &atoms {
            debug_assert_eq!(self.cur_offset(), atom.offset);
            let len = atom.name.len();
            let is_ascii = is_ascii_string(atom.name.as_bytes());
            let is_numeric = is_numeric_string(&atom.name);
            let header = string_header(len as u64, is_ascii, is_numeric);
            self.push_raw(header);

            let len_words = (len as u32 + self.word_bytes) / self.word_bytes;
            for j in 0..len_words {
                let start = (j * self.word_bytes) as usize;
                let end = usize::min(start + self.word_bytes as usize, len);
                let mut value = 0u64;
                for (k, byte) in atom.name.as_bytes()[start..end].iter().enumerate() {
                    value |= (*byte as u64) << (k * 8);
                }
                self.push_raw(value);
            }
        }

        self.sorted_atoms_offset = self.cur_offset();
        self.push_raw(js_value_array_header(sorted_atoms.len() as u64));
        for atom in &sorted_atoms {
            self.push_rom(atom.offset);
        }

        Ok(())
    }

    fn define_atoms_props(
        &mut self,
        props: &'a [PropDef<'a>],
        props_kind: PropsKind,
    ) -> Result<(), BuildError> {
        let props = prop_slice(props)?;
        for prop in props {
            if let Some(name) = prop.name() {
                self.add_atom(name);
            }
            match prop {
                PropDef::PropString { value, .. } => {
                    self.add_atom(value);
                }
                PropDef::Class { class, .. } => {
                    self.define_atoms_class(class)?;
                }
                PropDef::CGetSet {
                    name,
                    get_func_name,
                    set_func_name,
                    ..
                } => {
                    if get_func_name.is_some() {
                        self.add_atom(&format!("get {}", name));
                    }
                    if set_func_name.is_some() {
                        self.add_atom(&format!("set {}", name));
                    }
                }
                _ => {}
            }
        }

        if let PropsKind::Class | PropsKind::Proto = props_kind {
            if props_kind == PropsKind::Proto {
                self.add_atom("constructor");
            } else {
                self.add_atom("prototype");
            }
        }

        Ok(())
    }

    fn define_atoms_class(&mut self, class: &ClassDef<'a>) -> Result<(), BuildError> {
        if let Some(parent) = class.parent_class {
            self.define_atoms_class(parent)?;
        }
        if class.func_name.is_some() {
            self.add_atom(class.name);
        }
        if let Some(class_props) = class.class_props {
            let kind = if class.func_name.is_some() {
                PropsKind::Class
            } else {
                PropsKind::Object
            };
            self.define_atoms_props(class_props, kind)?;
        }
        if let Some(proto_props) = class.proto_props {
            self.define_atoms_props(proto_props, PropsKind::Proto)?;
        }
        Ok(())
    }

    fn define_props(
        &mut self,
        props: Option<&'a [PropDef<'a>]>,
        props_kind: PropsKind,
        class_id: Option<&str>,
    ) -> Result<u32, BuildError> {
        let props = props_or_empty(props);
        let base_len = prop_list_len(props)?;
        let mut n_props = base_len;
        if matches!(props_kind, PropsKind::Proto | PropsKind::Class) {
            n_props += 1;
        }
        let n_props_u32 = n_props as u32;

        let mut ident_tab: Vec<Option<u32>> = vec![None; n_props];
        for (idx, prop) in props[..base_len].iter().enumerate() {
            ident_tab[idx] = self.define_value(prop)?;
        }

        let props_ident = self.cur_offset();
        let is_global_object = matches!(props_kind, PropsKind::Global);
        let mut prop_hash: Vec<u32> = Vec::new();
        if is_global_object {
            self.push_raw(js_value_array_header((2 * n_props_u32) as u64));
        } else {
            let hash_size = hash_table_size(n_props, self.word_bytes);
            let hash_mask = hash_size - 1;
            let mut hash_table = vec![0u32; hash_size as usize];
            prop_hash = vec![0u32; n_props];
            for (prop_idx, i) in (0..n_props).enumerate() {
                let prop_idx = prop_idx as u32;
                let name = if i < base_len {
                    props[i].name().unwrap_or_default()
                } else if matches!(props_kind, PropsKind::Proto) {
                    "constructor"
                } else {
                    "prototype"
                };
                let h = self.hash_prop(name)? & hash_mask;
                prop_hash[i] = hash_table[h as usize];
                hash_table[h as usize] = 2 + hash_size + 3 * prop_idx;
            }
            self.push_raw(js_value_array_header(
                (2 + hash_size + n_props_u32 * 3) as u64,
            ));
            self.push_raw((n_props_u32 as u64) << 1);
            self.push_raw((hash_mask as u64) << 1);
            for entry in hash_table {
                self.push_raw((entry as u64) << 1);
            }
        }

        for i in 0..n_props {
            let prop = if i < base_len { props[i] } else { PropDef::End };
            let name = if i < base_len {
                prop.name().unwrap_or_default()
            } else if matches!(props_kind, PropsKind::Proto) {
                "constructor"
            } else {
                "prototype"
            };
            let name_word = self.atom_word(name)?;
            self.words.push(name_word);

            let mut prop_type = JS_PROP_NORMAL;
            match prop {
                PropDef::PropDouble { value, .. } => {
                    if let Some(ident) = ident_tab[i] {
                        self.push_rom(ident);
                    } else {
                        self.push_raw(int_to_js_value(value as i64, self.word_bytes));
                    }
                }
                PropDef::CGetSet { name, .. } => {
                    if is_global_object {
                        return Err(BuildError::GlobalGetSetForbidden(name.to_string()));
                    }
                    prop_type = JS_PROP_GETSET;
                    let ident = ident_tab[i].expect("getset must allocate");
                    self.push_rom(ident);
                }
                PropDef::Class { .. } => {
                    let ident = ident_tab[i].expect("class must allocate");
                    self.push_rom(ident);
                }
                PropDef::PropUndefined { .. } => {
                    self.push_raw(JS_UNDEFINED);
                }
                PropDef::PropNull { .. } => {
                    self.push_raw(JS_NULL);
                }
                PropDef::PropString { value, .. } => {
                    let value_word = self.atom_word(value)?;
                    self.words.push(value_word);
                }
                PropDef::CFunc {
                    name,
                    length,
                    magic,
                    cproto_name,
                    func_name,
                } => {
                    let idx = self.add_cfunc(name, length, magic, cproto_name, func_name);
                    self.push_raw(js_value_make_special(JS_TAG_SHORT_FUNC, idx as u64));
                }
                PropDef::End => {
                    let class_id = class_id.ok_or_else(|| {
                        BuildError::MissingClassId(format!("props for {}", props_kind.name()))
                    })?;
                    let class_id_value = class_id_value(class_id)? as i64;
                    if matches!(props_kind, PropsKind::Proto) {
                        self.push_raw(int_to_js_value(-(class_id_value) - 1, self.word_bytes));
                    } else {
                        self.push_raw(int_to_js_value(class_id_value, self.word_bytes));
                    }
                    prop_type = JS_PROP_SPECIAL;
                }
            }

            if !is_global_object {
                let next = prop_hash[i];
                let flags = ((next as u64) << 1) | (prop_type << 30);
                self.push_raw(flags);
            }
        }

        Ok(props_ident)
    }

    fn define_value(&mut self, prop: &PropDef<'a>) -> Result<Option<u32>, BuildError> {
        match prop {
            PropDef::PropDouble { value, .. } => {
                if is_short_int(*value) {
                    Ok(None)
                } else {
                    let ident = self.cur_offset();
                    self.push_raw(js_mb_header_def(JS_MTAG_FLOAT64));
                    let bits = value.to_bits();
                    if self.word_bytes == 8 {
                        self.push_raw(bits);
                    } else {
                        let lo = bits as u32 as u64;
                        let hi = (bits >> 32) as u32 as u64;
                        self.push_raw(lo);
                        self.push_raw(hi);
                    }
                    Ok(Some(ident))
                }
            }
            PropDef::Class { class, .. } => {
                let ident = self.define_class(class)?;
                Ok(Some(ident))
            }
            PropDef::CGetSet {
                name,
                magic,
                cproto_name,
                get_func_name,
                set_func_name,
            } => {
                let get_idx = if let Some(get_name) = get_func_name {
                    let get_label = format!("get {}", name);
                    Some(self.add_cfunc(
                        &get_label,
                        0,
                        magic,
                        cproto_name,
                        get_name,
                    ))
                } else {
                    None
                };
                let set_idx = if let Some(set_name) = set_func_name {
                    let set_label = format!("set {}", name);
                    Some(self.add_cfunc(
                        &set_label,
                        1,
                        magic,
                        cproto_name,
                        set_name,
                    ))
                } else {
                    None
                };
                let ident = self.cur_offset();
                self.push_raw(js_value_array_header(2));
                if let Some(idx) = get_idx {
                    self.push_raw(js_value_make_special(JS_TAG_SHORT_FUNC, idx as u64));
                } else {
                    self.push_raw(JS_UNDEFINED);
                }
                if let Some(idx) = set_idx {
                    self.push_raw(js_value_make_special(JS_TAG_SHORT_FUNC, idx as u64));
                } else {
                    self.push_raw(JS_UNDEFINED);
                }
                Ok(Some(ident))
            }
            _ => Ok(None),
        }
    }

    fn define_class(&mut self, class: &ClassDef<'a>) -> Result<u32, BuildError> {
        if let Some(entry) = self.find_class_entry(class) {
            return Ok(entry.class_idx);
        }

        let parent_idx = if let Some(parent) = class.parent_class {
            Some(self.define_class(parent)?)
        } else {
            None
        };

        let mut ctor_idx = None;
        if let Some(func_name) = class.func_name {
            let class_id = class
                .class_id
                .ok_or_else(|| BuildError::MissingClassId(class.name.to_string()))?;
            let cproto_name = class
                .cproto_name
                .ok_or_else(|| BuildError::MissingClassConstructor(class.name.to_string()))?;
            let idx = self.add_cfunc(class.name, class.length as u8, class_id, cproto_name, func_name);
            ctor_idx = Some(idx);
        }

        let (class_props_idx, proto_props_idx) = if ctor_idx.is_some() {
            let class_id = class
                .class_id
                .ok_or_else(|| BuildError::MissingClassId(class.name.to_string()))?;
            let class_props_idx =
                self.define_props(class.class_props, PropsKind::Class, Some(class_id))?;
            let proto_props_idx =
                self.define_props(class.proto_props, PropsKind::Proto, Some(class_id))?;
            (Some(class_props_idx), Some(proto_props_idx))
        } else if class.class_props.is_some() {
            let class_props_idx =
                self.define_props(class.class_props, PropsKind::Object, class.class_id)?;
            (Some(class_props_idx), None)
        } else {
            (None, None)
        };

        let ident = self.cur_offset();
        self.push_raw(js_mb_header_def(JS_MTAG_OBJECT));
        if let Some(offset) = class_props_idx {
            self.push_rom(offset);
        } else {
            self.push_raw(JS_NULL);
        }
        let ctor_word = int_to_word(ctor_idx.map(|idx| idx as i64).unwrap_or(-1), self.word_bytes);
        self.push_raw(ctor_word);
        if let Some(offset) = proto_props_idx {
            self.push_rom(offset);
        } else {
            self.push_raw(JS_NULL);
        }
        if let Some(offset) = parent_idx {
            self.push_rom(offset);
        } else {
            self.push_raw(JS_NULL);
        }

        let mut entry = ClassEntry {
            class: class as *const _,
            class_idx: ident,
            class_id: None,
            finalizer_name: None,
        };
        if ctor_idx.is_some() {
            entry.class_id = class.class_id.map(str::to_string);
            entry.finalizer_name = class.finalizer_name.map(str::to_string);
        }
        self.class_entries.push(entry);
        Ok(ident)
    }

    fn find_class_entry(&self, class: &ClassDef<'a>) -> Option<&ClassEntry<'a>> {
        let ptr = class as *const _;
        self.class_entries.iter().find(|entry| entry.class == ptr)
    }

    fn clear_class_entries(&mut self) {
        self.class_entries.clear();
    }

    fn atom_value_only(&self, name: &str) -> Result<u64, BuildError> {
        validate_property_name(name)?;
        if name.len() == 1 {
            let byte = name.as_bytes()[0] as u64;
            return Ok(js_value_make_special(JS_TAG_STRING_CHAR, byte));
        }
        let offset = self
            .find_atom_offset(name)
            .ok_or_else(|| BuildError::UndefinedAtom(name.to_string()))?;
        Ok((offset as u64) * (self.word_bytes as u64) + 1)
    }

    fn atom_word(&self, name: &str) -> Result<StdlibWord, BuildError> {
        validate_property_name(name)?;
        if name.len() == 1 {
            let byte = name.as_bytes()[0] as u64;
            return Ok(StdlibWord::Raw(js_value_make_special(JS_TAG_STRING_CHAR, byte)));
        }
        let offset = self
            .find_atom_offset(name)
            .ok_or_else(|| BuildError::UndefinedAtom(name.to_string()))?;
        Ok(StdlibWord::RomOffset(offset))
    }

    fn hash_prop(&self, name: &str) -> Result<u32, BuildError> {
        let prop = self.atom_value_only(name)?;
        let word_bytes = self.word_bytes as u64;
        Ok(((prop / word_bytes) ^ (prop % word_bytes)) as u32)
    }

    fn finalize_cfuncs(&self) -> Vec<GenCFuncMeta> {
        self.cfunc_list
            .iter()
            .map(|entry| GenCFuncMeta {
                name: entry.name.clone(),
                cproto_name: entry.cproto_name.clone(),
                func_name: entry.func_name.clone(),
                arg_count: entry.length,
                magic: entry.magic.clone(),
            })
            .collect()
    }

    fn finalize_cfinalizers(&self) -> Vec<GenCFinalizerMeta> {
        self.class_entries
            .iter()
            .filter_map(|entry| {
                let class_id = entry.class_id.as_ref()?;
                let finalizer = entry.finalizer_name.as_ref()?;
                if finalizer == "NULL" {
                    return None;
                }
                Some(GenCFinalizerMeta {
                    class_id: class_id.clone(),
                    finalizer_name: finalizer.clone(),
                })
            })
            .collect()
    }
}

impl<'a> PropDef<'a> {
    fn name(&self) -> Option<&'a str> {
        match self {
            PropDef::CFunc { name, .. } => Some(name),
            PropDef::CGetSet { name, .. } => Some(name),
            PropDef::PropDouble { name, .. } => Some(name),
            PropDef::PropUndefined { name } => Some(name),
            PropDef::PropNull { name } => Some(name),
            PropDef::PropString { name, .. } => Some(name),
            PropDef::Class { name, .. } => Some(name),
            PropDef::End => None,
        }
    }
}

impl PropsKind {
    fn name(self) -> &'static str {
        match self {
            PropsKind::Global => "global",
            PropsKind::Proto => "proto",
            PropsKind::Class => "class",
            PropsKind::Object => "object",
        }
    }
}

fn props_or_empty<'a>(props: Option<&'a [PropDef<'a>]>) -> &'a [PropDef<'a>] {
    props.unwrap_or(&EMPTY_PROPS)
}

fn prop_list_len(props: &[PropDef<'_>]) -> Result<usize, BuildError> {
    for (idx, prop) in props.iter().enumerate() {
        if matches!(prop, PropDef::End) {
            return Ok(idx);
        }
    }
    Err(BuildError::MissingPropEnd)
}

fn prop_slice<'a>(props: &'a [PropDef<'a>]) -> Result<&'a [PropDef<'a>], BuildError> {
    let len = prop_list_len(props)?;
    Ok(&props[..len])
}

fn hash_table_size(n_props: usize, word_bytes: u32) -> u32 {
    if n_props <= 1 {
        return 1;
    }
    let n = (n_props - 1) as u32;
    let hash_size_log2 = (32 - n.leading_zeros()) - 1;
    let mut hash_size = 1u32 << hash_size_log2;
    let max_size = ATOM_ALIGN / word_bytes;
    if hash_size > max_size {
        hash_size = max_size;
    }
    hash_size
}

fn is_short_int(value: f64) -> bool {
    if value < JS_SHORTINT_MIN as f64 || value > JS_SHORTINT_MAX as f64 {
        return false;
    }
    value.trunc() == value
}

fn validate_property_name(name: &str) -> Result<(), BuildError> {
    if !is_ascii_string(name.as_bytes()) {
        return Err(BuildError::NonAsciiPropertyName(name.to_string()));
    }
    if matches!(name.as_bytes().first(), Some(b'0'..=b'9')) {
        return Err(BuildError::NumericPropertyName(name.to_string()));
    }
    Ok(())
}

fn is_ascii_string(bytes: &[u8]) -> bool {
    bytes.iter().all(|byte| *byte <= 0x7f)
}

fn is_numeric_string(value: &str) -> bool {
    matches!(value, "NaN" | "Infinity" | "-Infinity")
}

const fn js_value_make_special(tag: u64, value: u64) -> u64 {
    tag | (value << JS_TAG_SPECIAL_BITS)
}

const fn js_mb_header_def(tag: u64) -> u64 {
    tag << 1
}

const fn js_value_array_header(size: u64) -> u64 {
    js_mb_header_def(JS_MTAG_VALUE_ARRAY) | (size << JS_MTAG_BITS)
}

fn string_header(len: u64, is_ascii: bool, is_numeric: bool) -> u64 {
    (JS_MTAG_STRING << 1)
        | (1 << JS_MTAG_BITS)
        | ((is_ascii as u64) << (JS_MTAG_BITS + 1))
        | ((is_numeric as u64) << (JS_MTAG_BITS + 2))
        | (len << (JS_MTAG_BITS + 3))
}

fn int_to_word(value: i64, word_bytes: u32) -> u64 {
    if word_bytes == 8 {
        value as u64
    } else {
        (value as i32 as u32) as u64
    }
}

fn int_to_js_value(value: i64, word_bytes: u32) -> u64 {
    int_to_word(value << 1, word_bytes)
}

fn class_id_value(class_id: &str) -> Result<i32, BuildError> {
    CLASS_ID_TABLE
        .iter()
        .position(|entry| *entry == class_id)
        .map(|idx| idx as i32)
        .ok_or_else(|| BuildError::UnknownClassId(class_id.to_string()))
}

fn format_word(value: u64, word_bytes: u8) -> String {
    let width = (word_bytes as usize) * 2;
    format!("0x{:0width$x}", value, width = width)
}

const JS_NULL: u64 = js_value_make_special(JS_TAG_NULL, 0);
const JS_UNDEFINED: u64 = js_value_make_special(JS_TAG_UNDEFINED, 0);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn atom_padding_for_aligned_len() {
        let predefined = &["abcd"];
        let global = [PropDef::End];
        let input = BuildInput {
            global_object: &global,
            c_function_decl: None,
            predefined_atoms: predefined,
            word_bytes: 4,
            class_count: 0,
        };
        let image = build_stdlib(&input).unwrap();
        let header = string_header(4, true, false);
        assert_eq!(image.sorted_atoms_offset, 3);
        assert_eq!(image.words[0], StdlibWord::Raw(header));
        let expected = u32::from_le_bytes(*b"abcd") as u64;
        assert_eq!(image.words[1], StdlibWord::Raw(expected));
        assert_eq!(image.words[2], StdlibWord::Raw(0));
    }

    #[test]
    fn sorted_atom_and_global_offsets() {
        let predefined = &["", "foo"];
        let global = [
            PropDef::PropString {
                name: "bar",
                value: "baz",
            },
            PropDef::End,
        ];
        let input = BuildInput {
            global_object: &global,
            c_function_decl: None,
            predefined_atoms: predefined,
            word_bytes: 4,
            class_count: 0,
        };
        let image = build_stdlib(&input).unwrap();
        assert_eq!(image.sorted_atoms_offset, 8);
        assert_eq!(image.global_object_offset, 13);
        assert_eq!(image.stdlib_table_len, 16);
    }

    #[test]
    fn numeric_property_name_rejected() {
        let predefined = &["good"];
        let global = [PropDef::PropNull { name: "1bad" }, PropDef::End];
        let input = BuildInput {
            global_object: &global,
            c_function_decl: None,
            predefined_atoms: predefined,
            word_bytes: 4,
            class_count: 0,
        };
        let err = build_stdlib(&input).unwrap_err();
        assert!(matches!(err, BuildError::NumericPropertyName(name) if name == "1bad"));
    }

    #[test]
    fn non_ascii_property_name_rejected() {
        let predefined = &["good"];
        let global = [PropDef::PropNull { name: "caf\u{00e9}" }, PropDef::End];
        let input = BuildInput {
            global_object: &global,
            c_function_decl: None,
            predefined_atoms: predefined,
            word_bytes: 4,
            class_count: 0,
        };
        let err = build_stdlib(&input).unwrap_err();
        assert!(matches!(err, BuildError::NonAsciiPropertyName(name) if name == "caf\u{00e9}"));
    }

    #[test]
    fn getset_in_global_is_rejected() {
        let predefined = &["prop"];
        let global = [
            PropDef::CGetSet {
                name: "prop",
                magic: "0",
                cproto_name: "generic",
                get_func_name: Some("js_get_prop"),
                set_func_name: None,
            },
            PropDef::End,
        ];
        let input = BuildInput {
            global_object: &global,
            c_function_decl: None,
            predefined_atoms: predefined,
            word_bytes: 4,
            class_count: 0,
        };
        let err = build_stdlib(&input).unwrap_err();
        assert!(matches!(err, BuildError::GlobalGetSetForbidden(name) if name == "prop"));
    }

    #[test]
    fn float_allocated_for_non_short_int() {
        let predefined = &["value"];
        let global = [
            PropDef::PropDouble {
                name: "value",
                value: 1.5,
            },
            PropDef::End,
        ];
        let input = BuildInput {
            global_object: &global,
            c_function_decl: None,
            predefined_atoms: predefined,
            word_bytes: 4,
            class_count: 0,
        };
        let image = build_stdlib(&input).unwrap();
        let float_header = StdlibWord::Raw(js_mb_header_def(JS_MTAG_FLOAT64));
        assert!(image.words.iter().any(|word| *word == float_header));
    }

    #[test]
    fn short_int_does_not_allocate_float() {
        let predefined = &["value"];
        let global = [
            PropDef::PropDouble {
                name: "value",
                value: 1.0,
            },
            PropDef::End,
        ];
        let input = BuildInput {
            global_object: &global,
            c_function_decl: None,
            predefined_atoms: predefined,
            word_bytes: 4,
            class_count: 0,
        };
        let image = build_stdlib(&input).unwrap();
        let float_header = StdlibWord::Raw(js_mb_header_def(JS_MTAG_FLOAT64));
        assert!(!image.words.iter().any(|word| *word == float_header));
    }

    #[test]
    fn missing_prop_end_errors() {
        let predefined = &["value"];
        let global = [PropDef::PropNull { name: "value" }];
        let input = BuildInput {
            global_object: &global,
            c_function_decl: None,
            predefined_atoms: predefined,
            word_bytes: 4,
            class_count: 0,
        };
        let err = build_stdlib(&input).unwrap_err();
        assert!(matches!(err, BuildError::MissingPropEnd));
    }
}
