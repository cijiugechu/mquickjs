#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StdlibWord {
    Raw(u64),
    RomOffset(u32),
}

#[derive(Clone, Copy, Debug)]
pub struct CFuncMeta {
    pub name: &'static str,
    pub cproto_name: &'static str,
    pub func_name: &'static str,
    pub arg_count: u8,
    pub magic: &'static str,
}

#[derive(Clone, Copy, Debug)]
pub struct CFinalizerMeta {
    pub class_id: &'static str,
    pub finalizer_name: &'static str,
}

#[derive(Clone, Copy, Debug)]
pub struct StdlibImage {
    pub words: &'static [StdlibWord],
    pub c_functions: &'static [CFuncMeta],
    pub c_finalizers: &'static [CFinalizerMeta],
    pub stdlib_table_len: u32,
    pub stdlib_table_align: u32,
    pub sorted_atoms_offset: u32,
    pub global_object_offset: u32,
    pub word_bytes: u8,
    pub class_count: u32,
}
