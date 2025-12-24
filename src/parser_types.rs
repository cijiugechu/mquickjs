use crate::jsvalue::JSValue;

// C: `BlockEnv` in mquickjs.c (stack structure with JSValue-only fields).
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct BlockEnv {
    prev: JSValue,
    label_name: JSValue,
    label_break: JSValue,
    label_cont: JSValue,
    label_finally: JSValue,
    drop_count: JSValue,
}

impl BlockEnv {
    pub const fn new(
        prev: JSValue,
        label_name: JSValue,
        label_break: JSValue,
        label_cont: JSValue,
        label_finally: JSValue,
        drop_count: JSValue,
    ) -> Self {
        Self {
            prev,
            label_name,
            label_break,
            label_cont,
            label_finally,
            drop_count,
        }
    }

    pub const fn prev(self) -> JSValue {
        self.prev
    }

    pub const fn label_name(self) -> JSValue {
        self.label_name
    }

    pub const fn label_break(self) -> JSValue {
        self.label_break
    }

    pub const fn label_cont(self) -> JSValue {
        self.label_cont
    }

    pub const fn label_finally(self) -> JSValue {
        self.label_finally
    }

    pub const fn drop_count(self) -> JSValue {
        self.drop_count
    }
}

// C: `JSSourcePos` typedef in mquickjs.c.
pub type SourcePos = u32;

// C: `JSToken` in mquickjs.c.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Token {
    val: i32,
    source_pos: SourcePos,
    extra: TokenExtra,
    value: JSValue,
}

impl Token {
    pub const fn new(val: i32, source_pos: SourcePos, extra: TokenExtra, value: JSValue) -> Self {
        Self {
            val,
            source_pos,
            extra,
            value,
        }
    }

    pub const fn val(self) -> i32 {
        self.val
    }

    pub const fn source_pos(self) -> SourcePos {
        self.source_pos
    }

    pub const fn extra(self) -> TokenExtra {
        self.extra
    }

    pub const fn value(self) -> JSValue {
        self.value
    }
}

// C: `JSToken` union payload in mquickjs.c.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TokenExtra {
    None,
    Number(f64),
    RegExp { flags: u32, end_pos: u32 },
}

// C: `JSParsePos` in mquickjs.c.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct ParsePos {
    got_lf: bool,
    regexp_allowed: bool,
    source_pos: SourcePos,
}

impl ParsePos {
    pub const fn new(got_lf: bool, regexp_allowed: bool, source_pos: SourcePos) -> Self {
        Self {
            got_lf,
            regexp_allowed,
            source_pos,
        }
    }

    pub const fn got_lf(self) -> bool {
        self.got_lf
    }

    pub const fn regexp_allowed(self) -> bool {
        self.regexp_allowed
    }

    pub const fn source_pos(self) -> SourcePos {
        self.source_pos
    }
}
