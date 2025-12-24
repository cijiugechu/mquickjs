use crate::jsvalue::JSValue;

// C: `JSCFunctionData` in mquickjs.c.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct CFunctionData {
    idx: u32,
    params: JSValue,
}

impl CFunctionData {
    pub const fn new(idx: u32, params: JSValue) -> Self {
        Self { idx, params }
    }

    pub const fn idx(self) -> u32 {
        self.idx
    }

    pub const fn params(self) -> JSValue {
        self.params
    }
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    #[test]
    fn cfunction_data_roundtrip() {
        let data = CFunctionData::new(7, crate::jsvalue::JS_NULL);
        assert_eq!(data.idx(), 7);
        assert_eq!(data.params(), crate::jsvalue::JS_NULL);
    }
}
