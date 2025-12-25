use crate::jsvalue::JSValue;

// C: `JSArrayData` in mquickjs.c.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct ArrayData {
    tab: JSValue,
    len: u32,
}

impl ArrayData {
    pub const LEN_MAX: u32 = (1 << 30) - 1;

    pub fn new(tab: JSValue, len: u32) -> Self {
        debug_assert!(len <= Self::LEN_MAX);
        Self { tab, len }
    }

    pub const fn tab(self) -> JSValue {
        self.tab
    }

    pub(crate) unsafe fn tab_ptr(this: *mut Self) -> *mut JSValue {
        // SAFETY: caller guarantees `this` is valid for writes.
        unsafe { core::ptr::addr_of_mut!((*this).tab) }
    }

    pub const fn len(self) -> u32 {
        self.len
    }

    pub const fn is_empty(self) -> bool {
        self.len == 0
    }
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    #[test]
    fn array_data_roundtrip() {
        let data = ArrayData::new(crate::jsvalue::JS_NULL, 12);
        assert_eq!(data.tab(), crate::jsvalue::JS_NULL);
        assert_eq!(data.len(), 12);
        assert!(!data.is_empty());
    }

    #[test]
    fn array_data_len_max() {
        let data = ArrayData::new(crate::jsvalue::JS_NULL, ArrayData::LEN_MAX);
        assert_eq!(data.len(), ArrayData::LEN_MAX);
    }
}
