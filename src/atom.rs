use crate::cutils::{unicode_to_utf8, utf8_get};
use crate::dtoa::{js_atod, js_dtoa, AtodFlags, JS_DTOA_FORMAT_FREE};
use crate::jsvalue::{
    is_ptr,
    value_get_special_tag,
    value_get_special_value,
    value_to_ptr,
    JSValue,
    JS_TAG_STRING_CHAR,
};
use crate::string::js_string::JSString;
use core::cmp::Ordering;

const STRING_CHAR_BUF_LEN: usize = 5;

// Invariants:
// - Atom tables are sorted with `js_string_compare` (UTF-16 ordering semantics).
// - Unique strings are JSString values with the `is_unique` flag set.
// - Unique string entries are weak references and may be pruned after GC.
#[derive(Debug, Default)]
pub struct AtomTables {
    rom_tables: Vec<Vec<JSValue>>,
    unique_strings: Vec<JSValue>,
}

impl AtomTables {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_rom_table(&mut self, table: Vec<JSValue>) {
        debug_assert!(is_sorted_atoms(&table));
        self.rom_tables.push(table);
    }

    pub fn unique_strings(&self) -> &[JSValue] {
        &self.unique_strings
    }

    pub fn unique_len(&self) -> usize {
        self.unique_strings.len()
    }

    pub fn make_unique_string(&mut self, val: JSValue) -> JSValue {
        if !is_ptr(val) {
            return val;
        }
        let ptr = match value_to_ptr::<JSString>(val) {
            Some(ptr) => ptr,
            None => return val,
        };
        if unsafe { ptr.as_ref() }.is_unique() {
            return val;
        }

        for table in &self.rom_tables {
            if let Ok(idx) = find_atom(table, val) {
                return table[idx];
            }
        }

        match find_atom(&self.unique_strings, val) {
            Ok(idx) => self.unique_strings[idx],
            Err(insert_at) => {
                let is_numeric = unsafe { is_numeric_string(ptr.as_ref()) };
                // SAFETY: `val` is expected to reference a live `JSString`.
                unsafe {
                    ptr.as_ref().set_unique_flags(true, is_numeric);
                }
                self.unique_strings.insert(insert_at, val);
                val
            }
        }
    }

    pub fn sweep_unique_strings<F>(&mut self, mut is_marked: F) -> usize
    where
        F: FnMut(JSValue) -> bool,
    {
        self.unique_strings
            .retain(|val| is_marked(*val));
        self.unique_strings.len()
    }

    pub fn visit_rom_atoms<F>(&self, mut f: F)
    where
        F: FnMut(JSValue),
    {
        for table in &self.rom_tables {
            for &val in table {
                f(val);
            }
        }
    }
}

fn find_atom(arr: &[JSValue], val: JSValue) -> Result<usize, usize> {
    arr.binary_search_by(|probe| cmp_js_values(*probe, val))
}

fn cmp_js_values(a: JSValue, b: JSValue) -> Ordering {
    match js_string_compare(a, b) {
        0 => Ordering::Equal,
        x if x < 0 => Ordering::Less,
        _ => Ordering::Greater,
    }
}

fn is_sorted_atoms(values: &[JSValue]) -> bool {
    values
        .windows(2)
        .all(|pair| cmp_js_values(pair[0], pair[1]) != Ordering::Greater)
}

fn is_numeric_string(string: &JSString) -> bool {
    if string.is_empty() || !string.is_ascii() {
        return false;
    }
    let bytes = string.buf();
    let mut idx = 0usize;
    if bytes[0] == b'-' {
        if bytes.len() == 1 {
            return false;
        }
        idx = 1;
    }
    if !bytes[idx].is_ascii_digit() {
        return false;
    }
    let input = match core::str::from_utf8(bytes) {
        Ok(s) => s,
        Err(_) => return false,
    };
    let parsed = match js_atod(input, 10, AtodFlags::empty()) {
        Ok(parsed) => parsed,
        Err(_) => return false,
    };
    if parsed.next != bytes.len() {
        return false;
    }
    let printed = match js_dtoa(parsed.value, 10, 0, JS_DTOA_FORMAT_FREE) {
        Ok(s) => s,
        Err(_) => return false,
    };
    printed.as_bytes() == bytes
}

fn string_bytes(val: JSValue, scratch: &mut [u8; STRING_CHAR_BUF_LEN]) -> Option<&[u8]> {
    if is_ptr(val) {
        let ptr = value_to_ptr::<JSString>(val)?;
        // SAFETY: `val` is expected to reference a live `JSString`.
        let string = unsafe { ptr.as_ref() };
        return Some(string.buf());
    }
    if value_get_special_tag(val) == JS_TAG_STRING_CHAR {
        let codepoint = value_get_special_value(val) as u32;
        let len = unicode_to_utf8(scratch, codepoint);
        debug_assert!(len > 0);
        return Some(&scratch[..len]);
    }
    None
}

fn string_get_cp(bytes: &[u8], index: usize) -> i32 {
    let mut start = index;
    while start > 0 && (bytes[start] & 0xc0) == 0x80 {
        start -= 1;
    }
    let mut len = 0usize;
    utf8_get(&bytes[start..], &mut len)
}

fn js_string_compare(val1: JSValue, val2: JSValue) -> i32 {
    let mut buf1 = [0u8; STRING_CHAR_BUF_LEN];
    let mut buf2 = [0u8; STRING_CHAR_BUF_LEN];
    let bytes1 = string_bytes(val1, &mut buf1).expect("expected string value");
    let bytes2 = string_bytes(val2, &mut buf2).expect("expected string value");
    let len = bytes1.len().min(bytes2.len());
    let mut i = 0usize;
    while i < len {
        if bytes1[i] != bytes2[i] {
            break;
        }
        i += 1;
    }
    if i != len {
        let mut c1 = string_get_cp(bytes1, i);
        let mut c2 = string_get_cp(bytes2, i);
        if (c1 < 0x10000 && c2 < 0x10000) || (c1 >= 0x10000 && c2 >= 0x10000) {
            return if c1 < c2 { -1 } else { 1 };
        }
        if c1 < 0x10000 {
            c2 = 0xd800 + ((c2 - 0x10000) >> 10);
            return if c1 <= c2 { -1 } else { 1 };
        }
        c1 = 0xd800 + ((c1 - 0x10000) >> 10);
        return if c1 < c2 { -1 } else { 1 };
    }
    if bytes1.len() == bytes2.len() {
        0
    } else if bytes1.len() < bytes2.len() {
        -1
    } else {
        1
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jsvalue::{value_from_ptr, value_make_special};
    use crate::string::js_string::is_ascii_bytes;
    use core::ptr::NonNull;

    fn make_string(
        keepalive: &mut Vec<Box<JSString>>,
        bytes: &[u8],
        is_unique: bool,
    ) -> JSValue {
        let is_ascii = is_ascii_bytes(bytes);
        let string = JSString::new(bytes.to_vec(), is_unique, is_ascii, false).unwrap();
        keepalive.push(Box::new(string));
        let index = keepalive.len() - 1;
        let ptr = NonNull::from(keepalive[index].as_ref());
        value_from_ptr(ptr)
    }

    #[test]
    fn make_unique_string_inserts_and_marks_numeric() {
        let mut tables = AtomTables::new();
        let mut keepalive = Vec::with_capacity(1);
        let val = make_string(&mut keepalive, b"123", false);
        let out = tables.make_unique_string(val);
        assert_eq!(out, val);
        assert_eq!(tables.unique_len(), 1);
        let ptr = value_to_ptr::<JSString>(val).unwrap();
        let string = unsafe { ptr.as_ref() };
        assert!(string.is_unique());
        assert!(string.is_numeric());
    }

    #[test]
    fn make_unique_string_non_numeric_stays_false() {
        let mut tables = AtomTables::new();
        let mut keepalive = Vec::with_capacity(1);
        let val = make_string(&mut keepalive, b"01", false);
        tables.make_unique_string(val);
        let ptr = value_to_ptr::<JSString>(val).unwrap();
        let string = unsafe { ptr.as_ref() };
        assert!(string.is_unique());
        assert!(!string.is_numeric());
    }

    #[test]
    fn make_unique_string_reuses_rom_table_entry() {
        let mut tables = AtomTables::new();
        let mut keepalive = Vec::with_capacity(2);
        let rom_val = make_string(&mut keepalive, b"foo", true);
        let val = make_string(&mut keepalive, b"foo", false);
        tables.add_rom_table(vec![rom_val]);
        let out = tables.make_unique_string(val);
        assert_eq!(out, rom_val);
        assert_eq!(tables.unique_len(), 0);
        let ptr = value_to_ptr::<JSString>(val).unwrap();
        let string = unsafe { ptr.as_ref() };
        assert!(!string.is_unique());
    }

    #[test]
    fn make_unique_string_ignores_string_char_values() {
        let mut tables = AtomTables::new();
        let val = value_make_special(JS_TAG_STRING_CHAR, 0x61);
        let out = tables.make_unique_string(val);
        assert_eq!(out, val);
        assert_eq!(tables.unique_len(), 0);
    }

    #[test]
    fn make_unique_string_keeps_table_sorted() {
        let mut tables = AtomTables::new();
        let mut keepalive = Vec::with_capacity(2);
        let val_b = make_string(&mut keepalive, b"b", false);
        let val_a = make_string(&mut keepalive, b"a", false);
        tables.make_unique_string(val_b);
        tables.make_unique_string(val_a);
        assert_eq!(tables.unique_strings(), &[val_a, val_b]);
    }

    #[test]
    fn sweep_unique_strings_removes_unmarked() {
        let mut tables = AtomTables::new();
        let mut keepalive = Vec::with_capacity(3);
        let val_a = make_string(&mut keepalive, b"a", false);
        let val_b = make_string(&mut keepalive, b"b", false);
        let val_c = make_string(&mut keepalive, b"c", false);
        tables.make_unique_string(val_a);
        tables.make_unique_string(val_b);
        tables.make_unique_string(val_c);
        let kept = [val_a, val_c];
        let new_len = tables.sweep_unique_strings(|val| kept.contains(&val));
        assert_eq!(new_len, 2);
        assert_eq!(tables.unique_strings(), &[val_a, val_c]);
    }

    #[test]
    fn string_compare_respects_utf16_surrogate_order() {
        let mut keepalive = Vec::with_capacity(2);
        let bmp = make_string(&mut keepalive, &[0xee, 0x80, 0x80], false);
        let non_bmp = make_string(&mut keepalive, &[0xf0, 0x90, 0x80, 0x80], false);
        assert_eq!(js_string_compare(bmp, non_bmp), 1);
        assert_eq!(js_string_compare(non_bmp, bmp), -1);
    }
}
