use crate::containers::StringHeader;
use crate::cutils::{unicode_to_utf8, utf8_get};
use crate::dtoa::{js_atod, js_dtoa, AtodFlags, JS_DTOA_FORMAT_FREE};
use crate::jsvalue::{
    is_ptr,
    value_get_special_tag,
    value_get_special_value,
    value_to_ptr,
    JSValue,
    JSWord,
    JS_TAG_STRING_CHAR,
};
use crate::memblock::{MbHeader, MTag};
use core::cmp::Ordering;
use core::mem::size_of;
use core::ptr::{self, NonNull};
use core::slice;

const STRING_CHAR_BUF_LEN: usize = 5;

// Invariants:
// - Atom tables are sorted with `js_string_compare` (UTF-16 ordering semantics).
// - Atom tables store heap-backed JSString memblocks (MTag::String).
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

    pub fn set_unique_strings(&mut self, values: Vec<JSValue>) {
        debug_assert!(is_sorted_atoms(&values));
        self.unique_strings = values;
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
        let ptr = match value_to_ptr::<u8>(val) {
            Some(ptr) => ptr,
            None => return val,
        };
        let Some(header) = string_header(ptr) else {
            return val;
        };
        if header.is_unique() {
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
                let bytes = string_bytes_from_ptr(ptr, header);
                let is_numeric = is_numeric_string(bytes, header.is_ascii());
                set_unique_flags(ptr, header, is_numeric);
                self.unique_strings.insert(insert_at, val);
                val
            }
        }
    }

    pub fn empty_string_atom(&self) -> Option<JSValue> {
        if let Some(table) = self.rom_tables.first() {
            table.first().copied()
        } else {
            self.unique_strings.first().copied()
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

    pub fn visit_root_slots<F>(&mut self, mut f: F)
    where
        F: FnMut(*mut JSValue),
    {
        for table in &mut self.rom_tables {
            for val in table.iter_mut() {
                f(val as *mut JSValue);
            }
        }
        for val in self.unique_strings.iter_mut() {
            f(val as *mut JSValue);
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

fn is_numeric_string(bytes: &[u8], is_ascii: bool) -> bool {
    if bytes.is_empty() || !is_ascii {
        return false;
    }
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
        let ptr = value_to_ptr::<u8>(val)?;
        let header = string_header(ptr)?;
        let bytes = string_bytes_from_ptr(ptr, header);
        return Some(bytes);
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

fn string_header(ptr: NonNull<u8>) -> Option<StringHeader> {
    let header_word = unsafe {
        // SAFETY: `ptr` points to readable header storage.
        ptr::read_unaligned(ptr.as_ptr().cast::<JSWord>())
    };
    let header = MbHeader::from_word(header_word);
    if header.tag() != MTag::String {
        return None;
    }
    Some(StringHeader::from(header))
}

fn string_bytes_from_ptr<'a>(ptr: NonNull<u8>, header: StringHeader) -> &'a [u8] {
    let len = header.len() as usize;
    unsafe {
        // SAFETY: caller ensures `ptr` points to a string memblock with `len` bytes.
        slice::from_raw_parts(ptr.as_ptr().add(size_of::<JSWord>()), len)
    }
}

fn set_unique_flags(ptr: NonNull<u8>, header: StringHeader, is_numeric: bool) {
    let new_header = StringHeader::new(
        header.len(),
        true,
        header.is_ascii(),
        is_numeric,
        header.header().gc_mark(),
    );
    unsafe {
        // SAFETY: `ptr` points to writable string header storage.
        ptr::write_unaligned(ptr.as_ptr().cast::<JSWord>(), MbHeader::from(new_header).word());
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::containers::{string_alloc_size, JS_STRING_LEN_MAX};
    use crate::heap::HeapLayout;
    use crate::jsvalue::{value_from_ptr, value_make_special, JSValue, JSW};
    use crate::memblock::MTag;
    use crate::string::js_string::is_ascii_bytes;
    use core::ptr::{self, NonNull};

    struct HeapArena {
        _storage: Vec<JSWord>,
        layout: HeapLayout,
    }

    impl HeapArena {
        fn new(words: usize) -> Self {
            let mut storage = vec![0 as JSWord; words];
            let base = NonNull::new(storage.as_mut_ptr() as *mut u8).unwrap();
            let stack_top =
                unsafe { NonNull::new_unchecked(base.as_ptr().add(words * JSW as usize)) };
            let stack_bottom =
                unsafe { NonNull::new_unchecked(stack_top.as_ptr() as *mut JSValue) };
            let layout = HeapLayout::new(base, base, stack_top, stack_bottom, 0);
            Self {
                _storage: storage,
                layout,
            }
        }

        fn alloc_string(&mut self, bytes: &[u8], is_unique: bool) -> JSValue {
            let is_ascii = is_ascii_bytes(bytes);
            let len = JSWord::try_from(bytes.len()).unwrap();
            assert!(len <= JS_STRING_LEN_MAX);
            let size = string_alloc_size(len);
            let ptr = self
                .layout
                .malloc(size, MTag::String, |_| {})
                .expect("alloc string");
            let header = StringHeader::new(len, is_unique, is_ascii, false, false);
            unsafe {
                // SAFETY: `ptr` is a valid string memblock allocation.
                ptr::write_unaligned(ptr.as_ptr().cast::<JSWord>(), MbHeader::from(header).word());
                let payload = ptr.as_ptr().add(size_of::<JSWord>());
                ptr::write_bytes(payload, 0, size - size_of::<JSWord>());
                ptr::copy_nonoverlapping(bytes.as_ptr(), payload, bytes.len());
            }
            value_from_ptr(ptr)
        }
    }

    fn header_for(val: JSValue) -> StringHeader {
        let ptr = value_to_ptr::<u8>(val).expect("string ptr");
        string_header(ptr).expect("string header")
    }

    #[test]
    fn make_unique_string_inserts_and_marks_numeric() {
        let mut tables = AtomTables::new();
        let mut arena = HeapArena::new(64);
        let val = arena.alloc_string(b"123", false);
        let out = tables.make_unique_string(val);
        assert_eq!(out, val);
        assert_eq!(tables.unique_len(), 1);
        let header = header_for(val);
        assert!(header.is_unique());
        assert!(header.is_numeric());
    }

    #[test]
    fn make_unique_string_non_numeric_stays_false() {
        let mut tables = AtomTables::new();
        let mut arena = HeapArena::new(64);
        let val = arena.alloc_string(b"01", false);
        tables.make_unique_string(val);
        let header = header_for(val);
        assert!(header.is_unique());
        assert!(!header.is_numeric());
    }

    #[test]
    fn make_unique_string_reuses_rom_table_entry() {
        let mut tables = AtomTables::new();
        let mut arena = HeapArena::new(128);
        let rom_val = arena.alloc_string(b"foo", true);
        let val = arena.alloc_string(b"foo", false);
        tables.add_rom_table(vec![rom_val]);
        let out = tables.make_unique_string(val);
        assert_eq!(out, rom_val);
        assert_eq!(tables.unique_len(), 0);
        let header = header_for(val);
        assert!(!header.is_unique());
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
        let mut arena = HeapArena::new(64);
        let val_b = arena.alloc_string(b"b", false);
        let val_a = arena.alloc_string(b"a", false);
        tables.make_unique_string(val_b);
        tables.make_unique_string(val_a);
        assert_eq!(tables.unique_strings(), &[val_a, val_b]);
    }

    #[test]
    fn sweep_unique_strings_removes_unmarked() {
        let mut tables = AtomTables::new();
        let mut arena = HeapArena::new(128);
        let val_a = arena.alloc_string(b"a", false);
        let val_b = arena.alloc_string(b"b", false);
        let val_c = arena.alloc_string(b"c", false);
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
        let mut arena = HeapArena::new(128);
        let bmp = arena.alloc_string(&[0xee, 0x80, 0x80], false);
        let non_bmp = arena.alloc_string(&[0xf0, 0x90, 0x80, 0x80], false);
        assert_eq!(js_string_compare(bmp, non_bmp), 1);
        assert_eq!(js_string_compare(non_bmp, bmp), -1);
    }
}
