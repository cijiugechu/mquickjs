use crate::jsvalue::JSValue;
use core::ptr::NonNull;

// C: `JSContext` memory map fields in mquickjs.c.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct ContextShell {
    heap_base: NonNull<u8>,
    heap_free: NonNull<u8>,
    stack_top: NonNull<u8>,
    stack_bottom: NonNull<JSValue>,
    sp: NonNull<JSValue>,
    fp: NonNull<JSValue>,
    min_free_size: u32,
    n_rom_atom_tables: u8,
    string_pos_cache_counter: u8,
    class_count: u16,
    interrupt_counter: i16,
}

impl ContextShell {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        heap_base: NonNull<u8>,
        heap_free: NonNull<u8>,
        stack_top: NonNull<u8>,
        stack_bottom: NonNull<JSValue>,
        sp: NonNull<JSValue>,
        fp: NonNull<JSValue>,
        min_free_size: u32,
        n_rom_atom_tables: u8,
        string_pos_cache_counter: u8,
        class_count: u16,
        interrupt_counter: i16,
    ) -> Self {
        Self {
            heap_base,
            heap_free,
            stack_top,
            stack_bottom,
            sp,
            fp,
            min_free_size,
            n_rom_atom_tables,
            string_pos_cache_counter,
            class_count,
            interrupt_counter,
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub fn from_raw(
        heap_base: *mut u8,
        heap_free: *mut u8,
        stack_top: *mut u8,
        stack_bottom: *mut JSValue,
        sp: *mut JSValue,
        fp: *mut JSValue,
        min_free_size: u32,
        n_rom_atom_tables: u8,
        string_pos_cache_counter: u8,
        class_count: u16,
        interrupt_counter: i16,
    ) -> Option<Self> {
        Some(Self::new(
            NonNull::new(heap_base)?,
            NonNull::new(heap_free)?,
            NonNull::new(stack_top)?,
            NonNull::new(stack_bottom)?,
            NonNull::new(sp)?,
            NonNull::new(fp)?,
            min_free_size,
            n_rom_atom_tables,
            string_pos_cache_counter,
            class_count,
            interrupt_counter,
        ))
    }

    pub const fn heap_base(self) -> NonNull<u8> {
        self.heap_base
    }

    pub const fn heap_free(self) -> NonNull<u8> {
        self.heap_free
    }

    pub const fn stack_top(self) -> NonNull<u8> {
        self.stack_top
    }

    pub const fn stack_bottom(self) -> NonNull<JSValue> {
        self.stack_bottom
    }

    pub const fn sp(self) -> NonNull<JSValue> {
        self.sp
    }

    pub const fn fp(self) -> NonNull<JSValue> {
        self.fp
    }

    pub const fn min_free_size(self) -> u32 {
        self.min_free_size
    }

    pub const fn n_rom_atom_tables(self) -> u8 {
        self.n_rom_atom_tables
    }

    pub const fn string_pos_cache_counter(self) -> u8 {
        self.string_pos_cache_counter
    }

    pub const fn class_count(self) -> u16 {
        self.class_count
    }

    pub const fn interrupt_counter(self) -> i16 {
        self.interrupt_counter
    }

    pub fn is_valid(self) -> bool {
        let heap_base = self.heap_base.as_ptr() as usize;
        let heap_free = self.heap_free.as_ptr() as usize;
        let stack_top = self.stack_top.as_ptr() as usize;
        let stack_bottom = self.stack_bottom.as_ptr() as usize;
        let sp = self.sp.as_ptr() as usize;
        let fp = self.fp.as_ptr() as usize;

        heap_base <= heap_free
            && heap_free <= stack_top
            && stack_bottom <= sp
            && sp <= stack_top
            && stack_bottom <= fp
            && fp <= stack_top
    }
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    #[test]
    fn context_shell_roundtrip_and_validation() {
        let shell = ContextShell::new(
            NonNull::new(0x1000 as *mut u8).unwrap(),
            NonNull::new(0x1800 as *mut u8).unwrap(),
            NonNull::new(0x2000 as *mut u8).unwrap(),
            NonNull::new(0x1400 as *mut JSValue).unwrap(),
            NonNull::new(0x1800 as *mut JSValue).unwrap(),
            NonNull::new(0x1800 as *mut JSValue).unwrap(),
            64,
            1,
            0,
            10,
            -1,
        );
        assert_eq!(shell.heap_base().as_ptr(), 0x1000 as *mut u8);
        assert_eq!(shell.heap_free().as_ptr(), 0x1800 as *mut u8);
        assert_eq!(shell.stack_top().as_ptr(), 0x2000 as *mut u8);
        assert_eq!(shell.stack_bottom().as_ptr(), 0x1400 as *mut JSValue);
        assert_eq!(shell.sp().as_ptr(), 0x1800 as *mut JSValue);
        assert_eq!(shell.fp().as_ptr(), 0x1800 as *mut JSValue);
        assert_eq!(shell.min_free_size(), 64);
        assert_eq!(shell.n_rom_atom_tables(), 1);
        assert_eq!(shell.string_pos_cache_counter(), 0);
        assert_eq!(shell.class_count(), 10);
        assert_eq!(shell.interrupt_counter(), -1);
        assert!(shell.is_valid());
    }

    #[test]
    fn context_shell_invalid_heap_order() {
        let shell = ContextShell::new(
            NonNull::new(0x2000 as *mut u8).unwrap(),
            NonNull::new(0x1800 as *mut u8).unwrap(),
            NonNull::new(0x1000 as *mut u8).unwrap(),
            NonNull::new(0x1400 as *mut JSValue).unwrap(),
            NonNull::new(0x1800 as *mut JSValue).unwrap(),
            NonNull::new(0x1800 as *mut JSValue).unwrap(),
            64,
            1,
            0,
            10,
            -1,
        );
        assert!(!shell.is_valid());
    }
}
