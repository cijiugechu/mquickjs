use core::mem::size_of;
use core::ptr::{self, NonNull};

use crate::heap::HeapLayout;
use crate::jsvalue::{value_from_ptr, JSValue, JSWord, JSW};
use crate::memblock::MTag;

const DEFAULT_HEAP_WORDS: usize = 4096;

fn is_negative_zero(value: f64) -> bool {
    value == 0.0 && value.is_sign_negative()
}

/// Minimal heap-backed allocator for parser-owned numeric constants.
///
/// TODO: Replace with real JSRuntime/GC allocation once available. Parser
/// constants must become GC-managed roots and participate in compaction.
pub struct ParserRuntime {
    #[allow(dead_code)]
    heap_storage: Vec<JSWord>, // keeps heap backing storage alive
    heap: HeapLayout,
    minus_zero: Option<JSValue>,
}

impl ParserRuntime {
    pub fn new_default() -> Self {
        Self::new(DEFAULT_HEAP_WORDS)
    }

    pub fn new(words: usize) -> Self {
        assert!(words > 0, "parser heap must be non-empty");
        let mut heap_storage = vec![0 as JSWord; words];
        let base = NonNull::new(heap_storage.as_mut_ptr() as *mut u8)
            .expect("heap storage must be non-null");
        let bytes = words * JSW as usize;
        let stack_top = unsafe {
            // SAFETY: heap_storage has `bytes` writable bytes.
            NonNull::new_unchecked(base.as_ptr().add(bytes))
        };
        let stack_bottom = NonNull::new(stack_top.as_ptr() as *mut JSValue)
            .expect("stack bottom must be non-null");
        let heap = HeapLayout::new(base, base, stack_top, stack_bottom, 0);
        Self {
            heap_storage,
            heap,
            minus_zero: None,
        }
    }

    /// Mirrors C __JS_NewFloat64: caller ensures `value` is not a short int.
    pub fn new_float64(&mut self, value: f64) -> Option<JSValue> {
        if is_negative_zero(value) {
            return self.minus_zero();
        }
        #[cfg(target_pointer_width = "64")]
        {
            let abs = value.abs();
            let min = 2.0_f64.powi(-127);
            let max = 2.0_f64.powi(128);
            if abs >= min && abs <= max {
                return Some(crate::jsvalue::short_float_from_f64(value));
            }
        }
        self.alloc_float64(value)
    }

    fn minus_zero(&mut self) -> Option<JSValue> {
        if let Some(val) = self.minus_zero {
            return Some(val);
        }
        let val = self.alloc_float64(-0.0)?;
        self.minus_zero = Some(val);
        Some(val)
    }

    fn alloc_float64(&mut self, value: f64) -> Option<JSValue> {
        let size = size_of::<JSWord>() + size_of::<f64>();
        let ptr = self.heap.malloc(size, MTag::Float64, |_| {
            // TODO: trigger GC/compaction once runtime is wired.
        })?;
        let payload = unsafe {
            // SAFETY: allocation returned at least `size` bytes.
            ptr.as_ptr().add(size_of::<JSWord>()) as *mut f64
        };
        unsafe {
            // SAFETY: payload points within the allocated float64 memblock.
            ptr::write_unaligned(payload, value);
        }
        Some(value_from_ptr(ptr))
    }
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;
    use crate::jsvalue::{is_ptr, is_short_float, value_to_ptr};

    unsafe fn read_float(ptr: NonNull<u8>) -> f64 {
        let payload = unsafe { ptr.as_ptr().add(size_of::<JSWord>()) as *const f64 };
        // SAFETY: payload points within a float64 memblock.
        unsafe { ptr::read_unaligned(payload) }
    }

    #[test]
    fn new_float64_allocates_and_stores_value() {
        let mut rt = ParserRuntime::new(256);
        let value = 1.0e300;
        let val = rt.new_float64(value).expect("alloc");
        assert!(is_ptr(val));
        let ptr = value_to_ptr::<u8>(val).expect("ptr");
        let stored = unsafe { read_float(ptr) };
        assert_eq!(stored, value);
    }

    #[test]
    fn new_float64_caches_minus_zero() {
        let mut rt = ParserRuntime::new(64);
        let first = rt.new_float64(-0.0).expect("alloc");
        let second = rt.new_float64(-0.0).expect("alloc");
        assert_eq!(first, second);
        let ptr = value_to_ptr::<u8>(first).expect("ptr");
        let stored = unsafe { read_float(ptr) };
        assert_eq!(stored.to_bits(), (-0.0f64).to_bits());
    }

    #[cfg(target_pointer_width = "64")]
    #[test]
    fn new_float64_uses_short_float_in_range() {
        let mut rt = ParserRuntime::new(32);
        let val = rt.new_float64(1.5).expect("alloc");
        assert!(is_short_float(val));
    }

    #[cfg(target_pointer_width = "32")]
    #[test]
    fn new_float64_allocates_on_32bit() {
        let mut rt = ParserRuntime::new(64);
        let val = rt.new_float64(1.5).expect("alloc");
        assert!(is_ptr(val));
    }
}
