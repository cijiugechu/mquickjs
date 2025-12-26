use crate::atom::AtomTables;
use crate::gc::{gc_mark_all, gc_sweep, GcMarkConfig, GcRoots, GcSweepFinalizers};
use crate::gc_ref::GcRefState;
use crate::heap::{compact_heap, HeapLayout, RootVisitor};
use crate::jsvalue::JSValue;
use crate::parser::parse_state::JSParseState;
use crate::string::string_pos_cache::{StringPosCacheEntry, JS_STRING_POS_CACHE_SIZE};

// Invariants:
// - The heap layout must describe a valid, contiguous heap buffer.
// - Root slots must be valid JSValue locations for the duration of the GC cycle.
// - Heap pointers in roots must refer to live memblocks; non-heap pointers are ignored.
pub struct GcRuntimeRoots<'a> {
    class_roots: &'a mut [JSValue],
    stack_roots: &'a mut [JSValue],
    gc_refs: Option<&'a GcRefState>,
    parse_state: Option<&'a mut JSParseState>,
    atom_tables: Option<&'a mut AtomTables>,
    string_pos_cache: Option<&'a mut [StringPosCacheEntry; JS_STRING_POS_CACHE_SIZE]>,
}

impl<'a> GcRuntimeRoots<'a> {
    pub fn new(class_roots: &'a mut [JSValue], stack_roots: &'a mut [JSValue]) -> Self {
        Self {
            class_roots,
            stack_roots,
            gc_refs: None,
            parse_state: None,
            atom_tables: None,
            string_pos_cache: None,
        }
    }

    pub fn with_gc_refs(mut self, refs: &'a GcRefState) -> Self {
        self.gc_refs = Some(refs);
        self
    }

    pub fn with_parse_state(mut self, state: &'a mut JSParseState) -> Self {
        self.parse_state = Some(state);
        self
    }

    pub fn with_atom_tables(mut self, tables: &'a mut AtomTables) -> Self {
        self.atom_tables = Some(tables);
        self
    }

    pub fn with_string_pos_cache(
        mut self,
        cache: &'a mut [StringPosCacheEntry; JS_STRING_POS_CACHE_SIZE],
    ) -> Self {
        self.string_pos_cache = Some(cache);
        self
    }
}

impl RootVisitor for GcRuntimeRoots<'_> {
    fn visit_roots<F>(&mut self, mut f: F)
    where
        F: FnMut(*mut JSValue),
    {
        for val in self.class_roots.iter_mut() {
            f(val as *mut JSValue);
        }
        for val in self.stack_roots.iter_mut() {
            f(val as *mut JSValue);
        }
        if let Some(refs) = self.gc_refs {
            refs.visit_root_slots(&mut f);
        }
        if let Some(state) = self.parse_state.as_deref_mut() {
            for slot in state.gc_root_slots_mut() {
                f(slot);
            }
        }
        if let Some(cache) = self.string_pos_cache.as_deref_mut() {
            for entry in cache.iter_mut() {
                f(entry.str_ptr());
            }
        }
        if let Some(tables) = self.atom_tables.as_deref_mut() {
            tables.visit_root_slots(&mut f);
        }
    }
}

/// # Safety
/// The heap must be valid and roots must remain stable for the duration of the collection.
pub unsafe fn gc_collect(
    heap: &mut HeapLayout,
    roots: &mut GcRuntimeRoots<'_>,
    mark_config: GcMarkConfig,
    finalizers: Option<&GcSweepFinalizers<'_>>,
) {
    {
        let mut mark_roots = GcRoots::new(&*roots.class_roots, &*roots.stack_roots);
        if let Some(refs) = roots.gc_refs {
            mark_roots = mark_roots.with_gc_refs(refs);
        }
        if let Some(state) = roots.parse_state.as_deref() {
            mark_roots = mark_roots.with_parse_state(state);
        }
        if let Some(tables) = roots.atom_tables.as_deref_mut() {
            mark_roots = mark_roots.with_atom_tables(tables);
        }
        if let Some(cache) = roots.string_pos_cache.as_deref_mut() {
            mark_roots = mark_roots.with_string_pos_cache(cache);
        }
        gc_mark_all(heap, &mut mark_roots, mark_config);
    }

    unsafe {
        // SAFETY: caller guarantees the heap layout is valid.
        gc_sweep(heap, finalizers);
        // SAFETY: caller guarantees roots and heap are valid for compaction.
        compact_heap(heap, roots);
    }

    if let Some(state) = roots.parse_state.as_deref_mut() {
        state.refresh_source_buf_from_str();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::containers::ByteArrayHeader;
    use crate::gc_ref::GcRef;
    use crate::heap::mblock_size;
    use crate::jsvalue::{value_from_ptr, value_to_ptr, JSValue, JSWord, JSW, JS_NULL};
    use crate::memblock::{MbHeader, MTag};
    use core::ptr::{self, NonNull};

    struct HeapArena {
        _storage: Vec<JSWord>,
        layout: HeapLayout,
    }

    impl HeapArena {
        fn new(words: usize, stack_words: usize) -> Self {
            let mut storage = vec![0 as JSWord; words];
            let base = NonNull::new(storage.as_mut_ptr() as *mut u8).unwrap();
            let stack_top =
                unsafe { NonNull::new_unchecked(base.as_ptr().add(words * JSW as usize)) };
            let stack_bottom = unsafe {
                NonNull::new_unchecked(
                    stack_top
                        .as_ptr()
                        .sub(stack_words * JSW as usize) as *mut JSValue,
                )
            };
            let layout = HeapLayout::new(base, base, stack_top, stack_bottom, 0);
            Self {
                _storage: storage,
                layout,
            }
        }
    }

    unsafe fn write_header(ptr: NonNull<u8>, header: MbHeader) {
        unsafe {
            ptr::write_unaligned(ptr.as_ptr().cast::<JSWord>(), header.word());
        }
    }

    fn alloc_byte_array(arena: &mut HeapArena, len: JSWord) -> NonNull<u8> {
        let size = crate::containers::byte_array_alloc_size(len);
        let ptr = arena
            .layout
            .malloc(size, MTag::ByteArray, |_| {})
            .expect("alloc byte array");
        let header = ByteArrayHeader::new(len, false);
        unsafe {
            write_header(ptr, MbHeader::from(header));
        }
        ptr
    }

    #[test]
    fn gc_collect_compacts_and_updates_gc_refs() {
        let mut arena = HeapArena::new(256, 16);
        let live1_ptr = alloc_byte_array(&mut arena, 3);
        let dead_ptr = alloc_byte_array(&mut arena, 5);
        let live2_ptr = alloc_byte_array(&mut arena, 4);

        let live1_val = value_from_ptr(live1_ptr);
        let live2_val = value_from_ptr(live2_ptr);

        let live1_size = unsafe { mblock_size(live1_ptr) };
        let live2_size = unsafe { mblock_size(live2_ptr) };

        let mut stack_roots = [live1_val];
        let mut class_roots: [JSValue; 0] = [];

        let mut refs = GcRefState::new();
        let mut reference = Box::new(GcRef::new(JS_NULL));
        let slot = refs.add_gc_ref(reference.as_mut());
        unsafe {
            *slot = live2_val;
        }

        let mut roots = GcRuntimeRoots::new(&mut class_roots, &mut stack_roots).with_gc_refs(&refs);
        let dead_addr = dead_ptr.as_ptr();

        unsafe {
            gc_collect(&mut arena.layout, &mut roots, GcMarkConfig::default(), None);
        }

        assert_eq!(stack_roots[0], live1_val);
        let updated = reference.val();
        let updated_ptr = value_to_ptr::<u8>(updated).expect("gc ref updated");
        assert_eq!(updated_ptr.as_ptr(), dead_addr);

        let expected_free = unsafe {
            arena
                .layout
                .heap_base()
                .as_ptr()
                .add(live1_size + live2_size)
        };
        assert_eq!(arena.layout.heap_free().as_ptr(), expected_free);

        refs.delete_gc_ref(&reference);
    }
}
