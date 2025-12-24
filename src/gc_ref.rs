use crate::jsvalue::{JSValue, JS_UNDEFINED};
use crate::list::{LinkedList, LinkedListLink, UnsafeRef, intrusive_adapter};

// C: `JSGCRef` in mquickjs.h.
//
// Rust-idiomatic note: use `intrusive-collections` lists to avoid hand-rolled
// pointer manipulation while still modeling intrusive GC reference lists.
pub struct GcRef {
    stack_link: LinkedListLink,
    list_link: LinkedListLink,
    val: JSValue,
}

impl GcRef {
    pub fn new(val: JSValue) -> Self {
        Self {
            stack_link: LinkedListLink::new(),
            list_link: LinkedListLink::new(),
            val,
        }
    }

    pub const fn val(&self) -> JSValue {
        self.val
    }
}

intrusive_adapter!(GcRefStackAdapter = UnsafeRef<GcRef>: GcRef { stack_link: LinkedListLink });
intrusive_adapter!(GcRefListAdapter = UnsafeRef<GcRef>: GcRef { list_link: LinkedListLink });

// C: `JSContext::top_gc_ref`/`last_gc_ref` lists used by JS_*GCRef helpers.
pub struct GcRefState {
    stack: LinkedList<GcRefStackAdapter>,
    list: LinkedList<GcRefListAdapter>,
}

impl GcRefState {
    pub fn new() -> Self {
        Self {
            stack: LinkedList::new(GcRefStackAdapter::new()),
            list: LinkedList::new(GcRefListAdapter::new()),
        }
    }

    pub fn push_stack(&mut self, reference: &mut GcRef) {
        reference.val = JS_UNDEFINED;
        // SAFETY: caller guarantees `reference` is pinned for the duration of the list membership.
        let node = unsafe { UnsafeRef::from_raw(reference) };
        self.stack.push_front(node);
    }

    pub fn pop_stack(&mut self) -> Option<UnsafeRef<GcRef>> {
        self.stack.pop_front()
    }

    pub fn add_list(&mut self, reference: &mut GcRef) {
        reference.val = JS_UNDEFINED;
        // SAFETY: caller guarantees `reference` is pinned for the duration of the list membership.
        let node = unsafe { UnsafeRef::from_raw(reference) };
        self.list.push_front(node);
    }

    /// # Safety
    /// `reference` must point to a node currently linked in `list`.
    pub unsafe fn remove_list(&mut self, reference: *const GcRef) -> Option<UnsafeRef<GcRef>> {
        // SAFETY: caller must pass a node currently linked in `list`.
        unsafe { self.list.cursor_mut_from_ptr(reference).remove() }
    }

    pub fn is_stack_empty(&self) -> bool {
        self.stack.is_empty()
    }

    pub fn is_list_empty(&self) -> bool {
        self.list.is_empty()
    }
}

impl Default for GcRefState {
    fn default() -> Self {
        Self::new()
    }
}
