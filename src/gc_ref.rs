use crate::jsvalue::{JSValue, JS_UNDEFINED};
use core::cell::UnsafeCell;
use crate::list::{LinkedList, LinkedListLink, UnsafeRef, intrusive_adapter};

// C: `JSGCRef` in mquickjs.h.
//
// Rust-idiomatic note: use `intrusive-collections` lists to avoid hand-rolled
// pointer manipulation while still modeling intrusive GC reference lists.
pub struct GcRef {
    stack_link: LinkedListLink,
    list_link: LinkedListLink,
    val: UnsafeCell<JSValue>,
}

impl GcRef {
    pub fn new(val: JSValue) -> Self {
        Self {
            stack_link: LinkedListLink::new(),
            list_link: LinkedListLink::new(),
            val: UnsafeCell::new(val),
        }
    }

    pub fn val(&self) -> JSValue {
        // SAFETY: interior mutability is intentional for GC root slots.
        unsafe { *self.val.get() }
    }

    pub fn set_val(&self, val: JSValue) {
        // SAFETY: interior mutability is intentional for GC root slots.
        unsafe {
            *self.val.get() = val;
        }
    }

    pub fn val_ptr(&self) -> *mut JSValue {
        self.val.get()
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

    /// C: `JS_PushGCRef`. Caller must keep `reference` pinned while linked.
    pub fn push_gc_ref(&mut self, reference: &mut GcRef) -> *mut JSValue {
        reference.set_val(JS_UNDEFINED);
        let val_ptr = reference.val_ptr();
        // SAFETY: caller guarantees `reference` is pinned for the duration of the list membership.
        let node = unsafe { UnsafeRef::from_raw(reference) };
        self.stack.push_front(node);
        val_ptr
    }

    /// C: `JS_PopGCRef`. `reference` must be the current stack top.
    pub fn pop_gc_ref(&mut self, reference: &GcRef) -> JSValue {
        let top = self
            .stack
            .front()
            .get()
            .map(|node| node as *const GcRef);
        debug_assert_eq!(top, Some(reference as *const GcRef));
        let popped = self.stack.pop_front().expect("GC ref stack underflow");
        let popped_ref = popped.as_ref();
        debug_assert!(core::ptr::eq(popped_ref, reference));
        popped_ref.val()
    }

    /// C: `JS_AddGCRef`. Caller must keep `reference` pinned while linked.
    pub fn add_gc_ref(&mut self, reference: &mut GcRef) -> *mut JSValue {
        reference.set_val(JS_UNDEFINED);
        let val_ptr = reference.val_ptr();
        // SAFETY: caller guarantees `reference` is pinned for the duration of the list membership.
        let node = unsafe { UnsafeRef::from_raw(reference) };
        self.list.push_front(node);
        val_ptr
    }

    /// # Safety
    /// `reference` must point to a node currently linked in `list`.
    pub unsafe fn remove_list(&mut self, reference: *const GcRef) -> Option<UnsafeRef<GcRef>> {
        // SAFETY: caller must pass a node currently linked in `list`.
        unsafe { self.list.cursor_mut_from_ptr(reference).remove() }
    }

    /// C: `JS_DeleteGCRef`. Panics if `reference` is not linked in the list.
    pub fn delete_gc_ref(&mut self, reference: &GcRef) {
        let mut cursor = self.list.front_mut();
        while let Some(node) = cursor.get() {
            if core::ptr::eq(node, reference) {
                cursor.remove();
                return;
            }
            cursor.move_next();
        }
        panic!("GC ref not found in list");
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jsvalue::{JS_FALSE, JS_TRUE};

    #[test]
    fn push_pop_gc_ref_roundtrips_value() {
        let mut state = GcRefState::new();
        let mut reference = GcRef::new(JS_FALSE);
        let slot = state.push_gc_ref(&mut reference);
        // SAFETY: slot points to the ref value while the ref is linked.
        unsafe {
            assert_eq!(*slot, JS_UNDEFINED);
            *slot = JS_TRUE;
        }
        let val = state.pop_gc_ref(&reference);
        assert_eq!(val, JS_TRUE);
        assert!(state.is_stack_empty());
    }

    #[test]
    fn add_delete_gc_ref_updates_list() {
        let mut state = GcRefState::new();
        let mut reference = GcRef::new(JS_FALSE);
        let slot = state.add_gc_ref(&mut reference);
        // SAFETY: slot points to the ref value while the ref is linked.
        unsafe {
            *slot = JS_TRUE;
        }
        assert!(!state.is_list_empty());
        state.delete_gc_ref(&reference);
        assert!(state.is_list_empty());
    }

    #[test]
    #[should_panic(expected = "GC ref not found in list")]
    fn delete_missing_ref_panics() {
        let mut state = GcRefState::new();
        let reference = GcRef::new(JS_FALSE);
        state.delete_gc_ref(&reference);
    }
}
