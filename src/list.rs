//! Intrusive list equivalent to `mquickjs-c/list.h`.
//!
//! Invariants carried over from the C implementation:
//! - Nodes embed their link and must not move while linked (use `Box`/`Pin`).
//! - A node can belong to at most one list at a time.
//! - Nodes must be unlinked before they are dropped.
//! - Traversal is forward/backward via the list links; the list is circular.

pub use intrusive_collections::{LinkedList, LinkedListLink, UnsafeRef, intrusive_adapter};

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    #[derive(Debug)]
    struct Node {
        link: LinkedListLink,
        value: i32,
    }

    impl Node {
        fn new(value: i32) -> Self {
            Self {
                link: LinkedListLink::new(),
                value,
            }
        }
    }

    intrusive_adapter!(NodeAdapter = UnsafeRef<Node>: Node { link: LinkedListLink });

    fn reclaim(node: UnsafeRef<Node>) {
        // SAFETY: the test owns all nodes, and each node is unlinked before reclamation.
        unsafe {
            drop(UnsafeRef::into_box(node));
        }
    }

    #[test]
    fn push_front_back_and_iterate() {
        let mut list = LinkedList::new(NodeAdapter::new());
        list.push_front(UnsafeRef::from_box(Box::new(Node::new(1))));
        list.push_back(UnsafeRef::from_box(Box::new(Node::new(2))));
        list.push_back(UnsafeRef::from_box(Box::new(Node::new(3))));

        let values: Vec<i32> = list.iter().map(|node| node.value).collect();
        assert_eq!(values, vec![1, 2, 3]);

        while let Some(node) = list.pop_front() {
            reclaim(node);
        }
        assert!(list.is_empty());
    }

    #[test]
    fn remove_while_iterating() {
        let mut list = LinkedList::new(NodeAdapter::new());
        for value in [1, 2, 3, 4] {
            list.push_back(UnsafeRef::from_box(Box::new(Node::new(value))));
        }

        let mut cursor = list.cursor_mut();
        cursor.move_next();
        while let Some(node) = cursor.get() {
            if node.value % 2 == 0 {
                let removed = cursor.remove().expect("cursor should point to a node");
                reclaim(removed);
            } else {
                cursor.move_next();
            }
        }

        let remaining: Vec<i32> = list.iter().map(|node| node.value).collect();
        assert_eq!(remaining, vec![1, 3]);

        while let Some(node) = list.pop_front() {
            reclaim(node);
        }
    }
}
