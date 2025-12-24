use core::ptr::{self, NonNull};

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TaggedPtr(*mut u8);

impl TaggedPtr {
    pub const fn from_bits(bits: usize) -> Self {
        Self(ptr::without_provenance_mut(bits))
    }

    pub fn addr(self) -> usize {
        self.0.addr()
    }

    pub fn from_ptr<T>(ptr: NonNull<T>, tag: usize, tag_mask: usize) -> Self {
        let raw = ptr.as_ptr().cast::<u8>();
        let addr = raw.addr();
        debug_assert!((addr & tag_mask) == 0);
        let tagged = raw.map_addr(|addr| addr | tag);
        Self(tagged)
    }

    pub fn to_ptr<T>(self, tag: usize, tag_mask: usize) -> Option<NonNull<T>> {
        let addr = self.0.addr();
        if (addr & tag_mask) != tag {
            return None;
        }
        let untagged = self.0.with_addr(addr & !tag_mask);
        NonNull::new(untagged.cast::<T>())
    }
}
