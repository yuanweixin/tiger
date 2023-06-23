use std::num::NonZeroUsize;

use crate::symbol::{Symbol, Interner};
#[derive(Eq, PartialEq, Copy, Clone, Hash)]
pub struct Temp(NonZeroUsize);

#[derive(Eq, PartialEq, Copy, Clone, Hash)]
pub struct Label(Symbol);

pub struct TempAuthority {
    next_id : NonZeroUsize
}

impl TempAuthority {
    pub fn new() -> Self {
        Self {
            next_id : NonZeroUsize::MIN
        }
    }

    pub fn new_temp(&mut self) -> Temp {
        // This will panic if we wrap around. It is unlikely as we would have to exhaust
        // the size of a usize first. Practically impossible on 64bit ints.
        let t = Temp(self.next_id);
        self.next_id = NonZeroUsize::new(self.next_id.get().wrapping_add(1)).unwrap();
        t
    }
}

pub struct LabelAuthority {
    next_id : NonZeroUsize
}

impl LabelAuthority {
    pub fn new() -> Self {
        Self {
            next_id : NonZeroUsize::MIN
        }
    }

    pub fn new_label(&mut self, pool: &mut Interner) -> Label {
        let id = self.next_id.get();
        self.next_id = NonZeroUsize::new(self.next_id.get().wrapping_add(1)).unwrap();
        let sym = pool.intern(&format!("__label__{}", id));
        Label(sym)
    }
}