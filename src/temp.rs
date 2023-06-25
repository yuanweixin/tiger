use std::hash::Hash;
use std::num::NonZeroUsize;

use crate::symbol::{Interner, Symbol};

#[derive(Eq, PartialEq, Copy, Clone, Hash)]
pub struct Temp(NonZeroUsize);

#[derive(Eq, PartialEq, Copy, Clone, Hash)]
pub struct Label(Symbol);

// Approximates the ml modules given by Appel in flavor.
// Difference include: use of associated types, and the fact that symbol interning does not rely on global variables.

pub struct GenTemporary {
    next_id: NonZeroUsize
}

impl GenTemporary {
    pub fn new() -> Self {
        Self {
            next_id: NonZeroUsize::MIN,
        }
    }

    pub fn new_temp(&mut self) -> Temp {
        let t = Temp(self.next_id);
        self.next_id = NonZeroUsize::new(self.next_id.get().wrapping_add(1)).unwrap();
        t
    }

    pub fn make_string(temp: Temp) -> String {
        format!("__temp_{}", temp.0)
    }

    pub fn new_label(&mut self, pool: &mut Interner) -> Label {
        let id = self.next_id.get();
        self.next_id = NonZeroUsize::new(self.next_id.get().wrapping_add(1)).unwrap();
        let sym = pool.intern(&format!("__label__{}", id));
        Label(sym)
    }
    pub fn named_label(s: &str, pool: &mut Interner) -> Label {
        let sym = pool.intern(s);
        Label(sym)
    }
}
