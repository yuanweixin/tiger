use std::hash::Hash;
use std::num::NonZeroUsize;

use crate::symbol::{Interner, Symbol};

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug)]
pub struct Temp(NonZeroUsize);

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug)]
pub struct Label(Symbol);

// Approximates the ml modules given by Appel in flavor.
// Difference include: use of associated types, and the fact that symbol interning does not rely on global variables.

pub struct GenTemporary {
    next_id: NonZeroUsize,
    pool: Interner
}

impl GenTemporary {
    pub fn new() -> Self {
        Self {
            next_id: NonZeroUsize::MIN,
            pool : Interner::new()
        }
    }

    #[inline]
    pub fn resolve(&self, s: &Symbol) -> Option<&str> {
        self.pool.resolve(s)
    }

    #[inline]
    pub fn resolve_label(&self, l: Label) -> Option<&str> {
        self.resolve(&l.0)
    }

    #[inline]
    pub fn intern(&mut self, name: &str) -> Symbol {
        self.pool.intern(name)
    }

    pub fn new_temp(&mut self) -> Temp {
        let t = Temp(self.next_id);
        self.next_id = NonZeroUsize::new(self.next_id.get().wrapping_add(1)).unwrap();
        t
    }

    pub fn make_string(temp: Temp) -> String {
        format!("__temp_{}", temp.0)
    }

    pub fn new_label(&mut self) -> Label {
        let id = self.next_id.get();
        self.next_id = NonZeroUsize::new(self.next_id.get().wrapping_add(1)).unwrap();
        let sym = self.pool.intern(&format!("__label__{}", id));
        Label(sym)
    }
    pub fn named_label(s: &str, pool: &mut Interner) -> Label {
        let sym = pool.intern(s);
        Label(sym)
    }
}
