use std::{hash::Hash, collections::HashMap};
use std::num::NonZeroUsize;

use crate::symbol::{Interner, Symbol};

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug)]
pub struct Temp(NonZeroUsize);
#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug)]
pub struct Label(Symbol);

impl PartialEq<Label> for &Label {
    fn eq(&self, rhs: &Label) -> bool {
        return self.0 == rhs.0;
    }
}

impl PartialEq<&Label> for Label {
    fn eq(&self, rhs: &&Label) -> bool {
        return self.0 == rhs.0;
    }
}

pub trait Uuids {
    fn new() -> Self
    where
        Self: Sized;

    fn resolve(&self, s: &Symbol) -> Option<&str>;

    fn resolve_label(&self, l: Label) -> Option<&str>;

    fn intern(&mut self, name: &str) -> Symbol;

    fn new_temp(&mut self) -> Temp;

    // Most of Temp's are not associated with a string, so they
    // don't need the Symbol treatment. But some of them do (for
    // instance, the named registers of a target architecture).
    // So we will just provide a way to look that up. The output
    // of multiple calls with the same name should be identical.
    fn named_temp(&mut self, name: &'static str) -> Temp;

    // TODO what's this used for?
    fn make_string(temp: Temp) -> String
    where
        Self: Sized;

    fn new_label(&mut self) -> Label;

    // TODO what's this used for?
    fn named_label(s: &str, pool: &mut Interner) -> Label
    where
        Self: Sized;
}

// Approximates the ml modules given by Appel in flavor.
// Difference include: use of associated types, and the fact that symbol interning does not rely on global variables.

pub struct UuidsImpl {
    next_id: NonZeroUsize,
    pool: Interner,
    named_temps : HashMap<&'static str, Temp>
}

impl Uuids for UuidsImpl {
    fn new() -> Self {
        Self {
            next_id: NonZeroUsize::MIN,
            pool: Interner::new(),
            named_temps : HashMap::new()
        }
    }

    fn named_temp(&mut self, name: &'static str) -> Temp
    {
        let x = self.named_temps.get(name);
        if x.is_none() {
            let t = self.new_temp();
            self.named_temps.insert(name, t);
            return t;
        }
        *x.unwrap()
    }

    #[inline]
    fn resolve(&self, s: &Symbol) -> Option<&str> {
        self.pool.resolve(s)
    }

    #[inline]
    fn resolve_label(&self, l: Label) -> Option<&str> {
        self.resolve(&l.0)
    }

    #[inline]
    fn intern(&mut self, name: &str) -> Symbol {
        self.pool.intern(name)
    }

    fn new_temp(&mut self) -> Temp {
        let t = Temp(self.next_id);
        self.next_id = NonZeroUsize::new(self.next_id.get().wrapping_add(1)).unwrap();
        t
    }

    fn make_string(temp: Temp) -> String {
        format!("__temp_{}", temp.0)
    }

    fn new_label(&mut self) -> Label {
        let id = self.next_id.get();
        self.next_id = NonZeroUsize::new(self.next_id.get().wrapping_add(1)).unwrap();
        let sym = self.pool.intern(&format!("L{}", id));
        Label(sym)
    }
    fn named_label(s: &str, pool: &mut Interner) -> Label {
        let sym = pool.intern(s);
        Label(sym)
    }
}

pub mod test_helpers {
    use super::*;
    use crate::symbol;

    pub fn new_temp(s: usize) -> Temp {
        Temp(NonZeroUsize::new(s).unwrap())
    }

    // NOTE: the underlying library increments the passed in usize by 1,
    // so in test code should expect the output to have a value s+1
    pub fn new_label(s: usize) -> Label {
        Label(symbol::test_helpers::new_symbol(
            NonZeroUsize::new(s).unwrap(),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn named_temp_multiple_call_return_same() {
        let mut gen = UuidsImpl::new();
        let t1 = gen.named_temp("hello");
        let t2 = gen.named_temp("hello");
        let t3 = gen.named_temp("hello");
        assert_eq!(t1, t2);
        assert_eq!(t2, t3);
        assert_eq!(t1, t3);
    }
}
