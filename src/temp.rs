use std::num::NonZeroUsize;
use std::{collections::HashMap, fmt::Display, hash::Hash};

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

/// The mapping of temporary to strings.
/// This is used for things like assigning temporaries to actual machine register names.
pub type TempMap = HashMap<Temp, &'static str>;

pub trait Uuids {
    fn new() -> Self
    where
        Self: Sized;

    fn resolve(&self, s: &Symbol) -> Option<&str>;

    fn resolve_label(&self, l: Label) -> Option<&str>;

    fn intern(&mut self, name: &str) -> Symbol;

    fn new_temp(&mut self) -> Temp;

    // Since the design went with not having temp generation at the global
    // level (i.e. can't do a lazy static and use that in the frame impl to
    // assign a temporary to each of the target register), the sln is to allow
    // "interning" of temporaries. the association is remembered and can be
    // retrieved into a temp map (temp to string) in another call. the main
    // purpose of this function is to allow associating temporary with strings.
    fn named_temp(&mut self, name: &'static str) -> Temp;

    // Given the list of temp names, return a temp map (temp -> str). If an existing
    // association exist, it is reused. Otherwise a new one is generated and placed
    // into the TempMap, with the side effect of also remembering that association
    // in the implementation.
    fn to_temp_map(&self, names: Vec<&'static str>) -> TempMap;

    fn new_label(&mut self) -> Label;

    fn named_label(&mut self, s: &str) -> Label;
}

// Approximates the ml modules given by Appel in flavor.
// Difference include: use of associated types, and the fact that symbol interning does not rely on global variables.

pub struct UuidsImpl {
    next_id: NonZeroUsize,
    pool: Interner,
    name_temp: HashMap<&'static str, Temp>,
}

impl Display for Temp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "t{}", self.0)
    }
}

impl Uuids for UuidsImpl {
    fn to_temp_map(&self, names: Vec<&'static str>) -> TempMap {
        todo!()
    }

    fn new() -> Self {
        Self {
            next_id: NonZeroUsize::MIN,
            pool: Interner::new(),
            name_temp: HashMap::new(),
        }
    }

    fn named_temp(&mut self, name: &'static str) -> Temp {
        if let Some(t) = self.name_temp.get(name) {
            return *t;
        }
        let t = self.new_temp();
        self.name_temp.insert(name, t);
        t
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

    fn new_label(&mut self) -> Label {
        let id = self.next_id.get();
        self.next_id = NonZeroUsize::new(self.next_id.get().wrapping_add(1)).unwrap();
        let sym = self.pool.intern(&format!("L{}", id));
        Label(sym)
    }
    fn named_label(&mut self, s: &str) -> Label {
        let sym = self.pool.intern(s);
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
