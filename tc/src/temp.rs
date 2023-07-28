use std::collections::HashSet;
use std::num::NonZeroUsize;
use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    hash::Hash,
};

use crate::{
    frame,
    symbol::{Interner, Symbol},
    symtab::SymbolTable,
};

// most temp are unnamed, only the registers are named
// the extra cost is an extra word. otoh this makes it
// clearer what "kind" a temporary is.
#[derive(Eq, PartialEq, Copy, Clone, Hash)]
pub enum Temp {
    Named(Symbol),
    Unnamed(NonZeroUsize),
}

#[derive(Eq, PartialEq, Copy, Clone, Hash)]
pub enum Label {
    Named(Symbol),
    Unnamed(NonZeroUsize),
}

impl Temp {
    // Need this to properly format named temporaries.
    pub fn debug_to_string(&self, tm: &TempMap) -> String {
        if let Some(s) = tm.get(self) {
            String::from(*s)
        } else {
            // fall back to the Debug impl
            // which should output t123, etc.
            match self {
                Temp::Named(..) => unreachable!(), // it's a bug if this happens
                Temp::Unnamed(id) => format!("_t{}", id)
            }
        }
    }
}

impl Label {
    pub fn debug_to_string(&self, gen: &dyn Uuids) -> String {
        match self {
            Label::Unnamed(id) => format!(".L{}", id),
            Label::Named(sym) => format!("{}", gen.resolve(&sym).unwrap()),
        }
    }

    pub fn resolve_named_label<'a>(&self, gen: &'a dyn Uuids) -> &'a str {
        match self {
            Label::Unnamed(..) => panic!("impl bug: expected named label"),

            Label::Named(sym) => gen.resolve(&sym).unwrap(),
        }
    }
}

/// The mapping of temporary to strings.
/// This is used for things like assigning temporaries to actual machine register names.
#[derive(Debug)]
pub struct TempMap(HashMap<Temp, &'static str>);

impl TempMap {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn get(&self, t: &Temp) -> Option<&&str> {
        self.0.get(t)
    }

    pub fn contains_key(&self, t: &Temp) -> bool {
        self.0.contains_key(t)
    }

    pub fn insert(&mut self, t: Temp, v: &'static str) {
        self.0.insert(t, v);
    }
}

pub trait Uuids {
    fn new() -> Self
    where
        Self: Sized;

    fn resolve(&self, s: &Symbol) -> Option<&str>;

    fn intern(&mut self, name: &str) -> Symbol;

    fn new_unnamed_temp(&mut self) -> Temp;

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
    fn to_temp_map(&mut self, names: &[frame::Register]) -> TempMap;

    fn new_unnamed_label(&mut self) -> Label;

    fn named_label(&mut self, s: &str) -> Label;
}

// Approximates the ml modules given by Appel in flavor.
// Difference include: use of associated types, and the fact that symbol interning does not rely on global variables.

pub struct UuidsImpl {
    next_id: NonZeroUsize,
    pool: Interner,
    name_temp: SymbolTable<Temp>,
    pub named_labels: HashSet<Label>, // this exists purely for debug so we could print out the mappings.
}

impl Debug for Temp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Temp::Named(s) => f.write_fmt(format_args!("named_tmp{}", s.to_usize())),
            Temp::Unnamed(id) => f.write_fmt(format_args!("t{}", id)),
        }
    }
}

impl Debug for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Label::Named(s) => f.write_fmt(format_args!("named_label{}", s.to_usize())),
            Label::Unnamed(id) => f.write_fmt(format_args!(".L{}", id)),
        }
    }
}

impl Display for Temp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Temp::Named(sym) => write!(f, "nt_sym_{}", sym.to_usize()),
            Temp::Unnamed(id) => write!(f, "t{}", id),
        }
    }
}

impl Uuids for UuidsImpl {
    fn to_temp_map(&mut self, names: &[frame::Register]) -> TempMap {
        let mut tm = TempMap::new();
        for name in names {
            let t = self.named_temp(name);
            tm.insert(t, name);
        }
        tm
    }

    fn new() -> Self {
        Self {
            next_id: NonZeroUsize::MIN,
            pool: Interner::new(),
            name_temp: SymbolTable::empty(),
            named_labels: HashSet::new(),
        }
    }

    fn named_temp(&mut self, name: &'static str) -> Temp {
        let sym = self.pool.intern(name);
        match self.name_temp.look(sym) {
            Some(t) => *t,
            None => {
                let t = Temp::Named(sym);
                self.name_temp.enter(sym, t);
                t
            }
        }
    }

    #[inline]
    fn resolve(&self, s: &Symbol) -> Option<&str> {
        self.pool.resolve(s)
    }

    #[inline]
    fn intern(&mut self, name: &str) -> Symbol {
        self.pool.intern(name)
    }

    fn new_unnamed_temp(&mut self) -> Temp {
        let t = Temp::Unnamed(self.next_id);
        self.next_id = NonZeroUsize::new(self.next_id.get().wrapping_add(1)).unwrap();
        t
    }

    fn new_unnamed_label(&mut self) -> Label {
        let id = self.next_id;
        self.next_id = NonZeroUsize::new(self.next_id.get().wrapping_add(1)).unwrap();
        Label::Unnamed(id)
    }
    fn named_label(&mut self, s: &str) -> Label {
        let sym = self.pool.intern(s);
        self.named_labels.insert(Label::Named(sym));
        Label::Named(sym)
    }
}

pub mod test_helpers {
    use super::*;

    pub fn new_unnamed_temp(s: usize) -> Temp {
        Temp::Unnamed(NonZeroUsize::new(s).unwrap())
    }

    pub fn new_unnamed_label(s: usize) -> Label {
        Label::Unnamed(NonZeroUsize::new(s).unwrap())
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
