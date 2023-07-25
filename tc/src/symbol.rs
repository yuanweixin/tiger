use string_interner::{DefaultBackend, DefaultSymbol, StringInterner};

#[derive(Eq, PartialEq, Hash, Copy, Clone, Debug)]
pub struct Symbol(DefaultSymbol);

impl Symbol {
    #[inline]
    pub fn to_usize(&self) -> usize {
        string_interner::Symbol::to_usize(self.0)
    }
}

pub struct Interner(StringInterner<DefaultBackend<DefaultSymbol>>);

impl Interner {
    #[inline]
    pub fn intern(&mut self, name: &str) -> Symbol {
        Symbol(self.0.get_or_intern(name))
    }

    pub fn new() -> Self {
        Interner(StringInterner::new())
    }

    #[inline]
    pub fn resolve(&self, s: &Symbol) -> Option<&str> {
        self.0.resolve(s.0)
    }
}

pub mod test_helpers {
    use std::num::NonZeroUsize;
    use string_interner::Symbol;
    use super::*;

    #[inline]
    pub fn new_symbol(s: NonZeroUsize) -> super::Symbol {
        super::Symbol(DefaultSymbol::try_from_usize(s.get()).unwrap())
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn intern_gives_back_existing_symbol() {
        let mut i = Interner::new();
        let s1 = i.intern("hello");
        let s2 = i.intern("hello");
        assert_eq!(s1, s2);
    }

    #[test]
    fn test_resolve() {
        let mut i = Interner::new();
        let s1 = i.intern("hello");
        assert_eq!(Some("hello"), i.resolve(&s1));
    }
}
