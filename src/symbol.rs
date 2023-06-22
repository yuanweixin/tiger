use string_interner::{DefaultBackend, DefaultSymbol, StringInterner};

#[derive(Eq, PartialEq, Hash, Copy, Clone)]
pub struct Symbol(DefaultSymbol);

pub struct Interner(StringInterner<DefaultBackend<DefaultSymbol>>);

impl Interner {
    pub fn intern(&mut self, name: &str) -> Symbol {
        Symbol(self.0.get_or_intern(name))
    }

    pub fn new() -> Self {
        Interner(StringInterner::new())
    }

    pub fn resolve(&self, s: &Symbol) -> Option<&str> {
        self.0.resolve(s.0)
    }
}

impl Symbol {
    pub fn name<'a>(&'a self, interner: &'a Interner) -> &str {
        // SAFETY: it is assumed that the caller provides a `Symbol`
        // obtained by calling `symbol` fn.
        interner.0.resolve(self.0).unwrap()
    }
}
