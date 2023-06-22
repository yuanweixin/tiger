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
}

impl Symbol {
    pub fn name<'a>(&'a self, interner: &'a Interner) -> &str {
        // SAFETY: it is assumed that the caller provides a `Symbol`
        // obtained by calling `symbol` fn.
        interner.0.resolve(self.0).unwrap()
    }
}
