use crate::symbol::Symbol;

#[derive(Eq, PartialEq, Copy, Clone, Hash)]
pub struct Temp(usize);

#[derive(Eq, PartialEq, Copy, Clone, Hash)]
pub struct Label(Symbol);
