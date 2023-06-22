use crate::symbol::Symbol;
use std::collections::HashMap;

pub struct SymbolTable<T> {
    // The symbol table construct. A symbol maps to a list of LIFO entries.
    // In other words, if symbol for variable "a" is mapped to 5, then in
    // a nested scope is mapped to 6, the list should hold [5,6].
    tbl: HashMap<Symbol, Vec<T>>,
    // The stack is used, to track the symbols pushed in the current scope.
    // When the scope ends, this provides info on what symbols we need to
    // remove from the symbol table.
    stack: Vec<StackSymbol>,
}

#[derive(Copy, Clone)]
enum StackSymbol {
    // Needed to determine how many symbols to pop off.
    BeginScopeMarker,
    Sym(Symbol),
}

impl<T> SymbolTable<T> {
    pub fn empty() -> Self {
        SymbolTable {
            tbl: HashMap::new(),
            stack: Vec::new(),
        }
    }

    pub fn enter(&mut self, symbol: Symbol, v: T) {
        let existing = self.tbl.get(&symbol);
        if existing.is_none() {
            self.tbl.insert(symbol, Vec::new());
        }
        self.tbl.get_mut(&symbol).unwrap().push(v);
        self.stack.push(StackSymbol::Sym(symbol));
    }

    pub fn look(&self, symbol: Symbol) -> Option<&T> {
        if let Some(vec) = self.tbl.get(&symbol) {
            vec.last()
        } else {
            None
        }
    }

    pub fn begin_scope(&mut self) {
        self.stack.push(StackSymbol::BeginScopeMarker);
    }

    pub fn end_scope(&mut self) {
        while let Some(sym) = self.stack.last().copied() {
            match sym {
                StackSymbol::BeginScopeMarker => {
                    break;
                }
                StackSymbol::Sym(s) => {
                    self.stack.pop();
                    self.tbl.get_mut(&s).unwrap().pop();
                }
            }
        }
        // it is assumed the caller has correctly started the scope with a marker.
        self.stack.pop();
    }
}
