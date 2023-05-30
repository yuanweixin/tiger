// Lifted from https://github.com/andy-hanson/rust-symbol/blob/master/src/lib.rs

use std::collections::{HashMap};

#[allow(dead_code)]
pub struct SymbolTable<'a, S : Symbol<'a>> {
	pub map: HashMap<&'a str, S>
}
 

#[allow(dead_code)]
impl<'a, S : Symbol<'a>> SymbolTable<'a, S> {
	pub fn new() -> SymbolTable<'a, S> {
		SymbolTable { map: HashMap::new() }
	}

	pub fn intern(&mut self, string: &'a str) -> S {
		if let Some(symbol) = self.map.get(string) {
			return *symbol
		}

		let symbol = S::from_str(&string);
		self.map.insert(&string, symbol);
		symbol
	}
}

pub trait Symbol<'a> : Copy {
	fn from_str(string: &'a str) -> Self;
	fn to_str(&self) -> &'a str;
}

#[macro_export]
macro_rules! symbol_type {
	($name:ident) => {
		#[derive(Clone, Copy, Debug)]
		pub struct $name<'a>(pub &'a str);
		impl<'a> ::symbol::Symbol<'a> for $name<'a> {
			fn from_str(string: &'a str) -> $name<'a> {
				$name(string)
			}
			fn to_str(&self) -> &'a str {
				self.0
			}
		}
		impl<'a> PartialEq for $name<'a> {
			fn eq(&self, other: &$name) -> bool {
				let s1: &str = self.0;
				let s2: &str = other.0;
				s1 as *const _ == s2 as *const _
			}
		}
		impl<'a> Eq for $name<'a> {}
		impl<'a> ::core::hash::Hash for $name<'a> {
			fn hash<H: ::core::hash::Hasher>(&self, state: &mut H) {
				state.write_usize(self.0 as *const _ as *const () as usize);
			}
		}
	}
}
