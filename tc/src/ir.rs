// appel's tree ir language
use crate::{int_types::TigerInt, temp, temp::TempMap, Uuids};

#[derive(Hash, Eq, PartialEq, Debug, Clone)]
pub enum IrExp {
    Null,
    Const(TigerInt),
    Name(temp::Label),
    Temp(temp::Temp),
    Binop(IrBinop, Box<IrExp>, Box<IrExp>),
    Mem(Box<IrExp>),
    Call(Box<IrExp>, Vec<IrExp>),
    Eseq(Box<IrStm>, Box<IrExp>),
}

#[derive(Hash, Eq, PartialEq, Debug, Clone)]
pub enum IrStm {
    Move(Box<IrExp>, Box<IrExp>),
    Exp(Box<IrExp>),
    Jump(Box<IrExp>, Vec<temp::Label>),
    Cjump(IrRelop, Box<IrExp>, Box<IrExp>, temp::Label, temp::Label),
    Seq(Box<IrStm>, Box<IrStm>),
    Label(temp::Label),
}

impl IrExp {
    fn debug_to_string_helper(
        &self,
        tm: &TempMap,
        gen: &dyn Uuids,
        pretty: bool,
        indent: usize,
    ) -> String {
        let base_str = match self {
            IrExp::Null => return String::from("Null"),
            IrExp::Const(i) => format!("{}", i),
            IrExp::Name(l) => format!("{}", l.debug_to_string(gen)),
            IrExp::Temp(t) => format!("{}", t.debug_to_string(tm)),
            IrExp::Binop(b, e1, e2) => {
                format!(
                    "Binop({:?}, {}, {})",
                    b,
                    e1.debug_to_string_helper(tm, gen, pretty, indent + 1),
                    e2.debug_to_string_helper(tm, gen, pretty, indent + 1)
                )
            }
            IrExp::Mem(e) => format!(
                "Mem({})",
                e.debug_to_string_helper(tm, gen, pretty, indent + 1)
            ),
            IrExp::Call(e, args) => {
                let mut buf = String::new();
                buf.push_str(
                    format!(
                        "Call({}, [",
                        e.debug_to_string_helper(tm, gen, pretty, indent + 1)
                    )
                    .as_str(),
                );
                for arg in args {
                    buf.push_str(
                        &arg.debug_to_string_helper(tm, gen, pretty, indent + 1)
                            .as_str(),
                    );
                }
                buf.push_str(format!("\n{}])", "  ".repeat(indent)).as_str());
                buf
            }
            IrExp::Eseq(s, e) => format!(
                "Eseq({}, {})",
                s.debug_to_string_helper(tm, gen, pretty, indent + 1),
                e.debug_to_string_helper(tm, gen, pretty, indent + 1)
            ),
        };
        if pretty {
            let newline_tab = if pretty {
                format!("\n{}", "  ".repeat(indent))
            } else {
                String::new()
            };
            format!("{}{}", newline_tab, base_str)
        } else {
            base_str
        }
    }

    pub fn debug_to_string(&self, tm: &TempMap, gen: &dyn Uuids, pretty: bool) -> String {
        self.debug_to_string_helper(tm, gen, pretty, 0)
    }
}

impl IrStm {
    fn debug_to_string_helper(
        &self,
        tm: &TempMap,
        gen: &dyn Uuids,
        pretty: bool,
        indent: usize,
    ) -> String {
        let base_str = match self {
            IrStm::Move(a, b) => {
                format!(
                    "Move({}, {})",
                    a.debug_to_string_helper(tm, gen, pretty, indent + 1),
                    b.debug_to_string_helper(tm, gen, pretty, indent + 1)
                )
            }
            IrStm::Exp(a) => format!(
                "Exp({})",
                a.debug_to_string_helper(tm, gen, pretty, indent + 1)
            ),
            IrStm::Jump(a, l) => format!(
                "Jump({}, {:?})",
                a.debug_to_string_helper(tm, gen, pretty, indent + 1),
                l.iter().map(|l| l.debug_to_string(gen)).collect::<Vec<_>>()
            ),
            IrStm::Cjump(r, a, b, t, f) => format!(
                "Cjump({:?}, {}, {}, {:?}, {:?})",
                r,
                a.debug_to_string_helper(tm, gen, pretty, indent + 1),
                b.debug_to_string_helper(tm, gen, pretty, indent + 1),
                t.debug_to_string(gen),
                f.debug_to_string(gen)
            ),
            IrStm::Seq(a, b) => format!(
                "Seq({}, {})",
                a.debug_to_string_helper(tm, gen, pretty, indent + 1),
                b.debug_to_string_helper(tm, gen, pretty, indent + 1)
            ),
            IrStm::Label(l) => return format!("{}", l.debug_to_string(gen)),
        };
        if pretty {
            let newline_tab = if pretty {
                format!("\n{}", "  ".repeat(indent))
            } else {
                String::new()
            };
            format!("{}{}", newline_tab, base_str)
        } else {
            base_str
        }
    }

    pub fn debug_to_string(&self, tm: &TempMap, gen: &dyn Uuids, pretty: bool) -> String {
        self.debug_to_string_helper(tm, gen, pretty, 0)
    }
}

#[derive(Hash, Eq, PartialEq, Debug, Clone)]
pub enum IrBinop {
    Plus,
    Minus,
    Mul,
    Div,
    And,
    Or,
    Lshift,
    Rshift,
    ArShift,
    Xor,
}

#[derive(Hash, Eq, PartialEq, Debug, Clone)]
pub enum IrRelop {
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    Ult,
    Ule,
    Ugt,
    Uge,
}

/// a collection of helper methods to avoid having to type Box::new
/// when constructing IrExp and IrStm. This would also make refactoring
/// much easier if we decide to change the representation. The function
/// names are intentionally in pascal case to mirror the name of the enum
/// values to make usage seamless.
#[allow(non_snake_case)]
pub mod helpers {
    use super::*;

    #[inline]
    pub fn Binop(r: IrBinop, a: IrExp, b: IrExp) -> IrExp {
        IrExp::Binop(r, Box::new(a), Box::new(b))
    }

    #[inline]
    pub fn Mem(e: IrExp) -> IrExp {
        IrExp::Mem(Box::new(e))
    }

    #[inline]
    pub fn Call(f: IrExp, args: Vec<IrExp>) -> IrExp {
        IrExp::Call(Box::new(f), args)
    }

    #[inline]
    pub fn Eseq(s: IrStm, e: IrExp) -> IrExp {
        IrExp::Eseq(Box::new(s), Box::new(e))
    }

    #[inline]
    pub fn Move(a: IrExp, b: IrExp) -> IrStm {
        IrStm::Move(Box::new(a), Box::new(b))
    }

    #[inline]
    pub fn Exp(a: IrExp) -> IrStm {
        IrStm::Exp(Box::new(a))
    }

    #[inline]
    pub fn Jump(a: IrExp, l: Vec<temp::Label>) -> IrStm {
        IrStm::Jump(Box::new(a), l)
    }

    #[inline]
    pub fn Cjump(r: IrRelop, a: IrExp, b: IrExp, t: temp::Label, f: temp::Label) -> IrStm {
        IrStm::Cjump(r, Box::new(a), Box::new(b), t, f)
    }

    #[inline]
    pub fn Seq(a: IrStm, b: IrStm) -> IrStm {
        IrStm::Seq(Box::new(a), Box::new(b))
    }
}
