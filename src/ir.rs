// appel's tree ir language
use crate::absyn::types::Oper;
use crate::int_types::TigerInt;
use crate::temp;

#[derive(Debug, Clone)]
pub enum IrExp {
    Const(TigerInt),
    Name(temp::Label),
    Temp(temp::Temp),
    Binop(IrBinop, Box<IrExp>, Box<IrExp>),
    Mem(Box<IrExp>),
    Call(Box<IrExp>, Vec<IrExp>),
    Eseq(Box<IrStm>, Box<IrExp>),
}

#[derive(Debug, Clone)]
pub enum IrStm {
    Move(Box<IrExp>, Box<IrExp>),
    Exp(Box<IrExp>),
    Jump(Box<IrExp>, Vec<temp::Label>),
    Cjump(IrRelop, Box<IrExp>, Box<IrExp>, temp::Label, temp::Label),
    Seq(Box<IrStm>, Box<IrStm>),
    Label(temp::Label),
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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
