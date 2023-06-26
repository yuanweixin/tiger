// appel's tree ir language
use crate::temp;
use crate::absyn::{Oper};
use crate::int_types::TigerInt;

#[derive(Debug)]
pub enum IrExp {
    Const(TigerInt),
    Name(temp::Label),
    Temp(temp::Temp),
    Binop(IrBinop, Box<IrExp>, Box<IrExp>),
    Mem(Box<IrExp>),
    Call(Box<IrExp>, Vec<IrExp>),
    Eseq(Box<IrStm>, Box<IrExp>),
}

#[derive(Debug)]
pub enum IrStm {
    Move(IrExp, IrExp),
    Exp(IrExp),
    Jump(IrExp, Vec<temp::Label>),
    Cjump(IrRelop, IrExp, IrExp, temp::Label, temp::Label),
    Seq(Box<IrStm>, Box<IrStm>),
    Label(temp::Label),
}

#[derive(Debug)]
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

#[derive(Debug)]
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
