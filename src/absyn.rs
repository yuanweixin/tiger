pub use cfgrammar::Span;
use strum_macros::Display;
use crate::int_types::tiger_int;

/// ((start line, start column), (end line, end column))
pub type Pos =((usize,usize), (usize,usize));

#[derive(Display, Debug)]
pub enum Var {
    SimpleVar(Span, Pos),
    FieldVar(Box<Var>, Span, Pos),
    SubscriptVar(Box<Var>, Box<Exp>, Pos),
}

#[derive(Display, Debug)]
pub enum Oper {
    PlusOp,
    MinusOp,
    TimesOp,
    DivideOp,
    EqOp,
    NeqOp,
    LtOp,
    LeOp,
    GtOp,
    GeOp
}

#[derive(Display, Debug)]
pub enum Ty {
    NameTy(Span, Pos),
    RecordTy(Vec<Field>),
    ArrayTy(Span, Pos)
}

#[derive(Display, Debug)]
pub enum Dec {
    FunctionDec(Vec<Fundec>),
    VarDec {
        name: Span,
        escape: bool,
        typ: Option<(Span, Pos)>,
        init: Box<Exp>,
        pos: Pos
    },
    TypeDec(Vec<TyDec>)
}

#[derive(Debug)]
pub struct Fundec {
  pub name: Span,
  pub params: Vec<Field>,
  pub result: Option<(Span, Pos)>,
  pub body: Box<Exp>,
  pub pos: Pos
}

#[derive(Debug)]
pub struct Field {
    pub name: Span,
    pub escape: bool,
    pub typ: Span,
    pub pos: Pos
}

#[derive(Debug)]
pub struct TyDec {
    pub  name: Span,
    pub  ty: Box<Ty>,
    pub pos: Pos
}

#[derive(Display, Debug)]
pub enum Exp {
    VarExp(Box<Var>),
    NilExp,
    IntExp(tiger_int),
    StringExp(Span, Pos),
    CallExp {
        func: Span,
        args: Vec<Exp>,
        pos: Pos
    },
    OpExp {
        left: Box<Exp>,
        oper: Oper,
        right: Box<Exp>,
        pos: Pos
    },
    RecordExp {
        fields: Vec<(Span, Exp, Pos)>,
        typ: Span,
        pos: Pos
    },
    SeqExp(Vec<Exp>),
    AssignExp {
        var: Box<Var>,
        exp: Box<Exp>,
        pos: Pos
    },
    IfExp {
        test: Box<Exp>,
        then: Box<Exp>,
        els: Option<Box<Exp>>,
        pos: Pos
    },
    WhileExp {
        test: Box<Exp>,
        body: Box<Exp>,
        pos: Pos
    },
    ForExp {
        var: Span,
        escape: bool,
        lo: Box<Exp>,
        hi: Box<Exp>,
        body: Box<Exp>,
        pos: Pos
    },
    BreakExp(Pos),
    LetExp {
        decs: Vec<Dec>,
        body: Box<Exp>,
        pos: Pos
    },
    ArrayExp {
        typ: Span,
        size: Box<Exp>,
        init: Box<Exp>,
        pos: Pos
    }
}

