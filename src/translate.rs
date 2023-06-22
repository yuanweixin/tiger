use crate::{
    temp::Label,
    ir::{IrExp, IrStm, IrBinop},
    frame::Frame,
    absyn::{Oper}
};

type Conditional = fn(Label, Label) -> IrStm;

pub enum Level {
    Top,
    Nested {
        parent: Box<Level>,
        frame: Box<dyn Frame>
    }
}

pub enum TrExp {
    Ex(IrExp),
    Nx(IrStm),
    Cx(Conditional),
}

// A dummy IR to be returned in translation if a type check error happens.
pub const ERROR_TR_EXP : TrExp = TrExp::Ex(IrExp::Const(42));

pub fn get_label(l: Level) -> Option<Label> {
    match l {
        Level::Top => None,
        Level::Nested { ref frame, ..} => Some(frame.name())
    }
}


fn un_ex(tr: TrExp) -> IrExp {
    todo!();
}

fn un_cx() -> Conditional {
    todo!();
}

fn un_nx() -> IrStm {
    todo!();
}

pub fn binop(o: &Oper, lhs: TrExp, rhs: TrExp) -> TrExp {
    todo!();
    // match o {
    //     Oper::PlusOp => Ex(IrExp::Binop(IrBinop::Plus, un_ex(lhs), )),

    // }
}

pub fn string_cmp(is_equality: bool, lhs: TrExp, rhs: TrExp) -> TrExp {
    todo!();
}
