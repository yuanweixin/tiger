use crate::{
    temp::Label,
    ir::{IrExp, IrStm, IrBinop},
    frame::Frame,
    absyn::{Oper},
    int_types::tiger_int
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

pub fn call_exp() -> TrExp { todo!(); }
// pub fn call_exp(func: Label, caller_level: &Level, args: Vec<TrExp>, called_level: &Level) -> TrExp { todo!(); }

pub fn nil_exp() -> TrExp {
    todo!()
}

pub fn int_exp(i: tiger_int) -> TrExp {
    todo!()
}

pub fn string_exp(s: &str) -> TrExp {
    todo!()
}

pub fn record_exp(site_irs : Vec<TrExp>) -> TrExp {
    todo!()
}

pub fn seq_exp(exp_irs: Vec<TrExp>, has_return_value: bool) -> TrExp {
    todo!()
}

pub fn assignment(dst_ir: TrExp, src_ir: TrExp) -> TrExp {
    todo!()
}

pub fn array_exp() -> TrExp {
    todo!()
}

pub fn let_exp(var_init_irs: Vec<TrExp>, let_body_ir: TrExp) -> TrExp {
    todo!()
}

pub fn break_stmt(l: Label) -> TrExp {
    todo!()
}

pub fn for_loop(lo_ir: TrExp, hi_ir: TrExp, body_ir: TrExp, for_done_label: Label) -> TrExp {
    todo!()
}

pub fn while_loop(cond_ir: TrExp, body_ir: TrExp, done_label: Label) -> TrExp {
    todo!()
}

pub fn conditional(cond_ir: TrExp, then_ir: TrExp, else_ir: Option<TrExp>) -> TrExp {
    todo!()
}

