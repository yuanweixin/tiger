// TODO
#![allow(dead_code)]
#![allow(unused_variables)]


use crate::{
    absyn::Oper,
    frame,
    frame::Frame,
    int_types::TigerInt,
    ir::{IrExp, IrStm},
    symbol::Interner,
    temp::{GenTemporary, Label},
};
use std::{
    rc::Rc,
    cell::RefCell
};

type Conditional = fn(Label, Label) -> IrStm;

#[derive(Debug)]
pub enum Level {
    Top,
    Nested {
        // use Rc because the Level objects form a dag where the child levels point back at the parent levels.
        // this whole mechanism just to be able to mutate some shit is fucking crazy.
        parent: Rc<RefCell<Level>>,
        frame: Box<dyn Frame>,
    },
}

pub type Access = (Rc<RefCell<Level>>, frame::Access);

#[derive(Debug)]
pub enum TrExp {
    Ex(IrExp),
    Nx(IrStm),
    Cx(Conditional),
}

// A dummy IR to be returned in translation if a type check error happens.
pub const ERROR_TR_EXP: TrExp = TrExp::Ex(IrExp::Const(42));

impl Level {
    pub fn outermost() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Level::Top))
    }

    pub fn alloc_local(&mut self, escape: bool) -> Access {
        // TODO
        (Rc::new(RefCell::new(Level::Top)), frame::Access::InFrame(64))
    }

    pub fn new_level<T: Frame + 'static>(
        parent: Rc<RefCell<Level>>,
        mut escapes: Vec<bool>,
        gen_temp_label: &mut GenTemporary,
        pool: &mut Interner,
    ) -> Rc<RefCell<Level>> {
        // prepend true for the static link
        escapes.insert(0, true);
        Rc::new(RefCell::new(Level::Nested {
            parent: parent.clone(),
            frame: Box::new(T::new(gen_temp_label.new_label(pool), escapes)),
        }))
    }
}

impl Level {
    pub fn get_label(&self) -> Option<Label> {
        match self {
            Level::Top => None,
            Level::Nested { ref frame, .. } => Some(frame.name()),
        }
    }

    pub fn formal_without_static_link(&self, idx: usize) -> frame::Access {
        match self {
            Level::Top => {
                panic!("impl bug, Level::formals only usable in contexts where a nested level can appear");
            }
            Level::Nested { ref frame ,..} => {
                frame.formals()[idx+1].clone()
            }
        }
    }
}

fn un_ex(tr: TrExp) -> IrExp {
    // todo!();
    IrExp::Const(42)
}

fn un_cx() -> Conditional {
    fn todo(a: Label, b: Label) -> IrStm {
        IrStm::Label(a)
    }
    todo
    // todo!();
}

fn un_nx() -> IrStm {
    // todo!();
    IrStm::Exp(IrExp::Const(42))
}

pub fn binop(o: &Oper, lhs: TrExp, rhs: TrExp) -> TrExp {
    // todo!()
    lhs
}

pub fn string_cmp(is_equality: bool, lhs: TrExp, rhs: TrExp) -> TrExp {
    // todo!()
    lhs
}

pub fn call_exp() -> TrExp {
    // todo!();
    TrExp::Ex(IrExp::Const(42))

}
// pub fn call_exp(func: Label, caller_level: &Level, args: Vec<TrExp>, called_level: &Level) -> TrExp { todo!(); }

pub fn nil_exp() -> TrExp {
    // todo!()
    TrExp::Ex(IrExp::Const(42))

}

pub fn int_exp(i: TigerInt) -> TrExp {
    // todo!()
    TrExp::Ex(IrExp::Const(42))

}

pub fn string_exp(s: &str) -> TrExp {
    // todo!()
    TrExp::Ex(IrExp::Const(42))
}

pub fn record_exp(site_irs: Vec<TrExp>) -> TrExp {
    // todo!()
    TrExp::Ex(IrExp::Const(42))
}

pub fn seq_exp(exp_irs: Vec<TrExp>, has_return_value: bool) -> TrExp {
    // todo!()
    TrExp::Ex(IrExp::Const(42))
}

pub fn assignment(dst_ir: TrExp, src_ir: TrExp) -> TrExp {
    // todo!()
    TrExp::Ex(IrExp::Const(42))


}

pub fn array_exp() -> TrExp {
    // todo!()
    TrExp::Ex(IrExp::Const(42))
}
pub fn let_exp(var_init_irs: Vec<TrExp>, let_body_ir: TrExp) -> TrExp {
    // todo!()
    TrExp::Ex(IrExp::Const(42))
}

pub fn break_stmt(l: Label) -> TrExp {
    // todo!()
    TrExp::Ex(IrExp::Const(42))
}

pub fn for_loop(lo_ir: TrExp, hi_ir: TrExp, body_ir: TrExp, for_done_label: Label) -> TrExp {
    // todo!()
    TrExp::Ex(IrExp::Const(42))
}

pub fn while_loop(cond_ir: TrExp, body_ir: TrExp, done_label: Label) -> TrExp {
    // todo!()
    TrExp::Ex(IrExp::Const(42))
}

pub fn conditional(cond_ir: TrExp, then_ir: TrExp, else_ir: Option<TrExp>) -> TrExp {
    // todo!()
    ERROR_TR_EXP
}

pub fn simple_var() -> TrExp {
    // todo!()
    ERROR_TR_EXP
}

pub fn record_field(lhs_var_ir: TrExp, field_pos: usize) -> TrExp {
    // todo!()
    ERROR_TR_EXP
}

pub fn subscript_var(lhs_ir: TrExp, idx_ir: TrExp, exit_label: Label) -> TrExp {
    // todo!()
    ERROR_TR_EXP
}

// TODO
// pub fn proc_entry_exit(fragments: &mut Vec<Fragment>, level: Level, body: TrExp) {
pub fn proc_entry_exit(level: Rc<RefCell<Level>>, body: TrExp) {
    // todo!()
}
