use crate::{
    absyn::Oper,
    frame,
    frame::Frame,
    int_types::TigerInt,
    ir::{IrBinop, IrExp, IrStm},
    symbol::Interner,
    temp::{GenTemporary, Label},
};
use std::rc::Rc;

type Conditional = fn(Label, Label) -> IrStm;

pub enum Level {
    Top,
    Nested {
        // use Rc because the Level objects form a dag where the child levels point back at the parent levels.
        parent: Rc<Level>,
        frame: Box<dyn Frame>,
    },
}

pub type Access = (Rc<Level>, frame::Access);

pub enum TrExp {
    Ex(IrExp),
    Nx(IrStm),
    Cx(Conditional),
}

// A dummy IR to be returned in translation if a type check error happens.
pub const ERROR_TR_EXP: TrExp = TrExp::Ex(IrExp::Const(42));

impl Level {
    pub fn outermost() -> Rc<Self> {
        Rc::new(Level::Top)
    }

    pub fn alloc_local(&mut self, escape: bool) -> Access {
        todo!()
    }

    pub fn new_level<T: Frame>(
        parent: Rc<Level>,
        mut escapes: Vec<bool>,
        gen_temp_label: &mut GenTemporary,
        pool: &mut Interner,
    ) -> Rc<Level> {
        // prepend true for the static link
        escapes.insert(0, true);
        Rc::new(Level::Nested {
            parent: parent.clone(),
            frame: Box::new(T::new(gen_temp_label.new_label(pool), escapes)),
        })
    }
}

impl Level {
    pub fn get_label(&self) -> Option<Label> {
        match self {
            Level::Top => None,
            Level::Nested { ref frame, .. } => Some(frame.name()),
        }
    }

    pub fn formals(&self) -> Vec<Access> {
        todo!()
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

pub fn call_exp() -> TrExp {
    todo!();
}
// pub fn call_exp(func: Label, caller_level: &Level, args: Vec<TrExp>, called_level: &Level) -> TrExp { todo!(); }

pub fn nil_exp() -> TrExp {
    todo!()
}

pub fn int_exp(i: TigerInt) -> TrExp {
    todo!()
}

pub fn string_exp(s: &str) -> TrExp {
    todo!()
}

pub fn record_exp(site_irs: Vec<TrExp>) -> TrExp {
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

pub fn simple_var() -> TrExp {
    todo!()
}

pub fn record_field(lhs_var_ir: TrExp, field_pos: usize) -> TrExp {
    todo!()
}

pub fn subscript_var(lhs_ir: TrExp, idx_ir: TrExp, exit_label: Label) -> TrExp {
    todo!()
}

// TODO
// pub fn proc_entry_exit(fragments: &mut Vec<Fragment>, level: Level, body: TrExp) {
pub fn proc_entry_exit(level: Rc<Level>, body: TrExp) {
    todo!()
}
