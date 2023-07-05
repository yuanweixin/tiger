use crate::{
    absyn::Oper,
    absyn::Oper::*,
    frame,
    frame::Frame,
    int_types::TigerInt,
    ir::{IrBinop::*, IrExp, IrExp::*, IrRelop, IrRelop::*, IrStm, IrStm::*},
    symbol::Interner,
    temp::{GenTemporary, Label},
};
use std::{cell::RefCell, rc::Rc};

#[derive(Debug)]
pub enum Level {
    Top,
    Nested {
        // use Rc because the Level objects form a dag where the child levels point back at the parent levels.
        // this whole mechanism just to be able to mutate some shit is fucking crazy.
        parent: Rc<RefCell<Level>>,
        frame: Rc<dyn Frame>,
    },
}

// needed to generate levels.
impl std::cmp::Eq for Level {}

impl PartialEq for Level {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Level::Top, Level::Top) => true,
            (Level::Top, _) | (_, Level::Top) => false,
            // each frame should have been assigned a unique label.
            (Level::Nested { frame: f1, .. }, Level::Nested { frame: f2, .. }) => {
                f1.name() == f2.name()
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Access(pub Rc<RefCell<Level>>, pub frame::Access);

#[derive(Debug, Clone)]
pub enum Conditional {
    Truthy,
    Falsy,
    Cond(IrRelop, Box<IrExp>, Box<IrExp>), // rel, lhs, rhs
}

impl Conditional {
    fn eval(self, t: Label, f: Label) -> IrStm {
        match self {
            Self::Truthy => Jump(Box::new(Name(t)), vec![t]),
            Self::Falsy => Jump(Box::new(Name(f)), vec![f]),
            Self::Cond(relop, lhs, rhs) => Cjump(relop, lhs, rhs, t, f),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TrExp {
    Ex(Box<IrExp>),
    Nx(Box<IrStm>),
    Cx(Conditional),
}

use TrExp::*;

impl Level {
    pub fn outermost() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Level::Top))
    }

    pub fn alloc_local(myself: Rc<RefCell<Level>>, escape: bool) -> Access {
        let frame_access = match *myself.borrow_mut() {
            Level::Top => {
                panic!("impl bug, cannot allocate local in top level");
            }
            Level::Nested { frame, .. } => frame.alloc_local(escape),
        };
        Access(myself, frame_access)
    }

    pub fn new_level<T: Frame + 'static>(
        parent: Rc<RefCell<Level>>,
        mut escapes: Vec<bool>,
        gen_temp_label: &mut GenTemporary,
    ) -> (Rc<RefCell<Level>>, Label) {
        // prepend true for the static link
        escapes.insert(0, true);
        let function_label = gen_temp_label.new_label();
        (
            Rc::new(RefCell::new(Level::Nested {
                parent: parent.clone(),
                frame: Rc::new(T::new(function_label, escapes)),
            })),
            function_label,
        )
    }
}

impl Level {
    pub fn formal_without_static_link(&self, idx: usize) -> frame::Access {
        match self {
            Level::Top => {
                panic!("impl bug, Level::formals only usable in contexts where a nested level can appear");
            }
            Level::Nested { ref frame, .. } => frame.formals()[idx + 1].clone(),
        }
    }
}

fn make_seq(stmts: Vec<IrStm>) -> Box<IrStm> {
    assert!(
        !stmts.is_empty(),
        "bug in impl, trying to combine 0 IrStm into 1, clearly impossible"
    );
    if stmts.len() == 1 {
        return Box::new(stmts.into_iter().next().unwrap());
    }
    let mut iter = stmts.into_iter();
    let last = iter.next().unwrap();
    let sec_last = iter.next().unwrap();
    let mut so_far = IrStm::Seq(Box::new(sec_last), Box::new(last));
    while let Some(nxt) = iter.next() {
        so_far = IrStm::Seq(Box::new(nxt), Box::new(so_far));
    }
    Box::new(so_far)
}

fn un_ex(tr: TrExp, gen: &mut GenTemporary) -> Box<IrExp> {
    match tr {
        Ex(exp) => exp,
        Cx(cond) => {
            let r = gen.new_temp();
            let t = gen.new_label();
            let f = gen.new_label();
            Box::new(Eseq(
                make_seq(vec![
                    Move(Box::new(Temp(r)), Box::new(Const(1))),
                    cond.eval(t, f),
                    Label(f),
                    Move(Box::new(Temp(r)), Box::new(Const(0))),
                    Label(t),
                ]),
                Box::new(Temp(r)),
            ))
        }
        Nx(stm) => Box::new(Eseq(stm, Box::new(Const(0)))),
    }
}

fn un_cx(tr: TrExp) -> Conditional {
    match tr {
        Nx(..) => {
            panic!("un_cx(Nx) should never occur in a well typed program.");
        }
        Cx(cond) => cond,
        Ex(exp) => match *exp {
            Const(0) => Conditional::Falsy,
            Const(1) => Conditional::Truthy,
            _ => Conditional::Cond(Ne, Box::new(Const(0)), exp),
        },
    }
}

fn un_nx(tr: TrExp, gen: &mut GenTemporary) -> Box<IrStm> {
    match tr {
        Nx(stm) => stm,
        Cx(c) => {
            let l = gen.new_label();
            let s = c.eval(l, l);
            make_seq(vec![s, Label(l)])
        }
        Ex(exp) => Box::new(Exp(exp)),
    }
}

pub fn binop(o: &Oper, lhs: TrExp, rhs: TrExp, gen: &mut GenTemporary) -> TrExp {
    let left = un_ex(lhs, gen);
    let right = un_ex(rhs, gen);
    match o {
        PlusOp => Ex(Box::new(Binop(Plus, left, right))),
        MinusOp => Ex(Box::new(Binop(Minus, left, right))),
        TimesOp => Ex(Box::new(Binop(Mul, left, right))),
        DivideOp => Ex(Box::new(Binop(Div, left, right))),
        EqOp => Cx(Conditional::Cond(Eq, left, right)),
        NeqOp => Cx(Conditional::Cond(Ne, left, right)),
        LtOp => Cx(Conditional::Cond(Lt, left, right)),
        LeOp => Cx(Conditional::Cond(Le, left, right)),
        GtOp => Cx(Conditional::Cond(Gt, left, right)),
        GeOp => Cx(Conditional::Cond(Ge, left, right)),
    }
}

pub fn string_cmp<T: Frame>(
    is_equality: bool,
    lhs: TrExp,
    rhs: TrExp,
    gen: &mut GenTemporary,
) -> TrExp {
    if is_equality {
        Ex(Box::new(T::external_call(
            "stringEqual",
            vec![*un_ex(lhs, gen), *un_ex(rhs, gen)],
        )))
    } else {
        let r = gen.new_temp();
        Ex(Box::new(Eseq(
            Box::new(Move(
                Box::new(Temp(r)),
                Box::new(T::external_call(
                    "stringEqual",
                    vec![*un_ex(lhs, gen), *un_ex(rhs, gen)],
                )),
            )),
            Box::new(Binop(Xor, Box::new(Temp(r)), Box::new(Const(1)))),
        )))
    }
}



pub fn call_exp<T: Frame>(
    func: Label,
    caller_level: Rc<RefCell<Level>>,
    args: Vec<TrExp>,
    callee_level: Rc<RefCell<Level>>,
    gen: &mut GenTemporary,
) -> TrExp {
    // let mut augmented_args = Vec::new();

    // if *callee_level.borrow() == Level::Top {
    //     // happens iff calling one of the built-in procs.
    //     // no static link for them because they don't need them!
    //     for arg in args {
    //         augmented_args.push(*un_ex(arg, gen));
    //     }
    //     let fn_name = gen.resolve_label(func);
    //     if fn_name.is_none() {
    //         panic!("impl bug, call_exp has unknown fn_name; did you type check this first?");
    //     }
    //     return Ex(Box::new(T::external_call(fn_name.unwrap(), augmented_args)));
    // }
    // resolve the common ancestor.
    // it is assumed the input has been typed checked successfully.
    // all user defined functions run under the scope of `main`, so this means
    // the common ancestor always exist.
    //
    // given a callee, the common ancestor can be:
    // 1. the
    return Ex(Box::new(Const(42)));
}

pub fn nil_exp() -> TrExp {
    Ex(Box::new(Const(0)))
}

pub fn int_exp(i: TigerInt) -> TrExp {
    Ex(Box::new(Const(i)))
}

pub fn string_exp<T: Frame>(
    s: &str,
    gen: &mut GenTemporary,
    frags: &mut Vec<frame::Frag<T>>,
) -> TrExp {
    for frag in frags.iter() {
        match frag {
            frame::Frag::String(label, ..) => {
                return Ex(Box::new(Name(*label)));
            }
            _ => {}
        }
    }
    let l = gen.new_label();
    let new_frag = frame::Frag::<T>::String(l, String::from(s));
    frags.push(new_frag);
    Ex(Box::new(Name(l)))
}

pub fn record_exp<T: Frame>(site_irs: Vec<TrExp>, gen: &mut GenTemporary) -> TrExp {
    let r = gen.new_temp();
    let i = gen.new_temp();
    let done = gen.new_label();
    let body = gen.new_label();
    let test = gen.new_label();
    let mut instrs = Vec::new();

    instrs.push(Move(
        Box::new(Temp(r)),
        Box::new(T::external_call(
            "malloc",
            vec![Const(
                T::word_size().checked_mul(site_irs.len()).unwrap() as i32
            )],
        )),
    ));
    let mut idx: usize = 0;
    for site_ir in site_irs {
        let offset = idx.checked_mul(T::word_size()).unwrap() as i32;
        instrs.push(Move(
            Box::new(Mem(Box::new(Binop(
                Plus,
                Box::new(Temp(r)),
                Box::new(Const(offset)),
            )))),
            un_ex(site_ir, gen),
        ));
        idx += 1;
    }
    Ex(Box::new(Eseq(make_seq(instrs), Box::new(Temp(r)))))
}

pub fn seq_exp(exp_irs: Vec<TrExp>, has_return_value: bool, gen: &mut GenTemporary) -> TrExp {
    if exp_irs.len() == 0 {
        // for example, an empty let block
        Nx(Box::new(Exp(Box::new(Const(0)))))
    } else if exp_irs.len() == 1 {
        if has_return_value {
            Ex(un_ex(exp_irs.into_iter().next().unwrap(), gen))
        } else {
            Nx(un_nx(exp_irs.into_iter().next().unwrap(), gen))
        }
    } else {
        let mut irstms = Vec::new();
        if has_return_value {
            let upper = exp_irs.len() - 1;
            let mut iter = exp_irs.into_iter();
            while irstms.len() < upper {
                irstms.push(*un_nx(iter.next().unwrap(), gen));
            }
            Ex(Box::new(Eseq(
                make_seq(irstms),
                un_ex(iter.next().unwrap(), gen),
            )))
        } else {
            for tr in exp_irs.into_iter() {
                irstms.push(*un_nx(tr, gen));
            }
            Nx(make_seq(irstms))
        }
    }
}

pub fn assignment(dst_ir: TrExp, src_ir: TrExp, gen: &mut GenTemporary) -> TrExp {
    Nx(Box::new(Move(un_ex(dst_ir, gen), un_ex(src_ir, gen))))
}

pub fn array_exp<T: Frame>(size_ir: TrExp, init_val_ir: TrExp, gen: &mut GenTemporary) -> TrExp {
    Ex(Box::new(T::external_call(
        "initArray",
        vec![*un_ex(size_ir, gen), *un_ex(init_val_ir, gen)],
    )))
}

pub fn let_exp(var_init_irs: Vec<TrExp>, let_body_ir: TrExp, gen: &mut GenTemporary) -> TrExp {
    if var_init_irs.len() == 0 {
        return let_body_ir;
    }
    let mut seqs = Vec::new();
    for ir in var_init_irs {
        seqs.push(*un_nx(ir, gen));
    }
    if let Nx(_) = let_body_ir {
        seqs.push(*un_nx(let_body_ir, gen));
        Nx(make_seq(seqs))
    } else {
        Ex(Box::new(Eseq(make_seq(seqs), un_ex(let_body_ir, gen))))
    }
}

pub fn break_stmt(l: Label) -> TrExp {
    Nx(Box::new(Jump(Box::new(Name(l)), vec![l])))
}

pub fn for_loop(
    lo_ir: TrExp,
    hi_ir: TrExp,
    body_ir: TrExp,
    for_done_label: Label,
    gen: &mut GenTemporary,
) -> TrExp {
    // note the i < limit check is done BEFORE incrementing i to avoid the edge
    // case where limit == intmax, where if we increment i first we either get
    // overflow error or an infinite loop, depending on the platform.
    let test_label = gen.new_label();
    let body_label = gen.new_label();
    let cont_label = gen.new_label();
    let i = gen.new_temp();
    let limit = gen.new_temp();
    // the extra check after `body` avoids overflow where hi=maxint.
    // let var i := lo
    //  var limit := hi
    // in while i <= limit
    //      do (body;
    //             if i == limit goto done;
    //             i := i + 1)
    Nx(make_seq(vec![
        Move(Box::new(Temp(i)), un_ex(lo_ir, gen)),
        Move(Box::new(Temp(limit)), un_ex(hi_ir, gen)),
        Label(test_label),
        Cjump(
            Le,
            Box::new(Temp(i)),
            Box::new(Temp(limit)),
            body_label,
            for_done_label,
        ),
        Label(body_label),
        *un_nx(body_ir, gen),
        Cjump(
            Eq,
            Box::new(Temp(i)),
            Box::new(Temp(limit)),
            for_done_label,
            cont_label,
        ),
        Label(cont_label),
        Move(
            Box::new(Temp(i)),
            Box::new(Binop(Plus, Box::new(Temp(i)), Box::new(Const(1)))),
        ),
        Jump(Box::new(Name(test_label)), vec![test_label]),
    ]))
}

pub fn while_loop(
    cond_ir: TrExp,
    body_ir: TrExp,
    done_label: Label,
    gen: &mut GenTemporary,
) -> TrExp {
    let test = gen.new_label();
    let done = gen.new_label();
    let body = gen.new_label();

    Nx(make_seq(vec![
        Label(test),
        un_cx(cond_ir).eval(body, done),
        Label(body),
        *un_nx(body_ir, gen),
        Jump(Box::new(Name(test)), vec![test]),
        Label(done),
    ]))
}

#[inline]
fn full_conditional(
    cond_ir: TrExp,
    then_ir: TrExp,
    else_ir: TrExp,
    gen: &mut GenTemporary,
) -> TrExp {
    fn helper(
        branch_ir: TrExp,
        true_cx: Label,
        false_cx: Label,
        done: Label,
        gen: &mut GenTemporary,
        return_register: IrExp,
    ) -> IrStm {
        match branch_ir {
            Cx(..) => un_cx(branch_ir).eval(true_cx, false_cx),
            Ex(..) => *make_seq(vec![
                Move(Box::new(return_register), un_ex(branch_ir, gen)),
                Jump(Box::new(Name(done)), vec![done]),
            ]),
            Nx(..) => *make_seq(vec![
                *un_nx(branch_ir, gen),
                Jump(Box::new(Name(done)), vec![done]),
            ]),
        }
    }

    let true_branch_label = gen.new_label();
    let false_branch_label = gen.new_label();
    let true_cx_branch_label = gen.new_label();
    let false_cx_branch_label = gen.new_label();
    let done = gen.new_label();
    let r = gen.new_temp();

    match (&then_ir, &else_ir) {
        (Nx(..), Nx(..)) => {
            let true_branch_stmt = helper(
                then_ir,
                true_cx_branch_label,
                false_cx_branch_label,
                done,
                gen,
                Temp(r),
            );
            let false_branch_stmt = helper(
                else_ir,
                true_cx_branch_label,
                false_cx_branch_label,
                done,
                gen,
                Temp(r),
            );

            Nx(make_seq(vec![
                un_cx(cond_ir).eval(true_branch_label, false_branch_label),
                Label(true_branch_label),
                true_branch_stmt,
                Label(false_branch_label),
                false_branch_stmt,
                Label(done),
            ]))
        }
        (Nx(..), _) | (_, Nx(..)) => {
            panic!("impl bug, conditional ir generation should be invoked on if-then-else branch with the same return types on both branches");
        }
        (Cx(..), _) | (_, Cx(..)) => {
            let true_branch_stmt = helper(
                then_ir,
                true_cx_branch_label,
                false_cx_branch_label,
                done,
                gen,
                Temp(r),
            );
            let false_branch_stmt = helper(
                else_ir,
                true_cx_branch_label,
                false_cx_branch_label,
                done,
                gen,
                Temp(r),
            );
            Ex(Box::new(Eseq(
                make_seq(vec![
                    un_cx(cond_ir).eval(true_branch_label, false_branch_label),
                    Label(true_branch_label),
                    true_branch_stmt,
                    Label(false_branch_label),
                    false_branch_stmt,
                    Label(false_cx_branch_label),
                    Move(Box::new(Temp(r)), Box::new(Const(0))),
                    Jump(Box::new(Name(done)), vec![done]),
                    Label(true_cx_branch_label),
                    Move(Box::new(Temp(r)), Box::new(Const(1))),
                    Jump(Box::new(Name(done)), vec![done]),
                    Label(done),
                ]),
                Box::new(Temp(r)),
            )))
        }
        (_, _) => {
            let true_branch_stmt = helper(
                then_ir,
                true_cx_branch_label,
                false_cx_branch_label,
                done,
                gen,
                Temp(r),
            );
            let false_branch_stmt = helper(
                else_ir,
                true_cx_branch_label,
                false_cx_branch_label,
                done,
                gen,
                Temp(r),
            );

            Ex(Box::new(Eseq(
                make_seq(vec![
                    un_cx(cond_ir).eval(true_branch_label, false_branch_label),
                    Label(true_branch_label),
                    true_branch_stmt,
                    Label(false_branch_label),
                    false_branch_stmt,
                    Label(done),
                ]),
                Box::new(Temp(r)),
            )))
        }
    }
}

pub fn conditional(
    cond_ir: TrExp,
    then_ir: TrExp,
    else_ir: Option<TrExp>,
    gen: &mut GenTemporary,
) -> TrExp {
    if else_ir.is_none() {
        let t = gen.new_label();
        let f = gen.new_label();
        Nx(make_seq(vec![
            un_cx(cond_ir).eval(t, f),
            Label(t),
            *un_nx(then_ir, gen),
            Label(f),
        ]))
    } else {
        // complexity comes from attempt at pattern matching special cases.
        full_conditional(cond_ir, then_ir, else_ir.unwrap(), gen)
    }
}

pub fn simple_var<T: Frame>(
    access: Access,
    current_level: Rc<RefCell<Level>>,
    gen: &mut GenTemporary,
) -> TrExp {
    let final_level = access.0;
    let cur_level = current_level.clone();
    let mut access_expr = Temp(T::frame_pointer(gen));

    while *final_level.borrow() != *cur_level.borrow() {
        // to access x in another frame,
        // let
        // (note in our scheme, x always escapes due to reference in a nested
        // function, precluding any possibility of it being allocated in a register)
        //
        //  general form of translation is below: (Appel p156)
        // MEM(+Const(x_offset), Mem(+Const(k_n-1), ..., Mem(+Const(k_1), Temp(FP)))
        //
        // where x_offset = the offset of x in its own frame
        // k_i = the offset of the static link in the frame.
        // since static link is always the first formal parameter in the frame, we
        // can access its offset as level.frame.formals()[0]
        match &*cur_level.borrow() {
            Level::Top => {
                panic!("impl bug! ran out of levels while accessing a variable in some parent level, did this code pass type checking first?");
            }
            Level::Nested { frame, .. } => match frame.as_ref().formals()[0] {
                frame::Access::InReg(..) => {
                    panic!("impl bug, static link should always be InFrame")
                }
                frame::Access::InFrame(static_link_offset) => {
                    access_expr = Mem(Box::new(Binop(
                        Plus,
                        Box::new(Const(static_link_offset)),
                        Box::new(access_expr),
                    )));
                }
            },
        }
    }
    let frame_access = access.1;
    match frame_access {
        frame::Access::InReg(reg) => {
            if *final_level.borrow() != *current_level.borrow() {
                // note we are comparing the original input current level against
                // the level where the var being accessed is declared, NOT the one
                // we have been bashing above.
                //
                // this is a bug because we are accessing a variable declared
                // outside the current function and yet it is assigned to a register.
                panic!("impl bug: a variable is InReg but it is accessed in a nested function");
            }
            // this is only possible in the non-nested access.
            Ex(Box::new(Temp(reg)))
        }
        frame::Access::InFrame(x_offset) => Ex(Box::new(Mem(Box::new(Binop(
            Plus,
            Box::new(Const(x_offset)),
            Box::new(access_expr),
        ))))),
    }
}

pub fn record_field<T: Frame>(
    lhs_var_ir: TrExp,
    field_pos: usize,
    gen: &mut GenTemporary,
) -> TrExp {
    Ex(Box::new(Mem(Box::new(Binop(
        Plus,
        un_ex(lhs_var_ir, gen),
        // hmm, this could potentially overflow in extreme edge case.
        Box::new(Const((field_pos * T::word_size()) as i32)),
    )))))
}

const INVALID_ARRAY_ACCESS_EXIT_CODE: i32 = -1;
pub fn subscript_var<T: Frame>(
    lhs_ir: TrExp,
    idx_ir: TrExp,
    exit_label: Label,
    gen: &mut GenTemporary,
) -> TrExp {
    // byte offset of idx+1 basically.
    let idx = Binop(
        Plus,
        Box::new(Const(T::word_size() as i32)),
        Box::new(Binop(
            Mul,
            un_ex(idx_ir, gen),
            Box::new(Const(T::word_size() as i32)),
        )),
    );
    let bad = gen.new_label();
    let upper_check = gen.new_label();
    let access = gen.new_label();
    let lhs_unexed = un_ex(lhs_ir, gen);
    Ex(Box::new(Eseq(
        make_seq(vec![
            Cjump(
                Ge,
                Box::new(idx.clone()),
                Box::new(Const(0)),
                upper_check,
                bad,
            ),
            Label(upper_check),
            Cjump(Lt, Box::new(idx.clone()), lhs_unexed.clone(), access, bad),
            Label(bad),
            Exp(Box::new(T::external_call(
                "exit",
                vec![Const(INVALID_ARRAY_ACCESS_EXIT_CODE)],
            ))),
            Label(access),
        ]),
        Box::new(Mem(Box::new(Binop(Plus, lhs_unexed, Box::new(idx))))),
    )))
}

// pub fn proc_entry_exit(fragments: &mut Vec<Fragment>, level: Level, body: TrExp) {
pub fn proc_entry_exit(level: Rc<RefCell<Level>>, body: TrExp) {
    // todo!()
}
