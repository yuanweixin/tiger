use crate::ir::helpers::*;
use crate::{
    absyn::types::{Oper, Oper::*},
    frame,
    frame::Frame,
    int_types::TigerInt,
    ir::{
        IrBinop::*,
        IrExp,
        IrExp::{Const, Name, Temp},
        IrRelop,
        IrRelop::*,
        IrStm,
        IrStm::Label,
    },
    temp,
    temp::Uuids,
};
use std::{cell::RefCell, rc::Rc};

#[derive(Debug)]
pub enum Level {
    Top,
    Nested {
        // use Rc because the Level objects form a dag where the child levels point back at the parent levels.
        // this whole mechanism just to be able to mutate some shit is fucking crazy.
        parent: Rc<RefCell<Level>>,
        frame: Rc<RefCell<dyn Frame>>,
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
                f1.borrow().name() == f2.borrow().name()
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
    Cond(IrRelop, IrExp, IrExp), // rel, lhs, rhs
}

impl Conditional {
    fn eval(self, t: temp::Label, f: temp::Label) -> IrStm {
        match self {
            Self::Truthy => Jump(Name(t), vec![t]),
            Self::Falsy => Jump(Name(f), vec![f]),
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

// convenience functions to avoid typing Box::new...
#[allow(non_snake_case)]
fn Ex(ie: IrExp) -> TrExp {
    TrExp::Ex(Box::new(ie))
}

#[allow(non_snake_case)]
fn Nx(is: IrStm) -> TrExp {
    TrExp::Nx(Box::new(is))
}

impl Level {
    fn static_link(&self, existing_access_expr: IrExp) -> IrExp {
        // note in our scheme, the static link is the first formal parameter and always escapes
        //
        //  general form of translation is below: (Appel p156)
        // MEM(+Const(x_offset), Mem(+Const(k_n-1), ..., Mem(+Const(k_1), Temp(FP)))
        //
        // where
        //
        // x_offset = the offset of x in its own frame
        // k_i = the offset of the static link in the frame.
        match self {
            Level::Top => {
                panic!("impl bug: Top level has no static link");
            }
            Level::Nested { frame, .. } => match frame.borrow().formals()[0] {
                frame::Access::InReg(..) => {
                    panic!("impl bug, static link should always be InFrame")
                }
                // static link offset 0 in the scheme presented in Appel?
                // if so, then this would be pointless to add 0 to it.
                frame::Access::InFrame(static_link_offset) => {
                    if static_link_offset == 0 {
                        Mem(existing_access_expr)
                    } else {
                        Mem(Binop(Plus, Const(static_link_offset), existing_access_expr))
                    }
                }
            },
        }
    }

    pub fn outermost() -> Rc<RefCell<Self>> {
        Level::Top.into()
    }

    pub fn alloc_local(myself: Rc<RefCell<Level>>, escape: bool, gen: &mut dyn Uuids) -> Access {
        let frame_access = match *myself.borrow_mut() {
            Level::Top => {
                panic!("impl bug, cannot allocate local in top level");
            }
            Level::Nested { ref frame, .. } => frame.borrow_mut().alloc_local(escape, gen),
        };
        Access(myself, frame_access)
    }

    pub fn new_level<T: Frame + 'static>(
        parent: Rc<RefCell<Level>>,
        mut escapes: Vec<bool>,
        gen_temp_label: &mut dyn Uuids,
    ) -> (Rc<RefCell<Level>>, temp::Label) {
        // prepend true for the static link
        escapes.insert(0, true);
        let function_label = gen_temp_label.new_label();
        (
            Level::Nested {
                parent: parent.clone(),
                frame: Rc::new(RefCell::new(T::new(
                    function_label,
                    escapes,
                    gen_temp_label,
                ))),
            }
            .into(),
            function_label,
        )
    }

    fn get_parent(&self) -> Rc<RefCell<Level>> {
        match self {
            Level::Top => {
                panic!("impl bug: unable to get the parent level of the Top level!");
            }
            Level::Nested { parent, .. } => parent.clone(),
        }
    }

    pub fn formal_without_static_link(&self, idx: usize) -> frame::Access {
        match self {
            Level::Top => {
                panic!("impl bug, Level::formals only usable in contexts where a nested level can appear");
            }
            Level::Nested { ref frame, .. } => frame.borrow_mut().formals()[idx + 1].clone(),
        }
    }
}

fn make_seq(stmts: Vec<IrStm>) -> IrStm {
    assert!(
        !stmts.is_empty(),
        "bug in impl, trying to combine 0 IrStm into 1, clearly impossible"
    );
    if stmts.len() == 1 {
        return stmts.into_iter().next().unwrap();
    }
    let mut iter = stmts.into_iter();
    let last = iter.next().unwrap();
    let sec_last = iter.next().unwrap();
    let mut so_far = Seq(sec_last, last);
    while let Some(nxt) = iter.next() {
        so_far = Seq(nxt, so_far);
    }
    so_far
}

fn un_ex(tr: TrExp, gen: &mut dyn Uuids) -> IrExp {
    match tr {
        TrExp::Ex(exp) => *exp,
        TrExp::Cx(cond) => {
            let r = gen.new_temp();
            let t = gen.new_label();
            let f = gen.new_label();
            Eseq(
                make_seq(vec![
                    Move(Temp(r), Const(1)),
                    cond.eval(t, f),
                    Label(f),
                    Move(Temp(r), Const(0)),
                    Label(t),
                ]),
                Temp(r),
            )
        }
        TrExp::Nx(stm) => IrExp::Eseq(stm, Box::new(Const(0))),
    }
}

fn un_cx(tr: TrExp) -> Conditional {
    match tr {
        TrExp::Nx(..) => {
            panic!("un_cx(Nx) should never occur in a well typed program.");
        }
        TrExp::Cx(cond) => cond,
        TrExp::Ex(exp) => match *exp {
            Const(0) => Conditional::Falsy,
            Const(1) => Conditional::Truthy,
            _ => Conditional::Cond(Ne, Const(0), *exp),
        },
    }
}

fn un_nx(tr: TrExp, gen: &mut dyn Uuids) -> IrStm {
    match tr {
        TrExp::Nx(stm) => *stm,
        TrExp::Cx(c) => {
            let l = gen.new_label();
            let s = c.eval(l, l);
            make_seq(vec![s, Label(l)])
        }
        TrExp::Ex(exp) => IrStm::Exp(exp),
    }
}

pub fn binop(o: &Oper, lhs: TrExp, rhs: TrExp, gen: &mut dyn Uuids) -> TrExp {
    let left = un_ex(lhs, gen);
    let right = un_ex(rhs, gen);
    match o {
        PlusOp => Ex(Binop(Plus, left, right)),
        MinusOp => Ex(Binop(Minus, left, right)),
        TimesOp => Ex(Binop(Mul, left, right)),
        DivideOp => Ex(Binop(Div, left, right)),
        EqOp => TrExp::Cx(Conditional::Cond(Eq, left, right)),
        NeqOp => TrExp::Cx(Conditional::Cond(Ne, left, right)),
        LtOp => TrExp::Cx(Conditional::Cond(Lt, left, right)),
        LeOp => TrExp::Cx(Conditional::Cond(Le, left, right)),
        GtOp => TrExp::Cx(Conditional::Cond(Gt, left, right)),
        GeOp => TrExp::Cx(Conditional::Cond(Ge, left, right)),
    }
}

pub fn string_cmp<T: Frame>(
    is_equality: bool,
    lhs: TrExp,
    rhs: TrExp,
    gen: &mut dyn Uuids,
) -> TrExp {
    if is_equality {
        Ex(T::external_call(
            gen.named_label("stringEqual"),
            vec![un_ex(lhs, gen), un_ex(rhs, gen)],
        ))
    } else {
        let r = gen.new_temp();
        Ex(Eseq(
            Move(
                Temp(r),
                T::external_call(
                    gen.named_label("stringEqual"),
                    vec![un_ex(lhs, gen), un_ex(rhs, gen)],
                ),
            ),
            Binop(Xor, Temp(r), Const(1)),
        ))
    }
}

pub fn call_exp<T: Frame>(
    func: temp::Label,
    caller_level: Rc<RefCell<Level>>,
    args: Vec<TrExp>,
    callee_level: Rc<RefCell<Level>>,
    gen: &mut dyn Uuids,
    is_unit_return_type: bool,
) -> TrExp {
    // here's the cases.
    // callee's parent is the top level
    //    callee is one of the predefined, external call
    // b is a's parent -> pass b's FP.
    // b calls a, a is ancestor (have at least 1 scope between a and b)
    //    keep going up b until including a, building up the static link expression.
    // b calls a, where b == a (special case of above)
    //    pass the static link of a which is caller.fp
    // b calls a, a and b share ancestor
    //    keep going up b until get to frame before a's ancestor, building up the static link expression.

    let mut augmented_args = Vec::new();
    if *callee_level.borrow() == Level::Top {
        // happens iff calling one of the built-in procs.
        // no static link for them because they don't need them!
        for arg in args {
            augmented_args.push(un_ex(arg, gen));
        }
        let fn_name = gen.resolve_label(func);
        if fn_name.is_none() {
            panic!("impl bug, call_exp has unknown fn_name; did you type check this first?");
        }
        let s = String::from(fn_name.unwrap());
        let label = gen.named_label(s.as_str());
        return if is_unit_return_type {
            Nx(Exp(T::external_call(label, augmented_args)))
        } else {
            Ex(T::external_call(label, augmented_args))
        };
    }

    // caller is parent, this can be seen as the "base case" for traversing up the
    // call stack lexically until we hit the callee's parent.
    if *callee_level.borrow().get_parent().borrow() == *caller_level.borrow() {
        // use the parent's frame pointer as static link.
        augmented_args.push(Temp(T::frame_pointer(gen)));
        for arg in args {
            augmented_args.push(un_ex(arg, gen));
        }

        return if is_unit_return_type {
            Nx(Exp(Call(Name(func), augmented_args)))
        } else {
            Ex(Call(Name(func), augmented_args))
        };
    }

    // this is the "recursive" case where we keep going up the caller until the caller's
    // parent is equal to the callee's parent.
    let mut p = caller_level;
    // start at the current frame's FP.
    let mut expr = Temp(T::frame_pointer(gen));
    // keep moving up the call stack lexically until we hit the callee's parent.
    while *p.borrow() != Level::Top && *p.borrow() != *callee_level.borrow().get_parent().borrow() {
        // get the expression to access the static link
        expr = p.borrow().static_link(expr);
        let tmp = p.borrow().get_parent();
        p = tmp;
    }
    augmented_args.push(expr);
    for arg in args {
        augmented_args.push(un_ex(arg, gen));
    }
    return if is_unit_return_type {
        Nx(Exp(Call(Name(func), augmented_args)))
    } else {
        Ex(Call(Name(func), augmented_args))
    };
}

pub fn nil_exp() -> TrExp {
    // Appel appendix:
    // nil denotes a value nil belonging to every record type.
    // if record variable v contains value nil, it is a checked runtime
    // error to select a field from v.
    Ex(Const(0))
}

pub fn int_exp(i: TigerInt) -> TrExp {
    Ex(Const(i))
}

pub fn string_exp(s: &str, gen: &mut dyn Uuids, frags: &mut Vec<frame::Frag>) -> TrExp {
    for frag in frags.iter() {
        match frag {
            frame::Frag::String(label, ..) => {
                return Ex(Name(*label));
            }
            _ => {}
        }
    }
    let l = gen.new_label();
    let new_frag = frame::Frag::String(l, String::from(s));
    frags.push(new_frag);
    Ex(Name(l))
}

pub fn record_exp<T: Frame>(site_irs: Vec<TrExp>, gen: &mut dyn Uuids) -> TrExp {
    let r = gen.new_temp();
    let mut instrs = Vec::new();

    instrs.push(Move(
        Temp(r),
        T::external_call(
            gen.named_label("malloc"),
            vec![Const(
                T::word_size().checked_mul(site_irs.len()).unwrap() as i32
            )],
        ),
    ));
    let mut idx: usize = 0;
    for site_ir in site_irs {
        let offset = idx.checked_mul(T::word_size()).unwrap() as i32;
        instrs.push(Move(
            Mem(Binop(Plus, Temp(r), Const(offset))),
            un_ex(site_ir, gen),
        ));
        idx += 1;
    }
    Ex(Eseq(make_seq(instrs), Temp(r)))
}

pub fn seq_exp(exp_irs: Vec<TrExp>, has_return_value: bool, gen: &mut dyn Uuids) -> TrExp {
    if exp_irs.len() == 0 {
        // for example, an empty let block
        Nx(Exp(Const(0)))
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
                irstms.push(un_nx(iter.next().unwrap(), gen));
            }
            Ex(Eseq(make_seq(irstms), un_ex(iter.next().unwrap(), gen)))
        } else {
            for tr in exp_irs.into_iter() {
                irstms.push(un_nx(tr, gen));
            }
            Nx(make_seq(irstms))
        }
    }
}

pub fn assignment(dst_ir: TrExp, src_ir: TrExp, gen: &mut dyn Uuids) -> TrExp {
    Nx(Move(un_ex(dst_ir, gen), un_ex(src_ir, gen)))
}

pub fn array_exp<T: Frame>(size_ir: TrExp, init_val_ir: TrExp, gen: &mut dyn Uuids) -> TrExp {
    Ex(T::external_call(
        gen.named_label("initArray"),
        vec![un_ex(size_ir, gen), un_ex(init_val_ir, gen)],
    ))
}

pub fn let_exp(var_init_irs: Vec<TrExp>, let_body_ir: TrExp, gen: &mut dyn Uuids) -> TrExp {
    if var_init_irs.len() == 0 {
        return let_body_ir;
    }
    let mut seqs = Vec::new();
    for ir in var_init_irs {
        seqs.push(un_nx(ir, gen));
    }
    if let TrExp::Nx(_) = let_body_ir {
        seqs.push(un_nx(let_body_ir, gen));
        Nx(make_seq(seqs))
    } else {
        Ex(Eseq(make_seq(seqs), un_ex(let_body_ir, gen)))
    }
}

pub fn break_stmt(l: temp::Label) -> TrExp {
    Nx(Jump(Name(l), vec![l]))
}

pub fn for_loop(
    lo_ir: TrExp,
    hi_ir: TrExp,
    body_ir: TrExp,
    for_done_label: temp::Label,
    gen: &mut dyn Uuids,
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
        Move(Temp(i), un_ex(lo_ir, gen)),
        Move(Temp(limit), un_ex(hi_ir, gen)),
        Label(test_label),
        Cjump(Le, Temp(i), Temp(limit), body_label, for_done_label),
        Label(body_label),
        un_nx(body_ir, gen),
        Cjump(Eq, Temp(i), Temp(limit), for_done_label, cont_label),
        Label(cont_label),
        Move(Temp(i), Binop(Plus, Temp(i), Const(1))),
        Jump(Name(test_label), vec![test_label]),
    ]))
}

pub fn while_loop(cond_ir: TrExp, body_ir: TrExp, done: temp::Label, gen: &mut dyn Uuids) -> TrExp {
    let test = gen.new_label();
    let body = gen.new_label();

    Nx(make_seq(vec![
        Label(test),
        un_cx(cond_ir).eval(body, done),
        Label(body),
        un_nx(body_ir, gen),
        Jump(Name(test), vec![test]),
        Label(done),
    ]))
}

#[inline]
fn full_conditional(cond_ir: TrExp, then_ir: TrExp, else_ir: TrExp, gen: &mut dyn Uuids) -> TrExp {
    fn helper(
        branch_ir: TrExp,
        true_cx: temp::Label,
        false_cx: temp::Label,
        done: temp::Label,
        gen: &mut dyn Uuids,
        return_register: IrExp,
    ) -> IrStm {
        match branch_ir {
            TrExp::Cx(..) => un_cx(branch_ir).eval(true_cx, false_cx),
            TrExp::Ex(..) => make_seq(vec![
                Move(return_register, un_ex(branch_ir, gen)),
                Jump(Name(done), vec![done]),
            ]),
            TrExp::Nx(..) => make_seq(vec![un_nx(branch_ir, gen), Jump(Name(done), vec![done])]),
        }
    }

    let true_branch_label = gen.new_label();
    let false_branch_label = gen.new_label();
    let true_cx_branch_label = gen.new_label();
    let false_cx_branch_label = gen.new_label();
    let done = gen.new_label();
    let r = gen.new_temp();

    match (&then_ir, &else_ir) {
        (TrExp::Nx(..), TrExp::Nx(..)) => {
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
        (TrExp::Nx(..), _) | (_, TrExp::Nx(..)) => {
            panic!("impl bug, conditional ir generation should be invoked on if-then-else branch with the same return types on both branches. got then={:#?}, else={:#?}", then_ir, else_ir);
        }
        (TrExp::Cx(..), _) | (_, TrExp::Cx(..)) => {
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
            Ex(Eseq(
                make_seq(vec![
                    un_cx(cond_ir).eval(true_branch_label, false_branch_label),
                    Label(true_branch_label),
                    true_branch_stmt,
                    Label(false_branch_label),
                    false_branch_stmt,
                    Label(false_cx_branch_label),
                    Move(Temp(r), Const(0)),
                    Jump(Name(done), vec![done]),
                    Label(true_cx_branch_label),
                    Move(Temp(r), Const(1)),
                    Jump(Name(done), vec![done]),
                    Label(done),
                ]),
                Temp(r),
            ))
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

            Ex(Eseq(
                make_seq(vec![
                    un_cx(cond_ir).eval(true_branch_label, false_branch_label),
                    Label(true_branch_label),
                    true_branch_stmt,
                    Label(false_branch_label),
                    false_branch_stmt,
                    Label(done),
                ]),
                Temp(r),
            ))
        }
    }
}

pub fn conditional(
    cond_ir: TrExp,
    then_ir: TrExp,
    else_ir: Option<TrExp>,
    gen: &mut dyn Uuids,
) -> TrExp {
    if else_ir.is_none() {
        let t = gen.new_label();
        let f = gen.new_label();
        Nx(make_seq(vec![
            un_cx(cond_ir).eval(t, f),
            Label(t),
            un_nx(then_ir, gen),
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
    gen: &mut dyn Uuids,
) -> TrExp {
    // given the level the var is declared in, our job is to come up with
    // an expression to "travel up" the stack until we get to the nearest
    // stack record of that level.

    let var_declaration_level = access.0;
    let cur_level = current_level.clone();

    // start at the static link which is what the frame pointer points to.
    let mut access_expr = Temp(T::frame_pointer(gen));

    // TODO fix me
    while *var_declaration_level.borrow() != *cur_level.borrow() {
        access_expr = cur_level.borrow().static_link(access_expr);
    }

    let frame_access = access.1;
    match frame_access {
        frame::Access::InReg(reg) => {
            if *var_declaration_level.borrow() != *current_level.borrow() {
                // note we are comparing the original input current level against
                // the level where the var being accessed is declared, NOT the one
                // we have been bashing above.
                //
                // this is a bug because we are accessing a variable declared
                // outside the current function and yet it is assigned to a register.
                panic!("impl bug: a variable is InReg but it is accessed in a nested function");
            }
            // this is only possible in the non-nested access.
            Ex(Temp(reg))
        }
        frame::Access::InFrame(x_offset) => Ex(Mem(Binop(Plus, Const(x_offset), access_expr))),
    }
}

pub fn record_field<T: Frame>(lhs_var_ir: TrExp, field_pos: usize, gen: &mut dyn Uuids) -> TrExp {
    Ex(Mem(Binop(
        Plus,
        un_ex(lhs_var_ir, gen),
        // hmm, this could potentially overflow in extreme edge case.
        Const((field_pos * T::word_size()) as i32),
    )))
}

const INVALID_ARRAY_ACCESS_EXIT_CODE: i32 = -1;
pub fn subscript_var<T: Frame>(lhs_ir: TrExp, idx_ir: TrExp, gen: &mut dyn Uuids) -> TrExp {
    // byte offset of idx+1 basically.
    let idx = Binop(
        Plus,
        Const(T::word_size() as i32),
        Binop(Mul, un_ex(idx_ir, gen), Const(T::word_size() as i32)),
    );
    let bad = gen.new_label();
    let upper_check = gen.new_label();
    let access = gen.new_label();
    let lhs_unexed = un_ex(lhs_ir, gen);
    Ex(Eseq(
        make_seq(vec![
            Cjump(Ge, idx.clone(), Const(0), upper_check, bad),
            Label(upper_check),
            Cjump(Lt, idx.clone(), lhs_unexed.clone(), access, bad),
            Label(bad),
            Exp(T::external_call(
                gen.named_label("exit"),
                vec![Const(INVALID_ARRAY_ACCESS_EXIT_CODE)],
            )),
            Label(access),
        ]),
        Mem(Binop(Plus, lhs_unexed, idx)),
    ))
}

pub fn proc_entry_exit(
    level: Rc<RefCell<Level>>,
    body: TrExp,
    frags: &mut Vec<frame::Frag>,
    gen: &mut dyn Uuids,
) {
    match &*level.borrow() {
        Level::Top => panic!("impl bug, proc_entry_exit cannot be used on Top level"),
        Level::Nested { frame, .. } => {
            let augmented = frame.borrow().proc_entry_exit1(un_nx(body, gen));
            frags.push(frame::Frag::Proc {
                body: augmented,
                frame: frame.clone(),
            });
        }
    }
}

macro_rules! new_frame {
    ($x: expr) => {
        Rc::new(RefCell::new($x))
    };
}

impl From<Level> for Rc<RefCell<Level>> {
    fn from(value: Level) -> Self {
        Rc::new(RefCell::new(value))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        frame::{Escapes, TempMap},
        ir,
        symtab::SymbolTable,
        temp::{test_helpers, Uuids, UuidsImpl},
    };

    #[derive(Debug)]
    struct TestFrame {
        name: temp::Label,
        formals: Vec<frame::Access>,
        local_offset: i32,
    }

    const FP: &str = "fp";

    impl Frame for TestFrame {
        fn temp_map(gen: &mut dyn Uuids) -> frame::TempMap
        where
            Self: Sized,
        {
            todo!()
        }

        fn external_call(_: temp::Label, _: Vec<crate::ir::IrExp>) -> crate::ir::IrExp
        where
            Self: Sized,
        {
            IrExp::Const(42)
        }

        fn word_size() -> usize
        where
            Self: Sized,
        {
            4
        }

        fn registers<'a>() -> &'a [frame::Register<'a>]
        where
            Self: Sized,
        {
            &[FP]
        }

        fn string(_: temp::Label, _: &str) -> String
        where
            Self: Sized,
        {
            todo!()
        }

        fn frame_pointer(g: &mut dyn Uuids) -> temp::Temp
        where
            Self: Sized,
        {
            g.named_temp(FP)
        }

        fn proc_entry_exit1(&self, _: IrStm) -> IrStm {
            todo!()
            // IrStm::Exp(Box::new(IrExp::Const(42)))
        }

        fn proc_entry_exit2()
        where
            Self: Sized,
        {
            todo!()
        }

        fn proc_entry_exit3()
        where
            Self: Sized,
        {
            todo!()
        }

        fn new(name: temp::Label, formals: Vec<Escapes>, g: &mut dyn Uuids) -> Self {
            TestFrame {
                name,
                formals: formals
                    .iter()
                    .map(|esc| {
                        if *esc {
                            frame::Access::InReg(g.new_temp())
                        } else {
                            todo!()
                        }
                    })
                    .collect(),
                local_offset: -4,
            }
        }
        fn name(&self) -> temp::Label {
            self.name
        }

        fn formals(&self) -> &[frame::Access] {
            &self.formals[..]
        }
        fn alloc_local(&mut self, _: frame::Escapes, _: &mut dyn Uuids) -> frame::Access {
            frame::Access::InFrame(42)
        }
    }

    #[test]
    fn test_level_equality() {
        let name1 = temp::test_helpers::new_label(100);
        let name2 = temp::test_helpers::new_label(200);
        let mut gen = UuidsImpl::new();
        let level1 = Level::Nested {
            parent: Level::Top.into(),
            frame: new_frame!(TestFrame::new(name1, vec![], &mut gen)),
        };
        let level1too = Level::Nested {
            parent: Level::Top.into(),
            frame: new_frame!(TestFrame::new(name1, vec![], &mut gen)),
        };
        assert_eq!(level1, level1too);

        let level2 = Level::Nested {
            parent: Rc::new(RefCell::new(Level::Top)),
            frame: Rc::new(RefCell::new(TestFrame::new(name2, vec![], &mut gen))),
        };
        assert!(level1 != level2);
        assert!(level1too != level2);
    }

    #[test]
    #[should_panic]
    fn static_link_on_top_level_craps_out() {
        Level::Top.static_link(IrExp::Temp(test_helpers::new_temp(0)));
    }

    #[test]
    #[should_panic]
    fn static_link_craps_out_if_in_reg() {
        let name2 = temp::test_helpers::new_label(200);
        let escapes = false;
        let mut gen = UuidsImpl::new();
        let frame = new_frame!(TestFrame::new(name2, vec![escapes], &mut gen));
        let l = Level::Nested {
            parent: Level::Top.into(),
            frame,
        };
        l.static_link(IrExp::Temp(test_helpers::new_temp(1)));
    }

    #[test] // TODO
    fn static_link_nested_level_0_offset() {
        let name2 = temp::test_helpers::new_label(200);
        let escapes = true;
        let mut gen = UuidsImpl::new();
        let frame = new_frame!(TestFrame::new(name2, vec![escapes], &mut gen));
        let l = Level::Nested {
            parent: Level::Top.into(),
            frame,
        };
    }

    #[test]
    fn static_link_nested_level_nonzero_offset() {}

    fn simple_var() {}

    fn call_exp() {}
}
