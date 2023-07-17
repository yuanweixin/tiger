use crate::ir::helpers::*;
use crate::{
    absyn::types::{Oper, Oper::*},
    frame,
    frame::{Frame, FrameRef},
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

macro_rules! new_frame {
    ($x: expr) => {
        Rc::new(RefCell::new($x))
    };
}

type LevelRef = Rc<RefCell<Level>>;

#[derive(Debug)]
pub enum Level {
    Top,
    Nested {
        // use Rc because the Level objects form a dag where the child levels point back at the parent levels.
        // this whole mechanism just to be able to mutate some shit is fucking crazy.
        parent: LevelRef,
        frame: FrameRef,
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
pub struct Access(pub LevelRef, pub frame::Access);

#[derive(Eq, PartialEq, Debug, Clone)]
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

#[derive(Eq, PartialEq, Debug, Clone)]
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
    fn current_frame<T: Frame>(gen: &mut dyn Uuids) -> IrExp {
        Temp(T::frame_pointer(gen))
    }

    fn parent_frame(&self, base: IrExp) -> IrExp {
        match self {
            Level::Top => {
                panic!("impl bug: Top level has no static link");
            }
            Level::Nested { frame, .. } => {
                let static_link_param_access = frame.borrow().formals()[0].clone();
                match static_link_param_access {
                    frame::Access::InReg(..) => {
                        panic!("impl bug, static link should always have Access of InFrame")
                    }
                    frame::Access::InFrame(static_link_offset) => {
                        if static_link_offset == 0 {
                            Mem(base)
                        } else {
                            Mem(Binop(Plus, Const(static_link_offset), base))
                        }
                    }
                }
            }
        }
    }

    pub fn outermost() -> Rc<RefCell<Self>> {
        Level::Top.into()
    }

    pub fn alloc_local(myself: LevelRef, escape: bool, gen: &mut dyn Uuids) -> Access {
        let frame_access = match *myself.borrow_mut() {
            Level::Top => {
                panic!("impl bug, cannot allocate local in top level");
            }
            Level::Nested { ref frame, .. } => frame.borrow_mut().alloc_local(escape, gen),
        };
        Access(myself, frame_access)
    }

    pub fn new_level<T: Frame + 'static>(
        parent: LevelRef,
        mut escapes: Vec<bool>,
        gen: &mut dyn Uuids,
        fun_name: &str,
    ) -> (LevelRef, temp::Label) {
        // prepend true for the static link
        escapes.insert(0, true);
        let function_label = gen.named_label(fun_name);
        (
            Level::Nested {
                parent: parent.clone(),
                frame: new_frame!(T::new(function_label, escapes, gen,)),
            }
            .into(),
            function_label,
        )
    }

    fn get_parent(&self) -> LevelRef {
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

pub fn make_seq(stmts: Vec<IrStm>) -> IrStm {
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
            let t = gen.new_unnamed_label();
            let f = gen.new_unnamed_label();
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
            let l = gen.new_unnamed_label();
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
    caller_level: LevelRef,
    args: Vec<TrExp>,
    callee_level: LevelRef,
    gen: &mut dyn Uuids,
    is_unit_return_type: bool,
) -> TrExp {
    // here's the cases.
    // callee's parent is the top level
    //    callee is one of the predefined, external call
    // b is a's parent -> pass b's FP.
    // b calls a, a is ancestor (have at least 1 frame between a and b)
    //    keep going up b until including a, building up the static link expression.
    // b calls a, where b == a (special case of above)
    //    pass the static link of a which is caller.fp
    // b calls a, a and b share ancestor
    //    keep going up b until get to frame before a's ancestor, building up the static link expression.

    let mut augmented_args = Vec::new();
    if *callee_level.borrow().get_parent().borrow() == Level::Top {
        // happens iff calling one of the built-in procs.
        // no static link for them because they don't need them!
        for arg in args {
            augmented_args.push(un_ex(arg, gen));
        }
        return if is_unit_return_type {
            Nx(Exp(T::external_call(func, augmented_args)))
        } else {
            Ex(T::external_call(func, augmented_args))
        };
    }

    // caller is parent, this can be seen as the "base case" for traversing up the
    // call stack lexically until we hit the callee's parent.
    if *callee_level.borrow().get_parent().borrow() == *caller_level.borrow() {
        // use the parent's frame (value of frame pointer) as static link.
        augmented_args.push(
            callee_level
                .borrow()
                .parent_frame(Temp(T::frame_pointer(gen))),
        );
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
    let mut expr = Level::current_frame::<T>(gen);
    // keep moving up the call stack lexically until we hit the callee's parent.
    while *p.borrow() != Level::Top &&
            *p.borrow() != *callee_level.borrow().get_parent().borrow() {
        expr = p.borrow().parent_frame(expr);
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
    let l = gen.new_unnamed_label();
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
    let test_label = gen.new_unnamed_label();
    let body_label = gen.new_unnamed_label();
    let cont_label = gen.new_unnamed_label();
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
    let test = gen.new_unnamed_label();
    let body = gen.new_unnamed_label();

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

    let true_branch_label = gen.new_unnamed_label();
    let false_branch_label = gen.new_unnamed_label();
    let true_cx_branch_label = gen.new_unnamed_label();
    let false_cx_branch_label = gen.new_unnamed_label();
    let done = gen.new_unnamed_label();
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
        let t = gen.new_unnamed_label();
        let f = gen.new_unnamed_label();
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
    definition_level_access: Access, // contains the level at which var is defined, as well as the variable's access in the frame
    use_level: LevelRef,             // where the var is being used
    gen: &mut dyn Uuids,
) -> TrExp {
    // given the level the var is declared in, our job is to come up with
    // an expression to "travel up" the stack until we get to the nearest
    // stack record of that level.

    let var_declaration_level = definition_level_access.0;
    let mut cur_level = use_level.clone();

    // start at the static link which is what the frame pointer points to.
    let mut access_expr = Level::current_frame::<T>(gen);

    while *var_declaration_level.borrow() != *cur_level.borrow() {
        access_expr = cur_level.borrow().parent_frame(access_expr);
        let p = cur_level.borrow().get_parent();
        cur_level = p;
    }

    let frame_access = definition_level_access.1;
    match frame_access {
        frame::Access::InReg(reg) => {
            if *var_declaration_level.borrow() != *use_level.borrow() {
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
    let bad = gen.new_unnamed_label();
    let upper_check = gen.new_unnamed_label();
    let access = gen.new_unnamed_label();
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
    level: LevelRef,
    body: TrExp,
    frags: &mut Vec<frame::Frag>,
    gen: &mut dyn Uuids,
    can_spill: bool
) {
    match &*level.borrow() {
        Level::Top => panic!("impl bug, proc_entry_exit cannot be used on Top level"),
        Level::Nested { frame, .. } => {
            let augmented = frame.borrow_mut().proc_entry_exit1(un_nx(body, gen), can_spill, gen);
            frags.push(frame::Frag::Proc {
                body: augmented,
                frame: frame.clone(),
            });
        }
    }
}

impl From<Level> for LevelRef {
    fn from(value: Level) -> Self {
        Rc::new(RefCell::new(value))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        frame::Escapes,
        ir::helpers::*,
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
        fn temp_map(_: &mut dyn Uuids) -> temp::TempMap
        where
            Self: Sized,
        {
            todo!()
        }

        fn external_call(func: temp::Label, args: Vec<crate::ir::IrExp>) -> crate::ir::IrExp
        where
            Self: Sized,
        {
            Call(Name(func), args)
        }

        fn word_size() -> usize
        where
            Self: Sized,
        {
            4
        }

        fn registers() -> &'static [frame::Register]
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

        fn proc_entry_exit1(&mut self, _: IrStm, _: bool, _: &mut dyn Uuids) -> IrStm {
            todo!()
        }

        fn proc_entry_exit2(&self, _: &mut Vec<crate::assem::Instr>, _: &mut dyn Uuids) {
            todo!()
        }

        fn proc_entry_exit3(&self, _: &Vec<crate::assem::Instr>, _: &mut dyn Uuids) -> (frame::Prologue, frame::Epilogue) {
            todo!()
        }

        fn new(name: temp::Label, formals: Vec<Escapes>, g: &mut dyn Uuids) -> Self {
            let mut formals_accesses = Vec::with_capacity(formals.len());
            let mut offset = 0;
            for esc in formals {
                if esc {
                    formals_accesses.push(frame::Access::InFrame(offset));
                    offset += 4;
                } else {
                    formals_accesses.push(frame::Access::InReg(g.new_temp()));
                }
            }
            TestFrame {
                name,
                formals: formals_accesses,
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
            let offset = self.local_offset;
            self.local_offset -= 4;
            frame::Access::InFrame(offset)
        }
    }

    #[test]
    fn test_level_equality() {
        let name1 = temp::test_helpers::new_unnamed_label(100);
        let name2 = temp::test_helpers::new_unnamed_label(200);
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
            parent: Level::Top.into(),
            frame: new_frame!(TestFrame::new(name2, vec![], &mut gen)),
        };
        assert!(level1 != level2);
        assert!(level1too != level2);

        assert_eq!(Level::Top, Level::Top);
    }

    #[test]
    #[should_panic]
    fn parent_frame_on_top_level_craps_out() {
        Level::Top.parent_frame(Temp(test_helpers::new_unnamed_temp(0)));
    }

    #[test]
    fn parent_frame_nested_level_0_offset() {
        let mut gen = UuidsImpl::new();
        let (lvl, _) =
            Level::new_level::<TestFrame>(Level::Top.into(), vec![], &mut gen, "somefunc");
        let actual = lvl
            .borrow()
            .parent_frame(Temp(TestFrame::frame_pointer(&mut gen)));
        let expected = Mem(Temp(TestFrame::frame_pointer(&mut gen)));
        assert_eq!(expected, actual);
    }

    #[test]
    fn static_link_nested_level_nonzero_offset() {
        let name1 = temp::test_helpers::new_unnamed_label(100);
        let mut gen = UuidsImpl::new();
        let mut frame = TestFrame::new(name1, vec![true], &mut gen);
        frame.formals = vec![frame::Access::InFrame(8)];
        let lvl = Level::Nested {
            parent: Level::Top.into(),
            frame: new_frame!(frame),
        };
        let actual = lvl.parent_frame(Temp(TestFrame::frame_pointer(&mut gen)));
        let expected = Mem(Binop(
            Plus,
            Const(8),
            Temp(TestFrame::frame_pointer(&mut gen)),
        ));
        assert_eq!(expected, actual);
    }

    #[test]
    fn simple_var_current_level() {
        let mut gen: UuidsImpl = Uuids::new();
        let name1 = temp::test_helpers::new_unnamed_label(100);

        let var_escapes = true;
        let frame = TestFrame::new(name1, vec![true, var_escapes], &mut gen);
        let var_access = frame.formals()[1].clone();
        let def_level: LevelRef = Level::Nested {
            parent: Level::Top.into(),
            frame: new_frame!(frame),
        }
        .into();

        assert_eq!(frame::Access::InFrame(4), var_access);

        let actual = simple_var::<TestFrame>(
            Access(def_level.clone(), var_access),
            def_level.clone(),
            &mut gen,
        );

        // FP = this frame
        // at offset 4, so it is [4 + FP]
        let expected = Ex(Mem(Binop(
            Plus,
            Const(4),
            Temp(TestFrame::frame_pointer(&mut gen)),
        )));
        assert_eq!(expected, actual);
    }

    #[test]
    fn simple_var_in_parent_level() {
        let mut gen: UuidsImpl = Uuids::new();
        let name1 = temp::test_helpers::new_unnamed_label(100);

        let var_escapes = true;
        let frame = TestFrame::new(name1, vec![true, var_escapes], &mut gen);
        let var_access = frame.formals()[1].clone();
        // rust is fucking verbose and type inference sucks
        let def_level: LevelRef = Level::Nested {
            parent: Level::Top.into(),
            frame: new_frame!(frame),
        }
        .into();

        let (parent_one, _) =
            Level::new_level::<TestFrame>(def_level.clone(), vec![], &mut gen, "somefunc2");
        let (use_level, _) =
            Level::new_level::<TestFrame>(parent_one.clone(), vec![], &mut gen, "somefunc1");

        assert_eq!(frame::Access::InFrame(4), var_access);

        let actual =
            simple_var::<TestFrame>(Access(def_level.clone(), var_access), use_level, &mut gen);

        // FP = this frame
        // [FP] = parent
        // [[FP]] = parent parent (what we want)
        // at offset 4, so it is [4 + [[FP]]]
        let expected = Ex(Mem(Binop(
            Plus,
            Const(4),
            Mem(Mem(Temp(TestFrame::frame_pointer(&mut gen)))),
        )));
        assert_eq!(expected, actual);
    }

    #[test]
    fn call_exp_top_level() {
        let mut gen: UuidsImpl = Uuids::new();
        let is_unit_return_type = true;
        let func = gen.named_label("somefun");
        let args = vec![Ex(Const(0)), Ex(Const(1))];

        let caller_level: LevelRef = Level::Top.into();
        let (callee_level, _) = Level::new_level::<TestFrame>(
            caller_level.clone(),
            vec![false, false],
            &mut gen,
            "somefun",
        );

        let actual = call_exp::<TestFrame>(
            func,
            caller_level,
            args,
            callee_level,
            &mut gen,
            is_unit_return_type,
        );
        let expected = Nx(Exp(Call(Name(func), vec![Const(0), Const(1)])));
        assert_eq!(expected, actual);
    }
    #[test]
    fn call_exp_parent_calls_child() {
        let mut gen: UuidsImpl = Uuids::new();
        let is_unit_return_type = true;
        let args = vec![Ex(Const(0)), Ex(Const(1))];

        let (caller_level, _) =
            Level::new_level::<TestFrame>(Level::Top.into(), vec![], &mut gen, "test_parent");
        let (callee_level, func) = Level::new_level::<TestFrame>(
            caller_level.clone(),
            vec![false, false],
            &mut gen,
            "child",
        );

        let actual = call_exp::<TestFrame>(
            func,
            caller_level,
            args,
            callee_level,
            &mut gen,
            is_unit_return_type,
        );
        // being called by your parent, so the static link to your parent should be first argument.
        let expected = Nx(Exp(Call(
            Name(func),
            vec![
                Mem(Temp(TestFrame::frame_pointer(&mut gen))),
                Const(0),
                Const(1),
            ],
        )));
        assert_eq!(expected, actual);
    }

    #[test]
    fn call_exp_self_call() {
        let mut gen: UuidsImpl = Uuids::new();
        let is_unit_return_type = true;
        let args = vec![Ex(Const(0)), Ex(Const(1))];

        let (parent_level, _) =
            Level::new_level::<TestFrame>(Level::Top.into(), vec![], &mut gen, "test_parent");
        let (callee_level, func) = Level::new_level::<TestFrame>(
            parent_level.clone(),
            vec![false, false],
            &mut gen,
            "child",
        );

        let actual = call_exp::<TestFrame>(
            func,
            callee_level.clone(),
            args,
            callee_level,
            &mut gen,
            is_unit_return_type,
        );
        // being called by yourself, so the static link to parent should be first argument.
        let expected = Nx(Exp(Call(
            Name(func),
            vec![
                Mem(Temp(TestFrame::frame_pointer(&mut gen))),
                Const(0),
                Const(1),
            ],
        )));
        assert_eq!(expected, actual);
    }

    #[test]
    fn call_exp_callee_is_own_ancestor() {
        let mut gen: UuidsImpl = Uuids::new();
        let is_unit_return_type = true;
        let args = vec![Ex(Const(0)), Ex(Const(1))];

        let (parent_parent, _) =
            Level::new_level::<TestFrame>(Level::Top.into(), vec![], &mut gen, "main");
        let (parent_level, _) =
            Level::new_level::<TestFrame>(parent_parent, vec![], &mut gen, "f");
        let (callee_level, func) = Level::new_level::<TestFrame>(
            parent_level.clone(),
            vec![false, false],
            &mut gen,
            "g",
        );

        let actual = call_exp::<TestFrame>(
            func,
            callee_level,
            args,
            parent_level,
            &mut gen,
            is_unit_return_type,
        );
        let expected = Nx(Exp(Call(
            Name(func),
            vec![
                // here, g calls f, but f is actually g's parent.
                // so we actually use static link to f's parent, which in this case is 2 levels up.
                Mem(Mem(Temp(TestFrame::frame_pointer(&mut gen)))),
                Const(0),
                Const(1),
            ],
        )));
        assert_eq!(expected, actual);
    }
    #[test]
    fn call_exp_callee_caller_common_ancestor() {
        let mut gen: UuidsImpl = Uuids::new();
        let is_unit_return_type = true;
        let args = vec![Ex(Const(0)), Ex(Const(1))];

        let (common_parent, _) =
            Level::new_level::<TestFrame>(Level::Top.into(), vec![], &mut gen, "main");
        let (peer_a, _) =
            Level::new_level::<TestFrame>(common_parent.clone(), vec![], &mut gen, "a");
        let (peer_b, func) = Level::new_level::<TestFrame>(
            common_parent.clone(),
            vec![false, false],
            &mut gen,
            "b",
        );
        // c will call a
        let (c_inside_b, _) = Level::new_level::<TestFrame>(
            peer_b.clone(),
            vec![],
            &mut gen,
            "c",
        );

        let actual = call_exp::<TestFrame>(
            func,
            c_inside_b,
            args,
            peer_a,
            &mut gen,
            is_unit_return_type,
        );
        let expected = Nx(Exp(Call(
            Name(func),
            vec![
                // c -> b -> main (2 levels)
                Mem(Mem(Temp(TestFrame::frame_pointer(&mut gen)))),
                Const(0),
                Const(1),
            ],
        )));
        assert_eq!(expected, actual);
    }
}
