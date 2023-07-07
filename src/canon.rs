use crate::{
    ir::IrExp,
    ir::IrExp::*,
    ir::IrStm,
    ir::IrStm::*,
    temp::{GenTemporary, Label},
};

use std::collections::VecDeque;

#[inline]
fn nop() -> IrStm {
    Exp(Box::new(Const(0)))
}

impl std::ops::Rem for IrStm {
    type Output = IrStm;

    fn rem(self, rhs: Self) -> IrStm {
        Seq(Box::new(self), Box::new(rhs))
    }
}

fn reorder(mut ev: VecDeque<IrExp>, gen: &mut GenTemporary) -> (IrStm, VecDeque<IrExp>) {
    if ev.is_empty() {
        (nop(), VecDeque::with_capacity(0))
    } else {
        match ev[0] {
            Call(..) => {
                let t = gen.new_temp();
                let e0 = ev.pop_front().unwrap();
                let eseq = Eseq(
                    Box::new(Move(Box::new(Temp(t)), Box::new(e0))),
                    Box::new(Temp(t)),
                );
                ev.push_front(eseq);
                reorder(ev, gen)
            }
            _ => {
                let e0 = ev.pop_front().unwrap();
                let (stmt, e) = lift_exp(e0, gen);
                let (stmts_rest, mut e_rest) = reorder(ev, gen);
                if commutes(&stmts_rest, &e) {
                    e_rest.push_front(e);
                    (stmt % stmts_rest, e_rest)
                } else {
                    let t = gen.new_temp();
                    e_rest.push_front(Temp(t));
                    (
                        stmt % Move(Box::new(Temp(t)), Box::new(e)) % stmts_rest,
                        e_rest,
                    )
                }
            }
        }
    }
}

fn reorder_exp<F>(ev: VecDeque<IrExp>, make: F, gen: &mut GenTemporary) -> (IrStm, IrExp)
where
    F: FnOnce(VecDeque<IrExp>) -> IrExp,
{
    let (s, ee) = reorder(ev, gen);
    (s, make(ee))
}

fn reorder_stm<F>(ev: VecDeque<IrExp>, make: F, gen: &mut GenTemporary) -> IrStm
where
    F: FnOnce(VecDeque<IrExp>) -> IrStm,
{
    let (s, ee) = reorder(ev, gen);
    s % make(ee)
}

fn commutes(s: &IrStm, e: &IrExp) -> bool {
    // Whether s can be evaluated ahead of e without causing side effects that
    // changes the value of e.
    match (s, e) {
        (Exp(x), _) if matches!(x.as_ref(), Const(..)) => true,
        (_, Name(..)) => true,
        (_, Const(..)) => true,
        (_, _) => false,
    }
}

fn lift_stm(s: IrStm, gen: &mut GenTemporary) -> IrStm {
    match s {
        Seq(a, b) => lift_stm(*a, gen) % lift_stm(*b, gen),
        Jump(e, labs) => reorder_stm(
            VecDeque::from(vec![*e]),
            |mut ev| Jump(Box::new(ev.pop_front().unwrap()), labs),
            gen,
        ),
        Cjump(p, a, b, t, f) => reorder_stm(
            VecDeque::from(vec![*a, *b]),
            |mut ev| {
                Cjump(
                    p.clone(),
                    Box::new(ev.pop_front().unwrap()),
                    Box::new(ev.pop_front().unwrap()),
                    t,
                    f,
                )
            },
            gen,
        ),
        Move(dst, src) => {
            // lack of box matching forces this shitty nested match
            match (*dst, *src) {
                (Temp(t), Call(e, e1)) => {
                    let mut vd = VecDeque::from(e1);
                    vd.push_front(*e);
                    reorder_stm(
                        vd,
                        |mut ev| {
                            Move(
                                Box::new(Temp(t)),
                                Box::new(Call(Box::new(ev.pop_front().unwrap()), Vec::from(ev))),
                            )
                        },
                        gen,
                    )
                }
                (Temp(t), b) => reorder_stm(
                    VecDeque::from(vec![b]),
                    |mut ev| Move(Box::new(Temp(t)), Box::new(ev.pop_front().unwrap())),
                    gen,
                ),
                (Mem(e), b) => reorder_stm(
                    VecDeque::from(vec![*e, b]),
                    |mut ev| {
                        Move(
                            Box::new(Mem(Box::new(ev.pop_front().unwrap()))),
                            Box::new(ev.pop_front().unwrap()),
                        )
                    },
                    gen,
                ),
                (Eseq(s, e), src) => lift_stm(Seq(s, Box::new(Move(e, Box::new(src)))), gen),
                (a, b) => reorder_stm(VecDeque::with_capacity(0), |_| Move(Box::new(a), Box::new(b)), gen),
            }
        }
        Exp(ebox) => match *ebox {
            Call(e, mut el) => {
                let mut vd = VecDeque::from(el);
                vd.push_front(*e);
                reorder_stm(
                    vd,
                    |mut ev| {
                        Exp(Box::new(Call(
                            Box::new(ev.pop_front().unwrap()),
                            Vec::from(ev),
                        )))
                    },
                    gen,
                )
            }
            e => reorder_stm(
                VecDeque::from(vec![e]),
                |mut ev| Exp(Box::new(ev.pop_front().unwrap())),
                gen,
            ),
        },
        s => reorder_stm(VecDeque::with_capacity(0), |_| s, gen),
    }
}

fn lift_exp(e: IrExp, gen: &mut GenTemporary) -> (IrStm, IrExp) {
    match e {
        Binop(p, a, b) => reorder_exp(
            VecDeque::from(vec![*a, *b]),
            |mut ev| {
                Binop(
                    p.clone(),
                    Box::new(ev.pop_front().unwrap()),
                    Box::new(ev.pop_front().unwrap()),
                )
            },
            gen,
        ),
        Mem(a) => reorder_exp(
            VecDeque::from(vec![*a]),
            |mut ev| Mem(Box::new(ev.pop_front().unwrap())),
            gen,
        ),
        Eseq(s, e) => {
            let stmt = lift_stm(*s, gen);
            let (stmts, ee) = lift_exp(*e, gen);
            (stmt % stmts, ee)
        }
        Call(e, el) => {
            let mut vd = VecDeque::from(el);
            vd.push_front(*e);
            reorder_exp(
                vd,
                |mut ev| Call(Box::new(ev.pop_front().unwrap()), Vec::from(ev)),
                gen,
            )
        }
        e => reorder_exp(VecDeque::with_capacity(0), |_| e, gen),
    }
}

pub fn linearize(i: IrStm, gen: &mut GenTemporary) -> Vec<IrStm> {
    fn helper(i: IrStm, mut rest: Vec<IrStm>) -> Vec<IrStm> {
        match i {
            Seq(a, b) => helper(*a, helper(*b, rest)),
            _ => {
                rest.insert(0, i);
                rest
            }
        }
    }
    helper(lift_stm(i, gen), Vec::new())
}

pub fn basic_blocks(_: Vec<IrStm>) -> (Vec<Vec<IrStm>>, Label) {
    todo!()
}

pub fn trace_schedule(_: Vec<Vec<IrStm>>, _: Label) -> Vec<IrStm> {
    todo!()
}
