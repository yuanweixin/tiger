use crate::{
    ir::IrExp,
    ir::IrExp::*,
    ir::IrStm,
    ir::{IrRelop, IrRelop::*, IrStm::*},
    temp,
    temp::GenTemporary,
};

use std::{
    collections::{HashMap, VecDeque},
    iter::Peekable,
};

#[inline]
fn nop(nop_marker_label: temp::Label) -> IrStm {
    Label(nop_marker_label)
}

impl std::ops::Rem for IrStm {
    type Output = IrStm;

    fn rem(self, rhs: Self) -> IrStm {
        Seq(Box::new(self), Box::new(rhs))
    }
}

fn reorder(
    mut ev: VecDeque<IrExp>,
    gen: &mut GenTemporary,
    nop_marker_label: temp::Label,
) -> (IrStm, VecDeque<IrExp>) {
    if ev.is_empty() {
        // note: this can insert a bunch of spurious nop statements.
        (nop(nop_marker_label), VecDeque::with_capacity(0))
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
                reorder(ev, gen, nop_marker_label)
            }
            _ => {
                let e0 = ev.pop_front().unwrap();
                let (stmt, e) = lift_exp(e0, gen, nop_marker_label);
                if ev.is_empty() {
                    // there's nothing more to do, just return to avoid generating nop's.
                    return (stmt, VecDeque::from(vec![e]));
                }
                let (stmts_rest, mut e_rest) = reorder(ev, gen, nop_marker_label);
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

fn reorder_exp<F>(
    ev: VecDeque<IrExp>,
    make: F,
    gen: &mut GenTemporary,
    nop_marker_label: temp::Label,
) -> (IrStm, IrExp)
where
    F: FnOnce(VecDeque<IrExp>) -> IrExp,
{
    let (s, ee) = reorder(ev, gen, nop_marker_label);
    (s, make(ee))
}

fn reorder_stm<F>(
    ev: VecDeque<IrExp>,
    make: F,
    gen: &mut GenTemporary,
    nop_marker_label: temp::Label,
) -> IrStm
where
    F: FnOnce(VecDeque<IrExp>) -> IrStm,
{
    let (s, ee) = reorder(ev, gen, nop_marker_label);
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

fn lift_stm(s: IrStm, gen: &mut GenTemporary, nop_marker_label: temp::Label) -> IrStm {
    match s {
        Seq(a, b) => lift_stm(*a, gen, nop_marker_label) % lift_stm(*b, gen, nop_marker_label),
        Jump(e, labs) => reorder_stm(
            VecDeque::from(vec![*e]),
            |mut ev| Jump(Box::new(ev.pop_front().unwrap()), labs),
            gen,
            nop_marker_label,
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
            nop_marker_label,
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
                        nop_marker_label,
                    )
                }
                (Temp(t), b) => reorder_stm(
                    VecDeque::from(vec![b]),
                    |mut ev| Move(Box::new(Temp(t)), Box::new(ev.pop_front().unwrap())),
                    gen,
                    nop_marker_label,
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
                    nop_marker_label,
                ),
                (Eseq(s, e), src) => lift_stm(
                    Seq(s, Box::new(Move(e, Box::new(src)))),
                    gen,
                    nop_marker_label,
                ),
                (a, b) => reorder_stm(
                    VecDeque::with_capacity(0),
                    |_| Move(Box::new(a), Box::new(b)),
                    gen,
                    nop_marker_label,
                ),
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
                    nop_marker_label,
                )
            }
            e => reorder_stm(
                VecDeque::from(vec![e]),
                |mut ev| Exp(Box::new(ev.pop_front().unwrap())),
                gen,
                nop_marker_label,
            ),
        },
        s => reorder_stm(VecDeque::with_capacity(0), |_| s, gen, nop_marker_label),
    }
}

fn lift_exp(e: IrExp, gen: &mut GenTemporary, nop_marker_label: temp::Label) -> (IrStm, IrExp) {
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
            nop_marker_label,
        ),
        Mem(a) => reorder_exp(
            VecDeque::from(vec![*a]),
            |mut ev| Mem(Box::new(ev.pop_front().unwrap())),
            gen,
            nop_marker_label,
        ),
        Eseq(s, e) => {
            let stmt = lift_stm(*s, gen, nop_marker_label);
            let (stmts, ee) = lift_exp(*e, gen, nop_marker_label);
            (stmt % stmts, ee)
        }
        Call(e, el) => {
            let mut vd = VecDeque::from(el);
            vd.push_front(*e);
            reorder_exp(
                vd,
                |mut ev| Call(Box::new(ev.pop_front().unwrap()), Vec::from(ev)),
                gen,
                nop_marker_label,
            )
        }
        e => reorder_exp(VecDeque::with_capacity(0), |_| e, gen, nop_marker_label),
    }
}

pub fn linearize(i: IrStm, gen: &mut GenTemporary) -> Vec<IrStm> {
    // From an arbitrary Tree statement, produce a list of cleaned trees
    //    satisfying the following properties:
    //       1.  No SEQ's or ESEQ's
    //       2.  The parent of every CALL is an EXP(..) or a MOVE(TEMP t,..)
    fn helper(i: IrStm, mut rest: Vec<IrStm>, nop_marker_label: temp::Label) -> Vec<IrStm> {
        match i {
            Seq(a, b) => helper(*a, helper(*b, rest, nop_marker_label), nop_marker_label),
            Label(l) if l == nop_marker_label => rest,
            _ => {
                rest.insert(0, i);
                rest
            }
        }
    }
    let nop_marker_label = gen.new_label();
    helper(
        lift_stm(i, gen, nop_marker_label),
        Vec::new(),
        nop_marker_label,
    )
}

pub struct Block {
    stmts: Vec<IrStm>,
    marked: bool,
}

impl Block {
    fn new() -> Self {
        Self {
            stmts: Vec::new(),
            marked: false,
        }
    }

    fn label(&self) -> temp::Label {
        match self.stmts[0] {
            Label(l) => l,
            _ => panic!("impl bug: block.label() should only be called on a nonempty block with Label as first stmt")
        }
    }

    fn successors(&self) -> Vec<temp::Label> {
        match &self.stmts[self.stmts.len() - 1] {
            // Order here important, we want the false branch of the Cjump first so that
            // if the corresponding basic block has not been included in an existing trace,
            // it will be selected before the true block is considered.
            Cjump(_, _, _, t, f) => vec![f.clone(), t.clone()],
            Jump(_, labels) => labels.clone(),
            _ => panic!("impl bug: block does not end in a Jump or Cjump"),
        }
    }

    #[inline]
    fn push(&mut self, i: IrStm) {
        // convenience method.
        self.stmts.push(i);
    }

    #[inline]
    fn len(&self) -> usize {
        // convenience method.
        self.stmts.len()
    }
}

#[cfg(debug_assertions)]
fn validate_block(this_block: &Block) {
    debug_assert!(this_block.len() > 0);
    debug_assert!(matches!(this_block.stmts[0], Label(..)));
    debug_assert!(
        matches!(this_block.stmts[this_block.len() - 1], Cjump(..))
            || matches!(this_block.stmts[this_block.len() - 1], Jump(..))
    );
}

pub fn basic_blocks(
    stmts: Vec<IrStm>,
    gen: &mut GenTemporary,
) -> (HashMap<temp::Label, Block>, temp::Label) {
    // according to Appel (p180), this basic blocks function is applied to each function body in turn.
    // the "epilogue" will not be part of this body, but will eventually follow the last statement.
    //
    // From a list of cleaned trees, produce a list of
    //  basic blocks satisfying the following properties:
    //       1. and 2. as above;
    //       3.  Every block begins with a LABEL;
    //           4.  A LABEL appears only at the beginning of a block;
    //           5.  Any JUMP or CJUMP is the last stm in a block;
    //           6.  Every block ends with a JUMP or CJUMP;
    //        Also produce the "label" to which control will be passed
    //        upon exit.
    let mut blist = HashMap::new();
    let mut this_block = Block::new();
    let done_label = gen.new_label();

    let mut iter = stmts.into_iter().peekable();
    while iter.len() > 0 {
        // add the start label.
        match iter.peek() {
            None => {}
            Some(Label(..)) => {
                this_block.push(iter.next().unwrap());
            }
            Some(_) => {
                let l = gen.new_label();
                this_block.push(Label(l));
            }
        }
        // add the rest in the current block until we hit a Jump or Cjump, or a Label, or run out.
        while let Some(stmt) = iter.peek() {
            match stmt {
                Jump(..) | Cjump(..) => {
                    // clean end of block
                    this_block.push(iter.next().unwrap());
                    if cfg!(debug_assertions) {
                        validate_block(&this_block);
                    }
                    blist.insert(this_block.label(), this_block);
                    this_block = Block::new();
                    break;
                }
                Label(l) => {
                    // need to make up a jump to the label of the next block
                    this_block.push(Jump(Box::new(Name(*l)), vec![*l]));
                    if cfg!(debug_assertions) {
                        validate_block(&this_block);
                    }
                    blist.insert(this_block.label(), this_block);
                    this_block = Block::new();
                    break;
                }
                _ => {
                    // normal stmt, add it.
                    this_block.push(iter.next().unwrap());
                }
            }
        }
        // did we run out of stmts before encountering Jump or Cjump? If so add a end label.
        if this_block.len() > 0 {
            this_block.push(Jump(Box::new(Name(done_label)), vec![done_label]));
            if cfg!(debug_assertions) {
                validate_block(&this_block);
            }
            blist.insert(this_block.label(), this_block);
            this_block = Block::new(); // need this to satisfy borrow checker.

            debug_assert!(iter.len() == 0); // sanity check
        }
    }
    (blist, done_label)
}

fn invert_cjump(
    r: IrRelop,
    a: Box<IrExp>,
    b: Box<IrExp>,
    lt: temp::Label,
    lf: temp::Label,
) -> IrStm {
    let new_op = match r {
        Eq => Ne,
        Ne => Eq,
        Lt => Ge,
        Gt => Le,
        Le => Gt,
        Ge => Lt,
        Ult => Uge,
        Ule => Ugt,
        Ugt => Ule,
        Uge => Ult,
    };
    Cjump(new_op, a, b, lf, lt)
}

pub fn trace_schedule(
    mut blist: HashMap<temp::Label, Block>,
    done_label: temp::Label,
    gen: &mut GenTemporary,
) -> Vec<IrStm> {
    // From a list of basic blocks satisfying properties 1-6,
    //         along with an "exit" label,
    //     produce a list of stms such that:
    //       1. and 2. as above;
    //           7. Every CJUMP(_,t,f) is immediately followed by LABEL f.
    //         The blocks are reordered to satisfy property 7; also
    //     in this reordering as many JUMP(T.NAME(lab)) statements
    //         as possible are eliminated by falling through into T.LABEL(lab).
    let mut res = Vec::new();
    while !blist.is_empty() {
        let mut new_trace = Vec::new();

        let (head_label, _) = blist.iter().next().unwrap();
        let mut b = Some(blist.remove(&head_label.clone()).unwrap());
        while b.is_some() && !b.as_ref().unwrap().marked {
            b.as_mut().unwrap().marked = true;
            let succs = b.as_ref().unwrap().successors();
            new_trace.push(b.unwrap());
            b = None;
            for succ_label in succs {
                match blist.remove(&succ_label) {
                    None => continue,
                    Some(block) => {
                        b = Some(block);
                        break;
                    }
                }
            }

            if b.is_none() {
                // means, we are done with the trace as we cannot find unmarked blocks of any of the successors.
                // time to output the trace.
                let mut blk_iter = new_trace.into_iter().peekable();
                // this must exist by construction. need it to be Option to be able to swap the next block's iter into this
                // when we are done with the current block.
                let mut cur_blk = blk_iter.next().map(|b| b.stmts.into_iter().peekable());
                // this may or may not exist.
                let mut nxt_blk = blk_iter.next().map(|b| b.stmts.into_iter().peekable());

                while cur_blk.is_some() {
                    let cb_iter = cur_blk.unwrap();
                    for stmt in cb_iter {
                        match stmt {
                            Jump(..) => {
                                // is next exist? if so we skip this jump and that label because it must be my label
                                if nxt_blk.is_some() {
                                    nxt_blk = nxt_blk.map(|mut it| {
                                        if cfg!(debug_assertions) {
                                            match it.next() {
                                                Some(Label(..)) => {},
                                                x => panic!("basic block expected to start with Label but got {:#?}", x)
                                            }
                                        } else {
                                            it.next(); // consume the first stmt, which should be the Label.
                                        }
                                        it
                                    });
                                } else {
                                    res.push(stmt);
                                }
                            }
                            Cjump(p, a, b, lt, lf) => {
                                // am i followed by my false label?
                                // yes -> add me
                                // no because next block is my true label -> negate me and add that
                                // no because next block doesn't exist -> make up label lf', add me with false to lf', label lf', and jump to original lf
                                if nxt_blk.is_some() {
                                    // it would be a bug if unwrap() fails as every basic block must be nonempty.
                                    match nxt_blk.as_mut().unwrap().peek().unwrap() {
                                        Label(nxt_lb) => {
                                            if nxt_lb == lf {
                                                res.push(Cjump(p, a, b, lt, lf));
                                            } else {
                                                debug_assert!(nxt_lb == lt, "impl bug: cjump in a basic block with an existing next block is not followed by its true or false label");
                                                // followed by true label.
                                                res.push(invert_cjump(p, a, b, lt, lf));
                                            }
                                        }
                                        _ => {
                                            panic!("impl bug: basic block must start with a Label")
                                        }
                                    }
                                } else {
                                    let lff = gen.new_label();
                                    res.push(Cjump(p, a, b, lt, lff));
                                    res.push(Label(lff));
                                    res.push(Jump(Box::new(Name(lf)), vec![lf]));
                                }
                            }
                            _ => res.push(stmt),
                        }
                    }
                    cur_blk = nxt_blk;
                    nxt_blk = blk_iter.next().map(|b| b.stmts.into_iter().peekable());
                }
                break;
            }
        }
    }
    // TODO figure 8.3 has situation where the Label(done), epilogue statements is already part of the
    // trace. Idk at what point or where those are supposed to be added in. The next line prob wrong
    // but putting it here for now. Although, in canon.ml given on Appel's website, they do just tag on
    // the done label as the last IrStm of the output.
    res.push(Label(done_label));
    res
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        absyn, frame,
        frame::{Escapes, Frame},
        ir::{IrExp, IrStm},
        symbol::Interner,
        symbol::Symbol,
        temp::{self, GenTemporary, Label},
    };

    fn caller_linearize(i: IrStm) -> Vec<IrStm> {
        let mut gen = GenTemporary::new();
        linearize(Exp(Box::new(Const(42))), &mut gen)
    }

    mod linearize {
        use super::*;

        #[test]
        fn const_is_identity() {
            let mut gen = GenTemporary::new();
            match linearize(Exp(Box::new(Const(42))), &mut gen).as_slice() {
                [Exp(x)] => {
                    match **x {
                        Const(42) => {},
                        _ => assert!(false)
                    }
                }
                x => {
                    println!("got {:#?}", x);
                    assert!(false);
                }
            }
        }

        #[test]
        fn name_is_identity() {
            let mut gen = GenTemporary::new();
            let l = gen.new_label();
            match linearize(Exp(Box::new(Name(l))), &mut gen).as_slice() {
                [Exp(x)] => {
                    match **x {
                        Name(y) if y == l => {},
                        _ => assert!(false)
                    }
                }
                x => {
                    println!("got {:#?}", x);
                    assert!(false);
                }
            }
        }

        #[test]
        fn temp_is_identity() {
            let mut gen = GenTemporary::new();
            let t = gen.new_temp();
            match linearize(Exp(Box::new(Temp(t))), &mut gen).as_slice() {
                [Exp(x)] => {
                    match **x {
                        Temp(x) if x == t => {},
                        _ => assert!(false)
                    }
                }
                x => {
                    println!("got {:#?}", x);
                    assert!(false);
                }
            }
        }

        #[test]
        fn top_level_eseq_remains() {
            // TODO refactor this shit code to have helpers to initiaize
            // so that don't need to type Box::new() every fucking where.
            // let mut gen = GenTemporary::new();
            // let t = gen.new_temp();
            // match linearize(Exp(Box::new(Eseq(Box::new(), Box::new(Temp(t))))), &mut gen).as_slice() {
            //     [Exp(x)] => {
            //         match **x {
            //             Temp(x) if x == t => {},
            //             _ => assert!(false)
            //         }
            //     }
            //     x => {
            //         println!("got {:#?}", x);
            //         assert!(false);
            //     }
            // }
        }

        fn binop() {}

        fn mem() {}

        fn exp_call() {}

        fn move_() {}

        fn jump() {}

        fn cjump() {}

        #[test]
        fn seq_is_eliminated() {
             let mut gen = GenTemporary::new();
            let t = gen.new_temp();
            match linearize(Seq(Box::new(Exp(Box::new(Const(1)))), Box::new(Exp(Box::new(Const(2))))), &mut gen).as_slice() {
                [Exp(x), Exp(y)] => {
                    match (&**x, &**y) {
                        (Const(1), Const(2)) => {},
                        _ => assert!(false)
                    }
                }
                x => {
                    println!("got {:#?}", x);
                    assert!(false);
                }
            }
        }

        fn label() {}
    }

    #[test]
    fn basic_block() {}
}
