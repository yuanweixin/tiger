use crate::{
    ir::{
        helpers::*,
        IrExp,
        IrExp::{Const, Name, Temp},
        IrRelop,
        IrRelop::*,
        IrStm,
        IrStm::Label,
    },
    temp,
    temp::GenTemporary,
};

use std::collections::{HashMap, VecDeque};

#[inline]
fn nop(nop_marker_label: temp::Label) -> IrStm {
    Label(nop_marker_label)
}

impl std::ops::Rem for IrStm {
    type Output = IrStm;

    fn rem(self, rhs: Self) -> IrStm {
        Seq(self, rhs)
    }
}

fn reorder(
    mut ev: VecDeque<IrExp>,
    gen: &mut dyn GenTemporary,
    nop_marker_label: temp::Label,
) -> (IrStm, VecDeque<IrExp>) {
    if ev.is_empty() {
        // note: this can insert a bunch of spurious nop statements.
        (nop(nop_marker_label), VecDeque::with_capacity(0))
    } else {
        match ev[0] {
            IrExp::Call(..) => {
                let t = gen.new_temp();
                let e0 = ev.pop_front().unwrap();
                let eseq = Eseq(Move(Temp(t), e0), Temp(t));
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

                // the comparison with nop_marker_label is a small optimization to
                // eliminate the introduction of a new, unnecessary temporary in order to evaluate
                // e before the nop placeholder.
                if commutes(&stmts_rest, &e) || Label(nop_marker_label) == stmts_rest {
                    e_rest.push_front(e);
                    (stmt % stmts_rest, e_rest)
                } else {
                    let t = gen.new_temp();
                    e_rest.push_front(Temp(t));
                    (stmt % Move(Temp(t), e) % stmts_rest, e_rest)
                }
            }
        }
    }
}

fn reorder_exp<F>(
    ev: VecDeque<IrExp>,
    make: F,
    gen: &mut dyn GenTemporary,
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
    gen: &mut dyn GenTemporary,
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
        (IrStm::Exp(x), _) if matches!(x.as_ref(), Const(..)) => true,
        (_, Name(..)) => true,
        (_, Const(..)) => true,
        (_, _) => false,
    }
}

fn lift_stm(s: IrStm, gen: &mut dyn GenTemporary, nop_marker_label: temp::Label) -> IrStm {
    match s {
        IrStm::Seq(a, b) => {
            lift_stm(*a, gen, nop_marker_label) % lift_stm(*b, gen, nop_marker_label)
        }
        IrStm::Jump(e, labs) => reorder_stm(
            VecDeque::from(vec![*e]),
            |mut ev| Jump(ev.pop_front().unwrap(), labs),
            gen,
            nop_marker_label,
        ),
        IrStm::Cjump(p, a, b, t, f) => reorder_stm(
            VecDeque::from(vec![*a, *b]),
            |mut ev| {
                Cjump(
                    p.clone(),
                    ev.pop_front().unwrap(),
                    ev.pop_front().unwrap(),
                    t,
                    f,
                )
            },
            gen,
            nop_marker_label,
        ),
        IrStm::Move(dst, src) => {
            // lack of box matching forces this shitty nested match
            match (*dst, *src) {
                (Temp(t), IrExp::Call(e, e1)) => {
                    let mut vd = VecDeque::from(e1);
                    vd.push_front(*e);
                    reorder_stm(
                        vd,
                        |mut ev| Move(Temp(t), Call(ev.pop_front().unwrap(), Vec::from(ev))),
                        gen,
                        nop_marker_label,
                    )
                }
                (Temp(t), b) => reorder_stm(
                    VecDeque::from(vec![b]),
                    |mut ev| Move(Temp(t), ev.pop_front().unwrap()),
                    gen,
                    nop_marker_label,
                ),
                (IrExp::Mem(e), b) => reorder_stm(
                    VecDeque::from(vec![*e, b]),
                    |mut ev| Move(Mem(ev.pop_front().unwrap()), ev.pop_front().unwrap()),
                    gen,
                    nop_marker_label,
                ),
                (IrExp::Eseq(s, e), src) => lift_stm(
                    IrStm::Seq(s, Box::new(IrStm::Move(e, Box::new(src)))),
                    gen,
                    nop_marker_label,
                ),
                (a, b) => reorder_stm(
                    VecDeque::with_capacity(0),
                    |_| Move(a, b),
                    gen,
                    nop_marker_label,
                ),
            }
        }
        IrStm::Exp(ebox) => match *ebox {
            IrExp::Call(e, mut el) => {
                let mut vd = VecDeque::from(el);
                vd.push_front(*e);
                reorder_stm(
                    vd,
                    |mut ev| Exp(Call(ev.pop_front().unwrap(), Vec::from(ev))),
                    gen,
                    nop_marker_label,
                )
            }
            e => reorder_stm(
                VecDeque::from(vec![e]),
                |mut ev| Exp(ev.pop_front().unwrap()),
                gen,
                nop_marker_label,
            ),
        },
        s => reorder_stm(VecDeque::with_capacity(0), |_| s, gen, nop_marker_label),
    }
}

fn lift_exp(e: IrExp, gen: &mut dyn GenTemporary, nop_marker_label: temp::Label) -> (IrStm, IrExp) {
    match e {
        IrExp::Binop(p, a, b) => reorder_exp(
            VecDeque::from(vec![*a, *b]),
            |mut ev| Binop(p.clone(), ev.pop_front().unwrap(), ev.pop_front().unwrap()),
            gen,
            nop_marker_label,
        ),
        IrExp::Mem(a) => reorder_exp(
            VecDeque::from(vec![*a]),
            |mut ev| Mem(ev.pop_front().unwrap()),
            gen,
            nop_marker_label,
        ),
        IrExp::Eseq(s, e) => {
            let stmt = lift_stm(*s, gen, nop_marker_label);
            let (stmts, ee) = lift_exp(*e, gen, nop_marker_label);
            (stmt % stmts, ee)
        }
        IrExp::Call(e, el) => {
            let mut vd = VecDeque::from(el);
            vd.push_front(*e);
            reorder_exp(
                vd,
                |mut ev| Call(ev.pop_front().unwrap(), Vec::from(ev)),
                gen,
                nop_marker_label,
            )
        }
        e => reorder_exp(VecDeque::with_capacity(0), |_| e, gen, nop_marker_label),
    }
}

pub fn linearize(i: IrStm, gen: &mut dyn GenTemporary) -> Vec<IrStm> {
    // From an arbitrary Tree statement, produce a list of cleaned trees
    //    satisfying the following properties:
    //       1.  No SEQ's or ESEQ's
    //       2.  The parent of every CALL is an EXP(..) or a MOVE(TEMP t,..)
    fn helper(i: IrStm, mut rest: Vec<IrStm>, nop_marker_label: temp::Label) -> Vec<IrStm> {
        match i {
            IrStm::Seq(a, b) => helper(*a, helper(*b, rest, nop_marker_label), nop_marker_label),
            // explicitly eliminate the nop statements.
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
            IrStm::Cjump(_, _, _, t, f) => vec![f.clone(), t.clone()],
            IrStm::Jump(_, labels) => labels.clone(),
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
        matches!(this_block.stmts[this_block.len() - 1], IrStm::Cjump(..))
            || matches!(this_block.stmts[this_block.len() - 1], IrStm::Jump(..))
    );
}

pub fn basic_blocks(
    stmts: Vec<IrStm>,
    gen: &mut dyn GenTemporary,
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
                IrStm::Jump(..) | IrStm::Cjump(..) => {
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
                    this_block.push(Jump(Name(*l), vec![*l]));
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
            this_block.push(Jump(Name(done_label), vec![done_label]));
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
    IrStm::Cjump(new_op, a, b, lf, lt)
}

pub fn trace_schedule(
    mut blist: HashMap<temp::Label, Block>,
    done_label: temp::Label,
    gen: &mut dyn GenTemporary,
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
                            IrStm::Jump(..) => {
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
                            IrStm::Cjump(p, a, b, lt, lf) => {
                                // am i followed by my false label?
                                // yes -> add me
                                // no because next block is my true label -> negate me and add that
                                // no because next block doesn't exist -> make up label lf', add me with false to lf', label lf', and jump to original lf
                                if nxt_blk.is_some() {
                                    // it would be a bug if unwrap() fails as every basic block must be nonempty.
                                    match nxt_blk.as_mut().unwrap().peek().unwrap() {
                                        Label(nxt_lb) => {
                                            if nxt_lb == lf {
                                                res.push(IrStm::Cjump(p, a, b, lt, lf));
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
                                    res.push(IrStm::Cjump(p, a, b, lt, lff));
                                    res.push(Label(lff));
                                    res.push(Jump(Name(lf), vec![lf]));
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
    // tack on the done label as the last IrStm of the output.
    res.push(Label(done_label));
    res
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        absyn, frame,
        frame::{Escapes, Frame},
        ir::{IrBinop::*, IrExp, IrRelop::*, IrStm},
        symbol::Interner,
        symbol::Symbol,
        temp::{self, test_helpers, GenTemporary, Label},
    };
    use itertools;
    use std::num::NonZeroUsize;

    struct GenTemporaryForTest {
        syms: Box<dyn Iterator<Item = usize>>,
    }

    impl GenTemporaryForTest {
        fn new(syms: Vec<usize>) -> Self {
            Self {
                syms: Box::new(itertools::chain(vec![1].into_iter(), syms.into_iter()).into_iter()),
            }
        }
    }

    impl GenTemporary for GenTemporaryForTest {
        fn new() -> Self {
            panic!();
        }

        #[inline]
        fn resolve(&self, s: &Symbol) -> Option<&str> {
            panic!();
        }

        #[inline]
        fn resolve_label(&self, l: Label) -> Option<&str> {
            panic!();
        }

        #[inline]
        fn intern(&mut self, name: &str) -> Symbol {
            panic!();
        }

        fn new_temp(&mut self) -> temp::Temp {
            let nxt = self.syms.next();
            match nxt {
                None => {
                    panic!("test impl bug or actual bug: ran out of symbols");
                }
                Some(s) => test_helpers::new_temp(s),
            }
        }

        fn make_string(temp: temp::Temp) -> String {
            panic!();
        }

        fn new_label(&mut self) -> temp::Label {
            let nxt = self.syms.next();
            match nxt {
                None => {
                    panic!("test impl bug or actual bug: ran out of symbols");
                }
                Some(s) => temp::test_helpers::new_label(s),
            }
        }

        fn named_label(s: &str, pool: &mut Interner) -> Label {
            panic!();
        }
    }

    mod linearize {
        use crate::temp::GenTemporaryImpl;

        use super::*;

        const NOP_LABEL_ID: NonZeroUsize = NonZeroUsize::MIN;

        #[test]
        fn const_is_identity() {
            let mut gen: GenTemporaryImpl = GenTemporary::new();
            let expected = vec![Exp(Const(42))];
            let actual = linearize(Exp(Const(42)), &mut gen);
            assert_eq!(expected, actual);
        }

        #[test]
        fn name_is_identity() {
            let mut gen: GenTemporaryImpl = GenTemporary::new();
            let l = gen.new_label();
            let expected = vec![Exp(Name(l))];
            let actual = linearize(Exp(Name(l)), &mut gen);
            assert_eq!(expected, actual);
        }

        #[test]
        fn temp_is_identity() {
            let mut gen: GenTemporaryImpl = GenTemporary::new();
            let t = gen.new_temp();
            let expected = vec![Exp(Temp(t))];
            let actual = linearize(Exp(Temp(t)), &mut gen);
            assert_eq!(expected, actual);
        }

        #[test]
        fn exp_eseq() {
            let mut gen: GenTemporaryImpl = GenTemporary::new();
            let l = gen.new_label();
            let t = gen.new_temp();

            let expected = vec![Label(l), Exp(Temp(t))];
            let actual = linearize(Exp(Eseq(Label(l), Temp(t))), &mut gen);
            assert_eq!(expected, actual);
        }

        #[test]
        fn eseq_eseq() {
            let mut gen: GenTemporaryImpl = GenTemporary::new();
            let l = gen.new_label();
            let l2 = gen.new_label();
            let t = gen.new_temp();

            let expected = vec![Label(l), Label(l2), Exp(Temp(t))];
            let actual = linearize(Exp(Eseq(Label(l), Eseq(Label(l2), Temp(t)))), &mut gen);
            assert_eq!(expected, actual);
        }

        #[test]
        fn binop_eseq_left() {
            let mut gen: GenTemporaryImpl = GenTemporary::new();
            let l = gen.new_label();
            let l2 = gen.new_label();
            let t = gen.new_temp();

            let expected = vec![Label(l), Label(l2), Exp(Binop(Plus, Temp(t), Const(2)))];
            let actual = linearize(
                Exp(Binop(
                    Plus,
                    Eseq(Label(l), Eseq(Label(l2), Temp(t))),
                    Const(2),
                )),
                &mut gen,
            );
            assert_eq!(expected, actual);
        }

        #[test]
        fn mem_eseq() {
            let mut gen: GenTemporaryImpl = GenTemporary::new();
            let l = gen.new_label();
            let l2 = gen.new_label();
            let t = gen.new_temp();

            let expected = vec![Label(l), Label(l2), Exp(Mem(Temp(t)))];
            let actual = linearize(Exp(Mem(Eseq(Label(l), Eseq(Label(l2), Temp(t))))), &mut gen);
            assert_eq!(expected, actual);
        }

        #[test]
        fn jump_eseq() {
            let mut gen: GenTemporaryImpl = GenTemporary::new();
            let l = gen.new_label();
            let l2 = gen.new_label();
            let t = gen.new_temp();

            let expected = vec![Label(l), Label(l2), Jump(Temp(t), vec![l])];
            let actual = linearize(
                Jump(Eseq(Label(l), Eseq(Label(l2), Temp(t))), vec![l]),
                &mut gen,
            );
            assert_eq!(expected, actual);
        }

        #[test]
        fn cjump_eseq_left() {
            let mut gen: GenTemporaryImpl = GenTemporary::new();

            let l = gen.new_label();
            let l2 = gen.new_label();
            let t = gen.new_temp();
            let t2 = gen.new_temp();

            let expected = vec![Label(l), Label(l2), Cjump(Gt, Temp(t), Temp(t2), l, l2)];
            let actual = linearize(
                Cjump(
                    Gt,
                    Eseq(Label(l), Eseq(Label(l2), Temp(t))),
                    Temp(t2),
                    l,
                    l2,
                ),
                &mut gen,
            );
            assert_eq!(expected, actual);
        }

        #[test]
        fn binop_right_eseq_commutes() {
            let mut gen: GenTemporaryImpl = GenTemporary::new();
            let l = gen.new_label();
            let l2 = gen.new_label();
            let t = gen.new_temp();

            let expected = vec![Label(l), Label(l2), Exp(Binop(Plus, Const(2), Temp(t)))];
            let actual = linearize(
                Exp(Binop(
                    Plus,
                    Const(2),
                    Eseq(Label(l), Eseq(Label(l2), Temp(t))),
                )),
                &mut gen,
            );
            assert_eq!(expected, actual);
        }

        #[test]
        fn binop_right_eseq_no_commute() {
            let mut gen = GenTemporaryForTest::new(vec![2]);
            let l = test_helpers::new_label(999);
            let l2 = test_helpers::new_label(1000);
            let t = test_helpers::new_temp(1001);
            let t2 = test_helpers::new_temp(2);
            let t3 = test_helpers::new_temp(3);
            let expected = vec![
                Move(Temp(t2), Temp(t3)),
                Label(l),
                Label(l2),
                Exp(Binop(Plus, Temp(t2), Temp(t))),
            ];
            let actual = linearize(
                Exp(Binop(
                    Plus,
                    Temp(t3),
                    Eseq(Label(l), Eseq(Label(l2), Temp(t))),
                )),
                &mut gen,
            );
            assert_eq!(expected, actual);
        }

        #[test]
        fn cjump_right_eseq_no_commute() {
            let mut gen = GenTemporaryForTest::new(vec![1]);

            let l = test_helpers::new_label(999);
            let l2 = test_helpers::new_label(1000);
            let t = test_helpers::new_temp(1001);
            let t2 = test_helpers::new_temp(1002);

            let exp_generated_temp = test_helpers::new_temp(1);

            let expected = vec![
                Move(Temp(exp_generated_temp), Temp(t2)),
                Label(l),
                Label(l2),
                Cjump(Gt, Temp(exp_generated_temp), Temp(t), l, l2),
            ];
            let actual = linearize(
                Cjump(
                    Gt,
                    Temp(t2),
                    Eseq(Label(l), Eseq(Label(l2), Temp(t))),
                    l,
                    l2,
                ),
                &mut gen,
            );
            assert_eq!(expected, actual);
        }

        #[test]
        fn cjump_right_eseq_commutes() {
            let mut gen: GenTemporaryImpl = GenTemporary::new();

            let l = gen.new_label();
            let l2 = gen.new_label();
            let t = gen.new_temp();

            let expected = vec![Label(l), Label(l2), Cjump(Gt, Const(42), Temp(t), l, l2)];
            let actual = linearize(
                Cjump(
                    Gt,
                    Const(42),
                    Eseq(Label(l), Eseq(Label(l2), Temp(t))),
                    l,
                    l2,
                ),
                &mut gen,
            );
            assert_eq!(expected, actual);
        }

        #[test]
        fn move_temp_eseq() {
            let mut gen: GenTemporaryImpl = GenTemporary::new();
            let l = gen.new_label();
            let l2 = gen.new_label();
            let t = gen.new_temp();
            let t2 = gen.new_temp();

            let expected = vec![Label(l), Label(l2), Move(Temp(t), Temp(t2))];
            let actual = linearize(
                Move(Temp(t), Eseq(Label(l), Eseq(Label(l2), Temp(t2)))),
                &mut gen,
            );
            assert_eq!(expected, actual);
        }

        #[test]
        fn move_mem_eseq_left() {
            let mut gen: GenTemporaryImpl = GenTemporary::new();
            let l = gen.new_label();
            let l2 = gen.new_label();
            let t = gen.new_temp();
            let t2 = gen.new_temp();

            let expected = vec![Label(l), Label(l2), Move(Mem(Temp(t)), Temp(t2))];
            let actual = linearize(
                Move(Mem(Eseq(Label(l), Eseq(Label(l2), Temp(t)))), Temp(t2)),
                &mut gen,
            );
            assert_eq!(expected, actual);
        }

        #[test]
        fn move_mem_eseq_right() {
            let mut gen: GenTemporaryImpl = GenTemporary::new();
            let t = gen.new_temp();

            let expected = vec![Move(Temp(t), Const(42)), Move(Temp(t), Mem(Temp(t)))];
            let actual = linearize(
                Move(Temp(t), Mem(Eseq(Move(Temp(t), Const(42)), Temp(t)))),
                &mut gen,
            );
            assert_eq!(expected, actual);
        }

        #[test]
        fn exp_call_eseq() {
            let mut gen = GenTemporaryForTest::new(vec![2]);
            let t = test_helpers::new_temp(100);
            let l = test_helpers::new_label(101);
            let l2 = test_helpers::new_label(102);
            let t2 = test_helpers::new_temp(2);

            let expected = vec![
                Label(l),
                Move(Temp(t2), Temp(t)),
                Label(l2),
                Exp(Call(Temp(t2), vec![Const(1), Const(42)])),
            ];
            let actual = linearize(
                // l2 commutes with const(1), then it cannot commute with temp(t), so temp(t) is moved to a fresh temp.
                // in this case it doesn't matter but the rewriting rule is mechanical and dumb.
                Exp(Call(
                    Eseq(Label(l), Temp(t)),
                    vec![Const(1), Eseq(Label(l2), Const(42))],
                )),
                &mut gen,
            );
            assert_eq!(expected, actual);
        }

        #[test]
        fn exp_call_eseq_no_commute() {
            let mut gen = GenTemporaryForTest::new(vec![2, 3]);
            let t = test_helpers::new_temp(100);
            let t2 = test_helpers::new_temp(2);
            let t3 = test_helpers::new_temp(3);

            let expected = vec![
                Move(Temp(t), Const(42)),
                Move(Temp(t3), Temp(t)),
                Move(Temp(t2), Temp(t)),
                Move(Temp(t), Const(43)),
                Exp(Call(Temp(t3), vec![Temp(t2), Temp(t)])),
            ];
            let actual = linearize(
                Exp(Call(
                    Eseq(Move(Temp(t), Const(42)), Temp(t)),
                    vec![Temp(t), Eseq(Move(Temp(t), Const(43)), Temp(t))],
                )),
                &mut gen,
            );
            assert_eq!(expected, actual);
        }

        #[test]
        fn move_temp_call_eseq() {
            let mut gen: GenTemporaryImpl = GenTemporary::new();
            let t = gen.new_temp();

            let expected = vec![
                Move(Temp(t), Const(43)),
                Move(Temp(t), Call(Temp(t), vec![Temp(t)])),
            ];
            let actual = linearize(
                Move(
                    Temp(t),
                    Call(Eseq(Move(Temp(t), Const(43)), Temp(t)), vec![Temp(t)]),
                ),
                &mut gen,
            );
            assert_eq!(expected, actual);
        }

        #[test]
        fn seq_is_eliminated() {
            let mut gen : GenTemporaryImpl = GenTemporary::new();
            let t = gen.new_temp();
            let expected = vec![Exp(Const(1)), Exp(Const(2))];
            let actual = linearize(Seq(Exp(Const(1)), Exp(Const(2))), &mut gen);
            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn basic_block() {

    }

    // validates all the original statement present.
    // TODO kinda pita with the elimination of jumps and rearranging of cjumps.
    #[test]
    fn trace() {}
}
