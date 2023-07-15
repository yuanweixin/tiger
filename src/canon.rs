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
    temp::Uuids,
};

use std::collections::{HashMap, LinkedList, VecDeque};

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
    gen: &mut dyn Uuids,
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
    gen: &mut dyn Uuids,
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
    gen: &mut dyn Uuids,
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

fn lift_stm(s: IrStm, gen: &mut dyn Uuids, nop_marker_label: temp::Label) -> IrStm {
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

fn lift_exp(e: IrExp, gen: &mut dyn Uuids, nop_marker_label: temp::Label) -> (IrStm, IrExp) {
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

pub fn linearize(i: IrStm, gen: &mut dyn Uuids) -> Vec<IrStm> {
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

#[derive(Debug)]
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
    gen: &mut dyn Uuids,
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
                IrStm::Seq(..) => {
                    panic!("impl bug: Seq encountered during construction of basic block, but it ought to have been eliminated by the linearize step");
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

pub trait BlockList {
    // gives a successor of the block (i.e. the block it (c)jumps to) that is present in the list.
    fn successor(&mut self, b: &Block) -> Option<Block>;

    // gives the next available block to start a trace.
    // this gives the trace algorithm some flexibility in picking what to use to start trace.
    // the main intended use is to let us use Vec representation during testing to have
    // reproducible/controllable block input order, so that we can write proper tests.
    fn next(&mut self) -> Option<Block>;
}

impl BlockList for HashMap<temp::Label, Block> {
    fn successor(&mut self, b: &Block) -> Option<Block> {
        for succ in b.successors() {
            let x = self.remove(&succ);
            if x.is_some() {
                return x;
            }
        }
        None
    }

    fn next(&mut self) -> Option<Block> {
        let mut x = None;
        if self.len() > 0 {
            x = Some(self.iter().next().unwrap().0.clone());
        }
        if x.is_some() {
            return self.remove(&x.unwrap());
        }
        None
    }
}

impl BlockList for Vec<Block> {
    fn successor(&mut self, b: &Block) -> Option<Block> {
        debug_assert!(b.stmts.len() > 0);
        let succs = b.successors();
        for (i, ele) in self.iter().enumerate() {
            if succs.contains(&ele.label()) {
                return Some(self.remove(i));
            }
        }
        None
    }

    fn next(&mut self) -> Option<Block> {
        if self.len() > 0 {
            Some(self.remove(0))
        } else {
            None
        }
    }
}

pub fn trace_schedule(
    mut blist: impl BlockList,
    done_label: temp::Label,
    gen: &mut dyn Uuids,
) -> Vec<IrStm> {
    let mut res = Vec::new();

    fn collapse_trace(trace: Vec<Block>, res: &mut Vec<IrStm>, gen: &mut dyn Uuids) {
        debug_assert!(!trace.is_empty());
        let mut blk_iter = trace.into_iter().peekable();
        let mut cur_blk = blk_iter.next().map(|b| b.stmts.into_iter().peekable());
        let mut nxt_blk = blk_iter.next().map(|b| b.stmts.into_iter().peekable());

        while let Some(cb_iter) = cur_blk {
            for stmt in cb_iter {
                match stmt {
                    // if next block exist and the label is in target list,
                    // then we should skip this jump. otherwise include this jump.
                    IrStm::Jump(_, ref targets) => match nxt_blk {
                        Some(ref mut nxt_iter) => {
                            let x = nxt_iter.peek().unwrap();
                            if let Label(start_label) = x {
                                if !targets.contains(start_label) {
                                    res.push(stmt);
                                }
                            } else {
                                panic!("impl bug: basic block expected to start with a Label but got {:#?}", x)
                            }
                        }
                        None => res.push(stmt),
                    },
                    IrStm::Cjump(p, a, b, lt, lf) => {
                        // am i followed by my false label?
                        // yes -> add me
                        // no because next block is my true label -> negate me and add that
                        // no because next block doesn't exist -> make up label lf', add me with false to lf', label lf', and jump to original lf
                        if nxt_blk.is_some() {
                            match nxt_blk.as_mut().unwrap().peek().unwrap() {
                                Label(nxt_lb) => {
                                    if nxt_lb == lf {
                                        res.push(IrStm::Cjump(p, a, b, lt, lf));
                                    } else if nxt_lb == lt {
                                        let inverted = invert_cjump(p, a, b, lt, lf);
                                        res.push(inverted);
                                    } else {
                                        panic!("impl bug: cjump in a basic block with an existing next block in a trace, but is not followed by its true or false label");
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
    }

    while let Some(block) = blist.next() {
        let mut new_trace = Vec::new();

        let mut cur_block = block;
        while let Some(succ_block) = blist.successor(&cur_block) {
            new_trace.push(cur_block);
            cur_block = succ_block;
        }
        new_trace.push(cur_block);
        collapse_trace(new_trace, &mut res, gen);
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
        temp::{self, test_helpers, Uuids, Label},
    };
    use itertools;
    use std::num::NonZeroUsize;

    struct UuidForTest {
        syms: Box<dyn Iterator<Item = usize>>,
    }

    impl UuidForTest {
        fn new_for_linearize(syms: Vec<usize>) -> Self {
            Self {
                // very shitty, includes 1 to account for use in linearize
                // where a placeholder label get generated to represent nop.
                syms: Box::new(itertools::chain(vec![1].into_iter(), syms.into_iter()).into_iter()),
            }
        }

        fn empty(syms: Vec<usize>) -> Self {
            Self {
                syms: Box::new(syms.into_iter()),
            }
        }
    }

    impl Uuids for UuidForTest {
        fn to_temp_map(&self, names: Vec<&'static str>) -> temp::TempMap {
            todo!()
        }

        fn new() -> Self {
            panic!();
        }

        fn named_temp(&mut self, _: &str) -> temp::Temp {
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

        fn new_label(&mut self) -> temp::Label {
            let nxt = self.syms.next();
            match nxt {
                None => {
                    panic!("test impl bug or actual bug: ran out of symbols");
                }
                Some(s) => temp::test_helpers::new_label(s),
            }
        }

        fn named_label(&mut self, _: &str) -> Label {
            panic!();
        }
    }

    mod linearize {
        use crate::temp::UuidsImpl;

        use super::*;

        #[test]
        fn const_is_identity() {
            let mut gen: UuidsImpl = Uuids::new();
            let expected = vec![Exp(Const(42))];
            let actual = linearize(Exp(Const(42)), &mut gen);
            assert_eq!(expected, actual);
        }

        #[test]
        fn name_is_identity() {
            let mut gen: UuidsImpl = Uuids::new();
            let l = gen.new_label();
            let expected = vec![Exp(Name(l))];
            let actual = linearize(Exp(Name(l)), &mut gen);
            assert_eq!(expected, actual);
        }

        #[test]
        fn temp_is_identity() {
            let mut gen: UuidsImpl = Uuids::new();
            let t = gen.new_temp();
            let expected = vec![Exp(Temp(t))];
            let actual = linearize(Exp(Temp(t)), &mut gen);
            assert_eq!(expected, actual);
        }

        #[test]
        fn exp_eseq() {
            let mut gen: UuidsImpl = Uuids::new();
            let l = gen.new_label();
            let t = gen.new_temp();

            let expected = vec![Label(l), Exp(Temp(t))];
            let actual = linearize(Exp(Eseq(Label(l), Temp(t))), &mut gen);
            assert_eq!(expected, actual);
        }

        #[test]
        fn eseq_eseq() {
            let mut gen: UuidsImpl = Uuids::new();
            let l = gen.new_label();
            let l2 = gen.new_label();
            let t = gen.new_temp();

            let expected = vec![Label(l), Label(l2), Exp(Temp(t))];
            let actual = linearize(Exp(Eseq(Label(l), Eseq(Label(l2), Temp(t)))), &mut gen);
            assert_eq!(expected, actual);
        }

        #[test]
        fn binop_eseq_left() {
            let mut gen: UuidsImpl = Uuids::new();
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
            let mut gen: UuidsImpl = Uuids::new();
            let l = gen.new_label();
            let l2 = gen.new_label();
            let t = gen.new_temp();

            let expected = vec![Label(l), Label(l2), Exp(Mem(Temp(t)))];
            let actual = linearize(Exp(Mem(Eseq(Label(l), Eseq(Label(l2), Temp(t))))), &mut gen);
            assert_eq!(expected, actual);
        }

        #[test]
        fn jump_eseq() {
            let mut gen: UuidsImpl = Uuids::new();
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
            let mut gen: UuidsImpl = Uuids::new();

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
            let mut gen: UuidsImpl = Uuids::new();
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
            let mut gen = UuidForTest::new_for_linearize(vec![2]);
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
            let mut gen = UuidForTest::new_for_linearize(vec![1]);

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
            let mut gen: UuidsImpl = Uuids::new();

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
            let mut gen: UuidsImpl = Uuids::new();
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
            let mut gen: UuidsImpl = Uuids::new();
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
            let mut gen: UuidsImpl = Uuids::new();
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
            let mut gen = UuidForTest::new_for_linearize(vec![2]);
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
            let mut gen = UuidForTest::new_for_linearize(vec![2, 3]);
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
            let mut gen: UuidsImpl = Uuids::new();
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
            let mut gen: UuidsImpl = Uuids::new();
            let t = gen.new_temp();
            let expected = vec![Exp(Const(1)), Exp(Const(2))];
            let actual = linearize(Seq(Exp(Const(1)), Exp(Const(2))), &mut gen);
            assert_eq!(expected, actual);
        }
    }

    mod basic_block {
        use super::*;
        use crate::{
            canon::basic_blocks,
            temp::{test_helpers, Uuids, UuidsImpl},
        };

        #[test]
        fn single_block_no_begin_label_no_jump_cjump_end() {
            // checks
            // 1. insertion of label if not present
            // 2. insertion of jump to end label if not present
            // 3. insertion of jump at end of block if not present.
            let t = test_helpers::new_temp(1);
            let stmts = vec![Move(Temp(t), Const(1))];
            let mut gen: UuidsImpl = Uuids::new();
            let (blk_map, end_lbl) = basic_blocks(stmts, &mut gen);
            assert_eq!(1, blk_map.len());
            let (lbl, blk) = blk_map.into_iter().next().unwrap();
            assert_eq!(
                vec![
                    Label(lbl),
                    Move(Temp(t), Const(1)),
                    Jump(Name(end_lbl), vec![end_lbl])
                ],
                blk.stmts
            );
        }

        #[test]
        fn jumps_only() {
            let lbl = test_helpers::new_label(1);
            let stmts = vec![Jump(Name(lbl), vec![lbl]), Jump(Name(lbl), vec![lbl])];
            let mut gen: UuidsImpl = Uuids::new();
            let (blk_map, _) = basic_blocks(stmts, &mut gen);
            assert_eq!(2, blk_map.len());
            for (blk_lbl, blk) in blk_map {
                assert_eq!(vec![Label(blk_lbl), Jump(Name(lbl), vec![lbl])], blk.stmts);
            }
        }

        #[test]
        fn cjumps_only() {
            let l1 = test_helpers::new_label(1);
            let l2 = test_helpers::new_label(1);
            let stmts = vec![
                Cjump(Ge, Const(1), Const(1), l1, l2),
                Cjump(Ge, Const(1), Const(1), l1, l2),
            ];
            let mut gen: UuidsImpl = Uuids::new();
            let (blk_map, _) = basic_blocks(stmts, &mut gen);
            assert_eq!(2, blk_map.len());
            for (blk_lbl, blk) in blk_map {
                assert_eq!(
                    vec![Label(blk_lbl), Cjump(Ge, Const(1), Const(1), l1, l2)],
                    blk.stmts
                );
            }
        }

        #[test]
        #[should_panic]
        fn craps_out_on_seq() {
            let mut gen: UuidsImpl = Uuids::new();
            let l = test_helpers::new_label(200);
            let input = vec![Seq(Label(l), Label(l))];

            basic_blocks(input, &mut gen);
        }

        #[test]
        fn potpourri() {
            let mut gen = UuidForTest::empty(vec![1, 2, 3, 4]);
            let l1 = test_helpers::new_label(200);
            let l2 = test_helpers::new_label(201);
            let t = test_helpers::new_label(101);
            let f = test_helpers::new_label(102);
            // Seq is not included here because irl never show up as the input would have been
            // already transformed by linearize to eliminate Seq's.
            let input = vec![
                // label insert expected
                Exp(Const(1)),
                Exp(Const(2)),
                Jump(Name(t), vec![t]),
                // label insert expected
                Cjump(Ge, Const(1), Const(1), t, f),
                Label(l1),
                Jump(Name(t), vec![t]),
                Label(l2),
                Cjump(Ge, Const(1), Const(1), t, f),
                // label insert expected
                Exp(Const(3)),
                // jump insert to end label expected
            ];
            let (blk_map, end_lbl) = basic_blocks(input, &mut gen);
            assert_eq!(5, blk_map.len());

            assert_eq!(
                vec![Label(l1), Jump(Name(t), vec![t])],
                blk_map.get(&l1).unwrap().stmts
            );
            assert_eq!(
                vec![Label(l2), Cjump(Ge, Const(1), Const(1), t, f)],
                blk_map.get(&l2).unwrap().stmts
            );

            let tmp_lbl = test_helpers::new_label(2);
            assert_eq!(
                vec![
                    Label(tmp_lbl),
                    Exp(Const(1)),
                    Exp(Const(2)),
                    Jump(Name(t), vec![t])
                ],
                blk_map.get(&tmp_lbl).unwrap().stmts
            );

            let tmp_lbl = test_helpers::new_label(3);
            assert_eq!(
                vec![Label(tmp_lbl), Cjump(Ge, Const(1), Const(1), t, f)],
                blk_map.get(&tmp_lbl).unwrap().stmts
            );

            let tmp_lbl = test_helpers::new_label(4);
            assert_eq!(
                vec![
                    Label(tmp_lbl),
                    Exp(Const(3)),
                    Jump(Name(end_lbl), vec![end_lbl])
                ],
                blk_map.get(&tmp_lbl).unwrap().stmts
            );
        }
    }

    mod trace {
        use crate::temp::UuidsImpl;

        use super::*;

        #[test]
        fn blist_vec_test() {
            let l1 = test_helpers::new_label(100);
            let l2 = test_helpers::new_label(101);
            let l3 = test_helpers::new_label(102);
            let done_label = test_helpers::new_label(200);
            let mut blocks = vec![
                Block {
                    marked: false,
                    stmts: vec![Label(l1), Jump(Name(l3), vec![l3])],
                },
                Block {
                    marked: false,
                    stmts: vec![Label(l2), Jump(Name(l3), vec![l3])],
                },
                Block {
                    marked: false,
                    stmts: vec![Label(l3), Jump(Name(done_label), vec![done_label])],
                },
            ];
            assert_eq!(
                vec![Label(l1), Jump(Name(l3), vec![l3])],
                (&mut blocks as &mut dyn BlockList).next().unwrap().stmts
            );
            assert_eq!(
                vec![Label(l2), Jump(Name(l3), vec![l3])],
                (&mut blocks as &mut dyn BlockList).next().unwrap().stmts
            );
            assert_eq!(
                vec![Label(l3), Jump(Name(done_label), vec![done_label])],
                (&mut blocks as &mut dyn BlockList).next().unwrap().stmts
            );
            assert!((&mut blocks as &mut dyn BlockList).next().is_none());
        }

        // validates all the original statement present.
        // TODO kinda pita with the elimination of jumps and rearranging of cjumps.
        #[test]
        fn jumps_follow_by_jump_dst() {
            let l1 = test_helpers::new_label(100);
            let l2 = test_helpers::new_label(101);
            let l3 = test_helpers::new_label(102);
            let done_label = test_helpers::new_label(200);
            let mut gen: UuidsImpl = Uuids::new();
            let blocks = vec![
                Block {
                    marked: false,
                    stmts: vec![Label(l1), Jump(Name(l3), vec![l3])],
                },
                Block {
                    marked: false,
                    stmts: vec![Label(l2), Jump(Name(l3), vec![l3])],
                },
                Block {
                    marked: false,
                    stmts: vec![Label(l3), Jump(Name(done_label), vec![done_label])],
                },
            ];
            let actual = trace_schedule(blocks, done_label, &mut gen);
            let expected: Vec<IrStm> = vec![
                Label(l1),
                Label(l3),
                Jump(Name(done_label), vec![done_label]),
                Label(l2),
                Jump(Name(l3), vec![l3]),
                Label(done_label), // this should always follow
            ];
            assert_eq!(expected, actual);
        }

        #[test]
        fn jump_to_end_lbl_follow_by_end_lbl() {
            let l1 = test_helpers::new_label(100);
            let done_label = test_helpers::new_label(200);
            let mut gen: UuidsImpl = Uuids::new();
            let blocks = vec![Block {
                marked: false,
                stmts: vec![Label(l1), Jump(Name(done_label), vec![done_label])],
            }];
            let actual = trace_schedule(blocks, done_label, &mut gen);
            let expected: Vec<IrStm> = vec![
                Label(l1),
                // will just tolerate this jump followed by its target label because the
                // done_label isn't part of the blocklist but is instead tacked on at the end.
                // we can always remove it if we need to.
                Jump(Name(done_label), vec![done_label]),
                Label(done_label), // this should always follow
            ];
            assert_eq!(expected, actual);
        }

        #[test]
        fn cjump_follow_by_false_label_left_alone() {
            let l1 = test_helpers::new_label(100);
            let lt = test_helpers::new_label(201);
            let lf = test_helpers::new_label(202);
            let done_label = test_helpers::new_label(300);
            let mut gen: UuidsImpl = Uuids::new();
            let blocks = vec![
                Block {
                    marked: false,
                    stmts: vec![Label(l1), Cjump(Ge, Const(0), Const(0), lt, lf)],
                },
                Block {
                    marked: false,
                    stmts: vec![Label(lf), Jump(Name(done_label), vec![done_label])],
                },
            ];
            let actual = trace_schedule(blocks, done_label, &mut gen);
            let expected: Vec<IrStm> = vec![
                Label(l1),
                Cjump(Ge, Const(0), Const(0), lt, lf),
                Label(lf),
                Jump(Name(done_label), vec![done_label]),
                Label(done_label), // this should always follow
            ];
            assert_eq!(expected, actual);
        }

        #[test]
        fn cjump_follow_by_true_label_is_negated() {
            let l1 = test_helpers::new_label(100);
            let lt = test_helpers::new_label(201);
            let lf = test_helpers::new_label(202);
            let done_label = test_helpers::new_label(300);
            let mut gen: UuidsImpl = Uuids::new();
            let blocks = vec![
                Block {
                    marked: false,
                    stmts: vec![Label(l1), Cjump(Ge, Const(0), Const(0), lt, lf)],
                },
                Block {
                    marked: false,
                    stmts: vec![Label(lt), Jump(Name(done_label), vec![done_label])],
                },
            ];
            let actual = trace_schedule(blocks, done_label, &mut gen);
            let expected: Vec<IrStm> = vec![
                Label(l1),
                Cjump(Lt, Const(0), Const(0), lf, lt),
                Label(lt),
                Jump(Name(done_label), vec![done_label]),
                Label(done_label), // this should always follow
            ];
            assert_eq!(expected, actual);
        }

        #[test]
        fn cjump_not_follow_by_true_or_false_has_empty_block_inserted() {
            let l1 = test_helpers::new_label(100);
            let l2 = test_helpers::new_label(101);
            let lt = test_helpers::new_label(201);
            let lf = test_helpers::new_label(202);
            let done_label = test_helpers::new_label(300);
            let mut gen = UuidForTest::empty(vec![1]);
            let ff = test_helpers::new_label(1);
            let blocks = vec![
                Block {
                    marked: false,
                    stmts: vec![Label(l1), Cjump(Ge, Const(0), Const(0), lt, lf)],
                },
                Block {
                    marked: false,
                    stmts: vec![Label(l2), Jump(Name(done_label), vec![done_label])],
                },
            ];
            let actual = trace_schedule(blocks, done_label, &mut gen);
            let expected: Vec<IrStm> = vec![
                Label(l1),
                Cjump(Ge, Const(0), Const(0), lt, ff),
                Label(ff),
                Jump(Name(lf), vec![lf]),
                Label(l2),
                Jump(Name(done_label), vec![done_label]),
                Label(done_label), // this should always follow
            ];
            assert_eq!(expected, actual);
        }
    }
}
