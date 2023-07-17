use crate::{
    assem::{Dst, Instr, Src},
    frame::{Access, Escapes, Frame, Register},
    ir,
    ir::{helpers::*, IrExp, IrStm, IrBinop},
    temp,
    temp::{Label, Uuids},
    translate
};

#[derive(Debug)]
pub struct x86_64_Frame {
    name: Label,
    formals: Vec<Access>,
    // the sequence of moves to put register parameter to place from which it is seen
    // in this frame. if it's a InReg param it should be Move(t_fresh, <temp corresponding to arg reg>)
    // if it is a InFrame then it should be Move(Mem(+(FP, Offset)), <temp corresponding to arg reg>)
    // since we always have static link that escapes, this statement should always be constructable.
    // if not, it is an impl bug!
    formals_move: IrStm,
    next_local_offset: i32,
}

pub const RBP: &str = "rbp";

// this isn't directly accessible as a register but various instructions read/write
// parts of it. e.g. cmp updates CF, OF, SF, ZF, AF, and PF flags. a subset of jump
// instructions will act based on this register. Ideally, could track more fine grained
// bits of the flag, but just tracking the entire register seems more KISS.
pub const FLAGS: &str = "flags";

// pub const
pub const RDI: &str = "rdi";
pub const RSI: &str = "rsi";
pub const RDX: &str = "rdx";
pub const RCX: &str = "rcx";
pub const RSP: &str = "rsp";
pub const RAX: &str = "rax";
pub const RBX: &str = "rbx";
pub const R8: &str = "r8";
pub const R9: &str = "r9";
pub const R10: &str = "r10";
pub const R11: &str = "r11";
pub const R12: &str = "r12";
pub const R13: &str = "r13";
pub const R14: &str = "r14";
pub const R15: &str = "r15";

// used for passing arguments.
pub const ARG_REGS: &[&str] = &[RDI, RSI, RDX, RCX, R8, R9];

// implement special registers such as FP, SP.
pub const SPECIAL_REGS: &[&str] = &[RSP, RBP];

pub const CALLEE_SAVES: &[&str] = &[RBP, RSP, RBX, R12, R13, R14, R15];

pub const CALLER_SAVES: &[&str] = &[RAX, RCX, RDX, RSI, RDI, R8, R9, R10, R11];

#[inline]
pub fn named_register(gen: &mut dyn Uuids, name: &'static str) -> temp::Temp {
    gen.named_temp(name)
}

pub fn callee_saves(gen: &mut dyn Uuids) -> Vec<temp::Temp> {
    CALLEE_SAVES
        .iter()
        .map(|name| gen.named_temp(name))
        .collect()
}

pub fn caller_saves(gen: &mut dyn Uuids) -> Vec<temp::Temp> {
    CALLER_SAVES.iter().map(|reg| gen.named_temp(reg)).collect()
}

pub fn arg_regs(gen: &mut dyn Uuids) -> Vec<temp::Temp> {
    ARG_REGS.iter().map(|reg| gen.named_temp(reg)).collect()
}

pub fn special_regs(gen: &mut dyn Uuids) -> Vec<temp::Temp> {
    SPECIAL_REGS.iter().map(|reg| gen.named_temp(reg)).collect()
}

impl Frame for x86_64_Frame {
    fn registers() -> &'static [Register]
    where
        Self: Sized,
    {
        &[
            RDI, RSI, RDX, RCX, RSP, RAX, RBX, R8, R9, R10, R11, R12, R13, R14, R15,
        ]
    }

    fn temp_map(gen: &mut dyn Uuids) -> temp::TempMap
    where
        Self: Sized,
    {
        gen.to_temp_map(Self::registers())
    }

    fn external_call(name: Label, exps: Vec<ir::IrExp>) -> ir::IrExp
    where
        Self: Sized,
    {
        Call(IrExp::Name(name), exps)
    }

    fn word_size() -> usize
    where
        Self: Sized,
    {
        8
    }

    fn string(l: temp::Label, s: &str) -> String
    where
        Self: Sized,
    {
        let id = match l {
            temp::Label::Named(_) => panic!("impl bug: string should not have named label"),
            temp::Label::Unnamed(id) => id,
        };
        format!(".L{}:\n\t.string {}", id, s)
    }

    fn frame_pointer(gen: &mut dyn Uuids) -> temp::Temp
    where
        Self: Sized,
    {
        gen.named_temp(RBP)
    }

    fn proc_entry_exit1(&self, body: IrStm, can_spill: bool) -> IrStm {
        // TODO insert self.formals_move
        // TODO
        if can_spill {
            todo!()
        } else {
            //
        }

        IrStm::Exp(Box::new(IrExp::Const(42)))
    }

    fn proc_entry_exit2(&self, instrs: &mut Vec<crate::assem::Instr>, gen: &mut dyn Uuids) {
        let mut regs = callee_saves(gen);
        regs.append(&mut special_regs(gen));
        regs.dedup();
        instrs.push(Instr::Oper {
            assem: "".into(),
            src: Src(regs),
            dst: Dst::empty(),
            jump: vec![],
        });
    }

    fn proc_entry_exit3(
        &self,
        instrs: &Vec<crate::assem::Instr>,
    ) -> (super::Prologue, super::Epilogue) {
        todo!()
    }

    fn new(name: Label, formals_escapes: Vec<Escapes>, gen: &mut dyn Uuids) -> Self {
        let mut formals = Vec::with_capacity(formals_escapes.len());
        let mut moves = Vec::new();
        let mut next_local_offset: i32 = -8;
        for (i, escape) in formals_escapes.iter().enumerate() {
            if i > 5 || *escape {
                formals.push(Access::InFrame(next_local_offset));
                next_local_offset = next_local_offset
                    .checked_add(Self::word_size() as i32 * -1)
                    .unwrap();
                if i < 6 {
                    let arg_reg = ARG_REGS[i];
                    moves.push(Move(
                        Mem(Binop(
                            IrBinop::Plus,
                            IrExp::Const(next_local_offset),
                            IrExp::Temp(Self::frame_pointer(gen)),
                        )),
                        IrExp::Temp(gen.named_temp(arg_reg)),
                    ));
                }
            } else {
                let t = gen.new_temp();
                formals.push(Access::InReg(t));
                let arg_reg = ARG_REGS[i];
                moves.push(Move(IrExp::Temp(t), IrExp::Temp(gen.named_temp(arg_reg))));
            }
        }
        Self {
            name,
            formals,
            formals_move: translate::make_seq(moves),
            next_local_offset
        }
    }

    fn name(&self) -> Label {
        self.name
    }

    fn formals(&self) -> &[Access] {
        &self.formals
    }

    fn alloc_local(&mut self, escapes: Escapes, gen: &mut dyn Uuids) -> Access {
        if escapes {
            let res = Access::InFrame(self.next_local_offset);
            self.next_local_offset.checked_add(-8).unwrap();
            res
        } else {
            Access::InReg(gen.new_temp())
        }
    }
}
