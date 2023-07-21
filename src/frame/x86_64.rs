use core::num;

use itertools::Itertools;

use crate::{
    assem::{Dst, Instr, Src},
    frame::{Access, Escapes, Frame, Register},
    ir,
    ir::{helpers::*, IrBinop, IrExp, IrStm},
    temp,
    temp::{Label, Uuids},
    translate,
};

#[derive(Debug)]
pub struct x86_64_Frame {
    name: Label,
    formals: Vec<Access>,
    // the sequence of moves to put register parameter to place from which it is seen
    // in this frame. if it's a InReg param it should be Move(t_fresh, <temp corresponding to arg reg>)
    // if it is a InFrame then it should be Move(Mem(+(FP, Offset)), <temp corresponding to arg reg>).
    // with the exception of the tigermain, every user defined fn is compiled with the static link. although,
    // it is possible (as an optimization) to skip the static link if none of the calls can result in a
    // variable access of a variable defined in a higher level.
    // since we always have static link that escapes, this statement should always be constructable.
    // if not, it is an impl bug!
    formals_move: Option<IrStm>,
    num_locals: usize,
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

pub const WORD_SIZE: usize = 8;

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
    fn asm_file_prologue() -> &'static str where Self: Sized {
        ".intel_syntax noprefix"
    }

    fn registers() -> &'static [Register]
    where
        Self: Sized,
    {
        &[
            RDI, RSI, RDX, RCX, RSP, RAX, RBX, RBP, R8, R9, R10, R11, R12, R13, R14, R15,
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
        WORD_SIZE
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

    fn return_value_register(gen: &mut dyn Uuids) -> temp::Temp
    where
        Self: Sized,
    {
        gen.named_temp(RAX)
    }

    fn proc_entry_exit1(&mut self, body: IrStm, can_spill: bool, gen: &mut dyn Uuids) -> IrStm {
        let mut moves = Vec::new();
        // TODO add these callee save stuff later.
        // if can_spill {
        //     todo!()
        // } else {
        //     moves.push(IrStm::Label(gen.named_label(".callee_save")));
        //     // move callee save registers and the return address registers.
        //     for name in CALLEE_SAVES.iter().chain([RAX].iter()) {
        //         let acc = self.alloc_local(true, gen);
        //         match acc {
        //             Access::InFrame(offset) => {
        //                 moves.push(Move(
        //                     Mem(Binop(
        //                         IrBinop::Plus,
        //                         IrExp::Const(offset),
        //                         IrExp::Temp(Self::frame_pointer(gen)),
        //                     )),
        //                     IrExp::Temp(gen.named_temp(name)),
        //                 ));
        //             }
        //             Access::InReg(..) => panic!("impl bug: expected InFrame for local"),
        //         }
        //     }
        //     if let Some(ref formals_move) = self.formals_move {
        //         moves.push(formals_move.clone());
        //     }
        moves.push(body);
        translate::make_seq(moves)
        // }
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
        gen: &mut dyn Uuids,
    ) -> (super::Prologue, super::Epilogue) {
        let function_name =                 self.name.resolve_named_label(gen);
        let prologue = if self.num_locals > 0 {
            format!(
                "{}:\n.{}_prologue:\n\tpush rbp\n\tmov rbp, rsp\n\tsub rsp, {}",
                function_name,
                function_name,
                self.num_locals * WORD_SIZE
            )
        } else {
            format!(
                "{}:\n.{}_prologue:\n\tpush rbp\n\tmov rbp, rsp",
                function_name,
                function_name,
            )
        };

        let epilogue = if self.num_locals > 0 {
            format!(
                ".{}_epilogue:\n\tadd rsp, {}\n\tpop rbp\n\tret\n\t.globl {}\n\t.type {}, @function",
                function_name,
                self.num_locals * WORD_SIZE,
                function_name,
                function_name,
            )
        } else {
            format!(
                ".{}_epilogue:\n\tpop rbp\n\tret",
                function_name,
            )
        };
        return (prologue, epilogue);
    }

    fn new(name: Label, formals_escapes: Vec<Escapes>, gen: &mut dyn Uuids) -> Self {
        let mut formals = Vec::with_capacity(formals_escapes.len());

        // if it's part of the first 6 args, it is InReg, unless it escapes.
        // everything else is InFrame.
        let mut in_frame_offset = 0;

        for (i, escape) in formals_escapes.iter().enumerate() {
            if i < ARG_REGS.len() {
                if *escape {
                    formals.push(Access::InFrame(in_frame_offset));
                    in_frame_offset += WORD_SIZE as i32;
                } else {
                    let t = gen.new_unnamed_temp();
                    formals.push(Access::InReg(t));
                }
            } else {
                formals.push(Access::InFrame(in_frame_offset));
                in_frame_offset += WORD_SIZE as i32;
            }
        }
        // create the moves.
        let mut moves = Vec::new();
        if formals.len() > 0 {
            moves.push(IrStm::Label(gen.named_label(".move_arguments")));
        }
        for (i, f) in formals.iter().enumerate() {
            if i < ARG_REGS.len() {
                // these need to be moved from the arg register into whatever location
                // they were assigned to.
                match f {
                    Access::InReg(t) => {
                        moves.push(Move(
                            IrExp::Temp(*t),
                            IrExp::Temp(gen.named_temp(ARG_REGS[i])),
                        ));
                    }
                    Access::InFrame(offset) => {
                        moves.push(Move(
                            Mem(Binop(
                                IrBinop::Plus,
                                IrExp::Const(*offset),
                                IrExp::Temp(Self::frame_pointer(gen)),
                            )),
                            IrExp::Temp(gen.named_temp(ARG_REGS[i])),
                        ));
                    }
                }
            } else {
                // already on stack, no need to move.
                break;
            }
        }

        Self {
            name,
            formals,
            formals_move: if moves.len() > 0 {
                Some(translate::make_seq(moves))
            } else {
                // happens when there is no args at all.
                // this happens with the top level pre-defined fns and tigermain.
                None
            },
            num_locals: 0,
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
            let offset = ((self.num_locals + 1) * WORD_SIZE) as i32 * -1;
            let res = Access::InFrame(offset);
            self.num_locals += 1;
            res
        } else {
            Access::InReg(gen.new_unnamed_temp())
        }
    }
}
