use std::{cmp::max, collections::HashMap};

use crate::{
    assem::*,
    frame,
    frame::{x86_64, x86_64::x86_64_Frame, Frame, FrameRef},
    ir::{
        IrBinop::{self, *},
        IrExp::*,
        IrRelop::*,
        IrStm::*,
    },
    temp::{TempMap, Uuids},
};

pub struct X86Asm;

impl Codegen for X86Asm {
    fn munch_stm(stm: IrStm, result: &mut Vec<Instr>, gen: &mut dyn Uuids) {
        match stm {
            Move(dst_exp, src_exp) => {
                // After canonicalizing, we should only end up with
                // Move(Mem, _) or Move(Temp, _)

                let src = Self::munch_exp(*src_exp, result, gen);

                match *dst_exp {
                    IrExp::Mem(tgt) => {
                        let dst = Self::munch_exp(*tgt, result, gen);
                        result.push(Instr::Oper {
                            assem: "mov ['S1], 'S0".into(),
                            dst: Dst::empty(),
                            src: Src(vec![src, dst]),
                            jump: vec![],
                        });
                    }
                    IrExp::Temp(x) => {
                        result.push(Instr::Move {
                            assem: "mov 'D, 'S",
                            dst: x,
                            src,
                        });
                    }
                    _ => panic!("impl bug, Move should be to a Temp or Mem"),
                }
            }
            Jump(e, target_labels) => {
                let t = Self::munch_exp(*e, result, gen);
                result.push(Instr::Oper {
                    assem: "jmp 'S0".into(),
                    dst: Dst::empty(),
                    src: Src(vec![t]),
                    jump: target_labels,
                });
            }
            Cjump(r, a, b, lt, lf) => {
                let ta = Self::munch_exp(*a, result, gen);
                let tb = Self::munch_exp(*b, result, gen);
                result.push(Instr::Oper {
                    assem: "cmp 'D0, 'S0".into(),
                    dst: Dst(vec![ta]),
                    src: Src(vec![tb, ta]),
                    jump: vec![],
                });

                let assem = match r {
                    Eq => "je ['J0]",
                    Ne => "jne ['J0]",
                    Lt => "jl ['J0]",
                    Gt => "jg ['J0]",
                    Le => "jle ['J0]",
                    Ge => "jge ['J0]",
                    Ult => "jb ['J0]",
                    Ule => "jbe ['J0]",
                    Ugt => "ja ['J0]",
                    Uge => "jae ['J0]",
                };
                result.push(Instr::Oper {
                    assem: assem.into(),
                    dst: Dst::empty(),
                    src: Src::empty(),
                    jump: vec![lt, lf],
                });
            }
            Exp(e) => {
                let e_temp = Self::munch_exp(*e, result, gen);
            }
            Label(lab) => match lab {
                temp::Label::Named(..) => result.push(Instr::Label {
                    assem: "'L:".into(), // function labels don't need the .L prefix
                    lab,
                }),
                temp::Label::Unnamed(..) => result.push(Instr::Label {
                    assem: ".L'L:".into(), // non-fn labels need a .L prefix
                    lab,
                }),
            },
            Seq(..) => {
                panic!("impl bug: Seq should have been eliminated");
            }
        }
    }

    /// Given the IrExp, outputs the abstract register that holds the value.
    fn munch_exp(exp: IrExp, result: &mut Vec<Instr>, gen: &mut dyn Uuids) -> temp::Temp {
        match exp {
            Binop(op, a, b) => {
                let instr = match op {
                    Plus => "add 'D0, 'S0",
                    Minus => "sub 'D0, 'S0",
                    IrBinop::Mul => "imul 'D0, 'S0",
                    IrBinop::Div => "idiv 'S0", // TODO RAX is trashed
                    IrBinop::And => "and 'D0, 'S0",
                    IrBinop::Or => "or 'D0, 'S0",
                    // base tiger language doesn't have these
                    // so presumably they must come from optimizations.
                    Lshift => "shl 'D0, 'S0", // TODO shl r/m64, imm8; shl r/m64, CL; shl r/m64, 1; masked to 63 bits for the REX instructions
                    Rshift => "shr 'D0, 'S0", // TODO ditto as above;
                    ArShift => "sar 'D0, 'S0", // note idiv rounds quotient toward 0, sar rounds quotient toward neg infinity
                    IrBinop::Xor => "xor 'D0, 'S0",
                };
                let is_div = matches!(op, IrBinop::Div);
                let a_temp = Self::munch_exp(*a, result, gen);
                let b_temp = Self::munch_exp(*b, result, gen);
                result.push(Instr::Oper {
                    assem: instr.into(),
                    dst: Dst(if is_div {
                        // div is a single operand operation that operates on RAX.
                        // by construction, a_temp is a fresh temporary, so it will not be RAX.
                        vec![a_temp, x86_64::named_register(gen, x86_64::RAX)]
                    } else {
                        vec![a_temp]
                    }),
                    // op a, b where a is both read and written to.
                    // must include a here because it is also a source.
                    // in trivial reg allocation, this would result in a load of a_temp from memory before the instruction.
                    // but, since we use 'S0, b_temp must come first...not deep but very tricky.
                    src: Src(vec![b_temp, a_temp]),
                    jump: vec![],
                });
                a_temp
            }
            IrExp::Call(f, args) => {
                let num_args = args.len();
                let f_temp = Self::munch_exp(*f, result, gen);
                let mut arg_regs = Vec::with_capacity(args.len());
                for arg_exp in args {
                    arg_regs.push(Self::munch_exp(arg_exp, result, gen));
                }

                if num_args > 0 {
                    let mut i = arg_regs.len() - 1;
                    // args after the 6th one go on stack.
                    while i > 5 {
                        result.push(Instr::Oper {
                            assem: "push 'S0".into(),
                            dst: Dst(vec![]),
                            src: Src(vec![arg_regs[i]]),
                            jump: vec![],
                        });
                        i -= 1;
                    }

                    // note about the use of rax, rdx as return value registers.
                    // tiger lang only has reference types or plain int types, so only need to implement the m=1 case.
                    // let m = number of return values.
                    // for m=1, rax holds return value.
                    // for m=2, we'd expect results in rax, rdx.
                    // for m>2, we'd have to reserve space on the caller stack to hold the results,
                    // BEFORE pushing arguments onto the stack. rdi would hold the value of rsp after
                    // we allocated that space but before pushing arguments, which gives us 1 less register
                    // to pass arguments in (so 5 instead of 6).
                    while i + 1 > 0 {
                        let arg_passing_regs = x86_64::arg_regs(gen);
                        result.push(Instr::Oper {
                            assem: "mov 'D0, 'S0".into(),
                            dst: Dst(vec![arg_passing_regs[i]]),
                            src: Src(vec![arg_regs[i]]),
                            jump: vec![],
                        });
                        if i == 0 {
                            break;
                        }
                        i -= 1;
                    }
                }
                // do the call
                result.push(Instr::Oper {
                    assem: "call 'S0".into(),
                    dst: Dst(vec![x86_64::named_register(gen, x86_64::RAX)]),
                    src: Src(vec![f_temp]),
                    jump: vec![],
                });

                // persist the result register.
                let dest = gen.new_unnamed_temp();
                result.push(Instr::Oper {
                    assem: "mov 'D0, 'S0".into(),
                    dst: Dst(vec![dest]),
                    src: Src(vec![x86_64::named_register(gen, x86_64::RAX)]),
                    jump: vec![],
                });

                // don't think we'd ever get to a point where someone
                // passes enough arguments to overflow an i32.
                if max(0, num_args as i32 - 6) > 0 {
                    result.push(Instr::Oper {
                        assem: format!("add rsp, {}", x86_64::WORD_SIZE * (num_args - 6)),
                        dst: Dst(vec![x86_64::named_register(gen, x86_64::RAX)]),
                        src: Src::empty(),
                        jump: vec![],
                    });
                }

                dest
            }
            Const(i) => {
                let t = gen.new_unnamed_temp();
                result.push(Instr::Oper {
                    assem: format!("mov  'D0, {}", i),
                    dst: Dst(vec![t]),
                    src: Src::empty(),
                    jump: vec![],
                });
                t
            }
            Temp(t) => t,
            Name(label) => {
                let t = gen.new_unnamed_temp();
                let is_named_label = match label {
                    temp::Label::Named(..) => true,
                    temp::Label::Unnamed(..) => false,
                };

                result.push(Instr::Oper {
                    assem: if is_named_label {
                        "lea 'D0, 'J0@PLT[rip]".into()
                    } else {
                        "lea 'D0, .L'J0[rip]".into()
                    },
                    dst: Dst(vec![t]),
                    src: Src::empty(),
                    // TODO is jump used for analysis?
                    // if so we need to print out the label into the string
                    jump: vec![label],
                });
                t
            }
            Mem(e) => Self::munch_exp(*e, result, gen),
            Eseq(..) => panic!("impl bug: Eseq should have been eliminated"),
            Null => {
                let t = gen.new_unnamed_temp();
                result.push(Instr::Oper {
                    assem: "mov 'D0, 0".into(),
                    dst: Dst(vec![t]),
                    src: Src::empty(),
                    jump: vec![],
                });
                t
            }
        }
    }

    /// The entry point for translating into
    fn code_gen_frame(_: FrameRef, stm: IrStm, instrs: &mut Vec<Instr>, gen: &mut dyn Uuids) {
        Self::munch_stm(stm, instrs, gen);
    }
}

/// This module contains the code to do trivial register allocation.
pub mod trivial_reg {
    use super::*;

    /// In a given frame, maps the temporary to the offset in the frame.
    pub struct TempOffset(HashMap<temp::Temp, i32>);

    impl TempOffset {
        pub fn new() -> Self {
            TempOffset(HashMap::new())
        }

        fn get_or_allocate(
            &mut self,
            frame: FrameRef,
            tmp: temp::Temp,
            gen: &mut dyn Uuids,
        ) -> i32 {
            if let Some(offset) = self.0.get(&tmp) {
                return *offset;
            }
            let access = frame.borrow_mut().alloc_local(true, gen);
            match access {
                frame::Access::InFrame(offset) => {
                    self.0.insert(tmp, offset);
                    offset
                }
                _ => panic!("impl bug, alloc_local call here should return InFrame"),
            }
        }
    }

    /// The registers we use on x86.
    /// Ordering matters. RAX is listed last because it is used as return register.
    /// The Jump at the end of a basic block would require a register, so we don't want that to
    /// trample the RAX content.
    const TRIVIAL_REGISTERS: [&str; 3] = [x86_64::RCX, x86_64::RDX, x86_64::RAX, ];

    /// Performs register allocation for a single instruction.
    /// If a machine register is part of the `src` of an instruction (i.e. "use" set of the instruction)
    /// then we are not gonna use it, because it is "live" until the actual instruction. Instead we would
    /// pick the next available register. It is assumed an instruction at most uses 3 registers, so it is
    /// a bug if we do run out.
    ///
    /// The algorithm is simple. We look at the temporaries in the src/dst fields of Instr::Move and Instr::Oper.
    ///
    /// If temporary is built-in, we leave it alone.
    ///
    /// If it is a src register, we pick the next available machine register r, and do "mov r, [fp + offset]"
    ///         where offset is the stack offset of this register. due to trace scheduling, it is possible for
    ///         the use of a source temporary to show up before it is assigned to. hence, we use get_or_allocate
    ///         for source temporaries as well.
    /// Otherwise, it is a dst register.
    ///         allocate a spot for it in the frame if there is not one.
    ///         then, we assign an available machine register rres to it.
    ///         the instruction would have the src/dst registers replaced with machine register temporaries.
    ///         finally, after the instruction, we include a "mov [fp + offset], rres"
    ///             where offset is the fp relative offset of the dst register.
    ///         note: we will assume there is at most 1 abstract register that is used as the dst.
    pub fn do_trivial_register_allcation(
        frame: FrameRef,
        input: Instr,
        output: &mut Vec<Instr>, // each input maps to 1 or more final instructions.
        gen: &mut dyn Uuids,
        built_ins: &TempMap,
        temp_to_offset: &mut TempOffset,
    ) {
        if matches!(input, Instr::Label { .. }) {
            output.push(input);
            return;
        }

        // eliminate candidates that shows up in the source because they are the instruction's
        // data dependency, so we cannot use (overwrite) the register before we can perform the instruction.
        let sources = input.get_sources();
        let candidates = TRIVIAL_REGISTERS
            .map(|s| gen.named_temp(s))
            .iter()
            .filter(|x| !sources.contains(x))
            .map(|x| *x)
            .collect::<Vec<temp::Temp>>();

        // registers to color. derived from the template string.
        // note: we also filter out anything that is already colored.
        let srcs = input
            .get_sources()
            .iter()
            .filter(|r| !built_ins.contains_key(r))
            .map(|x| *x)
            .collect::<Vec<temp::Temp>>();
        let dsts = input
            .get_dests()
            .iter()
            .filter(|r| !built_ins.contains_key(r))
            .map(|x| *x)
            .collect::<Vec<temp::Temp>>();

        // we should always have enough colors to cover all the registers needed.
        assert!(
            srcs.len() + dsts.len() <= candidates.len(),
            "srcs={:?}, dsts={:?}, candidates={:?}",
            srcs,
            dsts,
            candidates
        );

        // validate the assumption that we only explicitly write to 1 abstract register.
        // i don't think any of the x86 instructions we use here can update more than 1.
        assert!(dsts.len() <= 1);

        // allocate space for dst in the current frame if it doesn't exist.
        for ref d in dsts.iter() {
            temp_to_offset.get_or_allocate(frame.clone(), **d, gen);
        }

        // do coloring.
        let mut colors = HashMap::with_capacity(3);
        let mut choices = candidates.iter();
        for r in srcs.iter().chain(dsts.iter()) {
            let color = choices.next().unwrap();
            colors.insert(r, *color);
        }

        // now we actually do code gen.
        match input {
            Instr::Move { assem, src, dst } => {
                if srcs.len() > 0 {
                    // copy src from memory into its assigned register.
                    let x: temp::Temp = srcs[0];
                    debug_assert!(x == src, "impl bug");
                    let src_offset = temp_to_offset.get_or_allocate(frame.clone(), src, gen);
                    output.push(Instr::Oper {
                        assem: format!("mov 'D0, [rbp + {}]", src_offset),
                        dst: Dst(vec![*colors.get(&x).unwrap()]),
                        src: Src::empty(),
                        jump: vec![],
                    });
                }

                output.push(Instr::Move {
                    assem,
                    src: *colors.get(&src).unwrap_or(&src),
                    dst: *colors.get(&dst).unwrap_or(&dst),
                });

                if dsts.len() > 0 {
                    // copy dst from its register back to memory.
                    let x = dsts[0];
                    debug_assert!(x == dst, "impl bug");
                    let dst_offset = temp_to_offset.get_or_allocate(frame.clone(), dst, gen);

                    output.push(Instr::Oper {
                        assem: format!("mov [rbp + {}], 'S0", dst_offset),
                        dst: Dst::empty(),
                        src: Src(vec![*colors.get(&x).unwrap()]),
                        jump: vec![],
                    });
                }
            }
            Instr::Oper {
                assem,
                dst,
                src,
                jump,
            } => {
                if srcs.len() > 0 {
                    for ref s in srcs.iter() {
                        let src_offset = temp_to_offset.get_or_allocate(frame.clone(), **s, gen);
                        output.push(Instr::Oper {
                            assem: format!("mov 'D0, [rbp + {}]", src_offset),
                            dst: Dst(vec![*colors.get(s).unwrap()]),
                            src: Src::empty(),
                            jump: vec![],
                        });
                    }
                }

                output.push(Instr::Oper {
                    assem,
                    dst: Dst(dst
                        .0
                        .iter()
                        .map(|x| colors.get(x).unwrap_or(x))
                        .map(|x| *x)
                        .collect()),
                    src: Src(src
                        .0
                        .iter()
                        .map(|x| colors.get(x).unwrap_or(x))
                        .map(|x| *x)
                        .collect()),
                    jump,
                });

                if dsts.len() > 0 {
                    for d in dsts.iter() {
                        let dst_offset = temp_to_offset.get_or_allocate(frame.clone(), *d, gen);
                        output.push(Instr::Oper {
                            assem: format!("mov [rbp + {}], 'S0", dst_offset),
                            dst: Dst::empty(),
                            src: Src(vec![*colors.get(&d).unwrap()]),
                            jump: vec![],
                        });
                    }
                }
            }
            Instr::Label { .. } => unreachable!(),
        }
    }
}
