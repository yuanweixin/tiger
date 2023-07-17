use std::cmp::max;

use crate::{
    assem::*,
    frame::{x86_64, FrameRef},
    ir::{
        IrBinop::{self, *},
        IrExp::*,
        IrRelop::*,
        IrStm::*,
    },
    temp::Uuids,
};

pub struct X86Asm;

impl Codegen for X86Asm {
    fn munch_stm(stm: IrStm, result: &mut Vec<Instr>, gen: &mut dyn Uuids) {
        match stm {
            Move(dst_exp, src_exp) => {
                let dst = Self::munch_exp(*dst_exp, result, gen);
                let src = Self::munch_exp(*src_exp, result, gen);

                result.push(Instr::Oper {
                    assem: "mov 'D0, 'S0".into(),
                    dst: Dst(vec![dst]),
                    src: Src(vec![src, dst]),
                    jump: vec![],
                });
            }
            Jump(e, target_labels) => {
                let t = Self::munch_exp(*e, result, gen);
                result.push(Instr::Oper {
                    assem: "jmp ['S0]".into(),
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
                let new_t = gen.new_temp();
                result.push(Instr::Oper {
                    assem: "mov 'D0, 'S0".into(),
                    dst: Dst(vec![new_t]),
                    src: Src(vec![e_temp]),
                    jump: vec![],
                });
            }
            Label(lab) => match lab {
                temp::Label::Named(..) => result.push(Instr::Label {
                    assem: "'L".into(), // function labels don't need the .L prefix
                    lab,
                }),
                temp::Label::Unnamed(..) => result.push(Instr::Label {
                    assem: ".L'L".into(), // non-fn labels need a .L prefix
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
                    IrBinop::Mul => "mul 'D0, 'S0",
                    IrBinop::Div => "div 'S0", // RAX is trashed
                    IrBinop::And => "and 'D0, 'S0",
                    IrBinop::Or => "or 'D0, 'S0",
                    // base tiger language doesn't have these
                    // so presumably they must come from optimizations.
                    Lshift => "shl 'D0, 'S0", // TODO shl r/m64, imm8; shl r/m64, CL; shl r/m64, 1; masked to 63 bits for the REX instructions
                    Rshift => "shr 'D0, 'S0", // TODO ditto as above;
                    ArShift => "sar 'D0, 'S0", // note idiv rounds quotient toward 0, sar rounds quotient toward neg infinity
                    IrBinop::Xor => "xor 'D0, 'S0",
                };
                let a_temp = Self::munch_exp(*a, result, gen);
                let b_temp = Self::munch_exp(*b, result, gen);
                result.push(Instr::Oper {
                    assem: instr.into(),
                    dst: Dst(vec![a_temp]),
                    src: Src(vec![b_temp]),
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
                let dest = gen.new_temp();
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
                        assem: format!("add rsp, 8 * {}", num_args - 6),
                        dst: Dst(vec![x86_64::named_register(gen, x86_64::RAX)]),
                        src: Src::empty(),
                        jump: vec![],
                    });
                }

                dest
            }
            Const(i) => {
                let t = gen.new_temp();
                result.push(Instr::Oper {
                    // TODO does this shit work for negative numbers?
                    assem: format!("mov 'D0, {}", i),
                    dst: Dst(vec![t]),
                    src: Src::empty(),
                    jump: vec![],
                });
                t
            }
            Temp(t) => t,
            Name(label) => {
                let t = gen.new_temp();
                result.push(Instr::Oper {
                    assem: "lea 'D0, ['J0]".into(),
                    dst: Dst(vec![t]),
                    src: Src::empty(),
                    // TODO is jump used for analysis?
                    // if so we need to print out the label into the string
                    jump: vec![label],
                });
                t
            }
            Mem(e) => {
                let t = Self::munch_exp(*e, result, gen);
                result.push(Instr::Oper {
                    assem: "mov 'D0, 'S0".into(),
                    dst: Dst(vec![t]),
                    src: Src(vec![t]),
                    jump: vec![],
                });
                t
            }
            Eseq(..) => panic!("impl bug: Eseq should have been eliminated"),
        }
    }

    /// The entry point for translating into
    fn code_gen_frame(_: FrameRef, stm: IrStm, instrs: &mut Vec<Instr>, gen: &mut dyn Uuids) {
        Self::munch_stm(stm, instrs, gen);
    }
}
