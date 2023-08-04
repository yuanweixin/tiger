use std::{cmp::max, collections::HashMap, error::Error, fmt::Write};

use crate::{
    assem::*,
    frame,
    frame::{x86_64, FrameRef},
    ir::{
        IrBinop::{self, *},
        IrExp::*,
        IrRelop::*,
        IrStm::*,
    },
    temp::{TempMap, Uuids},
};

pub struct X86Asm;

enum Scale {
    One,
    Two,
    Four,
    Eight,
}

impl Scale {
    fn from(value: i32) -> Option<Self> {
        match value {
            1 => Some(Scale::One),
            2 => Some(Scale::Two),
            4 => Some(Scale::Four),
            8 => Some(Scale::Eight),
            _ => None,
        }
    }

    fn as_str(&self) -> &str {
        match self {
            Scale::One => "1",
            Scale::Two => "2",
            Scale::Four => "4",
            Scale::Eight => "8",
        }
    }
}
// Base-index-scale-displacement

enum AddressingMode {
    Bisd {
        base: Option<IrExp>,
        index: Option<IrExp>,
        scale: Scale,
        disp: Option<i32>,
    },
    NotMem(IrExp),
}

use AddressingMode::*;

impl AddressingMode {
    fn consume_as_source(
        self,
        oper: &str,
        srcs: &mut Vec<temp::Temp>,
        fmt: &mut String,
        result: &mut Vec<Instr>,
        gen: &mut dyn Uuids,
    ) -> Result<(), Box<dyn Error>> {
        write!(fmt, "{} ", oper)?;
        self.consume(srcs, fmt, result, gen)?;
        write!(fmt, ", %'D0")?;
        Ok(())
    }

    fn consume_as_dst(
        self,
        oper: &str,
        src: temp::Temp,
        srcs: &mut Vec<temp::Temp>,
        fmt: &mut String,
        result: &mut Vec<Instr>,
        gen: &mut dyn Uuids,
    ) -> Result<(), Box<dyn Error>> {
        srcs.push(src);
        write!(fmt, "{} %'S0, ", oper)?;
        self.consume(srcs, fmt, result, gen)?;
        Ok(())
    }

    fn consume(
        self,
        srcs: &mut Vec<temp::Temp>,
        fmt: &mut String,
        result: &mut Vec<Instr>,
        gen: &mut dyn Uuids,
    ) -> Result<(), Box<dyn Error>> {
        match self {
            Bisd {
                base,
                index,
                scale,
                disp,
            } => {
                let next_src = srcs.len();

                match (base, index, disp) {
                    (Some(b), None, None) => {
                        let t = X86Asm::munch_exp(b, result, gen)?;
                        write!(fmt, "(%'S{})", next_src)?;
                        srcs.push(t);
                    }
                    (Some(b), None, Some(d)) => {
                        let t = X86Asm::munch_exp(b, result, gen)?;
                        write!(fmt, "{}(%'S{})", d, next_src)?;
                        srcs.push(t);
                    }
                    (Some(b), Some(i), None) => {
                        let bt = X86Asm::munch_exp(b, result, gen)?;
                        let it = X86Asm::munch_exp(i, result, gen)?;
                        write!(
                            fmt,
                            "(%'S{}, %'S{}, {})",
                            next_src,
                            next_src + 1,
                            scale.as_str()
                        )?;
                        srcs.push(bt);
                        srcs.push(it);
                    }
                    (Some(b), Some(i), Some(d)) => {
                        let bt = X86Asm::munch_exp(b, result, gen)?;
                        let it = X86Asm::munch_exp(i, result, gen)?;
                        write!(
                            fmt,
                            "{}(%'S{}, %'S{}, {})",
                            d,
                            next_src,
                            next_src + 1,
                            scale.as_str()
                        )?;
                        srcs.push(bt);
                        srcs.push(it);
                    }
                    (None, Some(i), None) => {
                        let it = X86Asm::munch_exp(i, result, gen)?;
                        write!(fmt, "(, %'S{}, {})", next_src, scale.as_str())?;
                        srcs.push(it);
                    }
                    (None, Some(i), Some(d)) => {
                        let it = X86Asm::munch_exp(i, result, gen)?;
                        write!(fmt, "{}(, %'S{}, {})", d, next_src, scale.as_str())?;
                        srcs.push(it);
                    }
                    _ => unreachable!(),
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn match_addressing_mode(e: IrExp) -> AddressingMode {
        match e {
            // ----------------------------------------------t1 + t2 * k + c----------------------------------------------
            // and variations accounting for commutivity and associativity
            Mem(box Binop(
                // [e1 + (e2 * k + c)]
                Plus,
                box e1,
                box Binop(Plus, box Binop(Mul, box e2, box Const(k)), box Const(c)),
            ))
            | Mem(box Binop(
                // [e1 + (k * e2 + c)]
                Plus,
                box e1,
                box Binop(Plus, box Binop(Mul, box Const(k), box e2), box Const(c)),
            ))
            | Mem(box Binop(
                // [e1 + (c + e2 * k)]
                Plus,
                box e1,
                box Binop(Plus, box Const(c), box Binop(Mul, box e2, box Const(k))),
            ))
            | Mem(box Binop(
                // [e1 + (c + k * e2)]
                Plus,
                box e1,
                box Binop(Plus, box Const(c), box Binop(Mul, box Const(k), box e2)),
            ))
            | Mem(box Binop(
                // [e2 * k + (e1 + c)]
                Plus,
                box Binop(Mul, box e2, box Const(k)),
                box Binop(Plus, box e1, box Const(c)),
            ))
            | Mem(box Binop(
                // [k * e2 + (e1 + c)]
                Plus,
                box Binop(Mul, box Const(k), box e2),
                box Binop(Plus, box e1, box Const(c)),
            ))
            | Mem(box Binop(
                // [e2 * k + (c + e1)]
                Plus,
                box Binop(Mul, box e2, box Const(k)),
                box Binop(Plus, box Const(c), box e1),
            ))
            | Mem(box Binop(
                // [k * e2 + (c + e1)]
                Plus,
                box Binop(Mul, box Const(k), box e2),
                box Binop(Plus, box Const(c), box e1),
            ))
            | Mem(box Binop(
                Plus, // [c + (e1 + e2 * k)]
                box Const(c),
                box Binop(Plus, box e1, box Binop(Mul, box e2, box Const(k))),
            ))
            | Mem(box Binop(
                Plus, // [c + (e2 * k + e1)]
                box Const(c),
                box Binop(Plus, box Binop(Mul, box e2, box Const(k)), box e1),
            ))
            | Mem(box Binop(
                Plus, // [c + (e1 + k * e2) ]
                box Const(c),
                box Binop(Plus, box e1, box Binop(Mul, box Const(k), box e2)),
            ))
            | Mem(box Binop(
                Plus, // [c + (k * e2 + e1) ]
                box Const(c),
                box Binop(Plus, box Binop(Mul, box Const(k), box e2), box e1),
            ))
            | // ----------------------------------------------associativity----------------------------------------------
             Mem(box Binop(
                // [e1 + e2 * k + c]
                Plus,
                box Binop(Plus, box e1, box Binop(Mul, box e2, box Const(k))),
                box Const(c)
            ))
            | Mem(box Binop(
                // [e1 + k * e2 + c]
                Plus,
                box Binop(Plus, box e1, box Binop(Mul, box Const(k), box e2)),
                box Const(c)
            ))
            | Mem(box Binop(
                // [e1 + c + e2 * k]
                Plus,
                box Binop(Plus, box e1, box Const(c)),
                box Binop(Mul, box e2, box Const(k))
            ))
            | Mem(box Binop(
                // [e1 + c + k * e2]
                Plus,
                box Binop(Plus, box e1, box Const(c)),
                box Binop(Mul, box Const(k), box e2)
            ))
            | Mem(box Binop(
                // [e2 * k + e1 + c]
                Plus,
                box Binop(Plus, box Binop(Mul, box e2, box Const(k)), box e1),
                box Const(c)
            ))
            | Mem(box Binop(
                // [k * e2 + e1 + c]
                Plus,
                box Binop(Plus, box Binop(Mul, box Const(k), box e2), box e1),
                box Const(c)
            ))
            | Mem(box Binop(
                // [e2 * k + c + e1]
                Plus,
                box Binop(Plus, box Binop(Mul, box e2, box Const(k)), box Const(c)),
                box e1
            ))
            | Mem(box Binop(
                // [k * e2 + c + e1]
                Plus,
                box Binop(Plus, box Binop(Mul, box Const(k), box e2), box Const(c)),
                box e1
            ))
            | Mem(box Binop(
                Plus, // [c + e1 + e2 * k]
                box Binop(Plus, box Const(c), box e1),
                box Binop(Mul, box e2, box Const(k)),
            ))
            | Mem(box Binop(
                Plus, // [c + e2 * k + e1]
                box Binop(Plus, box Const(c),  box Binop(Mul, box e2, box Const(k))),
                box e1
            ))
            | Mem(box Binop(
                Plus, // [c + e1 + k * e2 ]
                box Binop(Plus, box Const(c), box e1),
                box Binop(Mul, box Const(k), box e2)
            ))
            | Mem(box Binop(
                Plus, // [c + k * e2 + e1 ]
                box Binop(Plus, box Const(c), box Binop(Mul, box Const(k), box e2)),
                box e1
            )) if let Some(scale) = Scale::from(k) => Bisd {
                base : Some(e1),
                index: Some(e2),
                scale,
                disp: Some(c),
            },


            // ----------------------------------------------[e * k + c]----------------------------------------------
            Mem(box Binop(Plus, box Binop(Mul, box e, box Const(k)), box Const(c)))
            | Mem(box Binop(Plus, box Binop(Mul, box Const(k), box e), box Const(c)))
            | Mem(box Binop(Plus, box Const(c), box Binop(Mul, box e, box Const(k))))
            | Mem(box Binop(Plus, box Const(c), box Binop(Mul, box Const(k), box e)))
            if let Some(scale) = Scale::from(k)
            =>
                Bisd { base: None, index: Some(e), scale: scale, disp: Some(c) },


            // ----------------------------------------------[e + c]----------------------------------------------
            Mem(box Binop(Plus, box e, box Const(c)))
            | Mem(box Binop(Plus, box Const(c), box e)) =>
                Bisd { base: Some(e), index: None, scale: Scale::One, disp: Some(c) },


            // ----------------------------------------------[[e * k]----------------------------------------------[
            Mem(box Binop(Mul, box e, box Const(k)))
            | Mem(box Binop(Mul, box Const(k), box e))
            if let Some(scale) = Scale::from(k) =>
                Bisd { base: None, index: Some(e), scale: scale, disp: None },


            // ----------------------------------------------[e1 + e2 * k]----------------------------------------------
            Mem(box Binop(Plus, box e1, box Binop(Mul, box e2, box Const(k))))
            | Mem(box Binop(Plus, box e1, box Binop(Mul, box Const(k), box e2)))
            | Mem(box Binop(Plus, box Binop(Mul, box e2, box Const(k)), box e1))
            | Mem(box Binop(Plus, box Binop(Mul, box Const(k), box e2), box e1))
            if let Some(scale) = Scale::from(k) =>
                Bisd { base: Some(e1), index: Some(e2), scale, disp: None },


            // ----------------------------------------------[e]----------------------------------------------
            Mem(box e) => Bisd {
                base: Some(e),
                index: None,
                scale: Scale::One,
                disp: None,
            },
            _ => NotMem(e)
        }
    }
}

impl Codegen for X86Asm {
    fn munch_stm(
        stm: IrStm,
        result: &mut Vec<Instr>,
        gen: &mut dyn Uuids,
    ) -> Result<(), Box<dyn Error>> {
        match stm {
            Move(box Temp(t), box Const(0)) => {
                result.push(Instr::Oper {
                    assem: "xorq %'S0, %'S0".into(),
                    dst: Dst(vec![t]),
                    src: Src(vec![t]),
                    jump: vec![],
                });
            }
            Move(box Temp(t), box Const(c)) => {
                result.push(Instr::Oper {
                    assem: format!("movq ${}, %'D0", c),
                    dst: Dst(vec![t]),
                    src: Src::empty(),
                    jump: vec![],
                });
            }
            Move(
                // move [t+x], [t+x] + 1 -> add [t+x], 1
                box Mem(box Binop(Plus, box Temp(t1), box Const(c1))),
                box Binop(
                    Plus,
                    box Mem(box Binop(Plus, box Temp(t2), box Const(c2))),
                    box Const(1),
                ),
            ) if t1 == t2 && c1 == c2 => {
                result.push(Instr::Oper {
                    assem: format!("addq $1, {}(%'S0)", c1),
                    dst: Dst::empty(),
                    src: Src(vec![t1]),
                    jump: vec![],
                });
            }
            Move(box dst_exp, box src_exp) => {
                let addr_mode_dst = AddressingMode::match_addressing_mode(dst_exp);
                let addr_mode_src = AddressingMode::match_addressing_mode(src_exp);
                match (addr_mode_dst, addr_mode_src) {
                    (NotMem(dst), NotMem(src)) => {
                        let dst = Self::munch_exp(dst, result, gen)?;
                        let src = Self::munch_exp(src, result, gen)?;
                        result.push(Instr::Move {
                            assem: "movq %'S, %'D",
                            dst,
                            src,
                        });
                    }
                    (dst @ Bisd { .. }, NotMem(src)) => {
                        let mut fmt = String::new();
                        let mut srcs = Vec::new();

                        let s0 = Self::munch_exp(src, result, gen)?;

                        dst.consume_as_dst("movq", s0, &mut srcs, &mut fmt, result, gen)?;
                        result.push(Instr::Oper {
                            assem: fmt,
                            dst: Dst::empty(),
                            src: Src(srcs),
                            jump: vec![],
                        });
                    }
                    (NotMem(dst), src @ Bisd { .. }) => {
                        let mut fmt = String::new();
                        let mut srcs = Vec::new();

                        let d0 = Self::munch_exp(dst, result, gen)?;

                        src.consume_as_source("movq", &mut srcs, &mut fmt, result, gen)?;
                        result.push(Instr::Oper {
                            assem: fmt,
                            dst: Dst(vec![d0]),
                            src: Src(srcs),
                            jump: vec![],
                        });
                    }
                    (dst @ Bisd { .. }, src @ Bisd { .. }) => {
                        // need to do it 2 times. once to move the src into a temporary.
                        let mut fmt = String::new();
                        let mut srcs = Vec::new();
                        let src_temp = gen.new_unnamed_temp();

                        src.consume_as_source("movq", &mut srcs, &mut fmt, result, gen)?;

                        result.push(Instr::Oper {
                            assem: fmt,
                            dst: Dst(vec![src_temp]),
                            src: Src(srcs),
                            jump: vec![],
                        });
                        // then move the temporary into the dst.
                        fmt = String::new();
                        srcs = Vec::new();

                        dst.consume_as_dst("movq", src_temp, &mut srcs, &mut fmt, result, gen)?;
                        result.push(Instr::Oper {
                            assem: fmt,
                            dst: Dst::empty(),
                            src: Src(srcs),
                            jump: vec![],
                        });
                    }
                }
            }
            Jump(box e, target_labels) => {
                let t = Self::munch_exp(e, result, gen)?;
                result.push(Instr::Oper {
                    assem: "jmp *%'S0".into(),
                    dst: Dst::empty(),
                    src: Src(vec![t]),
                    jump: target_labels,
                });
            }
            Cjump(r, box a, box b, lt, lf) => {
                let ta = Self::munch_exp(a, result, gen)?;
                let tb = Self::munch_exp(b, result, gen)?;
                // TODO one of these can be a Mem
                result.push(Instr::Oper {
                    assem: "cmpq %'S0, %'D0".into(),
                    dst: Dst(vec![ta]),
                    src: Src(vec![tb, ta]),
                    jump: vec![],
                });

                let assem = match r {
                    Eq => "je .L'J0",
                    Ne => "jne .L'J0",
                    Lt => "jl .L'J0",
                    Gt => "jg .L'J0",
                    Le => "jle .L'J0",
                    Ge => "jge .L'J0",
                    Ult => "jb .L'J0",
                    Ule => "jbe .L'J0",
                    Ugt => "ja .L'J0",
                    Uge => "jae .L'J0",
                };

                // a sanity check just in case.
                assert!(matches!(lt, temp::Label::Unnamed(..)));
                assert!(matches!(lf, temp::Label::Unnamed(..)));

                result.push(Instr::Oper {
                    assem: assem.into(),
                    dst: Dst::empty(),
                    src: Src::empty(),
                    jump: vec![lt, lf],
                });
            }
            Exp(box e) => {
                Self::munch_exp(e, result, gen)?;
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
        Ok(())
    }

    /// Given the IrExp, outputs the abstract register that holds the value.
    fn munch_exp(
        exp: IrExp,
        result: &mut Vec<Instr>,
        gen: &mut dyn Uuids,
    ) -> Result<temp::Temp, Box<dyn Error>> {
        let res = match exp {
            Binop(Plus, box Temp(t1), box Temp(t2)) => {
                let tfresh = gen.new_unnamed_temp();
                result.push(Instr::Oper {
                    assem: "lea (%'S0, %'S1), %'D0".into(),
                    dst: Dst(vec![tfresh]),
                    src: Src(vec![t1, t2]),
                    jump: vec![],
                });
                tfresh
            }
            Binop(Plus, box Temp(t1), box Binop(Mul, box Temp(t2), box Const(k)))
            | Binop(Plus, box Temp(t1), box Binop(Mul, box Const(k), box Temp(t2)))
            | Binop(Plus, box Binop(Mul, box Temp(t2), box Const(k)), box Temp(t1))
            | Binop(Plus, box Binop(Mul, box Const(k), box Temp(t2)), box Temp(t1))
                if [1, 2, 4, 8].contains(&k) =>
            {
                let tfresh = gen.new_unnamed_temp();
                result.push(Instr::Oper {
                    assem: format!("lea ('S0, 'S1, {}), %'D0", k),
                    dst: Dst(vec![tfresh]),
                    src: Src(vec![t1, t2]),
                    jump: vec![],
                });
                tfresh
            }
            Binop(op, box a, box b) => {
                // TODO: if one of these is a mem, could probably handle that.
                let instr = match op {
                    Plus => "addq %'S0, %'D0",  // ok
                    Minus => "subq %'S0, %'D0", // ok
                    IrBinop::Mul => "imulq %'S0, %'D0",
                    IrBinop::Div => "movq $0, %rdx\n\tidivq %'S0", // rdx is zero'ed otherwise it complains about "floating point exception"
                    IrBinop::And => "andq %'S0, %'D0",
                    IrBinop::Or => "orq %'S0, %'D0",
                    // base tiger language doesn't have these
                    // so presumably they must come from optimizations.
                    Lshift => "shlq %'S0, %'D0", // TODO shl r/m64, imm8; shl r/m64, CL; shl r/m64, 1; masked to 63 bits for the REX instructions
                    Rshift => "shrq %'S0, %'D0", // TODO ditto as above;
                    ArShift => "sarq %'S0, %'D0", // note idiv rounds quotient toward 0, sar rounds quotient toward neg infinity
                    IrBinop::Xor => "xorq %'S0, %'D0",
                };
                let is_div = matches!(op, IrBinop::Div);
                // TODO add a test case to cover this of `a` being a temporary.
                let a_temp = match a {
                    // because, if dst is already a temporary, this would have the undesirable
                    // side effect of overwriting the temporary. otoh, if dst is some complex
                    // expression, it would have a fresh temporary generated, in which case
                    // it would be safe to simply overwrite it.
                    IrExp::Temp(t) => {
                        let fresh = gen.new_unnamed_temp();
                        result.push(Instr::Move {
                            assem: "movq %'S, %'D",
                            dst: fresh,
                            src: t,
                        });
                        fresh
                    }
                    _ => Self::munch_exp(a, result, gen)?,
                };

                let b_temp = Self::munch_exp(b, result, gen)?;
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
            IrExp::Call(box f, args) => {
                let num_args = args.len();
                let mut arg_regs = Vec::with_capacity(args.len());

                // this would preserve evaluation order.
                // otherwise, we could just evaluate when we do the push, in reverse order,
                // in order to take advantage of addressing modes.
                for arg_exp in args {
                    arg_regs.push(Self::munch_exp(arg_exp, result, gen)?);
                }

                let mut caller_save_temps = Vec::new();
                for reg_name in frame::x86_64::CALLER_SAVES {
                    // will move to temporaries; it will spill in trivial register allocation, and
                    // we hope that the actual register allocator will coalesce these moves.
                    let t = gen.new_unnamed_temp();
                    caller_save_temps.push(t);
                    result.push(Instr::Move {
                        assem: "mov %'S, %'D",
                        dst: t,
                        src: gen.named_temp(reg_name),
                    });
                }

                if num_args > 0 {
                    let mut i = arg_regs.len() - 1;
                    // args after the 6th one go on stack.
                    while i > 5 {
                        result.push(Instr::Oper {
                            assem: "push %'S0".into(),
                            dst: Dst(vec![gen.named_temp(frame::x86_64::RSP)]),
                            src: Src(vec![arg_regs[i]]),
                            jump: vec![],
                        });
                        i -= 1;
                    }

                    // a note about the use of rax, rdx as return value registers.
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
                            assem: "movq %'S0, %'D0".into(),
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
                match f {
                    IrExp::Name(func_label) => {
                        result.push(Instr::Oper {
                            assem: format!("call {}@PLT", func_label.resolve_named_label(gen)),
                            dst: Dst(vec![x86_64::named_register(gen, x86_64::RAX)]),
                            src: Src::empty(),
                            jump: vec![],
                        });
                    }
                    _ => unreachable!(),
                }

                // persist the result register.
                let dest = gen.new_unnamed_temp();
                result.push(Instr::Oper {
                    assem: "movq %'S0, %'D0".into(),
                    dst: Dst(vec![dest]),
                    src: Src(vec![x86_64::named_register(gen, x86_64::RAX)]),
                    jump: vec![],
                });

                // restore caller save registers.
                for (reg_name, t) in frame::x86_64::CALLER_SAVES.iter().zip(caller_save_temps) {
                    result.push(Instr::Move {
                        assem: "mov %'S, %'D",
                        src: t,
                        dst: gen.named_temp(reg_name),
                    });
                }

                // don't think we'd ever get to a point where someone
                // passes enough arguments to overflow an i32.
                if max(0, num_args as i32 - 6) > 0 {
                    result.push(Instr::Oper {
                        assem: format!("addq ${}, %'D0", x86_64::WORD_SIZE * (num_args - 6)),
                        dst: Dst(vec![x86_64::named_register(gen, x86_64::RSP)]),
                        src: Src(vec![x86_64::named_register(gen, x86_64::RSP)]),
                        jump: vec![],
                    });
                }

                dest
            }
            Const(i) => {
                let t = gen.new_unnamed_temp();
                result.push(Instr::Oper {
                    assem: format!("movq ${}, %'D0", i),
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
                        "lea 'J0@PLT(%rip), %'D0".into()
                    } else {
                        "lea .L'J0(%rip), %'D0".into()
                    },
                    dst: Dst(vec![t]),
                    src: Src::empty(),
                    // TODO is jump used for analysis?
                    // if so we need to print out the label into the string
                    jump: vec![label],
                });
                t
            }
            Mem(box e) => {
                let m = AddressingMode::match_addressing_mode(e);
                let result_temp = gen.new_unnamed_temp();
                match m {
                    Bisd { .. } => {
                        let mut fmt = String::new();
                        let mut srcs = Vec::new();

                        m.consume_as_source("movq", &mut srcs, &mut fmt, result, gen)?;
                        result.push(Instr::Oper {
                            assem: fmt,
                            dst: Dst(vec![result_temp]),
                            src: Src(srcs),
                            jump: vec![],
                        });
                    }
                    NotMem(e) => {
                        let address = Self::munch_exp(e, result, gen)?;
                        result.push(Instr::Move {
                            assem: "movq (%'S), %'D",
                            dst: result_temp,
                            src: address,
                        });
                    }
                }
                result_temp
            }
            Eseq(..) => panic!("impl bug: Eseq should have been eliminated"),
            Null => {
                let t = gen.new_unnamed_temp();
                result.push(Instr::Oper {
                    assem: "movq $0, %'D0".into(),
                    dst: Dst(vec![t]),
                    src: Src::empty(),
                    jump: vec![],
                });
                t
            }
        };
        Ok(res)
    }

    /// The entry point for translating into
    fn code_gen_frame(_: FrameRef, stm: IrStm, instrs: &mut Vec<Instr>, gen: &mut dyn Uuids) {
        Self::munch_stm(stm, instrs, gen).unwrap();
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
    const TRIVIAL_REGISTERS: [&str; 3] = [x86_64::R10, x86_64::R11, x86_64::RAX];

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

        if cfg!(debug_assertions) {
            for name in TRIVIAL_REGISTERS {
                assert!(!frame::x86_64::ARG_REGS.contains(&name), "ARG_REGS and TRIVIAL_REGISTERS overlap, which could result in clobbering of args during trivial register allocation!");
            }
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
                        assem: format!("movq {}(%rbp), %'D0", src_offset),
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
                        assem: format!("movq %'S0, {}(%rbp)", dst_offset),
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
                            assem: format!("movq {}(%rbp), %'D0", src_offset),
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
                            assem: format!("movq %'S0, {}(%rbp)", dst_offset),
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

#[cfg(test)]
mod tests {
    use super::*;
}
