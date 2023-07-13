use crate::{
    assem::*,
    frame::{x86_64, x86_64::x86_64_Frame},
    int_types::TigerInt,
    ir::{
        IrBinop::{self, *},
        IrExp::*,
        IrRelop::{self, *},
        IrStm::*,
    },
};

struct X86Asm;

type LabelList = Vec<temp::Label>;

enum X86Instr {
    Lea,
    Mov,
    Add,
    Sub,
    Mul,
    Div,
    Inc,
    Dec,
    And,
    Or,
    Xor,
    Not,
    Shl,
    Shr,
    Sar,
    Jmp,
    Jz,
    Je,
    Jnz,
    Jne,
    Jl,
    Jle,
    Jg,
    Jge,
    Jb,
    Jbe,
    Ja,
    Jae,
    Push,
    Pop,
    Test,
    Cmp,
    Call,
    Ret,
}
use X86Instr::{
    Add, And, Call, Cmp, Dec, Div, Inc, Ja, Jae, Jb, Jbe, Je, Jg, Jge, Jl, Jle, Jmp, Jne, Jnz, Jz,
    Lea, Mov, Not, Or, Pop, Push, Ret, Sar, Shl, Shr, Sub, Test, Xor,
};

// TODO outline of dynamic programming
// irstm -> list<Instr>
// lazy, just hash irstm
// memo : table<irstm, total_cost>
// if self in memo:
//      return memo[self]
// for each choice in choices(irstm)
//      total_cost = 0
//      for each child in choice:
//         total_cost += tile(child)
//      total_cost += self_cost
//      if self in memo and total_cost < self_cost || self not in memo
//          memo[self] = (total_cost, choice)
//
// second pass is to traverse tree and use the memoized choices
//
enum Scale {
    One,
    Two,
    Four,
    Eight,
}
enum Displacement {
    Eight(i8),
    ThirtyTwo(i32),
}

enum Operand {
    Immediate8(i8),
    Immediate16(i16),
    Immediate32(i32),
    Immediate64(i64),
    Register(temp::Temp),
    Mem {
        base: temp::Temp,
        index: Option<(temp::Temp, Scale)>,
        offset: Option<Displacement>,
    },
    Label(temp::Label),
}

struct AuxData {
    instr: X86Instr,
    ops: Vec<Operand>,
}

trait Helper {
    fn mov(&mut self, dst: Dst, src: Src, aux: AuxData);
    fn jump(&mut self, src: Src, target_labels: LabelList, aux: AuxData);
    fn cmp(&mut self, dst: Dst, src: Src, aux: AuxData);
    fn jcc(&mut self, r: IrRelop, target_labels: LabelList);
    fn label(&mut self, lab: temp::Label);
    fn add(&mut self, dst: Dst, src: Src, aux: AuxData);
    fn lea(&mut self, dst: Dst, src: Src, aux: AuxData);
    fn push(&mut self, src: Src, aux: AuxData);
    fn call(&mut self, src: Src, aux: AuxData);
}

impl Helper for Vec<Instr<AuxData>> {
    fn call(&mut self, src: Src, aux: AuxData) {
        self.push(Instr::Oper {
            dst: Dst(vec![]),
            src,
            jump: vec![],
            aux,
        });
    }

    fn push(&mut self, src: Src, aux: AuxData) {
        self.push(Instr::Oper {
            dst: Dst(vec![]),
            src,
            jump: vec![],
            aux,
        });
    }

    fn mov(&mut self, dst: Dst, src: Src, aux: AuxData) {
        self.push(Instr::Oper {
            dst,
            src,
            jump: vec![],
            aux,
        });
    }

    fn jump(&mut self, src: Src, target_labels: LabelList, aux: AuxData) {
        self.push(Instr::Oper {
            dst: Dst(vec![]),
            src: src,
            jump: target_labels,
            aux,
        });
    }

    fn jcc(&mut self, r: IrRelop, target_labels: LabelList) {
        let instr = match r {
            Eq => Je,
            Ne => Jne,
            Lt => Jl,
            Gt => Jg,
            Le => Jle,
            Ge => Jge,
            Ult => Jb,
            Ule => Jbe,
            Ugt => Ja,
            Uge => Jae,
        };
        let aux = AuxData { instr, ops: vec![] };
        self.push(Instr::Oper {
            dst: Dst(vec![]),
            src: Src(vec![]),
            jump: target_labels,
            aux,
        });
    }

    fn cmp(&mut self, dst: Dst, src: Src, aux: AuxData) {
        self.push(Instr::Oper {
            dst,
            src,
            jump: vec![],
            aux: aux,
        });
    }

    fn label(&mut self, lab: temp::Label) {
        self.push(Instr::Label { lab });
    }

    fn add(&mut self, dst: Dst, src: Src, aux: AuxData) {
        self.push(Instr::Oper {
            dst,
            src,
            jump: vec![],
            aux,
        });
    }

    fn lea(&mut self, dst: Dst, src: Src, aux: AuxData) {
        self.push(Instr::Oper {
            dst,
            src,
            jump: vec![],
            aux,
        });
    }
}

impl Codegen<AuxData> for X86Asm {
    fn munch_stm(stm: IrStm, result: &mut Vec<Instr<AuxData>>, gen: &mut dyn Uuids) {
        match stm {
            Move(dst_exp, src_exp) => {
                let dst = Self::munch_exp(*dst_exp, result, gen);
                let src = Self::munch_exp(*src_exp, result, gen);
                let aux = AuxData {
                    instr: Mov,
                    ops: vec![Operand::Register(dst), Operand::Register(src)],
                };
                result.mov(Dst(vec![dst]), Src(vec![dst, src]), aux);
            }
            Jump(e, target_labels) => {
                let t = Self::munch_exp(*e, result, gen);
                let aux = AuxData {
                    instr: Jmp,
                    ops: vec![],
                };
                result.jump(Src(vec![t]), target_labels, aux);
            }
            Cjump(r, a, b, lt, lf) => {
                let ta = Self::munch_exp(*a, result, gen);
                let tb = Self::munch_exp(*b, result, gen);
                let cmp_aux = AuxData {
                    instr: Cmp,
                    ops: vec![Operand::Register(ta), Operand::Register(tb)],
                };
                result.cmp(Dst(vec![ta]), Src(vec![tb, ta]), cmp_aux);
                result.jcc(r, vec![lt, lf]);
            }
            Exp(e) => {
                let e_temp = Self::munch_exp(*e, result, gen);
                let new_t = gen.new_temp();
                let aux = AuxData {
                    instr: Mov,
                    ops: vec![Operand::Register(new_t), Operand::Register(e_temp)],
                };
                result.mov(Dst(vec![new_t]), Src(vec![e_temp]), aux);
            }
            Label(l) => result.label(l),
            Seq(..) => {
                panic!("impl bug: Seq should have been eliminated");
            }
        }
    }

    /// Given the IrExp, outputs the abstract register that holds the value.
    fn munch_exp(exp: IrExp, result: &mut Vec<Instr<AuxData>>, gen: &mut dyn Uuids) -> temp::Temp {
        match exp {
            Binop(op, a, b) => {
                let instr = match op {
                    Plus => Add,
                    Minus => Sub,
                    IrBinop::Mul => X86Instr::Mul,
                    IrBinop::Div => Div,
                    IrBinop::And => And,
                    IrBinop::Or => Or,
                    Lshift => Shl,
                    Rshift => Shr,
                    ArShift => Sar,
                    IrBinop::Xor => Xor,
                };

                let a_temp = Self::munch_exp(*a, result, gen);
                let b_temp = Self::munch_exp(*b, result, gen);
                let aux = AuxData {
                    instr: Add,
                    ops: vec![Operand::Register(a_temp), Operand::Register(b_temp)],
                };
                result.add(Dst(vec![a_temp]), Src(vec![b_temp]), aux);
                a_temp
            }
            IrExp::Call(f, args) => {
                let f_temp = Self::munch_exp(*f, result, gen);
                let mut arg_regs = Vec::with_capacity(args.len());
                for arg_exp in args {
                    arg_regs.push(Self::munch_exp(arg_exp, result, gen));
                }

                let mut i = arg_regs.len() - 1;
                // args after the 6th one go on stack.
                while i > 5 {
                    let aux = AuxData {
                        instr: Push,
                        ops: vec![Operand::Register(arg_regs[i])],
                    };
                    Helper::push(result, Src(vec![arg_regs[i]]), aux);
                    i -= 1;
                }
                // first 6 args go on registers, in this order.
                let arg_passing_regs = x86_64::argument_passing_registers(gen);
                loop {
                    let aux = AuxData {
                        instr: Mov,
                        ops: vec![
                            Operand::Register(arg_passing_regs[i]),
                            Operand::Register(arg_regs[i]),
                        ],
                    };
                    result.mov(Dst(vec![arg_passing_regs[i]]), Src(vec![arg_regs[i]]), aux);
                    if let Some(new_i) = i.checked_sub(1) {
                        i = new_i;
                    } else {
                        break;
                    }
                }
                // do the call
                let aux = AuxData {
                    instr: Call,
                    ops: vec![Operand::Register(f_temp)],
                };
                result.call(Src(vec![f_temp]), aux);

                // move result to temporary
                let t = gen.new_temp();
                let aux = AuxData {
                    instr: Mov,
                    ops:
                };

                // adjust rsp

                t
            }
            Const(i) => {
                let t = gen.new_temp();
                let aux = AuxData {
                    instr: Mov,
                    ops: vec![Operand::Immediate32(i)],
                };
                result.mov(Dst(vec![t]), Src(vec![]), aux);
                t
            }
            Temp(t) => t,
            Name(label) => {
                let t = gen.new_temp();
                let aux = AuxData {
                    instr: Lea,
                    ops: vec![Operand::Label(label)],
                };
                result.lea(Dst(vec![t]), Src(vec![]), aux);
                t
            }
            Mem(e) => {
                let t = Self::munch_exp(*e, result, gen);
                let aux = AuxData {
                    instr: Mov,
                    ops: vec![
                        Operand::Register(t),
                        Operand::Mem {
                            base: t,
                            index: None,
                            offset: None,
                        },
                    ],
                };
                result.mov(Dst(vec![t]), Src(vec![t]), aux);
                t
            }
            Eseq(..) => panic!("impl bug: Eseq should have been eliminated"),
        }
    }

    /// The entry point for translating into
    fn code_gen(
        f: Box<dyn Frame>,
        stm: IrStm,
        instrs: &mut Vec<Instr<AuxData>>,
        gen: &mut dyn Uuids,
    ) {
    }
}
