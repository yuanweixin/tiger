pub mod x86;

use crate::{
    frame::Frame,
    ir::{IrExp, IrStm},
    temp::{self, Uuids},
};

pub struct Src(Vec<temp::Temp>);
pub struct Dst(Vec<temp::Temp>);

impl Dst {
    fn empty() -> Self {
        Dst(vec![])
    }
}

impl Src {
    fn empty() -> Self {
        Src(vec![])
    }
}

pub enum Instr<T> {
    Oper {
        // template for the final assembly generation.
        // target specific implementations are expected to have some custom placeholders
        // in the template, for the src, dst registers, relevant jump labels. other placeholders
        // referring to other data can be used in tandem with the aux data.
        assem: &'static str,
        // basically all the registers that gets "trashed" by the assembly.
        // for example, mul on x86 would affect EAX,EDX, so those need to be listed.
        dst: Dst,
        // this needs to list all the "dependency" registers.
        // for instance, a CALL might have its arguments translated into temp's.
        // even though those temp's would not appear in the assembly string, they
        // need to be included in source, as input for relevant backend stages.w
        src: Src,
        jump: Vec<temp::Label>,
        // this is not in the appel book, but basically any auxiliary data structure
        // that can become input for generation of the final asm strings. this basically
        // puts the logic of mapping in-memory representation to string into the
        // ToAssembly trait implementation, instead of making it a sort of format
        aux: Option<T>,
    },
    Label {
        assem: &'static str,
        lab: temp::Label,
    },
    Move {
        assem: &'static str,
        dst: temp::Temp,
        src: temp::Temp,
    },
}

pub trait Codegen<T> {
    /// Given an IrStm, emits the abstract assembly for it.
    fn munch_stm(stm: IrStm, result: &mut Vec<Instr<T>>, gen: &mut dyn Uuids);

    /// Given the IrExp, outputs the abstract register that holds the value.
    fn munch_exp(exp: IrExp, result: &mut Vec<Instr<T>>, gen: &mut dyn Uuids) -> temp::Temp;

    /// The entry point for translating into
    fn code_gen(f: Box<dyn Frame>, stm: IrStm, instrs: &mut Vec<Instr<T>>, gen: &mut dyn Uuids);
}
