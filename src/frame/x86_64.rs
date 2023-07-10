use crate::{
    frame::{Access, Frame, Register, Escapes},
    ir::{IrExp, IrStm},
    temp,
    temp::{GenTemporary, Label},
    symbol::Interner,
};
use std::num::NonZeroUsize;

#[derive(Debug)]
pub struct x86_64_Frame {
    name: Label,
    formals: Vec<Access>,
}

impl Frame for x86_64_Frame {
    fn external_call(name: &str, exps: Vec<crate::ir::IrExp>) -> crate::ir::IrExp
    where
        Self: Sized,
    {
        IrExp::Const(42)
    }

    fn word_size() -> usize
    where
        Self: Sized,
    {
        8
    }

    fn registers<'a>() -> &'a [Register<'a>]
    where
        Self: Sized,
    {
        &[]
    }

    fn string(label: crate::temp::Label, val: &str) -> String
    where
        Self: Sized,
    {
        todo!()
    }

    fn frame_pointer(gen: &mut dyn GenTemporary) -> crate::temp::Temp
    where
        Self: Sized,
    {
        temp::test_helpers::new_temp(1)
    }

    fn proc_entry_exit1(&self, body: IrStm) -> IrStm {
        IrStm::Exp(Box::new(IrExp::Const(42)))
    }

    fn proc_entry_exit2()
    where
        Self: Sized,
    {
        todo!()
    }

    fn proc_entry_exit3()
    where
        Self: Sized,
    {
        todo!()
    }

    fn new(name: Label, formals: Vec<Escapes>) -> Self {
        todo!();
        // let mut gen = GenTemporary::new();
        // let mut pool = Interner::new();

        // let mut frame_formals = Vec::with_capacity(1 + formals.len());
        // // dummy values.
        // // this first one is for the static link.
        // frame_formals.push(Access::InFrame(42));

        // for _ in formals {
        //     frame_formals.push(Access::InFrame(42));
        // }
        // x86_64_Frame {
        //     name: gen.new_label(),
        //     formals: frame_formals,
        // }
    }
    fn name(&self) -> Label {
        self.name
    }

    fn formals(&self) -> &[Access] {
        &self.formals[1..]
    }
    fn alloc_local(&mut self, escapes: Escapes) -> Access {
        Access::InFrame(42)
    }
}
