use crate::{
    frame::{Access, Escapes, Frame, Register},
    ir,
    ir::{helpers::*, IrExp, IrStm},
    symbol::Interner,
    temp,
    temp::{Label, Uuids},
};
use std::num::NonZeroUsize;

#[derive(Debug)]
pub struct x86_64_Frame {
    name: Label,
    formals: Vec<Access>,
    next_local_offset: i32,
}

const RBP: &str = "rbp";

impl Frame for x86_64_Frame {
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

    fn registers<'a>() -> &'a [Register<'a>]
    where
        Self: Sized,
    {
        &[]
    }

    fn string(label: temp::Label, val: &str) -> String
    where
        Self: Sized,
    {
        todo!()
    }

    fn frame_pointer(gen: &mut dyn Uuids) -> temp::Temp
    where
        Self: Sized,
    {
        gen.named_temp(RBP)
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

    fn new(name: Label, formals_escapes: Vec<Escapes>, gen: &mut dyn Uuids) -> Self {
        let mut formals = Vec::with_capacity(formals_escapes.len());
        for (i, escape) in formals_escapes.iter().enumerate() {
            if i > 5 || *escape {
                formals.push(Access::InFrame(42));
            } else {
                formals.push(Access::InReg(gen.new_temp()));
            }
        }
        Self {
            name,
            formals,
            next_local_offset: -8,
        }
    }

    fn name(&self) -> Label {
        self.name
    }

    fn formals(&self) -> &[Access] {
        &self.formals[1..]
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
