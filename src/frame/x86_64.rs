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
pub const R8: &str = "r8";
pub const R9: &str = "r9";
pub const RSP: &str = "rsp";
pub const RAX: &str = "rax";

#[inline]
pub fn flags_register(gen: &mut dyn Uuids) -> temp::Temp {
    gen.named_temp(FLAGS)
}

#[inline]
pub fn named_register(gen: &mut dyn Uuids, name: &'static str) -> temp::Temp
{
    gen.named_temp(name)
}

#[inline]
pub fn argument_passing_registers(gen: &mut dyn Uuids) -> Vec<temp::Temp> {
    vec![
        gen.named_temp(RDI),
        gen.named_temp(RSI),
        gen.named_temp(RDX),
        gen.named_temp(RCX),
        gen.named_temp(R8),
        gen.named_temp(R9),
    ]
}

impl Frame for x86_64_Frame {
    fn temp_map(gen: &mut dyn Uuids) -> super::TempMap where Self: Sized {
        todo!()
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
        // assuming register allocator can spill,
        // we just need to insert a bunch of Move(t, c) for each of the callee-save
        // registers c, and t is a fresh temporary. this lets the register allocator
        // spill if necessary (as pre-colored temporaries are never spilled.
        // on procedure exit, we will do the reverse and restore (i.e. Move(c, t) where
        // t is the same t not a fresh register).
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
