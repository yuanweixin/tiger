use crate::{
    frame::{Access, Escapes, Frame, Register},
    ir,
    ir::{helpers::*, IrExp, IrStm},
    temp,
    temp::{Label, Uuids},
};

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
pub fn named_register(gen: &mut dyn Uuids, name: &'static str) -> temp::Temp
{
    gen.named_temp(name)
}

pub fn argument_passing_registers(gen: &mut dyn Uuids) -> Vec<temp::Temp> {
    ARG_REGS.iter().map(|reg| gen.named_temp(reg)).collect()
}

impl Frame for x86_64_Frame {
    fn temp_map(gen: &mut dyn Uuids) -> temp::TempMap where Self: Sized {
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
