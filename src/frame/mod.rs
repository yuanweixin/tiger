pub mod x86_64;

use crate::{
    assem::Instr,
    ir::{IrExp, IrStm},
    temp::{self, Uuids},
    temp::{Label, Temp},
};

use std::{cell::RefCell, fmt::Debug, rc::Rc};

pub type FrameRef = Rc<RefCell<dyn Frame>>;

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Access {
    // FP + offset
    InFrame(i32),
    // An abstract register
    InReg(Temp),
}

pub type Escapes = bool;

pub type Register = &'static str;

pub type Prologue = String;
pub type Epilogue = String;

pub trait Frame: Debug {
    // We could also have put Sized on the trait.
    // Self:Sized seems a bit more flexible as it allows default impl if the
    // associated func is not actually called in an impl.
    // https://stackoverflow.com/questions/44096235/understanding-traits-and-object-safety#:~:text=There%20is%20no%20inheritance%20in%20Rust.%20In%20both,types%20that%20implement%20Sized%20can%20implement%20this%20method.
    fn new(name: Label, formals: Vec<Escapes>, gen: &mut dyn Uuids) -> Self
    where
        Self: Sized;
    fn name(&self) -> Label;
    fn formals(&self) -> &[Access];
    fn alloc_local(&mut self, escapes: Escapes, gen: &mut dyn Uuids) -> Access;
    fn external_call(fn_name: Label, exps: Vec<IrExp>) -> IrExp
    where
        Self: Sized;
    fn word_size() -> usize
    where
        Self: Sized;
    fn registers() -> &'static [Register]
    where
        Self: Sized;
    // TODO not sure what kind of table this is
    // fn temp_map -> Sym
    fn string(label: temp::Label, val: &str) -> String
    where
        Self: Sized; // TODO signature

    /// Handle call arguments and callee saved registers.
    /// This is part of the code of a function body.
    /// 1. moving call arguments into the abstract registers or memory locations in the callee.
    /// 2. callee saved registers: if register allocator implements spilling, should make up new
    /// temporaries and move callee saved registers to these. If no spilling, then all callee-save
    /// and return address registers should be written to the frame at start of proc body and
    /// fetched back afterwards.
    fn proc_entry_exit1(&mut self, body: IrStm, can_spill: bool, gen: &mut dyn Uuids) -> IrStm;

    /// this function appends a "sink" instruction at the end of the function body to tell register
    /// allocator that certian registers are live at procedure exit. this typically means all the
    /// special registers (e.g. stack pointer, return address), and callee save registers.
    fn proc_entry_exit2(&self, instrs: &mut Vec<Instr>, gen: &mut dyn Uuids);

    /// Creates the prologue and epilogue assembly language.
    fn proc_entry_exit3(&self, instrs: &Vec<Instr>, gen: &mut dyn Uuids) -> (Prologue, Epilogue);

    // since we don't use global state to track the symbol table, temporaries, labels,
    // will need to pass this in and grab it each time. each frame implementation shall
    // intern a unique string indicating the frame's identity (e.g. __x86_frame_pointer__)
    // which will be used to retrieve the temp object.
    //
    // in retrospect, might have been easier to respect tradition and just do these as global states.
    fn frame_pointer(gen: &mut dyn Uuids) -> temp::Temp
    where
        Self: Sized;

    fn temp_map(gen: &mut dyn Uuids) -> temp::TempMap
    where
        Self: Sized;
}

#[derive(Debug)]
pub enum Frag {
    Proc {
        // the output of proc_entry_exit1
        body: IrStm,
        // contains the machine specific info about local vars and params
        frame: FrameRef,
    },
    // represents static strings
    String(Label, String),
}
