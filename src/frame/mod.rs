use crate::{
    temp::{self, GenTemporary},
    ir,
    temp::{Temp, Label},
    ir::{IrStm, IrExp}
};

use std::fmt::Debug;

#[derive(Clone, Debug)]
pub enum Access {
    // FP + offset
    InFrame(i32),
    // An abstract register
    InReg(Temp)
}

pub type Escapes = bool;

pub type Register = &'static str;

pub trait Frame : Debug {
    // We could also have put Sized on the trait.
    // Self:Sized seems a bit more flexible as it allows default impl if the
    // associated func is not actually called in an impl.
    // https://stackoverflow.com/questions/44096235/understanding-traits-and-object-safety#:~:text=There%20is%20no%20inheritance%20in%20Rust.%20In%20both,types%20that%20implement%20Sized%20can%20implement%20this%20method.
    fn new(name: Label, formals: Vec<Escapes>) -> Self where Self:Sized;
    fn name(&self) -> Label;
    fn formals(&self) -> &[Access];
    fn alloc_local(&mut self, escapes: Escapes) -> Access;
    fn external_call(name: &str, exps: Vec<IrExp>) -> IrExp where Self:Sized;
    fn word_size() -> usize where Self:Sized;
    fn registers() -> &'static [Register] where Self:Sized;
    // TODO not sure what kind of table this is
    // fn temp_map -> Sym
    fn string(label: temp::Label, val: &str) -> String where Self:Sized; // TODO signature

    fn proc_entry_exit1() where Self:Sized;
    fn proc_entry_exit2() where Self:Sized;
    fn proc_entry_exit3() where Self:Sized;

    // since we don't use global state to track the symbol table, temporaries, labels,
    // will need to pass this in and grab it each time. each frame implementation shall
    // intern a unique string indicating the frame's identity (e.g. __x86_frame_pointer__)
    // which will be used to retrieve the temp object.
    //
    // in retrospect, might have been easier to respect tradition and just do these as global states.
    fn frame_pointer(gen: &mut GenTemporary) -> temp::Temp where Self:Sized;
}

pub enum Frag<T : Frame> {
    Proc {
        body: IrStm,
        frame: T // TODO might use a ref or Rc here, or even a dyn
    },
    String(Label, String)
}
