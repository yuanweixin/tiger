pub mod x86_64;

use crate::{
    temp::{self, Uuids},
    temp::{Temp, Label},
    ir::{IrStm, IrExp}
};

use std::{
    fmt::Debug,
    rc::Rc,
    cell::RefCell
};

#[derive(Clone, Debug)]
pub enum Access {
    // FP + offset
    InFrame(i32),
    // An abstract register
    InReg(Temp)
}

pub type Escapes = bool;

pub type Register<'a> = &'a str;

pub trait Frame : Debug {
    // We could also have put Sized on the trait.
    // Self:Sized seems a bit more flexible as it allows default impl if the
    // associated func is not actually called in an impl.
    // https://stackoverflow.com/questions/44096235/understanding-traits-and-object-safety#:~:text=There%20is%20no%20inheritance%20in%20Rust.%20In%20both,types%20that%20implement%20Sized%20can%20implement%20this%20method.
    fn new(name: Label, formals: Vec<Escapes>, gen: &mut dyn Uuids) -> Self where Self:Sized;
    fn name(&self) -> Label;
    fn formals(&self) -> &[Access];
    fn alloc_local(&mut self, escapes: Escapes) -> Access;
    fn external_call(fn_name: Label, exps: Vec<IrExp>) -> IrExp where Self:Sized;
    fn word_size() -> usize where Self:Sized;
    fn registers<'a>() -> &'a [Register<'a>] where Self:Sized;
    // TODO not sure what kind of table this is
    // fn temp_map -> Sym
    fn string(label: temp::Label, val: &str) -> String where Self:Sized; // TODO signature

    fn proc_entry_exit1(&self, body: IrStm) -> IrStm;
    fn proc_entry_exit2() where Self:Sized;
    fn proc_entry_exit3() where Self:Sized;

    // since we don't use global state to track the symbol table, temporaries, labels,
    // will need to pass this in and grab it each time. each frame implementation shall
    // intern a unique string indicating the frame's identity (e.g. __x86_frame_pointer__)
    // which will be used to retrieve the temp object.
    //
    // in retrospect, might have been easier to respect tradition and just do these as global states.
    fn frame_pointer(gen: &mut dyn Uuids) -> temp::Temp where Self:Sized;
}

pub enum Frag {
    Proc {
        // the output of proc_entry_exit1
        body: IrStm,
        // contains the machine specific info about local vars and params
        frame: Rc<RefCell<dyn Frame>>
    },
    // represents static strings
    String(Label, String)
}
