use crate::{
    temp::{Temp, Label},
    ir::{IrStm}
};

pub enum Access {
    // FP + offset
    InFrame(i64),
    // An abstract register
    InReg(Temp)
}

pub struct Escapes(bool);

pub trait Frame {
    fn new(name: Label, formals: Vec<Escapes>) -> Self where Self:Sized; // Rust thing, to exclude this static fn from being considered for trait object vtable inclusion, so that `Frame` can be used as trait objects.
    fn name(&self) -> Label;
    fn formals(&self) -> Vec<Access>;
    fn alloc_local(&mut self, escapes: Escapes) -> Access;
}

pub enum Frag<T : Frame> {
    Proc {
        body: IrStm,
        frame: T
    },
    String(Label, String)
}