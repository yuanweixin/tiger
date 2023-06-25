// tiger_lang is treated as a lib crate for use in the `tests` folder.
// need this as a dep of absyn.
pub mod int_types;
// for the `tests` folder, since that treats the crate as a lib. in order to ref its contents
// this `lib.rs` file is needed and the needed modules made public via `pub mod`. tedious.
pub mod absyn;