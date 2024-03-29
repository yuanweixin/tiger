pub mod x86_64;

use crate::{
    frame::FrameRef,
    ir::{IrExp, IrStm},
    temp::{self, Uuids},
};

use std::{error::Error, iter::Peekable, str::FromStr};

#[derive(Debug)]
pub struct Src(pub Vec<temp::Temp>);
#[derive(Debug)]
pub struct Dst(pub Vec<temp::Temp>);

impl Dst {
    pub fn empty() -> Self {
        Dst(vec![])
    }
}

impl Src {
    pub fn empty() -> Self {
        Src(vec![])
    }
}

#[derive(Debug)]
pub enum Instr {
    Oper {
        // template for the final assembly generation.
        // target specific implementations are expected to have some custom placeholders
        // in the template, for the src, dst registers, relevant jump labels. other placeholders
        // referring to other data can be used in tandem with the aux data.
        assem: String,
        // basically all the registers that gets "trashed" by the assembly.
        // for example, mul on x86 would affect EAX,EDX, so those need to be listed.
        dst: Dst,
        // this needs to list all the "dependency" registers.
        // for instance, a CALL might have its arguments translated into temp's.
        // even though those temp's would not appear in the assembly string, they
        // need to be included in source, as input for relevant backend stages.w
        src: Src,
        jump: Vec<temp::Label>,
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

pub trait Codegen {
    /// Given an IrStm, emits the abstract assembly for it.
    fn munch_stm(
        stm: IrStm,
        result: &mut Vec<Instr>,
        gen: &mut dyn Uuids,
    ) -> Result<(), Box<dyn Error>>;

    /// Given the IrExp, outputs the abstract register that holds the value.
    fn munch_exp(
        exp: IrExp,
        result: &mut Vec<Instr>,
        gen: &mut dyn Uuids,
    ) -> Result<temp::Temp, Box<dyn Error>>;

    /// The entry point for translating into.
    /// The frame argument is only used if we attempt to eliminate the frame pointer.
    /// This is described on p206 of Appel.
    /// Roughly, a label is generated to hold the frame size of the function represented by the frame.
    /// The frame would be queried for the value of that label. For example, let sp be stack pointer,
    /// L14_framesize be the location of that constant. Then, to allocate the frame, we do sp+L14_framesize.
    /// The L14_framesize is expected to be generated by the prologue of the frame (frame::proc_entry_exit3).
    fn code_gen_frame(f: FrameRef, stm: IrStm, instrs: &mut Vec<Instr>, gen: &mut dyn Uuids);
}

fn consume_control_char(iter: &mut impl Iterator<Item = (usize, char)>, assem: &str) -> char {
    let kind = iter.next();
    if kind.is_none() {
        panic!("impl bug: missing control character in {}", assem);
    }
    kind.unwrap().1
}

fn consume_index(
    tmp: &mut String,
    iter: &mut Peekable<impl Iterator<Item = (usize, char)>>,
    assem: &str,
) -> usize {
    tmp.clear();
    while let Some((_, d)) = iter.peek() {
        if d.is_numeric() {
            tmp.push(*d);
            iter.next(); // consume
        } else {
            break;
        }
    }

    match usize::from_str(tmp.as_str()) {
        Ok(template_arg_idx) => template_arg_idx,
        Err(..) => {
            panic!("impl bug: invalid asm template string '{}'", assem)
        }
    }
}

impl Instr {
    pub fn get_sources(&self) -> Vec<temp::Temp> {
        match self {
            Instr::Label { .. } => vec![],
            Instr::Move { src, .. } => vec![*src],
            Instr::Oper { src, .. } => src.0.clone(),
        }
    }

    pub fn get_dests(&self) -> Vec<temp::Temp> {
        match self {
            Instr::Label { .. } => vec![],
            Instr::Move { dst, .. } => vec![*dst],
            Instr::Oper { dst, .. } => dst.0.clone(),
        }
    }

    pub fn format(&self, tm: &temp::TempMap, relaxed: bool, gen: &mut dyn Uuids) -> String {
        #[inline]
        fn label_string(l: temp::Label, gen: &mut dyn Uuids) -> String {
            match l {
                temp::Label::Unnamed(id) => {
                    format!("{}", id)
                }
                l @ temp::Label::Named(..) => l.resolve_named_label(gen).into(),
            }
        }

        fn add_temp_string(t: temp::Temp, tm: &temp::TempMap, relaxed: bool, res: &mut String) {
            if let Some(s) = tm.get(&t) {
                res.push_str(s)
            } else {
                if relaxed {
                    res.push_str(ToString::to_string(&t).as_str())
                } else {
                    panic!(
                        "impl bug: unable to find a register assignment for temp {}",
                        ToString::to_string(&t)
                    );
                }
            }
        }

        let mut res = String::new();

        match self {
            Instr::Oper {
                assem,
                dst,
                src,
                jump,
            } => {
                let mut tmp = String::new();
                let mut iter = assem.char_indices().peekable();
                while let Some((_, c)) = iter.peek() {
                    match c {
                        '\'' => {
                            iter.next(); // consume the '
                            let kind_char = consume_control_char(&mut iter, assem);
                            let template_arg_idx = consume_index(&mut tmp, &mut iter, assem);

                            match kind_char {
                                'S' | 's' => {
                                    if template_arg_idx >= src.0.len() {
                                        panic!("impl bug: invalid index {} in asm template string referencing src'{}'", template_arg_idx, assem);
                                    }
                                    let t = src.0[template_arg_idx];

                                    add_temp_string(t, &tm, relaxed, &mut res);
                                }
                                'J' | 'j' => {
                                    if template_arg_idx >= jump.len() {
                                        panic!("impl bug: invalid index {} in asm template string referencing jump'{}'", template_arg_idx, assem);
                                    }
                                    let t = jump[template_arg_idx];
                                    res.push_str(label_string(t, gen).as_str());
                                }
                                'D' | 'd' => {
                                    if template_arg_idx >= dst.0.len() {
                                        panic!("impl bug: invalid index {} in asm template string referencing dst '{}'", template_arg_idx, assem);
                                    }
                                    let t = dst.0[template_arg_idx];
                                    add_temp_string(t, &tm, relaxed, &mut res);
                                }
                                x => panic!(
                                    "impl bug: invalid control char {} in asm template {}",
                                    x, assem
                                ),
                            }
                        }
                        some_char => {
                            res.push(*some_char);
                            iter.next(); // consume
                        }
                    }
                }
            }
            Instr::Label { assem, lab } => {
                let mut iter = assem.char_indices().peekable();
                while let Some((_, c)) = iter.peek() {
                    match c {
                        '\'' => {
                            iter.next(); // consume the '

                            let cc = consume_control_char(&mut iter, assem);
                            if 'L' != cc {
                                panic!("impl bug: invalid control character {}", cc);
                            }

                            res.push_str(label_string(*lab, gen).as_str());
                        }
                        some_char => {
                            res.push(*some_char);
                            iter.next(); // consume
                        }
                    }
                }
            }
            Instr::Move { assem, dst, src } => {
                let mut iter = assem.char_indices().peekable();
                while let Some((_, c)) = iter.peek() {
                    match c {
                        '\'' => {
                            iter.next(); // consume the '
                            let kind = iter.next();
                            if kind.is_none() {
                                panic!("impl bug: invalid asm template string {}", assem);
                            }

                            let (_, kind_char) = kind.unwrap();
                            match kind_char {
                                'S' | 's' => {
                                    add_temp_string(*src, &tm, relaxed, &mut res);
                                }
                                'D' | 'd' => {
                                    add_temp_string(*dst, &tm, relaxed, &mut res);
                                }
                                x => panic!(
                                    "impl bug: invalid control char {} in asm template {}",
                                    x, assem
                                ),
                            }
                        }
                        some_char => {
                            res.push(*some_char);
                            iter.next(); // consume
                        }
                    }
                }
            }
        }
        res
    }
}

#[cfg(test)]
mod tests {
    use crate::temp::UuidsImpl;

    use super::*;

    #[test]
    fn format_move_relaxed() {
        let dst = temp::test_helpers::new_unnamed_temp(1);
        let src = temp::test_helpers::new_unnamed_temp(2);
        let i = Instr::Move {
            assem: "mov 'D, 'S",
            dst,
            src,
        };
        let tm = temp::TempMap::new();
        let relaxed = true;
        let mut gen: UuidsImpl = Uuids::new();
        let actual = i.format(&tm, relaxed, &mut gen);
        let expected = "mov t1, t2";
        assert_eq!(expected, actual);
    }

    #[test]
    #[should_panic]
    fn format_move_strict() {
        let dst = temp::test_helpers::new_unnamed_temp(1);
        let src = temp::test_helpers::new_unnamed_temp(2);
        let i = Instr::Move {
            assem: "mov t'D, t'S",
            dst,
            src,
        };
        let tm = temp::TempMap::new();
        let relaxed = false;
        let mut gen: UuidsImpl = Uuids::new();
        i.format(&tm, relaxed, &mut gen);
    }

    #[test]
    fn format_move_uses_temp_map_entry() {
        let dst = temp::test_helpers::new_unnamed_temp(1);
        let src = temp::test_helpers::new_unnamed_temp(2);
        let i = Instr::Move {
            assem: "mov t'D, t'S",
            dst,
            src,
        };
        let mut tm = temp::TempMap::new();
        tm.insert(dst, "d800");
        tm.insert(src, "s800");
        let relaxed = true;
        let mut gen: UuidsImpl = Uuids::new();
        let actual = i.format(&tm, relaxed, &mut gen);
        let expected = "mov td800, ts800";
        assert_eq!(expected, actual);
    }

    #[test]
    #[should_panic]
    fn format_move_strict_missing_temp_map_mapping() {
        let dst = temp::test_helpers::new_unnamed_temp(1);
        let src = temp::test_helpers::new_unnamed_temp(2);
        let i = Instr::Move {
            assem: "mov t'D, t'S",
            dst,
            src,
        };
        let tm = temp::TempMap::new();
        let relaxed = false;
        let mut gen: UuidsImpl = Uuids::new();
        i.format(&tm, relaxed, &mut gen);
    }

    #[test]
    #[should_panic]
    fn format_move_invalid_control_char() {
        let dst = temp::test_helpers::new_unnamed_temp(1);
        let src = temp::test_helpers::new_unnamed_temp(2);
        let i = Instr::Move {
            assem: "mov t'T, t'S",
            dst,
            src,
        };
        let tm = temp::TempMap::new();
        let relaxed = false;
        let mut gen: UuidsImpl = Uuids::new();
        i.format(&tm, relaxed, &mut gen);
    }

    #[test]
    #[should_panic]
    fn format_move_missing_control_char() {
        let dst = temp::test_helpers::new_unnamed_temp(1);
        let src = temp::test_helpers::new_unnamed_temp(2);
        let i = Instr::Move {
            assem: "mov t', t'S",
            dst,
            src,
        };
        let tm = temp::TempMap::new();
        let relaxed = false;
        let mut gen: UuidsImpl = Uuids::new();
        i.format(&tm, relaxed, &mut gen);
    }

    #[test]
    fn format_unnamed_label() {
        let lab = temp::test_helpers::new_unnamed_label(1);
        let i = Instr::Label { assem: "'L", lab };
        let tm = temp::TempMap::new();
        let relaxed = false;
        let mut gen: UuidsImpl = Uuids::new();
        let actual = i.format(&tm, relaxed, &mut gen);
        let expected = "1";
        assert_eq!(expected, actual);
    }

    #[test]
    #[should_panic]
    fn format_label_missing_control_character() {
        let lab = temp::test_helpers::new_unnamed_label(1);
        let i = Instr::Label { assem: "'", lab };
        let tm = temp::TempMap::new();
        let relaxed = false;
        let mut gen: UuidsImpl = Uuids::new();
        i.format(&tm, relaxed, &mut gen);
    }

    #[test]
    #[should_panic]
    fn format_label_invalid_control_character() {
        let lab = temp::test_helpers::new_unnamed_label(1);
        let i = Instr::Label { assem: "'X", lab };
        let tm = temp::TempMap::new();
        let relaxed = false;
        let mut gen: UuidsImpl = Uuids::new();
        i.format(&tm, relaxed, &mut gen);
    }

    #[test]
    fn format_named_label() {
        let mut gen: UuidsImpl = Uuids::new();
        let sym = gen.intern("hello");
        let lab = temp::Label::Named(sym);
        let i = Instr::Label { assem: "'L", lab };
        let tm = temp::TempMap::new();
        let relaxed = false;
        let actual = i.format(&tm, relaxed, &mut gen);
        let expected = "hello";
        assert_eq!(expected, actual);
    }

    #[test]
    #[should_panic]
    fn format_named_label_missing_symbol() {
        let mut gen: UuidsImpl = Uuids::new();
        let sym = gen.intern("hello");
        gen = Uuids::new(); // reset
        let lab = temp::Label::Named(sym);
        let i = Instr::Label { assem: "'L", lab };
        let tm = temp::TempMap::new();
        let relaxed = false;
        i.format(&tm, relaxed, &mut gen);
    }

    #[test]
    fn format_oper_relaxed() {
        let dst = temp::test_helpers::new_unnamed_temp(1);
        let src = temp::test_helpers::new_unnamed_temp(2);
        let lbl = temp::test_helpers::new_unnamed_label(3);
        let i = Instr::Oper {
            assem: "mov 'D0, 'S0, .L'J0".into(),
            dst: Dst(vec![dst]),
            src: Src(vec![src]),
            jump: vec![lbl],
        };
        let tm = temp::TempMap::new();
        let relaxed = true;
        let mut gen: UuidsImpl = Uuids::new();
        let actual = i.format(&tm, relaxed, &mut gen);
        let expected = "mov t1, t2, .L3";
        assert_eq!(expected, actual);
    }

    #[test]
    #[should_panic]
    fn format_oper_strict() {
        let dst = temp::test_helpers::new_unnamed_temp(1);
        let src = temp::test_helpers::new_unnamed_temp(2);
        let lbl = temp::test_helpers::new_unnamed_label(3);
        let i = Instr::Oper {
            assem: "mov 'D0, 'S0, .L'J0".into(),
            dst: Dst(vec![dst]),
            src: Src(vec![src]),
            jump: vec![lbl],
        };
        let tm = temp::TempMap::new();
        let relaxed = false;
        let mut gen: UuidsImpl = Uuids::new();
        i.format(&tm, relaxed, &mut gen);
    }

    #[test]
    #[should_panic]
    fn format_oper_invalid_src_idx() {
        let dst = temp::test_helpers::new_unnamed_temp(1);
        let src = temp::test_helpers::new_unnamed_temp(2);
        let lbl = temp::test_helpers::new_unnamed_label(3);
        let i = Instr::Oper {
            assem: "mov t'D0, t'S1, .L'J0".into(),
            dst: Dst(vec![dst]),
            src: Src(vec![src]),
            jump: vec![lbl],
        };
        let tm = temp::TempMap::new();
        let relaxed = true;
        let mut gen: UuidsImpl = Uuids::new();
        i.format(&tm, relaxed, &mut gen);
    }

    #[test]
    #[should_panic]
    fn format_oper_invalid_dst_idx() {
        let dst = temp::test_helpers::new_unnamed_temp(1);
        let src = temp::test_helpers::new_unnamed_temp(2);
        let lbl = temp::test_helpers::new_unnamed_label(3);
        let i = Instr::Oper {
            assem: "mov t'D1, t'S0, .L'J0".into(),
            dst: Dst(vec![dst]),
            src: Src(vec![src]),
            jump: vec![lbl],
        };
        let tm = temp::TempMap::new();
        let relaxed = true;
        let mut gen: UuidsImpl = Uuids::new();
        i.format(&tm, relaxed, &mut gen);
    }

    #[test]
    #[should_panic]
    fn format_oper_invalid_jump_idx() {
        let dst = temp::test_helpers::new_unnamed_temp(1);
        let src = temp::test_helpers::new_unnamed_temp(2);
        let lbl = temp::test_helpers::new_unnamed_label(3);
        let i = Instr::Oper {
            assem: "mov t'D0, t'S0, .L'J1".into(),
            dst: Dst(vec![dst]),
            src: Src(vec![src]),
            jump: vec![lbl],
        };
        let tm = temp::TempMap::new();
        let relaxed = true;
        let mut gen: UuidsImpl = Uuids::new();
        i.format(&tm, relaxed, &mut gen);
    }

    #[test]
    #[should_panic]
    fn format_oper_invalid_control_char() {
        let dst = temp::test_helpers::new_unnamed_temp(1);
        let src = temp::test_helpers::new_unnamed_temp(2);
        let lbl = temp::test_helpers::new_unnamed_label(3);
        let i = Instr::Oper {
            assem: "mov t'K0, t'S0, .L'J0".into(),
            dst: Dst(vec![dst]),
            src: Src(vec![src]),
            jump: vec![lbl],
        };
        let tm = temp::TempMap::new();
        let relaxed = true;
        let mut gen: UuidsImpl = Uuids::new();
        i.format(&tm, relaxed, &mut gen);
    }

    #[test]
    #[should_panic]
    fn format_oper_missing_control_char() {
        let dst = temp::test_helpers::new_unnamed_temp(1);
        let src = temp::test_helpers::new_unnamed_temp(2);
        let lbl = temp::test_helpers::new_unnamed_label(3);
        let i = Instr::Oper {
            assem: "mov t'".into(),
            dst: Dst(vec![dst]),
            src: Src(vec![src]),
            jump: vec![lbl],
        };
        let tm = temp::TempMap::new();
        let relaxed = true;
        let mut gen: UuidsImpl = Uuids::new();
        i.format(&tm, relaxed, &mut gen);
    }

    #[test]
    #[should_panic]
    fn format_oper_bad_syntax_missing_idx() {
        let dst = temp::test_helpers::new_unnamed_temp(1);
        let src = temp::test_helpers::new_unnamed_temp(2);
        let lbl = temp::test_helpers::new_unnamed_label(3);
        let i = Instr::Oper {
            assem: "mov t'D".into(),
            dst: Dst(vec![dst]),
            src: Src(vec![src]),
            jump: vec![lbl],
        };
        let tm = temp::TempMap::new();
        let relaxed = true;
        let mut gen: UuidsImpl = Uuids::new();
        i.format(&tm, relaxed, &mut gen);
    }
}
