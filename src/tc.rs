use std::env;

use clap::{arg, command, Arg, ArgAction};
use lrlex::lrlex_mod;
use lrpar::lrpar_mod;
use std::fs;

mod absyn;
mod assem;
mod canon;
mod escape;
mod frame;
mod int_types;
mod ir;
mod semant;
mod symbol;
mod symtab;
mod temp;
mod translate;
mod util;

use crate::{
    assem::{x86_64::X86Asm, Codegen},
    frame::{x86_64::x86_64_Frame, Frame},
    ir::IrStm,
    temp::{Uuids, UuidsImpl},
};

lrlex_mod!("tiger.l");
lrpar_mod!("tiger.y");

fn main() {
    let matches = command!()
        .arg(Arg::new("file"))
        .arg(arg!(--irgen "dump the ir, after constant folding (if enabled), to file named <basename>.ir").required(false))
        .arg(arg!(--irrun "run the ir through interpreter"))
        .arg(arg!(--"report-opts" "output the list of optimizations supported by compiler"))
        .arg(arg!(--optir <phase> "output the ir after <phase>, where <phase> can be initial|final"))
        .arg(arg!(-O<opt> "enable optimization <opt>, can be specified multiple times. other optimizations are disabled unless enabled. <opt> is the list of optimizations output from --report-opts. -O0 means disable all optimizations.").action(ArgAction::Append))
        .get_matches();

    let file = matches.get_one::<String>("file");
    if let None = file {
        eprintln!("Missing required argument <file>");
        util::exit(util::ReturnCode::ExUsage);
    }

    let input = fs::read_to_string(file.unwrap());

    if let Err(ref err) = input {
        eprintln!("Unable to read file {}, got error {}", file.unwrap(), err);
        util::exit(util::ReturnCode::OtherErrors);
    }

    let lexerdef: lrlex::LRNonStreamingLexerDef<lrlex::DefaultLexerTypes> = tiger_l::lexerdef();

    let lexer = lexerdef.lexer(input.as_ref().unwrap());

    let (ast_opt, errs) = tiger_y::parse(&lexer);

    if errs.len() > 0 {
        for e in errs {
            println!("{}", e.pp(&lexer, &tiger_y::token_epp));
        }
        util::exit(util::ReturnCode::SyntaxError);
    }

    let mut gen: UuidsImpl = Uuids::new();

    // TODO later this is false iff we -O0, or we -O<some non-register alloc> and never enable register allocation
    let can_spill = false;

    let frags = semant::translate::<x86_64_Frame>(
        input.as_ref().unwrap(),
        &mut ast_opt.unwrap().unwrap(),
        &mut gen,
        can_spill,
    );

    if frags.is_err() {
        println!("type checking failed");
        util::exit(util::ReturnCode::TypeError);
    }

    // TODO missing tigermain
    // missing all the calls, hmm
    let mut xxx = Vec::new();
    for frag in frags.unwrap().into_iter() {
        match frag {
            frame::Frag::String(label, s) => {
                println!("{}", x86_64_Frame::string(label, s.as_str()));
            }
            frame::Frag::Proc { body, frame } => {
                let mut asm = Vec::new();
                let linearized = canon::linearize(body, &mut gen);
                let (blist, done_label) = canon::basic_blocks(linearized, &mut gen);
                let trace = canon::trace_schedule(blist, done_label, &mut gen);

                for stm in trace.into_iter() {
                    assem::x86_64::X86Asm::code_gen_frame(frame.clone(), stm, &mut asm, &mut gen);
                }
                frame.borrow().proc_entry_exit2(&mut asm, &mut gen);
                let (prologue, epilogue) = frame.borrow().proc_entry_exit3(&asm, &mut gen);
                xxx.push((prologue, epilogue, asm));
            }
        };
    }

    let tm = gen.to_temp_map(x86_64_Frame::registers());
    for (prologue, epilogue, asm) in xxx.iter() {
        println!("{}", prologue);
        for i in asm {
            let s = i.format(&tm, true, &mut gen);
            if s.len() > 0 {
                // because proc_entry_exit2 added the dummy instruction, it will cause an empty line
                println!("\t{}", i.format(&tm, true, &mut gen));
            }
        }
        println!("{}", epilogue);
    }
}
