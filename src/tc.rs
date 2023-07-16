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
    assem::{Codegen, x86_64::X86Asm},
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

    let frags = semant::translate::<x86_64_Frame>(
        input.as_ref().unwrap(),
        &mut ast_opt.unwrap().unwrap(),
        &mut gen,
    );

    if frags.is_err() {
        println!("type checking failed");
        util::exit(util::ReturnCode::TypeError);
    }

    let mut asm = Vec::new();
    for frag in frags.unwrap().into_iter() {
        match frag {
            frame::Frag::String(label, s) => {
                // TODO
                println!("{}", x86_64_Frame::string(label, s.as_str()));
            }
            frame::Frag::Proc { body, frame } => {
                let linearized = canon::linearize(body, &mut gen);
                let (blist, done_label) = canon::basic_blocks(linearized, &mut gen);
                let trace = canon::trace_schedule(blist, done_label, &mut gen);
                for stm in trace.into_iter() {
                    assem::x86_64::X86Asm::code_gen_frame(frame.clone(), stm, &mut asm, &mut gen);
                }
            }
        };
    }

    // todo fix up with the procEntryExit_x crap.
    let tm = temp::TempMap::new();
    for i in asm.iter() {
        println!("{}", i.format(&tm, true, &mut gen));
    }
    println!("asm:\n{:#?}", asm);
}
