use std::env;

use lrlex::lrlex_mod;
use lrpar::lrpar_mod;
use clap::{command, Arg};
use std::fs;

mod int_types;
mod util;
mod absyn;
mod symtab;
mod symbol;
mod temp;
mod ir;
mod translate;
mod frame;
mod semant;
mod escape;
mod canon;

use crate::frame::x86_64::x86_64_Frame;

lrlex_mod!("tiger.l");
lrpar_mod!("tiger.y");

fn main() {

    let matches = command!()
        .arg(Arg::new("file"))
        .get_matches();

    let file = matches.get_one::<String>("file");
    if let None = file {
            eprintln!("Missing required argument <file>");
            util::exit(util::ReturnCode::ExUsage);
    }

    let input = fs::read_to_string(file.unwrap());

    if let Err( ref err) = input {
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

    let ir = semant::translate::<x86_64_Frame>(input.as_ref().unwrap(), &mut ast_opt.unwrap().unwrap());

    if ir.is_err() {
        println!("type checking failed");
        util::exit(util::ReturnCode::TypeError);
    }

    println!("{:#?}", ir.unwrap());
}





