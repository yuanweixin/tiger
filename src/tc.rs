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

    let lexerdef = tiger_l::lexerdef();

    let lexer = lexerdef.lexer(input.as_ref().unwrap());

    let (_, errs) = tiger_y::parse(&lexer);

    if errs.len() > 0 {
        for e in errs {
            println!("{}", e.pp(&lexer, &tiger_y::token_epp));
        }
        util::exit(util::ReturnCode::SyntaxError);
    }
}





