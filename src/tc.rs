use std::env;

use lrlex::lrlex_mod;
use lrpar::lrpar_mod;
use clap::Parser;
use std::fs;

mod util;
mod absyn;
mod symtab;

#[derive(Parser, Debug)]
#[command(author="Wei Xin Yuan", version="0.0.1", about="Tiger Compiler", long_about = None, override_usage="tc [Option] file")]
struct Args {
   /// The file to compile.
   // Option because license needs to be specified alone.
   file: Option<String>,
}

lrlex_mod!("tiger.l");
lrpar_mod!("tiger.y");

fn main() {
    let args = Args::parse();

    if let None = args.file {
        eprintln!("Missing required argument <file>");
        util::exit(util::ReturnCode::ExUsage);
    }

    let input = fs::read_to_string(args.file.as_ref().unwrap());

    if let Err( ref err) = input {
        eprintln!("Unable to read file {}, got error {}", args.file.as_ref().unwrap(), err);
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





