use std::env;

use lrlex::lrlex_mod;
use lrpar::lrpar_mod;
use clap::Parser;
use std::fs;

mod util;

const LICENSE : &str = r#"The MIT License (MIT)

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE."#;

#[derive(Parser, Debug)]
#[command(author="Wei Xin Yuan", version="0.0.1", about="Tiger Compiler", long_about = None, override_usage="tc [Option] file")]
struct Args {
   /// The file to compile. 
   // Option because license needs to be specified alone. 
   file: Option<String>,

   /// Display the license, and exit successfully. 
   #[arg(long, exclusive=true)]
   license: bool, 

   /// List the registered tasks.
   #[arg(long)]
   task_list: bool,

   /// Show the task graph. 
   #[arg(long)]
   task_graph: bool,

   /// Report the order in which the tasks will be run. 
   #[arg(long)]
   task_selection: bool,

   /// Prepend a directory to include path.
   #[arg(short='p', long)]
   library_prepend: bool, 

   /// Append a directory to include path.
   #[arg(short='P', long)]
   library_append: bool, 

   /// Report the include search path.
   #[arg(long)]
   library_display: bool,

   /// Enable RE/Flex scanners traces. 
   #[arg(long)]
   scan_trace: bool,

   /// Enable parsers traces.
   #[arg(long)]
   parse_trace: bool,

   /// Parse the file given as argument (objects forbidden).
   #[arg(long)]
   parse: bool,

   /// Enable object constructs of the language (class and method declarations, object creation, method calls, etc.).
   #[arg(short, long)]
   object: bool,

   /// Same as --object --parse, i.e. parse the file given as argument, allowing objects.
   #[arg(long)]
   object_parse: bool,

   /// Load the definitions of the file prelude before the actual argument. The result is equivalent to parsing:
   /// let
   /// import "prelude"
   /// in
   /// /* The argument file. */
   /// end
   /// To disable any prelude file, use --no-prelude. The default value is builtin, denoting the builtin prelude.
   #[arg(long, conflicts_with="no_prelude", default_value_t=("builtin".to_string()))]
   prelude: String,

   /// Don't include prelude.
   #[arg(short='X', long, conflicts_with="prelude")]
   no_prelude: bool
}

lrlex_mod!("tiger.l");
lrpar_mod!("tiger.y");

fn main() {
    let args = Args::parse();

    if args.license {
        println!("{}", LICENSE);
        util::exit(util::ReturnCode::Ok);
    } 
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

    let (res, errs) = tiger_y::parse(&lexer);

    for e in errs {
        println!("{}", e.pp(&lexer, &tiger_y::token_epp));
    }

    if let Some(Ok(r)) = res {
        // TODO further process 
    }
}





