use std::{
    collections::HashMap, env, error::Error, fs, fs::File, io::BufWriter, io::Write, path::PathBuf,
};

use clap::{arg, command, Arg, ArgAction, ArgMatches};
use lrlex::lrlex_mod;
use lrpar::lrpar_mod;

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
    assem::x86_64::trivial_reg,
    assem::Codegen,
    frame::{x86_64::x86_64_Frame, Frame},
    temp::{Uuids, UuidsImpl},
};

lrlex_mod!("tiger.l");
lrpar_mod!("tiger.y");

const DEBUG_TRIVIAL_REG_ALLOCATION: bool = true;
struct OptOpt {
    opt_name: &'static str,
    desc: &'static str,
}

struct CompilerOptions(ArgMatches);

impl CompilerOptions {
    // todo get the rest of them
    pub fn optimizations_disabled(&self) -> bool {
        return true;
    }
    pub fn constant_folding_enabled(&self) -> bool {
        return false;
    }
    pub fn register_allocation_enabled(&self) -> bool {
        return false;
    }
    pub fn report_opts(&self) -> bool {
        return false;
    }
    pub fn file(&self) -> Option<&String> {
        self.0.get_one::<String>("file")
    }
}

fn opt_opts() -> HashMap<&'static str, OptOpt> {
    let mut res = HashMap::new();
    // TODO this needs to reflect what's actually implemented :shrug:
    res.insert(
        "cf",
        OptOpt {
            opt_name: "cf",
            desc: "constant folding",
        },
    );
    res.insert(
        "reg",
        OptOpt {
            opt_name: "reg",
            desc: "register allocation",
        },
    );
    res.insert(
        "mc",
        OptOpt {
            opt_name: "mc",
            desc: "move coalescing, implies register allocation",
        },
    );
    res.insert(
        "cse",
        OptOpt {
            opt_name: "cse",
            desc: "common subexpression elimination",
        },
    );
    res.insert(
        "cse",
        OptOpt {
            opt_name: "cse",
            desc: "common subexpression elimination",
        },
    );

    res.insert(
        "alg",
        OptOpt {
            opt_name: "alg",
            desc: "algebraic optimizations (identities and reassociation)",
        },
    );
    res.insert(
        "copy",
        OptOpt {
            opt_name: "copy",
            desc: "copy propagation",
        },
    );

    res.insert(
        "dce",
        OptOpt {
            opt_name: "dce",
            desc: "dead code elimination",
        },
    );
    res.insert(
        "inl",
        OptOpt {
            opt_name: "inl",
            desc: "inlining",
        },
    );

    res.insert(
        "sr",
        OptOpt {
            opt_name: "sr",
            desc: "strength reduction",
        },
    );

    res.insert(
        "lu",
        OptOpt {
            opt_name: "lu",
            desc: "loop unrolling",
        },
    );

    res.insert(
        "licm",
        OptOpt {
            opt_name: "licm",
            desc: "loop-invariant code motion",
        },
    );

    res.insert(
        "pre",
        OptOpt {
            opt_name: "pre",
            desc: "partial redundancy elimination",
        },
    );

    res.insert(
        "cp",
        OptOpt {
            opt_name: "cp",
            desc: "constant propagation",
        },
    );

    res.insert(
        "vn",
        OptOpt {
            opt_name: "vn",
            desc: "local value numbering",
        },
    );

    res.insert(
        "sa",
        OptOpt {
            opt_name: "sa",
            desc: "stack-allocate non-escaping records and arrays",
        },
    );

    res
}

fn get_enabled_optimizations() {
    // TODO get here when this be needed.
}

fn main() -> Result<(), Box<dyn Error>> {
    let matches = command!()
        .arg(Arg::new("file"))
        .arg(arg!(--irgen "dump the ir, after constant folding (if enabled), to file named <basename>.ir").required(false))
        .arg(arg!(--"report-opts" "output the list of optimizations supported by compiler"))
        .arg(arg!(--optir <phase> "output the ir after <phase>, where <phase> can be initial|final"))
        .arg(arg!(-O<opt> "enable optimization <opt>, can be specified multiple times. other optimizations are disabled unless enabled. <opt> is the list of optimizations output from --report-opts. -O0 means disable all optimizations.").action(ArgAction::Append))
        .get_matches();
    let opts = CompilerOptions(matches);

    if opts.report_opts() {
        println!("Supported optimizations");
        for (_, optopt) in opt_opts() {
            println!("\t{}: {}", optopt.opt_name, optopt.desc);
        }
        util::exit(util::ReturnCode::Ok);
    }

    let file = opts.file();
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

    let tm = gen.to_temp_map(x86_64_Frame::registers());
    let mut xxx = Vec::new();
    for frag in frags.unwrap().into_iter() {
        match frag {
            frame::Frag::String(label, s) => {
                println!("{}", x86_64_Frame::string(label, s.as_str()));
            }
            frame::Frag::Proc { body, frame } => {
                let mut assems = Vec::new();
                let linearized = canon::linearize(body, &mut gen);
                let (blist, done_label) = canon::basic_blocks(linearized, &mut gen);
                let trace = canon::trace_schedule(blist, done_label, &mut gen);

                // the trivial allocator could have been done as a single pass over the
                // finished list of instr. but it's done here inside each fragment so we
                // still have access to the IrStm that leads to the blocks of asm. this is
                // for sanity and ease of debugging by printing out the strings.
                let mut temp_offset = trivial_reg::TempOffset::new();
                for stm in trace.into_iter() {
                    if DEBUG_TRIVIAL_REG_ALLOCATION {
                        println!("\n{:?}", stm);
                    }
                    // IrStm -> Vec<InStr>
                    let mut trivial_reg_alloc_input = Vec::new();
                    assem::x86_64::X86Asm::code_gen_frame(
                        frame.clone(),
                        stm,
                        &mut trivial_reg_alloc_input,
                        &mut gen,
                    );

                    // This maps each Instr to 1 or more InStr.
                    if !opts.register_allocation_enabled() {
                        for instr in trivial_reg_alloc_input {
                            if DEBUG_TRIVIAL_REG_ALLOCATION {
                                println!("#{}", instr.format(&tm, true, &mut gen));
                            }
                            // this extra temporary is used to aid debugging.
                            let mut generated = Vec::new();
                            trivial_reg::do_trivial_register_allcation(
                                frame.clone(),
                                instr,
                                &mut generated,
                                &mut gen,
                                &tm,
                                &mut temp_offset,
                            );
                            for ins in generated {
                                if DEBUG_TRIVIAL_REG_ALLOCATION {
                                    println!("{}", ins.format(&tm, true, &mut gen));
                                }
                                assems.push(ins);
                            }
                        }
                    } else {
                        assems.extend(trivial_reg_alloc_input);
                    }
                }

                let s = String::from(frame.borrow().name().resolve_named_label(&gen));
                frame.borrow().proc_entry_exit2(&mut assems, &mut gen);
                let (prologue, epilogue) = frame.borrow().proc_entry_exit3(&assems, &mut gen);
                xxx.push((prologue, epilogue, assems, s));
            }
        };
    }

    // TODO [2 days 7/19, 7/20] trivial register allocation
    // TODO [1 days] link it with the runtime - yup full of errors
    // TODO next steps
    // 1. [0.5-1 day] set up test cases where the main program is invoked on valid programs, then said programs get run.
    // 2. [0.25 day] for test purpose, might want to allow tigermain to return int types. this way output can be checked on cmdline.
    // 3. [0.5 day] fix up the use and deps in the generated Instr objects for register allocation use.
    // 4.a [2-3 days] understand
    // 4.b [2-3 days] implement liveness.
    // 5.a. [2-3 days] understand register allocation.
    // 5.b. [2-3 days] impl register allocation
    // 6. [2 days] figure out how to even test 4, 5
    // 7. [1+1 days] plug in register allocation.
    // 8. [0.5 day] fix up the compiler options.
    let output_path = PathBuf::from(opts.file().unwrap()).with_extension("s");

    let outf = File::create(output_path)?;


    let mut bout = BufWriter::new(outf);

    writeln!(bout, "{}", x86_64_Frame::asm_file_prologue())?;
    for (prologue, epilogue, asm, fn_name) in xxx.iter() {
        writeln!(bout, "{}", prologue)?;
        writeln!(bout, ".{}_body:", fn_name)?;
        for i in asm {
            let s = i.format(&tm, true, &mut gen);
            if s.len() > 0 {
                // because proc_entry_exit2 added the dummy instruction, it will cause an empty line
                if s.chars().next().unwrap() != '.' {
                    writeln!(bout, "\t{}", i.format(&tm, true, &mut gen))?;
                } else {
                    // label, don't indent.
                    writeln!(bout, "{}", i.format(&tm, true, &mut gen))?;
                }
            }
        }
        writeln!(bout, "{}", epilogue)?;
    }

    bout.flush()?;

    Result::Ok(())
}
