// wtf, is this haskell?
// also wtf, when are they gonna stablize these basic pattern matching features?
#![feature(box_patterns)]
#![feature(if_let_guard)]
#![feature(let_chains)]

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
mod optimize;


use std::{
    collections::HashMap, env, error::Error, fs, fs::File, io::BufWriter, io::Write, path::PathBuf,
};

use clap::{arg, command, Arg, ArgAction, ArgMatches};
use lrlex::lrlex_mod;
use lrpar::lrpar_mod;
use crate::{
    assem::x86_64::trivial_reg,
    assem::Codegen,
    frame::{x86_64::x86_64_Frame, Frame},
    temp::{Uuids, UuidsImpl},
};

lrlex_mod!("tiger.l");
lrpar_mod!("tiger.y");

pub const DEBUG_END_TO_END: bool = true;
struct OptOpt {
    opt_name: &'static str,
    desc: &'static str,
}

trait CompilerOptions {
    fn optimizations_disabled(&self) -> bool;
    fn constant_folding_enabled(&self) -> bool;
    fn register_allocation_enabled(&self) -> bool;
    fn report_opts(&self) -> bool;
    fn dump_ir_raw(&self) -> bool;
    fn dump_ir_linearized(&self) -> bool;
    fn dump_ir_blocklist(&self) -> bool;
    fn dump_ir_trace(&self) -> bool;
    fn dump_codegen(&self) -> bool;
    fn file(&self) -> Option<&String>;
}

impl CompilerOptions for ArgMatches {
    fn dump_ir_linearized(&self) -> bool {
        self.get_one::<String>("dumpir")
            .map(|x| x == "linearized")
            .unwrap_or(false)
    }

    fn dump_ir_blocklist(&self) -> bool {
        self.get_one::<String>("dumpir")
            .map(|x: &String| x == "block")
            .unwrap_or(false)
    }
    fn dump_ir_trace(&self) -> bool {
        self.get_one::<String>("dumpir")
            .map(|x: &String| x == "trace")
            .unwrap_or(false)
    }

    fn dump_ir_raw(&self) -> bool {
        self.get_one::<String>("dumpir")
            .map(|x: &String| x == "raw")
            .unwrap_or(false)
    }

    fn dump_codegen(&self) -> bool {
        self.get_one("dumpcodegen").map(|x| *x).unwrap_or(false)
    }

    // todo get the rest of them
    fn optimizations_disabled(&self) -> bool {
        return true;
    }
    fn constant_folding_enabled(&self) -> bool {
        return false;
    }
    fn register_allocation_enabled(&self) -> bool {
        return false;
    }
    fn report_opts(&self) -> bool {
        return false;
    }
    fn file(&self) -> Option<&String> {
        self.get_one::<String>("file")
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

fn dump_temp_map(gen: &UuidsImpl, tm: &temp::TempMap) {
    println!("named temp map {:?}", tm);
    for nl in gen.named_labels.iter() {
        println!("label: {:?} -> {}", nl, nl.resolve_named_label(gen));
    }
}

fn run_on_file(opts: &dyn CompilerOptions) -> Result<util::ReturnCode, Box<dyn Error>> {
    let fpath = opts.file();
    if let None = fpath {
        eprintln!("Missing required argument <file>");
        return Ok(util::ReturnCode::ExUsage);
    }

    let input = fs::read_to_string(fpath.unwrap());

    if let Err(ref err) = input {
        eprintln!("Unable to read file {}, got error {}", fpath.unwrap(), err);
        return Ok(util::ReturnCode::OtherErrors);
    }

    let lexerdef: lrlex::LRNonStreamingLexerDef<lrlex::DefaultLexerTypes> = tiger_l::lexerdef();

    let lexer = lexerdef.lexer(input.as_ref().unwrap());

    let (ast_opt, errs) = tiger_y::parse(&lexer);

    if errs.len() > 0 {
        for e in errs {
            println!("{}", e.pp(&lexer, &tiger_y::token_epp));
        }
        return Ok(util::ReturnCode::SyntaxError);
    }

    let mut gen: UuidsImpl = Uuids::new();

    let frags = semant::translate::<x86_64_Frame>(
        input.as_ref().unwrap(),
        &mut ast_opt.unwrap().unwrap(),
        &mut gen,
    );

    if frags.is_err() {
        println!("type checking failed");
        // TODO need a custom error type to hold the error return codes? not sure.
        return Ok(util::ReturnCode::TypeError);
    }

    let tm = gen.to_temp_map(x86_64_Frame::registers());
    let mut prologue_epilogue_instrs_fn = Vec::new();
    let output_path = PathBuf::from(opts.file().unwrap()).with_extension("s");
    let outf = File::create(output_path)?;
    let mut bout = BufWriter::new(outf);

    // just dump the temp map once!
    if opts.dump_codegen()
        || opts.dump_ir_blocklist()
        || opts.dump_ir_linearized()
        || opts.dump_ir_raw()
        || opts.dump_ir_trace()
    {
        dump_temp_map(&gen, &tm);
    }

    for frag in frags.unwrap().into_iter() {
        match frag {
            frame::Frag::String(label, s) => {
                if opts.dump_codegen() {
                    println!("{}", x86_64_Frame::string(label, s.as_str()));
                }
                writeln!(bout, "{}", x86_64_Frame::string(label, s.as_str()))?;
            }
            frame::Frag::Proc { body, frame } => {
                if opts.dump_ir_raw() {
                    println!(
                        "\n### function {} IR BEGIN###{}\n### IR END ###",
                        frame.borrow().name().resolve_named_label(&gen),
                        body.debug_to_string(&tm, &gen, true)
                    );
                }
                let mut assems = Vec::new();
                let linearized = canon::linearize(body, &mut gen);
                if opts.dump_ir_linearized() {
                    println!(
                        "\n### function {} linearized BEGIN###{:#?}\n### linearized END ###",
                        frame.borrow().name().resolve_named_label(&gen),
                        linearized
                            .iter()
                            .map(|stm| stm.debug_to_string(&tm, &gen, false))
                            .collect::<Vec<_>>()
                    );
                }
                let (blist, start_label, done_label) = canon::basic_blocks(linearized, &mut gen);
                if opts.dump_ir_blocklist() {
                    let blist_formatted = blist
                        .iter()
                        .map(|(k, v)| {
                            (
                                k.debug_to_string(&gen),
                                v.iter()
                                    .map(|stm| stm.debug_to_string(&tm, &gen, false))
                                    .collect::<Vec<_>>(),
                            )
                        })
                        .collect::<Vec<_>>();
                    println!(
                        "\n### function {} blist BEGIN###{:#?}\n### blist END ###",
                        frame.borrow().name().resolve_named_label(&gen),
                        blist_formatted
                    );
                }

                let trace = canon::trace_schedule(blist, done_label.0, &mut gen);
                if opts.dump_ir_trace() {
                    println!(
                        "\n### function {} trace BEGIN###{:#?}\n### trace END ###",
                        frame.borrow().name().resolve_named_label(&gen),
                        trace
                            .iter()
                            .map(|stm| stm.debug_to_string(&tm, &gen, false))
                            .collect::<Vec<_>>()
                    );
                }
                // the trivial allocator could have been done as a single pass over the
                // finished list of instr. but it's done here inside each fragment so we
                // still have access to the IrStm that leads to the blocks of asm. this is
                // for ease of debugging by printing out the IrStm immediately followed by
                // the asm generated.
                let mut temp_offset = trivial_reg::TempOffset::new();
                for stm in trace.into_iter() {
                    if opts.dump_codegen() {
                        println!("\n!!{:?}", stm.debug_to_string(&tm, &gen, false));
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
                            if opts.dump_codegen() {
                                println!("#!{}", instr.format(&tm, true, &mut gen));
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
                                if opts.dump_codegen() {
                                    println!("{}", ins.format(&tm, true, &mut gen));
                                }
                                assems.push(ins);
                            }
                        }
                    } else {
                        assems.extend(trivial_reg_alloc_input);
                    }
                }

                let function_name = String::from(frame.borrow().name().resolve_named_label(&gen));
                frame.borrow().proc_entry_exit2(&mut assems, &mut gen);
                let (prologue, epilogue) =
                    frame
                        .borrow()
                        .proc_entry_exit3(&mut gen, start_label.0);
                prologue_epilogue_instrs_fn.push((prologue, epilogue, assems, function_name));
            }
        };
    }

    for (prologue, epilogue, asm, function_name) in prologue_epilogue_instrs_fn.iter() {
        writeln!(bout, "{}", prologue)?;
        for i in asm {
            let s = i.format(&tm, true, &mut gen);
            if s.len() > 0 { // because proc_entry_exit2 added the dummy instruction, it will cause an empty string when formatted.
                if s.chars().next().unwrap() != '.' { // not a label.
                    writeln!(bout, "\t{}", i.format(&tm, true, &mut gen))?;
                } else { // don't indent label.
                    writeln!(bout, "{}", i.format(&tm, true, &mut gen))?;
                }
            }
        }
        writeln!(bout, "{}", epilogue)?;
    }

    bout.flush()?;

    Result::Ok(util::ReturnCode::Ok)
}
fn main() -> Result<(), Box<dyn Error>> {
    let matches = command!()
        .arg(Arg::new("file"))
        .arg(arg!(--"report-opts" "output the list of optimizations supported by compiler"))
        .arg(arg!(--dumpir <phase> "output the ir after <phase>, where <phase> can be raw|linearized|block|trace"))
        .arg(arg!(--dumpcodegen "output the chunks of asm generated along with the ir they correspond to, useful for debugging the codegen phase"))
        .arg(arg!(-O<opt> "enable optimization <opt>, can be specified multiple times. other optimizations are disabled unless enabled. <opt> is the list of optimizations output from --report-opts. -O0 means disable all optimizations.").action(ArgAction::Append))
        .get_matches();

    if matches.report_opts() {
        println!("Supported optimizations");
        for (_, optopt) in opt_opts() {
            println!("\t{}: {}", optopt.opt_name, optopt.desc);
        }
        util::exit(util::ReturnCode::Ok);
    }

    let res = run_on_file(&matches);
    if res.is_ok() {
        // even though it says Ok, the return code might not be 0. think of it as
        // "checked exceptions".
        util::exit(res.unwrap());
    } else {
        // this is like "unchecked exceptions", just a catch all for any other error
        // that wasn't caughted explicitly.
        eprintln!("{:?}", res.err());
        util::exit(util::ReturnCode::OtherErrors);
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TestOpts<'a> {
        optimizations_disabled: bool, // overrides other optimization options
        constant_folding_enabled: bool,
        register_allocation_enabled: bool,
        report_opts: bool,
        file: Option<&'a String>,
    }

    impl<'a> Default for TestOpts<'a> {
        fn default() -> Self {
            Self {
                optimizations_disabled: true, // overrides other optimization options
                constant_folding_enabled: false,
                register_allocation_enabled: false,
                report_opts: false,
                file: None,
            }
        }
    }

    impl<'a> CompilerOptions for TestOpts<'a> {
        fn dump_codegen(&self) -> bool {
            false
        }
        fn dump_ir_linearized(&self) -> bool {
            false
        }
        fn dump_ir_blocklist(&self) -> bool {
            false
        }
        fn dump_ir_trace(&self) -> bool {
            false
        }
        fn dump_ir_raw(&self) -> bool {
            false
        }
        fn optimizations_disabled(&self) -> bool {
            self.optimizations_disabled
        }
        fn constant_folding_enabled(&self) -> bool {
            !self.optimizations_disabled() && self.constant_folding_enabled
        }
        fn register_allocation_enabled(&self) -> bool {
            !self.optimizations_disabled() && self.register_allocation_enabled
        }
        fn report_opts(&self) -> bool {
            self.report_opts
        }
        fn file(&self) -> Option<&String> {
            self.file
        }
    }

    #[test]
    fn compile_appel_good_programs() {
        // rough test to ensure the thing doesn't crash.
        // ofc, this is a far cry from checking the stuff works as expected.
        let paths = util::get_tig_files_in("tests/tiger_programs/semant/good/");

        for path in paths {
            let fname = path.to_str().map(|x| String::from(x));
            println!("running on file: {}", fname.as_ref().unwrap());
            let opts = TestOpts {
                file: fname.as_ref(),
                ..TestOpts::default()
            };
            let res = run_on_file(&opts);
            assert_eq!(util::ReturnCode::Ok, res.unwrap());
        }
    }
}
