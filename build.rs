use cfgrammar::yacc::YaccKind;
use lrlex::CTLexerBuilder;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    CTLexerBuilder::new()
        .rust_edition(lrlex::RustEdition::Rust2021)
        .lrpar_config(|ctp| {
            ctp.yacckind(YaccKind::Grmtools)
                .rust_edition(lrpar::RustEdition::Rust2021)
                .error_on_conflicts(false)
                .grammar_in_src_dir("tiger.y")
                .unwrap()
        })
        .lexer_in_src_dir("tiger.l")?
        .build()?;
    Ok(())
}