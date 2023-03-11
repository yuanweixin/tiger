use cfgrammar::yacc::YaccKind;
use cfgrammar::yacc::YaccOriginalActionKind;
use lrlex::CTLexerBuilder;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    CTLexerBuilder::new()
        .lrpar_config(|ctp| {
            // ctp.yacckind(YaccKind::Grmtools)
            ctp.yacckind(YaccKind::Original(YaccOriginalActionKind::NoAction))
                .grammar_in_src_dir("tiger.y")
                .unwrap()
        })
        .lexer_in_src_dir("tiger.l")?
        .build()?;
    Ok(())
}