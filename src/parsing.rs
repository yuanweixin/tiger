
// throw the parsing related tests in this file.
// it acts up if we put it in tiger.y or absyn.rs, prob has to do with self-reference.
// at any rate, tiger.y is not a regular rust file, so inappropriate place for tests.
#[cfg(test)]
mod tests {
    use lrlex::lrlex_mod;
    use lrpar::lrpar_mod;
    use std::fs;

    lrlex_mod!("tiger.l");
    lrpar_mod!("tiger.y");

    #[test]
    fn test_good() {
        let paths = fs::read_dir("tests/tiger_programs/parsing/good").unwrap();

        for path in paths {
            let p = path.unwrap().path();
            println!("path is {}", p.display());
            let input = fs::read_to_string(p).unwrap();
            let lexerdef = tiger_l::lexerdef();
            let lexer = lexerdef.lexer(&input);
            let (_res, errs) = tiger_y::parse(&lexer);
            if errs.len() > 0 {
                println!("{:#?}", errs);
            }
            assert_eq!(errs.len(), 0);
        }
    }

    #[test]
    fn test_bad() {
        let paths = fs::read_dir("tests/tiger_programs/parsing/bad").unwrap();

        for path in paths {
            let p = path.unwrap().path();
            println!("path is {}", p.display());
            let input = fs::read_to_string(p).unwrap();
            let lexerdef = tiger_l::lexerdef();
            let lexer = lexerdef.lexer(&input);
            let (_res, errs) = tiger_y::parse(&lexer);
            assert!(errs.len() > 0);
        }
    }
}
