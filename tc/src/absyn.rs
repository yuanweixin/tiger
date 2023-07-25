pub mod types {
    use crate::int_types::TigerInt;
    pub use cfgrammar::Span;
    use strum_macros::Display;

    /// ((start line, start column), (end line, end column))
    pub type Pos = ((usize, usize), (usize, usize));

    #[derive(Display, Debug)]
    pub enum Var {
        SimpleVar(Span, Pos),
        FieldVar(Box<Var>, Span, Pos),
        SubscriptVar(Box<Var>, Box<Exp>, Pos),
    }

    #[derive(Display, Debug)]
    pub enum Oper {
        PlusOp,
        MinusOp,
        TimesOp,
        DivideOp,
        EqOp,
        NeqOp,
        LtOp,
        LeOp,
        GtOp,
        GeOp,
    }

    #[derive(Display, Debug)]
    pub enum Ty {
        NameTy(Span, Pos),
        RecordTy(Vec<Field>),
        ArrayTy(Span, Pos),
    }

    #[derive(Display, Debug)]
    pub enum Dec {
        FunctionDec(Vec<Fundec>),
        VarDec {
            name: Span,
            escape: bool,
            typ: Option<(Span, Pos)>,
            init: Box<Exp>,
            pos: Pos,
        },
        TypeDec(Vec<TyDec>),
    }

    #[derive(Debug)]
    pub struct Fundec {
        pub name: Span,
        pub params: Vec<Field>,
        pub result: Option<(Span, Pos)>,
        pub body: Box<Exp>,
        pub pos: Pos,
    }

    #[derive(Debug)]
    pub struct Field {
        pub name: Span,
        pub escape: bool,
        pub typ: Span,
        pub pos: Pos,
    }

    #[derive(Debug)]
    pub struct TyDec {
        pub name: Span,
        pub ty: Box<Ty>,
        pub pos: Pos,
    }

    #[derive(Display, Debug)]
    pub enum Exp {
        VarExp(Box<Var>),
        NilExp,
        IntExp(TigerInt),
        StringExp(Span, Pos),
        CallExp {
            func: Span,
            args: Vec<Exp>,
            pos: Pos,
        },
        OpExp {
            left: Box<Exp>,
            oper: Oper,
            right: Box<Exp>,
            pos: Pos,
        },
        RecordExp {
            fields: Vec<(Span, Exp, Pos)>,
            typ: Span,
            pos: Pos,
        },
        SeqExp(Vec<Exp>),
        AssignExp {
            var: Box<Var>,
            exp: Box<Exp>,
            pos: Pos,
        },
        IfExp {
            test: Box<Exp>,
            then: Box<Exp>,
            els: Option<Box<Exp>>,
            pos: Pos,
        },
        WhileExp {
            test: Box<Exp>,
            body: Box<Exp>,
            pos: Pos,
        },
        ForExp {
            var: Span,
            escape: bool,
            lo: Box<Exp>,
            hi: Box<Exp>,
            body: Box<Exp>,
            pos: Pos,
        },
        BreakExp(Pos),
        LetExp {
            decs: Vec<Dec>,
            body: Box<Exp>,
            pos: Pos,
        },
        ArrayExp {
            typ: Span,
            size: Box<Exp>,
            init: Box<Exp>,
            pos: Pos,
        },
    }
}

// throw the parsing related tests in this file.
// it acts up if we put it in tiger.y or absyn.rs, prob has to do with self-reference.
// at any rate, tiger.y is not a regular rust file, so inappropriate place for tests.
#[cfg(test)]
mod tests {
    use crate::util;
    use lrlex::lrlex_mod;
    use lrpar::lrpar_mod;
    use std::fs;

    lrlex_mod!("tiger.l");
    lrpar_mod!("tiger.y");

    #[test]
    fn test_good() {
        let paths = util::get_tig_files_in("tests/tiger_programs/parsing/good");

        assert!(paths.len() > 0);
        for p in paths {
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
        let paths = util::get_tig_files_in("tests/tiger_programs/parsing/bad");
        assert!(paths.len() > 0);
        for p in paths {
            println!("path is {}", p.display());
            let input = fs::read_to_string(p).unwrap();
            let lexerdef = tiger_l::lexerdef();
            let lexer = lexerdef.lexer(&input);
            let (_res, errs) = tiger_y::parse(&lexer);
            assert!(errs.len() > 0);
        }
    }
}
