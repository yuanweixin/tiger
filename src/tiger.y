%start  program

%token "STRING"
%token "UMINUS"

%right "OF"
%nonassoc "DO" "THEN"
%nonassoc "ELSE"
%nonassoc "ASSIGN"
%left "OR"
%left "AND"
%nonassoc "EQ" "NEQ" "LT" "LE" "GT" "GE"
%left "PLUS" "MINUS"
%left "TIMES" "DIVIDE"
%left "UMINUS"

%%
program -> Result<Box<Exp>, ()>:
    exp { $1 }
  | "UMINUS"  { panic!("UMINUS should have been impossible to match") } /* this exists solely to satisfy grmtool's draconian insistence that a token is used in the grammar. sometimes toolmakers get ahead of themselves trying to be smart */
  ;

/* === Expressions. === */
exps -> Result<Box<Exp>, ()>: /* Empty */ { Ok(Box::new(Exp::SeqExp(Vec::new()))) }
    | exps_helper { Ok(Box::new(Exp::SeqExp($1?))) }
    ;
    
exps_helper -> Result<Vec<(Box<Exp>, Pos)>, ()>: 
    exp  { 
      let expList = vec![($1?, 42)];
      Ok(expList)
    }
    | exps_helper "SEMICOLON" exp  { 
        flatten($1, Ok(($3?, 42)))
      }
    ;

exp -> Result<Box<Exp>, ()>:
  /* Literals. */
    "NIL" { Ok(Box::new(Exp::NilExp)) }
  | "INT" { 
    match $lexer.span_str($1.map_err(|_| ())?.span()).parse::<i32>() {
      Ok(val) => Ok(Box::new(Exp::IntExp(val))),
      Err(_) => {
        let ((line, col), _) = $lexer.line_col(span($1)?);
        eprintln!("Unable to parse {} as int at line {} column {}", 
          $lexer.span_str($1.map_err(|_| ())?.span()), line, col);
        Err(())
      }
    }

  }
  | "STRING" { 
    let exp = Exp::StringExp(span($1)?, 42);
    Ok(Box::new(exp)) 
  }

  /* Array and record creations. */
  | "ID" "LBRACK" exp "RBRACK" "OF" exp { 
      Ok(Box::new(Exp::ArrayExp 
        {
          typ: span($1)?,
          size: $3?,
          init: $6?,
          pos: 42
        }))
  }
  | "ID" "LBRACE" field_value_list "RBRACE" { 
      Ok(Box::new(Exp::RecordExp 
        {
          fields: $3?,
          typ: span($1)?, 
          pos: 42
        }))
    }

  /* Variables, field, elements of an array. */
  | lvalue { Ok(Box::new(Exp::VarExp($1?))) }

  /* Function call. */
  | "ID" "LPAREN" args "RPAREN" { Ok(
      Box::new(Exp::CallExp {
        func: span($1)?,
        args: $3?, 
        pos: 42
      })
  ) }
  /* Operations. */
  /* we let slide here the edge case of -INT_MIN using toy lang as excuse, not very robust. */
  | "MINUS" exp %prec "UMINUS" { 
      let zeroExp = Exp::IntExp(0);
      Ok(Box::new(Exp::OpExp {
        left: Box::new(Exp::IntExp(0)),
        oper: Oper::MinusOp,
        right: $2?,
      }))
    }
  | exp "OR" exp { 
      Ok(Box::new(Exp::IfExp {
        test: $1?, 
        then: Box::new(Exp::IntExp(1)),
        els: Some($3?),
        pos: 42
      }))
   }
  | exp "AND" exp  {     
      Ok(Box::new(Exp::IfExp {
        test: $1?, 
        then: $3?, 
        els: Some(Box::new(Exp::IntExp(0))),
        pos: 42
      }))
 }
  | exp "EQ" exp  { 
      Ok(Box::new(Exp::OpExp {
          left: $1?,
          oper: Oper::EqOp,
          right: $3?
      }))
  }
  | exp "NEQ" exp  { 
      Ok(Box::new(Exp::OpExp {
          left: $1?,
          oper: Oper::NeqOp,
          right: $3?
      }))

  }
  | exp "LT" exp  { 
      Ok(Box::new(Exp::OpExp {
          left: $1?,
          oper: Oper::LtOp,
          right: $3?
      }))

   }
  | exp "LE" exp  { 
      Ok(Box::new(Exp::OpExp {
          left: $1?,
          oper: Oper::LeOp,
          right: $3?
      }))
  }
  | exp "GT" exp  { 
      Ok(Box::new(Exp::OpExp {
          left: $1?,
          oper: Oper::GtOp,
          right: $3?
      }))
  }
  | exp "GE" exp  { 
          Ok(Box::new(Exp::OpExp {
          left: $1?,
          oper: Oper::GeOp,
          right: $3?
      }))
  }
  | exp "PLUS" exp  { 
          Ok(Box::new(Exp::OpExp {
          left: $1?,
          oper: Oper::PlusOp,
          right: $3?
      }))
  }
  | exp "MINUS" exp  { 
        Ok(Box::new(Exp::OpExp {
          left: $1?,
          oper: Oper::MinusOp,
          right: $3?
      }))
  }
  | exp "TIMES" exp  { 
      Ok(Box::new(Exp::OpExp {
          left: $1?,
          oper: Oper::TimesOp,
          right: $3?
      }))
  }
  | exp "DIVIDE" exp  { 
        Ok(Box::new(Exp::OpExp {
          left: $1?,
          oper: Oper::DivideOp,
          right: $3?
      }))
  }
  | "LPAREN" exps "RPAREN" { Ok($2?) }

  /* Assignment. */
  | lvalue "ASSIGN" exp { 
      Ok(Box::new(Exp::AssignExp {
        var : $1?, 
        exp: $3?, 
        pos: 42
      })) 
  }
  /* Control structures. */
  | "IF" exp "THEN" exp { Ok(
        Box::new(Exp::IfExp {
          test: $2?,
          then: $4?, 
          els: None,
          pos: 42
        }
      ))
  }
  | "IF" exp "THEN" exp "ELSE" exp { 
    Ok(
        Box::new(Exp::IfExp {
          test: $2?,
          then: $4?, 
          els: Some($6?),
          pos: 42
        }
      ))
  }
  | "WHILE" exp "DO" exp { 
      Ok(
        Box::new(Exp::WhileExp {
          test: $2?,
          body: $4?,
          pos: 42
        }
      ))
  }
  | "FOR" "ID" "ASSIGN" exp "TO" exp "DO" exp { 
      Ok(
        Box::new(Exp::ForExp {
          var: span($1)?, 
          escape: false,
          lo: $4?, 
          hi: $6?, 
          body: $8?, 
          pos: 42
        }
      ))
  }
  | "BREAK" { Ok(Box::new(Exp::BreakExp(42))) }
  | "LET" decs "IN" exps "END" { Ok(Box::new(Exp::LetExp {
      decs: $2?, 
      body: $4?, 
      pos: 42
    })) }
  ;

field_value_list -> Result<Vec<(Span, Box<Exp>, Pos)>, ()>: /* Empty */ { Ok(Vec::new()) }
    | "ID" "EQ" exp  { 
      Ok(vec![(span($1)?, $3?, 42)])
    }
    | field_value_list "COMMA" "ID" "EQ" exp  { 
        flatten($1, Ok((span($3)?, $5?, 42)))
      }
    ;

args -> Result<Vec<Box<Exp>>, ()>: /* Empty */ { Ok(Vec::new()) }
    | args_helper { Ok($1?) }
    ;

args_helper -> Result<Vec<Box<Exp>>, ()>:
    exp  { Ok(vec![$1?]) }
    | args_helper "COMMA" exp { 
        flatten($1, $3)
    }
    ;

lvalue -> Result<Box<Var>, ()>:
    "ID" { Ok(Box::new(Var::SimpleVar(span($1)?, 42))) }
  | "ID" "DOT" "ID" { 
      let sv = Box::new(Var::SimpleVar(span($1)?, 42));
      Ok(Box::new(Var::FieldVar(sv, span($3)?, 42)))
    }
  | "ID" "LBRACK" exp "RBRACK" { 
      let sv = Box::new(Var::SimpleVar(span($1)?, 42));
      Ok(Box::new(Var::SubscriptVar(sv, $3?, 42)))
    }
  /* Record field access. */
  | lvalue "DOT" "ID" { Ok(Box::new(Var::FieldVar($1?, span($3)?, 42))) }
  /* Array subscript. */
  | lvalue "LBRACK" exp "RBRACK" { Ok(Box::new(Var::SubscriptVar($1?, $3?, 42))) }
  ;


/* === declarations. === */
decs -> Result<Vec<Box<Dec>>, ()>: /* Empty */ { Ok(Vec::new()) }
  | dec_helper { Ok($1?) }
  ;

dec_helper -> Result<Vec<Box<Dec>>, ()>:
   tydec { Ok(vec![$1?]) }
  | fundec { Ok(vec![$1?]) }
  | vardec  { Ok(vec![$1?]) }
  | dec_helper tydec { flatten($1, $2) }
  | dec_helper fundec  { flatten($1, $2) }
  | dec_helper vardec { flatten($1, $2) }
  ;


/* Variable declaration. */
vardec -> Result<Box<Dec>, ()>: 
    "VAR" "ID" "ASSIGN" exp  { Ok(Box::new(Dec::VarDec {
      name: span($2)?,
      escape: false,
      typ : None,
      init: $4?,
      pos: 42
    })) }
    | "VAR" "ID" "COLON" "ID" "ASSIGN" exp  { 
      Ok(Box::new(Dec::VarDec {
        name: span($2)?,
        escape: false,
        typ: Some ((span($4)?, 42)),
        init: $6?,
        pos: 42
      })) 
    }
    ;

/* Type declaration. TODO repeated */
tydec -> Result<Box<Dec>, ()>: 
  "TYPE" "ID" "EQ" ty  { 
      Ok(Box::new(
        Dec::TypeDec(
          vec![
            TyDec {
            name: span($2)?,
            ty: $4?,
            pos: 42
          }])))
  }
  ;

/* Function declaration. TODO repeated */
fundec -> Result<Box<Dec>, ()>:
    "FUNCTION" "ID" "LPAREN" tyfields "RPAREN" "EQ" exp { 
      Ok(
        Box::new(
          Dec::FunctionDec(
            vec![
              Fundec {
                name: span($2)?, 
                params: $4?, 
                result: None,
                body: $7?,
                pos: 42
              }]
            ))) 
    }
  |  "FUNCTION" "ID" "LPAREN" tyfields "RPAREN" "COLON" "ID" "EQ" exp { 
    Ok(
      Box::new(
        Dec::FunctionDec(
          vec![ 
            Fundec {
            name: span($2)?, 
            params: $4?, 
            result: Some ((span($7)?, 42)), 
            body: $9?,
            pos: 42
          }])))
    }
  ;


/* === Types. === */
ty -> Result<Box<Ty>, ()>:
   /* Type alias. */
     "ID" { Ok(Box::new(Ty::NameTy(span($1)?,42))) }
   /* Record type definition. */
   | "LBRACE" tyfields "RBRACE" { Ok(Box::new(Ty::RecordTy($2?))) }
   /* Array type definition. */
   | "ARRAY" "OF" "ID" { Ok(Box::new(Ty::ArrayTy(span($3)?,42))) }
   ;

tyfields -> Result<Vec<Field>, ()>: 
    /* Empty */ { 
      Ok(Vec::new()) 
    }
    | tyfields_helper { $1 }
    ;

tyfields_helper -> Result<Vec<Field>, ()>:
    "ID" "COLON" "ID"  { 
      Ok(vec![Field {
        name: span($1)?,
        escape: false, 
        typ: span($3)?, 
        pos: 42
      }])
    }
    | tyfields_helper "COMMA" "ID" "COLON" "ID"  { 
        flatten($1, Ok(Field {
                        name: span($3)?, 
                        escape: false, 
                        typ: span($5)?,
                        pos: 42
        }))
    }
    ;

%%

use cfgrammar::Span;
use lrlex::DefaultLexeme;

type Pos = u32;

pub enum Var {
    SimpleVar(Span, Pos),
    FieldVar(Box<Var>, Span, Pos),
    SubscriptVar(Box<Var>, Box<Exp>, Pos),
}

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
    GeOp
}

pub enum Ty {
    NameTy(Span, Pos),
    RecordTy(Vec<Field>),
    ArrayTy(Span, Pos)
}

pub enum Dec {
    FunctionDec(Vec<Fundec>),
    VarDec {
        name: Span,
        escape: bool,
        typ: Option<(Span, Pos)>,
        init: Box<Exp>,
        pos: Pos
    },
    TypeDec(Vec<TyDec>)
}

pub struct Fundec {
  name: Span,
  params: Vec<Field>,
  result: Option<(Span, Pos)>,
  body: Box<Exp>,
  pos: Pos
}

pub struct Field {
  name: Span, 
  escape: bool,
  typ: Span,
  pos: Pos
}

pub struct TyDec {
    name: Span,
    ty: Box<Ty>,
    pos: Pos
}

pub enum Exp {
    VarExp(Box<Var>),
    NilExp,
    IntExp(i32),
    StringExp(Span, Pos),
    CallExp {
        func: Span,
        args: Vec<Box<Exp>>, 
        pos: Pos
    },
    OpExp {
        left: Box<Exp>,
        oper: Oper,
        right: Box<Exp>
    },
    RecordExp {
        fields: Vec<(Span, Box<Exp>, Pos)>,
        typ: Span, 
        pos: Pos
    },
    SeqExp(Vec<(Box<Exp>, Pos)>),
    AssignExp {
        var: Box<Var>,
        exp: Box<Exp>,
        pos: Pos
    },
    IfExp {
        test: Box<Exp>,
        then: Box<Exp>,
        els: Option<Box<Exp>>,
        pos: Pos
    },
    WhileExp {
        test: Box<Exp>,
        body: Box<Exp>,
        pos: Pos
    },
    ForExp {
        var: Span,
        escape: bool,
        lo: Box<Exp>,
        hi: Box<Exp>,
        body: Box<Exp>,
        pos: Pos
    },
    BreakExp(Pos),
    LetExp {
        decs: Vec<Box<Dec>>,
        body: Box<Exp>,
        pos: Pos
    },
    ArrayExp {
        typ: Span,
        size: Box<Exp>,
        init: Box<Exp>,
        pos: Pos
    }
}

fn flatten<T>(lhs: Result<Vec<T>, ()>, rhs: Result<T,()>) -> Result<Vec<T>, ()> 
{
  let mut flt = lhs?;
  flt.push(rhs?);
  Ok(flt)
}

fn span(dl: Result<DefaultLexeme, DefaultLexeme>) -> Result<Span, ()> {
  Ok(dl.map_err(|_| ())?.span())
}
