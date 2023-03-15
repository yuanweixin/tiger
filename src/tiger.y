%start  program

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
    exp { Ok($1) }
  | "UMINUS"  { Ok(Box::new(Exp::NilExp)) } /* this exists solely to satisfy grmtool's draconian insistence that a token is used in the grammar. sometimes toolmakers get ahead of themselves trying to be smart */
  ;

/* === Expressions. === */
exps -> Result<Box<Exp>, ()>: /* Empty */ { Ok(Box::new(Exp::SeqExp(Vec::new()))) }
    | exps_helper { Ok($1) }
    ;
    
exps_helper -> Result<Box<Exp>, ()>: 
    exp  { 
      let expList = vec![$1];
      Ok(Box::new(Exp::SeqExp(expList)))
      }
    | exps_helper "SEMICOLON" exp  {  Ok($1) }
    ;

exp -> Result<Box<Exp>, ()>:
  /* Literals. */
    "NIL" { Ok(Box::new(Exp::NilExp)) }
  | "INT" { Ok(Box::new(Exp::IntExp(42))) } /* TODO */
  | "STRING" { Ok(Box::new(Exp::StringExp("TODO", 42))) }

  /* Array and record creations. */
  | "ID" "LBRACK" exp "RBRACK" "OF" exp { 
      Ok(Box::new(Exp::ArrayExp 
        {
          typ: 42, 
          size: Box::new($3),
          init: Box::new($6),
          pos: 42
        }))
  }
  | "ID" "LBRACE" field_value_list "RBRACE" { 
      Ok(Box::new(Exp::RecordExp 
        {
          fields: $3,
          typ: 42, 
          pos: 42
        }))
    }

  /* Variables, field, elements of an array. */
  | lvalue { Ok(Box::new(Exp::VarExp($1))) }

  /* Function call. */
  | "ID" "LPAREN" args "RPAREN" { Ok(
      Box::new(Exp::CallExp {
        func: $1,
        args: $3, 
        pos: 42
      })
  ) }
  /* Operations. */
  /* we let slide here the edge case of -INT_MIN using toy lang as excuse, not very robust. */
  | "MINUS" exp %prec "UMINUS" { 
      let zeroExp = Exp::IntExp(0);
      Ok(Box::new(Exp::OpExp {
        left: Boxed::new(Exp::IntExp(0)),
        oper: Oper::MinusOp,
        right: $2,
      }))
    }
  | exp "OR" exp { 
      Ok(Box::new(Exp::IfExp {
        test: $1, 
        then: Box::new(Exp::IntExp(1)),
        els: Some($3),
        pos: 42
      }))
   }
  | exp "AND" exp  {     
      Ok(Box::new(Exp::IfExp {
        test: $1, 
        then: $3, 
        els: Some(Box::new(Exp::IntExp(0))),
        pos: 42
      }))
 }
  | exp "EQ" exp  { 
      Ok(Box::new(Exp::OpExp {
          left: $1,
          oper: Oper::EqOp,
          right: $3
      }))
  }
  | exp "NEQ" exp  { 
      Ok(Box::new(Exp::OpExp {
          left: $1,
          oper: Oper::NeqOp,
          right: $3
      }))

  }
  | exp "LT" exp  { 
      Ok(Box::new(Exp::OpExp {
          left: $1,
          oper: Oper::LtOp,
          right: $3
      }))

   }
  | exp "LE" exp  { 
      Ok(Box::new(Exp::OpExp {
          left: $1,
          oper: Oper::LeOp,
          right: $3
      }))
  }
  | exp "GT" exp  { 
      Ok(Box::new(Exp::OpExp {
          left: $1,
          oper: Oper::GtOp,
          right: $3
      }))
  }
  | exp "GE" exp  { 
          Ok(Box::new(Exp::OpExp {
          left: $1,
          oper: Oper::GeOp,
          right: $3
      }))
  }
  | exp "PLUS" exp  { 
          Ok(Box::new(Exp::OpExp {
          left: $1,
          oper: Oper::PlusOp,
          right: $3
      }))
  }
  | exp "MINUS" exp  { 
        Ok(Box::new(Exp::OpExp {
          left: $1,
          oper: Oper::MinusOp,
          right: $3
      }))
  }
  | exp "TIMES" exp  { 
      Ok(Box::new(Exp::OpExp {
          left: $1,
          oper: Oper::TimesOp,
          right: $3
      }))
  }
  | exp "DIVIDE" exp  { 
        Ok(Box::new(Exp::OpExp {
          left: $1,
          oper: Oper::DivideOp,
          right: $3
      }))
  }
  | "LPAREN" exps "RPAREN" { Ok{$1} }

  /* Assignment. */
  | lvalue "ASSIGN" exp { 
      Ok(Boxed::new(Exp::AssignExp {
        var : $1, 
        exp: $3, 
        pos: 42
      })) 
  }
  /* Control structures. */
  | "IF" exp "THEN" exp { Ok(
        Box::new(Exp::IfExp {
          test: $2,
          then: $4, 
          els: None,
          pos: 42
        }
      ))
  }
  | "IF" exp "THEN" exp "ELSE" exp { 
    Ok(
        Box::new(Exp::IfExp {
          test: $2,
          then: $4, 
          els: Some($6),
          pos: 42
        }
      ))
  }
  | "WHILE" exp "DO" exp { 
      Ok(
        Box::new(Exp::WhileExp {
          test: $2,
          body: $4,
          pos: 42
        }
      ))
  }
  | "FOR" "ID" "ASSIGN" exp "TO" exp "DO" exp { 
      Ok(
        Box::new(Exp::ForExp {
          var: 42, // TODO 
          escape: false,
          lo: $4, 
          hi: $6, 
          body: $8, 
          pos: 42
        }
      ))
  }
  | "BREAK" { Ok(Box::new(Exp::BreakExp(42))) }
  | "LET" chunks "IN" exps "END" { Ok(Box::new(Exp::LetExp {
      decs: $2, 
      body: $4, 
      pos: 42
    })) }
  ;

field_value_list -> Result<u64, ()>: /* Empty */ { Ok(42) }
    | "ID" "EQ" exp  { Ok(42) }
    | field_value_list "COMMA" "ID" "EQ" exp  { Ok(42) }
    ;

args -> Result<u64, ()>: /* Empty */ { Ok(42) }
    | args_helper { Ok(42) }
    ;

args_helper -> Result<u64, ()>:
    exp  { Ok(42) }
    | args_helper "COMMA" exp { Ok(42) }
    ;

lvalue -> Result<u64, ()>:
    "ID" { Ok(42) }
  | "ID" "DOT" "ID" { Ok(42) }
  | "ID" "LBRACK" exp "RBRACK" { Ok(42) }
  /* Record field access. */
  | lvalue "DOT" "ID" { Ok(42) }
  /* Array subscript. */
  | lvalue "LBRACK" exp "RBRACK" { Ok(42) }
  ;

/* === Chunks of declarations. === */
chunks -> Result<u64, ()>: /* Empty */ { Ok(42) }
  | chunk_helper { Ok(42) }
  ;

chunk_helper -> Result<u64, ()>:
   tydec { Ok(42) }
  | fundec { Ok(42) }
  | vardec  { Ok(42) }
  | chunk_helper tydec { Ok(42) }
  | chunk_helper fundec  { Ok(42) }
  | chunk_helper vardec { Ok(42) }
  ;


/* Variable declaration. */
vardec -> Result<u64, ()>: 
    "VAR" "ID" "ASSIGN" exp  { Ok(42) }
    | "VAR" "ID" "COLON" "ID" "ASSIGN" exp  { Ok(42) }
    ;

/* Type declaration. */
tydec -> Result<u64, ()>: 
  "TYPE" "ID" "EQ" ty  { Ok(42) }
;

/* Function declaration. */
fundec -> Result<u64, ()>:
    "FUNCTION" "ID" "LPAREN" tyfields "RPAREN" "EQ" exp { Ok(42) }
  |  "FUNCTION" "ID" "LPAREN" tyfields "RPAREN" "COLON" "ID" "EQ" exp { Ok(42) }
  | "PRIMITIVE" "ID" "LPAREN" tyfields "RPAREN"  { Ok(42) }
  | "PRIMITIVE" "ID" "LPAREN" tyfields "RPAREN" "COLON" "ID"  { Ok(42) }
  ;


/* === Types. === */
ty -> Result<u64, ()>:
   /* Type alias. */
     "ID" { Ok(42) }
   /* Record type definition. */
   | "LBRACE" tyfields "RBRACE" { Ok(42) }
   /* Array type definition. */
   | "ARRAY" "OF" "ID" { Ok(42) }
   ;

tyfields -> Result<u64, ()>: /* Empty */ { Ok(42) }
    | tyfields_helper { Ok(42) }
    ;

tyfields_helper -> Result<u64, ()>:
    "ID" "COLON" "ID"  { Ok(42) }
    | tyfields_helper "COMMA" "ID" "COLON" "ID"  { Ok(42) }
    ;

%%

type Symbol = i32; 
type Pos = u32;

pub enum Var {
    SimpleVar(Symbol),
    FieldVar(Box<Var>, Symbol, Pos),
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
    NameTy(Symbol, Pos),
    RecordTy(Vec<Field>),
    ArrayTy(Symbol, Pos)
}

pub enum Dec {
    FunctionDec(Vec<Box<Fundec>>),
    VarDec {
        name: Symbol,
        escape: bool,
        typ: Option<(Symbol, Pos)>,
        init: Box<Exp>,
        pos: Pos
    },
    TypeDec(Vec<Box<TyDec>>)
}

pub struct Fundec {
  name: Symbol,
  params: Vec<Box<Field>>,
  result: Option<(Symbol, Pos)>,
  body: Box<Exp>,
  pos: Pos
}

pub struct Field {
  name: Symbol, 
  escape: bool,
  typ: Symbol,
  pos: Pos
}

pub struct TyDec {
    name: Symbol,
    ty: Ty,
    pos: Pos
}

pub enum Exp {
    VarExp(Box<Var>),
    NilExp,
    IntExp(i32), 
    StringExp(String, Pos),
    CallExp {
        func: Symbol,
        args: Vec<Box<Exp>>, 
        pos: Pos
    },
    OpExp {
        left: Box<Exp>,
        oper: Oper,
        right: Box<Exp>
    },
    RecordExp {
        fields: Vec<(Symbol, Box<Exp>, Pos)>,
        typ: Symbol, 
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
        var: Symbol,
        escape: bool,
        lo: Box<Exp>,
        hi: Box<Exp>,
        body: Box<Exp>,
        pos: Pos
    },
    BreakExp(Pos),
    LetEx {
        decs: Vec<Box<Dec>>,
        body: Box<Exp>,
        pos: Pos
    },
    ArrayExp {
        typ: Symbol,
        size: Box<Exp>,
        init: Box<Exp>,
        pos: Pos
    }
}


