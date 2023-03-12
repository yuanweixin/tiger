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
program -> Result<u64, ()>:
    exp { Ok(42) }
  | chunks { Ok(42) }
  | "UMINUS"  { Ok(42) }
  ;


/* === Expressions. === */
exps -> Result<u64, ()>: /* Empty */ { Ok(42) }
    | exps_helper { Ok(42) }
    ;
    
exps_helper -> Result<u64, ()>: 
    exp  { Ok(42) }
    | exps_helper "SEMICOLON" exp  { Ok(42) }
    ;

exp -> Result<u64, ()>:
  /* Literals. */
    "NIL" { Ok(42) }
  | "INT" { Ok(42) }
  | "STRING" { Ok(42) }

  /* Array and record creations. */
  | "ID" "LBRACK" exp "RBRACK" "OF" exp { Ok(42) }
  | "ID" "LBRACE" field_value_list "RBRACE" { Ok(42) }

  /* Variables, field, elements of an array. */
  | lvalue { Ok(42) }

  /* Function call. */
  | "ID" "LPAREN" args "RPAREN" { Ok(42) }

  /* Operations. */
  | "MINUS" exp %prec "UMINUS" { Ok(42) }
  | exp "OR" exp { Ok(42) }
  | exp "AND" exp  { Ok(42) }
  | exp "EQ" exp  { Ok(42) }
  | exp "NEQ" exp  { Ok(42) }
  | exp "LT" exp  { Ok(42) }
  | exp "LE" exp  { Ok(42) }
  | exp "GT" exp  { Ok(42) }
  | exp "GE" exp  { Ok(42) }
  | exp "PLUS" exp  { Ok(42) }
  | exp "MINUS" exp  { Ok(42) }
  | exp "TIMES" exp  { Ok(42) }
  | exp "DIVIDE" exp  { Ok(42) }

  | "LPAREN" exps "RPAREN" { Ok(42) }

  /* Assignment. */
  | lvalue "ASSIGN" exp { Ok(42) }

  /* Control structures. */
  | "IF" exp "THEN" exp { Ok(42) }
  | "IF" exp "THEN" exp "ELSE" exp { Ok(42) }
  | "WHILE" exp "DO" exp { Ok(42) }
  | "FOR" "ID" "ASSIGN" exp "TO" exp "DO" exp { Ok(42) }
  | "BREAK" { Ok(42) }
  | "LET" chunks "IN" exps "END" { Ok(42) }
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
  | tydec { Ok(42) }
  | fundec { Ok(42) }
  | vardec  { Ok(42) }
  | "IMPORT" "STRING" { Ok(42) }
  | chunk_helper tydec { Ok(42) }
  | chunk_helper fundec  { Ok(42) }
  | chunk_helper vardec { Ok(42) }
  | chunk_helper "IMPORT" "STRING" { Ok(42) }
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