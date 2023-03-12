%start program

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
program :
    exp
  | chunks
  ;


/* === Expressions. === */
exps : /* Empty */
    | exps_helper
    ;
    
exps_helper : 
    exp 
    | exps_helper "SEMICOLON" exp 
    ;

exp :
  /* Literals. */
    "NIL"
  | "INT"
  | "STRING"

  /* Array and record creations. */
  | "ID" "LBRACK" exp "RBRACK" "OF" exp
  | "ID" "LBRACE" field_value_list "RBRACE"

  /* Variables, field, elements of an array. */
  | lvalue

  /* Function call. */
  | "ID" "LPAREN" args "RPAREN"

  /* Operations. */
  | "MINUS" exp %prec "UMINUS"
  | exp "OR" exp
  | exp "AND" exp 
  | exp "EQ" exp 
  | exp "NEQ" exp 
  | exp "LT" exp 
  | exp "LE" exp 
  | exp "GT" exp 
  | exp "GE" exp 
  | exp "PLUS" exp 
  | exp "MINUS" exp 
  | exp "TIMES" exp 
  | exp "DIVIDE" exp 

  | "LPAREN" exps "RPAREN"

  /* Assignment. */
  | lvalue "ASSIGN" exp

  /* Control structures. */
  | "IF" exp "THEN" exp
  | "IF" exp "THEN" exp "ELSE" exp
  | "WHILE" exp "DO" exp
  | "FOR" "ID" "ASSIGN" exp "TO" exp "DO" exp
  | "BREAK"
  | "LET" chunks "IN" exps "END"
  ;

field_value_list : /* Empty */
    | "ID" "EQ" exp 
    | field_value_list "COMMA" "ID" "EQ" exp 
    ;

args : /* Empty */
    | args_helper
    ;

args_helper :
    exp 
    | args_helper "COMMA" exp
    ;

lvalue :
    "ID"
  | "ID" "DOT" "ID"
  | "ID" "LBRACK" exp "RBRACK"
  /* Record field access. */
  | lvalue "DOT" "ID"
  /* Array subscript. */
  | lvalue "LBRACK" exp "RBRACK"
  ;

/* === Chunks of declarations. === */
chunks : /* Empty */
  | chunk
  | chunks chunk
  ;

chunk :
  tydec 
  | fundec
  | vardec
  | "IMPORT" "STRING"
  ;

/* Variable declaration. */
vardec : 
    "VAR" "ID" "ASSIGN" exp 
    | "VAR" "ID" "COLON" "ID" "ASSIGN" exp 
    ;

/* Type declaration. */
tydec : "TYPE" "ID" "EQ" ty 
;

/* Function declaration. */
fundec :
    "FUNCTION" "ID" "LPAREN" tyfields "RPAREN" "EQ" exp
  |  "FUNCTION" "ID" "LPAREN" tyfields "RPAREN" "COLON" "ID" "EQ" exp
  | "PRIMITIVE" "ID" "LPAREN" tyfields "RPAREN" 
  | "PRIMITIVE" "ID" "LPAREN" tyfields "RPAREN" "COLON" "ID" 
  ;


/* === Types. === */
ty :
   /* Type alias. */
     "ID"
   /* Record type definition. */
   | "LBRACE" tyfields "RBRACE"
   /* Array type definition. */
   | "ARRAY" "OF" "ID"
   ;

tyfields : /* Empty */
    | tyfields_helper
    ;

tyfields_helper :
    "ID" "COLON" "ID" 
    | tyfields_helper "COMMA" "ID" "COLON" "ID" 
    ;

%%