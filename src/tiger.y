%start program
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
exps :
    exp 
    | exps "SEMICOLON" exp 
    ;

exp :
  /* Literals. */
    "NIL"
  | "INT"
  | "STRING"

  /* Array and record creations. */
  | type_id "LBRACK" exp "RBRACK" "OF" exp
  | type_id "LBRACE" field_value_list "RBRACE"

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
    | "VAR" "ID" "COLON" type_id "ASSIGN" exp 
    ;

/* Type declaration. */
tydec : "TYPE" "ID" "EQ" ty 
;

/* Function declaration. */
fundec :
    "FUNCTION" "ID" "LPAREN" tyfields "RPAREN" "EQ" exp
    "FUNCTION" "ID" "LPAREN" tyfields "RPAREN" "COLON" type_id "EQ" exp
  | "PRIMITIVE" "ID" "LPAREN" tyfields "RPAREN" 
  | "PRIMITIVE" "ID" "LPAREN" tyfields "RPAREN" "COLON" type_id 
  ;


/* === Types. === */
ty :
   /* Type alias. */
     type_id
   /* Record type definition. */
   | "LBRACE" tyfields "RBRACE"
   /* Array type definition. */
   | "ARRAY" "OF" type_id
   ;

tyfields : /* Empty */
    | tyfields_helper
    ;

tyfields_helper :
    "ID" "COLON" type_id 
    | tyfields_helper "COMMA" "ID" "COLON" type_id 
    ;

type_id : "ID" ;
%%