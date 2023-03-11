%start prog
%right "OF"
%nonassoc "DO" "THEN"
%nonassoc "ELSE"
%nonassoc "Assign"
%left "OR"
%left "AND"
%nonassoc "EQ" "NEQ" "LT" "LE" "GT" "GE"
%left "PLUS" "MINUS"
%left "TIMES" "DIVIDE"
%left "UMINUS"

%%
prog -> Result<u64, ()>:
    exp { Ok(1) }
    ;
    
exp -> Result<u64, ()>:
    "NIL" { Ok(1) }
    "INT" { Ok(1) }
    "STRING" { Ok(1) }
    "ID" "LPAREN" "RPAREN" { Ok(1) }
    ;
%%
// Any functions here are in scope for all the grammar actions above.

fn parse_int(s: &str) -> Result<u64, ()> {
    match s.parse::<u64>() {
        Ok(val) => Ok(val),
        Err(_) => {
            eprintln!("{} cannot be represented as a u64", s);
            Err(())
        }
    }
}
