{
exception Lexical_error
  
let reservedWords = [
  (* Keywords *)
  ("else" , Parser.ELSE);
  ("false", Parser.FALSE);
  ("if"   , Parser.IF);
  ("then" , Parser.THEN);
  ("true" , Parser.TRUE);
  ("in"   , Parser.IN);
  ("let"  , Parser.LET);
  ("and"  , Parser.AND);
  ("fun"  , Parser.FUN);
  ("dfun" , Parser.DFUN);
  ("rec"  , Parser.REC);
  ("match", Parser.MATCH);
  ("with" , Parser.WITH);
] 
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }

| "-"? ['0'-'9']+
    { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "("  { Parser.LPAREN }
| ")"  { Parser.RPAREN }
| ";;" { Parser.SEMISEMI }
| "+"  { Parser.PLUS }
| "*"  { Parser.MULT }
| "<"  { Parser.LT }
| "&&" { Parser.BOOLAND }
| "||" { Parser.BOOLOR }
| "="  { Parser.EQ }
| "(*" { comment 0 lexbuf; main lexbuf }
| "->" { Parser.RARROW }
| "["  { Parser.LSQBRA }
| "]"  { Parser.RSQBRA }
| "::" { Parser.COLON2 }
| "|"  { Parser.PIPE }
| ";"  { Parser.SEMI }
      
| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try 
        List.assoc id reservedWords
      with
      _ -> Parser.ID id
     }
| eof { raise End_of_file }
| _   { raise Lexical_error }
        
(* skip commet region *)        
and comment depth = parse
    "(*" { comment (depth + 1) lexbuf }
  | "*)" { if depth = 0 then () else comment (depth - 1) lexbuf }
  | _    { comment depth lexbuf }
