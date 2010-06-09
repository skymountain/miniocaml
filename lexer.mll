{
exception Lexical_error of string
  
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
  ("as"   , Parser.AS);
]

let err s = raise (Lexical_error s)
}

rule main raise_eof = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main raise_eof lexbuf }

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
| "(*" { comment 0 lexbuf; main raise_eof lexbuf }
(* | "(\*" { print_endline "comment start"; comment 0 lexbuf; main lexbuf } *)
| "->" { Parser.RARROW }
| "["  { Parser.LSQBRA }
| "]"  { Parser.RSQBRA }
| "::" { Parser.COLON2 }
| "|"  { Parser.PIPE }
| ";"  { Parser.SEMI }
| "_"  { Parser.UNDERBAR }

| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try 
        List.assoc id reservedWords
      with
      _ -> Parser.ID id
     }
| eof { print_endline "eof"; if raise_eof then raise End_of_file else Parser.EOF }
| _ as x { err (Printf.sprintf "Lexical error: %c"  x) }
        
(* skip commet region *)
and comment depth = parse
    "(*" { comment (depth + 1) lexbuf }
  | "*)" { if depth = 0 then () else comment (depth - 1) lexbuf }
  | eof  { err "Comment not terminated" }
  | _ { comment depth lexbuf }
  (*   "(\*" { print_endline "comment inc"; comment (depth + 1) lexbuf } *)
  (* | "*\)" { if depth = 0 then (print_endline "comment end"; ()) else (print_endline "comment dec"; comment (depth - 1) lexbuf) } *)
  (* | eof  { err "Comment not terminated" } *)
  (* | _ as x { print_char x; comment depth lexbuf } *)
