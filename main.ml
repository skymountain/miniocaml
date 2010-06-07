open Syntax
open Eval

let rec read_eval_print env =
  let rec next_eval env =
    print_string "# ";
    flush stdout;
    try 
      let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
      eval_decl env decl
    with
      Parsing.Parse_error -> print_endline "Syntax error"; next_eval env
    | Failure _           -> print_endline "Lexical error"; next_eval env
    | Eval.Error s        -> print_endline s; next_eval env
  in
  let (ids, newenv, vs) = next_eval env in
  List.iter2 (fun id v ->
                Printf.printf "val %s = " id;
                pp_val v;
                print_newline()
             )
    ids vs;
  (* Printf.printf "val %s = " id; *)
  (* pp_val v; *)
  (* print_newline(); *)
  read_eval_print newenv
    
let initial_env = 
  Environment.extend "i" (IntV 1)
    (Environment.extend "v" (IntV 5) 
       (Environment.extend "x" (IntV 10)
          (Environment.extend "ii" (IntV 2)
             (Environment.extend "iii" (IntV 3)
                (Environment.extend "iv" (IntV 4) Environment.empty)))))
    
let _ =
  read_eval_print initial_env
    
