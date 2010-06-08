open Syntax
open Eval

let err s = print_endline s

let rec read_eval_print env ch promp =
  let rec next_eval env =
    print_string promp;
    flush stdout;
    try 
      let decl = Parser.toplevel Lexer.main (Lexing.from_channel ch) in
      eval_decl env decl
    with
      Parsing.Parse_error -> err "Syntax error"; next_eval env
    | Lexer.Lexical_error -> err "Lexical error"; next_eval env
    | Eval.Error s        -> err s; next_eval env
  in
  let (ids, newenv, vs) = next_eval env in
  List.iter2 (fun id v ->
                Printf.printf "val %s = " id;
                pp_val v;
                print_newline()
             )
    ids vs;
  read_eval_print newenv ch promp
    
let initial_env = 
  Environment.extend "i" (IntV 1)
    (Environment.extend "v" (IntV 5) 
       (Environment.extend "x" (IntV 10)
          (Environment.extend "ii" (IntV 2)
             (Environment.extend "iii" (IntV 3)
                (Environment.extend "iv" (IntV 4) Environment.empty)))))

let read_eval_print env ch promp =
  try read_eval_print env ch promp with
    End_of_file -> ()

let _ =
  if Array.length Sys.argv <= 1 then read_eval_print initial_env stdin "# "
  else
    let files = List.tl (Array.to_list Sys.argv) in
    let f file =
      try
        let ch = open_in file in
        read_eval_print initial_env ch "";
        close_in ch
      with
        Sys_error _ -> err ("Cannot open the file: " ^ file)
    in
    List.iter f files
