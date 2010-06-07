open Syntax
open Eval

let rec read_eval_print env ch promp =
  let rec next_eval env =
    print_string promp;
    flush stdout;
    try 
      let decl = Parser.toplevel Lexer.main (Lexing.from_channel ch) in
      eval_decl env decl
    with
      Parsing.Parse_error -> print_endline "Syntax error"; next_eval env
    | Lexer.Lexical_error -> print_endline "Lexical error"; next_eval env
    | Eval.Error s        -> print_endline s; next_eval env
  in
  let (ids, newenv, vs) = next_eval env in
  List.iter2 (fun id v ->
                Printf.printf "print:\n";
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

(* task: ;; で区切られたプログラムを読み取れるようにする．  *)
(* 要因:  文字列を読みこんだとき，最初のプログラムだけを実行する．構文をセミコロン以降も続行するように変更 *)
let _ =
  if Array.length Sys.argv <= 1 then read_eval_print initial_env stdin "# "
  else
    match Array.to_list Sys.argv with
      _::files -> List.iter (fun file ->
                               match
                                 try Some (open_in file) with
                                   Sys_error _ -> None
                               with
                                 Some ch -> read_eval_print initial_env ch ""; close_in ch
                               | None    -> ()
                            ) files
     (* never reach *)
    | _ -> ()
