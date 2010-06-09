open Syntax
open Eval

let err s = print_endline s

let rec read_eval_print env parse lexbuf promp c eof =
  let rec next_eval env =
    print_string promp;
    flush stdout;
    try 
      let decl = parse (Lexer.main eof) lexbuf in
      eval env decl
    with
      Parsing.Parse_error   -> err "Syntax error"; cf () env
    | Lexer.Lexical_error s -> err s; cf () env
    | Eval.Error s          -> err s; cf () env
    | Syntax.Parse_error s  -> err s; cf () env
  and cf () = if c then next_eval else (raise End_of_file)
  in
  let (ids, newenv, vs) = next_eval env in
  List.iter2 (fun id v ->
                Printf.printf "val %s = " id;
                pp_val v;
                print_newline()
             )
    ids vs;
  if c then read_eval_print newenv parse lexbuf promp c eof
  else ()
    
let initial_env = 
  Environment.extend "i" (IntV 1)
    (Environment.extend "v" (IntV 5) 
       (Environment.extend "x" (IntV 10)
          (Environment.extend "ii" (IntV 2)
             (Environment.extend "iii" (IntV 3)
                (Environment.extend "iv" (IntV 4) Environment.empty)))))

let read_eval_print env parse lexbuf promp c eof =
  try read_eval_print env parse lexbuf promp c eof with
    End_of_file -> ()

let read_all file =
  let ch = open_in file in
  let rec f acc =
    try
      f (input_line ch::acc)
    with
      End_of_file -> acc
  in
  let s = String.concat "\n" (List.rev (f [])) in
  close_in ch;
  (* print_endline (Printf.sprintf "%s: ->|\n%s\n|<-" file s); *)
  s
    
let _ =
  if Array.length Sys.argv <= 1 then read_eval_print initial_env Parser.toplevel (Lexing.from_channel stdin) "# " true true
  else
    let files = List.tl (Array.to_list Sys.argv) in
    let f file =
      try
        let s = read_all file in
        read_eval_print initial_env Parser.toplevel_batch (Lexing.from_string s) "" false false;
      with
        Sys_error _ -> err ("Cannot open the file: " ^ file)
    in
    List.iter f files
