open Syntax 

type exval = 
    IntV of int
  | BoolV of bool
  | ProcV of id * exp * dnval Environment.t
and dnval = exval

exception Error of string

let err s = raise (Error s)

(* pretty printing *)
let rec pp_val = function
    IntV i -> 
      print_int i
  | BoolV b -> 
      if b then print_string "true" else print_string "false"
  | ProcV _ -> print_string "<function>"

(* evaluate arithmetic expression *)        
let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with
    Plus, IntV i1 , IntV i2  -> IntV (i1 + i2)
  | Plus, _       , _        -> err ("Both arguments must be integer: +")
  | Mult, IntV i1 , IntV i2  -> IntV (i1 * i2)
  | Mult, _       , _        -> err ("Both arguments must be integer: *")
  | Lt  , IntV i1 , IntV i2  -> BoolV (i1 < i2)
  | Lt  , _       , _        -> err ("Both arguments must be integer: <")
  | Band, BoolV b1, BoolV b2 -> BoolV (b1 && b2)
  | Band, _       , _        -> err ("Both argument must be bool: &&")
  | Bor , BoolV b1, BoolV b2 -> BoolV (b1 || b2)
  | Bor , _       , _        -> err ("Both argument must be bool: ||")

(* evaluate expression *)
let rec eval_exp env = function
    Var x -> 
      (try Environment.lookup x env with 
        Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit i -> IntV i
  | BLit b -> BoolV b
  | BinOp (op, exp1, exp2) ->   (* exp1 op exp2 *)
      let arg1 = eval_exp env exp1 in
      let arg2 = eval_exp env exp2 in
      apply_prim op arg1 arg2
  | IfExp (exp1, exp2, exp3) -> (* if exp1 then exp2 else exp3 *)
      let test = eval_exp env exp1 in
        (match test with
            BoolV true -> eval_exp env exp2 
          | BoolV false -> eval_exp env exp3
          | _ -> err ("Test expression must be boolean: if"))
  | LetExp (ids, es, exp2) -> (* let id = exp1 and id' = ... in exp2 *)
      let vs = eval_exps env es in
      eval_exp (Environment.extendl ids vs env) exp2
  | FunExp (id, exp) -> ProcV (id, exp, env)
  | AppExp (exp1, exp2) ->
      let funval = eval_exp env exp1 in
      let arg = eval_exp env exp2 in
      (match funval with
         ProcV (id, body, env') ->
           let newenv = Environment.extend id arg env' in
           eval_exp newenv body
       | _ -> err "Non-function value is applied")
      
(* evaluate expressions *)
and eval_exps env es =  List.map (fun e-> eval_exp env e) es

(* evaluate declaretion *)  
let eval_decl env l =
  let rec f acc_ids env acc_vs = function
      LetBlockSeq (ids, es, r) ->
        let vs = eval_exps env es in 
        f  (ids@acc_ids) (Environment.extendl ids vs env) (vs@acc_vs) r
          
    | LetBlock (ids, es) ->
        (* let l = List.map (fun (id, e) -> (id, eval_exp env e)) l in *)
        (* let env = List.fold_left (fun env (id,v) -> Environment.extend id v env) env l in *)
        let vs = eval_exps env es in
        List.rev (ids@acc_ids), (Environment.extendl ids vs env), List.rev (vs@acc_vs)
  in
  f [] env [] l

(* evaluate program *)    
let eval env = function
    Exp e -> let v = eval_exp env e in (["-"], env, [v])
  | Decl l -> eval_decl env l
