open Syntax 

module StrSet = Set.Make (struct
                            type t = string
                            let compare = String.compare
                          end)
let ebound = StrSet.empty

(* for debug *)
let print_set s set =
  print_endline s;
  StrSet.iter (fun s -> print_string (Printf.sprintf "%s," s)) set;
  print_newline ();
  
type exval = 
    IntV of int
  | BoolV of bool
  | ProcV of id * exp * dnval Environment.t ref (* static binding *)
  | DProcV of id * exp                          (* dynamic binding *)
  | ListV of exval list
and dnval = exval

exception Error of string
exception Exit 
  
let err s = raise (Error s)

(* pretty printing *)
let rec pp_val = function
    IntV i -> 
      print_int i
  | BoolV b -> 
      if b then print_string "true" else print_string "false"
  | ProcV _ -> print_string "<function>"
  | DProcV _ -> print_string "<dfunction>"
  | ListV vs -> print_string "["; List.iter (fun v -> pp_val v; print_string ";") vs; print_string "]"
      
(********************************************************)      
(************************* eval *************************)
(********************************************************)
      
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
  | Cons, h       , ListV t  -> ListV (h::t)
  | Cons, _       , _        -> err ("Second argument must be list: ::")
      
(* evaluate expression *)
let rec eval_exp env = function
    Var x -> 
      (try Environment.lookup x env with 
        Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit i -> IntV i
  | BLit b -> BoolV b
  (* exp1 op exp2 *)
  | BinOp (op, exp1, exp2) ->   
      let arg1 = eval_exp env exp1 in
      let arg2 = eval_exp env exp2 in
      apply_prim op arg1 arg2
  (* if exp1 then exp2 else exp3 *)
  | IfExp (exp1, exp2, exp3) -> 
      let test = eval_exp env exp1 in
        (match test with
            BoolV true -> eval_exp env exp2 
          | BoolV false -> eval_exp env exp3
          | _ -> err ("Test expression must be boolean: if"))
  (* let id = exp1 and id' = ... in exp2 *)
  | LetExp (ids, es, exp2) -> 
      let vs = eval_exps env es in
      eval_exp (Environment.extendl ids vs env) exp2
  (* fun id -> exp *)
  | FunExp (id, exp) -> ProcV (id, exp, ref env)
  (* dfun id -> exp *)
  | DFunExp (id, exp) -> DProcV (id, exp)
  (* exp1 exp2 *)
  | AppExp (exp1, exp2) ->
      let funval = eval_exp env exp1 in
      let arg = eval_exp env exp2 in
      (match funval with
         ProcV (id, body, env') ->
           let newenv = Environment.extend id arg !env' in
           eval_exp newenv body
       | DProcV (id, body) ->
           let newenv = Environment.extend id arg env in
           eval_exp newenv body
       | _ -> err "Non-function value is applied")
  (* let rec id = fun para -> exp1 and id' = ... in exp2 *)
  | LetRecExp (ids, paras, exps, exp2) ->
      let dummyenv = ref Environment.empty in
      let rec f env = function
          id::idt, para::parat, exp::expt ->
            let newenv = Environment.extend id (ProcV (para, exp, dummyenv)) env in
            f newenv (idt, parat, expt)
        | [], [], [] -> env
        | _ -> assert false
      in
      let newenv = f env (ids, paras, exps) in
      dummyenv := newenv;
      eval_exp newenv exp2
  (* list literal *)
  | LLit exps ->
      ListV (List.map (eval_exp env) exps)
  (* match *)
  | MatchExp (cond, l) ->
      let rec matching env condv = function
          Wildcard -> env, ebound, true
        | Const c  -> (match condv, c with
                         IntV i1 , CInt i2  when i1 = i2 -> env, ebound, true
                       | BoolV b1, CBool b2 when b1 = b2 -> env, ebound, true
                       | ListV [], CNull                 -> env, ebound, true
                       | _                               -> env, ebound, false)
        | As (p, id) ->
            (match matching env condv p with
               _, bounds, false -> env, StrSet.add id bounds, false
             | _, bounds, _ when StrSet.mem id bounds
                 -> err "One or more Variables is bound several times"
             | env', bounds, _ -> (Environment.extend id condv env'), StrSet.add id bounds, true)
        | Or (p1, p2) ->
            (match matching env condv p1, matching env condv p2 with
               (_,b1,_), (_,b2,_) when not (StrSet.equal b1 b2)
                 -> begin
                   err "Same variables must occur on both sides of | pattern"
                 end
             | (_,b,false), (_,_,false) -> env, b, false
             | (env',b,true),_ | _,(env',b,true) -> env', b, true)
        | Lpat ps when (match condv with ListV _ -> true | _ -> false) ->
            let vs = match condv with ListV x -> x | _ -> assert false in
            (try
               let vps = List.combine vs ps in
               let (env', b', x) =
               List.fold_left (fun (env, bounds, b) (v, p) ->
                                 let env', bounds', b' = matching env v p in
                                 if StrSet.equal (StrSet.inter bounds bounds') ebound then
                                   env', StrSet.union bounds bounds', b && b'
                                 else
                                   err "One or more Variables is bound several times")
                 (env, ebound, true) vps in
               (env', b', x)
             with
               Invalid_argument _ -> (env, ebound, false))
        | Lpat _ -> (env, ebound, false)
      in
      let condv = eval_exp env cond in
      let x =
        List.fold_left (fun x (p, e) ->
                          match x with
                            Some x -> Some x
                          | None   -> (match matching env condv p with
                                         (_,_,false) -> None
                                       | (env',_,_) -> Some (env', e)))
          None l
      in
      match x with
        Some (env', e) -> eval_exp env' e
      | None           -> err "Matching fails"
          
(* evaluate expressions *)
and eval_exps env es =  List.map (fun e-> eval_exp env e) es

(***********************************************************)
(************************* program *************************)
(***********************************************************)
  
(* evaluate (let-)declaretion *)  
let rec eval_let_decl ids env vs l =
  let rec f acc_ids env acc_vs = function
      LetBlockSeq (ids, es, r) ->
        let vs = eval_exps env es in 
        eval_decl ((List.rev ids)@acc_ids) (Environment.extendl ids vs env) ((List.rev vs)@acc_vs) r
          
    | LetBlock (ids, es) ->
        let vs = eval_exps env es in
        List.rev (ids@acc_ids), (Environment.extendl ids vs env), List.rev (vs@acc_vs)
    | _ -> assert false;
  in
  f [] env [] l
and eval_letrec_decl ids env vs l =
  (* evaluate series of expressions with "and" *)
  let rec eval_andexp env acc_vs dummyenv =
    function
      id::idt, para::parat, exp::expt ->
        let v = (ProcV (para, exp, dummyenv)) in
        eval_andexp env (v::acc_vs) dummyenv (idt, parat, expt) 
    | [], [], [] -> List.rev acc_vs
    | _ -> assert false
  in
  (* evaluate straight "and" bloxks, and extend a environmen with them. *)
  let eval_anddecl env ids paras exps =
    let dummyenv = ref Environment.empty in
    let vs = eval_andexp env [] dummyenv (ids, paras, exps) in
    let newenv = Environment.extendl ids vs env in
    dummyenv := newenv;
    newenv, vs
  in
  let rec f acc_ids env acc_vs = function
      LetRecBlockSeq (ids, paras, exps, r) -> (* one of straight blocks *)
        let newenv, vs = eval_anddecl env ids paras exps in
        eval_decl ((List.rev ids)@acc_ids) newenv ((List.rev vs)@acc_vs) r
    | LetRecBlock (ids, paras, exps) ->
        let newenv, vs = eval_anddecl env ids paras exps in
        List.rev (ids@acc_ids), newenv, List.rev (vs@acc_vs)
    | _ -> assert false;
  in
  f [] env [] l
and eval_decl ids env vs =
  function
    (LetBlockSeq _) as l    -> eval_let_decl ids env vs l
  | (LetBlock _) as l       -> eval_let_decl ids env vs l
  | (LetRecBlockSeq _) as l -> eval_letrec_decl ids env vs l
  | (LetRecBlock _) as l    -> eval_letrec_decl ids env vs l
    
(* evaluate program *)    
let eval env = function
    Exp e -> let v = eval_exp env e in (["-"], env, [v])
  | Decl l -> eval_decl [] env [] l
  (* | RecDecl l -> eval_recdecl env l *)
        
      (* (id, para, exp) -> *)
      (* let dummyenv = ref Environment.empty in *)
      (* let v = ProcV (para, exp, dummyenv) in *)
      (* let newenv = Environment.extend id v env in *)
      (* dummyenv := newenv; *)
      (* [id], newenv, [v] *)
