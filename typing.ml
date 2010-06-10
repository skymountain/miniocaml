open Syntax

module StrSet = Misc.StrSet
exception Error of string
let err s = raise (Error s)

(* Type Environment *)
type tyenv = tysc Environment.t
(* [idn->tyn] . ... . [id1 -> ty1] (type variable) *)
type subst = (tyvar * ty) list
    
let rec str_of_type = function
    TyInt -> "int"
  | TyBool -> "bool"
  | TyVar var -> Printf.sprintf "tv:%d" var
  | TyFun (ty1, ty2) -> Printf.sprintf "( %s -> %s )" (str_of_type ty1) (str_of_type ty2)
  | TyList ty -> (str_of_type ty) ^ " list"

let rec str_of_tysc (TyScheme (vars, ty)) =
  let varstr = String.concat "," (List.map str_of_type (List.map (fun x -> TyVar x) vars)) in
  if String.length varstr = 0 then str_of_type ty
  else Printf.sprintf "|%s| . %s" varstr (str_of_type ty)
  
let rec pp_ty ty =
  print_string (str_of_type ty)
let rec pp_tysc ty =
  print_string (str_of_tysc ty)
    
let fresh_tyvar =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1; v
  in body

let tysc_of_ty ty = TyScheme([], ty)
       
let rec freevar_ty = function
    TyInt | TyBool -> MySet.empty
  | TyVar var -> MySet.singleton var
  | TyFun (ty1, ty2) -> MySet.union (freevar_ty ty1) (freevar_ty ty2)
  | TyList ty -> freevar_ty ty

let freevar_tysc (TyScheme (tyvars, ty)) =
  MySet.diff (freevar_ty ty) (MySet.from_list tyvars)

let freevar_tyenv tyenv =
  Environment.fold_right (fun tysc tyvars -> MySet.union (freevar_tysc tysc) tyvars) tyenv MySet.empty

let rec subst_type s ty =
  match s with
    [] -> ty
  | (id, ty')::t ->
      let rec f = function
          TyInt | TyBool as t -> t
        | TyVar var as v -> if id = var then ty' else v
        | TyFun (ty1, ty2) -> TyFun (f ty1, f ty2)
        | TyList ty -> TyList (f ty)
      in
      let newty = f ty in
      subst_type t newty

let closure ty tyenv subst =
  let fv_tyenv' = freevar_tyenv tyenv in
  let fv_tyenv =
    MySet.bigunion
      (MySet.map
         (fun id -> freevar_ty (subst_type subst (TyVar id)))
         fv_tyenv')
  in
  let ids = MySet.diff (freevar_ty ty) fv_tyenv in
  TyScheme (MySet.to_list ids, ty)

let closure_tys tyenv subst tys = List.map (fun x -> closure x tyenv subst) tys
    
let rec subst_types subst = function
    h::t -> (subst_type subst h)::(subst_types subst t)
  | []   -> []
        
let rec eq_ty ty1 ty2 = match ty1, ty2 with
    TyInt, TyInt | TyBool, TyBool -> true
  | TyVar var1, TyVar var2 -> var1 = var2
  | TyFun (arg1, ret1), TyFun (arg2, ret2) -> eq_ty arg1 arg2 && eq_ty ret1 ret2
  | TyList ty1, TyList ty2 -> eq_ty ty1 ty2
  | _ -> false

let rec eq_tys t1 t2 = List.for_all (fun (t1,t2) -> eq_ty t1 t2) (List.combine t1 t2)
      
let rec unify set =
  let is_var = function
      TyVar _ -> true
    | _       -> false
  in
  let variable = function
      TyVar x -> x
    | _ -> assert false
  in
  match set with
    [] -> []
  | (ty1, ty2)::t when eq_ty ty1 ty2 -> unify t
  | (ty1, ty2)::t when is_var ty1 ->
      
      (* print_string "@@"; *)
      (* pp_ty ty1; *)
      (* print_string ", "; *)
      (* pp_ty ty2; *)
      (* print_endline "@@"; *)
      
      let var1 = variable ty1 in
      if MySet.member var1 (freevar_ty ty2) then err "The expression is not typable. Perhaps: impredicative typing"
      else 
        let subst =  (var1, ty2) in
        let subst_comp = [subst] in
        let newset = List.map (fun (ty1, ty2) -> subst_type subst_comp ty1, subst_type subst_comp ty2) t in

        (* print_string "+++after substitution start+++\n"; *)
        (* List.iter (fun (ty1, ty2) -> *)
        (*              print_string ">>"; *)
        (*              pp_ty ty1; *)
        (*              print_string ", "; *)
        (*              pp_ty ty2; *)
        (*              print_endline "<<") newset; *)
        (* print_string "+++after substitution end+++\n"; *)
        
        subst::(unify newset)
  | (ty1, ty2)::t when is_var ty2 ->
      unify ((ty2, ty1)::t)
  | (TyFun (argty1, retty1), TyFun (argty2, retty2))::t ->
      unify ((argty1, argty2)::(retty1, retty2)::t)
  | (TyList ty1, TyList ty2)::t -> unify ((ty1, ty2)::t)
  | _ -> err "Fail to unify type expression"
  
(* substitution -> equation set *)
let rec eqs_of_subst = function
    (id, ty)::t -> (TyVar id, ty)::(eqs_of_subst t)
  | [] -> []

let unify_neweqs subst eqs = unify (eqs@(eqs_of_subst subst))
let unify_neweq subst eq = unify_neweqs subst [eq]

(* for debug *)
(* let pp_subst subst = *)
(*   print_endline "---subst start---"; *)
(*   let rec f = function *)
(*       [] -> print_endline "---subst end---" *)
(*     | (v,t)::tys -> Printf.printf "tvar: %d, type: %s\n" v (str_of_type t); f tys *)
(*   in *)
(*   f subst *)

let unify_with_sign subst tys signs =
  List.map
    (fun (ty, sign) -> match sign with
       Some sign -> subst_type (unify_neweq subst (ty, sign)) ty
     | None      -> ty)
    (List.combine tys signs)

let unify_with_retsig subst tys retsigs =
  let rec to_retty = function
      TyFun (paraty, retty) -> (match retty with
                             TyFun _ -> (fun x -> TyFun (paraty, to_retty retty x))
                           | t -> (fun x -> TyFun (paraty, x)))
    | _ -> assert false
  in
  let rettys = List.map to_retty tys in
  let signs = List.map (fun (retty, retsig) ->
                          match retsig with
                            None -> None
                          | Some t -> Some (retty t)) (List.combine rettys retsigs)
  in
  unify_with_sign subst tys signs

    
let ty_prim op subst ty1 ty2 : Syntax.ty * (Syntax.tyvar * Syntax.ty) list=
  let f arg1_ty arg2_ty concl_ty =
    let subst = unify_neweqs subst [ty1, arg1_ty; ty2, arg2_ty] in
    subst_type subst concl_ty, subst
  in
  match op with
    Plus | Mult -> f TyInt TyInt TyInt 
  | Lt -> f TyInt TyInt TyBool 
  | Band | Bor -> f TyBool TyBool TyBool
  | Cons ->
      let varty = TyVar (fresh_tyvar ()) in
      f varty (TyList varty) (TyList varty)

let rec ty_exp tyenv subst : exp -> ty * (tyvar * ty) list = function
    (* transform each of bound type variable into a fresh type variable *)
    Var x ->
      (try
         let TyScheme (vars, ty) = Environment.lookup x tyenv in
         let subst' = List.map (fun id -> (id, TyVar (fresh_tyvar ()))) vars in
         subst_type subst' ty, subst
       with
         Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit _ -> TyInt, subst
  | BLit _ -> TyBool, subst
  | BinOp (op, exp1, exp2) ->
      let tyarg1, subst1 = ty_exp tyenv subst exp1 in
      let tyarg2, subst2 = ty_exp tyenv subst1 exp2 in
      ty_prim op subst2 tyarg1 tyarg2
  | IfExp (cond, exp1, exp2) ->
      let condty = TyBool in
      let condty, subst = ty_exp_expectty tyenv subst cond condty in
      let ety = TyVar (fresh_tyvar ()) in
      let ety, subst = ty_exp_expectty tyenv subst exp1 ety in
      let ety, subst = ty_exp_expectty tyenv subst exp2 ety in
      subst_type subst ety, subst
      
      (* print_string "--- if start ---\n"; *)
      (* pp_ty ety; *)
            
      (* let _, subst = ty_exp_expectty tyenv subst exp1 ety in *)

      (* pp_subst subst; *)

      (* print_string "||| pp_subst start |||\n"; *)
      (* pp_ty (subst_type subst ety); *)
      (* print_string "\n||| pp_subst end |||\n"; *)
      (* print_string ", "; *)
      
      (* let _, subst = ty_exp_expectty tyenv subst exp2 ety in *)

      (* pp_subst subst; *)

      (* print_string "||| pp_subst start |||\n"; *)
      (* pp_ty (subst_type subst ety); *)
      (* print_string "||| pp_subst end |||\n"; *)
      (* print_string "--- if end ---\n"; *)
      
      (* subst_type subst ety, subst *)

      (******************************)
      
      (* let ety1 = TyVar (fresh_tyvar ()) in *)
      (* let ety2 = TyVar (fresh_tyvar ()) in *)
      (* let ety1', subst = ty_exp_expectty tyenv subst exp1 ety1 in *)
      (* let ety2', subst = ty_exp_expectty tyenv subst exp2 ety2 in *)

      (* print_string "1: "; *)
      (* pp_ty (subst_type subst ety1); *)
      (* print_string ", "; *)
      (* pp_ty ety1'; *)

      (* print_string ",  2: "; *)
      (* pp_ty (subst_type subst ety2); *)
      (* print_string ", "; *)
      (* pp_ty ety2'; *)
      
      (* let subst = unify_neweq subst (ety1, ety2) in *)

      (* print_string ",  subst: ety1::"; *)
      (* pp_ty (subst_type subst ety1); *)
      (* print_string ",  ety2::"; *)
      (* pp_ty (subst_type subst ety2); *)
      (* print_newline (); *)


      (* let ety1 = ety1' in *)
      (* let ety2 = ety2' in *)
      (* let subst = unify_neweq subst (ety1, ety2) in *)
      
      (* subst_type subst ety2, subst *)
  | LetExp (ids, es, retsigs, exp2) ->
      let tys, subst = ty_exps tyenv subst es in
      let tys = unify_with_retsig subst tys retsigs in
      let tyscs = closure_tys tyenv subst tys in
      let newtyenv = Environment.extendl ids tyscs tyenv in
      ty_exp newtyenv subst exp2
  | FunExp (id, paraty, exp) ->
      let tyvar = match paraty with None -> TyVar (fresh_tyvar ()) | Some t -> t in
      let retty, subst = ty_exp (Environment.extend id (tysc_of_ty tyvar) tyenv) subst exp in
      subst_type subst (TyFun (tyvar, retty)), subst
  | AppExp (exp1, exp2) ->      
      let argty = TyVar (fresh_tyvar ()) in
      let retty = TyVar (fresh_tyvar ()) in
      let fty = TyFun (argty, retty) in
      let fty, subst1 = ty_exp_expectty tyenv subst exp1 fty in
      let argty, subst2 = ty_exp_expectty tyenv subst1 exp2 argty in
      subst_type subst2 retty, subst2
  | LetRecExp (ids, args, paratys, bodies, retsigs, exp) ->
      (* let sigtys = *)
      (*   List.fold_left (fun acc _ -> *)
      (*                     let argvar = TyVar (fresh_tyvar ()) in *)
      (*                     let retvar = TyVar (fresh_tyvar ()) in *)
      (*                     let idty = TyFun (argvar, retvar) in *)
      (*                     (argvar, retvar, idty)::acc) [] ids *)
      (* in *)
      (* let idtyscs = List.map (fun (_,_,x) -> tysc_of_ty x) sigtys in *)
      (* let f id arg body subst (argvar, retvar, idty) = *)
      (*   (\* let argvar = TyVar (fresh_tyvar ()) in *\) *)
      (*   (\* let retvar = TyVar (fresh_tyvar ()) in *\) *)
      (*   (\* let idty = TyFun (argvar, retvar) in *\) *)
      (*   let tyenv = Environment.extendl ids idtyscs tyenv in *)
      (*   let tyenv = Environment.extend id (tysc_of_ty idty) tyenv in *)
      (*   let tyenv = Environment.extend arg (tysc_of_ty argvar) tyenv in *)
      (*   let bty, subst = ty_exp tyenv subst body in *)
      (*   let subst = unify_neweq subst (retvar, bty) in *)
      (*   subst_type subst idty, subst *)
      (* in *)
      (* let tys, newsubst = *)
      (*   List.fold_right (fun (id,arg,body,sigty) (tys, subst) -> *)
      (*                      let ty, newsubst = f id arg body subst sigty in *)
      (*                      ty::tys, newsubst) *)
      (*     (Misc.combine4 ids args bodies sigtys) ([], subst)  *)
      (* in *)
      (* let tyscs = List.map (fun x -> closure x tyenv newsubst) tys in *)
      (* let newtyenv = Environment.extendl ids tyscs tyenv in *)      
      let tys, subst = ty_letrec tyenv subst ids args paratys bodies in
      let tys = unify_with_retsig subst tys retsigs in
      let tyscs = closure_tys tyenv subst tys in
      let tyenv = Environment.extendl ids tyscs tyenv in
      ty_exp tyenv subst exp
  | LLit [] ->
      subst_type subst (TyList (TyVar (fresh_tyvar ()))), subst
  | LLit (fexp::exps) ->
      let fty, subst = ty_exp tyenv subst fexp in
      let tys, subst = ty_exps tyenv subst exps in
      let eqs = List.map (fun ty -> (fty,ty)) tys in
      let subst = unify_neweqs subst eqs in
      subst_type subst (TyList fty), subst
  | MatchExp (cond, pat_exps) ->
      let condty, newsubst = ty_exp tyenv subst cond in
      let condty = subst_type newsubst condty in
      let to_type = function
          CInt _ -> TyInt
        | CBool _ -> TyBool
        | CNull -> TyList (TyVar (fresh_tyvar ()))
      in
      let rec ty_pattern (tyenv : Syntax.tysc Environment.t) subst condty = function
          Wildcard -> tyenv, subst, Misc.ebound
        | Const c  -> (match condty, c with
                         TyInt, CInt _ -> tyenv, subst, Misc.ebound
                       | TyBool, CBool _ -> tyenv, subst, Misc.ebound
                       | TyList _, CNull -> tyenv, subst, Misc.ebound
                       | TyVar _, _ -> tyenv, unify_neweq subst (condty, to_type c), Misc.ebound
                       | _ -> err (Printf.sprintf "Type of pattern is %s, condition's type is %s"
                                     (str_of_type (to_type c)) (str_of_type condty)))
        | As (p, id) ->
            let newtyenv, newsubst, bounds = ty_pattern tyenv subst condty p in
            if StrSet.mem id bounds then err "One or more Variables is bound several times"
            else
              Environment.extend id (tysc_of_ty condty) tyenv, newsubst, StrSet.add id bounds
        | Or (p1, p2) ->
            let tyenv1, subst, b1 = ty_pattern tyenv subst condty p1 in
            let tyenv2, subst, b2 = ty_pattern tyenv subst condty p2 in
            if not (StrSet.equal b1 b2) then err "Same variables must occur on both sides of | pattern"
            else
              let ty_of_tysc = function
                  TyScheme ([], ty) -> ty
                | _ -> assert false
              in
              let bvars, btys1, btys2 = (StrSet.fold (fun x (bvars, ty1s, ty2s) ->
                                                        x::bvars,
                                                        (Environment.lookup x tyenv1)::ty1s,
                                                        (Environment.lookup x tyenv2)::ty2s)
                                           b1 ([], [], []))
              in
              let subst = unify_neweqs subst (List.combine (List.map ty_of_tysc btys1) (List.map ty_of_tysc btys2)) in
              Environment.extendl bvars btys1 tyenv(*tyenv1*), subst, b1
                
              (* let btys1, btys2 = subst_types subst2 btys1, subst_types subst2 btys2 in *)
              (* if not (eq_tys btys1 btys2) then *)
              (*   err "Each variable must have same type on both sides of | pattern" *)
              (* else  *)
              (*   Environment.extendl bvars btys1 tyenv, subst2, b1 *)
        | Lpat ps ->
            ty_pattern tyenv subst condty (List.fold_right (fun p acc -> Conspat (p, acc)) ps (Const CNull))
        | Conspat (hp, tp) ->
            let elemty, subst' = 
              (match condty with
                 TyVar _ ->
                   let elemty = TyVar (fresh_tyvar ()) in
                   elemty, unify_neweq subst (condty, TyList elemty)
               | TyList elemty ->
                   elemty, subst
               | t  -> err (Printf.sprintf "Type of pattern is %s, condition's type is %s"
                              (str_of_type (TyList (TyVar (fresh_tyvar ()))))
                              (str_of_type condty)))
            in
            (* let var = TyVar (fresh_tyvar ()) in *)
            (* let subst' = unify_neweq subst (var, elemty) in *)
            let tyenv', subst', bounds' = ty_pattern tyenv subst' elemty hp in
            let newtyenv, newsubst, bounds'' = ty_pattern tyenv' subst' condty tp in
            newtyenv, newsubst, Misc.union_of_dbounds bounds' bounds'' err
        | Varpat id ->
            Environment.extend id (tysc_of_ty condty) tyenv, subst, StrSet.add id StrSet.empty
      in
      let ty_exp tyenv subst condty p exp =
        let newtyenv, newsubst, _ = ty_pattern tyenv subst condty p in
        ty_exp newtyenv newsubst exp
      in
      (match pat_exps with
         [p, exp] -> ty_exp tyenv subst condty p exp
       | (p, exp)::pat_exps ->
           let fexpty, fsubst =  ty_exp tyenv subst condty p exp in
           let newsubst = List.fold_left (fun subst (p, exp) ->
                                            let expty', subst' = ty_exp tyenv subst condty p exp in
                                            let newsubst = unify_neweq subst' (fexpty, expty') in
                                            if not (eq_ty (subst_type newsubst fexpty) (subst_type newsubst expty')) then
                                              err "Each of pattern expression must have same type"
                                            else newsubst) fsubst pat_exps
           in
           subst_type newsubst fexpty, newsubst
       | _ -> assert false)
  | TypedExp (exp, ty) ->
      let ety, subst = ty_exp tyenv subst exp in
      let subst = unify_neweq subst (ety, ty) in
      ty, subst
  | DFunExp _ -> err "Not implemented !"

and ty_exp_expectty tyenv subst exp expectty =
  let ty, subst = ty_exp tyenv subst exp in
  (* print_endline "___start____"; *)
  (* let subst = unify_neweq subst (ty, expectty) in *)
  (* print_endline "___end____"; *)
  (* ty, subst *)
  ty, unify_neweq subst (ty, expectty)

and ty_exps tyenv subst exps =
  List.fold_right (fun exp (tys, subst) ->
                     let ty, newsubst = ty_exp tyenv subst exp in
                     ty::tys, newsubst)
    exps ([], subst)

and ty_letrec tyenv subst ids args paratys bodies =
  let sigtys =
    List.fold_left (fun acc paraty ->
                      let argvar = match paraty with None -> TyVar (fresh_tyvar ()) | Some t -> t in
                      let retvar = TyVar (fresh_tyvar ()) in
                      let idty = TyFun (argvar, retvar) in
                      (argvar, retvar, idty)::acc) [] paratys
  in
  let idtyscs = List.map (fun (_,_,x) -> tysc_of_ty x) sigtys in
  let f id arg body subst (argvar, retvar, idty) =
    (* let argvar = TyVar (fresh_tyvar ()) in *)
    (* let retvar = TyVar (fresh_tyvar ()) in *)
    (* let idty = TyFun (argvar, retvar) in *)
    let tyenv = Environment.extendl ids idtyscs tyenv in
    let tyenv = Environment.extend id (tysc_of_ty idty) tyenv in
    let tyenv = Environment.extend arg (tysc_of_ty argvar) tyenv in
    let bty, subst = ty_exp tyenv subst body in
    let subst = unify_neweq subst (retvar, bty) in
    subst_type subst idty, subst
  in
  let tys, newsubst =
    List.fold_right (fun (id,arg,body,sigty) (tys, subst) ->
                       let ty, newsubst = f id arg body subst sigty in
                       ty::tys, newsubst)
      (Misc.combine4 ids args bodies sigtys) ([], subst) 
  in
  tys, newsubst

let rec ty_decl ids tyenv tyscs =
  function
    LetBlock _    | LetBlockSeq _    as l -> ty_let_decl    ids tyenv tyscs l
  | LetRecBlock _ | LetRecBlockSeq _ as l -> ty_letrec_decl ids tyenv tyscs l
      
and ty_let_decl acc_ids tyenv acc_tyscs =
  let f ids es retsigs =
    let tys, subst = ty_exps tyenv [] es in
    let tys = unify_with_retsig subst tys retsigs in
    closure_tys tyenv [] tys 
  in
  function
    LetBlockSeq (ids, es, rettys, r) ->
      let tyscs = f ids es rettys in
      ty_decl ((List.rev ids)@acc_ids) (Environment.extendl ids tyscs tyenv) ((List.rev tyscs)@acc_tyscs) r
  | LetBlock (ids, es, rettys) ->
      let tyscs = f ids es rettys in
      List.rev (tyscs@acc_tyscs), (Environment.extendl ids tyscs tyenv)
  | _ -> assert false
      
and ty_letrec_decl  acc_ids tyenv acc_tyscs = function
    LetRecBlockSeq (ids, paras, paratys, bodies, retsigs, r) ->
      let tys, subst = ty_letrec tyenv [] ids paras paratys bodies in
      let tys = unify_with_retsig subst tys retsigs in
      let tyscs = closure_tys tyenv subst tys in
      ty_decl ((List.rev ids)@acc_ids) (Environment.extendl ids tyscs tyenv) (List.rev (tyscs@acc_tyscs)) r 
  | LetRecBlock (ids, paras, paratys, bodies, retsigs) ->
      let tys, subst = ty_letrec tyenv [] ids paras paratys bodies in
      let tys = unify_with_retsig subst tys retsigs in
      let tyscs = closure_tys tyenv subst tys in
      List.rev (tyscs@acc_tyscs), Environment.extendl ids tyscs tyenv
  | _ -> assert false
      
let rec ty tyenv = function
    Exp e -> let ty, subst = ty_exp tyenv [] e in [closure ty tyenv subst], tyenv
  | Decl l -> ty_decl [] tyenv [] l
  | Seq (p1, p2) ->
      let tyscs1, tyenv = ty tyenv p1 in
      let tyscs2, tyenv = ty tyenv p2 in
      tyscs1@tyscs2, tyenv
