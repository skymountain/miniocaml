open Syntax

module StrSet = Misc.StrSet
exception Error of string
let err s = raise (Error s)

(* Type Environment *)
type tyenv = ty Environment.t
(* [idn->tyn] . ... . [id1 -> ty1] (type variable) *)
type subst = (tyvar * ty) list

let rec str_of_type = function
    TyInt -> "int"
  | TyBool -> "bool"
  | TyVar var -> Printf.sprintf "tv:%d" var
  | TyFun (ty1, ty2) -> Printf.sprintf "( %s -> %s )" (str_of_type ty1) (str_of_type ty2)
  | TyList ty -> (str_of_type ty) ^ " list"
      
let rec pp_ty ty =
  print_string (str_of_type ty)
    
let fresh_tyvar =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1; v
  in body

let rec freevar_ty = function
    TyInt | TyBool -> MySet.empty
  | TyVar var -> MySet.singleton var
  | TyFun (ty1, ty2) -> MySet.union (freevar_ty ty1) (freevar_ty ty2)
  | TyList ty -> freevar_ty ty
      
let rec subst_type s ty = match s with
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
      let var1 = variable ty1 in
      if MySet.member var1 (freevar_ty ty2) then err "The expression is not typable. Perhaps: impredicative typing"
      else 
        let subst =  (var1, ty2) in
        let subst_comp = [subst] in
        let newset = List.map (fun (ty1', ty2') -> subst_type subst_comp ty1', subst_type subst_comp ty2') t in
        subst::(unify newset)
  | (ty1, ty2)::t when is_var ty2 ->
      unify ((ty2, ty1)::t)
  | (TyFun (argty1, retty1), TyFun (argty2, retty2))::t ->
      unify ((argty1, argty2)::(retty1, retty2)::t)
  | (TyList ty1, TyList ty2)::t -> unify ((ty1, ty2)::t)
  | _ -> err "Fail to unify type expressions"
  
(* substitution -> equation set *)
let rec eqs_of_subst = function
    (id, ty)::t -> (TyVar id, ty)::(eqs_of_subst t)
  | [] -> []

let unify_neweqs subst eqs = unify (eqs@(eqs_of_subst subst))
let unify_neweq subst eq = unify_neweqs subst [eq]
      
let ty_prim op subst ty1 ty2 : Syntax.ty * (Syntax.tyvar * Syntax.ty) list=
  let f arg1_ty arg2_ty concl_ty =
    let subst = unify_neweqs subst [ty1, arg1_ty; ty2, arg2_ty] in
    concl_ty, subst
  in
  match op with
    Plus | Mult -> f TyInt TyInt TyInt 
  | Lt -> f TyInt TyInt TyBool 
  | Band | Bor -> f TyBool TyBool TyBool
  | Cons ->
      let varty = TyVar (fresh_tyvar ()) in
      f varty (TyList varty) (TyList varty)

let rec ty_exp tyenv (subst : (tyvar * ty) list) : exp -> ty * (tyvar * ty) list = function
    Var x ->
      (try subst_type subst (Environment.lookup x tyenv), subst with
         Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit _ -> TyInt, subst
  | BLit _ -> TyBool, subst
  | BinOp (op, exp1, exp2) ->
      let tyarg1, subst1 = ty_exp tyenv subst exp1 in
      let tyarg2, subst2 = ty_exp tyenv subst1 exp2 in
      ty_prim op subst2 tyarg1 tyarg2
  | IfExp (cond, exp1, exp2) ->
      let condty, condsubst = ty_exp tyenv subst cond in
      if subst_type condsubst condty <> TyBool then
        err ("Type of test expression must be boolean: if")
      else
        let ty1, subst1 = ty_exp tyenv condsubst exp1 in
        let ty2, subst2 = ty_exp tyenv subst1 exp2 in
        if subst_type subst2 ty1 <> subst_type subst2 ty2 then
          err ("Both then and else expressions must be same type")
        else ty1, subst2
  | LetExp (ids, es, exp2) ->
      let tys, subst' = ty_exps tyenv subst es in
      let newtyenv = Environment.extendl ids tys tyenv in
      ty_exp newtyenv subst' exp2
  | FunExp (id, exp) ->
      let tyvar = TyVar (fresh_tyvar ()) in
      let retty, subst' = ty_exp (Environment.extend id tyvar tyenv) subst exp in
      (* TyFun (subst_type subst' tyvar, retty), subst' *)
      TyFun (tyvar, retty), subst'
        
  (* | AppExp (exp1, exp2) -> *)
  (*     let is_fun = function TyFun _ -> true | _ -> false in *)
  (*     let func = function TyFun (arg, ret) -> arg, ret | _ -> assert false in *)
  (*     let ty1, subst1 = ty_exp tyenv subst exp1 in *)
  (*     let fty = subst_type subst1 ty1 in *)
  (*     if not (is_fun fty) then err "Non-function value cannot be applied" *)
  (*     else *)
  (*       let ty2, subst2 = ty_exp tyenv subst1 exp2 in *)
  (*       let fargty, retty = func fty in *)
  (*       let aargty = subst_type subst2 ty2 in  *)
  (*       if eq_ty fargty aargty then fty, subst2 *)
  (*       else *)
  (*         err (Printf.sprintf "Expected type: %s, actual type: %s" *)
  (*                (str_of_type fargty) (str_of_type aargty)) *)

  | LetRecExp (ids, args, bodies, exp) ->
      let f id arg body subst =
        let argvar = TyVar (fresh_tyvar ()) in
        let retvar = TyVar (fresh_tyvar ()) in
        let idty = TyFun (argvar, retvar) in
        let tyenv' = Environment.extend id idty tyenv in
        let tyenv' = Environment.extend arg argvar tyenv' in
        let bty, newsubst = ty_exp tyenv' subst body in
        TyFun (argvar, bty), unify_neweq newsubst (retvar, idty)
      in
      let tys, newsubst =
        List.fold_right (fun (id,arg,body) (tys, subst') ->
                           let ty, newsubst = f id arg body subst in
                           ty::tys, newsubst)
          (Misc.combine3 ids args bodies) ([], subst) 
      in
      let newtyenv = Environment.extendl ids tys tyenv in
      ty_exp newtyenv newsubst exp
  | LLit [] ->
      TyList (TyVar (fresh_tyvar ())), subst
  | LLit (fexp::exps) ->
      let fty, newsubst = ty_exp tyenv subst fexp in
      let tys, newsubst = ty_exps tyenv newsubst exps in
      let fty = subst_type newsubst fty in
      if List.for_all (fun ty -> eq_ty fty (subst_type newsubst ty)) tys then
        TyList fty, newsubst
      else
        err ("All elements of List must be same type")
  | MatchExp (cond, pat_exps) ->
      let condty, newsubst = ty_exp tyenv subst cond in
      let condty = subst_type newsubst condty in
      let to_type = function
          CInt _ -> TyInt
        | CBool _ -> TyBool
        | CNull -> TyList (TyVar (fresh_tyvar ()))
      in
      let rec ty_pattern tyenv subst condty = function
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
              Environment.extend id condty tyenv, newsubst, StrSet.add id bounds
        | Or (p1, p2) ->
            let tyenv1, subst1, b1 = ty_pattern tyenv subst condty p1 in
            let tyenv2, subst2, b2 = ty_pattern tyenv subst1 condty p2 in
            if not (StrSet.equal b1 b2) then err "Same variables must occur on both sides of | pattern"
            else
              let bvars, btys1, btys2 = (StrSet.fold (fun x (bvars, ty1s, ty2s) ->
                                                        x::bvars,
                                                        (Environment.lookup x tyenv1)::ty1s,
                                                        (Environment.lookup x tyenv2)::ty2s)
                                           b1 ([], [], []))
              in
              let btys1, btys2 = subst_types subst2 btys1, subst_types subst2 btys2 in
              if not (eq_tys btys1 btys2) then
                err "Each variable must have same type on both sides of | pattern"
              else 
                Environment.extendl bvars btys1 tyenv, subst2, b1
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
            Environment.extend id condty tyenv, subst, Misc.ebound
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
           fexpty, newsubst
       | _ -> assert false)
  | _ -> err ("Not Implemented!")
      
and ty_exps tyenv subst exps =
  List.fold_right (fun exp (tys, subst) ->
                     let ty, newsubst = ty_exp tyenv subst exp in
                     ty::tys, newsubst)
    exps ([], subst)
      
let ty_decl tyenv = function
    Exp e -> [ty_exp tyenv [] e]
  | _ -> err ("Not Implemented!")
