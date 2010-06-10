open Syntax

exception Error of string

let err s = raise (Error s)

(* Type Environment *)
type tyenv = ty Environment.t
(* [idn->tyn] . ... . [id1 -> ty1] (type variable) *)
type subst = (tyvar * ty) list

let rec to_str = function
    TyInt -> "int"
  | TyBool -> "bool"
  | TyVar var -> Printf.sprintf "tv:%d" var
  | TyFun (ty1, ty2) -> Printf.sprintf "( %s -> %s )" (to_str ty1) (to_str ty2)

let rec pp_ty ty =
  print_string (to_str ty)
    
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

let rec subst_type s ty = match s with
    [] -> ty
  | (id, ty')::t ->
      let rec f = function
          TyInt | TyBool as t -> t
        | TyVar var as v -> if id = var then ty' else v
        | TyFun (ty1, ty2) -> TyFun (f ty1, f ty2)
      in
      let newty = f ty in
      subst_type t newty

let rec eq_ty ty1 ty2 = match ty1, ty2 with
    TyInt, TyInt | TyBool, TyBool -> true
  | TyVar var1, TyVar var2 -> var1 = var2
  | TyFun (arg1, ret1), TyFun (arg2, ret2) -> eq_ty arg1 arg2 && eq_ty ret1 ret2
  | _ -> false

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
      if MySet.member var1 (freevar_ty ty2) then err "The expression is not typable"
      else 
        let subst =  (var1, ty2) in
        let subst_comp = [subst] in
        let newset = List.map (fun (ty1', ty2') -> subst_type subst_comp ty1', subst_type subst_comp ty2') t in
      subst::(unify newset)
  | (ty1, ty2)::t when is_var ty2 ->
      unify ((ty2, ty1)::t)
  | (TyFun (argty1, retty1), TyFun (argty2, retty2))::t ->
      unify ((argty1, argty2)::(retty1, retty2)::t)
  | _ -> err "Fail to unify type expressions"

(* substitution -> equation set *)
let rec eqs_of_subst = function
    (id, ty)::t -> (TyVar id, ty)::(eqs_of_subst t)
  | [] -> []
      
let ty_prim op subst ty1 ty2 : Syntax.ty * (Syntax.tyvar * Syntax.ty) list=
  let f arg1_ty arg2_ty concl_ty =
    let eqset = (ty1, arg1_ty)::(ty2, arg2_ty)::(eqs_of_subst subst) in
    let subst = unify eqset in
    concl_ty, subst
  in
  match op with
    Plus | Mult -> f TyInt TyInt TyInt 
  | Lt -> f TyInt TyInt TyBool 
  | Band | Bor -> f TyBool TyBool TyBool
  | Cons -> err "Not Implemented!"

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
          err ("Both then and else expressions must be same types")
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
  (*                (to_str fargty) (to_str aargty)) *)
  | _ -> err ("Not Implemented!")
      
and ty_exps tyenv subst exps =
  List.fold_right (fun exp (tys, subst) ->
                     let ty, newsubst = ty_exp tyenv subst exp in
                     ty::tys, newsubst)
    exps ([], subst)
      
let ty_decl tyenv = function
    Exp e -> [ty_exp tyenv [] e]
  | _ -> err ("Not Implemented!")
