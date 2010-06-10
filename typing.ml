open Syntax

exception Error of string

let err s = raise (Error s)

(* Type Environment *)
type tyenv = ty Environment.t
(* [idn->tyn] . ... . [id1 -> ty1] (type variable) *)
type subst = (tyvar * ty) list
    
let rec pp_ty = function
    TyInt -> print_string "int"
  | TyBool -> print_string "bool"
  | TyVar var -> Printf.printf "tv:%d" var
  | TyFun (ty1, ty2) -> print_string "("; pp_ty ty1; print_string "->"; pp_ty ty2; print_string ")"

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

let rec unify set =
  let rec eq_ty ty1 ty2 = match ty1, ty2 with
      TyInt, TyInt | TyBool, TyBool -> true
    | TyVar var1, TyVar var2 -> var1 = var2
    | TyFun (arg1, ret1), TyFun (arg2, ret2) -> eq_ty arg1 arg2 && eq_ty ret1 ret2
    | _ -> false
  in
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
      let subst =  [variable ty1, ty2] in
      let newset = List.map (fun (ty1', ty2') -> subst_type subst ty1', subst_type subst ty2') t in
      subst::(unify newset)
  | (ty1, ty2)::t when is_var ty2 ->
      unify ((ty2, ty1)::t)
  | (TyFun (argty1, retty1), TyFun (argty2, retty2))::t ->
      unify ((argty1, argty2)::(retty1, retty2)::t)
  | _ -> err "Fail to unify type expressions"
        
let ty_prim op ty1 ty2 = match op with
    Plus -> (match ty1, ty2 with
               TyInt, TyInt -> TyInt
             | _ -> err ("Argument must be of integer: +"))
  | Mult -> (match ty1, ty2 with
               TyInt, TyInt -> TyInt
             | _ -> err ("Argument must be of integer: *"))
  | Lt -> (match ty1, ty2 with
               TyInt, TyInt -> TyBool
             | _ -> err ("Argument must be of integer: <"))
  | Band -> (match ty1, ty2 with
               TyBool, TyBool -> TyBool
             | _ -> err ("Argument must be of boolean: &&"))
  | Bor ->  (match ty1, ty2 with
               TyBool, TyBool -> TyBool
             | _ -> err ("Argument must be of boolean: ||"))
  | Cons -> err "Not Implemented!"

let rec ty_exp tyenv = function
    Var x ->
      (try Environment.lookup x tyenv with
         Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit _ -> TyInt
  | BLit _ -> TyBool
  | BinOp (op, exp1, exp2) ->
      let tyarg1 = ty_exp tyenv exp1 in
      let tyarg2 = ty_exp tyenv exp2 in
      ty_prim op tyarg1 tyarg2
  | IfExp (cond, exp1, exp2) ->
      if ty_exp tyenv cond <> TyBool then
        err ("Type of test expression must be boolean: if")
      else
        let ty1, ty2 = ty_exp tyenv exp1, ty_exp tyenv exp2 in
        if ty1 <> ty2 then
          err ("Both then and else expressions must be a same type")
        else ty1
  | LetExp (ids, es, exp2) ->
      let tys = ty_exps tyenv es in
      let newtyenv = Environment.extendl ids tys tyenv in
      ty_exp newtyenv exp2 
  | _ -> err ("Not Implemented!")
and ty_exps tyenv =
  List.map (ty_exp tyenv)

let ty_decl tyenv = function
    Exp e -> [ty_exp tyenv e]
  | _ -> err ("Not Implemented!")
