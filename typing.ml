open Syntax

exception Error of string

let err s = raise (Error s)

(* Type Environment *)
type tyenv = ty Environment.t

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
