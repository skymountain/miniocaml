(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Mult | Lt | Band | Bor

type exp =
    Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp

type program = 
    Exp of exp
