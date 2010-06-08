(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Mult | Lt | Band | Bor

type exp =
    Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetExp of id list * exp list * exp
  | FunExp of id * exp
  | AppExp of exp * exp
  | DFunExp of id * exp
      
type letBlockSeq =
    LetBlockSeq of id list * exp list * letBlockSeq
  | LetBlock of id list * exp list (* ids declared with "and" simultaneously *)
      
type program = 
    Exp of exp
  | Decl of letBlockSeq
