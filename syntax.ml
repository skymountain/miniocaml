(* ML interpreter / type reconstruction *)
exception Parse_error of string
type id = string

(* binary operator *)    
type binOp = Plus | Mult | Lt | Band | Bor | Cons

(* pattern expression *)
type constant =
    CInt of int
  | CBool of bool
  | CNull
type pattern =
    Wildcard
  | Const of constant
  | As of pattern * id
  | Or of pattern * pattern
  | Lpat of pattern list
  | Conspat of pattern * pattern  (* cons pattern *)
  | Varpat of id

(* type *)
type ty =
    TyInt
  | TyBool
      
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
  | LetRecExp of id list * id list * exp list * exp
  | LLit of exp list 
  (* | MatchExp of exp * exp * id * id * exp (\* match exp with [] -> exp | id::id -> exp *\) *)
  | MatchExp of exp * (pattern * exp) list
      
type letBlockSeq =
    LetBlockSeq of id list * exp list * letBlockSeq
  | LetBlock of id list * exp list (* ids declared with "and" simultaneously *)
  | LetRecBlockSeq of id list * id list * exp list * letBlockSeq
  | LetRecBlock of id list * id list * exp list
      
type program = 
    Exp of exp
  | Decl of letBlockSeq
  | Seq of program * program
  (* | RecDecl of letRecBlockSeq *)


