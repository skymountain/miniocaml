%{
open Syntax

let to_funexp ids exp =
  List.fold_left (fun acc id -> FunExp (id, acc)) exp (List.rev ids)
    
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT BOOLAND BOOLOR
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ AND
%token RARROW FUN

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    Expr SEMISEMI { Exp $1 }
  | Decl SEMISEMI { Decl $1 }
      
Decl :
    Let      { let x, y = $1 in LetBlock (x, y) }
  | Let Decl { let x, y = $1 in LetBlockSeq (x, y, $2) }

Let :
    LET Letsub { $2 }

Letsub :
    ID EQ Expr { [$1], [$3] }
  | ID EQ Expr AND Letsub { let ids, vs = $5 in $1::ids, $3::vs }
  | ID IDs EQ Expr { [$1], [to_funexp $2 $4] }
  | ID IDs EQ Expr AND Letsub { let ids, vs = $6 in $1::ids, (to_funexp $2 $4)::vs }
      
Expr :
    IfExpr { $1 }
  | LetExpr { $1 }
  | BORExpr { $1 }
  | FunExpr { $1 }

LetExpr :
    Let IN Expr { let x, y = $1 in LetExp (x, y, $3) }

FunExpr :
    FUN IDs RARROW Expr { to_funexp $2 $4 }

IDs :
    ID { [$1] }
  | ID IDs { $1::$2 }
      
BORExpr :  /* left association */
    BORExpr BOOLOR BANDExpr { BinOp (Bor, $1, $3) }
  | BANDExpr { $1 }
      
BANDExpr : /* left association */
    BANDExpr BOOLAND LTExpr { BinOp (Band, $1, $3) }
  | LTExpr { $1 }

LTExpr : 
    PExpr LT PExpr { BinOp (Lt, $1, $3) }
  | PExpr { $1 }

PExpr :
    PExpr PLUS MExpr { BinOp (Plus, $1, $3) }
  | MExpr { $1 }

MExpr : 
    MExpr MULT AppExpr { BinOp (Mult, $1, $3) }
  | AppExpr { $1 }

AppExpr :
    AppExpr AExpr { AppExp ($1, $2) }
  | AExpr { $1 }
      
AExpr :
    INTV { ILit $1 }
  | TRUE { BLit true }
  | FALSE { BLit false }
  | ID { Var $1 }
  | LPAREN Expr RPAREN { $2 }

IfExpr :
    IF Expr THEN Expr ELSE Expr { IfExp ($2, $4, $6) }
