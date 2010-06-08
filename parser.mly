%{
open Syntax

let to_funexp' ids exp f =
  List.fold_left (fun acc id -> f id acc) exp (List.rev ids)
let to_funexp ids exp = to_funexp' ids exp (fun id acc -> FunExp (id, acc))
let to_dfunexp ids exp = to_funexp' ids exp (fun id acc -> DFunExp (id, acc))
  
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT BOOLAND BOOLOR
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ AND
%token RARROW FUN DFUN REC
%token LSQBRA RSQBRA COLON2 SEMI
%token MATCH WITH PIPE

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    Decl SEMISEMI { $1 }

Decl: 
    Expr    { Exp $1 }
  | LetDecl { Decl $1 }
      
LetDecl :
  /* let-decl */
    Let      { let x, y = $1 in (LetBlock (x, y)) }
  | Let LetDecl { let x, y = $1 in (LetBlockSeq (x, y, $2)) }
  /* let rec-decl */
  | LetRec { let x, y, z = $1 in (LetRecBlock (x, y, z)) }
  | LetRec LetDecl { let x, y, z = $1 in (LetRecBlockSeq (x, y, z, $2)) }  
      
Let :
    LET Letsub { $2 }
Letsub :
    ID EQ Expr { [$1], [$3] }
  | ID EQ Expr AND Letsub { let ids, exps = $5 in $1::ids, $3::exps }
  | ID IDs EQ Expr { [$1], [to_funexp $2 $4] }
  | ID IDs EQ Expr AND Letsub { let ids, exps = $6 in $1::ids, (to_funexp $2 $4)::exps }

LetRec :
    LET REC LetRecsub { $3 }
LetRecsub :
    ID EQ FUN ID RARROW Expr { [$1], [$4], [$6] }
  | ID EQ FUN ID RARROW Expr AND LetRecsub { let ids, paras, exps = $8 in $1::ids, $4::paras, $6::exps }
  | ID IDs EQ Expr { let h, t = List.hd $2, List.tl $2 in [$1], [h], [to_funexp t $4] }
  | ID IDs EQ Expr AND LetRecsub { let ids, paras, exps = $6 in let h, t = List.hd $2, List.tl $2 in
                                   $1::ids, h::paras, (to_funexp t $4)::exps }

Expr :
    IfExpr { $1 }
  | LetExpr { $1 }
  | BORExpr { $1 }
  | FunExpr { $1 }
  | LetRecExpr { $1 }
  | MatchExpr { $1 }

/* let */
LetExpr :
    Let IN Expr { let x, y = $1 in LetExp (x, y, $3) }
LetRecExpr :
    LetRec IN Expr { let x, y, z = $1 in LetRecExp (x, y, z, $3) }

/* function */
FunExpr :
    FUN IDs RARROW Expr { to_funexp $2 $4 }
  | DFUN IDs RARROW Expr { to_dfunexp $2 $4 }
      
IDs :
    ID { [$1] }
  | ID IDs { $1::$2 }

/* match */
MatchExpr :
    MATCH Expr WITH LSQBRA RSQBRA RARROW Expr PIPE ID COLON2 ID RARROW Expr
    { if $9 = $11 then raise (Syntax.Parse_error "Cannot use same name between a head variable and a tail one.")
      else MatchExp ($2, $7, $9, $11, $13) }
      
/* basic expression */
BORExpr :  /* left association */
    BORExpr BOOLOR BANDExpr { BinOp (Bor, $1, $3) }
  | BANDExpr { $1 }
      
BANDExpr : /* left association */
    BANDExpr BOOLAND LTExpr { BinOp (Band, $1, $3) }
  | LTExpr { $1 }

LTExpr : 
    ListExpr LT ListExpr { BinOp (Lt, $1, $3) }
  | ListExpr { $1 }

ListExpr :
    PExpr COLON2 ListExpr { BinOp (Cons, $1, $3) }
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
  | LSQBRA RSQBRA { LLit [] }
  | LSQBRA ExpList RSQBRA { LLit $2 }
  | LPAREN Expr RPAREN { $2 }

ExpList :
    Expr { [$1] }
  | Expr SEMI ExpList { $1::$3 }
      
IfExpr :
    IF Expr THEN Expr ELSE Expr { IfExp ($2, $4, $6) }
