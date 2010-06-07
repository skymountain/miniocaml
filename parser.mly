%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT BOOLAND BOOLOR
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    Expr SEMISEMI { Exp $1 }
  | Decl SEMISEMI { Decl $1 }
      
Decl :
    LET ID EQ Expr { LetLast ($2, $4) }
  | LET ID EQ Expr Decl     { LetSeq ($2, $4, $5) }

Expr :
    IfExpr { $1 }
  | LetExpr { $1 }
  | BORExpr { $1 }

LetExpr :
    LET ID EQ Expr IN Expr { LetExp ($2, $4, $6) }
      
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
    MExpr MULT AExpr { BinOp (Mult, $1, $3) }
  | AExpr { $1 }

AExpr :
    INTV { ILit $1 }
  | TRUE { BLit true }
  | FALSE { BLit false }
  | ID { Var $1 }
  | LPAREN Expr RPAREN { $2 }

IfExpr :
    IF Expr THEN Expr ELSE Expr { IfExp ($2, $4, $6) }
