%{
open Syntax

let to_funexp' xs exp f =
  List.fold_left (fun acc x -> f x acc) exp (List.rev xs)
let to_funexp x exp = to_funexp' x exp (fun (id, paraty) acc -> FunExp (id, paraty, acc))
let to_dfunexp ids exp = to_funexp' ids exp (fun id acc -> DFunExp (id, acc))
let to_const =
  function
    ILit i  -> CInt i
  | BLit b  -> CBool b
  | LLit [] -> CNull
  | _       -> assert false

let fexp_of_biop biop =
  FunExp ("x", None, FunExp ("y", None, BinOp (biop, (Var "x"), (Var "y"))))
    
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT BOOLAND BOOLOR
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ AND
%token RARROW FUN DFUN REC
%token LSQBRA RSQBRA COLON2 SEMI
%token MATCH WITH PIPE UNDERBAR AS
%token COLON LIST INT BOOL
%token EOF

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%start toplevel_batch
%type <Syntax.program> toplevel_batch

%%

toplevel :
    Expr SEMISEMI { Exp $1 }
  | LetDecl SEMISEMI { Decl $1 }
/*      
    Expr SEMISEMI { print_endline "toplevel"; Exp $1 }
  | LetDecl SEMISEMI { print_endline "toplevel"; Decl $1 }
*/      
toplevel_batch :
    toplevel EOF { $1 }
  | toplevel toplevel_batch { Seq ($1, $2) }
/*
Decl: 
    Expr    { Exp $1 }
  | LetDecl { Decl $1 }
/*      
    Expr    { print_endline "Expr"; Exp $1 }
  | LetDecl { print_endline "LetDecl"; Decl $1 }
*/      
LetDecl :
  /* let-decl */
    Let            { let x, y, z = $1 in (LetBlock (x, y, z)) }
  | Let LetDecl    { let x, y, z = $1 in (LetBlockSeq (x, y, z, $2)) }
/*
    Let            { print_endline "Let"; let x, y = $1 in (LetBlock (x, y)) }
  | Let LetDecl    { print_endline "Let LetDecl"; let x, y = $1 in (LetBlockSeq (x, y, $2)) }
*/
  /* let rec-decl */
  | LetRec         { let x, y, z, a, b = $1 in (LetRecBlock (x, y, z, a, b)) }
  | LetRec LetDecl { let x, y, z, a, b = $1 in (LetRecBlockSeq (x, y, z, a, b, $2)) }  
      
Let :
    LET Letsub { $2 }
/*
    LET Letsub { Printf.printf "1->s:%i,e:%i\n" (Parsing.symbol_start ()) (Parsing.symbol_end ()); $2 }
*/
Letsub :
    ID TypedEQ Expr { [$1], [$3], [$2] }
  | ID TypedEQ Expr AND Letsub { let ids, exps, retsigs = $5 in $1::ids, $3::exps, $2::retsigs }
  | ID Paras TypedEQ Expr { [$1], [to_funexp $2 $4], [$3] }
  | ID Paras TypedEQ Expr AND Letsub { let ids, exps, retsigs = $6 in $1::ids, (to_funexp $2 $4)::exps, $3::retsigs }
/*
    ID EQ Expr { Printf.printf "2->s:%i,e:%i\n" (Parsing.symbol_start ()) (Parsing.symbol_end ()); [$1], [$3] }
*/

LetRec :
    LET REC LetRecsub { $3 }
LetRecsub :
ID TypedEQ FUN Para RARROW Expr { let para, paraty = $4 in [$1], [para], [paraty], [$6], [$2] }
  | ID TypedEQ FUN Para RARROW Expr AND LetRecsub { let ids, paras, paratys, exps, retsigs = $8 in
                                                    let para, paraty = $4 in
                                                    $1::ids, para::paras, paraty::paratys, $6::exps, $2::retsigs }
  | ID Paras TypedEQ Expr { let h, t = List.hd $2, List.tl $2 in
                            let para, paraty = h in
                            [$1], [para], [paraty], [to_funexp t $4], [$3] }
  | ID Paras TypedEQ Expr AND LetRecsub { let ids, paras, paratys, exps, retsigs = $6 in
                                        let h, t = List.hd $2, List.tl $2 in
                                        let para, paraty = h in
                                        $1::ids, para::paras, paraty::paratys, (to_funexp t $4)::exps, $3::retsigs }

TypedEQ :
    EQ { None }
  | COLON TypedExpr EQ { Some $2 }
      
Expr :
    BORExpr { $1 }
      
/* let */
LetExpr :
    Let IN Expr { let x, y, z = $1 in LetExp (x, y, z, $3) }
/*
    Let IN Expr { print_endline "Let Expr"; let x, y = $1 in LetExp (x, y, $3) }
*/
LetRecExpr :
    LetRec IN Expr { let x, y, z, a, b = $1 in LetRecExp (x, y, z, a, b, $3) }

/* function */
FunExpr :
    FUN Paras RARROW Expr { to_funexp $2 $4 }
  | DFUN IDs RARROW Expr { to_dfunexp $2 $4 }

Para :
    ID { $1, None }
  | LPAREN ID COLON TypedExpr RPAREN { $2, Some $4 }
Paras :
    Para { [$1] }
  | Para Paras { $1::$2 }
      
IDs :
    ID { [$1] }
  | ID IDs { $1::$2 }

/* match */
MatchExpr :
    MATCH Expr WITH PatternSeq { MatchExp ($2, $4) }

PatternSeq :
    Pattern RARROW Expr { [$1, $3] }
  | Pattern RARROW Expr PIPE PatternSeq { ($1, $3)::$5 }
      
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
  | SExpr { $1 } 
  | AExpr { $1 }
      
AExpr :
    Constant { $1 }
  | ID { Var $1 }
  | LSQBRA ExpList RSQBRA { LLit $2 }
  | LPAREN Expr RPAREN { $2 }
  | LPAREN Expr COLON TypedExpr RPAREN { TypedExp ($2, $4) }
  | LPAREN Biop RPAREN { fexp_of_biop $2 }

Constant :
    INTV { ILit $1 }
  | TRUE { BLit true }
  | FALSE { BLit false }
  | LSQBRA RSQBRA { LLit [] }
      
SExpr :
    IfExpr { $1 }
  | LetExpr { $1 }
  | FunExpr { $1 }
  | LetRecExpr { $1 }
  | MatchExpr { $1 }
      
ExpList :
    Expr { [$1] }
  | Expr SEMI ExpList { $1::$3 }
      
IfExpr :
    IF Expr THEN Expr ELSE Expr { IfExp ($2, $4, $6) }

/* pattern */
Pattern :
    OrPattern { $1 }
  | Pattern AS ID { As ($1, $3) }

OrPattern :
    OrPattern PIPE ConsPattern { Or ($1, $3) }
  | ConsPattern { $1 }

ConsPattern :
    APattern COLON2 ConsPattern { Conspat ($1, $3) }
  | APattern { $1 }
      
APattern :
    UNDERBAR { Wildcard }
  | Constant { Const (to_const $1) }
  | LPAREN Pattern RPAREN { $2 }
  | LSQBRA ListPattern RSQBRA { Lpat $2 }
  | LSQBRA RSQBRA { Lpat [] }
  | ID { Varpat $1 }
      
ListPattern :
    Pattern { [$1] }
  | Pattern SEMI ListPattern { $1::$3 }

/* type expression */
TypedExpr :
    ListTypeExpr RARROW TypedExpr { TyFun ($1, $3) }
  | ListTypeExpr { $1 }
      
ListTypeExpr :
    ATypeExpr LIST { TyList $1 }
  | ATypeExpr { $1 }
      
ATypeExpr :
    INT { TyInt }
  | BOOL { TyBool }
  | LPAREN TypedExpr RPAREN { $2 }

/* Binary operation */
Biop :
    PLUS { Plus }
  | MULT { Mult }
  | LT   { Lt }
  | BOOLAND { Band }
  | BOOLOR { Bor }
  | COLON2 { Cons }
