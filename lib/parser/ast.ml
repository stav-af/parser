type ident = string

type bComp = 
 | GT | LT | GE | LE | EQ | NE 

type bOp = 
 | CONJ | DISJ | BEQ | BNE

type aOp = 
 | SUB
 | ADD
 | MULT
 | DIV
 | MOD

type exp = 
 | EXPR of aOp * exp * exp
 | VAR of ident
 | VAL of int

type bExp =
 | TRUE | FALSE 
 | COMP of bComp * exp * exp 
 | BEXP of bOp * bExp * bExp

type stmt = 
 | SKIP
 | SEQ_STMT of stmt * stmt
 | ASSIGN of ident * exp
 | IF of bExp * stmt * stmt
 | WHILE of bExp * stmt
 | READ of ident
 | WRITE of exp


(* fix this, should just be a statement *)
type sc = 
 | SC

type null = 
 | NONE