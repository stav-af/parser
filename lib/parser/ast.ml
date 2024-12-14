type ident = string

type delim =
  | NONE

type bcomp = 
 | GT | LT | GE | LE | EQ | NE 

type bop = 
 | CONJ | DISJ | BEQ | BNE

type aop = 
 | SUB
 | ADD
 | MULT
 | DIV
 | MOD

type aexp = 
 | EXPR of aop * aexp * aexp
 | VAR of ident
 | VAL of int

type bexp =
 | TRUE | FALSE 
 | COMP of bcomp * aexp * aexp 
 | BEXP of bop * bexp * bexp

type stmt = 
 | SKIP
 | SEQ_STMT of stmt * stmt
 | ASSIGN of ident * aexp
 | IF of bexp * stmt * stmt
 | WHILE of bexp * stmt
 | READ of ident
 | WRITE_STR of string
 | WRITE_VAR of string
