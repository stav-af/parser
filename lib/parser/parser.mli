open Ast

type tokens = (string*string) list
type 'a parser = tokens -> ('a * tokens) list

val ex: aexp parser
val te: aexp parser
val fi: aexp parser 
val bf: bexp parser
val bex: bexp parser
val stmt: stmt parser
val comp_stmt: stmt parser
val block: stmt parser