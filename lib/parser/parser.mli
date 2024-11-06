open Ast

type tokens = (string*string) list
type 'a parser = tokens -> ('a * tokens) list

val ex: exp parser
val te: exp parser
val fi: exp parser 
val bf: bExp parser
val bex: bExp parser
val stmt: stmt parser
val comp_stmt: stmt parser
val block: stmt parser