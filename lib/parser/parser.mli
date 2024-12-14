open Ast

type tokens = (string*string) list
type 'a parser = tokens -> ('a * tokens) list

val _aexp: aexp parser
val _aterm: aexp parser
val _afinal: aexp parser 
val _bfinal: bexp parser
val _bexp: bexp parser
val _stmt: stmt parser
val _comp_stmt: stmt parser
val _block: stmt parser