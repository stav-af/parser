open Ast

module StringMap = Map.Make(String)


let rec eval_expr (mmap: int StringMap.t) (e: exp) =
  match e with
  | EXPR(op, e1, e2) -> (
    let v1 = eval_expr mmap e1 in
    let v2 = eval_expr mmap e2 in
    match op with
    | ADD -> v1 + v2
    | SUB -> v1 - v2
    | DIV -> v1 / v2
    | MULT -> v1 * v2
    | MOD -> v1 mod v2)
  | VAR(ident) -> StringMap.find ident mmap
  | VAL(n) -> n

let rec eval_bexp (mmap: int StringMap.t) (b: bExp) =
  match b with
  | TRUE -> true
  | FALSE -> false
  | COMP(op, e1, e2) -> (
    let v1 = eval_expr mmap e1 in
    let v2 = eval_expr  mmap e2 in
    match op with
    | GE -> v1 >= v2
    | LE -> v1 <= v2
    | GT -> v1 > v2
    | LT -> v1 < v2
    | EQ -> v1 = v2
    | NE -> v1 != v2)
  | BEXP(op, bex1, bex2) -> (
    let b1 = eval_bexp mmap bex1 in
    let b2 = eval_bexp mmap bex2 in
    match op with
    | CONJ -> b1 && b2
    | DISJ -> b1 || b2
    | BEQ -> b1 = b2
    | BNE -> b1 != b2)

and eval_stmt (mmap: int StringMap.t) (statement: stmt) =
  match statement with
 | SKIP -> mmap
 | SEQ_STMT(s1, s2) -> let mmap2 = eval_stmt mmap s1 in eval_stmt mmap2 s2
 | ASSIGN(ident, exp) -> let e = eval_expr mmap exp in StringMap.add ident e mmap
 | IF(bxp, s1, s2) -> if eval_bexp mmap bxp then eval_stmt mmap s1 else eval_stmt mmap s2
 | WHILE(bxp, s1) -> if eval_bexp mmap bxp then (eval_stmt mmap (SEQ_STMT(s1, WHILE(bxp, s1)))) else mmap
 | READ(ident) -> let inp = read_line() in (eval_stmt mmap (ASSIGN(ident, VAL(int_of_string inp))))
 | WRITE(e1) -> (Printf.printf "%d\n" (eval_expr mmap e1)); mmap


let eval(st: stmt) =
  let mmap = StringMap.empty in
  eval_stmt mmap st