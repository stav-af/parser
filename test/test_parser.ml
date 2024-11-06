open Core.Ast
open Core.Parser
(* Pretty-print functions for the given types *)

(* Pretty-print function for 'ident' type *)
(* Pretty-print function for 'bComp' type *)
(* let string_of_ident = function
  | VARNAME name -> "VARNAME(" ^ name ^ ")" *)

(* let string_of_bComp = function
  | GT -> ">"
  | LT -> "<"
  | GE -> ">="
  | LE -> "<="
  | EQ -> "=="
  | NE -> "!=" *)

(* Pretty-print function for 'bOp' type *)
(* let string_of_bOp = function
  | CONJ -> "&&"
  | DISJ -> "||"
  | BEQ -> "=="
  | BNE -> "!=" *)

(* Pretty-print function for 'aOp' type *)
let string_of_aOp = function
  | SUB -> "-"
  | ADD -> "+"
  | MULT -> "*"
  | DIV -> "/"
  | MOD -> "%"

(* Pretty-print function for 'exp' type *)
let rec string_of_exp = function
  | EXPR (op, left, right) ->
      "(" ^ (string_of_exp left) ^ " " ^ (string_of_aOp op) ^ " " ^ (string_of_exp right) ^ ")"
  | VAR name -> name
  | VAL n -> string_of_int n

(* Pretty-print function for 'bExp' type
let rec string_of_bExp = function
  | TRUE -> "true"
  | FALSE -> "false"
  | VAR name -> name
  | COMP (comp, left, right) ->
      "(" ^ (string_of_exp left) ^ " " ^ (string_of_bComp comp) ^ " " ^ (string_of_exp right) ^ ")"
  | BEXP (op, left, right) ->
      "(" ^ (string_of_bExp left) ^ " " ^ (string_of_bOp op) ^ " " ^ (string_of_bExp right) ^ ")" *)

(* Pretty-print function for 'stmt' type *)
(* let rec string_of_stmt = function
  | SKIP -> "skip"
  | SEQ_STMT (stmt1, stmt2) ->
      (string_of_stmt stmt1) ^ "; " ^ (string_of_stmt stmt2)
  | ASSIGN (id, exp) ->
      (string_of_ident id) ^ " := " ^ (string_of_exp exp)
  | IF (cond, stmt1, stmt2) ->
      "if (" ^ (string_of_bExp cond) ^ ") then { " ^ (string_of_stmt stmt1) ^ " } else { " ^ (string_of_stmt stmt2) ^ " }"
  | WHILE (cond, body) ->
      "while (" ^ (string_of_bExp cond) ^ ") do { " ^ (string_of_stmt body) ^ " }"
  | READ id -> "read " ^ (string_of_ident id)
  | WRITE id -> "write " ^ (string_of_ident id)

(* Pretty-print function for 'sc' type *)
let string_of_sc = function
  | SC -> "SC"

(* Pretty-print function for 'null' type *)
let string_of_null = function
  | NONE -> "none" *)

let test_expr =[
  ("n", "1");
  ("o", "+");
  ("n", "2")
]

  
let () =
  match (ex test_expr) with
  | (parsed_exp, _) :: _ -> Printf.printf "%s\n" (string_of_exp parsed_exp)
  | [] -> Printf.printf "No valid parse result\n"
