open Core.Ast
open Core.Parser
open Core.Eval

(* open Lexer.Formatter *)

open Codegen

open Defs.While
open Lexer.Lex

let rec display_exp = function
  | EXPR (op, e1, e2) ->
      let op_str = match op with
        | ADD -> "+"
        | SUB -> "-"
        | MULT -> "*"
        | DIV -> "/"
        | MOD -> "%"
      in
      Printf.sprintf "(%s %s %s)" (display_exp e1) op_str (display_exp e2)
  | VAR id -> id
  | VAL v -> string_of_int v

let rec display_bExp = function
  | TRUE -> "true"
  | FALSE -> "false"
  | COMP (comp, e1, e2) ->
      let comp_str = match comp with
        | GT -> ">"
        | LT -> "<"
        | GE -> ">="
        | LE -> "<="
        | EQ -> "=="
        | NE -> "!="
      in
      Printf.sprintf "(%s %s %s)" (display_exp e1) comp_str (display_exp e2)
  | BEXP (op, b1, b2) ->
      let op_str = match op with
        | CONJ -> "&&"
        | DISJ -> "||"
        | BEQ -> "=="
        | BNE -> "!="
      in
      Printf.sprintf "(%s %s %s)" (display_bExp b1) op_str (display_bExp b2)

and display_stmt = function
  | SKIP -> "skip"
  | SEQ_STMT (s1, s2) -> Printf.sprintf "%s; %s" (display_stmt s1) (display_stmt s2)
  | ASSIGN (id, exp) -> Printf.sprintf "%s := %s" id (display_exp exp)
  | IF (cond, s1, s2) -> Printf.sprintf "if %s then %s else %s" (display_bExp cond) (display_stmt s1) (display_stmt s2)
  | WHILE (cond, s1) -> Printf.sprintf "while %s do %s" (display_bExp cond) (display_stmt s1)
  | READ id -> Printf.sprintf "read(%s)" id
  | WRITE_VAR e1 -> Printf.sprintf "write(%s)" e1
  | WRITE_STR e1 -> Printf.sprintf "write(%s)" e1

let print_string_map (map: int StringMap.t) =
  StringMap.iter (fun key value ->
    Printf.printf "%s -> %d\n" key value
  ) map

(* let display_sc = function
  | SC -> "SC"

let display_null = function
  | NONE -> "NONE" *)

let fst (a1, _) = a1

let () =
  let ic = open_in "./lib/while_programs/collatz_repeated.while" in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  let prog = lexing_simp s while_toks in
  let asts = _comp_stmt prog in
  let ast = (fst (List.find  (fun (_, rest) -> rest = []) asts)) in

  let class_name = "test" in
  let test_dir = "out" in

  let code = compile ast class_name in

  let j_file = Printf.sprintf "%s/%s.j" test_dir class_name in
  let oc = open_out j_file in
  output_string oc code;
  close_out oc;

  let cmd_jasmin = Printf.sprintf "java -jar jasmin.jar -d %s %s" test_dir j_file in
  ignore (Sys.command cmd_jasmin);

  let cmd_java = Printf.sprintf "java -cp %s %s/%s" test_dir class_name class_name in
  ignore (Sys.command cmd_java);

  ()