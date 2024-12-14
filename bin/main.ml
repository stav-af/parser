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
  | FOR (i, lb, ub, stmt) -> Printf.sprintf "for %s = %s upto %s do {%s}" i (display_exp lb) (display_exp ub) (display_stmt stmt)

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
  let dir = "./lib/while_programs" in
  let files = Sys.readdir dir in

  Array.iter (fun file ->
    if Filename.check_suffix file ".while" then begin
      let filepath = Filename.concat dir file in

      let ic = open_in filepath in
      let n = in_channel_length ic in
      let s = really_input_string ic n in
      close_in ic;

      let prog = lexing_simp s while_toks in
      let asts = _comp_stmt prog in
      let ast = 
      match List.find_opt (fun (_, rest) -> rest = []) asts with
        | Some (a, _) -> a
        | None -> failwith "Error: No valid AST found with an empty rest" in

      let class_name = Filename.remove_extension file in

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
    end
  ) files