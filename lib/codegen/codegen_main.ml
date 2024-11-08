open Core.Ast
module Env = Map.Make(String)


let label_counter = ref 0

let new_label (s: string) =
  incr label_counter;
  Printf.sprintf "%n_%s:\n" !label_counter s

let map_size map =
  Env.fold (fun _ _ acc -> acc + 1) map 0

let c_bop (op: bcomp) (jlabel: string): string = 
  let op_str = match op with
  | EQ -> "ne "
  | NE -> "eq "
  | GT -> "le "
  | LT -> "ge "
  | GE -> "lt "
  | LE -> "gt " in
  "if_cmp" ^ op_str ^ jlabel

let c_aop (a: aop) =
  match a with
  | SUB -> "isub\n"
  | ADD -> "iadd\n"
  | MULT -> "imul\n"
  | DIV -> "idiv\n"
  | MOD -> "irem\n"


let rec c_stmt (st: stmt) (env: int Env.t): string * int Env.t = 
  match st with 
  | SKIP -> "", env
  | SEQ_STMT(st1, st2) -> 
    let (instr1, env1) = c_stmt st1 env in
    let (instr2, env2) = c_stmt st2 env1 in
    (instr1 ^ instr2, env2)
  | IF(COMP(b, e1, e2), s1, s2) -> 
    let ifelse = new_label "ifelse" in
    let endif = new_label "endif" in 
    let (cs1, env1) = c_stmt s1 env in
    let (cs2, env2) = c_stmt s2 env1 in
      (c_bexp (COMP(b, e1, e2)) env ifelse) ^
      (cs1) ^
      (Printf.sprintf "goto %s\n" endif) ^
      (ifelse) ^
      (cs2) ^
      (endif), env2
  | WHILE(COMP(b, e1, e2), s1) ->
    let (cs1, env1) = c_stmt s1 env in
    let l_whl = new_label "startwhile" in
    let l_brk = new_label "endwhile" in
      (l_whl) ^
      (c_bexp (COMP(b, e1, e2)) env l_brk) ^
      (cs1) ^
      (Printf.sprintf "goto %s" l_whl) ^
      (l_brk), env1
  | ASSIGN(id, e1) ->
    let ce1 = c_aexp e1 env in
    let idx = map_size env in
    let env1 = Env.add id idx env in 
    (ce1) ^
    (Printf.sprintf "istore %n\n" idx), env1
  | _ -> failwith "ERROR: Not implemented"


and c_bexp (bex: bexp) (env: int Env.t) (jlabel: string): string = 
  match bex with
  | TRUE -> ""
  | FALSE -> Printf.sprintf "goto %s\n" jlabel
  | COMP(op, e1, e2) ->
    let instr1 = c_aexp e1 env in
    let instr2 = c_aexp e2 env in
      instr1 ^ instr2 ^ (c_bop op jlabel)
  | _ -> failwith "ERROR: Compilation of binary expresssions is not yet supported.\n"

and c_aexp (exp : aexp) (env: int Env.t) : string =
  match exp with
  | VAL(n) -> Printf.sprintf "ldc %n\n" n
  | VAR(id) -> 
    let loc = Env.find id env in
    Printf.sprintf "iload %n\n" loc
  | EXPR(aop, e1, e2) ->
    let instr1 = c_aexp e1 env in
    let instr2 = c_aexp e2 env in
      instr1 ^ instr2 ^ (c_aop aop)

let () = 
  let env = Env.empty in
  let cond = COMP(GE, EXPR(ADD, VAL(1), VAL(2)), EXPR(SUB, VAL(10), VAL(4))) in
  let s1 = WHILE(cond, ASSIGN("y", VAL(2))) in
  let (prog, _) = (c_stmt s1 env) in
  print_endline prog