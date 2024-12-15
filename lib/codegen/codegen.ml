open Core.Ast

open Prelude
module Env = Map.Make(String)

let label_counter = ref 0
let new_label (s: string) =
  incr label_counter;
  Printf.sprintf "%s_%n" s !label_counter

let fmtl l = 
  Printf.sprintf "%s:\n" l

let fmt (operator) (operand) =
  Printf.sprintf "\t%s %s\n" operator operand

let map_size map_list =
  List.fold_left (fun acc map -> acc + Env.cardinal map) 0 map_list

let c_bop (op: bcomp): string = 
  let op_str = match op with
  | EQ -> "ne "
  | NE -> "eq "
  | GT -> "le "
  | LT -> "ge "
  | GE -> "lt "
  | LE -> "gt " in
  "\tif_icmp" ^ op_str

let c_aop (a: aop) =
  match a with
  | SUB -> "\tisub\n"
  | ADD -> "\tiadd\n"
  | MULT -> "\timul\n"
  | DIV -> "\tidiv\n"
  | MOD -> "\tirem\n"


let rec find_var id envl =
  match envl with 
  | [] -> failwith (Printf.sprintf "Tried to find %s but env was empty" id)
  | x::xs -> if Env.mem id x then Env.find id x else find_var id xs


(* Assign and decl are slightly different; 

Decl is for initializing a new variable, or overwrite if in same scope
Assign will reassign a variable instead of overwriting

Decl is can be used to reuse iterator variable in for-loops
Assign is more appropriate for the body of loops
  *)


let assign_var id envl =
  if List.exists (fun e -> Env.mem id e) envl then
    let env = List.find (fun e -> Env.mem id e) envl in
    (Env.find id env, envl)
  else
    let head = List.hd envl in
    let new_var = string_of_int (map_size envl) in
    let env' = Env.add id new_var head in
    (new_var, (env'::List.tl envl))

let decl_var id envl =
  let head = List.hd envl in 
  if Env.mem id head then
    (Env.find id head, envl)
  else 
    let new_var = string_of_int (map_size envl) in
    let env' = Env.add id new_var head in
    (new_var, (env'::(List.tl envl)))

let new_scope envl = 
  let env = Env.empty in 
  (env::envl)

let rec c_stmt (st: stmt) (env: string Env.t list) (l_brk: string): string * string Env.t list = 
  match st with 
  | SKIP -> "", env
  | BREAK -> fmt "goto" l_brk, env
  | SEQ_STMT(st1, st2) -> 
    let (instr1, env1) = c_stmt st1 env l_brk in
    let (instr2, env2) = c_stmt st2 env1 l_brk in
      (instr1 ^ instr2, env2)
  | IF(b, s1, s2) -> 
    let ifelse = new_label "Ifelse" in
    let endif = new_label "Endif" in 
    let (cs1, env1) = c_stmt s1 env l_brk in
    let (cs2, env2) = c_stmt s2 env1 l_brk in
      (c_bexp b env) ^
      (fmt "ifeq" ifelse) ^(* if our bexp evaluated to false, jump to else *)
      (cs1) ^
      (fmt "goto" endif) ^ (* we executed the 'then' branch, jump to end*)
      (fmtl ifelse) ^
      (cs2) ^
      (fmtl endif), env2
  | WHILE(b, s1) ->
    let l_whl = new_label "Startwhile" in
    let l_end = new_label "Endwhile" in
    let env' = new_scope env in
    let (cs1, env'') = c_stmt s1 env' l_end in
      (fmtl l_whl) ^
      (c_bexp b env) ^
      (fmt "ifeq" l_end) ^ (* if our bexp resolved to 0, break *)
      (cs1) ^
      (fmt "goto" l_whl) ^
      (fmtl l_end), (List.tl env'')
  | FOR (i, lb, ub, bl) ->
    let l_for = new_label "Startfor" in 
    let l_end = new_label "Endfor" in
    let env' = new_scope env in
    let (idx, env'') = decl_var i env' in 
    let c_lb = c_aexp lb env'' in
    let c_ub = c_aexp ub env'' in 
    let (c_bl, env''') = c_stmt bl env'' l_end in
      (c_lb) ^
      (fmt "istore" idx) ^
      (fmtl l_for) ^
      (fmt "iload" idx) ^
      (c_ub) ^
      (fmt "if_icmpgt" l_end) ^
      (c_bl) ^
      (fmt "iload" idx) ^
      (fmt "ldc" "1") ^
      "\tiadd\n" ^
      (fmt "istore" idx) ^
      (fmt "goto" l_for) ^
      (fmtl l_end), (List.tl env''')
  | ASSIGN(id, e1) ->
    let ce1 = c_aexp e1 env in
    let (idx, env1) = assign_var id env in
      (ce1) ^ 
      (fmt "istore" idx), env1
  | READ(id) ->
    let (idx, env') = assign_var id env in
      "\tinvokestatic XXX/XXX/read()I\n" ^
      (fmt "istore" idx), env'
  | WRITE_VAR(id) -> 
    let idx = find_var id env in
      (fmt "iload" idx) ^
      "\tinvokestatic XXX/XXX/write(I)V\n", env
  | WRITE_STR(str) -> 
      (fmt "ldc" str) ^
      "\tinvokestatic XXX/XXX/writes(Ljava/lang/String;)V\n", env
  (* | _ -> failwith "Not implemented" *)


and c_bexp (bex: bexp) (env: string Env.t list) : string = 
    match bex with
    | TRUE -> "\ticonst_1\n"
    | FALSE -> "\ticonst_0\n"
    | COMP(op, e1, e2) ->
      let instr1 = c_aexp e1 env in
      let instr2 = c_aexp e2 env in
      let comp_f = new_label "Comp_f" in
      let comp_end = new_label "Comp_end" in
        instr1 ^ 
        instr2 ^ 
        ((c_bop op) ^ comp_f) ^
        "\n\tldc 1\n" ^
        (fmt "goto" comp_end) ^
        (fmtl comp_f) ^
        ("\tldc 0\n") ^  
        (fmtl comp_end)
    | BEXP(bop, bexp1, bexp2) -> 
      begin
        match bop with
        | CONJ ->
          let l_false = new_label "Conj_false" in
          let l_end = new_label "Conj_end" in
          (c_bexp bexp1 env) ^ 
          (fmt "ifeq" l_false) ^ (* if first term is false, shortcut, push 0 and exit *)
          (c_bexp bexp2 env) ^ 
          (fmt "goto" l_end) ^   (* bexp1 was true, so result is result of bexp2 *)
          (fmtl l_false) ^       (* restate 0 and exit *)
          ("\ticonst_0\n") ^
          (fmtl l_end)
        | DISJ ->
          let l_true = new_label "Disj_true" in
          let l_end = new_label "Disj_end" in
          (c_bexp bexp1 env) ^
          (fmt "ifne" l_true) ^
          (c_bexp bexp2 env) ^
          (fmt "goto" l_end) ^
          (fmtl l_true) ^
          ("\tldc 1\n") ^
          (fmtl l_end)
        | BEQ -> 
          (c_bexp bexp1 env) ^
          (c_bexp bexp2 env) ^
          "\teq\n"
        | BNE ->
          (c_bexp bexp1 env) ^
          (c_bexp bexp2 env) ^
          "\tneq\n"
      end
  


and c_aexp (exp : aexp) (env: string Env.t list) : string =
  match exp with
  | VAL(n) -> (fmt "ldc" (string_of_int n))
  | VAR(id) -> 
    let loc = find_var id env in
    (fmt "iload" loc)
  | EXPR(aop, e1, e2) ->
    let instr1 = c_aexp e1 env in
    let instr2 = c_aexp e2 env in
      instr1 ^ instr2 ^ (c_aop aop)


let f_write str f_name = 
  let oc = open_out f_name in
  output_string oc str;
  close_out oc


let compile sstmt class_name = 
  let env = [Env.empty] in
  let l_brk = "End_program" in
  let (instr, _) = (c_stmt sstmt env l_brk) in
  let prog =
    Printf.sprintf "%s\n%s\n%s:\n\treturn\n.end method" 
      prelude
      instr
      l_brk in
  
  let pattern = Re.Perl.compile_pat "XXX" in
  Re.replace_string ~all:true ~by:class_name pattern prog