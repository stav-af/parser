open Core.Ast
module Env = Map.Make(String)


let label_counter = ref 0

let new_label (s: string) =
  incr label_counter;
  Printf.sprintf "%n_%s" !label_counter s

let fmtl l = 
  Printf.sprintf "%s:\n" l

let fmt (operator) (operand) =
  Printf.sprintf "\t%s %s\n" operator operand


let map_size map =
  Env.fold (fun _ _ acc -> acc + 1) map 0

let c_bop (op: bcomp): string = 
  let op_str = match op with
  | EQ -> "ne "
  | NE -> "eq "
  | GT -> "le "
  | LT -> "ge "
  | GE -> "lt "
  | LE -> "gt " in
  "\tif_cmp" ^ op_str ^ "\n"

let c_aop (a: aop) =
  match a with
  | SUB -> "\tisub\n"
  | ADD -> "\tiadd\n"
  | MULT -> "\timul\n"
  | DIV -> "\tidiv\n"
  | MOD -> "\tirem\n"


let rec c_stmt (st: stmt) (env: int Env.t): string * int Env.t = 
  match st with 
  | SKIP -> "", env
  | SEQ_STMT(st1, st2) -> 
    let (instr1, env1) = c_stmt st1 env in
    let (instr2, env2) = c_stmt st2 env1 in
    (instr1 ^ instr2, env2)
  | IF(b, s1, s2) -> 
    let ifelse = new_label "ifelse" in
    let endif = new_label "endif" in 
    let (cs1, env1) = c_stmt s1 env in
    let (cs2, env2) = c_stmt s2 env1 in
      (c_bexp b env) ^
      (fmt "ifeq" ifelse) (* if our bexp evaluated to false, jump to else *)
      (cs1) ^
      (fmt "goto" endif) ^ (* we executed the 'then' branch, jump to end*)
      (fmtl ifelse) ^
      (cs2) ^
      (fmtl endif), env2
  | WHILE(b, s1) ->
    let (cs1, env1) = c_stmt s1 env in
    let l_whl = new_label "startwhile" in
    let l_brk = new_label "endwhile" in
      (fmtl l_whl) ^
      (c_bexp b env) ^
      (fmt "ifeq" l_brk) ^ (* if our bexp resolved to 0, break *)
      (cs1) ^
      (fmt "goto" l_whl) ^
      (fmtl l_brk), env1
  | ASSIGN(id, e1) ->
    let ce1 = c_aexp e1 env in
    let idx = map_size env in
    let env1 = Env.add id idx env in 
    (ce1) ^
    (fmt "istore" (string_of_int idx)), env1
  | _ -> failwith "Not implemented\n"


and c_bexp (bex: bexp) (env: int Env.t) : string = 
    match bex with
    | TRUE -> "i_const0\n"
    | FALSE -> "i_const0\n"
    | COMP(op, e1, e2) ->
      let instr1 = c_aexp e1 env in
      let instr2 = c_aexp e2 env in
      instr1 ^ instr2 ^ (c_bop op)
    | BEXP(bop, bexp1, bexp2) -> 
      begin
        match bop with
        | CONJ ->
          let l_false = new_label "conj_false" in
          let l_end = new_label "conj_end" in
          (c_bexp bexp1 env) ^ 
          (fmt "ifeq" l_false) ^ (* if first term is false, shortcut, push 0 and exit *)
          (c_bexp bexp2 env) ^ 
          (fmt "goto" l_end) ^   (* bexp1 was true, so result is result of bexp2 *)
          (fmtl l_false) ^       (* restate 0 and exit *)
          ("\ti_const0\n") ^
          (fmtl l_end)
        | DISJ ->
          let l_true = new_label "disj_true" in
          let l_end = new_label "disj_end" in
          (c_bexp bexp1 env) ^
          (fmt "ifne" l_true) ^
          (c_bexp bexp2 env) ^
          (fmt "goto" l_end) ^
          (fmtl l_true) ^
          ("\ti_const1\n") ^
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
  


and c_aexp (exp : aexp) (env: int Env.t) : string =
  match exp with
  | VAL(n) -> (fmt "ldc" (string_of_int n))
  | VAR(id) -> 
    let loc = Env.find id env in
    (fmt "iload" (string_of_int loc))
  | EXPR(aop, e1, e2) ->
    let instr1 = c_aexp e1 env in
    let instr2 = c_aexp e2 env in
      instr1 ^ instr2 ^ (c_aop aop)

let () = 
  let env = Env.empty in
  let cond = COMP(GE, EXPR(ADD, VAL(1), VAL(2)), EXPR(SUB, VAL(10), VAL(4))) in
  (* let cond2 = COMP(LE, VAL(3), VAL(2)) in *)
  let out_cond = BEXP(CONJ, cond, cond) in
  let s1 = WHILE(out_cond, ASSIGN("y", VAL(2))) in
  let (prog, _) = (c_stmt s1 env) in
  print_endline prog