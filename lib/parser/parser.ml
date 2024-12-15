open Ast


type tokens = (string * string) list
type 'a parser = tokens -> ('a * tokens) list

(* Parser combinators *)
let map_parser (p: 'a parser) (f: 'a -> 'b) : 'b parser =
  fun inp -> 
    let res = p inp in
    List.map (fun (matched, unmatched) -> (f matched, unmatched)) res

let seq_parser (p: 'a parser) (q: 'b parser) : ('a * 'b) parser =
  fun inp ->
    let res_p = p inp in
    List.concat (
      List.map (fun (p_parsed, p_unparsed) ->
        let res_q = q p_unparsed in  (* Move this line inside the first mapping function *)
        List.map (fun (q_parsed, pq_unparsed) ->
          ((p_parsed, q_parsed), pq_unparsed)
        ) res_q
      ) res_p
    )
  

let alt_parser (p: 'a parser) (q: 'a parser) : 'a parser = 
  fun inp ->
    let res_p = p inp in
    let res_q = q inp in
    res_p @ res_q 

let create_parser (map : (string * 'a) list) : 'a parser = 
  fun toks ->
    match toks with
    | [] -> []
    | (_, tok_value) :: rest ->
        match List.assoc_opt tok_value map with
        | Some value -> [ (value, rest) ]
        | None -> []


let (++) = seq_parser
let (|~|) = alt_parser
let (>>=) = map_parser
let (!!) = fun tok value -> create_parser [(tok, value)]

let _bop: bop parser = 
  create_parser [
    ("==", BEQ);
    ("!=", BNE);
    ("&&", CONJ);
    ("||", DISJ)
  ]

let _nt: aexp parser =
  fun toks ->
    match toks with
    | ("n", num) :: rest -> ([ (VAL (int_of_string num), rest) ])
    | _ -> [] 

let _id: ident parser = 
  fun toks -> 
    match toks with 
    | ("i", name) :: rest -> [ (name, rest) ]
    | _ -> []

let _str = 
  fun toks ->
    match toks with 
    | ("str", str) :: rest -> [ (str, rest) ]
    | _ -> []



let _sc      = !! ";" NONE
let _do      = !! "do" NONE
let _skip    = !! "skip" SKIP
let _read    = !! "read" NONE
let _write   = !! "write" NONE
let _while   = !! "while" NONE
let _if      = !! "if" NONE
let _then    = !! "then" NONE
let _else    = !! "else" NONE
let _assign  = !! ":=" NONE
let _lp      = !! "(" NONE
let _rp      = !! ")" NONE
let _lb      = !! "{" NONE
let _rb      = !! "}" NONE
let _comma   = !! "," NONE
let _define  = !! "def" NONE
let _for     = !! "for" NONE
let _upto    = !! "upto" NONE
let _do      = !! "do" NONE
let _break   = !! "break" BREAK

let _bool = (!! "true" TRUE) |~| (!! "false" FALSE)

let _add  = (!! "+" ADD) |~| (!! "-" SUB)
let _mult = (!! "/" DIV) |~| (!! "%" MOD) |~| (!! "*" MULT)

let _bcomp = 
  (!! "<=" LE) |~| 
  (!! ">=" GE) |~| 
  (!! "==" EQ) |~| 
  (!! "!=" NE) |~| 
  (!! ">" GT) |~| 
  (!! "<" LT)

let _bop =
  (!! "==" BEQ) |~|
  (!! "!=" BNE) |~|
  (!! "&&" CONJ) |~|
  (!! "||" DISJ)



let rec _bexp: bexp parser = fun inp -> (
    (_bfinal) |~| 
    ((_bfinal ++ _bop ++_bexp) >>= fun ((a,b),c) -> BEXP(b, a, c)) |~|
    ((_aexp++_bcomp ++_aexp) >>= fun ((a,b),c) -> COMP(b,a,c))
  ) inp
and _bfinal: bexp parser = fun inp -> (
    _bool |~|
    ((_aexp ++ _bcomp ++ _aexp) >>= fun ((a, b), c) -> COMP(b, a, c)) |~|
    ((_lp ++ _bexp ++ _rp) >>= fun ((_, b), _) -> b)
  ) inp

and _aexp: aexp parser = fun inp -> (
    _aterm |~|
    ((_aterm ++ _add ++ _aexp) >>= fun ((a,b),c) -> EXPR(b, a, c))
  ) inp
and _aterm: aexp parser = fun inp -> (
    _afinal |~|
    ((_afinal ++ _mult ++ _aterm) >>= fun ((a,b),c) -> EXPR(b, a, c))
  ) inp
and _afinal: aexp parser = fun inp -> (
    _nt |~| 
    (_id >>= fun i -> VAR i)|~|
  ((_lp ++_aexp ++ _rp) >>= fun ((_,b),_) -> b )) inp

and _block: stmt parser = fun inp -> (
    _stmt |~|
    ((_lb ++ _comp_stmt ++ _rb) >>= fun ((_, s), _) -> (s))
  ) inp
and _comp_stmt: stmt parser = fun inp ->
  (((_stmt ++ _sc ++ _comp_stmt) >>= fun ((s1, _), s2) -> SEQ_STMT(s1, s2)) |~| 
   (_stmt)) inp
and _stmt: stmt parser = fun inp ->
  (_skip |~| _write_stmt |~| _break |~|
  ((_id ++ _assign ++_aexp) >>= fun ((a,_),c) -> ASSIGN(a, c)) |~|
  ((_if ++_bexp ++ _then ++ _block ++ _else ++ _block) >>= fun (((((_, b), _), t), _), e) -> IF(b, t, e)) |~|
  ((_while ++_bexp ++ _do ++ _block) >>= fun (((_, b),_), bl) -> WHILE(b, bl)) |~|
  ((_read ++ _id) >>= fun (_,b) -> READ(b)) |~|
  ((_for ++ _id ++ _assign ++ _aexp ++ _upto ++ _aexp ++ _do ++ _block)  
    >>= fun(((((((_, i), _), lo), _), hi), _), st) 
      ->  FOR(i, lo, hi, st))
  ) inp
and _write_stmt: stmt parser = fun inp -> (
    ((_write ++ _id) >>= fun (_, s) -> WRITE_VAR(s)) |~|
    ((_write ++ _lp ++ _id ++ _rp) >>= fun (((_, _), s), _) -> WRITE_VAR(s)) |~|
    ((_write ++ _str) >>= fun (_, s) -> WRITE_STR(s)) |~|
    ((_write ++ _lp ++ _str ++ _rp) >>= fun (((_, _), s), _) -> WRITE_STR(s))
    ) inp



    (* | OP("%")::rest -> [(, rest)]
    | OP("/")::rest -> [(, rest)]
    | OP("*")::rest -> [(, rest)] *)


(* let additive_parser : aOp parser = 
  fun inp: token -> 
    match inp with  *)

(*
Exp ::= Term | Term + Exp | Term - Exp
Term ::= F | F * Term | F / Term | F % Term
F ::= 0..9 | (Exp) | Identifier

BExp ::= BF | BF && BExp | BF || BExp | BF == BExp | BF != BExp
BF ::= B | (Exp Bop Exp) | (BExp) | Identifier
Bop := == | < | > | >= | <= | != 

B ::= true | false | (BExp)

CompoundStmt := Stmt | Stmt ; CompoundStmt
Stmt := 
  | Skip
  | Identifier := Exp
  | if BExp then Block else Block
  | while BExp do Block
  | read Identifier
  | write Identifier

Block := { CompoundStmt }
*)