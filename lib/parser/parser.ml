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

let (++) = seq_parser
let (|~|) = alt_parser
let (>>=) = map_parser

let create_parser (map : (string * 'a) list) : 'a parser = 
  fun toks ->
    match toks with
    | [] -> []
    | (_, tok_value) :: rest ->
        match List.assoc_opt tok_value map with
        | Some value -> (Printf.printf "matched %s\n" tok_value; [ (value, rest) ])
        | None -> []

let bop: bop parser = 
  create_parser [
    ("==", BEQ);
    ("!=", BNE);
    ("&&", CONJ);
    ("||", DISJ)
  ]

let nt: aexp parser =
  fun toks ->
    match toks with
    | ("n", num) :: rest -> ([ (VAL (int_of_string num), rest) ])
    | _ -> [] 

let id: ident parser = 
  fun toks -> 
    match toks with 
    | ("i", name) :: rest -> [ (name, rest) ]
    | _ -> []



let sc = create_parser [(";", SKIP)]
let _do = create_parser [("do", SKIP)]
let _skip = create_parser [("skip", SKIP)]
let _read = create_parser [("read", SKIP)]
let _write = create_parser [("write", SKIP)]
let _while = create_parser [("while", SKIP)]
let _if = create_parser [("if", SKIP)]
let _then = create_parser [("then", SKIP)]
let _else = create_parser [("else", SKIP)]
let assign = create_parser [(":=", SKIP)] (* aexplicit ignore needed *)
let lp = create_parser [("(", NONE)]
let rp = create_parser [(")", NONE)]
let lb = create_parser [("{", NONE)]
let rb = create_parser [("}", NONE)]
let b = create_parser [("true", TRUE); ("false", FALSE)]
let add = create_parser [("+", ADD); ("-", SUB)]
let mult = create_parser [("/", DIV); ("%", MOD); ("*", MULT)]
let bcomp = create_parser [
  ("<=", LE); (">=", GE); 
  ("==", EQ); ("!=", NE);
  (">", GT); ("<", LT)]


let rec bex: bexp parser = fun inp -> (print_endline "entered bexp parser");
  ((bf |~| 
  ((bf ++ bop ++ bex) >>= fun ((a,b),c) -> BEXP(b, a, c))) |~|
  ((ex ++ bcomp ++ ex) >>= fun ((a,b),c) -> COMP(b,a,c))) inp
and ex: aexp parser = fun inp -> (print_endline "entered aexp parser");
  (te |~|
  (((te ++ add ++ ex) >>= fun ((a,b),c) -> EXPR(b, a, c)))) inp
and te: aexp parser = fun inp ->
  (fi |~|
  ((fi ++ mult ++ te) >>= fun ((a,b),c) -> EXPR(b, a, c))) inp
and fi: aexp parser = fun inp ->
  (nt |~|
  (id >>= fun s -> VAR s) |~|
  ((lp ++ ex ++ rp) >>= fun ((_,b),_) -> b )) inp
and bf: bexp parser = fun inp ->
  ((b) |~|
  ((ex ++ bcomp ++ ex) >>= fun ((a, b), c) -> COMP(b, a, c)) |~|
  ((lp ++ bex ++ rp) >>= fun ((_, b), _) -> b)) inp
and comp_stmt: stmt parser = fun inp ->
  (((stmt ++ sc ++ comp_stmt) >>= fun ((s1, _), s2) -> SEQ_STMT(s1, s2)) |~| 
   (stmt)) inp
and stmt: stmt parser = fun inp ->
  (_skip |~|
  ((id ++ assign ++ ex) >>= fun ((a,_),c) -> ASSIGN(a, c)) |~|
  ((_if ++ bex ++ _then ++ block ++ _else ++ block) >>= fun (((((_, b), _), t), _), e) -> IF(b, t, e)) |~|
  ((_while ++ bex ++ _do ++ block) >>= fun (((_, b),_), bl) -> WHILE(b, bl)) |~|
  ((_read ++ id) >>= fun (_,b) -> READ(b)) |~|
  ((_write ++ lp ++ id ++ rp) >>= fun (((_, _), b), _) -> WRITE(b))) inp
and block: stmt parser = fun inp -> print_endline "blockparser";
  (((lb ++ comp_stmt ++ rb) >>= fun ((_, s), _) -> (s)) |~| 
  (stmt)) inp



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