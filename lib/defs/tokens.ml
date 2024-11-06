type token = 
  | INT of int
  | STR of string
  | KWORD of string
  | OP of string
  | ID of string
  | L_PAREN
  | R_PAREN
  | L_BRACE
  | R_BRACE
  | SEMI

let str_to_tok (ttype, tval) = 
    match ttype with 
    | "k" -> KWORD tval
    | "p" -> (match tval with
               | "(" -> L_PAREN
               | ")" -> R_PAREN
               | "{" -> L_BRACE
               | "}" -> R_BRACE
               | _ -> failwith ("Unknown parenthesis type: " ^ tval))
    | "s" -> (match tval with
               | ";" -> SEMI
               | _ -> failwith ("Unknown semicolon type: " ^ tval))
    | "i" -> ID tval
    | "n" -> INT (int_of_string tval)
    | "str" -> STR tval
    | "o" -> OP tval
    | _ -> failwith ("Unknown token type: " ^ ttype)


module Tokens = struct
  type t = token list
end