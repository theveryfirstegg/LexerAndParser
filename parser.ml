open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

let get_value input = match input with
| Some Tok_ID x -> x
| _ -> raise(InvalidInputException "invalid input")

let rec parse_expr toks = match lookahead toks with
| Some Tok_Let -> parse_Let toks
| Some Tok_If -> parse_If toks
| Some Tok_Fun -> parse_Fun toks
| _ -> parse_Or toks

and parse_Let toks =
let first = match_token toks Tok_Let in
let (t, p) = parse_Recursion first in 
let tok_ahead = lookahead t in
let tokid = match_token t (Tok_ID (get_value tok_ahead))in 
let tokeq = match_token tokid Tok_Equal in
let (t', q) = parse_expr tokeq in
let tokin = match_token t' Tok_In in  
let (t'', r) = parse_expr tokin in
(t'', Let(get_value tok_ahead, p, q, r))

and parse_Recursion toks = match lookahead toks with
| Some Tok_Rec -> (match_token toks Tok_Rec, true)
| _ -> (toks, false)

and parse_If toks = 
  let first = match_token toks Tok_If in
  let(t, p) = parse_expr first in
  let tokthen = match_token t Tok_Then in
  let (t', q) = parse_expr tokthen in
  let tokelse = match_token t' Tok_Else in
  let (s, k) = parse_expr tokelse in
  (s, If(p, q, k))

and parse_Fun toks = 
  let first = match_token toks Tok_Fun in
  let tok_ahead = lookahead first in
  let tokid = match_token first (Tok_ID (get_value tok_ahead)) in
  let tokarrow = match_token tokid Tok_Arrow in
  let (t, p) = parse_expr tokarrow in 
  (t, Fun(get_value tok_ahead, p))


and parse_Or toks = 
  let(t, p) = parse_And toks in match lookahead t with
  | Some Tok_Or -> let t' = match_token t Tok_Or in 
              let (t'', a) = parse_Or t' in
              (t'', Binop(Or, p, a))
  | _ -> (t, p)

and parse_And toks = 
  let(t, p) = parse_Equality toks in match lookahead t with
  | Some Tok_And -> let t' = match_token t Tok_And in
                let (t'', q) = parse_And t' in
                (t'', Binop (And, p, q))
  | _ -> (t, p)

and parse_Equality toks = 
  let(t, p) = parse_Relational toks in match lookahead t with
  | Some Tok_Equal -> let t' = match_token t Tok_Equal in
                  let (t'', q) = parse_Equality t' in
                  (t'', Binop(Equal, p, q))
  | Some Tok_NotEqual -> let t' = match_token t Tok_NotEqual in
                  let (t'', q) = parse_Equality t' in
                  (t'', Binop(NotEqual, p, q))
  | _ -> (t, p)

and parse_Relational toks = 
  let(t, p) = parse_Additive toks in match lookahead t with
  | Some Tok_Less -> let t' = match_token t Tok_Less in
                let (t'', q) = parse_Relational t' in
                (t'', Binop(Less, p, q))
  | Some Tok_Greater -> let t' = match_token t Tok_Greater in
                let (t'', q) = parse_Relational t' in
                (t'', Binop(Greater, p, q))
  | Some Tok_LessEqual -> let t' = match_token t Tok_LessEqual in
                let (t'', q) = parse_Relational t' in
                (t'', Binop(LessEqual, p, q))
  | Some Tok_GreaterEqual -> let t' = match_token t Tok_GreaterEqual in
                let (t'', q) = parse_Relational t' in
                (t'', Binop(GreaterEqual, p, q))
  | _ -> (t, p)

and parse_Additive toks = 
  let(t, p) = parse_Multiplicative toks in match lookahead t with
  | Some Tok_Add -> let t' = match_token t Tok_Add in
                let (t'', q) = parse_Additive t' in
                (t'', Binop(Add, p, q))
  | Some Tok_Sub -> let t' = match_token t Tok_Sub in
                let (t'', q) = parse_Additive t' in
                (t'', Binop(Sub, p, q))
  | _ -> (t, p)

and parse_Multiplicative toks = 
  let(t, p) = parse_Concat toks in match lookahead t with
  | Some Tok_Mult -> let t' = match_token t Tok_Mult in
                let (t'', q) = parse_Multiplicative t' in
                (t'', Binop(Mult, p, q))
  | Some Tok_Div -> let t' = match_token t Tok_Div in
                let (t'', q) = parse_Multiplicative t' in
                (t'', Binop(Div, p, q))
  | _ -> (t, p)

and parse_Concat toks = 
  let(t, p) = parse_Unary toks in match lookahead t with
  | Some Tok_Concat -> let t' = match_token t Tok_Concat in
                  let (t'', q) = parse_Concat t' in
                  (t'', Binop(Concat, p, q))
  | _ -> (t, p)

and parse_Unary toks = match lookahead toks with
| Some Tok_Not -> let t = match_token toks Tok_Not in
              let (t', q) = parse_Unary t in
              (t', Not(q))
| _ -> parse_FunctionCall toks

and parse_FunctionCall toks =
  let(t, p) = parse_Primary toks in match lookahead t with
  | Some Tok_Int(i) -> let (t'', q) = parse_Primary t in (t'', FunctionCall(p, q))
  | Some Tok_Bool(a) -> let (t'', q) = parse_Primary t in (t'', FunctionCall(p, q))
  | Some Tok_String(b) -> let (t'', q) = parse_Primary t in (t'', FunctionCall(p, q))
  | Some Tok_ID(x) -> let (t'', q) = parse_Primary t in (t'', FunctionCall(p, q))
  | Some Tok_LParen -> let (t'', q) = parse_expr t in
                  (t'', FunctionCall(p, q))
  | _ -> (t, p)

and parse_Primary toks = match lookahead toks with
| Some Tok_Int(i) -> let t = match_token toks (Tok_Int(i)) in (t, Value(Int i))
| Some Tok_Bool(a) -> let t = match_token toks (Tok_Bool(a)) in (t, Value(Bool a))
| Some Tok_String(s) -> let t = match_token toks (Tok_String(s)) in (t, Value(String s))
| Some Tok_ID(x) -> let t = match_token toks (Tok_ID(x)) in (t, ID(x))
| Some Tok_LParen -> let t = match_token toks Tok_LParen in 
                let (t', q) = parse_expr t in
                let t'' = match_token t' Tok_RParen in
                (t'', q)
| _ -> raise (InvalidInputException "failed to parse")
  
  



   


(* Part 3: Parsing mutop *)

let rec parse_mutop toks = match lookahead toks with
| Some Tok_Def -> parse_Def toks
| Some Tok_DoubleSemi -> let t = match_token toks Tok_DoubleSemi in (t, NoOp)
| _ -> parse_expr2 toks 

and parse_Def toks = 
  let first = match_token toks Tok_Def in
  let tok_ahead = lookahead first in
  let second = match_token first (Tok_ID(get_value tok_ahead)) in
  let third = match_token second Tok_Equal in
  let (t, p) = parse_expr third in
  let semi = match_token t Tok_DoubleSemi in
  (semi, Def(get_value tok_ahead, p))

and parse_expr2 toks = let(t, p) = parse_expr toks in 
  let semi = match_token t Tok_DoubleSemi in
  (semi, Expr(p))