(* Parsley: a small parser combinator library.
 *
 * Following the intro to parser combinators from
 * https://fsharpforfunandprofit.com/series/understanding-parser-combinators.html
 *)

(* {{{ Seq extension *)

let seq_is_empty seq =
  match seq () with
  | Seq.Nil -> true
  | _ -> false

(* }}} *)

type 'a result =
  | Success of 'a
  | Failure of string (* Failures have an error message *)

type 'a parser =
  Parser of (char Seq.t -> ('a * (char Seq.t)) result)

let pchar char_to_match =
  let inner input = match input () with
  | Seq.Nil -> Failure "EOF"
  | Seq.Cons(ch, next) -> 
      if ch = char_to_match then Success (ch, next)
      else let msg = Printf.sprintf "expected '%c', got '%c'" char_to_match ch
      in Failure msg
  in (Parser inner)

let run p input =
  match p with
  | Parser f -> f input

let run_str p str =
  String.to_seq str |> run p

(* Run parser to completion on a string. *)
let run_all p str =
  let result = run_str p str in
  match result with
  | Failure x -> Failure x
  | Success (value, restOfInput) ->
      if seq_is_empty restOfInput
      then Success value
      else Failure "trailing input"

let andThen p1 p2 =
  let inner input =
  match (run p1 input) with
  | Failure msg -> Failure msg
  | Success(v1, rem) ->
      match (run p2 rem) with
      | Failure msg -> Failure msg
      | Success (v2, rem2) -> Success ((v1,v2),rem2)
  in Parser inner;;

let (>.>) = andThen

let orElse p1 p2 =
  let inner input =
    match (run p1 input) with
    | Success result -> Success result
    | Failure _ -> run p2 input
  in Parser inner;;

let (<|>) = orElse

(* N.B.
 *
 * The intuition for map on lists is 'apply f to each element'.
 *
 * This is not the case for the more general functional map.
 * In general, map just says to apply the given function
 * underneath/inside the structure of interest.
 *
 * In particular, for parsers, we just apply the function to
 * the value inside the Success case.
 *)

let mapP f p =
  let inner input =
    match (run p input) with
    | Success (value, rem) -> Success (f value, rem)
    | Failure msg -> Failure msg
  in Parser inner

let (<!>) = mapP

(* return: just lift a value into parser world *)
let returnP x =
  let f input = Success (x, input)
  in Parser f

(* applyP: ('a -> 'b) parser -> 'a parser -> 'b parser
 *
 * here we are just using mapP to reach inside the parser *)
let applyP fP xP =
  (fP >.> xP) |> mapP (fun (f,x) -> f x)

let (<*>) = applyP

(* lift2: ('a -> 'b -> 'c) -> ('a parser) -> ('b parser) -> ('c parser)
 *
 * Given a function which takes two curried arguments, lift
 * it to parser world.
 *
 * An extension of applyP, really. *)
let lift2 f xP yP =
  returnP f <*> xP <*> yP

(* sequenceList: ('a parser) list -> ('a list) parser
 *
 *  cons: 'a -> 'a list -> 'a list
 * consP: 'a parser -> 'a list parser -> 'a list parser *)
let rec sequenceList parserList =
  (* First lift cons to parser world. *)
  let cons head tail = head :: tail in
  let consP = lift2 cons in
  match parserList with
  | [] -> returnP []
  | (p::ps) -> consP p (sequenceList ps)

(* sequence: ('a parser) seq.t -> ('a seq.t) parser *)
let rec sequence parserSeq =
  let consS x rest = fun () -> Seq.Cons(x, rest) in
  let consSP = lift2 consS in
  match parserSeq () with
  | Seq.Nil -> returnP Seq.empty
  | Seq.Cons(p, rest) -> consSP p (sequence rest)

let pstring str =
  str
  |> String.to_seq
  |> Seq.map pchar
  |> sequence
  |> mapP String.of_seq

let foldHelper f x0 parser input =
  let rec iter input =
    match (run parser input) with
    | Failure err -> (x0, input)
    | Success (thisVal, restOfInput) ->
        let (accVal, restOfInput) = iter restOfInput in
        (f thisVal accVal, restOfInput)
  in iter input

let foldP f x0 parser =
  let innerFn input =
    Success (foldHelper f x0 parser input)
  in Parser innerFn

(*
 * vim: foldmethod=marker
 *)
