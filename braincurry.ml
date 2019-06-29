(*
 * abstract: /
 * apply: @
 * bind: _
 * extend binder: +
 * builtin io: . ,
 *)

open Parsley

type ast =
  | IOWrite
  | IORead
  | Bind of int
  | Abs of ast
  | App of ast * ast

let parsePluses =
  let parsePlus = Parsley.pchar '+' in
  let succ = fun x_ch -> fun x_int -> x_int + 1 in
  let countP = Parsley.foldP succ 0 parsePlus in
  Parsley.mapP (fun x -> Bind x) countP

let parseBinder =
  Parsley.andThen (Parsley.pchar '_') parsePluses |>
  Parsley.mapP (fun (_, x) -> x)

let parseIO =
  let ioOfChar = function
    | '.' -> IOWrite
    | ',' -> IORead
    | _ -> assert false
  in
  Parsley.orElse (Parsley.pchar '.') (Parsley.pchar ',') |>
  Parsley.mapP ioOfChar

let rec parseBC =
  let parseAbs input =
    let p = ((pchar '/') >.~> parseBC) |> mapP (fun x -> Abs x)
    in run p input
  in let pAbs = Parser parseAbs
  in let parseApp input =
    let p =
      ((pchar '@') >.~> (parseBC >~~> parseBC)) |>
      mapP (fun (x,y) -> App (x,y))
    in run p input
  in let pApp = Parser parseApp
  in let theParser input =
   run (pApp <|> pAbs <|> parseIO <|> parseBinder) input
  in Parser theParser
