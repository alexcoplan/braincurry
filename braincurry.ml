(*
 * abstract: /
 * apply: @
 * bind: _
 * extend binder: +
 * builtin io: . ,
 *)

type ast =
  | IOWrite
  | IORead
  | Bind of int
  | Abs of ast
  | App of ast * ast

let parseBinder =
  let parsePlus = Parsley.pchar '+' in
  let succ = fun x_ch -> fun x_int -> x_int + 1 in
  let countP = Parsley.fold succ 0 parsePlus in
  Parsley.mapP (fun x -> Bind x) countP