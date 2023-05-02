type op = Add | Sub | Mul | Div

type t =
  | Cst of int
  | Op of t * op * t

val eval : t -> int
val pp : Format.formatter -> t -> unit
