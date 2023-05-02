type op = Add | Sub | Mul | Div

let pp_op fmt = function
  | Add -> Format.fprintf fmt "+"
  | Sub -> Format.fprintf fmt "-"
  | Mul -> Format.fprintf fmt "*"
  | Div -> Format.fprintf fmt "/"

type t =
  | Cst of int
  | Op of t * op * t

let rec eval = function
  | Cst i -> i
  | Op (left, Add, right) -> eval left + eval right
  | Op (left, Sub, right) -> eval left - eval right
  | Op (left, Mul, right) -> eval left * eval right
  | Op (left, Div, right) -> eval left / eval right

let rec pp fmt = function
  | Cst i -> Format.fprintf fmt "%i" i
  | Op (Cst i, op, Cst j) ->
      Format.fprintf fmt "%i %a %i" i pp_op op j
  | Op (Cst i, op, right) ->
      Format.fprintf fmt "%i %a (%a)" i pp_op op pp right
  | Op (left, op, Cst j) ->
      Format.fprintf fmt "(%a) %a %i" pp left pp_op op j
  | Op (left, op, right) ->
      Format.fprintf fmt "(%a) %a (%a)" pp left pp_op op pp right
