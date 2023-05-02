type 'a loc = { data : 'a; pos : int }

type token =
  | CST of int
  | PLUS
  | MINUS
  | TIMES
  | SLASH
  | LPAR
  | RPAR

exception SyntaxError of int

val scan : string -> token loc list
val pp_token : Format.formatter -> token -> unit
val pp_tokens : Format.formatter -> token list -> unit
