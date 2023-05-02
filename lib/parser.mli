exception ParsingError of int option * string

module type Parser = sig
  val emit : Lexer.token Lexer.loc list -> Ast.t
end

module PredictiveParser : Parser
