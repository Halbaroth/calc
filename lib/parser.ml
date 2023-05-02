exception ParsingError of int option * string

module type Parser = sig
  val emit : Lexer.token Lexer.loc list -> Ast.t
end

(* Predictive parser based on the following grammar:
    expr   -> term R
    R      -> '+' term R | '-' term R | e
    term   -> factor S
    S      -> '*' factor S | '/' factor S | e
    factor -> cst | '(' expr ')'
  where e stands for the empty string. *)
module PredictiveParser = struct
  open Ast

  let raise_error ?pos msg =
    Format.kasprintf (fun str -> raise @@ ParsingError (pos, str)) msg

  let expect_token token = function
    | { Lexer.data; _ } :: tokens when data = token ->
        tokens
    | { data; pos } :: _ ->
        raise_error ~pos "Token %a expected"
          Lexer.pp_token token pos Lexer.pp_token data
    | [] ->
        raise_error "Token %a expected, got nothing"
          Lexer.pp_token token

  let rec emit_r acc = function
    | { Lexer.data = Lexer.PLUS; _ } :: tokens ->
      let term, tokens = emit_term tokens in
      emit_r (Op (acc, Add, term)) tokens
    | { data = Lexer.MINUS; _ } :: tokens ->
      let term, tokens = emit_term tokens in
      emit_r (Op (acc, Sub, term)) tokens
    | tokens -> acc, tokens

  and emit_s acc = function
    | { Lexer.data = Lexer.TIMES; _ } :: tokens ->
      let factor, tokens = emit_factor tokens in
      emit_s (Op (acc, Mul, factor)) tokens
    | { data = Lexer.SLASH; _ } :: tokens ->
      let factor, tokens = emit_factor tokens in
      emit_s (Op (acc, Div, factor)) tokens
    | tokens -> acc, tokens

  and emit_factor = function
    | { data = Lexer.CST i; _ } :: tokens -> (Cst i), tokens;
    | { data = LPAR; _ } :: tokens ->
        let expr, tokens = emit_expr tokens in
        expr, expect_token RPAR tokens
    | { pos; _ } :: _ ->
        raise_error ~pos "Constant or left parenthesis expected"
    | [] ->
        raise_error "Constant or left parenthesis expected, \
          got nothing"

  and emit_term tokens =
    let factor, tokens = emit_factor tokens in
    emit_s factor tokens

  and emit_expr tokens =
    let term, tokens = emit_term tokens in
    emit_r term tokens

  let emit tokens =
    let res, tokens = emit_expr tokens in
    assert (tokens = []);
    res
end
