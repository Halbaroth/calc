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

let pp_token fmt = function
| CST i -> Format.fprintf fmt "(CST %i)" i
| PLUS -> Format.fprintf fmt "PLUS"
| MINUS -> Format.fprintf fmt "MINUS"
| TIMES -> Format.fprintf fmt "TIMES"
| SLASH -> Format.fprintf fmt "SLASH"
| LPAR -> Format.fprintf fmt "LPAR"
| RPAR -> Format.fprintf fmt "RPAR"

let pp_tokens =
  let pp_sep fmt () = Format.fprintf fmt " @," in
  Format.pp_print_list ~pp_sep pp_token

let is_int c =
  let code = Char.code c in
  48 <= code && code <= 57

let is_simple_lexeme c =
  c = '+' || c = '-' || c = '*' || c = '/' || c = '(' || c = ')'

let simple_tokens = [
  ('+', PLUS);
  ('-', MINUS);
  ('*', TIMES);
  ('/', SLASH);
  ('(', LPAR);
  (')', RPAR)
]

let scan str =
  let seq = String.to_seqi str in
  let rec scan_int pos buf seq =
    match Seq.uncons seq with
    | Some ((_, c), tl) when is_int c ->
        scan_int pos (c :: buf) tl
    | _ -> (
        let res =
          List.rev buf
          |> List.to_seq
          |> String.of_seq
          |> int_of_string
        in
        { data = CST res; pos }, seq)
  in
  let rec scan acc seq =
    match Seq.uncons seq with
    | Some (lc, tl) ->
        begin match lc with
        | _, ' ' -> scan acc tl
        | pos, c when is_simple_lexeme c ->
            let token = List.assoc c simple_tokens in
            scan ({ data = token; pos} :: acc) tl
        | pos, c when is_int c ->
            let token, tl = scan_int pos [c] tl in
            scan (token :: acc) tl
        | pos, _ -> raise (SyntaxError pos)
        end
    | None -> acc
  in
  scan [] seq |> List.rev
