open Calc

let () =
  Printer.init ();
  let stdin = Unix.in_channel_of_descr Unix.stdin in
  try
    while true do
      Format.printf "@{<fg_blue>❱@} @?";
      match In_channel.input_line stdin with
      | Some "q" | None -> raise Exit
      | Some "" -> ()
      | Some input ->
          try
            let tokens = Lexer.scan input in
            let res =
              Parser.PredictiveParser.emit tokens
              |> Ast.eval
            in
            Format.printf "= %i@." res
          with
          | Lexer.SyntaxError pos ->
              Printer.print_err ~pos ~input "Unexpected token"
          | Parser.ParsingError (pos, str) ->
              Printer.print_err ?pos ~input (Format.sprintf "%s" str)
    done
  with Exit -> ()
