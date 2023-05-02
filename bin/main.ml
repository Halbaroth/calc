(*****************************************************************************)
(*                                                                           *)
(*   Calc --- A naive calculator in OCaml                                    *)
(*   Copyright (C) 2023 Pierre Villemot                                      *)
(*                                                                           *)
(*   This program is free software: you can redistribute it and/or modify    *)
(*   it under the terms of the GNU General Public License as published by    *)
(*   the Free Software Foundation, either version 3 of the License, or       *)
(*   (at your option) any later version.                                     *)
(*                                                                           *)
(*   This program is distributed in the hope that it will be useful,         *)
(*   but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *)
(*   GNU General Public License for more details.                            *)
(*                                                                           *)
(*   You should have received a copy of the GNU General Public License       *)
(*   along with this program.  If not, see <https://www.gnu.org/licenses/>.  *)
(*                                                                           *)
(*****************************************************************************)

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
