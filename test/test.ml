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

open Base
open Calc

let eval str =
  Lexer.scan str
  |> Parser.PredictiveParser.emit
  |> Ast.eval

let equal str1 str2 = Int.equal (eval str1) (eval str2)
let diff str1 str2 = Int.compare (eval str1) (eval str2) <> 0

let%test "ass add" = equal "1+2+3" "1+(2+3)"
let%test "ass sub" = diff "1-2-3" "1-(2-3)"
let%test "ass div 1" = equal "2/3/3" "0"
let%test "ass div 2" = diff "2/3/3" "2"
let%test "ass pow 1" = equal "2^3^2" "512"
let%test "ass pow 2" = diff "2^3^2" "64"
let%test "ass pow 3" = equal "(2^3)^2" "64"
let%test "pred mul 1" = equal "1+2*3" "7"
let%test "pred mul 2" = diff "1+2*3" "9"
let%test "pred mul 3" = equal "(1+2)*3" "9"
let%test "pred div 1" = equal "2+2/3" "2"
let%test "pred div 2" = diff "2+2/3" "1"
let%test "pred div 3" = equal "(2+2)/3" "1"
