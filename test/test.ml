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

let emit str =
  Lexer.scan str
  |> Parser.PredictiveParser.emit

let eval str = emit str |> Ast.eval

let%test_unit "assoc add" =
  [%test_eq: Ast.t] (emit "1+2+3") Ast.(Op (Op (Cst 1, Add, Cst 2), Add, Cst 3))

let%test_unit "assoc mult" =
  [%test_eq: Ast.t] (emit "1*2*3") Ast.(Op (Op (Cst 1, Mul, Cst 2), Mul, Cst 3))

let%test_unit "assoc div 1" =
  [%test_eq: int] (eval "2/3/3") (eval "0")

let%test_unit "assoc div 2" =
  [%test_eq: int] (eval "2/(3/3)") (eval "2")

let%test_unit "assoc pow 1" =
  [%test_eq: int] (eval "2^3^2") (eval "512")

let%test_unit "assoc pow 2" =
  [%test_eq: int] (eval "(2^3)^2") (eval "64")

let%test_unit "pred mul 1" =
  [%test_eq: int] (eval "1+2*3") (eval "7")

let%test_unit "pred mul 3" =
  [%test_eq: int] (eval "(1+2)*3") (eval "9")

let%test_unit "pred div 1" =
  [%test_eq: int] (eval "2+2/3") (eval "2")

let%test_unit "pred div 3" =
  [%test_eq: int] (eval "(2+2)/3") (eval "1")
