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

open Ppx_sexp_conv_lib
open Conv

type op = Add | Sub | Mul | Div | Pow [@@deriving compare, sexp_of]

let pp_op fmt = function
  | Add -> Format.fprintf fmt "+"
  | Sub -> Format.fprintf fmt "-"
  | Mul -> Format.fprintf fmt "*"
  | Div -> Format.fprintf fmt "/"
  | Pow -> Format.fprintf fmt "^"

let compare_int = Int.compare

type t =
  | Ans
  | Cst of int
  | Op of t * op * t
  [@@deriving compare, sexp_of]

exception No_ans

let rec pow a = function
  | 0 -> 1
  | b ->
      let x = pow a (b/2) in
      if b mod 2 = 0 then x*x
      else x*x*a

let rec eval ?ans = function
  | Ans ->
      begin match ans with
      | Some i -> i
      | None -> raise No_ans
      end
  | Cst i -> i
  | Op (left, Add, right) -> eval ?ans left + eval ?ans right
  | Op (left, Sub, right) -> eval ?ans left - eval ?ans right
  | Op (left, Mul, right) -> eval ?ans left * eval ?ans right
  | Op (left, Div, right) -> eval ?ans left / eval ?ans right
  | Op (left, Pow, right) -> pow (eval ?ans left) (eval ?ans right)

let rec pp fmt = function
  | Ans -> Format.fprintf fmt "ans"
  | Cst i -> Format.fprintf fmt "%i" i
  | Op (Cst i, op, Cst j) ->
      Format.fprintf fmt "%i %a %i" i pp_op op j
  | Op (Cst i, op, right) ->
      Format.fprintf fmt "%i %a (%a)" i pp_op op pp right
  | Op (left, op, Cst j) ->
      Format.fprintf fmt "(%a) %a %i" pp left pp_op op j
  | Op (left, op, right) ->
      Format.fprintf fmt "(%a) %a (%a)" pp left pp_op op pp right
