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

open Format

type style =
  | FG_Red
  | FG_Blue
  | FG_Default

let close_tag = function
  | _ -> FG_Default

let style_of_tag = function
  | String_tag s ->
      begin match s with
      | "fg_blue" -> FG_Blue
      | "fg_red" -> FG_Red
      | "fg_default" -> FG_Default
      | _ -> raise Not_found
      end
  | _ -> raise Not_found

let to_ansi_value = function
  | FG_Blue -> 34
  | FG_Red -> 91
  | FG_Default -> 39

let ansi_tag = sprintf "\x1B[%im"

let start_mark_ansi_stag t =
  ansi_tag @@ to_ansi_value @@ style_of_tag t

let stop_mark_ansi_stag t =
  ansi_tag @@ to_ansi_value @@ close_tag @@ style_of_tag t

let add_ansi_marking fmt =
  pp_set_mark_tags fmt true;
  pp_set_formatter_stag_functions fmt
    { (pp_get_formatter_stag_functions fmt ())  with
      mark_open_stag = start_mark_ansi_stag;
      mark_close_stag = stop_mark_ansi_stag }

let init () =
  add_ansi_marking Format.std_formatter;
  add_ansi_marking Format.err_formatter

let color_char ?pos fmt str =
  match pos with
  | Some pos ->
      begin
        let len = String.length str in
        let prefix = String.sub str 0 pos in
        let suffix = String.sub str (pos+1) (len-pos-1) in
        fprintf fmt "%s@{<fg_red>%c@}%s"
          prefix str.[pos] suffix
      end
  | None -> fprintf fmt "%s" str

let print_err ?pos ?input =
  match input with
  | Some input ->
    eprintf "%a@\n@{<fg_red>Error@}: @[<hov 0>%s@]@."
      (color_char ?pos) input
  | None ->
    eprintf "@{<fg_red>Error@}: @[<hov 0>%s@]@."
