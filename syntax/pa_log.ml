(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Pa_log
 * Copyright (C) 2009 Jérémie Dimino
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exceptions;
 * either version 2.1 of the License, or (at your option) any later
 * version. See COPYING file for details.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *)

open Camlp4.PreCast

let debug = ref false

let levels = [
  "fatal";
  "error";
  "warning";
  "info";
  "debug";
]

let module_name _loc =
  let file_name = Loc.file_name _loc in
  if file_name = "" then
    ""
  else
    String.capitalize (Filename.basename (try
                                            Filename.chop_extension file_name
                                          with Invalid_argument _ ->
                                            file_name))

let rec apply e = function
  | [] -> e
  | x :: l -> let _loc = Ast.loc_of_expr x in apply <:expr< $e$ $x$ >> l

let split e =
  let rec aux acc = function
    | <:expr@loc< Log#exn $exn$ $fmt$ >> ->
        `Failure(exn, fmt, acc)
    | <:expr@loc< Log#$lid:level$ $fmt$ >> ->
        if level = "debug" && (not !debug) then
          `Delete
        else if List.mem level levels then
          `Log(fmt, level, acc)
        else
          Loc.raise loc (Failure (Printf.sprintf "invalid log level: %S" level))
    | <:expr@loc< $a$ $b$ >> ->
        aux (b :: acc) a
    | _ ->
        `Not_a_log
  in
  aux [] e

let map =
object
  inherit Ast.map as super

  method expr e =
    let _loc = Ast.loc_of_expr e in
    let module_name = module_name _loc in
    match split e with
      | `Delete ->
          <:expr< () >>
      | `Log(fmt, level, args) ->
          let args = List.map super#expr args and fmt = super#expr fmt and level = String.capitalize level in
          <:expr<
            if Lwt_log.$uid:level$ >= Lwt_log.level !Lwt_log.default then
              $apply <:expr< Lwt_log.log ~level:Lwt_log.$uid:level$
                               $if module_name = "" then
                                  fmt
                                else
                                  <:expr< Pervasives.( ^^ ) $str:module_name ^ ": "$ $fmt$ >> $ >> args$
          >>
      | `Failure(exn, fmt, args) ->
          let args = List.map super#expr args and fmt = super#expr fmt in
          <:expr<
            if Lwt_log.Error >= Lwt_log.level !Lwt_log.default then
              $apply <:expr<  Lwt_log.exn ~exn:$exn$
                                $if module_name = "" then
                                   fmt
                                 else
                                   <:expr< Pervasives.( ^^ ) $str:module_name ^ ": "$ $fmt$ >> $ >> args$
          >>
      | `Not_a_log ->
          super#expr e
end

let () =
  AstFilters.register_str_item_filter map#str_item;
  AstFilters.register_topphrase_filter map#str_item;

  Camlp4.Options.add "-lwt-debug" (Arg.Set debug) "keep debugging message"
