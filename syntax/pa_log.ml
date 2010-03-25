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
  "Fatal";
  "Error";
  "Warning";
  "Notice";
  "Info";
  "Debug";
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
  let rec aux section acc = function
    | <:expr@_loc< Lwt_log.$lid:func$ >> ->
        let level =
          String.capitalize (
            let len = String.length func in
            if len >= 2 && func.[len - 2] = '_' && func.[len - 1] = 'f' then
              String.sub func 0 (len - 2)
            else
              func
          )
        in
        if level = "Debug" && (not !debug) then
          `Delete
        else if List.mem level levels then
          `Log(func, section, level, acc)
        else
          `Not_a_log
    | <:expr@loc< $a$ $b$ >> -> begin
        match b with
          | <:expr< ~section >> ->
              aux `Label (b :: acc) a
          | <:expr@_loc< ~section:$section$ >> ->
              aux (`Expr section) (<:expr< ~section:__pa_log_section >> :: acc) a
          | b ->
              aux section (b :: acc) a
      end
    | _ ->
        `Not_a_log
  in
  aux `None [] e

let map =
object
  inherit Ast.map as super

  method expr e =
    let _loc = Ast.loc_of_expr e in
    match split e with
      | `Delete ->
          <:expr< Lwt.return () >>
      | `Log(func, `None, level, args) ->
          let args = List.map super#expr args in
          <:expr<
            if Lwt_log.$uid:level$ >= Lwt_log.Section.level Lwt_log.Section.main then
              $apply <:expr< Lwt_log.$lid:func$ >> args$
            else
              Lwt.return ()
          >>
      | `Log(func, `Label, level, args) ->
          let args = List.map super#expr args in
          <:expr<
            if Lwt_log.$uid:level$ >= Lwt_log.Section.level section then
              $apply <:expr< Lwt_log.$lid:func$ >> args$
            else
              Lwt.return ()
          >>
      | `Log(func, `Expr section, level, args) ->
          let args = List.map super#expr args in
          <:expr<
            let __pa_log_section = $section$ in
            if Lwt_log.$uid:level$ >= Lwt_log.Section.level __pa_log_section then
              $apply <:expr< Lwt_log.$lid:func$ >> args$
            else
              Lwt.return ()
          >>
      | `Not_a_log ->
          super#expr e
end

let () =
  AstFilters.register_str_item_filter map#str_item;
  AstFilters.register_topphrase_filter map#str_item;

  Camlp4.Options.add "-lwt-debug" (Arg.Set debug) "keep debugging message"
