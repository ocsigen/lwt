(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_ocaml_completion
 * Copyright (C) 2009 Jérémie Dimino
 *                    Pierre Chambart
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

(* Returns [acc] plus all modules of [dir] *)
let add_modules_from_directory acc dir =
  let dir = if dir = ""  then "./" else dir in
  let acc = ref acc in
  Array.iter (fun fname ->
                if Filename.check_suffix fname ".cmi" then
                  acc := Text.capitalize (Filename.chop_suffix fname ".cmi") :: !acc)
    (Sys.readdir (if dir = "" then Filename.current_dir_name else dir));
  !acc

let list_env () =
  let rec loop acc = function
    | Env.Env_empty -> acc
    | Env.Env_value(summary, id, _) -> loop (Ident.name id :: acc) summary
    | Env.Env_type(summary, id, _) -> loop (Ident.name id :: acc) summary
    | Env.Env_exception(summary, id, _) -> loop (Ident.name id :: acc) summary
    | Env.Env_module(summary, id, _) -> loop (Ident.name id :: acc) summary
    | Env.Env_modtype(summary, id, _) -> loop (Ident.name id :: acc) summary
    | Env.Env_class(summary, id, _) -> loop (Ident.name id :: acc) summary
    | Env.Env_cltype(summary, id, _) -> loop (Ident.name id :: acc) summary
    | Env.Env_open(summary, path) -> loop acc summary
  in
  let acc = [] in
  (* Add names of the environment *)
  let acc = loop acc (Env.summary !Toploop.toplevel_env) in
  (* Add accessible modules *)
  List.fold_left add_modules_from_directory acc !Config.load_path

let () =
  Topfind.don't_load_deeply ["lwt"; "lwt.unix"; "lwt.text"; "lwt.top"];
  Lwt_ocaml_completion.list_env := list_env
