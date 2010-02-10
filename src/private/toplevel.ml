(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Toplevel
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

open Types
open Lwt_read_line

module TextSet = Set.Make(Text)

let keywords = Lwt_ocaml_completion.keywords

(* Returns [acc] plus all modules of [dir] *)
let add_modules_from_directory acc dir =
  let dir = if dir = ""  then "./" else dir in
  let acc = ref acc in
  Array.iter (fun fname ->
                if Filename.check_suffix fname ".cmi" then
                  acc := TextSet.add (Text.capitalize (Filename.chop_suffix fname ".cmi")) !acc)
    (Sys.readdir (if dir = "" then Filename.current_dir_name else dir));
  !acc

(* List all names of the module with path [path] *)
let module_names path =
  try
    match Env.find_module path !Toploop.toplevel_env with
      | Tmty_signature decls ->
          List.fold_left
            (fun acc decl -> match decl with
               | Tsig_value(id, _)
               | Tsig_type(id, _, _)
               | Tsig_exception(id, _)
               | Tsig_module(id, _, _)
               | Tsig_modtype(id, _)
               | Tsig_class(id, _, _)
               | Tsig_cltype(id, _, _) ->
                   TextSet.add (Ident.name id) acc)
            TextSet.empty decls
      | _ ->
          TextSet.empty
  with Not_found ->
    TextSet.empty

(* List all names accessible without a path *)
let env_names () =
  let rec loop acc = function
    | Env.Env_empty -> acc
    | Env.Env_value(summary, id, _) -> loop (TextSet.add (Ident.name id) acc) summary
    | Env.Env_type(summary, id, _) -> loop (TextSet.add (Ident.name id) acc) summary
    | Env.Env_exception(summary, id, _) -> loop (TextSet.add (Ident.name id) acc) summary
    | Env.Env_module(summary, id, _) -> loop (TextSet.add (Ident.name id) acc) summary
    | Env.Env_modtype(summary, id, _) -> loop (TextSet.add (Ident.name id) acc) summary
    | Env.Env_class(summary, id, _) -> loop (TextSet.add (Ident.name id) acc) summary
    | Env.Env_cltype(summary, id, _) -> loop (TextSet.add (Ident.name id) acc) summary
    | Env.Env_open(summary, path) -> loop (TextSet.union acc (module_names path)) summary
  in
  (* Add names of the environment: *)
  let acc = loop TextSet.empty (Env.summary !Toploop.toplevel_env) in
  (* Add accessible modules: *)
  List.fold_left add_modules_from_directory acc !Config.load_path

let path_of_string text =
  match Text.split ~sep:"." text with
    | [] ->
        invalid_arg "Toplevel.make_path"
    | ident :: rest ->
        let rec loop path = function
          | [] -> path
          | component :: rest -> loop (Path.Pdot(path, component, 0)) rest
        in
        loop (Path.Pident(Ident.create_persistent ident)) rest

let complete_ident before ident after =
  match Text.rev_split ~sep:"." ~max:2 ident with
    | [ident]->
        complete ~suffix:"" before ident after (TextSet.union keywords (env_names ()))
    | [path; ident] ->
        let before = before ^ path ^ "." in
        let path = path_of_string path in
        complete ~suffix:"" before ident after (module_names path)
    | _ ->
        assert false

let () =
  Topfind.don't_load_deeply ["lwt"; "lwt.unix"; "lwt.text"; "lwt.top"];
  Lwt_ocaml_completion.complete_ident := complete_ident
