(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Ocamlbuild plugin
 * Copyright (C) 2008 Stéphane Glondu
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later
 * version.
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

open Ocamlbuild_plugin

let packages = ["unix"; "ssl"]
let ocamlfind x = S[A"ocamlfind"; A x];;

dispatch begin function

  | Before_options ->
      Options.include_dirs := "src" :: !Options.include_dirs;
      Options.make_links := false;
      (* Override default commands by ocamlfind ones *)
      Options.ocamlc := ocamlfind "ocamlc";
      Options.ocamlopt := ocamlfind "ocamlopt";
      Options.ocamldep := ocamlfind "ocamldep";
      (* There seems to be no other way to specify ocamldoc... *)
      Options.ocamldoc := ocamlfind "ocamldoc";

  | After_rules ->
      (* For each ocamlfind package one inject the -package option
         when compiling, computing dependencies, generating
         documentation and linking. *)
      List.iter begin fun pkg ->
        flag ["ocaml"; "compile";  "pkg_"^pkg] & S[A"-package"; A pkg];
        flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
        flag ["ocaml"; "doc";      "pkg_"^pkg] & S[A"-package"; A pkg];
        flag ["ocaml"; "link";     "pkg_"^pkg] & S[A"-package"; A pkg];
      end packages;

      (* The default "thread" tag is not compatible with ocamlfind.
         Indeed, the default rules add the "threads.cma" or
         "threads.cmxa" options when using this tag. When using the
         "-linkpkg" option with ocamlfind, this module will then be
         added twice on the command line.

         To solve this, one approach is to add the "-thread" option
         when using the "threads" package using the previous
         plugin. *)
       flag ["ocaml"; "pkg_threads"; "compile"] & S[A "-thread"];
       flag ["ocaml"; "pkg_threads"; "link"] & S[A "-thread"];

  | _ -> ()
end
