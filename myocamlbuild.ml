(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Myocamlbuild
 * Copyright (C) 2010 Jérémie Dimino
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

(* OASIS_START *)
(* OASIS_STOP *)

open Ocamlbuild_plugin

let split str =
  let rec skip_spaces i =
    if i = String.length str then
      []
    else
      if str.[i] = ' ' then
        skip_spaces (i + 1)
      else
        extract i (i + 1)
  and extract i j =
    if j = String.length str then
      [String.sub str i (j - i)]
    else
      if str.[j] = ' ' then
        String.sub str i (j - i) :: skip_spaces (j + 1)
      else
        extract i (j + 1)
  in
  skip_spaces 0

let define_c_library name env =
  if BaseEnvLight.var_get name env = "true" then begin
    let tag = Printf.sprintf "use_C_%s" name in

    let opt = List.map (fun x -> A x) (split (BaseEnvLight.var_get (name ^ "_opt") env))
    and lib = List.map (fun x -> A x) (split (BaseEnvLight.var_get (name ^ "_lib") env)) in

    (* Add flags for linking with the C library: *)
    flag ["ocamlmklib"; "c"; tag] & S lib;

    (* C stubs using the C library must be compiled with the library
       specifics flags: *)
    flag ["c"; "compile"; tag] & S (List.map (fun arg -> S[A"-ccopt"; arg]) opt);

    (* OCaml libraries must depends on the C library: *)
    flag ["link"; "ocaml"; tag] & S (List.map (fun arg -> S[A"-cclib"; arg]) lib)
  end

let () =
  dispatch
    (fun hook ->
       dispatch_default hook;
       match hook with
         | Before_options ->
             Options.make_links := false

         | After_rules ->
             dep ["file:src/unix/lwt_unix_stubs.c"] ["src/unix/lwt_unix_unix.c"; "src/unix/lwt_unix_windows.c"];
             dep ["pa_optcomp"] ["src/unix/lwt_config.ml"];

             (* Internal syntax extension *)
             List.iter
               (fun base ->
                  let tag = "pa_" ^ base and file = "syntax/pa_" ^ base ^ ".cmo" in
                  flag ["ocaml"; "compile"; tag] & S[A"-ppopt"; A file];
                  flag ["ocaml"; "ocamldep"; tag] & S[A"-ppopt"; A file];
                  flag ["ocaml"; "doc"; tag] & S[A"-ppopt"; A file];
                  dep ["ocaml"; "ocamldep"; tag] [file])
               ["lwt_options"; "lwt"; "lwt_log"; "optcomp"];

             (* Optcomp for .mli *)
             flag ["ocaml"; "compile"; "pa_optcomp_standalone"] & S[A"-pp"; A "./syntax/optcomp.byte"];
             flag ["ocaml"; "ocamldep"; "pa_optcomp_standalone"] & S[A"-pp"; A "./syntax/optcomp.byte"];
             flag ["ocaml"; "doc"; "pa_optcomp_standalone"] & S[A"-pp"; A "./syntax/optcomp.byte"];
             dep ["ocaml"; "ocamldep"; "pa_optcomp_standalone"] ["syntax/optcomp.byte"];

             (* Use an introduction page with categories *)
             tag_file "lwt-api.docdir/index.html" ["apiref"];
             dep ["apiref"] ["apiref-intro"];
             flag ["apiref"] & S[A "-intro"; P "apiref-intro"; A"-colorize-code"];

             (* Stubs: *)
             let env = BaseEnvLight.load ~allow_empty:true ~filename:MyOCamlbuildBase.env_filename () in

             (* Check for "unix" because other variables are not
                present in the setup.data file if lwt.unix is
                disabled. *)
             if BaseEnvLight.var_get "unix" env = "true" then begin
               define_c_library "glib" env;
               define_c_library "libev" env;
               define_c_library "pthread" env;

               flag ["c"; "compile"; "use_lwt_headers"] & S [A"-ccopt"; A"-Isrc/unix"];

               (* With ocaml >= 4, toploop.cmi is not in the stdlib
                  path *)
               let ocaml_major_version = Scanf.sscanf (BaseEnvLight.var_get "ocaml_version" env) "%d" (fun x -> x) in
               if ocaml_major_version >= 4 then
                 List.iter
                   (fun stage -> flag ["ocaml"; stage; "use_toploop"] & S[A "-package"; A "compiler-libs.toplevel"])
                   ["compile"; "ocamldep"; "doc"];

               (* Toplevel stuff *)

               flag ["ocaml"; "link"; "toplevel"] & A"-linkpkg";

               let stdlib_path = BaseEnvLight.var_get "standard_library" env in

               (* Try to find the path where compiler libraries
                  are. *)
               let compiler_libs =
                 let stdlib = String.chomp stdlib_path in
                 try
                   let path =
                     List.find Pathname.exists [
                       stdlib / "compiler-libs";
                       stdlib / "compiler-lib";
                       stdlib / ".." / "compiler-libs";
                       stdlib / ".." / "compiler-lib";
                     ]
                   in
                   path :: List.filter Pathname.exists [ path / "typing"; path / "utils"; path / "parsing" ]
                 with Not_found ->
                   []
               in

               (* Add directories for compiler-libraries: *)
               let paths = List.map (fun path -> S[A"-I"; A path]) compiler_libs in
               List.iter
                 (fun stage -> flag ["ocaml"; stage; "use_compiler_libs"] & S paths)
                 ["compile"; "ocamldep"; "doc"; "link"];

               dep ["file:src/top/toplevel_temp.top"] ["src/core/lwt.cma";
                                                       "src/react/lwt-react.cma";
                                                       "src/unix/lwt-unix.cma";
                                                       "src/text/lwt-text.cma";
                                                       "src/top/lwt-top.cma"];

               flag ["file:src/top/toplevel_temp.top"] & S[A"-I"; A"src/unix";
                                                           A"-I"; A"src/text";
                                                           A"src/core/lwt.cma";
                                                           A"src/react/lwt-react.cma";
                                                           A"src/unix/lwt-unix.cma";
                                                           A"src/text/lwt-text.cma";
                                                           A"src/top/lwt-top.cma"];

               (* Expunge compiler modules *)
               rule "toplevel expunge"
                 ~dep:"src/top/toplevel_temp.top"
                 ~prod:"src/top/lwt_toplevel.byte"
                 (fun _ _ ->
                    let directories =
                      stdlib_path
                      :: "src/core"
                      :: "src/react"
                      :: "src/unix"
                      :: "src/text"
                      :: "src/top"
                      :: (List.map
                            (fun lib ->
                               String.chomp
                                 (run_and_read
                                    ("ocamlfind query " ^ lib)))
                            ["findlib"; "react"; "unix"; "text"])
                    in
                    let modules =
                      List.fold_left
                        (fun set directory ->
                           List.fold_left
                             (fun set fname ->
                                if Pathname.check_extension fname "cmi" then
                                  StringSet.add (module_name_of_pathname fname) set
                                else
                                  set)
                             set
                             (Array.to_list (Pathname.readdir directory)))
                        StringSet.empty directories
                    in
                    Cmd(S[A(stdlib_path / "expunge");
                          A"src/top/toplevel_temp.top";
                          A"src/top/lwt_toplevel.byte";
                          A"outcometree"; A"topdirs"; A"toploop";
                          S(List.map (fun x -> A x) (StringSet.elements modules))]))
             end

         | _ ->
             ())

(* Compile the wiki version of the Ocamldoc.

   Thanks to Till Varoquaux on usenet:
   http://www.digipedia.pl/usenet/thread/14273/231/

*)

let ocamldoc_wiki tags deps docout docdir =
  let tags = tags -- "extension:html" in
  Ocamlbuild_pack.Ocaml_tools.ocamldoc_l_dir tags deps docout docdir

let () =
  try
    let wikidoc_dir =
      let base = Ocamlbuild_pack.My_unix.run_and_read "ocamlfind query wikidoc" in
      String.sub base 0 (String.length base - 1)
    in

    Ocamlbuild_pack.Rule.rule
      "ocamldoc: document ocaml project odocl & *odoc -> wikidocdir"
      ~insert:`top
      ~prod:"%.wikidocdir/index.wiki"
      ~stamp:"%.wikidocdir/wiki.stamp"
      ~dep:"%.odocl"
      (Ocamlbuild_pack.Ocaml_tools.document_ocaml_project
         ~ocamldoc:ocamldoc_wiki
         "%.odocl" "%.wikidocdir/index.wiki" "%.wikidocdir");

    tag_file "lwt-api.wikidocdir/index.wiki" ["apiref";"wikidoc"];
    flag ["wikidoc"] & S[A"-i";A wikidoc_dir;A"-g";A"odoc_wiki.cma"]

  with Failure e -> () (* Silently fail if the package wikidoc isn't available *)
