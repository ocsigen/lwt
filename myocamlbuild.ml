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

(* Keep that in sync with the list in discover.ml *)
let search_paths = [
  "/usr";
  "/usr/local";
  "/opt";
  "/opt/local";
  "/sw";
  "/mingw";
]

(* OASIS_START *)
(* OASIS_STOP *)

open Ocamlbuild_plugin

let pkg_config flags package =
  with_temp_file "lwt" "pkg-config"
    (fun tmp ->
       Command.execute ~quiet:true & Cmd(S[A "pkg-config"; A("--" ^ flags); A package; Sh ">"; A tmp]);
       List.map (fun arg -> A arg) (string_list_of_file tmp))

let define_c_library ~name ~c_name =
  let tag = Printf.sprintf "use_C_%s" name in

  (* Get flags for using pkg-config: *)
  let opt = pkg_config "cflags" c_name and lib = pkg_config "libs" c_name in

  (* Add flags for linking with the C library: *)
  flag ["ocamlmklib"; "c"; tag] & S lib;

  (* C stubs using the C library must be compiled with the library
     specifics flags: *)
  flag ["c"; "compile"; tag] & S(List.map (fun arg -> S[A"-ccopt"; arg]) opt);

  (* OCaml libraries must depends on the C library: *)
  flag ["link"; "ocaml"; tag] & S(List.map (fun arg -> S[A"-cclib"; arg]) lib)

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

             (* Use an introduction page with categories *)
             tag_file "lwt-api.docdir/index.html" ["apiref"];
             dep ["apiref"] ["apiref-intro"];
             flag ["apiref"] & S[A "-intro"; P "apiref-intro"; A"-colorize-code"];

             (* Glib bindings: *)
             let env = BaseEnvLight.load ~allow_empty:true ~filename:MyOCamlbuildBase.env_filename () in
             if BaseEnvLight.var_get "glib" env = "true" || BaseEnvLight.var_get "all" env = "true" then
               define_c_library ~name:"glib" ~c_name:"glib-2.0";

             let opts = S[A"-ppopt"; A "-let"; A"-ppopt"; A("windows=" ^ if BaseEnvLight.var_get "os_type" env <> "Unix" then "true" else "false")] in
             flag ["ocaml"; "compile"; "pa_optcomp"] & opts;
             flag ["ocaml"; "ocamldep"; "pa_optcomp"] & opts;
             flag ["ocaml"; "doc"; "pa_optcomp"] & opts;

             flag ["ocaml"; "link"; "toplevel"] & A"-linkpkg";

             let env = BaseEnvLight.load () in
             let stdlib_path = BaseEnvLight.var_get "standard_library" env in

             (* Try to find the path where compiler libraries are: *)
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
                        S(List.map (fun x -> A x) (StringSet.elements modules))]));

             (* Search for a header file in standard directories. *)
             let search_header header =
               let rec loop = function
                 | [] ->
                     None
                 | dir :: dirs ->
                     if Sys.file_exists (dir ^ "/include/" ^ header) then
                       Some dir
                     else
                       loop dirs
               in
               loop search_paths
             in

             (* Add directories for libev and pthreads *)
             let flags dir =
               flag ["ocamlmklib"; "c"; "use_stubs"] & A("-L" ^ dir ^ "/lib");
               flag ["c"; "compile"; "use_stubs"] & S[A"-ccopt"; A("-I" ^ dir ^ "/include")];
               flag ["link"; "ocaml"; "use_stubs"] & S[A"-cclib"; A("-L" ^ dir ^ "/lib")]
             in
             begin
               match search_header "ev.h", search_header "pthread.h" with
                 | None, None -> ()
                 | Some path, None | None, Some path -> flags path
                 | Some path1, Some path2 when path1 = path2 -> flags path1
                 | Some path1, Some path2 -> flags path1; flags path2
             end

         | _ ->
             ())

