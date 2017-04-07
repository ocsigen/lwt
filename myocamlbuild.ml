(* Lightweight thread library for OCaml
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

let c_library_tag name = Printf.sprintf "use_C_%s" name

let define_c_library name env =
  if BaseEnvLight.var_get name env = "true" then begin
    let tag = c_library_tag name in

    let opt =
      List.map
        (fun x -> A x)
        (split (BaseEnvLight.var_get (name ^ "_opt") env))
    and lib =
      List.map
        (fun x -> A x)
        (split (BaseEnvLight.var_get (name ^ "_lib") env))
    in

    (* Add flags for linking with the C library: *)
    flag ["ocamlmklib"; "c"; tag] & S lib;

    (* C stubs using the C library must be compiled with the library
       specifics flags: *)
    flag ["c"; "compile"; tag] &
      S (List.map (fun arg -> S[A"-ccopt"; arg]) opt);

    (* OCaml libraries must depends on the C library: *)
    flag ["link"; "ocaml"; tag] &
      S (List.map (fun arg -> S[A"-cclib"; arg]) lib)
  end

let conditional_warnings_as_errors () =
  match Sys.getenv "LWT_WARNINGS_AS_ERRORS" with
  | "yes" ->
    let flags = S [A "-warn-error"; A "+A"] in
    flag ["ocaml"; "compile"] flags;
    flag ["ocaml"; "link"] flags
  | _ -> ()
  | exception Not_found -> ()

let () = dispatch begin fun hook ->
  let env =
    BaseEnvLight.load
      ~allow_empty:true
      ~filename:(Pathname.basename BaseEnvLight.default_filename)
      ()
  in

  Ocamlbuild_cppo.dispatcher hook;

  dispatch_default hook;

  match hook with
  | Before_options ->
    Options.make_links := false

  | After_options ->
    if BaseEnvLight.var_get "coverage" env = "true" then
      Options.tag_lines :=
        ["<src/**>: package(bisect_ppx)";
         "<**/lwt_config.*>: -package(bisect_ppx)";
         "<tests/**/*.native> or <tests/**/*.byte>: package(bisect_ppx)";
         "<doc/examples/**>: package(bisect_ppx)"]
        @ !Options.tag_lines

  | After_rules ->
    (* Determine extension of CompiledObject: best *)
    let native_suffix =
      if BaseEnvLight.var_get "is_native" env = "true"
      then "native" else "byte"
    in

    flag ["ocaml"; "compile"; "ppx_lwt"] &
      S [A "-ppx"; A ("src/ppx/ppx_lwt_ex." ^ native_suffix)];

    (* Use an introduction page with categories *)
    tag_file "lwt-api.docdir/index.html" ["apiref"];
    dep ["apiref"] ["doc/apiref-intro"];
    flag ["apiref"] & S[A "-intro"; P "doc/apiref-intro"; A"-colorize-code"];

    (* Stubs: *)
    dep ["file:src/unix/lwt_unix_stubs.c"]
      ["src/unix/lwt_unix_unix.c"; "src/unix/lwt_unix_windows.c"];

    let c_libraries = ["glib"; "libev"; "pthread"] in

    (* Check for "unix" because other variables are not present in the
       setup.data file if lwt.unix is disabled. *)
    if BaseEnvLight.var_get "unix" env = "true" then begin
      List.iter (fun name -> define_c_library name env) c_libraries;
      flag ["c"; "compile"; "use_lwt_headers"] & S [A"-ccopt"; A"-Isrc/unix"];
    end;

    List.iter (fun name ->
      mark_tag_used (c_library_tag name)) c_libraries;

    conditional_warnings_as_errors ();

  | _ ->
    ()
  end

(* Compile the wiki version of the Ocamldoc.

   Thanks to Till Varoquaux on usenet:
   http://www.digipedia.pl/usenet/thread/14273/231/ *)
let ocamldoc_wiki tags deps docout docdir =
  let tags = tags -- "extension:html" in
  Ocamlbuild_pack.Ocaml_tools.ocamldoc_l_dir tags deps docout docdir

let () =
  try
    let wikidoc_dir =
      let base =
        Ocamlbuild_pack.My_unix.run_and_read
          "ocamlfind query wikidoc 2> /dev/null"
      in
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

  (* Silently fail if the package wikidoc isn't available *)
  with Failure e -> ()
