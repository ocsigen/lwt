(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Ocamlbuild plugin
 * Copyright (C) 2008 Stéphane Glondu
 * Laboratoire PPS - CNRS Université Paris Diderot
 *               2009 Jérémie Dimino
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

open Printf
open Ocamlbuild_plugin

(* +-----------------------------------------------------------------+
   | Configuration                                                   |
   +-----------------------------------------------------------------+ *)

let try_exec command =
  try
    let () = Command.execute ~quiet:true (Cmd(S[Sh command; Sh"> /dev/null"; Sh"2> /dev/null"])) in
    true
  with _ ->
    false

let () =
  if not (try_exec "ocamlfind printconf") then begin
    prerr_endline "ocamlfind is not available, please install it";
    exit 1
  end

let have_native = try_exec "ocamlfind ocamlopt -version"
let have_threads = try_exec "ocamlfind query threads"
let have_ssl = try_exec "ocamlfind query ssl"
let have_glib = try_exec "ocamlfind query lablgtk2" && try_exec "pkg-config glib-2.0"
let have_react = try_exec "ocamlfind query react"
let have_text = have_react && try_exec "ocamlfind query text"

let stdlib_path = String.chomp (run_and_read "ocamlfind ocamlc -where")

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

let have_toplevel =
  (List.exists (fun path -> Pathname.exists (path / "env.cmi")) compiler_libs) && have_text

let () =
  let yes_no = function true -> "yes" | false -> "no" in
  printf "\
+--[ compilation options ]----------+
| native compilation:           %3s |
| preemptive threads support:   %3s |
| ssl support:                  %3s |
| glib support:                 %3s |
| react support                 %3s |
| text support:                 %3s |
| super toplevel:               %3s |
+-----------------------------------+
%!" (yes_no have_native)
    (yes_no have_threads)
    (yes_no have_ssl)
    (yes_no have_glib)
    (yes_no have_react)
    (yes_no have_text)
    (yes_no have_toplevel)

type library = {
  name : string;
  (* The short name of the library ("core", "unix", ... *)
  deps : string list;
  (* Dependencies on other lwt libraries *)
  have : bool;
  (* Can we build it ? *)
  test : bool;
  (* Are they tests for it ? *)
}

(* All libraries with their dependencies *)
let libraries = [
  { name = "core";
    have = true;
    deps = [];
    test = true };
  { name = "unix";
    have = true;
    deps = ["core"];
    test = true };
  { name = "preemptive";
    have = have_threads;
    deps = ["core"; "unix"];
    test = false };
  { name = "extra";
    have = have_threads;
    deps = ["core"; "unix"; "preemptive"];
    test = false };
  { name = "glib";
    have = have_glib;
    deps = ["core"; "unix"];
    test = false };
  { name = "react";
    have = have_react;
    deps = ["core"];
    test = true };
  { name = "ssl";
    have = have_ssl;
    deps = ["core"; "unix"];
    test = false };
  { name = "text";
    have = have_text;
    deps = ["core"; "unix"; "react"];
    test = false };
  { name = "simple_top";
    have = true;
    deps = ["core"; "unix"];
    test = false };
  { name = "top";
    have = have_text;
    deps = ["core"; "unix"; "text"; "react"];
    test = false };
]

let lib_path name suffix =
  if name = "core" then
    sprintf "src/core/lwt.%s" suffix
  else
    sprintf "src/%s/lwt_%s.%s" name name suffix

(* +-----------------------------------------------------------------+
   | Ocamlfind                                                       |
   +-----------------------------------------------------------------+ *)

(* Packages we want to use in the program *)
let packages = [
  (* The camlp4 packages is just used to tell that we want to
     preprocess a file with camlp4 *)
  "camlp4";

  (* Handling of quotations of the form <:expr< >>, in original and
     revised syntax *)
  "camlp4.quotations.o";
  "camlp4.quotations.r";

  (* the "EXTEND ... END" syntax extension *)
  "camlp4.extend";

  (* The camlp4 library *)
  "camlp4.lib";

  (* Macro *)
  "camlp4.macro";

  (* Other packages we want to use *)
  "unix";
  "ssl";
  "lablgtk2";
  "text";
  "react";
  "findlib";
  "str";
]

(* List of syntaxes *)
let syntaxes = [
  (* Original syntax *)
  "camlp4o";

  (* Revised syntax *)
  "camlp4r"
]

(* +-----------------------------------------------------------------+
   | Utils                                                           |
   +-----------------------------------------------------------------+ *)

(* Given the tag [tag] add the command line options [f] to all stages
   of compilatiopn but linking *)
let flag_all_stages_except_link tag f =
  flag ["ocaml"; "compile"; tag] f;
  flag ["ocaml"; "ocamldep"; tag] f;
  flag ["ocaml"; "top"; tag] f;
  flag ["ocaml"; "doc"; tag] f

(* Same as [flag_all_stages_except_link] but also flag the linking
   stage *)
let flag_all_stages tag f =
  flag_all_stages_except_link tag f;
  flag ["ocaml"; "link"; tag] f

let substitute env text =
  List.fold_left (fun text (patt, repl) -> String.subst patt repl text) text env

let get_version _ =
  match string_list_of_file "VERSION" with
    | version :: _ -> version
    | _ -> failwith "invalid VERSION file"

(* +-----------------------------------------------------------------+
   | C stubs                                                         |
   +-----------------------------------------------------------------+ *)

let pkg_config flags package =
  with_temp_file "lwt" "pkg-config"
    (fun tmp ->
       Command.execute ~quiet:true & Cmd(S[A "pkg-config"; A("--" ^ flags); A package; Sh ">"; A tmp]);
       List.map (fun arg -> A arg) (string_list_of_file tmp))

let define_stubs name =
  let tag = sprintf "use_%s_stubs" name in
  dep ["link"; "ocaml"; tag] [sprintf "src/%s/stubs/liblwt_%s_stubs.a" name name];
  flag ["link"; "library"; "ocaml"; tag] & S[A"-cclib"; A(sprintf "-llwt_%s_stubs" name)];
  flag ["link"; "library"; "ocaml"; "byte"; tag] & S[A"-dllib"; A(sprintf "-llwt_%s_stubs" name)]

let define_c_library ~name ~c_name =
  let tag = sprintf "use_C_%s" name in

  (* Get flags for using pkg-config: *)
  let opt = pkg_config "cflags" c_name and lib = pkg_config "libs" c_name in

  (* Add flags for linking with the C library: *)
  flag ["ocamlmklib"; "c"; tag] & S lib;

  (* C stubs using the C library must be compiled with the library
     specifics flags: *)
  flag ["c"; "compile"; tag] & S(List.map (fun arg -> S[A"-ccopt"; arg]) opt);

  (* OCaml llibraries must depends on the C library: *)
  flag ["link"; "ocaml"; tag] & S(List.map (fun arg -> S[A"-cclib"; arg]) lib)

let _ =
  dispatch begin function
    | Before_options ->

        Options.make_links := false;

        (* override default commands by ocamlfind ones *)
        let ocamlfind x = S[A"ocamlfind"; A x] in
        Options.ocamlc := ocamlfind "ocamlc";
        Options.ocamlopt := ocamlfind "ocamlopt";
        Options.ocamldep := ocamlfind "ocamldep";
        Options.ocamlmktop := ocamlfind "ocamlmktop";
        (* possibility to override ocamldoc (for the website) *)
        let ocamldoc =
          try Sh (Sys.getenv "OCAMLDOC")
          with Not_found -> S[A"ocamlfind"; A"ocamldoc"]
        in
        (* FIXME: sometimes ocamldoc say that elements are not found
           even if they are present: *)
        Options.ocamldoc := S[ocamldoc; A"-hide-warnings"]

    | After_rules ->
        List.iter
          (fun lib ->
             let deps = List.map (sprintf "src/%s") lib.deps in
             Pathname.define_context (sprintf "src/%s" lib.name) (sprintf "src/%s/private" lib.name :: deps);
             Pathname.define_context (sprintf "src/%s/private" lib.name) (sprintf "src/%s" lib.name :: deps);
             Pathname.define_context (sprintf "tests/%s" lib.name) ("tests" :: "src/core" :: "src/unix" :: sprintf "src/%s" lib.name :: sprintf "src/%s/private" lib.name :: deps))
          libraries;

        Pathname.define_context "tests" ["src/core"; "src/unix"];

        (* +---------------------------------------------------------+
           | Virtual targets                                         |
           +---------------------------------------------------------+ *)

        let libs = List.filter_opt (fun lib -> if lib.have then Some lib.name else None) libraries in

        let byte =
          "syntax/pa_lwt.cmo" :: "syntax/pa_log.cmo" ::
            List.map (fun name -> lib_path name "cma") libs
          @ if have_toplevel then ["src/top/private/toplevel.top"] else []
        and native =
          List.map (fun name -> lib_path name "cmxa") libs in

        let virtual_rule name deps =
          rule name ~stamp:name ~deps (fun _ _ -> Nop)
        in

        virtual_rule "all" & "META" :: if have_native then byte @ native else byte;
        virtual_rule "byte" & "META" :: byte;
        virtual_rule "native" & "META" :: native;

        let tests = List.filter_opt (fun lib -> if lib.have && lib.test then Some lib.name else None) libraries in

        let tests = List.map (if have_native then
                                sprintf "tests/%s/main.native"
                              else
                                sprintf "tests/%s/main.byte") tests in

        virtual_rule "test_programs" & tests;

        (* +---------------------------------------------------------+
           | Internal syntaxes                                       |
           +---------------------------------------------------------+ *)

        List.iter
          (fun (tag, file) ->
             (* add "-ppopt file" to files using the syntax extension *)
             flag_all_stages_except_link tag & S[A"-ppopt"; A file];

             (* Make them depends on the syntax extension *)
             dep ["ocaml"; "ocamldep"; tag] [file])
          [("pa_lwt", "syntax/pa_lwt.cmo");
           ("pa_log", "syntax/pa_log.cmo")];

        (* +---------------------------------------------------------+
           | Ocamlfind stuff                                         |
           +---------------------------------------------------------+ *)

        (* When one link an OCaml binary, one should use -linkpkg *)
        flag ["ocaml"; "link"; "program"] & A"-linkpkg";
        flag ["ocaml"; "link"; "toplevel"] & A"-linkpkg";

        (* For each ocamlfind package one inject the -package option
           when compiling, computing dependencies, generating
           documentation and linking. *)
        List.iter
          (fun package -> flag_all_stages ("pkg_" ^ package) (S[A"-package"; A package]))
          packages;

        (* Like -package but for extensions syntax. Morover -syntax is
           useless when linking. *)
        List.iter
          (fun syntax -> flag_all_stages_except_link ("syntax_" ^ syntax) (S[A"-syntax"; A syntax]))
          syntaxes;

        (* +---------------------------------------------------------+
           | Toplevel                                                |
           +---------------------------------------------------------+ *)

        (* Add directories for compiler-libraries: *)
        flag_all_stages "use_compiler_libs" & S(List.map (fun path -> S[A"-I"; A path]) compiler_libs);

        (* Link with the toplevel library *)
        let libs = ["core"; "react"; "unix"; "text"; "top"] in
        dep ["file:src/top/private/toplevel_temp.top"] (List.map (fun name -> lib_path name "cma") libs);
        flag ["file:src/top/private/toplevel_temp.top"] & S(A"-I" :: A"src/unix/stubs" :: List.map (fun name -> A(lib_path name "cma")) libs);

        (* Expunge compiler modules *)
        rule "toplevel expunge"
          ~dep:"src/top/private/toplevel_temp.top"
          ~prod:"src/top/private/toplevel.top"
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
                    ["findlib"; "react"; "unix"; "ssl"; "text"])
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
                   A"src/top/private/toplevel_temp.top";
                   A"src/top/private/toplevel.top";
                   A"outcometree"; A"topdirs"; A"toploop";
                   S(List.map (fun x -> A x) (StringSet.elements modules))]));

        (* +---------------------------------------------------------+
           | C stubs                                                 |
           +---------------------------------------------------------+ *)

        define_stubs "unix";

        if have_glib then begin
          define_stubs "glib";
          define_c_library ~name:"glib" ~c_name:"glib-2.0"
        end;

        (* +---------------------------------------------------------+
           | Other                                                   |
           +---------------------------------------------------------+ *)

        (* Generation of "META" *)
        rule "META" ~deps:["META.in"; "VERSION"] ~prod:"META"
          (fun _ _ ->
             Echo([substitute [("@VERSION@", get_version ())] (read_file "META.in")], "META"));

        (* Generation of the lwt.odocl file *)
        let deps =
          List.filter_opt
            (fun lib ->
               if lib.have then
                 Some(lib_path lib.name "mllib")
               else
                 None)
            libraries;
        and prod = "lwt.odocl" in
        rule "lwt_doc" ~prod ~deps
          (fun _ _ -> Echo(List.concat
                             (List.map
                                (fun lib ->
                                   if lib.have then
                                     List.map (sprintf "src/%s/%s\n" lib.name)
                                       (List.filter (function
                                                       | "Lwt_chan" | "Lwt_util" -> false
                                                       | s -> not (String.is_prefix "private" s))
                                          (string_list_of_file (lib_path lib.name "mllib")))
                                   else
                                     [])
                                libraries)
                           @ ["syntax/Pa_lwt\n"; "syntax/Pa_log"],
                           prod));

        (* Use an introduction page with categories *)
        tag_file "lwt.docdir/index.html" ["apiref"];
        dep ["apiref"] ["apiref-intro"];
        flag ["apiref"] & S[A "-intro"; P "apiref-intro"; A"-colorize-code"];

        (* Build documentation then copy our css to the documentation
           directory *)
        rule "Documentation with custom css" ~deps:["utils/style.css"; "lwt.docdir/html.stamp"] ~stamp:"doc"
          (fun _ _ -> cp "utils/style.css" "lwt.docdir/style.css");

        (* The default "thread" tag is not compatible with ocamlfind.
           Indeed, the default rules add the "threads.cma" or
           "threads.cmxa" options when using this tag. When using the
           "-linkpkg" option with ocamlfind, this module will then be
           added twice on the command line.

           To solve this, one approach is to add the "-thread" option
           when using the "threads" package using the previous
           plugin. *)
        flag ["ocaml"; "pkg_threads"; "compile"] & S[A "-thread"];
        flag ["ocaml"; "pkg_threads"; "link"] & S[A "-thread"]
    | _ -> ()
  end
