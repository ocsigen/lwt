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

(* List of syntax extensions used internally. It is a list of:

   (tag, byte-code-file)

   - tag is the tag that must be used inside the source tree (in _tags
   files) in order to have file preprocessed with the given syntax extension

   - byte-code-file is the byte-code for the syntax extension
*)
let intern_syntaxes = [ ("pa_lwt", "syntax/pa_lwt.cmo") ]

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

let pkg_config =
  let binary = lazy
    (try
       Command.search_in_path "pkg-config"
     with
         Not_found ->
           failwith "The program ``pkg-config'' is required but not found, please intall it")
  in
  fun flags package ->
    let binary = Lazy.force binary in
    with_temp_file "lwt" "pkg-config"
      (fun tmp ->
         Command.execute ~quiet:true & Cmd(S[A binary; A("--" ^ flags); A package; Sh ">"; A tmp]);
         List.map (fun arg -> A arg) (string_list_of_file tmp))

let define_stubs name =
  let tag = sprintf "use_%s_stubs" name in
  dep ["link"; "ocaml"; tag] [sprintf "src/liblwt_%s_stubs.a" name];
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
        Options.ocamlc   := ocamlfind "ocamlc";
        Options.ocamlopt := ocamlfind "ocamlopt";
        Options.ocamldep := ocamlfind "ocamldep";
        (* FIXME: sometimes ocamldoc say that elements are not found
           even if they are present: *)
        Options.ocamldoc := S[A"ocamlfind"; A"ocamldoc"; A"-hide-warnings"]

    | After_rules ->
        Pathname.define_context "src" [ "src/private" ];
        Pathname.define_context "src/private" [ "src" ];

        (* +---------------------------------------------------------+
           | Internal syntaxes                                       |
           +---------------------------------------------------------+ *)

        List.iter
          (fun (tag, file) ->
             (* add "-ppopt file" to files using the syntax extension *)
             flag_all_stages_except_link tag & S[A"-ppopt"; A file];

             (* Make them depends on the syntax extension *)
             dep ["ocaml"; "ocamldep"; tag] [file])
          intern_syntaxes;

        (* +---------------------------------------------------------+
           | Ocamlfind stuff                                         |
           +---------------------------------------------------------+ *)

        (* When one link an OCaml binary, one should use -linkpkg *)
        flag ["ocaml"; "link"; "program"] & A"-linkpkg";

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
           | C stubs                                                 |
           +---------------------------------------------------------+ *)

        define_stubs "unix";
        define_stubs "glib";
        define_c_library ~name:"glib" ~c_name:"glib-2.0";

        (* +---------------------------------------------------------+
           | C stubs for Lwt_glib                                    |
           +---------------------------------------------------------+ *)

        (* Search 'pkg-config': *)
        let pkg_config = try
          Command.search_in_path "pkg-config"
        with
            Not_found ->
              failwith "The program ``pkg-config'' is required but not found, please intall it"
        in
        let get_args cmd =
          with_temp_file "ocaml-usb" "pkg-config"
            (fun tmp ->
               Command.execute ~quiet:true & Cmd(S[cmd; Sh ">"; A tmp]);
               List.map (fun arg -> A arg) (string_list_of_file tmp))
        in

        (* Get flags for glib-2.0 using pkg-config: *)
        let usb_opt = get_args & S[A pkg_config; A"--cflags"; A"glib-2.0"]
        and usb_lib = get_args & S[A pkg_config; A"--libs"; A"glib-2.0"] in

        (* Dependency for automatic compliation of C stubs: *)
        dep ["link"; "ocaml"; "use_glib_stubs"] ["src/liblwt_glib_stubs.a"];

        (* Link code using glib C stubs with '-llwt_glib_stubs': *)
        flag ["link"; "library"; "ocaml"; "use_glib_stubs"] & S[A"-cclib"; A"-llwt_glib_stubs"];

        (* For libraries add also a '-dllib' option for automatic
           addition of '-cclib -llwt_glib_stubs' when using the
           library: *)
        flag ["link"; "library"; "ocaml"; "byte"; "use_glib_stubs"] & S[A"-dllib"; A"-llwt_glib_stubs"];

        (* Add flags for linking with the C library libusb: *)
        flag ["ocamlmklib"; "c"; "use_libusb"] & S usb_lib;

        let ccopt = S(List.map (fun arg -> S[A"-ccopt"; arg]) usb_opt)
        and cclib = S(List.map (fun arg -> S[A"-cclib"; arg]) usb_lib) in

        (* C stubs using libusb must be compiled with libusb specifics
           flags: *)
        flag ["c"; "compile"; "use_C_glib"] & ccopt;

        (* OCaml llibraries must depends on the C library libusb: *)
        flag ["link"; "ocaml"; "use_C_glib"] & cclib;

        (* +---------------------------------------------------------+
           | Other                                                   |
           +---------------------------------------------------------+ *)

        (* Generation of "META" *)
        rule "META" ~deps:["META.in"; "VERSION"] ~prod:"META"
          (fun _ _ ->
             Echo([substitute [("@VERSION@", get_version ())] (read_file "META.in")], "META"));

        (* Generation of the lwt.odocl file *)
        let deps = ["src/lwt_extra.mllib"; "src/lwt.mllib"; "src/lwt_preemptive.mllib"; "src/lwt_ssl.mllib"]
        and prod = "lwt.odocl" in
        rule "lwt_doc" ~prod ~deps
          (fun _ _ -> Echo(List.map (sprintf "src/%s\n")
                             (* Filter deprecated modules: *)
                             (List.filter (function
                                             | "Lwt_chan" -> false
                                             | s -> not (String.is_prefix "private" s))
                                (List.concat (List.map string_list_of_file deps)))
                           @ ["syntax/Pa_lwt\n"],
                           prod));

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
