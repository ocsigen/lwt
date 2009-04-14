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
let intern_syntaxes = [ ("pa_monad", "syntax/pa_monad.cmo") ]

(* +-----------+
   | Ocamlfind |
   +-----------+ *)

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
]

(* List of syntaxes *)
let syntaxes = [
  (* Original syntax *)
  "camlp4o";

  (* Revised syntax *)
  "camlp4r"
]

(* +-------+
   | Utils |
   +-------+ *)

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

(* Define a internal library with required depency. File using it
   (like samples) must be tagged with "use_name".

   For example if the library sources are in the directory "src", and
   samples in directory "samples", you can have in myocamlbuild.ml:

     define_lib ~dir:"src" "foo"

   and in the _tags file:

     <samples/**/*>: use_foo
*)
let define_lib ?dir name =
  ocaml_lib ?dir name;
  dep ["ocaml"; "byte"; "use_" ^ name] [name ^ ".cma"];
  dep ["ocaml"; "native"; "use_" ^ name] [name ^ ".cmxa"]

let substitute env text =
  List.fold_left (fun text (patt, repl) -> String.subst patt repl text) text env

let get_version _ =
  match string_list_of_file "VERSION" with
    | version :: _ -> version
    | _ -> failwith "invalid VERSION file"

let _ =
  dispatch begin function
    | Before_options ->

        Options.make_links := false;

        (* override default commands by ocamlfind ones *)
        let ocamlfind x = S[A"ocamlfind"; A x] in
        Options.ocamlc   := ocamlfind "ocamlc";
        Options.ocamlopt := ocamlfind "ocamlopt";
        Options.ocamldep := ocamlfind "ocamldep";
        Options.ocamldoc := ocamlfind "ocamldoc"

    | After_rules ->
        define_lib ~dir:"src" "lwt";

        (* +-------------------+
           | Internal syntaxes |
           +-------------------+ *)

        List.iter
          (fun (tag, file) ->
             (* add "-ppopt file" to files using the syntax extension *)
             flag_all_stages_except_link tag & S[A"-ppopt"; A file];

             (* Make them depends on the syntax extension *)
             dep ["ocaml"; "ocamldep"; tag] [file])
          intern_syntaxes;

        (* +-----------------+
           | Ocamlfind stuff |
           +-----------------+ *)

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

        (* +-------+
           | Other |
           +-------+ *)

        (* Generation of "META" *)
        rule "META" ~deps:["META.in"; "VERSION"] ~prod:"META"
          (fun _ _ ->
             Echo([substitute [("@VERSION@", get_version ())] (read_file "META.in")], "META"));

        (* Generation of the lwt.odocl file *)
        let deps = ["src/lwt_extra.mllib"; "src/lwt.mllib"; "src/lwt_preemptive.mllib"; "src/lwt_ssl.mllib"]
        and prod = "lwt.odocl" in
        rule "lwt_doc" ~prod ~deps
          (fun _ _ -> Echo(List.map (sprintf "src/%s\n")
                             (List.concat (List.map string_list_of_file deps)), prod));

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
