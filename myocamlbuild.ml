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
(* DO NOT EDIT (digest: ce14a9d3f4546f95031b29f1cecb878c) *)
module OASISGettext = struct
# 21 "/home/dim/sources/oasis/src/oasis/OASISGettext.ml"
  
  let ns_ str = 
    str
  
  let s_ str = 
    str
  
  let f_ (str : ('a, 'b, 'c, 'd) format4) =
    str
  
  let fn_ fmt1 fmt2 n =
    if n = 1 then
      fmt1^^""
    else
      fmt2^^""
  
  let init = 
    []
  
end

module OASISExpr = struct
# 21 "/home/dim/sources/oasis/src/oasis/OASISExpr.ml"
  
  
  
  open OASISGettext
  
  type test = string 
  
  type flag = string 
  
  type t =
    | EBool of bool
    | ENot of t
    | EAnd of t * t
    | EOr of t * t
    | EFlag of flag
    | ETest of test * string
    
  
  type 'a choices = (t * 'a) list 
  
  let eval var_get t =
    let rec eval' = 
      function
        | EBool b ->
            b
  
        | ENot e -> 
            not (eval' e)
  
        | EAnd (e1, e2) ->
            (eval' e1) && (eval' e2)
  
        | EOr (e1, e2) -> 
            (eval' e1) || (eval' e2)
  
        | EFlag nm ->
            let v =
              var_get nm
            in
              assert(v = "true" || v = "false");
              (v = "true")
  
        | ETest (nm, vl) ->
            let v =
              var_get nm
            in
              (v = vl)
    in
      eval' t
  
  let choose ?printer ?name var_get lst =
    let rec choose_aux = 
      function
        | (cond, vl) :: tl ->
            if eval var_get cond then 
              vl 
            else
              choose_aux tl
        | [] ->
            let str_lst = 
              if lst = [] then
                s_ "<empty>"
              else
                String.concat 
                  (s_ ", ")
                  (List.map
                     (fun (cond, vl) ->
                        match printer with
                          | Some p -> p vl
                          | None -> s_ "<no printer>")
                     lst)
            in
              match name with 
                | Some nm ->
                    failwith
                      (Printf.sprintf 
                         (f_ "No result for the choice list '%s': %s")
                         nm str_lst)
                | None ->
                    failwith
                      (Printf.sprintf
                         (f_ "No result for a choice list: %s")
                         str_lst)
    in
      choose_aux (List.rev lst)
  
end


module BaseEnvLight = struct
# 21 "/home/dim/sources/oasis/src/base/BaseEnvLight.ml"
  
  module MapString = Map.Make(String)
  
  type t = string MapString.t
  
  let default_filename =
    Filename.concat 
      (Sys.getcwd ())
      "setup.data"
  
  let load ?(allow_empty=false) ?(filename=default_filename) () =
    if Sys.file_exists filename then
      begin
        let chn =
          open_in_bin filename
        in
        let st =
          Stream.of_channel chn
        in
        let line =
          ref 1
        in
        let st_line = 
          Stream.from
            (fun _ ->
               try
                 match Stream.next st with 
                   | '\n' -> incr line; Some '\n'
                   | c -> Some c
               with Stream.Failure -> None)
        in
        let lexer = 
          Genlex.make_lexer ["="] st_line
        in
        let rec read_file mp =
          match Stream.npeek 3 lexer with 
            | [Genlex.Ident nm; Genlex.Kwd "="; Genlex.String value] ->
                Stream.junk lexer; 
                Stream.junk lexer; 
                Stream.junk lexer;
                read_file (MapString.add nm value mp)
            | [] ->
                mp
            | _ ->
                failwith
                  (Printf.sprintf
                     "Malformed data file '%s' line %d"
                     filename !line)
        in
        let mp =
          read_file MapString.empty
        in
          close_in chn;
          mp
      end
    else if allow_empty then
      begin
        MapString.empty
      end
    else
      begin
        failwith 
          (Printf.sprintf 
             "Unable to load environment, the file '%s' doesn't exist."
             filename)
      end
  
  let var_get name env =
    let rec var_expand str =
      let buff =
        Buffer.create ((String.length str) * 2)
      in
        Buffer.add_substitute 
          buff
          (fun var -> 
             try 
               var_expand (MapString.find var env)
             with Not_found ->
               failwith 
                 (Printf.sprintf 
                    "No variable %s defined when trying to expand %S."
                    var 
                    str))
          str;
        Buffer.contents buff
    in
      var_expand (MapString.find name env)
  
  let var_choose lst env = 
    OASISExpr.choose
      (fun nm -> var_get nm env)
      lst
end


module MyOCamlbuildFindlib = struct
# 21 "/home/dim/sources/oasis/src/plugins/ocamlbuild/MyOCamlbuildFindlib.ml"
  
  (** OCamlbuild extension, copied from 
    * http://brion.inria.fr/gallium/index.php/Using_ocamlfind_with_ocamlbuild
    * by N. Pouillard and others
    *
    * Updated on 2009/02/28
    *
    * Modified by Sylvain Le Gall 
    *)
  open Ocamlbuild_plugin
  
  (* these functions are not really officially exported *)
  let run_and_read = 
    Ocamlbuild_pack.My_unix.run_and_read
  
  let blank_sep_strings = 
    Ocamlbuild_pack.Lexers.blank_sep_strings
  
  let split s ch =
    let x = 
      ref [] 
    in
    let rec go s =
      let pos = 
        String.index s ch 
      in
        x := (String.before s pos)::!x;
        go (String.after s (pos + 1))
    in
      try
        go s
      with Not_found -> !x
  
  let split_nl s = split s '\n'
  
  let before_space s =
    try
      String.before s (String.index s ' ')
    with Not_found -> s
  
  (* this lists all supported packages *)
  let find_packages () =
    List.map before_space (split_nl & run_and_read "ocamlfind list")
  
  (* this is supposed to list available syntaxes, but I don't know how to do it. *)
  let find_syntaxes () = ["camlp4o"; "camlp4r"]
  
  (* ocamlfind command *)
  let ocamlfind x = S[A"ocamlfind"; x]
  
  let dispatch =
    function
      | Before_options ->
          (* by using Before_options one let command line options have an higher priority *)
          (* on the contrary using After_options will guarantee to have the higher priority *)
          (* override default commands by ocamlfind ones *)
          Options.ocamlc     := ocamlfind & A"ocamlc";
          Options.ocamlopt   := ocamlfind & A"ocamlopt";
          Options.ocamldep   := ocamlfind & A"ocamldep";
          Options.ocamldoc   := ocamlfind & A"ocamldoc";
          Options.ocamlmktop := ocamlfind & A"ocamlmktop"
                                  
      | After_rules ->
          
          (* When one link an OCaml library/binary/package, one should use -linkpkg *)
          flag ["ocaml"; "link"; "program"] & A"-linkpkg";
          
          (* For each ocamlfind package one inject the -package option when
           * compiling, computing dependencies, generating documentation and
           * linking. *)
          List.iter 
            begin fun pkg ->
              flag ["ocaml"; "compile";  "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "doc";      "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "link";     "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "infer_interface"; "pkg_"^pkg] & S[A"-package"; A pkg];
            end 
            (find_packages ());
  
          (* Like -package but for extensions syntax. Morover -syntax is useless
           * when linking. *)
          List.iter begin fun syntax ->
          flag ["ocaml"; "compile";  "syntax_"^syntax] & S[A"-syntax"; A syntax];
          flag ["ocaml"; "ocamldep"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
          flag ["ocaml"; "doc";      "syntax_"^syntax] & S[A"-syntax"; A syntax];
          flag ["ocaml"; "infer_interface"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
          end (find_syntaxes ());
  
          (* The default "thread" tag is not compatible with ocamlfind.
           * Indeed, the default rules add the "threads.cma" or "threads.cmxa"
           * options when using this tag. When using the "-linkpkg" option with
           * ocamlfind, this module will then be added twice on the command line.
           *                        
           * To solve this, one approach is to add the "-thread" option when using
           * the "threads" package using the previous plugin.
           *)
          flag ["ocaml"; "pkg_threads"; "compile"] (S[A "-thread"]);
          flag ["ocaml"; "pkg_threads"; "link"] (S[A "-thread"]);
          flag ["ocaml"; "pkg_threads"; "infer_interface"] (S[A "-thread"])
  
      | _ -> 
          ()
  
end

module MyOCamlbuildBase = struct
# 21 "/home/dim/sources/oasis/src/plugins/ocamlbuild/MyOCamlbuildBase.ml"
  
  (** Base functions for writing myocamlbuild.ml
      @author Sylvain Le Gall
    *)
  
  
  
  open Ocamlbuild_plugin
  
  type dir = string 
  type file = string 
  type name = string 
  type tag = string 
  
# 55 "/home/dim/sources/oasis/src/plugins/ocamlbuild/MyOCamlbuildBase.ml"
  
  type t =
      {
        lib_ocaml: (name * dir list) list;
        lib_c:     (name * dir * file list) list; 
        flags:     (tag list * (spec OASISExpr.choices)) list;
      } 
  
  let env_filename =
    Pathname.basename 
      BaseEnvLight.default_filename
  
  let dispatch_combine lst =
    fun e ->
      List.iter 
        (fun dispatch -> dispatch e)
        lst 
  
  let dispatch t e = 
    let env = 
      BaseEnvLight.load 
        ~filename:env_filename 
        ~allow_empty:true
        ()
    in
      match e with 
        | Before_options ->
            let no_trailing_dot s =
              if String.length s >= 1 && s.[0] = '.' then
                String.sub s 1 ((String.length s) - 1)
              else
                s
            in
              List.iter
                (fun (opt, var) ->
                   try 
                     opt := no_trailing_dot (BaseEnvLight.var_get var env)
                   with Not_found ->
                     Printf.eprintf "W: Cannot get variable %s" var)
                [
                  Options.ext_obj, "ext_obj";
                  Options.ext_lib, "ext_lib";
                  Options.ext_dll, "ext_dll";
                ]
  
        | After_rules -> 
            (* Declare OCaml libraries *)
            List.iter 
              (function
                 | lib, [] ->
                     ocaml_lib lib;
                 | lib, dir :: tl ->
                     ocaml_lib ~dir:dir lib;
                     List.iter 
                       (fun dir -> 
                          flag 
                            ["ocaml"; "use_"^lib; "compile"] 
                            (S[A"-I"; P dir]))
                       tl)
              t.lib_ocaml;
  
            (* Declare C libraries *)
            List.iter
              (fun (lib, dir, headers) ->
                   (* Handle C part of library *)
                   flag ["link"; "library"; "ocaml"; "byte"; "use_lib"^lib]
                     (S[A"-dllib"; A("-l"^lib); A"-cclib"; A("-l"^lib)]);
  
                   flag ["link"; "library"; "ocaml"; "native"; "use_lib"^lib]
                     (S[A"-cclib"; A("-l"^lib)]);
                        
                   flag ["link"; "program"; "ocaml"; "byte"; "use_lib"^lib]
                     (S[A"-dllib"; A("dll"^lib)]);
  
                   (* When ocaml link something that use the C library, then one
                      need that file to be up to date.
                    *)
                   dep  ["link"; "ocaml"; "use_lib"^lib] 
                     [dir/"lib"^lib^"."^(!Options.ext_lib)];
  
                   (* TODO: be more specific about what depends on headers *)
                   (* Depends on .h files *)
                   dep ["compile"; "c"] 
                     headers;
  
                   (* Setup search path for lib *)
                   flag ["link"; "ocaml"; "use_"^lib] 
                     (S[A"-I"; P(dir)]);
              )
              t.lib_c;
  
              (* Add flags *)
              List.iter
              (fun (tags, cond_specs) ->
                 let spec = 
                   BaseEnvLight.var_choose cond_specs env
                 in
                   flag tags & spec)
              t.flags
        | _ -> 
            ()
  
  let dispatch_default t =
    dispatch_combine 
      [
        dispatch t;
        MyOCamlbuildFindlib.dispatch;
      ]
  
end


open Ocamlbuild_plugin;;
let package_default =
  {
     MyOCamlbuildBase.lib_ocaml =
       [
          ("src/core/lwt", ["src/core"]);
          ("src/unix/lwt-unix", ["src/unix"]);
          ("src/react/lwt-react", ["src/react"]);
          ("tests/test", ["tests"]);
          ("src/text/lwt-text", ["src/text"]);
          ("src/top/lwt-top", ["src/top"]);
          ("src/preemptive/lwt-preemptive", ["src/preemptive"]);
          ("src/simple_top/lwt-simple-top", ["src/simple_top"]);
          ("src/glib/lwt-glib", ["src/glib"]);
          ("syntax/lwt-syntax", ["syntax"]);
          ("syntax/lwt-syntax-log", ["syntax"]);
          ("src/extra/lwt-extra", ["src/extra"]);
          ("syntax/optcomp", ["syntax"]);
          ("syntax/lwt-syntax-options", ["syntax"]);
          ("src/ssl/lwt-ssl", ["src/ssl"])
       ];
     lib_c =
       [
          ("lwt-unix",
            "src/unix",
            ["src/unix/lwt_config.h"; "src/unix/lwt_unix.h"]);
          ("lwt-glib", "src/glib", [])
       ];
     flags =
       [
          (["oasis_library_lwt_unix_cclib"; "link"],
            [
               (OASISExpr.EBool true,
                 S [A "-cclib"; A "-lpthread"; A "-cclib"; A "-lev"])
            ]);
          (["oasis_library_lwt_unix_cclib"; "ocamlmklib"; "c"],
            [(OASISExpr.EBool true, S [A "-lpthread"; A "-lev"])])
       ];
     }
  ;;

let dispatch_default = MyOCamlbuildBase.dispatch_default package_default;;

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

