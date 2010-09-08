(*
 * myocamlbuild.ml
 * ---------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

open Ocamlbuild_plugin

let () =
  dispatch
    (function
       | After_rules ->
           tag_any ["use_dynlink"; "use_camlp4"; "use_meldor"];
           ocaml_lib
             ~extern:true
             ~dir:(Findlib.query "meldor").Findlib.location
             "meldor"
       | _ ->
           ())
