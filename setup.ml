(*
 * setup.ml
 * --------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of lwt.
 *)

(* OASIS_START *)
#use "topfind";;
#require "oasis.dynrun";;
open OASISDynRun;;
(* OASIS_STOP *)

let () =
  InternalInstallPlugin.lib_hook :=
    fun (cs, bs, lib) ->
      match lib.OASISTypes.lib_findlib_name with
        | Some "unix" ->
            (cs, bs, lib, ["src/unix/lwt_config.ml"; "src/unix/lwt_config.h"; "src/unix/lwt_unix.h"])
        | _ ->
            (cs, bs, lib, [])
;;

let () = setup ();;
