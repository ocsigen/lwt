(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* A [Lwt_dls] slot. On OCaml 4.14, [Lwt_dls.new_key] calls its argument at
   once: the lazy value defers [init] to the first [get] there too. *)

[@@@alert "-trespassing"]

module Lwt_dls = Lwt_dls

[@@@alert "+trespassing"]

type 'a t = 'a Lazy.t Lwt_dls.t

let make init = Lwt_dls.new_key (fun () -> lazy (init ()))
let get t = Lazy.force (Lwt_dls.get t)
