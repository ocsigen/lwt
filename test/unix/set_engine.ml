(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

open Lwt_engine

let () = set (new libev ()) 
let () = assert (match id () with Engine_id__libev eve -> Ev_backend.(equal eve default) | _ -> false)

let () = set (new libev ~backend:Ev_backend.poll ()) 
let () = assert (match id () with Engine_id__libev eve -> Ev_backend.(equal eve poll) | _ -> false)

let () = set (new select) 
let () = assert (match id () with Engine_id__select -> true | _ -> false)
