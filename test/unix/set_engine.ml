(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

open Lwt_engine

let () =
  if Lwt_config._HAVE_LIBEV && Lwt_config.libev_default then begin
    set (new libev ());
    assert (match id () with Engine_id__libev eve -> Ev_backend.(equal eve default) | _ -> false)
  end

let () =
  if Lwt_config._HAVE_LIBEV && Lwt_config.libev_default then begin
    set (new libev ~backend:Ev_backend.poll ());
    assert (match id () with Engine_id__libev eve -> Ev_backend.(equal eve poll) | _ -> false)
  end

let () = set (new select) 
let () = assert (match id () with Engine_id__select -> true | _ -> false)
