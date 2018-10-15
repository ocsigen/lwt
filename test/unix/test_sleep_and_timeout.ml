(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

open Test
open Lwt.Infix

let suite = suite "Lwt_unix sleep and timeout" [

    test "sleep" begin fun () ->
      let start_time = Unix.gettimeofday () in
      let duration = 1.0 in
      Lwt_unix.sleep duration
      >>= fun () ->
      let elapsed_time = Unix.gettimeofday () -. start_time in
      Lwt.return (elapsed_time -. duration < 0.005)
    end;
  ]
