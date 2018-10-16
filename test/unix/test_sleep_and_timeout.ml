(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

open Test
open Lwt.Infix

let cmp_elapsed_time start_time expected_time =
  let elapsed_time = Unix.gettimeofday () -. start_time in
  elapsed_time -. expected_time < 0.005

let suite = suite "Lwt_unix sleep and timeout" [
    test "sleep" begin fun () ->
      let start_time = Unix.gettimeofday () in
      let duration = 1.0 in
      Lwt_unix.sleep duration
      >>= fun () ->
      let check = cmp_elapsed_time start_time duration in
      Lwt.return check
    end;

    test "auto_yield" begin fun () ->
      let start_time = Unix.gettimeofday () in
      let duration = 1.0 in
      let wait = Lwt_unix.auto_yield duration in
      wait ()
      >>= fun () ->
      let check = cmp_elapsed_time start_time duration in
      Lwt.return check
    end;
  ]
