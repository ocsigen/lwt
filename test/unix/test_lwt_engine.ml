(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



open Test
open Lwt.Infix

let timing_tests = [
  test "libev: timer delays are not too short" begin fun () ->
    let start = Unix.gettimeofday () in

    Lwt.catch
      (fun () ->
        (* Block the entire process for one second. If using libev, libev's
           notion of the current time is not updated during this period. *)
        let () = Unix.sleep 1 in

        (* At this point, libev thinks that the time is what it was about one
           second ago. Now schedule exception Lwt_unix.Timeout to be raised in
           0.5 seconds. If the implementation is incorrect, the exception will
           be raised immediately, because the 0.5 seconds will be measured
           relative to libev's "current" time of one second ago. *)
        Lwt_unix.timeout 0.5)

      (function
      | Lwt_unix.Timeout ->
        Lwt.return (Unix.gettimeofday ())
      | exn ->
        Lwt.reraise exn)

    >>= fun stop ->

    Lwt.return (stop -. start >= 1.5)
  end;
]

let tests = timing_tests

let run_tests = [
  test "Lwt_main.run: nested call" ~sequential:true begin fun () ->
    (* The test itself is already running under Lwt_main.run, so we just have to
       call it once and make sure we get an exception. *)

    (* Make sure we are running in a callback called by Lwt_main.run, not
       synchronously when the testing executable is loaded. *)
    Lwt.pause () >>= fun () ->

    try
      Lwt_main.run (Lwt.return_unit);
      Lwt.return_false
    with Failure _ ->
      Lwt.return_true
  end;
]

let tests = tests @ run_tests

let suite = suite "lwt_engine" tests
