(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

open Test
open Lwt.Infix

(* None of the APIs make promises about how much larger the elapsed time will
 * be, but they all promise that it won't be less than the expected time. *)
let cmp_elapsed_time start_time expected_time =
  let elapsed_time = Unix.gettimeofday () -. start_time in
  elapsed_time -. expected_time >= 0.0

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

    test "timeout" begin fun () ->
      let start_time = Unix.gettimeofday () in
      let duration = 1.0 in
      Lwt.catch
        (fun () ->
           Lwt_unix.timeout duration
           >>= fun () -> Lwt.return_false
        )
        (function
          | Lwt_unix.Timeout ->
            let check = cmp_elapsed_time start_time duration in
            Lwt.return check
          | _ -> Lwt.return false
        )
    end;

    test "with_timeout does not throw exception" begin fun () ->
      let start_time = Unix.gettimeofday () in
      let duration = 1.0 in
      Lwt.catch
        (fun () ->
           Lwt_unix.with_timeout duration Lwt_unix.yield
           >>= fun () -> Lwt.return_true
        )
        (function
          | Lwt_unix.Timeout ->
            let check = cmp_elapsed_time start_time duration in
            Lwt.return check
          | _ -> Lwt.return false
        )
    end;

    test "with_timeout throws exception" begin fun () ->
      let start_time = Unix.gettimeofday () in
      let duration = 1.0 in
      let f () = Lwt_unix.sleep 2.0 in
      Lwt.catch
        (fun () ->
           Lwt_unix.with_timeout duration f
           >>= fun () ->
           Lwt.return false
        )
        (function
          | Lwt_unix.Timeout ->
            let check = cmp_elapsed_time start_time duration in
            Lwt.return check
          | _ -> Lwt.return false
        )
    end;
  ]
