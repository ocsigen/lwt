(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

open Test
open Lwt.Infix

(* Note: due to the time delays in the tests of this suite, it could really
   benefit from an option to run tests in parallel. *)

let suite =
  suite "Lwt_timeout"
    [
      test "basic" (fun () ->
          let p, r = Lwt.wait () in

          let start_time = Unix.gettimeofday () in

          let timeout =
            Lwt_timeout.create 1 (fun () ->
                let delta = Unix.gettimeofday () -. start_time in
                Lwt.wakeup_later r delta)
          in
          Lwt_timeout.start timeout;

          p >|= fun delta ->
          instrument
            (delta >= 2. && delta < 3.)
            "Lwt_timeout: basic: %f %f" start_time delta
          (* The above is a bug of the current implementation: it always gives too
             long a timeout. *));
      test "not started" (fun () ->
          let p, r = Lwt.wait () in

          Lwt_timeout.create 1 (fun () -> Lwt.wakeup_later r false) |> ignore;

          Lwt.async (fun () ->
              Lwt_unix.sleep 3. >|= fun () -> Lwt.wakeup_later r true);

          p);
      test "double start" (fun () ->
          let completions = ref 0 in

          let timeout =
            Lwt_timeout.create 1 (fun () -> completions := !completions + 1)
          in
          Lwt_timeout.start timeout;
          Lwt_timeout.start timeout;

          Lwt_unix.sleep 3. >|= fun () ->
          instrument (!completions = 1) "Lwt_timeout: double start: %i"
            !completions);
      test "restart" (fun () ->
          let p, r = Lwt.wait () in

          let completions = ref 0 in

          (* A dummy timeout, just to set up the reference. *)
          let timeout = ref (Lwt_timeout.create 1 ignore) in

          timeout :=
            Lwt_timeout.create 1 (fun () ->
                completions := !completions + 1;
                if !completions < 2 then Lwt_timeout.start !timeout
                else Lwt.wakeup_later r true);
          Lwt_timeout.start !timeout;

          p);
      test "stop" (fun () ->
          let p, r = Lwt.wait () in

          let timeout =
            Lwt_timeout.create 1 (fun () -> Lwt.wakeup_later r false)
          in
          Lwt_timeout.start timeout;
          Lwt_timeout.stop timeout;

          Lwt.async (fun () ->
              Lwt_unix.sleep 3. >|= fun () -> Lwt.wakeup_later r true);

          p);
      test "stop when not stopped" (fun () ->
          Lwt_timeout.create 1 ignore |> Lwt_timeout.stop;

          Lwt.return true);
      test "invalid delay" (fun () ->
          try
            ignore (Lwt_timeout.create 0 ignore);
            Lwt.return false
          with Invalid_argument _ -> Lwt.return true);
      test "change" (fun () ->
          let p, r = Lwt.wait () in

          let start_time = Unix.gettimeofday () in

          let timeout =
            Lwt_timeout.create 5 (fun () ->
                let delta = Unix.gettimeofday () -. start_time in
                Lwt.wakeup_later r delta)
          in
          Lwt_timeout.change timeout 1;
          Lwt_timeout.start timeout;

          p >|= fun delta ->
          instrument
            (delta >= 1.9 && delta < 3.1)
            "Lwt_timeout: change: %f %f" start_time delta);
      test "change does not start" (fun () ->
          let p, r = Lwt.wait () in

          let timeout =
            Lwt_timeout.create 1 (fun () -> Lwt.wakeup_later r false)
          in
          Lwt_timeout.change timeout 1;

          Lwt.async (fun () ->
              Lwt_unix.sleep 3. >|= fun () -> Lwt.wakeup_later r true);

          p);
      test "change after start" (fun () ->
          let p, r = Lwt.wait () in

          let start_time = Unix.gettimeofday () in

          let timeout =
            Lwt_timeout.create 5 (fun () ->
                let delta = Unix.gettimeofday () -. start_time in
                Lwt.wakeup_later r delta)
          in
          Lwt_timeout.start timeout;
          Lwt_timeout.change timeout 1;

          p >|= fun delta ->
          instrument
            (delta >= 1.9 && delta < 3.1)
            "Lwt_timeout: change after start: %f %f" start_time delta);
      test "change: invalid delay" (fun () ->
          let timeout = Lwt_timeout.create 1 ignore in
          try
            Lwt_timeout.change timeout 0;
            Lwt.return false
          with Invalid_argument _ -> Lwt.return true);
      test ~sequential:true "exception in action" (fun () ->
          let p, r = Lwt.wait () in

          Test.with_async_exception_hook
            (fun exn ->
              match exn with Exit -> Lwt.wakeup_later r true | _ -> raise exn)
            (fun () ->
              Lwt_timeout.create 1 (fun () -> raise Exit) |> Lwt_timeout.start;

              p));
      test "set_exn_handler" (fun () ->
          let p, r = Lwt.wait () in

          Lwt_timeout.set_exn_handler (fun exn ->
              match exn with Exit -> Lwt.wakeup_later r true | _ -> raise exn);

          Lwt_timeout.create 1 (fun () -> raise Exit) |> Lwt_timeout.start;

          p >|= fun result ->
          Lwt_timeout.set_exn_handler (fun exn -> !Lwt.async_exception_hook exn);
          result);
      test "two" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in

          let start_time = Unix.gettimeofday () in

          Lwt_timeout.create 1 (fun () ->
              let delta = Unix.gettimeofday () -. start_time in
              Lwt.wakeup r1 delta)
          |> Lwt_timeout.start;

          Lwt_timeout.create 2 (fun () ->
              let delta = Unix.gettimeofday () -. start_time in
              Lwt.wakeup r2 delta)
          |> Lwt_timeout.start;

          p1 >>= fun delta1 ->
          p2 >|= fun delta2 ->
          instrument
            (delta1 >= 1.9 && delta1 < 3. && delta2 >= 2.9 && delta2 < 4.)
            "Lwt_timeout: two: %f %f %f" start_time delta1 delta2);
      test "simultaneous" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in

          let start_time = Unix.gettimeofday () in

          Lwt_timeout.create 1 (fun () ->
              let delta = Unix.gettimeofday () -. start_time in
              Lwt.wakeup r1 delta)
          |> Lwt_timeout.start;

          Lwt_timeout.create 1 (fun () ->
              let delta = Unix.gettimeofday () -. start_time in
              Lwt.wakeup r2 delta)
          |> Lwt_timeout.start;

          p1 >>= fun delta1 ->
          p2 >|= fun delta2 ->
          instrument
            (delta1 >= 1. && delta1 < 2.6 && delta2 >= 1. && delta2 < 2.6)
            "Lwt_timeout: simultaneous: %f %f %f" start_time delta1 delta2);
      test "two, first stopped" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in

          let start_time = Unix.gettimeofday () in

          let timeout1 = Lwt_timeout.create 1 (fun () -> Lwt.wakeup r1 false) in
          Lwt_timeout.start timeout1;

          Lwt_timeout.create 2 (fun () ->
              let delta = Unix.gettimeofday () -. start_time in
              Lwt.wakeup r2 delta)
          |> Lwt_timeout.start;

          Lwt_timeout.stop timeout1;
          Lwt.async (fun () ->
              Lwt_unix.sleep 3. >|= fun () -> Lwt.wakeup r1 true);

          p1 >>= fun timeout1_not_fired ->
          p2 >|= fun delta2 ->
          instrument
            (timeout1_not_fired && delta2 >= 1.5 && delta2 < 3.5)
            "Lwt_timeout: two, first stopped: %b %f %f" timeout1_not_fired
            start_time delta2);
    ]
