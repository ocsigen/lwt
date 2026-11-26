(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* [Lwt_timeout] has a wheel per domain: a timeout fires on the loop of the
   domain that created it, and is refused on another one. [Lwt_preemptive] has
   a pool per loop. Needs a second domain, hence OCaml 5. *)

let failures = ref 0

let check name b =
  if not b
  then (
    Printf.eprintf "FAILED: %s\n" name;
    incr failures)

let refused f =
  match f () with
  | _ -> false
  | exception Lwt.Domain_crossing -> true
  | exception _ -> false

(* A watchdog thread turns a hang into a failure. It is a thread rather than
   [Unix.alarm], which Windows lacks. *)
let () =
  ignore
    (Thread.create
       (fun () ->
          Thread.delay 60.;
          prerr_endline "FAILED: timed out";
          Unix._exit 1)
       ())

let () =
  (* Each domain's wheel runs on its own loop. The wheel ticks once a second,
     and a timeout with the minimum delay of one lands one bucket ahead of the
     current one, so it fires on the second tick. Hence the waits below. *)
  let fired_here = ref false in
  let t = Lwt_timeout.create 1 (fun () -> fired_here := true) in
  Lwt_timeout.start t;
  Lwt_main.run (Lwt_unix.sleep 2.5);
  check "our own timeout fires on our loop" !fired_here;
  (* The wheels are independent. We start a timeout and never run a loop,
     which leaves our wheel pending. A domain that comes next must still get
     its own wheel, and that wheel must tick. *)
  let fired_orphan = ref false in
  let orphan = Lwt_timeout.create 1 (fun () -> fired_orphan := true) in
  Lwt_timeout.start orphan;
  let fired_there = ref false in
  Domain.join
    (Domain.spawn (fun () ->
       let t = Lwt_timeout.create 1 (fun () -> fired_there := true) in
       Lwt_timeout.start t;
       Lwt_main.run (Lwt_unix.sleep 2.5)));
  check "another domain's timeout fires on its own loop" !fired_there;
  check "and ours did not fire, our loop never having run" (not !fired_orphan);
  (* A timeout does not cross a domain boundary. *)
  let ours = Lwt_timeout.create 1 (fun () -> ()) in
  check "starting our timeout on another domain is refused"
    (Domain.join
       (Domain.spawn (fun () -> refused (fun () -> Lwt_timeout.start ours))));
  check "stopping our timeout on another domain is refused"
    (Domain.join
       (Domain.spawn (fun () -> refused (fun () -> Lwt_timeout.stop ours))));
  check "changing our timeout on another domain is refused"
    (Domain.join
       (Domain.spawn (fun () -> refused (fun () -> Lwt_timeout.change ours 2))));
  (* Detaching works from any domain: the result comes back through the
     detaching loop's own notification channel. *)
  check "another domain detaches blocking work"
    (Domain.join
       (Domain.spawn (fun () ->
          match Lwt_main.run (Lwt_preemptive.detach (fun x -> x * 2) 21) with
          | 42 -> true
          | _ -> false
          | exception _ -> false)));
  check "and so do we"
    (match Lwt_main.run (Lwt_preemptive.detach String.length "hello") with
    | 5 -> true
    | _ -> false
    | exception _ -> false);
  (* Several loops detaching at once, each with more work than the bounds allow,
     so the per-loop queue of waiting clients is exercised too. *)
  check "two domains detach at once, past their bounds"
    (let work () =
       Domain.spawn (fun () ->
         match
           Lwt_main.run
             (Lwt_list.map_p
                (Lwt_preemptive.detach String.length)
                ["a"; "bb"; "ccc"; "dddd"; "eeeee"; "ffffff"])
         with
         | l -> l = [1; 2; 3; 4; 5; 6]
         | exception _ -> false)
     in
     let a = work () and b = work () in
     Domain.join a && Domain.join b);
  (* An exit hook that detaches, and so runs after the pool has been shut down.
     It must still work: the worker it creates leaves after its one task and is
     joined by detach itself, so nothing hangs and nothing is left running. *)
  check "an exit hook may still detach"
    (let got = Atomic.make 0 in
     Domain.join
       (Domain.spawn (fun () ->
          Lwt_main.at_exit (fun () ->
            Lwt.bind
              (Lwt_preemptive.detach (fun x -> x * 3) 14)
              (fun v -> Atomic.set got v; Lwt.return_unit));
          ignore (Lwt_main.run (Lwt_preemptive.detach (fun x -> x) 1))));
     Atomic.get got = 42);
  (* A detached task that ends after the loop of its domain stopped: its worker
     then waits for another task, and the domain's exit must still end it. *)
  check "a domain exits after a detached task that outlived its loop"
    (Domain.join
       (Domain.spawn (fun () ->
          Lwt_main.run
            (ignore (Lwt_preemptive.detach (fun () -> Unix.sleepf 0.05) ());
             Lwt.return_unit);
          Unix.sleepf 0.3;
          true)));
  (* An exit hook waits for a call queued behind a busy worker, with one worker
     per loop. The pool may be shut down before the hook runs: the busy worker
     then leaves after its task, and the queued call must still get one. *)
  let bounds = Lwt_preemptive.get_bounds () in
  Lwt_preemptive.set_bounds (0, 1);
  check "an exit hook waits for a call queued behind a busy worker"
    (let got = Atomic.make 0 in
     Domain.join
       (Domain.spawn (fun () ->
          let queued = ref Lwt.return_unit in
          Lwt_main.at_exit (fun () -> !queued);
          ignore (Lwt_preemptive.detach (fun () -> Unix.sleepf 0.2) ());
          queued :=
            Lwt.map (Atomic.set got) (Lwt_preemptive.detach (fun () -> 42) ());
          Lwt_main.run (Lwt_unix.sleep 0.01)));
     Atomic.get got = 42);
  (* The count of workers stays right when a busy worker ends after the
     shutdown. *)
  check "the count of workers never drops below zero"
    (let count = Atomic.make (-1) in
     Domain.join
       (Domain.spawn (fun () ->
          Lwt_main.at_exit (fun () ->
            Lwt.map
              (fun () -> Atomic.set count (Lwt_preemptive.nbthreads ()))
              (Lwt_unix.sleep 0.3));
          ignore (Lwt_preemptive.detach (fun () -> Unix.sleepf 0.1) ());
          Lwt_main.run (Lwt_unix.sleep 0.01)));
     Atomic.get count = 0);
  Lwt_preemptive.set_bounds bounds;
  (* A [Domain.at_exit] callback registered before the first use of Lwt runs
     after the pool is shut down and the channel retired. Detaching there runs
     the function at once, and leaves no worker behind, or the domain would
     never end. With a minimum of one worker too, which [detach] would start
     otherwise. *)
  List.iter
    (fun min ->
       Lwt_preemptive.set_bounds (min, 4);
       check
         (Printf.sprintf
            "a late Domain.at_exit callback detaches, and the domain ends (min %d)"
            min)
         (let got = Atomic.make 0 in
          Domain.join
            (Domain.spawn (fun () ->
               Domain.at_exit (fun () ->
                 Atomic.set got
                   (match
                      Lwt_main.run (Lwt_preemptive.detach (fun x -> x * 2) 21)
                    with
                   | v -> v
                   | exception _ -> -1));
               Lwt_main.run (Lwt_preemptive.detach (fun () -> ()) ())));
          Atomic.get got = 42))
    [0; 1];
  Lwt_preemptive.set_bounds bounds;
  (* A detached call still running when the domain exits: a later callback that
     waits for it gets its result, once the pool's shutdown has joined the
     worker, though the loop's channel is gone. *)
  check "a late Domain.at_exit callback gets the result of a running call"
    (let got = Atomic.make 0 in
     Domain.join
       (Domain.spawn (fun () ->
          let running = ref (Lwt.return 0) in
          Domain.at_exit (fun () ->
            Atomic.set got
              (match Lwt_main.run !running with v -> v | exception _ -> -1));
          running := Lwt_preemptive.detach (fun () -> Unix.sleepf 0.2; 42) ();
          Lwt_main.run (Lwt_unix.sleep 0.01)));
     Atomic.get got = 42);
  if !failures > 0 then exit 1;
  print_endline "per-domain timeouts and thread pools: ok"
