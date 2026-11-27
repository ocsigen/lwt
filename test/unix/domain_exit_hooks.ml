(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* Exit hooks run on the domain that registered them, while that domain is
   still alive. *)

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
  (* A hook of the main domain: the spawned domain's exit must not run it. *)
  let main_hook_runs = ref 0 in
  Lwt_main.at_exit (fun () -> incr main_hook_runs; Lwt.return_unit);
  let ran_on = Atomic.make (-1) in
  let spawned_id = Atomic.make (-2) in
  let d =
    Domain.spawn (fun () ->
      Atomic.set spawned_id (Domain.self () :> int);
      Lwt_main.at_exit (fun () ->
        Atomic.set ran_on (Domain.self () :> int);
        Lwt.return_unit))
  in
  Domain.join d;
  (* Domain.at_exit fires before the join returns, so by now the hook must have
     run, and it must have run on the spawned domain. *)
  let ran = Atomic.get ran_on and spawned = Atomic.get spawned_id in
  if ran = -1
  then begin
    prerr_endline
      "the spawned domain's exit hook did not run when the domain exited";
    exit 1
  end;
  if ran <> spawned
  then begin
    Printf.eprintf
      "the spawned domain's exit hook ran on domain %d instead of %d\n" ran
      spawned;
    exit 1
  end;
  (* And the main domain's hooks are its own: the spawned domain's exit did not
     drain them. They run at process exit, after this. *)
  if !main_hook_runs <> 0
  then begin
    Printf.eprintf
      "the main domain's exit hook ran %d times when the spawned domain exited\n"
      !main_hook_runs;
    exit 1
  end;
  (* An exit hook that needs a job: the notification channel must outlive the
     drain that runs the hook. In the wrong order, this hangs. *)
  let path = Filename.temp_file "lwt-exit-hook-job" ".txt" in
  let wrote = Atomic.make false in
  Domain.join
    (Domain.spawn (fun () ->
       (* Register the hook first and touch Lwt_unix afterwards: this order
          decides whether the channel is retired before or after the drain. *)
       Lwt_main.at_exit (fun () ->
         Lwt.bind
           (Lwt_unix.openfile path [Unix.O_WRONLY; Unix.O_TRUNC] 0o600)
           (fun fd ->
              Lwt.bind
                (Lwt_unix.write fd (Bytes.of_string "done") 0 4)
                (fun _ ->
                   Lwt.bind (Lwt_unix.close fd) (fun () ->
                     Atomic.set wrote true; Lwt.return_unit))));
       (* A job before dying, so that this domain's channel exists by the time
          the drain runs. *)
       ignore (Lwt_main.run (Lwt_unix.stat "/"))));
  if not (Atomic.get wrote)
  then begin
    prerr_endline "an exit hook needing a job did not complete on domain exit";
    exit 1
  end;
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  (try Sys.remove path with _ -> ());
  if s <> "done"
  then begin
    Printf.eprintf "the exit hook's job wrote %S\n" s;
    exit 1
  end;
  print_endline "per-domain exit hooks: ok"
