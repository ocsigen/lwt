(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* Jobs, signal handlers and child waiting go through the notification
   descriptor of the domain that initialized [Lwt_unix], so they belong to it,
   and raise [Failure] elsewhere. What does not depend on the descriptor works
   on any domain: a loop with timers, and creating and sending a notification,
   the way to wake Lwt from another thread or domain. *)

let failures = ref 0

let check name b =
  if not b
  then (
    Printf.eprintf "FAILED: %s\n" name;
    incr failures)

(* [true] if [f] fails because it is not on the domain that initialized
   [Lwt_unix]. *)
let refused f =
  match f () with
  | _ -> false
  | exception Failure m ->
      let expected = "only available on the domain that initialized Lwt_unix" in
      let rec contains i =
        i + String.length expected <= String.length m
        && (String.sub m i (String.length expected) = expected
           || contains (i + 1))
      in
      contains 0
  | exception _ -> false

let () =
  (* A child that stays alive, forked before any other domain exists, since
     the runtime refuses [Unix.fork] once a domain has been spawned, even
     after it is joined. *)
  let child =
    match Unix.fork () with 0 -> Unix.sleep 30; Unix._exit 0 | pid -> pid
  in
  let on_other_domain f = Domain.join (Domain.spawn f) in
  (* Refused on another domain. *)
  check "a job is refused on another domain"
    (on_other_domain (fun () -> refused (fun () -> Lwt_unix.stat "/")));
  check "a signal handler is refused on another domain"
    (on_other_domain (fun () ->
       refused (fun () -> Lwt_unix.on_signal Sys.sigusr1 (fun _ -> ()))));
  check "waiting for a child is refused on another domain"
    (on_other_domain (fun () ->
       refused (fun () -> Lwt_main.run (Lwt_unix.waitpid [] child))));
  check "cancelling jobs is refused on another domain"
    (on_other_domain (fun () -> refused Lwt_unix.cancel_jobs));
  (* Allowed on another domain: a loop with timers, and the notification API. *)
  check "a loop with timers still runs on another domain"
    (on_other_domain (fun () ->
       match Lwt_main.run (Lwt_unix.sleep 0.01) with
       | () -> true
       | exception _ -> false));
  let fired = ref false in
  let n = Lwt_unix.make_notification ~once:true (fun () -> fired := true) in
  on_other_domain (fun () -> Lwt_unix.send_notification n);
  Lwt_main.run (Lwt_unix.sleep 0.05);
  check "a notification sent from another domain fires on the main domain"
    !fired;
  let fired_there = ref false in
  let n2 =
    on_other_domain (fun () ->
      Lwt_unix.make_notification ~once:true (fun () -> fired_there := true))
  in
  Lwt_unix.send_notification n2;
  Lwt_main.run (Lwt_unix.sleep 0.05);
  check "a notification created on another domain fires on the main domain"
    !fired_there;
  (* The main domain is unaffected. *)
  check "the main domain still runs jobs"
    (match Lwt_main.run (Lwt_unix.stat "/") with
    | _ -> true
    | exception _ -> false);
  check "the main domain still registers signal handlers"
    (match Lwt_unix.on_signal Sys.sigusr1 (fun _ -> ()) with
    | id ->
        Lwt_unix.disable_signal_handler id;
        true
    | exception _ -> false);
  (try Unix.kill child Sys.sigkill with _ -> ());
  (try ignore (Unix.waitpid [] child) with _ -> ());
  if !failures > 0 then exit 1;
  print_endline "per-domain Lwt_unix policy: ok"
