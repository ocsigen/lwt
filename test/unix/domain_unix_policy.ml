(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* Which Lwt_unix operations work on which domain, in both directions. Jobs,
   signal handlers and child waiting work on every domain. [fork] fails
   everywhere once a domain has been spawned, as [Unix.fork] does.

   Needs a second domain, hence OCaml 5. *)

let failures = ref 0

let check name b =
  if not b
  then (
    Printf.eprintf "FAILED: %s\n" name;
    incr failures)

(* A file to read from another domain, made here since no system file is
   present everywhere. *)
let readable_file =
  let path = Filename.temp_file "lwt-policy" ".txt" in
  let oc = open_out path in
  output_string oc "some content\n";
  close_out oc;
  at_exit (fun () -> try Sys.remove path with Sys_error _ -> ());
  path

let () =
  (* A child forked before any other domain exists, since the runtime refuses
     [Unix.fork] once a domain has been spawned, even after it is joined. It
     exits at once, and another domain reaps it below. *)
  let child = match Unix.fork () with 0 -> Unix._exit 0 | pid -> pid in
  let on_other_domain f = Domain.join (Domain.spawn f) in
  (* Jobs work on another domain. *)
  check "another domain runs a job"
    (on_other_domain (fun () ->
       match Lwt_main.run (Lwt_unix.stat "/") with
       | st -> st.Unix.st_kind = Unix.S_DIR
       | exception _ -> false));
  (* A file opened, read and closed on another domain: three jobs. *)
  check "another domain opens, reads and closes a file"
    (on_other_domain (fun () ->
       match
         Lwt_main.run
           (Lwt.bind (Lwt_unix.openfile readable_file [Unix.O_RDONLY] 0)
              (fun fd ->
                 let buf = Bytes.create 16 in
                 Lwt.bind (Lwt_unix.read fd buf 0 16) (fun n ->
                   Lwt.bind (Lwt_unix.close fd) (fun () -> Lwt.return n))))
       with
       | n -> n > 0
       | exception _ -> false));
  (* Signals reach every subscribed loop, so another domain may subscribe and
     receive the signal itself. *)
  check "another domain subscribes to a signal and receives it"
    (on_other_domain (fun () ->
       let got = ref false in
       let id = Lwt_unix.on_signal Sys.sigusr1 (fun _ -> got := true) in
       Unix.kill (Unix.getpid ()) Sys.sigusr1;
       Lwt_main.run (Lwt_unix.sleep 0.05);
       Lwt_unix.disable_signal_handler id;
       !got));
  (* And another domain waits for a child that the main domain forked. *)
  check "another domain waits for a child"
    (on_other_domain (fun () ->
       match Lwt_main.run (Lwt_unix.waitpid [] child) with
       | _ -> true
       | exception _ -> false));
  (* Spawning that domain is enough to make fork fail there. *)
  check "fork fails on another domain"
    (on_other_domain (fun () ->
       match Lwt_unix.fork () with
       | 0 -> Unix._exit 0
       | _ -> false
       | exception Failure _ -> true));
  (* Also on another domain: a loop with timers, and the notification API. *)
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
  (* The other direction: a notification created on another domain fires there,
     not here, so that domain has to run its own loop to see it. *)
  let fired_here = ref false in
  let fired_there = ref false in
  check "a notification fires on the domain that created it"
    (on_other_domain (fun () ->
       let n =
         Lwt_unix.make_notification ~once:true (fun () -> fired_there := true)
       in
       let mine =
         Lwt_unix.make_notification ~once:true (fun () -> fired_here := true)
       in
       ignore mine;
       Lwt_unix.send_notification n;
       Lwt_main.run (Lwt_unix.sleep 0.05);
       !fired_there));
  check "and it did not fire here" (not !fired_here);
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
  if !failures > 0 then exit 1;
  print_endline "per-domain Lwt_unix policy: ok"
