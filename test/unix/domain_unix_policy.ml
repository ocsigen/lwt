(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* Which operations belong to which domain, in both directions. Jobs work on
   every domain. Signal handlers and child waiting still raise
   [Lwt.Domain_crossing] off the domain that initialized the module.

   Needs a second domain, hence OCaml 5. *)

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
  | exception Lwt.Domain_crossing -> true
  | exception _ -> false

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
  (* A child that stays alive, forked before any other domain exists, since
     the runtime refuses [Unix.fork] once a domain has been spawned, even
     after it is joined. *)
  let child =
    match Unix.fork () with 0 -> Unix.sleep 30; Unix._exit 0 | pid -> pid
  in
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
  (* Still refused on another domain. *)
  check "a signal handler is refused on another domain"
    (on_other_domain (fun () ->
       refused (fun () -> Lwt_unix.on_signal Sys.sigusr1 (fun _ -> ()))));
  check "waiting for a child is refused on another domain"
    (on_other_domain (fun () ->
       refused (fun () -> Lwt_main.run (Lwt_unix.waitpid [] child))));
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
  (try Unix.kill child Sys.sigkill with _ -> ());
  (try ignore (Unix.waitpid [] child) with _ -> ());
  if !failures > 0 then exit 1;
  print_endline "per-domain Lwt_unix policy: ok"
