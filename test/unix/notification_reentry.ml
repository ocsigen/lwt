(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* A GC finalizer can run in the middle of make_notification or
   stop_notification, on the same thread, and create a notification itself. A
   finalizer that closes an Lwt_io channel does, since closing is a job. With a
   mutex around the table of notifiers, the finalizer locked it a second time.

   Each round registers a finalizer that creates and stops a notification, then
   creates and stops one. The test fails if a finalizer gets an exception. A
   deadlock is caught by the watchdog. *)

let rounds = 200_000

let () =
  ignore
    (Thread.create
       (fun () ->
          Thread.delay 60.;
          prerr_endline "notification_reentry: timed out";
          exit 2)
       ());
  let errors = ref 0 in
  let first = ref None in
  for i = 1 to rounds do
    Gc.finalise
      (fun _ ->
         try Lwt_unix.stop_notification (Lwt_unix.make_notification ignore)
         with exn ->
           incr errors;
           if !first = None then first := Some exn)
      (ref i);
    Lwt_unix.stop_notification (Lwt_unix.make_notification ignore)
  done;
  Gc.full_major ();
  match !first with
  | None -> print_endline "a finalizer that creates a notification: ok"
  | Some exn ->
      Printf.eprintf "%d finalizers failed, the first with %s\n" !errors
        (Printexc.to_string exn);
      exit 1
