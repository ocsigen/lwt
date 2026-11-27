(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* Write to Lwt_io.stdout and exit without flushing. The flush is an exit hook,
   which needs a job on a blocking descriptor, so the main domain's
   notification channel must still be alive when the exit hooks run. This test
   passes if the program exits. *)

let () =
  (* A hang must show as a failure, not block the suite. A thread rather than
     an alarm, since the test also runs on Windows. *)
  ignore
    (Thread.create
       (fun () ->
          Thread.delay 10.;
          prerr_endline "domain_exit_main: timed out";
          Unix._exit 2)
       ());
  (* Written and not flushed: the flush must happen in the exit hook. *)
  Lwt_main.run (Lwt_io.write Lwt_io.stdout "");
  (* And an exit hook of our own that needs a job, so that the test does not
     depend on Lwt_io's hook alone. The job may finish before the loop first
     waits, and then needs no notification, so the hook also waits for a slow
     detached call. Lwt_main ignores an exception from a hook, so the hook
     reports its own failure, and its success. *)
  Lwt_main.at_exit (fun () ->
    Lwt.catch
      (fun () ->
         Lwt.bind (Lwt_unix.openfile Filename.null [Unix.O_WRONLY] 0) (fun fd ->
           Lwt.bind (Lwt_unix.close fd) (fun () ->
             Lwt.map
               (fun () -> print_endline "main domain exit hooks with jobs: ok")
               (Lwt_preemptive.detach Unix.sleepf 0.2))))
      (fun exn ->
         prerr_endline ("domain_exit_main: " ^ Printexc.to_string exn);
         Unix._exit 1))
