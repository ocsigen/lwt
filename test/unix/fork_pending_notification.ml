(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* A notification still pending at a fork belongs to the parent: the child
   must not handle it. The child's own notifications must still reach its
   loop. The child used to keep the parent's buffer, whose wake-up had gone to
   the parent's descriptor, and its loop never woke up again. *)

let () =
  (* A hang shows as a failure. *)
  ignore (Unix.alarm 30);
  let received = ref 0 in
  let id = Lwt_unix.make_notification (fun () -> incr received) in
  (* Pending: the loop has not run since. *)
  Lwt_unix.send_notification id;
  match Lwt_unix.fork () with
  | 0 ->
      let own = ref 0 in
      let own_id = Lwt_unix.make_notification (fun () -> incr own) in
      Lwt_unix.send_notification own_id;
      Lwt_main.run (Lwt_unix.sleep 0.2);
      Unix._exit (if !own <> 1 then 1 else if !received <> 0 then 2 else 0)
  | child ->
      let _, status = Unix.waitpid [] child in
      Lwt_main.run (Lwt_unix.sleep 0.05);
      Lwt_unix.stop_notification id;
      let failed message =
        prerr_endline ("FAILED: " ^ message);
        exit 1
      in
      (match status with
      | Unix.WEXITED 0 -> ()
      | Unix.WEXITED 1 ->
          failed "the child does not receive a notification of its own"
      | Unix.WEXITED 2 ->
          failed "the child handles the notification pending in the parent"
      | _ -> failed "the child does not exit normally");
      if !received <> 1
      then
        failed
          (Printf.sprintf "the parent received its notification %d times"
             !received);
      print_endline "a notification pending at a fork: ok"
