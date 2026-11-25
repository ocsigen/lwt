(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* Three notifications in one batch, the first two handlers raising. The
   receive empties the notification buffer, so a handler that raises must not
   lose the ones behind it: the third handler must run, and each exception must
   come out of the loop, in order. *)

let () =
  let third_runs = ref 0 in
  let notifications =
    [ Lwt_unix.make_notification (fun () -> failwith "first")
    ; Lwt_unix.make_notification (fun () -> failwith "second")
    ; Lwt_unix.make_notification (fun () -> incr third_runs) ]
  in
  List.iter Lwt_unix.send_notification notifications;
  (* Each exception ends a run of the loop, so run it until nothing is left.
     The bound only guards against a regression that loops. *)
  let rec run_loop raised n =
    if n = 0
    then List.rev raised
    else
      match Lwt_main.run (Lwt_unix.sleep 0.05) with
      | () -> List.rev raised
      | exception Failure msg -> run_loop (msg :: raised) (n - 1)
  in
  let raised = run_loop [] 10 in
  List.iter Lwt_unix.stop_notification notifications;
  if !third_runs <> 1
  then begin
    Printf.eprintf
      "FAILED: the handler behind the ones that raise ran %d times, not once\n"
      !third_runs;
    exit 1
  end;
  if raised <> ["first"; "second"]
  then begin
    prerr_endline
      ("FAILED: the exceptions that come out of the loop are ["
     ^ String.concat "; " raised ^ "], not [first; second]");
    exit 1
  end;
  print_endline "notification handlers that raise: ok"
