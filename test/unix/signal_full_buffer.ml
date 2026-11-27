(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* A signal arriving while the loop's notification buffer is full. The handler
   may not allocate, so it marks the signal pending instead of growing the
   buffer. This checks that the handler leaves the buffer at its size, that the
   signal and the buffered notifications are all delivered, and that the signal
   goes to the handler of its arrival.

   The buffer starts with 4096 cells and never shrinks, so in a fresh process
   4096 notifications sent while the loop does not run fill it exactly. *)

external buffer_size :
   Lwt_unix.notification
  -> int
  = "lwt_unix_notification_buffer_size"
(* The size of the buffer that a notification goes to. Not part of the
   interface: only the tests use it. *)

let failures = ref 0

let check name b =
  if not b
  then (
    Printf.eprintf "FAILED: %s\n" name;
    incr failures)

let cells = 4096

let () =
  ignore (Unix.alarm 30);
  let received = ref 0 in
  let signalled = ref 0 in
  let id = Lwt_unix.make_notification (fun () -> incr received) in
  check
    (Printf.sprintf "the buffer starts with %d cells (%d)" cells
       (buffer_size id))
    (buffer_size id = cells);
  let handler = Lwt_unix.on_signal Sys.sigusr1 (fun _ -> incr signalled) in
  for _ = 1 to cells do
    Lwt_unix.send_notification id
  done;
  (* The buffer is full: the handler cannot store this one. *)
  Unix.kill (Unix.getpid ()) Sys.sigusr1;
  check "the signal handler does not grow the buffer" (buffer_size id = cells);
  let rec wait n =
    if (!signalled > 0 && !received = cells) || n = 0
    then Lwt.return_unit
    else Lwt.bind (Lwt_unix.sleep 0.01) (fun () -> wait (n - 1))
  in
  Lwt_main.run (wait 300);
  check "the signal reaches its handler" (!signalled >= 1);
  check
    (Printf.sprintf "every buffered notification is delivered (%d)" !received)
    (!received = cells);
  (* And the buffer still grows, outside a handler. *)
  for _ = 1 to cells + 1 do
    Lwt_unix.send_notification id
  done;
  received := 0;
  let rec wait n =
    if !received = cells + 1 || n = 0
    then Lwt.return_unit
    else Lwt.bind (Lwt_unix.sleep 0.01) (fun () -> wait (n - 1))
  in
  Lwt_main.run (wait 300);
  check "more notifications than the buffer held are all delivered"
    (!received = cells + 1);
  check "and the buffer grew for them" (buffer_size id > cells);
  Lwt_unix.disable_signal_handler handler;
  (* A signal that arrives on a full buffer goes to the handler of its arrival.
     If that handler is replaced before the loop runs, the new one must not
     receive it. *)
  let replaced = ref 0 in
  let later = ref 0 in
  let old_handler = Lwt_unix.on_signal Sys.sigusr1 (fun _ -> incr replaced) in
  received := 0;
  let size = buffer_size id in
  for _ = 1 to size do
    Lwt_unix.send_notification id
  done;
  Unix.kill (Unix.getpid ()) Sys.sigusr1;
  Lwt_unix.disable_signal_handler old_handler;
  let new_handler = Lwt_unix.on_signal Sys.sigusr1 (fun _ -> incr later) in
  let rec wait n =
    if !received = size || n = 0
    then Lwt.return_unit
    else Lwt.bind (Lwt_unix.sleep 0.01) (fun () -> wait (n - 1))
  in
  Lwt_main.run (wait 300);
  check
    (Printf.sprintf "the notifications before the signal are delivered (%d)"
       !received)
    (!received = size);
  check "a handler installed after the signal does not receive it" (!later = 0);
  check "nor does the handler it replaced" (!replaced = 0);
  Lwt_unix.disable_signal_handler new_handler;
  Lwt_unix.stop_notification id;
  if !failures > 0 then exit 1;
  print_endline "a signal with the notification buffer full: ok"
