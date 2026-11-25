(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* Many job completions at once, from worker threads, while the notification
   buffer is full. Each sender grows the buffer with its mutex released, so
   several of them may allocate at the same time. Every notification must
   still be delivered once.

   The jobs open a FIFO for reading. Each one blocks in open() on its own
   worker thread until a writer opens the FIFO, so opening it for writing
   releases them all at once.

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
let readers = 64

let () =
  (* A hang shows as a failure. *)
  ignore (Unix.alarm 30);
  (* The jobs must run on worker threads, whatever LWT_ASYNC_METHOD says. *)
  Lwt_unix.set_default_async_method Lwt_unix.Async_detach;
  let fifo = Filename.temp_file "lwt-notification-growth" ".fifo" in
  Sys.remove fifo;
  Unix.mkfifo fifo 0o600;
  at_exit (fun () -> try Unix.unlink fifo with Unix.Unix_error _ -> ());
  let opens =
    List.init readers (fun _ -> Lwt_unix.openfile fifo [Unix.O_RDONLY] 0)
  in
  (* A job only sends a notification once [run_job] has seen it still running,
     at the loop's next iteration. Before that, the loop reads its result
     itself. *)
  Lwt_main.run (Lwt.pause ());
  (* A job that ran synchronously, for lack of a worker, sends no notification
     either. *)
  let pending =
    List.length (List.filter (fun p -> Lwt.state p = Lwt.Sleep) opens)
  in
  check
    (Printf.sprintf "every open waits on a worker thread (%d of %d)" pending
       readers)
    (pending = readers);
  (* Let the workers reach open(). *)
  Unix.sleepf 0.2;
  let received = ref 0 in
  let id = Lwt_unix.make_notification (fun () -> incr received) in
  check
    (Printf.sprintf "the buffer starts with %d cells (%d)" cells
       (buffer_size id))
    (buffer_size id = cells);
  for _ = 1 to cells do
    Lwt_unix.send_notification id
  done;
  check "the main thread fills the buffer without growing it"
    (buffer_size id = cells);
  (* The buffer is full: release every reader at once. *)
  let writer = Unix.openfile fifo [Unix.O_WRONLY] 0 in
  (* Let the readers send their notifications before the loop runs. *)
  Unix.sleepf 0.2;
  check "the readers grew the buffer before the loop ran"
    (buffer_size id > cells);
  let all_opened =
    Lwt.pick
      [ Lwt.map (fun fds -> Some fds) (Lwt.all opens)
      ; Lwt.map (fun () -> None) (Lwt_unix.sleep 10.) ]
  in
  let fds = Lwt_main.run all_opened in
  check "every open completes" (fds <> None);
  check
    (Printf.sprintf "every buffered notification is delivered (%d)" !received)
    (!received = cells);
  Option.iter (List.iter (fun fd -> Lwt_main.run (Lwt_unix.close fd))) fds;
  Unix.close writer;
  Lwt_unix.stop_notification id;
  if !failures > 0 then exit 1;
  print_endline "job completions while the notification buffer is full: ok"
