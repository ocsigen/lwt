(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* Two domains each run their own [Lwt_main.run] at the same time, with socket
   I/O and timers. A writer sleeps between messages, so each loop blocks
   in its engine. *)

open Lwt.Infix

let failures = ref 0

let check name b =
  if not b
  then (
    Printf.eprintf "FAILED: %s\n" name;
    incr failures)

let messages = 40
let payload = "ping"
let size = String.length payload

(* A concurrent writer and reader over this domain's own socketpair, driven by
   this domain's loop. Returns the number of bytes read. *)
let workload arrived =
  let a, b = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let msg = Bytes.of_string payload in
  let writer () =
    let rec go i =
      if i = 0
      then Lwt.return_unit
      else
        Lwt_unix.write b msg 0 size >>= fun _ ->
        Lwt_unix.sleep 0.001 >>= fun () -> go (i - 1)
    in
    go messages
  in
  let reader () =
    let buf = Bytes.create size in
    let rec go acc =
      if acc >= messages * size
      then Lwt.return acc
      else
        Lwt_unix.read a buf 0 size >>= fun n ->
        if n = 0 then Lwt.return acc else go (acc + n)
    in
    go 0
  in
  (* Do not enter the loop before the other domain has reached this point. *)
  Atomic.incr arrived;
  while Atomic.get arrived < 2 do
    Domain.cpu_relax ()
  done;
  let got =
    Lwt_main.run (Lwt.both (writer ()) (reader ()) >|= fun (_, n) -> n)
  in
  (* A nested run is still refused, per domain. The [pause] is needed: without
     it, the inner call is evaluated while building the argument, before the
     outer loop has started, and nothing is nested. *)
  let nested_refused =
    Lwt_main.run
      ( Lwt.pause () >>= fun () ->
        Lwt.catch
          (fun () ->
             ignore (Lwt_main.run Lwt.return_unit);
             Lwt.return_false)
          (function Failure _ -> Lwt.return_true | e -> Lwt.reraise e) )
  in
  (try Lwt_main.run (Lwt_unix.close a) with _ -> ());
  (try Lwt_main.run (Lwt_unix.close b) with _ -> ());
  got, nested_refused

let () =
  let arrived = Atomic.make 0 in
  let theirs = Domain.spawn (fun () -> workload arrived) in
  let ours_got, ours_nested = workload arrived in
  let theirs_got, theirs_nested = Domain.join theirs in
  check "our loop moved all its bytes" (ours_got = messages * size);
  check "their loop moved all its bytes" (theirs_got = messages * size);
  check "a nested run is still refused here" ours_nested;
  check "a nested run is still refused there" theirs_nested;
  if !failures > 0 then exit 1;
  print_endline "two concurrent Lwt_main.run loops: ok"
