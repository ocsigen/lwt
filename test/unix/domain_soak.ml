(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* Domains spawned and let die, over and over, each using jobs, sockets, a
   worker, a signal and an exit hook first. This
   looks for leaks that a single pass cannot show: a descriptor or a thread
   leaked per domain, a channel never retired, a subscription outliving its
   loop. It compares the number of open descriptors and of live threads at the
   end with the start.

   The default duration is short; LWT_SOAK_SECONDS makes it longer:

     LWT_SOAK_SECONDS=60 dune exec test/unix/domain_soak.exe

   Needs a second domain, hence OCaml 5. *)

open Lwt.Infix

let failures = ref 0

let check name b =
  if not b
  then (
    Printf.eprintf "FAILED: %s\n" name;
    incr failures)

let budget =
  match Sys.getenv_opt "LWT_SOAK_SECONDS" with
  | Some s -> ( try float_of_string s with _ -> 1.5)
  | None -> 1.5

(* Counted from /proc, which is exact and needs no privileges. Without /proc
   (macOS), both counts are -1 and the checks pass trivially. *)
let open_descriptors () =
  match Sys.readdir "/proc/self/fd" with
  | entries -> Array.length entries
  | exception _ -> -1

let live_threads () =
  match Sys.readdir "/proc/self/task" with
  | entries -> Array.length entries
  | exception _ -> -1

(* One round: a domain that uses everything a loop can own, then dies. *)
let round i =
  Domain.join
    (Domain.spawn (fun () ->
       (* Its own channel and its own jobs. *)
       let path = Filename.temp_file "lwt-soak" ".txt" in
       let work =
         Lwt_unix.openfile path [Unix.O_WRONLY; Unix.O_TRUNC] 0o600
         >>= fun fd ->
         Lwt_unix.write fd (Bytes.of_string "soak") 0 4 >>= fun _ ->
         Lwt_unix.close fd
       in
       (* Its own socket I/O. *)
       let io =
         let a, b = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
         let buf = Bytes.create 4 in
         Lwt_unix.write b (Bytes.of_string "ping") 0 4 >>= fun _ ->
         Lwt_unix.read a buf 0 4 >>= fun _ ->
         Lwt_unix.close a >>= fun () -> Lwt_unix.close b
       in
       (* Its own preemptive worker, which keeps a domain alive if it is not
          ended. *)
       let detached = Lwt_preemptive.detach (fun x -> x * 2) i in
       (* Its own signal subscription, dropped with the loop. *)
       let sub = Lwt_unix.on_signal Sys.sigusr2 (fun _ -> ()) in
       (* Its own exit hook, which runs an Lwt loop as it goes. *)
       Lwt_main.at_exit (fun () -> Lwt_unix.sleep 0.001);
       let n = Lwt_main.run (Lwt.both work io >>= fun _ -> detached) in
       Lwt_unix.disable_signal_handler sub;
       (try Sys.remove path with _ -> ());
       assert (n = i * 2)))

let () =
  (* A hang shows as a failure. *)
  ignore (Unix.alarm (int_of_float budget + 120));
  (* A few rounds first, so that the baseline includes everything that is
     allocated once per process rather than once per domain: the pool of job
     threads in particular, which grows to the number of jobs in flight at once
     and keeps its threads, so a single round does not show its size. *)
  for i = 1 to 3 do
    round i
  done;
  Gc.full_major ();
  let fd0 = open_descriptors () and th0 = live_threads () in
  let t0 = Unix.gettimeofday () in
  let rounds = ref 0 in
  while Unix.gettimeofday () -. t0 < budget do
    incr rounds; round !rounds
  done;
  Gc.full_major ();
  let fd1 = open_descriptors () and th1 = live_threads () in
  Printf.printf "%d rounds in %.1fs: descriptors %d -> %d, threads %d -> %d\n"
    !rounds budget fd0 fd1 th0 th1;
  check "several rounds ran" (!rounds >= 3);
  (* Some slack: the runtime may keep a thread or a pipe of its own, and the
     job pool may still gain a thread when two rounds' jobs overlap more than
     during the warm-up. A leak per domain would show as tens or hundreds, one
     per round. *)
  check "descriptors did not accumulate" (fd1 <= fd0 + 2);
  check "threads did not accumulate" (th1 <= th0 + 4);
  if !failures > 0 then exit 1;
  print_endline "soak, domains coming and going: ok"
