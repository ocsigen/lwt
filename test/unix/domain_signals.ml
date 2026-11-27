(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* Every loop subscribed to a signal is woken, each on its own notification
   channel: two loops subscribed to the same signal both receive it. Needs a
   second domain, hence OCaml 5. *)

let failures = ref 0

let check name b =
  if not b
  then (
    Printf.eprintf "FAILED: %s\n" name;
    incr failures)

let arrived = Atomic.make 0
let got_a = Atomic.make false
let got_b = Atomic.make false

(* Subscribes, waits at the rendezvous, then runs a loop long enough to be woken
   by the signal that the subscribers send once both are subscribed. *)
let subscriber flag =
  let id = Lwt_unix.on_signal Sys.sigusr2 (fun _ -> Atomic.set flag true) in
  Atomic.incr arrived;
  while Atomic.get arrived < 2 do
    Domain.cpu_relax ()
  done;
  (* Each subscriber sends the signal once both have subscribed, so that neither
     can miss it. *)
  if Atomic.get arrived = 2 then Unix.kill (Unix.getpid ()) Sys.sigusr2;
  let deadline = 40 in
  let rec spin n =
    if Atomic.get flag || n = 0
    then ()
    else begin
      Lwt_main.run (Lwt_unix.sleep 0.05);
      spin (n - 1)
    end
  in
  spin deadline;
  Lwt_unix.disable_signal_handler id;
  Atomic.get flag

(* The child process of the check below that a departed loop's subscription
   goes with it. It runs before anything else, rather than after the checks
   that come first. *)
let () =
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "orphan-subscription"
  then begin
    Domain.join
      (Domain.spawn (fun () ->
         ignore (Lwt_unix.on_signal Sys.sigusr1 (fun _ -> ()));
         (* Run one iteration so that the subscription is in place. *)
         Lwt_main.run (Lwt_unix.sleep 0.01)));
    Unix.kill (Unix.getpid ()) Sys.sigusr1;
    (* If we are still here, the handler outlived its loop. *)
    Unix.sleepf 0.5;
    exit 0
  end

let () =
  (* A hang, at a domain exit for example, must show as a failure. The test
     does not run on Windows, so an alarm will do. *)
  ignore (Unix.alarm 120);
  let a = Domain.spawn (fun () -> subscriber got_a) in
  let b = Domain.spawn (fun () -> subscriber got_b) in
  let ra = Domain.join a and rb = Domain.join b in
  check "the first subscribed loop received the signal" ra;
  check "the second subscribed loop received it too" rb;
  (* Unsubscribing one loop must not unsubscribe the other, so do it again with
     one of them gone. *)
  Atomic.set arrived 0;
  Atomic.set got_a false;
  Atomic.set got_b false;
  let only_a =
    Domain.spawn (fun () ->
      let id =
        Lwt_unix.on_signal Sys.sigusr2 (fun _ -> Atomic.set got_a true)
      in
      Unix.kill (Unix.getpid ()) Sys.sigusr2;
      let rec spin n =
        if Atomic.get got_a || n = 0
        then ()
        else begin
          Lwt_main.run (Lwt_unix.sleep 0.05);
          spin (n - 1)
        end
      in
      spin 40;
      Lwt_unix.disable_signal_handler id;
      Atomic.get got_a)
  in
  check "a loop subscribing after the others have gone still receives"
    (Domain.join only_a);
  check "and the departed loops did not receive anything"
    (not (Atomic.get got_b));
  (* Two loops change their subscriptions to the same signal at the same time.
     If counting and recording the subscription are not one step, a subscribed
     loop can be
     left with no handler. One domain subscribes and unsubscribes as fast as it
     can; the other subscribes, sends the signal and waits for its handler,
     round after round. The test uses SIGURG, which is ignored by default, so a
     lost handler shows as a round that times out. *)
  let rounds = 200 in
  let done_ = Atomic.make false in
  let churner =
    Domain.spawn (fun () ->
      let n = ref 0 in
      while not (Atomic.get done_) do
        let id = Lwt_unix.on_signal Sys.sigurg (fun _ -> ()) in
        Lwt_unix.disable_signal_handler id;
        incr n
      done;
      !n)
  in
  let received =
    Domain.spawn (fun () ->
      let received = ref 0 in
      for _ = 1 to rounds do
        let got = ref false in
        let id = Lwt_unix.on_signal Sys.sigurg (fun _ -> got := true) in
        Unix.kill (Unix.getpid ()) Sys.sigurg;
        let rec spin n =
          if !got || n = 0
          then ()
          else begin
            Lwt_main.run (Lwt_unix.sleep 0.002);
            spin (n - 1)
          end
        in
        spin 500;
        if !got then incr received;
        Lwt_unix.disable_signal_handler id
      done;
      Atomic.set done_ true;
      !received)
  in
  let received = Domain.join received in
  let churned = Domain.join churner in
  check "a loop subscribing while another churns always has a handler"
    (received = rounds);
  check "the churning loop did churn" (churned > 0);
  (* A departed loop's subscription goes with it, and the process-wide handler
     with the last one, or the signal is swallowed. A child process subscribes
     on a spawned domain, lets the domain die, then sends itself SIGUSR1: it
     must be killed by the signal. *)
  (* The child process takes the branch at the top of this file. *)
  let child =
    Unix.create_process Sys.executable_name
      [|Sys.executable_name; "orphan-subscription"|]
      Unix.stdin Unix.stdout Unix.stderr
  in
  let _, status = Unix.waitpid [] child in
  check "a departed loop's signal subscription goes with it"
    (match status with
    | Unix.WSIGNALED n -> n = Sys.sigusr1
    | Unix.WEXITED _ | Unix.WSTOPPED _ -> false);
  (* A handler belongs to the loop that registered it, so disabling it from
     another domain is refused. Done anyway, it would be done half-way: the node
     pulled out of another loop's list, the subscription left with no handler,
     and the signal swallowed from then on. *)
  let foreign =
    Domain.join
      (Domain.spawn (fun () ->
         let id = Lwt_unix.on_signal Sys.sigurg (fun _ -> ()) in
         Lwt_main.run (Lwt_unix.sleep 0.01);
         id))
  in
  check "disabling another loop's handler is refused"
    (match Lwt_unix.disable_signal_handler foreign with
    | () -> false
    | exception Lwt.Domain_crossing -> true);
  if !failures > 0 then exit 1;
  print_endline "signals reach every subscribed loop: ok"
