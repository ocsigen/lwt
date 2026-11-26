(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* An engine may forward a signal itself (Lwt_engine.forwards_signal), and its
   loop then needs no process-wide handler. A later loop with an ordinary
   engine still needs one: the handler is installed by the first subscription
   that needs it. Needs other domains, hence OCaml 5. *)

let failures = ref 0

let check name b =
  if not b
  then (
    Printf.eprintf "FAILED: %s\n" name;
    incr failures)

(* An engine that says it forwards SIGUSR2, as one sharing a handler with
   another library would. This one forwards nothing. *)
class forwarding =
  object
    inherit Lwt_engine.select
    method! forwards_signal signum = signum = Sys.sigusr2
  end

let () =
  ignore (Unix.alarm 30);
  let subscribed = Atomic.make 0 in
  let received = Atomic.make false in
  let wait_for n =
    while Atomic.get subscribed < n do
      Domain.cpu_relax ()
    done
  in
  let a =
    Domain.spawn (fun () ->
      Lwt_engine.set (new forwarding);
      let id = Lwt_unix.on_signal Sys.sigusr2 ignore in
      Atomic.incr subscribed;
      (* Stay subscribed until the other loop has had its signal. *)
      Lwt_main.run
        (let rec wait () =
           if Atomic.get received
           then Lwt.return_unit
           else Lwt.bind (Lwt_unix.sleep 0.01) wait
         in
         wait ());
      Lwt_unix.disable_signal_handler id)
  in
  wait_for 1;
  let b =
    Domain.spawn (fun () ->
      let id =
        Lwt_unix.on_signal Sys.sigusr2 (fun _ -> Atomic.set received true)
      in
      Atomic.incr subscribed;
      Lwt_main.run
        (let rec wait n =
           if Atomic.get received || n = 0
           then Lwt.return_unit
           else Lwt.bind (Lwt_unix.sleep 0.01) (fun () -> wait (n - 1))
         in
         wait 300);
      Lwt_unix.disable_signal_handler id)
  in
  wait_for 2;
  Unix.kill (Unix.getpid ()) Sys.sigusr2;
  Domain.join b;
  Atomic.set received true;
  Domain.join a;
  check "a loop subscribing after one whose engine forwards gets the signal"
    (Atomic.get received);
  if !failures > 0 then exit 1;
  print_endline "a signal with a forwarding engine on another loop: ok"
