(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* [Lwt_timeout] has a wheel per domain: a timeout fires on the loop of the
   domain that created it, and is refused on another one. [Lwt_preemptive]
   keeps a shared pool, but only the domain that initialized [Lwt_unix] may
   detach. *)

let failures = ref 0

let check name b =
  if not b
  then (
    Printf.eprintf "FAILED: %s\n" name;
    incr failures)

let refused_with_failure f =
  match f () with
  | _ -> false
  | exception Failure _ -> true
  | exception _ -> false

let refused_with_invalid_arg f =
  match f () with
  | _ -> false
  | exception Invalid_argument _ -> true
  | exception _ -> false

let () =
  (* Each domain's wheel runs on its own loop. The wheel ticks once a second,
     and a timeout with the minimum delay of one lands one bucket ahead of the
     current one, so it fires on the second tick. Hence the waits below. *)
  let fired_here = ref false in
  let t = Lwt_timeout.create 1 (fun () -> fired_here := true) in
  Lwt_timeout.start t;
  Lwt_main.run (Lwt_unix.sleep 2.5);
  check "our own timeout fires on our loop" !fired_here;
  (* The wheels are independent. We start a timeout and never run a loop,
     which leaves our wheel pending. A domain that comes next must still get
     its own wheel, and that wheel must tick. *)
  let fired_orphan = ref false in
  let orphan = Lwt_timeout.create 1 (fun () -> fired_orphan := true) in
  Lwt_timeout.start orphan;
  let fired_there = ref false in
  Domain.join
    (Domain.spawn (fun () ->
       let t = Lwt_timeout.create 1 (fun () -> fired_there := true) in
       Lwt_timeout.start t;
       Lwt_main.run (Lwt_unix.sleep 2.5)));
  check "another domain's timeout fires on its own loop" !fired_there;
  check "and ours did not fire, our loop never having run" (not !fired_orphan);
  (* A timeout does not cross a domain boundary. *)
  let ours = Lwt_timeout.create 1 (fun () -> ()) in
  check "starting our timeout on another domain is refused"
    (Domain.join
       (Domain.spawn (fun () ->
          refused_with_invalid_arg (fun () -> Lwt_timeout.start ours))));
  check "stopping our timeout on another domain is refused"
    (Domain.join
       (Domain.spawn (fun () ->
          refused_with_invalid_arg (fun () -> Lwt_timeout.stop ours))));
  check "changing our timeout on another domain is refused"
    (Domain.join
       (Domain.spawn (fun () ->
          refused_with_invalid_arg (fun () -> Lwt_timeout.change ours 2))));
  (* Only the domain that initialized Lwt_unix may detach. *)
  check "detaching from another domain is refused"
    (Domain.join
       (Domain.spawn (fun () ->
          refused_with_failure (fun () ->
            Lwt_main.run (Lwt_preemptive.detach (fun () -> 1) ())))));
  check "initializing the pool from another domain is refused"
    (Domain.join
       (Domain.spawn (fun () ->
          refused_with_failure (fun () -> Lwt_preemptive.init 1 4 ignore))));
  (* A refused [init] must not leave the pool looking initialized, with no
     worker: [detach] would then wait forever, hence the time limit. *)
  check "the main domain still detaches"
    (match
       Lwt_main.run
         (Lwt.pick
            [ Lwt_preemptive.detach (fun x -> x * 2) 21
            ; Lwt.map (fun () -> -1) (Lwt_unix.sleep 5.) ])
     with
    | 42 -> true
    | _ -> false
    | exception _ -> false);
  if !failures > 0 then exit 1;
  print_endline "per-domain timeouts, shared thread pool: ok"
