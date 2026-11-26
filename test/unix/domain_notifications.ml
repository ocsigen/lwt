(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* A notification is delivered to the domain that created it, and to no other,
   so two loops are woken independently. Needs a second domain, hence
   OCaml 5. *)

external notification_channel_index :
   Lwt_unix.notification
  -> int
  = "lwt_unix_notification_channel_index"

external notification_pending :
   Lwt_unix.notification
  -> int
  = "lwt_unix_notification_pending"

let failures = ref 0

let check name b =
  if not b
  then (
    Printf.eprintf "FAILED: %s\n" name;
    incr failures)

(* An engine that cannot watch a descriptor, so that a loop cannot watch its
   channel. *)
class failing_engine =
  object
    inherit Lwt_engine.select
    method! private register_readable _ _ = failwith "failing engine"
  end

(* Atomic, because the two domains both look at these. *)
let ours = Atomic.make 0
let theirs = Atomic.make 0

let () =
  (* A hang, at a domain exit for example, must show as a failure. *)
  ignore
    (Thread.create
       (fun () ->
          Thread.delay 120.;
          prerr_endline "domain_notifications: timed out";
          exit 2)
       ());
  let n_ours = Lwt_unix.make_notification (fun () -> Atomic.incr ours) in
  (* The other domain makes its own, then sends both and drains its own channel.
     It must see its own notification and must not run ours. *)
  let seen_there =
    Domain.join
      (Domain.spawn (fun () ->
         let n_theirs =
           Lwt_unix.make_notification (fun () -> Atomic.incr theirs)
         in
         Lwt_unix.send_notification n_theirs;
         Lwt_unix.send_notification n_ours;
         Lwt_main.run (Lwt_unix.sleep 0.05);
         Atomic.get theirs))
  in
  check "the other domain ran its own notification" (seen_there = 1);
  check "the other domain did not run ours" (Atomic.get ours = 0);
  (* Ours is still waiting for us, and one iteration of our own loop delivers
     it. *)
  Lwt_main.run (Lwt_unix.sleep 0.05);
  check "our notification is delivered on our own loop" (Atomic.get ours = 1);
  check "and theirs was not run again here" (Atomic.get theirs = 1);
  (* A channel survives its loop stopping and starting again. *)
  Lwt_unix.send_notification n_ours;
  Lwt_main.run (Lwt_unix.sleep 0.05);
  check "the channel still works after the loop restarts" (Atomic.get ours = 2);
  (* One domain sends to the current loop's id as fast as it can, while loops
     come and go: no error, and no handler on the wrong domain. *)
  let current : Lwt_unix.notification option Atomic.t = Atomic.make None in
  let stop = Atomic.make false in
  let errors = Atomic.make 0 in
  let sent = Atomic.make 0 in
  let wrong_domain = Atomic.make 0 in
  let sender =
    Domain.spawn (fun () ->
      while not (Atomic.get stop) do
        match Atomic.get current with
        | None -> Domain.cpu_relax ()
        | Some id -> (
          match Lwt_unix.send_notification id with
          | () -> Atomic.incr sent
          | exception _ -> Atomic.incr errors)
      done)
  in
  for _ = 1 to 300 do
    Domain.join
      (Domain.spawn (fun () ->
         let me = (Domain.self () :> int) in
         let id =
           Lwt_unix.make_notification (fun () ->
             if (Domain.self () :> int) <> me then Atomic.incr wrong_domain)
         in
         Atomic.set current (Some id);
         Lwt_main.run (Lwt.pause ())))
  done;
  Atomic.set stop true;
  Domain.join sender;
  check "a sender racing with the exit of loops never gets an error"
    (Atomic.get errors = 0);
  check "and it did send" (Atomic.get sent > 0);
  check "no handler ran on another domain than its own"
    (Atomic.get wrong_domain = 0);
  (* A departed loop's notifications go with it: the handler below must never
     run, even once its slot has been reused 256 times (the generation has 8
     bits). *)
  let ran_on = Atomic.make (-1) in
  let stale =
    Domain.join
      (Domain.spawn (fun () ->
         Lwt_unix.make_notification (fun () ->
           Atomic.set ran_on (Domain.self () :> int))))
  in
  for _ = 1 to 255 do
    Domain.join
      (Domain.spawn (fun () -> ignore (Lwt_unix.make_notification ignore)))
  done;
  let ready = Atomic.make false in
  let victim =
    Domain.spawn (fun () ->
      ignore (Lwt_unix.make_notification ignore);
      Atomic.set ready true;
      Lwt_main.run (Lwt_unix.sleep 0.1))
  in
  while not (Atomic.get ready) do
    Domain.cpu_relax ()
  done;
  Lwt_unix.send_notification stale;
  Domain.join victim;
  check "a departed loop's handler never runs again, slot reused 256 times"
    (Atomic.get ran_on = -1);
  (* And the table does not grow with the domains that came and went. Measured
     after a warm-up, which takes what is allocated once per process. *)
  let live () = Gc.full_major (); (Gc.quick_stat ()).Gc.live_words in
  let round () =
    Domain.join
      (Domain.spawn (fun () ->
         for _ = 1 to 4 do
           ignore (Lwt_unix.make_notification ignore)
         done;
         Lwt_main.run (Lwt_unix.sleep 0.0001)))
  in
  for _ = 1 to 20 do
    round ()
  done;
  let w0 = live () in
  let rounds = 200 in
  for _ = 1 to rounds do
    round ()
  done;
  let w1 = live () in
  check "the table of notifiers does not grow with departed loops"
    (w1 - w0 < rounds * 8);
  (* An id of a departed loop is dropped by the C side, and not queued for the
     next owner of its slot: the generation tells them apart. Only the C side
     shows the difference, since the purge took the id out of the table. *)
  let gone =
    Domain.join (Domain.spawn (fun () -> Lwt_unix.make_notification ignore))
  in
  let next = Atomic.make None in
  let release = Atomic.make false in
  let owner =
    Domain.spawn (fun () ->
      Atomic.set next (Some (Lwt_unix.make_notification ignore));
      while not (Atomic.get release) do
        Domain.cpu_relax ()
      done)
  in
  let rec wait () =
    match Atomic.get next with
    | Some id -> id
    | None -> Domain.cpu_relax (); wait ()
  in
  let id = wait () in
  check "the next loop took the slot of the departed one"
    (notification_channel_index id = notification_channel_index gone);
  let before = notification_pending id in
  Lwt_unix.send_notification gone;
  check "an id of a departed loop is not queued for the next owner"
    (notification_pending id = before);
  Atomic.set release true;
  Domain.join owner;
  (* A [Domain.at_exit] callback registered before the loop's first use runs
     after the channel is retired. Creating a notification there fails, rather
     than hand out an id that no loop drains, or that names the next owner of
     the slot. *)
  let late = Atomic.make "not run" in
  Domain.join
    (Domain.spawn (fun () ->
       Domain.at_exit (fun () ->
         Atomic.set late
           (match Lwt_unix.make_notification ignore with
           | _ -> "created"
           | exception Failure _ -> "refused"));
       ignore (Lwt_unix.make_notification ignore)));
  check "a loop past the exit of its domain refuses to create a notification"
    (Atomic.get late = "refused");
  (* A loop whose engine cannot watch its channel frees the channel. There are
     more such loops than channels, and a loop with a working engine still gets
     one. *)
  let refused = ref 0 in
  for _ = 1 to 300 do
    if
      Domain.join
        (Domain.spawn (fun () ->
           Lwt_engine.set (new failing_engine);
           match Lwt_unix.make_notification ignore with
           | _ -> false
           | exception Failure msg -> msg = "failing engine"))
    then incr refused
  done;
  check "a loop whose engine fails gets no notification" (!refused = 300);
  check "and a loop with a working engine still gets one"
    (Domain.join
       (Domain.spawn (fun () ->
          match Lwt_unix.make_notification ignore with
          | _ -> true
          | exception _ -> false)));
  if !failures > 0 then exit 1;
  print_endline "per-domain notification channels: ok"
