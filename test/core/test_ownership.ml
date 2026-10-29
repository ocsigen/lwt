(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* Changing a pending promise from another domain than its creator raises
   Lwt.Foreign_promise. Resolved promises and reads are not checked. *)

let failures = ref 0

let must_raise name f =
  match f () with
  | _ ->
      Printf.eprintf "FAILED: %s did not raise\n" name;
      incr failures
  | exception Lwt.Foreign_promise -> ()
  | exception e ->
      Printf.eprintf "FAILED: %s raised %s instead of Foreign_promise\n" name
        (Printexc.to_string e);
      incr failures

let must_not_raise name f =
  match f () with
  | _ -> ()
  | exception e ->
      Printf.eprintf "FAILED: %s raised %s\n" name (Printexc.to_string e);
      incr failures

(* Runs [f] on a fresh domain; [Domain.join] re-raises its exceptions here. *)
let on_another_domain f = Domain.join (Domain.spawn f)

let () =
  (* Changing a foreign pending promise raises. *)
  let pending, resolver = Lwt.wait () in
  let cancelable, _ = Lwt.task () in
  on_another_domain (fun () ->
    must_raise "bind on a foreign pending promise" (fun () ->
      Lwt.bind pending (fun () -> Lwt.return_unit));
    must_raise "map on a foreign pending promise" (fun () ->
      Lwt.map (fun () -> ()) pending);
    must_raise "wakeup of a foreign pending promise" (fun () ->
      Lwt.wakeup resolver ());
    must_raise "wakeup_later of a foreign pending promise" (fun () ->
      Lwt.wakeup_later resolver ());
    must_raise "try_bind on a foreign pending promise" (fun () ->
      Lwt.try_bind
        (fun () -> pending)
        (fun () -> Lwt.return_unit)
        (fun _ -> Lwt.return_unit));
    must_raise "on_success on a foreign pending promise" (fun () ->
      Lwt.on_success pending (fun () -> ()));
    must_raise "on_cancel on a foreign pending promise" (fun () ->
      Lwt.on_cancel pending (fun () -> ()));
    must_raise "cancel of a foreign cancelable promise" (fun () ->
      Lwt.cancel cancelable);
    must_raise "choose over a foreign pending promise" (fun () ->
      Lwt.choose [pending]));
  (* The same operations on the domain that created the promise do not. *)
  must_not_raise "bind at home" (fun () ->
    Lwt.bind pending (fun () -> Lwt.return_unit));
  must_not_raise "wakeup at home" (fun () -> Lwt.wakeup resolver ());
  (* A resolved promise has no owner, and can be used from any domain. *)
  let resolved = Lwt.return 42 in
  let rejected = Lwt.fail Exit in
  on_another_domain (fun () ->
    must_not_raise "bind on a foreign resolved promise" (fun () ->
      Lwt.bind resolved (fun v -> Lwt.return (v + 1)));
    must_not_raise "bind on a foreign rejected promise" (fun () ->
      Lwt.catch (fun () -> rejected) (fun _ -> Lwt.return_unit));
    must_not_raise "Lwt.return_unit, the shared constant" (fun () ->
      Lwt.bind Lwt.return_unit (fun () -> Lwt.return_unit));
    (* and the result is right *)
    match Lwt.state (Lwt.bind resolved (fun v -> Lwt.return (v + 1))) with
    | Lwt.Return 43 -> ()
    | _ ->
        Printf.eprintf
          "FAILED: a foreign resolved bind produced the wrong value\n";
        incr failures);
  (* Reading a foreign pending promise is not checked. *)
  let still_pending, _ = Lwt.wait () in
  on_another_domain (fun () ->
    must_not_raise "Lwt.state on a foreign pending promise" (fun () ->
      Lwt.state still_pending));
  (* Canceling a foreign [protected] mirror is refused before anything is
     touched, so the mirror still resolves with its source. *)
  let source, resolve_source = Lwt.wait () in
  let mirror = Lwt.protected source in
  on_another_domain (fun () ->
    must_raise "cancel of a foreign protected mirror" (fun () ->
      Lwt.cancel mirror));
  Lwt.wakeup resolve_source 42;
  if Lwt.state mirror <> Lwt.Return 42
  then begin
    Printf.eprintf
      "FAILED: a refused foreign cancel still detached the mirror\n";
    incr failures
  end;
  (* A refused [choose] over a promise of ours and a foreign one leaves nothing
     on ours, so resolving ours runs its callbacks and does not raise. *)
  let foreign = on_another_domain (fun () -> fst (Lwt.wait ())) in
  let ours, resolve_ours = Lwt.wait () in
  let attached_before = Lwt.map (fun () -> ()) ours in
  must_raise "choose over a promise of ours and a foreign one" (fun () ->
    Lwt.choose [ours; foreign]);
  must_not_raise "resolving ours after a refused choose" (fun () ->
    Lwt.wakeup resolve_ours ());
  if Lwt.state attached_before <> Lwt.Return ()
  then begin
    Printf.eprintf "FAILED: a refused choose broke the resolution of ours\n";
    incr failures
  end;
  (* It prints with its public name. *)
  if Printexc.to_string Lwt.Foreign_promise <> "Lwt.Foreign_promise"
  then begin
    Printf.eprintf "FAILED: Foreign_promise prints as %s\n"
      (Printexc.to_string Lwt.Foreign_promise);
    incr failures
  end;
  if !failures > 0
  then begin
    Printf.eprintf "%d ownership checks behaved wrongly\n" !failures;
    exit 1
  end;
  print_endline "ownership check: ok"
