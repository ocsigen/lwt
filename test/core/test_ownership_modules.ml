(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* A mutex, condition variable, mvar, pool, stream or switch is refused on a
   domain other than the one that created it, and works on its own. Needs a
   second domain, hence OCaml 5. *)

let failures = ref 0

let check name b =
  if not b
  then (
    Printf.eprintf "FAILED: %s\n" name;
    incr failures)

let contains needle m =
  let n = String.length needle in
  let rec go i =
    i + n <= String.length m && (String.sub m i n = needle || go (i + 1))
  in
  go 0

(* [true] if [f] is refused because the value belongs to another domain, as
   opposed to any other [Invalid_argument]. *)
let refused f =
  match f () with
  | _ -> false
  | exception Invalid_argument m -> contains "belongs to another domain" m
  | exception _ -> false

let on_other_domain f = Domain.join (Domain.spawn f)

let () =
  (* Everything below is created on this domain and used on another one. *)
  let m = Lwt_mutex.create () in
  let cv : int Lwt_condition.t = Lwt_condition.create () in
  let mv = Lwt_mvar.create_empty () in
  let pool = Lwt_pool.create 1 (fun () -> Lwt.return 0) in
  let stream, push = Lwt_stream.create () in
  push (Some 1);
  check "Lwt_mutex.lock is refused on another domain"
    (on_other_domain (fun () -> refused (fun () -> Lwt_mutex.lock m)));
  check "Lwt_mutex.unlock is refused on another domain"
    (on_other_domain (fun () -> refused (fun () -> Lwt_mutex.unlock m)));
  check "Lwt_condition.signal is refused on another domain"
    (on_other_domain (fun () -> refused (fun () -> Lwt_condition.signal cv 1)));
  check "Lwt_condition.broadcast is refused on another domain"
    (on_other_domain (fun () ->
       refused (fun () -> Lwt_condition.broadcast cv 1)));
  check "Lwt_condition.wait is refused on another domain"
    (on_other_domain (fun () -> refused (fun () -> Lwt_condition.wait cv)));
  check "Lwt_mvar.put is refused on another domain"
    (on_other_domain (fun () -> refused (fun () -> Lwt_mvar.put mv 1)));
  check "Lwt_mvar.take is refused on another domain"
    (on_other_domain (fun () -> refused (fun () -> Lwt_mvar.take mv)));
  check "Lwt_pool.use is refused on another domain"
    (on_other_domain (fun () ->
       refused (fun () -> Lwt_pool.use pool (fun _ -> Lwt.return_unit))));
  check "Lwt_pool.clear is refused on another domain"
    (on_other_domain (fun () -> refused (fun () -> Lwt_pool.clear pool)));
  (* One element is queued, so this goes through [consume]. *)
  check "reading a stream is refused on another domain"
    (on_other_domain (fun () -> refused (fun () -> Lwt_stream.get stream)));
  check "pushing to a stream is refused on another domain"
    (on_other_domain (fun () -> refused (fun () -> push (Some 2))));
  check "cloning a stream is refused on another domain"
    (on_other_domain (fun () -> refused (fun () -> Lwt_stream.clone stream)));
  (* An empty stream, so the other domain reaches [feed] rather than [consume]. *)
  let empty = fst (Lwt_stream.create ()) in
  check "feeding a stream is refused on another domain"
    (on_other_domain (fun () -> refused (fun () -> Lwt_stream.get empty)));
  (* [parse] would otherwise write the stream back, and [wrap_exn] would turn
     the error into an element, at each read, forever. *)
  check "parsing a stream is refused on another domain, by [parse] itself"
    (on_other_domain (fun () ->
       match Lwt_stream.parse empty Lwt_stream.get with
       | _ -> false
       | exception Invalid_argument m -> contains "Lwt_stream.parse" m));
  check "wrapping a stream is refused on another domain"
    (on_other_domain (fun () -> refused (fun () -> Lwt_stream.wrap_exn empty)));
  (* Same for a bounded push. *)
  let _bstream, bpush = Lwt_stream.create_bounded 4 in
  check "a bounded push is refused on another domain"
    (on_other_domain (fun () -> refused (fun () -> bpush#push 1)));
  check "resizing a bounded push is refused on another domain"
    (on_other_domain (fun () -> refused (fun () -> bpush#resize 8)));
  check "closing a bounded push is refused on another domain"
    (on_other_domain (fun () -> refused (fun () -> bpush#close)));
  (* Another domain can use the values it creates. *)
  check "another domain uses its own values"
    (on_other_domain (fun () ->
       let m = Lwt_mutex.create () in
       let mv = Lwt_mvar.create 7 in
       let s, p = Lwt_stream.create () in
       p (Some 3);
       p None;
       match
         ( Lwt.state (Lwt_mutex.lock m)
         , Lwt.state (Lwt_mvar.take mv)
         , Lwt.state (Lwt_stream.get s) )
       with
       | Lwt.Return (), Lwt.Return 7, Lwt.Return (Some 3) -> true
       | _ -> false));
  (* Ours still work on this domain. *)
  check "ours still work here"
    (match
       ( Lwt.state (Lwt_mutex.lock m)
       , Lwt.state (Lwt_stream.get stream)
       , Lwt_mutex.is_locked m )
     with
    | Lwt.Return (), Lwt.Return (Some 1), true -> true
    | _ -> false);
  (* Turning a switch off, or adding a hook, is refused on another domain, and
     the hook does not run. *)
  let sw = Lwt_switch.create () in
  let hook_ran_on = Atomic.make (-1) in
  Lwt_switch.add_hook (Some sw) (fun () ->
    Atomic.set hook_ran_on (Domain.self () :> int);
    Lwt.return_unit);
  check "turning another domain's switch off is refused"
    (on_other_domain (fun () -> refused (fun () -> Lwt_switch.turn_off sw)));
  check "adding a hook to another domain's switch is refused"
    (on_other_domain (fun () ->
       refused (fun () ->
         Lwt_switch.add_hook (Some sw) (fun () -> Lwt.return_unit))));
  check "and the hook did not run"
    (Atomic.get hook_ran_on = -1 && Lwt_switch.is_on sw);
  ignore (Lwt_switch.turn_off sw);
  check "the owner turns it off, and the hook runs on its domain"
    (Atomic.get hook_ran_on = (Domain.self () :> int));
  check "adding a hook to another domain's switch that is off is refused"
    (on_other_domain (fun () ->
       refused (fun () ->
         Lwt_switch.add_hook_or_exec (Some sw) (fun () -> Lwt.return_unit))));
  (* Waiting with another domain's mutex is refused after the waiter was
     queued: the waiter must not stay and swallow the next signal. *)
  let foreign_mutex =
    on_other_domain (fun () ->
      let fm = Lwt_mutex.create () in
      ignore (Lwt_mutex.lock fm);
      fm)
  in
  let cv : int Lwt_condition.t = Lwt_condition.create () in
  check "waiting with a foreign mutex is refused"
    (refused (fun () -> Lwt_condition.wait ~mutex:foreign_mutex cv));
  let real_waiter = Lwt_condition.wait cv in
  Lwt_condition.signal cv 5;
  check "and leaves no waiter behind to swallow the next signal"
    (Lwt.state real_waiter = Lwt.Return 5);
  if !failures > 0 then exit 1;
  print_endline "values bound to their domain: ok"
