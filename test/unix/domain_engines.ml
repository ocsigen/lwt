(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* Each domain gets its own engine, and sees none of the other's
   registrations. *)

let failures = ref 0

let check name b =
  if not b
  then (
    Printf.eprintf "FAILED: %s\n" name;
    incr failures)

let () =
  let mine = Lwt_engine.get () in
  (* a fresh domain builds its own engine rather than inheriting ours *)
  let theirs = Domain.join (Domain.spawn (fun () -> Lwt_engine.get ())) in
  check "a fresh domain gets its own engine" (mine != theirs);
  (* registrations are per engine, so a fresh domain must see none of ours *)
  (* Relative to what is already registered here: Lwt_unix, once linked,
     registers its notification descriptor on this domain's engine. *)
  let base = Lwt_engine.readable_count () in
  let r, w = Unix.pipe ~cloexec:true () in
  let event = Lwt_engine.on_readable r (fun _ -> ()) in
  check "we see our own registration" (Lwt_engine.readable_count () = base + 1);
  let seen =
    Domain.join (Domain.spawn (fun () -> Lwt_engine.readable_count ()))
  in
  check "a fresh domain sees none of our registrations" (seen = 0);
  (* and a registration made over there must not appear here *)
  let r2, w2 = Unix.pipe ~cloexec:true () in
  let over_there =
    Domain.join
      (Domain.spawn (fun () ->
         let _ = Lwt_engine.on_readable r2 (fun _ -> ()) in
         Lwt_engine.readable_count ()))
  in
  check "the other domain sees its own registration" (over_there = 1);
  check "and we still see only ours" (Lwt_engine.readable_count () = base + 1);
  Lwt_engine.stop_event event;
  List.iter (fun fd -> try Unix.close fd with _ -> ()) [r; w; r2; w2];
  (* A spawned domain that installs its own engine, then exits. The exit must
     destroy the engine the domain has then (otherwise its epoll descriptor
     leaks), and not destroy again the one that [set] already destroyed. With
     [~destroy:false], the exit destroys both. Counted on /proc where there is
     one; elsewhere the test only checks that nothing crashes. *)
  let open_descriptors () =
    match Sys.readdir "/proc/self/fd" with
    | entries -> Some (Array.length entries)
    | exception Sys_error _ -> None
  in
  let engines =
    (fun () -> (new Lwt_engine.select :> Lwt_engine.t))
    ::
    (if Lwt_sys.have `libev
     then [(fun () -> (new Lwt_engine.libev () :> Lwt_engine.t))]
     else [])
  in
  List.iter
    (fun destroy ->
       List.iter
         (fun make ->
            let before = open_descriptors () in
            for _ = 1 to 20 do
              Domain.join
                (Domain.spawn (fun () ->
                   Lwt_engine.set ~destroy (make ());
                   Lwt_main.run (Lwt_unix.sleep 0.001)))
            done;
            let after = open_descriptors () in
            check
              (Printf.sprintf
                 "domains that set their engine (destroy:%b) and exit leak no descriptor"
                 destroy)
              (match before, after with Some b, Some a -> a <= b | _ -> true))
         engines)
    [true; false];
  (* A [Domain.at_exit] callback registered before the domain first uses Lwt
     runs after Lwt's own, which retire the engine. It must still find a
     working engine, not a destroyed one: for libev, that would be a freed
     loop. *)
  List.iter
    (fun make ->
       let late_run = Atomic.make false in
       Domain.join
         (Domain.spawn (fun () ->
            Domain.at_exit (fun () ->
              match Lwt_main.run (Lwt_unix.sleep 0.001) with
              | () -> Atomic.set late_run true
              | exception _ -> ());
            Lwt_engine.set (make ());
            Lwt_main.run (Lwt_unix.sleep 0.001)));
       check "Lwt still runs in a Domain.at_exit callback registered early"
         (Atomic.get late_run))
    engines;
  if !failures > 0 then exit 1;
  print_endline "per-domain engines: ok"
