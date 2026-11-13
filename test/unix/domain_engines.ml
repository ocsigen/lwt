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
  let r, w = Unix.pipe ~cloexec:true () in
  let event = Lwt_engine.on_readable r (fun _ -> ()) in
  check "we see our own registration" (Lwt_engine.readable_count () = 1);
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
  check "and we still see only ours" (Lwt_engine.readable_count () = 1);
  Lwt_engine.stop_event event;
  List.iter (fun fd -> try Unix.close fd with _ -> ()) [r; w; r2; w2];
  if !failures > 0 then exit 1;
  print_endline "per-domain engines: ok"
