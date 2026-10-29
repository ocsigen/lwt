(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* Two domains see two independent slots, and two different tokens. *)

(* Lwt's own tests may use the internal slot. *)
[@@@alert "-trespassing"]

module Lwt_dls = Lwt_dls

[@@@alert "+trespassing"]

let check name b =
  if not b
  then (
    Printf.eprintf "FAILED: %s\n" name;
    exit 1)

let () =
  let k = Lwt_dls.new_key (fun () -> 0) in
  (* the initializer runs per domain, so a fresh domain sees the initial value
     and not whatever the main domain has written *)
  Lwt_dls.set k 1;
  let seen = Domain.join (Domain.spawn (fun () -> Lwt_dls.get k)) in
  check "a fresh domain runs the initializer" (seen = 0);
  check "the main domain keeps its value" (Lwt_dls.get k = 1);
  (* a domain's token is its own, and stable *)
  let mine = Lwt_dls.self_token () in
  let theirs = Domain.join (Domain.spawn Lwt_dls.self_token) in
  check "two domains, two tokens" (mine <> theirs);
  check "a stable token" (Lwt_dls.self_token () = mine);
  (* writes stay local: write on both domains at once, and check that neither
     sees the other's value *)
  let iterations = 200_000 in
  let arrived = Atomic.make 0 in
  let rendezvous () =
    ignore (Atomic.fetch_and_add arrived 1);
    while Atomic.get arrived < 2 do
      Domain.cpu_relax ()
    done
  in
  let write_and_read mine () =
    Lwt_dls.set k mine;
    rendezvous ();
    let bad = ref 0 in
    for _ = 1 to iterations do
      if Lwt_dls.get k <> mine then incr bad;
      Lwt_dls.set k mine
    done;
    !bad
  in
  let other = Domain.spawn (write_and_read 2) in
  let here = write_and_read 3 () in
  let there = Domain.join other in
  if here > 0 || there > 0
  then begin
    Printf.eprintf
      "the per-domain slot leaks between domains: %d + %d bad reads out of %d each\n"
      here there iterations;
    exit 1
  end;
  print_endline "lwt_dls, two domains: ok"
