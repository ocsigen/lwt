(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* Two domains each drive their own pauses, resolution loop and Lwt.key
   storage, serving the pauses by hand as [Lwt_main] does. *)

let check name b =
  if not b
  then (
    Printf.eprintf "FAILED: %s\n" name;
    exit 1)

(* A chain of [n] pauses, so the pause list is exercised. *)
let rec pauses n =
  if n = 0
  then Lwt.return 0
  else Lwt.bind (Lwt.pause ()) (fun () -> pauses (n - 1))

(* Serves the pauses until [p] is resolved. *)
let rec drive p =
  match Lwt.state p with
  | Lwt.Sleep -> Lwt.wakeup_paused (); drive p
  | Lwt.Return v -> v
  | Lwt.Fail e -> raise e

let key : int Lwt.key = Lwt.new_key ()

(* Each domain checks that the storage it set is the storage it reads back,
   across suspensions, while the other domain does the same with another
   value. *)
let work mine iterations () =
  let bad = ref 0 in
  for _ = 1 to iterations do
    let seen =
      drive
        (Lwt.with_value key (Some mine) (fun () ->
           Lwt.bind (pauses 20) (fun _ -> Lwt.return (Lwt.get key))))
    in
    if seen <> Some mine then incr bad
  done;
  !bad

let () =
  check "no pause pending at start" (Lwt.paused_count () = 0);
  let iterations = 2_000 in
  let other = Domain.spawn (work 1 iterations) in
  let here = work 2 iterations () in
  let there = Domain.join other in
  if here > 0 || there > 0
  then begin
    Printf.eprintf
      "the core scheduler is not per domain: %d + %d runs saw the other domain's storage, out of %d each\n"
      here there iterations;
    exit 1
  end;
  check "no pause left pending" (Lwt.paused_count () = 0);
  (* a fresh domain starts from a clean scheduler rather than inheriting *)
  ignore (Lwt.pause ());
  check "a pause is pending here" (Lwt.paused_count () = 1);
  let seen = Domain.join (Domain.spawn (fun () -> Lwt.paused_count ())) in
  check "a fresh domain sees no pause of ours" (seen = 0);
  Lwt.abandon_paused ();
  print_endline "core scheduler, two domains: ok"
