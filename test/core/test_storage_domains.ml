(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* Each domain owns a storage binding the same key to its own value, and must
   read that value back on every lookup. Systhreads would hit the race far too
   rarely, so this needs domains, hence OCaml 5. *)

[@@@alert "-trespassing"]

module Storage = Lwt.Private.Sequence_associated_storage

[@@@alert "+trespassing"]

let domains = 4
let iterations = 200_000

(* Spin until every domain has arrived, so that the lookups run at once. *)
let arrived = Atomic.make 0

let rendezvous () =
  ignore (Atomic.fetch_and_add arrived 1);
  while Atomic.get arrived < domains do
    Domain.cpu_relax ()
  done

(* Returns the number of lookups that did not see this domain's own value. *)
let look_up (key : int Lwt.key) (mine : int) () : int =
  let storage = Storage.modify_storage key (Some mine) Storage.empty_storage in
  rendezvous ();
  let mismatches = ref 0 in
  for _ = 1 to iterations do
    match Storage.get_from_storage key storage with
    | Some v when v = mine -> ()
    | Some _ | None -> incr mismatches
  done;
  !mismatches

let () =
  let key : int Lwt.key = Lwt.new_key () in
  let others =
    List.init (domains - 1) (fun i -> Domain.spawn (look_up key (i + 1)))
  in
  let here = look_up key domains () in
  let mismatches =
    List.fold_left (fun acc d -> acc + Domain.join d) here others
  in
  if mismatches > 0
  then begin
    Printf.eprintf
      "Lwt.key storage is not domain-safe: %d mismatched lookups out of %d, over %d domains\n"
      mismatches (domains * iterations) domains;
    exit 1
  end;
  (* A key created on one domain must also be usable from another, and two keys
     of the same type must not alias. *)
  let k1 : string Lwt.key = Lwt.new_key () in
  let k2 : string Lwt.key = Lwt.new_key () in
  let storage =
    Storage.modify_storage k1 (Some "one") Storage.empty_storage
    |> Storage.modify_storage k2 (Some "two")
  in
  let check () =
    assert (Storage.get_from_storage k1 storage = Some "one");
    assert (Storage.get_from_storage k2 storage = Some "two")
  in
  check ();
  Domain.join (Domain.spawn check);
  (* Keys created concurrently must have distinct ids, or they would shadow
     each other in a storage. *)
  let keys_per_domain = 20_000 in
  let make () = List.init keys_per_domain (fun _ -> Lwt.new_key ()) in
  let other = Domain.spawn make in
  let mine = make () in
  let keys = mine @ Domain.join other in
  let storage =
    List.fold_left
      (fun (storage, i) key ->
         Storage.modify_storage key (Some i) storage, i + 1)
      (Storage.empty_storage, 0) keys
    |> fst
  in
  let shadowed =
    List.fold_left
      (fun (n, i) key ->
         ( (if Storage.get_from_storage key storage = Some i then n else n + 1)
         , i + 1 ))
      (0, 0) keys
    |> fst
  in
  if shadowed > 0
  then begin
    Printf.eprintf
      "Lwt.new_key is not domain-safe: %d of %d concurrently created keys share an id with another\n"
      shadowed (List.length keys);
    exit 1
  end
