(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* The per-domain slot, on one domain. test_lwt_dls_domains.ml checks it on
   two. *)

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
  (* as a cell, for one domain *)
  let k = Lwt_dls.new_key (fun () -> 41) in
  check "initializer" (Lwt_dls.get k = 41);
  Lwt_dls.set k 42;
  check "set then get" (Lwt_dls.get k = 42);
  (* distinct slots do not alias *)
  let k2 = Lwt_dls.new_key (fun () -> 0) in
  Lwt_dls.set k2 7;
  check "distinct slots" (Lwt_dls.get k = 42 && Lwt_dls.get k2 = 7);
  (* a slot holding a mutable record, as the core uses it *)
  let r = Lwt_dls.new_key (fun () -> ref 0) in
  Lwt_dls.get r := 5;
  check "record in a slot" (!(Lwt_dls.get r) = 5);
  print_endline "lwt_dls: ok"
