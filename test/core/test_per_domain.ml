(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* A top-level mutex with one instance per domain. Needs a second domain, hence
   OCaml 5. *)

let failures = ref 0

let check name b =
  if not b
  then (
    Printf.eprintf "FAILED: %s\n" name;
    incr failures)

let made = Atomic.make 0
let lock = Lwt_per_domain.make (fun () -> Atomic.incr made; Lwt_mutex.create ())

(* What a library does with it: take the lock around some work. *)
let with_resource f = Lwt_mutex.with_lock (Lwt_per_domain.get lock) f

let use () =
  match Lwt.state (with_resource (fun () -> Lwt.return "done")) with
  | Lwt.Return s -> s
  | Lwt.Fail e -> "raised " ^ Printexc.to_string e
  | Lwt.Sleep -> "pending"

let () =
  check "the main domain uses its instance" (use () = "done");
  check "and gets the same one each time"
    (Lwt_per_domain.get lock == Lwt_per_domain.get lock);
  let mine = Lwt_per_domain.get lock in
  let theirs_differ, theirs_used =
    Domain.join
      (Domain.spawn (fun () -> Lwt_per_domain.get lock != mine, use ()))
  in
  check "another domain has an instance of its own" theirs_differ;
  check "which it can use: no Invalid_argument" (theirs_used = "done");
  check "one instance made per domain that asked" (Atomic.get made = 2);
  if !failures > 0 then exit 1;
  print_endline "a top-level value with one instance per domain: ok"
