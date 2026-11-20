(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* On one domain, so on every compiler. The tests with several domains are in
   test_per_domain.ml. *)

open Test

let suite =
  suite "lwt_per_domain"
    [ test "init runs at the first get, not at make" (fun () ->
        let calls = ref 0 in
        let v = Lwt_per_domain.make (fun () -> incr calls; ref 0) in
        let before = !calls in
        let first = Lwt_per_domain.get v in
        let second = Lwt_per_domain.get v in
        Lwt.return (before = 0 && !calls = 1 && first == second)) ]
