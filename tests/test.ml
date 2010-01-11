(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Test
 * Copyright (C) 2009 Jérémie Dimino
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exceptions;
 * either version 2.1 of the License, or (at your option) any later
 * version. See COPYING file for details.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *)

open Lwt
open Lwt_io

type t = {
  name : string;
  run : unit -> bool Lwt.t;
}

type pack = {
  pack_name : string;
  pack_tests : t list;
}

let test ~name ~run = { name = name; run = run }
let pack ~name ~tests = { pack_name = name; pack_tests = tests }

let run ~name ~packs =
  let n = Lwt_main.run begin
    (* Count the number of tests in [packs] *)
    let total = List.fold_left (fun n { pack_tests = l } -> n + List.length l) 0 packs in

    lwt () = printlf "Running %d tests for library %S." total name in

    (* Iterate over packs: *)
    let rec loop_packs failures number = function
      | [] ->
          if failures = 0 then
            lwt () = printl "\r\027[JDone. All tests succeeded." in
            return 0
          else
            lwt () = printlf "\r\027[JDone. %d of %d tests failed." failures total in
            return 1
      | pack :: packs ->
          loop_tests failures pack.pack_name number packs pack.pack_tests

    (* Iterate over tests: *)
    and loop_tests failures pack_name number packs = function
      | [] ->
          loop_packs failures number packs
      | test :: tests ->
          lwt () = printf "\r\027[J(%d/%d) Running test %S from pack %S" number total test.name pack_name in
          lwt () = flush stdout in
          try_lwt
            test.run () >>= function
              | false ->
                  lwt () = printlf "\r\027[JTest %S from pack %S failed." test.name pack_name in
                  loop_tests failures pack_name (number + 1) packs tests
              | true ->
                  loop_tests failures pack_name (number + 1) packs tests
          with exn ->
            lwt () = printlf "\r\027[JTest %S from pack %S failed. It raised: %S" test.name pack_name (Printexc.to_string exn) in
            loop_tests failures pack_name (number + 1) packs tests
    in
    loop_packs 0 1 packs
  end in
  exit n
