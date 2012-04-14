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
  run : unit -> bool;
}

type suite = {
  suite_name : string;
  suite_tests : t list;
}

let test_direct ~name ~run = { name = name; run = run }
let test ~name ~run = { name = name; run = (fun () -> Lwt_main.run (run ())) }
let suite ~name ~tests = { suite_name = name; suite_tests = tests }

let run ~name ~suites =
  (* Count the number of tests in [suites] *)
  let total = List.fold_left (fun n { suite_tests = l } -> n + List.length l) 0 suites in

  Printf.printf "Running %d tests for library %S.\n%!" total name;

  (* Iterate over suites: *)
  let rec loop_suites failures number suites =
    match suites with
      | [] ->
          if failures = 0 then
            Printf.printf "\r\027[JDone. All tests succeeded.\n%!"
          else begin
            Printf.printf "\r\027[JDone. %d of %d tests failed.\n%!" failures total;
            exit 1
          end
      | suite :: suites ->
          loop_tests failures suite.suite_name number suites suite.suite_tests

  (* Iterate over tests: *)
  and loop_tests failures suite_name number suites tests =
    match tests with
      | [] ->
          loop_suites failures number suites
      | test :: tests ->
          Printf.printf "\r\027[J(%d/%d) Running test %S from suite %S%!" number total test.name suite_name;
          try
            if test.run () then
              loop_tests failures suite_name (number + 1) suites tests
            else begin
              Printf.printf "\r\027[J\027[31;1mTest %S from suite %S failed.\027[0m\n%!" test.name suite_name;
              loop_tests (failures + 1) suite_name (number + 1) suites tests
            end
          with exn ->
            Printf.printf "\r\027[J\027[31;1mTest %S from suite %S failed. It raised: %S.\027[0m\n%!" test.name suite_name (Printexc.to_string exn);
            loop_tests (failures + 1) suite_name (number + 1) suites tests
  in
  loop_suites 0 1 suites
