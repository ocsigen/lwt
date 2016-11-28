(* Lightweight thread library for OCaml
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

type t = {
  name : string;
  only_if : unit -> bool;
  run : unit -> bool;
}

type suite = {
  suite_name : string;
  suite_tests : t list;
}

let test_direct name ?(only_if = fun () -> true) run = {name; only_if; run}

let test name ?(only_if = fun () -> true) run =
  {name; only_if; run = fun () -> Lwt_main.run (run ())}

let suite name tests = { suite_name = name; suite_tests = tests }

let run name suites =
  (* Count the number of tests in [suites] *)
  let total =
    List.fold_left (fun n {suite_tests = l; _} ->
      n + List.length l) 0 suites
  in

  Printf.printf "Running %d tests for library %S.\n%!" total name;

  (* Iterate over suites: *)
  let rec loop_suites failures skipped number suites =
    match suites with
      | [] ->
          if failures = 0 then
            Printf.printf "\r\027[JDone. %d test(s) skipped.\n%!" skipped
          else begin
            Printf.printf "\r\027[JDone. %d of %d tests failed.\n%!" failures total;
            exit 1
          end
      | suite :: suites ->
          loop_tests
            failures skipped suite.suite_name number suites suite.suite_tests

  (* Iterate over tests: *)
  and loop_tests failures skipped suite_name number suites tests =
    match tests with
      | [] ->
          loop_suites failures skipped number suites
      | test :: tests ->
          if not (test.only_if ()) then begin
            Printf.printf "\r\027[J(%d/%d) Skipping test %S from suite %S%!"
              number total test.name suite_name;
            loop_tests
              failures (skipped + 1) suite_name (number + 1) suites tests
          end
          else begin
            Printf.printf "\r\027[J(%d/%d) Running test %S from suite %S%!"
              number total test.name suite_name;
            try
              if test.run () then
                loop_tests failures skipped suite_name (number + 1) suites tests
              else begin
                Printf.printf
                  "\r\027[J\027[31;1mTest %S from suite %S failed.\027[0m\n%!"
                  test.name suite_name;
                loop_tests
                  (failures + 1) skipped suite_name (number + 1) suites tests
              end
            with exn ->
              Printf.printf
                "\r\027[J\027[31;1mTest %S from suite %S failed. It raised: %S.\027[0m\n%!"
                test.name suite_name (Printexc.to_string exn);
              loop_tests
                (failures + 1) skipped suite_name (number + 1) suites tests
          end
  in
  loop_suites 0 0 1 suites

let temp_name =
  let rng = Random.State.make_self_init () in
  fun () ->
    let number = Random.State.int rng 10000 in
    Printf.sprintf "_build/lwt-testing-%04d" number

let temp_file () =
  Filename.temp_file ~temp_dir:"_build" "lwt-testing-" ""

let temp_directory () =
  let rec attempt () =
    let path = temp_name () in
    try
      Unix.mkdir path 0o755;
      path
    with Unix.Unix_error (Unix.EEXIST, "mkdir", _) -> attempt ()
  in
  attempt ()
