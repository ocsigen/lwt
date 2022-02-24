(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

exception Skip

type test = {
    test_name : string;
    skip_if_this_is_false : unit -> bool;
    sequential : bool;            (* Sequential is ignored in Alcotest *)
    run : [`Lwt of unit -> bool Lwt.t | `Direct of unit -> bool ];
  }

let test_direct test_name ?(only_if = fun () -> true) run =
  { test_name; skip_if_this_is_false = only_if; sequential = false; run = `Direct run; }

let test test_name ?(only_if = fun () -> true) ?(sequential = false) run =
  { test_name; skip_if_this_is_false = only_if; sequential; run = `Lwt run; }


(* Alcotest_lwt 1.5.0
 *
 * Copyright (c) 2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*)

module Alcotest_lwt_intf = struct
  module type V1 = sig
    include Alcotest_engine.V1.Cli.S with type return = unit Lwt.t

    val test_case :
      string ->
      Alcotest.speed_level ->
      (Lwt_switch.t -> 'a -> unit Lwt.t) ->
      'a test_case

    val test_case_sync :
      string -> Alcotest.speed_level -> ('a -> unit) -> 'a test_case
  end

  module type Alcotest_lwt = sig
    include V1

    (** {1 Versioned APIs} *)

    module V1 : V1
    (** An alias of the above API that provides a stability guarantees over major
        version changes. *)
  end
end

module Alcotest_lwt : sig
  include Alcotest_lwt_intf.Alcotest_lwt
end = struct
  let run_test fn args =
    let async_ex, async_waker = Lwt.wait () in
    let handle_exn ex =
      Printf.eprintf "Uncaught async exception: %s\n%s" (Printexc.to_string ex) (Printexc.get_backtrace ());
      if Lwt.state async_ex = Lwt.Sleep then Lwt.wakeup_exn async_waker ex
    in
    Lwt.async_exception_hook := handle_exn;
    Lwt_switch.with_switch (fun sw -> Lwt.pick [ fn sw args; async_ex ])

  module V1 = struct
    module Tester = Alcotest_engine.V1.Cli.Make (Alcotest.Unix_platform) (Lwt)
    include Tester

    let test_case_sync n s f = test_case n s (fun x -> Lwt.return (f x))
    let test_case n s f = test_case n s (run_test f)
  end

  include V1
end


let fold_left_map f accu l =
  let rec aux accu l_accu = function
    | [] -> accu, List.rev l_accu
    | x :: l ->
      let accu, x = f accu x in
      aux accu (x :: l_accu) l in
  aux accu [] l

let fold_left_mapi f accu l =
  let rec aux i accu l_accu = function
    | [] -> accu, List.rev l_accu
    | x :: l ->
      let accu, x = f i accu x in
      aux (i + 1) accu (x :: l_accu) l in
  aux 0 accu [] l


open Lwt.Infix

type suite = {
  suite_name : string;
  suite_tests : unit Alcotest_lwt.test_case list;
  skip_suite_if_this_is_false : unit -> bool;
  skip_indexes : int list;
}

let suite name ?(only_if = fun () -> true) tests =
  let to_test_case test =
    match test.run with
    | `Lwt run ->
      Alcotest_lwt.test_case test.test_name `Quick (fun _switch () ->
          run () >|= fun b ->
          Alcotest.(check bool) "success" b true)
    | `Direct run ->
      Alcotest_lwt.test_case_sync test.test_name `Quick (fun () ->
          let b = run () in
          Alcotest.(check bool) "success" b true)
  in
  let skip_indexes, tests =
    fold_left_mapi (fun i skip_indexes test ->
        if test.skip_if_this_is_false () then skip_indexes, to_test_case test
        else i :: skip_indexes, to_test_case test)
      [] (tests : test list)
  in
  {suite_name = name;
   suite_tests = tests;
   skip_suite_if_this_is_false = only_if;
   skip_indexes}

let run library_name suites =
  let skip = Hashtbl.create 16 in
  let skip_names, tests =
    fold_left_map (fun skip_names suite ->
        if suite.skip_suite_if_this_is_false () then
          begin
            Hashtbl.add skip suite.suite_name suite.skip_indexes;
            skip_names, (suite.suite_name, suite.suite_tests)
          end
        else
          suite.suite_name :: skip_names, (suite.suite_name, suite.suite_tests))
      [] suites in
  let filter ~name ~index =
    if List.mem name skip_names then `Skip
    else
      let skip_indexes = Hashtbl.find skip name in
      if List.mem index skip_indexes then `Skip
      else `Run
  in
  Alcotest_lwt.run ~filter library_name tests

let run library_name suites = Lwt_main.run @@ run library_name suites
let concurrent = run

let with_async_exception_hook hook f =
  let old_hook = !Lwt.async_exception_hook in
  Lwt.async_exception_hook := hook;
  Lwt.finalize f (fun () ->
      Lwt.async_exception_hook := old_hook;
      Lwt.return ())

let instrument = function
  | true -> Printf.ksprintf (fun _s -> true)
  | false -> Printf.ksprintf (fun s -> prerr_endline ("\n" ^ s); false)
