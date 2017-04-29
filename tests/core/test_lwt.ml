(* Lightweight thread library for OCaml
 * http://www.ocsigen.org/lwt
 * Module Test_lwt
 * Copyright (C) 2010 Jérémie Dimino, Pierre Chambart
 *               2017 Anton Bachin
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

open Test



(* An exception type fresh to this testing module. *)
exception Exception

let with_async_exception_hook hook f =
  let save = !Lwt.async_exception_hook in
  Lwt.async_exception_hook := hook;
  try
    let x = f () in
    Lwt.async_exception_hook := save;
    x
  with exn ->
    Lwt.async_exception_hook := save;
    raise exn

(* An add_loc function for [Lwt.backtrace_bind], etc. This should be defined
   literally at each place it is used, and it should be tested that the location
   of the re-raise is added to the backtrace. However, I believe that backtraces
   are broken right now, so neither of these is done. *)
let add_loc exn = try raise exn with exn -> exn



(* The list of all the tests in this file. This name is repeatedly shadowed as
   more and more tests are defined. *)
let tests = []



(* Tests for promises created with [Lwt.return], [Lwt.fail], and related
   functions, as well as state query (hard to test one without the other).
   These tests use assertions instead of relying on the correctness of a final
   [Lwt.return], not that it's particularly likely to be broken. *)

let trivial_promise_tests = [
  test "return" begin fun () ->
    assert (Lwt.state (Lwt.return "foo") = Lwt.Return "foo");
    Lwt.return true
  end;

  test "fail" begin fun () ->
    assert (Lwt.state (Lwt.fail Exception) = Lwt.Fail Exception);
    Lwt.return true
  end;

  test "of_result: resolved" begin fun () ->
    assert (Lwt.state (Lwt.of_result (Result.Ok "foo")) = Lwt.Return "foo");
    Lwt.return true
  end;

  test "of_result: failed" begin fun () ->
    assert
      (Lwt.state (Lwt.of_result (Result.Error Exception)) = Lwt.Fail Exception);
    Lwt.return true
  end;

  test "return_unit" begin fun () ->
    assert (Lwt.state Lwt.return_unit = Lwt.Return ());
    Lwt.return true
  end;

  test "return_true" begin fun () ->
    assert (Lwt.state Lwt.return_true = Lwt.Return true);
    Lwt.return true
  end;

  test "return_false" begin fun () ->
    assert (Lwt.state Lwt.return_false = Lwt.Return false);
    Lwt.return true
  end;

  test "return_none" begin fun () ->
    assert (Lwt.state Lwt.return_none = Lwt.Return None);
    Lwt.return true
  end;

  test "return_some" begin fun () ->
    assert (Lwt.state (Lwt.return_some "foo") = Lwt.Return (Some "foo"));
    Lwt.return true
  end;

  test "return_ok" begin fun () ->
    assert (Lwt.state (Lwt.return_ok "foo") = Lwt.Return (Result.Ok "foo"));
    Lwt.return true
  end;

  test "return_error" begin fun () ->
    assert
      (Lwt.state (Lwt.return_error "foo") = Lwt.Return (Result.Error "foo"));
    Lwt.return true
  end;

  test "fail_with" begin fun () ->
    assert (Lwt.state (Lwt.fail_with "foo") = Lwt.Fail (Failure "foo"));
    Lwt.return true
  end;

  test "fail_invalid_arg" begin fun () ->
    let p = Lwt.fail_invalid_arg "foo" in
    assert (Lwt.state p = Lwt.Fail (Invalid_argument "foo"));
    Lwt.return true
  end;
]
let tests = tests @ trivial_promise_tests



(* Tests for promises created with [Lwt.wait] and [Lwt.task], not including
   tests for cancelation of the latter. This also tests [Lwt.wakeup] and related
   functions, but not [Lwt.wakeup_later]. *)

let initial_promise_tests = [
  test "wait: pending" begin fun () ->
    let p, _ = Lwt.wait () in
    Lwt.return (Lwt.state p = Lwt.Sleep)
  end;

  test "task: pending" begin fun () ->
    let p, _ = Lwt.task () in
    Lwt.return (Lwt.state p = Lwt.Sleep)
  end;

  test "wait: resolve" begin fun () ->
    let p, r = Lwt.wait () in
    Lwt.wakeup r "foo";
    Lwt.return (Lwt.state p = Lwt.Return "foo")
  end;

  test "task: resolve" begin fun () ->
    let p, r = Lwt.task () in
    Lwt.wakeup r "foo";
    Lwt.return (Lwt.state p = Lwt.Return "foo")
  end;

  test "wait: fail" begin fun () ->
    let p, r = Lwt.wait () in
    Lwt.wakeup_exn r Exception;
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "task: fail" begin fun () ->
    let p, r = Lwt.task () in
    Lwt.wakeup_exn r Exception;
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "wait: complete" begin fun () ->
    let p, r = Lwt.wait () in
    Lwt.wakeup_result r (Result.Ok "foo");
    Lwt.return (Lwt.state p = Lwt.Return "foo")
  end;

  test "task: complete" begin fun () ->
    let p, r = Lwt.task () in
    Lwt.wakeup_result r (Result.Ok "foo");
    Lwt.return (Lwt.state p = Lwt.Return "foo")
  end;

  test "wait: double resolve" begin fun () ->
    let _, r = Lwt.wait () in
    Lwt.wakeup r "foo";
    try
      Lwt.wakeup r "foo";
      Lwt.return false
    with Invalid_argument _ ->
      Lwt.return true
  end;

  test "task: double resolve" begin fun () ->
    let _, r = Lwt.task () in
    Lwt.wakeup r "foo";
    try
      Lwt.wakeup r "foo";
      Lwt.return false
    with Invalid_argument _ ->
      Lwt.return true
  end;

  test "wait: double fail" begin fun () ->
    let _, r = Lwt.wait () in
    Lwt.wakeup_exn r Exception;
    try
      Lwt.wakeup_exn r Exception;
      Lwt.return false
    with Invalid_argument _ ->
      Lwt.return true
  end;

  test "task: double fail" begin fun () ->
    let _, r = Lwt.task () in
    Lwt.wakeup_exn r Exception;
    try
      Lwt.wakeup_exn r Exception;
      Lwt.return false
    with Invalid_argument _ ->
      Lwt.return true
  end;

  test "waiter_of_wakener" begin fun () ->
    let p, r = Lwt.wait () in
    Lwt.return (Lwt.waiter_of_wakener r == p)
  end;
]
let tests = tests @ initial_promise_tests



(* Tests for sequential composition functions, such as [Lwt.bind], but not
   including testing for interaction with cancelation and sequence-associated
   storage. Those tests come later. *)

let bind_tests = [
  test "bind: already resolved" begin fun () ->
    let p = Lwt.return "foo" in
    let p = Lwt.bind p (fun s -> Lwt.return (s ^ "bar")) in
    Lwt.return (Lwt.state p = Lwt.Return "foobar")
  end;

  (* A somewhat surprising behavior is that if [p] is resolved and [f] raises
     before evaluating to a promise, [bind p f] raises, instead of evaluating to
     a promise. On the other hand, if [p] is pending, and [f] raises, the
     exception is folded into the promise resulting from [bind]. *)
  test "bind: already resolved, f raises" begin fun () ->
    let p = Lwt.return "foo" in
    try
      Lwt.bind p (fun _ -> raise Exception) |> ignore;
      Lwt.return false
    with Exception ->
      Lwt.return true
  end;

  test "bind: already failed" begin fun () ->
    let p = Lwt.fail Exception in
    let p = Lwt.bind p (fun _ -> Lwt.return "foo") in
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "bind: pending" begin fun () ->
    let f_ran = ref false in
    let p, _ = Lwt.wait () in
    let p = Lwt.bind p (fun _ -> f_ran := true; Lwt.return ()) in
    Lwt.return (not !f_ran && Lwt.state p = Lwt.Sleep)
  end;

  test "bind: pending, resolves" begin fun () ->
    let p, r = Lwt.wait () in
    let p = Lwt.bind p (fun s -> Lwt.return (s ^ "bar")) in
    Lwt.wakeup r "foo";
    Lwt.return (Lwt.state p = Lwt.Return "foobar")
  end;

  test "bind: pending, resolves, f raises" begin fun () ->
    let p, r = Lwt.wait () in
    let p = Lwt.bind p (fun _ -> raise Exception) in
    Lwt.wakeup r ();
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "bind: pending, fails" begin fun () ->
    let p, r = Lwt.wait () in
    let p = Lwt.bind p (fun _ -> Lwt.return "foo") in
    Lwt.wakeup_exn r Exception;
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "bind: chain" begin fun () ->
    let p1, r1 = Lwt.wait () in
    let p2 = Lwt.bind p1 (fun s -> Lwt.return (s ^ "bar")) in
    let p3 = Lwt.bind p2 (fun s -> Lwt.return (s ^ "!!1")) in
    Lwt.wakeup r1 "foo";
    Lwt.return (Lwt.state p3 = Lwt.Return "foobar!!1")
  end;

  test "bind: suspended chain" begin fun () ->
    let p1, r = Lwt.wait () in
    let p2 = Lwt.return "foo" in
    let p3 = Lwt.bind p1 (fun () -> p2) in
    let p4 = Lwt.bind p1 (fun () -> p3) in
    Lwt.wakeup r ();
    Lwt.return (Lwt.state p4 = Lwt.Return "foo")
  end;

  test "bind: fanout" begin fun () ->
    let p1, r = Lwt.wait () in
    let p2 = Lwt.bind p1 (fun s -> Lwt.return (s ^ "bar")) in
    let p3 = Lwt.bind p1 (fun s -> Lwt.return (s ^ "!!1")) in
    let p4 = Lwt.bind p1 (fun s -> Lwt.return (s ^ "omg")) in
    Lwt.wakeup r "foo";
    Lwt.return
      (Lwt.state p2 = Lwt.Return "foobar" &&
       Lwt.state p3 = Lwt.Return "foo!!1" &&
       Lwt.state p4 = Lwt.Return "fooomg")
  end;

  test "bind: double pending" begin fun () ->
    let p1, r1 = Lwt.wait () in
    let p2, r2 = Lwt.wait () in
    let p = Lwt.bind p1 (fun _ -> p2) in
    Lwt.wakeup r1 "foo";
    assert (Lwt.state p = Lwt.Sleep);
    Lwt.wakeup r2 "bar";
    Lwt.return (Lwt.state p = Lwt.Return "bar")
  end;

  test "bind: same pending" begin fun () ->
    let p, r = Lwt.wait () in
    let p = Lwt.bind p (fun _ -> p) in
    Lwt.wakeup r "foo";
    Lwt.return (Lwt.state p = Lwt.Return "foo")
  end;

  test "bind: nested" begin fun () ->
    let p1, r1 = Lwt.wait () in
    let p2, r2 = Lwt.wait () in
    let p =
      Lwt.bind p1 (fun s -> Lwt.bind p2 (fun s' -> Lwt.return (s ^ s'))) in
    Lwt.wakeup r1 "foo";
    Lwt.wakeup r2 "bar";
    Lwt.return (Lwt.state p = Lwt.Return "foobar")
  end;

  (* This tests an implementation detail, namely the construction and flattening
     of a chain of proxy promises. *)
  test "bind: proxy chain" begin fun () ->
    let p1, r1 = Lwt.wait () in
    let p2, r2 = Lwt.wait () in
    let p3, r3 = Lwt.wait () in
    let p4 = Lwt.bind p1 (fun _ -> p3) in
    let p5 = Lwt.bind p2 (fun _ -> p4) in
    Lwt.wakeup r1 ();
    Lwt.wakeup r2 ();
    Lwt.wakeup r3 "bar";
    Lwt.return
      (Lwt.state p3 = Lwt.Return "bar" &&
       Lwt.state p4 = Lwt.Return "bar" &&
       Lwt.state p5 = Lwt.Return "bar")
  end;

  (* This tests an implementation detail, namely that proxy promise chaining
     does not form cycles. *)
  test "bind: cycle" begin fun () ->
    let p, r = Lwt.wait () in
    let p' = ref (Lwt.return ()) in
    p' := Lwt.bind p (fun _ -> !p');
    Lwt.wakeup r ();
    Lwt.return (Lwt.state !p' = Lwt.Sleep)
  end;
]
let tests = tests @ bind_tests

let backtrace_bind_tests = [
  test "backtrace_bind: resolved" begin fun () ->
    let p = Lwt.return "foo" in
    let p = Lwt.backtrace_bind add_loc p (fun s -> Lwt.return @@ s ^ "bar") in
    Lwt.return (Lwt.state p = Lwt.Return "foobar")
  end;

  test "backtrace_bind: failed" begin fun () ->
    let p = Lwt.fail Exception in
    let p = Lwt.backtrace_bind add_loc p (fun _ -> Lwt.return "foo") in
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "backtrace_bind: pending, resolves" begin fun () ->
    let p, r = Lwt.wait () in
    let p = Lwt.backtrace_bind add_loc p (fun s -> Lwt.return (s ^ "bar")) in
    Lwt.wakeup r "foo";
    Lwt.return (Lwt.state p = Lwt.Return "foobar")
  end;

  test "backtrace_bind: pending, resolves, f raises" begin fun () ->
    let p, r = Lwt.wait () in
    let p = Lwt.backtrace_bind add_loc p (fun () -> raise Exception) in
    Lwt.wakeup r ();
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "backtrace_bind: pending, fails" begin fun () ->
    let p, r = Lwt.wait () in
    let p = Lwt.backtrace_bind add_loc p (fun _ -> Lwt.return "foo") in
    Lwt.wakeup_exn r Exception;
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;
]
let tests = tests @ backtrace_bind_tests

let map_tests = [
  test "map: resolved" begin fun () ->
    let p = Lwt.return "foo" in
    let p = Lwt.map (fun s -> s ^ "bar") p in
    Lwt.return (Lwt.state p = Lwt.Return "foobar")
  end;

  test "map: resolved, f raises" begin fun () ->
    let p = Lwt.return "foo" in
    let p = Lwt.map (fun _ -> raise Exception) p in
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "map: failed" begin fun () ->
    let p = Lwt.fail Exception in
    let p = Lwt.map (fun _ -> "foo") p in
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "map: pending" begin fun () ->
    let f_ran = ref false in
    let p, _ = Lwt.wait () in
    let p = Lwt.map (fun _ -> f_ran := true) p in
    Lwt.return (not !f_ran && Lwt.state p = Lwt.Sleep)
  end;

  test "map: pending, resolves" begin fun () ->
    let p, r = Lwt.wait () in
    let p = Lwt.map (fun s -> s ^ "bar") p in
    Lwt.wakeup r "foo";
    Lwt.return (Lwt.state p = Lwt.Return "foobar")
  end;

  test "map: pending, resolves, f raises" begin fun () ->
    let p, r = Lwt.wait () in
    let p = Lwt.map (fun () -> raise Exception) p in
    Lwt.wakeup r ();
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "map: pending, fails" begin fun () ->
    let p, r = Lwt.wait () in
    let p = Lwt.map (fun _ -> Lwt.return "foo") p in
    Lwt.wakeup_exn r Exception;
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;
]
let tests = tests @ map_tests

let catch_tests = [
  test "catch: resolved" begin fun () ->
    let p =
      Lwt.catch
        (fun () -> Lwt.return "foo")
        (fun _ -> Lwt.return "bar")
    in
    Lwt.return (Lwt.state p = Lwt.Return "foo")
  end;

  test "catch: f raises" begin fun () ->
    let p =
      Lwt.catch
        (fun () -> raise Exception)
        (fun exn -> Lwt.return exn)
    in
    Lwt.return (Lwt.state p = Lwt.Return Exception)
  end;

  test "catch: failed" begin fun () ->
    let p =
      Lwt.catch
        (fun () -> Lwt.fail Exception)
        (fun exn -> Lwt.return exn)
    in
    Lwt.return (Lwt.state p = Lwt.Return Exception)
  end;

  test "catch: failed, h raises" begin fun () ->
    try
      ignore @@ Lwt.catch
        (fun () -> Lwt.fail Exit)
        (fun _ -> raise Exception);
      Lwt.return false
    with Exception ->
      Lwt.return true
  end;

  test "catch: pending" begin fun () ->
    let h_ran = ref false in
    let p =
      Lwt.catch
        (fun () -> fst (Lwt.wait ()))
        (fun _ -> h_ran := true; Lwt.return ())
    in
    Lwt.return (Lwt.state p = Lwt.Sleep)
  end;

  test "catch: pending, resolves" begin fun () ->
    let p, r = Lwt.wait () in
    let p =
      Lwt.catch
        (fun () -> p)
        (fun _ -> Lwt.return "bar")
    in
    Lwt.wakeup r "foo";
    Lwt.return (Lwt.state p = Lwt.Return "foo")
  end;

  test "catch: pending, fails" begin fun () ->
    let p, r = Lwt.wait () in
    let p =
      Lwt.catch
        (fun () -> p)
        (fun exn -> Lwt.return exn)
    in
    Lwt.wakeup_exn r Exception;
    Lwt.return (Lwt.state p = Lwt.Return Exception)
  end;

  test "catch: pending, fails, h raises" begin fun () ->
    let p, r = Lwt.wait () in
    let p =
      Lwt.catch
        (fun () -> p)
        (fun _ -> raise Exception)
    in
    Lwt.wakeup_exn r Exit;
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "catch: pending, fails, h pending" begin fun () ->
    let p1, r1 = Lwt.wait () in
    let p2, r2 = Lwt.wait () in
    let p =
      Lwt.catch
        (fun () -> p1)
        (fun _ -> p2)
    in
    Lwt.wakeup_exn r1 Exception;
    assert (Lwt.state p = Lwt.Sleep);
    Lwt.wakeup r2 "foo";
    Lwt.return (Lwt.state p = Lwt.Return "foo")
  end;
]
let tests = tests @ catch_tests

let backtrace_catch_tests = [
  test "backtrace_catch: resolved" begin fun () ->
    let p =
      Lwt.backtrace_catch add_loc
        (fun () -> Lwt.return "foo")
        (fun _ -> Lwt.return "bar")
    in
    Lwt.return (Lwt.state p = Lwt.Return "foo")
  end;

  test "backtrace_catch: f raises" begin fun () ->
    let p =
      Lwt.backtrace_catch add_loc
        (fun () -> raise Exception)
        (fun exn -> Lwt.return exn)
    in
    Lwt.return (Lwt.state p = Lwt.Return Exception)
  end;

  test "backtrace_catch: failed" begin fun () ->
    let p =
      Lwt.backtrace_catch add_loc
        (fun () -> Lwt.fail Exception)
        (fun exn -> Lwt.return exn)
    in
    Lwt.return (Lwt.state p = Lwt.Return Exception)
  end;

  test "backtrace_catch: pending" begin fun () ->
    let h_ran = ref false in
    let p =
      Lwt.backtrace_catch add_loc
        (fun () -> fst (Lwt.wait ()))
        (fun _ -> h_ran := true; Lwt.return ())
    in
    Lwt.return (Lwt.state p = Lwt.Sleep)
  end;

  test "backtrace_catch: pending, resolves" begin fun () ->
    let p, r = Lwt.wait () in
    let p =
      Lwt.backtrace_catch add_loc
        (fun () -> p)
        (fun _ -> Lwt.return "bar")
    in
    Lwt.wakeup r "foo";
    Lwt.return (Lwt.state p = Lwt.Return "foo")
  end;

  test "backtrace_catch: pending, fails" begin fun () ->
    let p, r = Lwt.wait () in
    let p =
      Lwt.backtrace_catch add_loc
        (fun () -> p)
        (fun exn -> Lwt.return exn)
    in
    Lwt.wakeup_exn r Exception;
    Lwt.return (Lwt.state p = Lwt.Return Exception)
  end;

  test "backtrace_catch: pending, fails, h raises" begin fun () ->
    let p, r = Lwt.wait () in
    let p =
      Lwt.backtrace_catch add_loc
        (fun () -> p)
        (fun _ -> raise Exception)
    in
    Lwt.wakeup_exn r Exit;
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;
]
let tests = tests @ backtrace_catch_tests

let try_bind_tests = [
  test "try_bind: resolved" begin fun () ->
    let p =
      Lwt.try_bind
        (fun () -> Lwt.return "foo")
        (fun s -> Lwt.return (s ^ "bar"))
        (fun _ -> Lwt.return "!!1")
    in
    Lwt.return (Lwt.state p = Lwt.Return "foobar")
  end;

  test "try_bind: resolved, f' raises" begin fun () ->
    try
      ignore @@ Lwt.try_bind
        (fun () -> Lwt.return ())
        (fun () -> raise Exception)
        (fun _ -> Lwt.return ());
      Lwt.return false
    with Exception ->
      Lwt.return true
  end;

  test "try_bind: failed" begin fun () ->
    let p =
      Lwt.try_bind
        (fun () -> Lwt.fail Exception)
        (fun _ -> Lwt.return Exit)
        (fun exn -> Lwt.return exn)
    in
    Lwt.return (Lwt.state p = Lwt.Return Exception)
  end;

  test "try_bind: f raises" begin fun () ->
    let p =
      Lwt.try_bind
        (fun () -> raise Exception)
        (fun _ -> Lwt.return Exit)
        (fun exn -> Lwt.return exn)
    in
    Lwt.return (Lwt.state p = Lwt.Return Exception)
  end;

  test "try_bind: failed, h raises" begin fun () ->
    try
      ignore @@ Lwt.try_bind
        (fun () -> Lwt.fail Exit)
        (fun _ -> Lwt.return ())
        (fun _ -> raise Exception);
      Lwt.return false
    with Exception ->
      Lwt.return true
  end;

  test "try_bind: pending" begin fun () ->
    let f_ran = ref false in
    let p, _ = Lwt.wait () in
    let p =
      Lwt.try_bind
        (fun () -> p)
        (fun _ -> f_ran := true; Lwt.return ())
        (fun _ -> f_ran := true; Lwt.return ())
    in
    Lwt.return (Lwt.state p = Lwt.Sleep)
  end;

  test "try_bind: pending, resolves" begin fun () ->
    let p, r = Lwt.wait () in
    let p =
      Lwt.try_bind
        (fun () -> p)
        (fun s -> Lwt.return (s ^ "bar"))
        (fun _ -> Lwt.return "!!1")
    in
    Lwt.wakeup r "foo";
    Lwt.return (Lwt.state p = Lwt.Return "foobar")
  end;

  test "try_bind: pending, resolves, f' raises" begin fun () ->
    let p, r = Lwt.wait () in
    let p =
      Lwt.try_bind
        (fun () -> p)
        (fun _ -> raise Exception)
        (fun _ -> Lwt.return ())
    in
    Lwt.wakeup r ();
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "try_bind: pending, resolves, f' pending" begin fun () ->
    let p1, r1 = Lwt.wait () in
    let p2, r2 = Lwt.wait () in
    let p =
      Lwt.try_bind
        (fun () -> p1)
        (fun () -> p2)
        (fun _ -> Lwt.return "bar")
    in
    Lwt.wakeup r1 ();
    assert (Lwt.state p = Lwt.Sleep);
    Lwt.wakeup r2 "foo";
    Lwt.return (Lwt.state p = Lwt.Return "foo")
  end;

  test "try_bind: pending, fails" begin fun () ->
    let p, r = Lwt.wait () in
    let p =
      Lwt.try_bind
        (fun () -> p)
        (fun _ -> Lwt.return Exit)
        (fun exn -> Lwt.return exn)
    in
    Lwt.wakeup_exn r Exception;
    Lwt.return (Lwt.state p = Lwt.Return Exception)
  end;

  test "try_bind: pending, fails, h raises" begin fun () ->
    let p, r = Lwt.wait () in
    let p =
      Lwt.try_bind
        (fun () -> p)
        (fun _ -> Lwt.return ())
        (fun _ -> raise Exception)
    in
    Lwt.wakeup_exn r Exit;
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "try_bind: pending, fails, h pending" begin fun () ->
    let p1, r1 = Lwt.wait () in
    let p2, r2 = Lwt.wait () in
    let p =
      Lwt.try_bind
        (fun () -> p1)
        (fun () -> Lwt.return "foo")
        (fun _ -> p2)
    in
    Lwt.wakeup_exn r1 Exception;
    assert (Lwt.state p = Lwt.Sleep);
    Lwt.wakeup r2 "bar";
    Lwt.return (Lwt.state p = Lwt.Return "bar")
  end;
]
let tests = tests @ try_bind_tests

let backtrace_try_bind_tests = [
  test "backtrace_try_bind: resolved" begin fun () ->
    let p =
      Lwt.backtrace_try_bind add_loc
        (fun () -> Lwt.return "foo")
        (fun s -> Lwt.return (s ^ "bar"))
        (fun _ -> Lwt.return "!!1")
    in
    Lwt.return (Lwt.state p = Lwt.Return "foobar")
  end;

  test "backtrace_try_bind: failed" begin fun () ->
    let p =
      Lwt.backtrace_try_bind add_loc
        (fun () -> Lwt.fail Exception)
        (fun _ -> Lwt.return Exit)
        (fun exn -> Lwt.return exn)
    in
    Lwt.return (Lwt.state p = Lwt.Return Exception)
  end;

  test "backtrace_try_bind: f raises" begin fun () ->
    let p =
      Lwt.backtrace_try_bind add_loc
        (fun () -> raise Exception)
        (fun _ -> Lwt.return Exit)
        (fun exn -> Lwt.return exn)
    in
    Lwt.return (Lwt.state p = Lwt.Return Exception)
  end;

  test "backtrace_try_bind: pending" begin fun () ->
    let f_ran = ref false in
    let p, _ = Lwt.wait () in
    let p =
      Lwt.backtrace_try_bind add_loc
        (fun () -> p)
        (fun _ -> f_ran := true; Lwt.return ())
        (fun _ -> f_ran := true; Lwt.return ())
    in
    Lwt.return (Lwt.state p = Lwt.Sleep)
  end;

  test "backtrace_try_bind: pending, resolves" begin fun () ->
    let p, r = Lwt.wait () in
    let p =
      Lwt.backtrace_try_bind add_loc
        (fun () -> p)
        (fun s -> Lwt.return (s ^ "bar"))
        (fun _ -> Lwt.return "!!1")
    in
    Lwt.wakeup r "foo";
    Lwt.return (Lwt.state p = Lwt.Return "foobar")
  end;

  test "backtrace_try_bind: pending, resolves, f' raises" begin fun () ->
    let p, r = Lwt.wait () in
    let p =
      Lwt.backtrace_try_bind add_loc
        (fun () -> p)
        (fun _ -> raise Exception)
        (fun _ -> Lwt.return ())
    in
    Lwt.wakeup r ();
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "backtrace_try_bind: pending, fails" begin fun () ->
    let p, r = Lwt.wait () in
    let p =
      Lwt.backtrace_try_bind add_loc
        (fun () -> p)
        (fun _ -> Lwt.return Exit)
        (fun exn -> Lwt.return exn)
    in
    Lwt.wakeup_exn r Exception;
    Lwt.return (Lwt.state p = Lwt.Return Exception)
  end;

  test "backtrace_try_bind: pending, fails, h raises" begin fun () ->
    let p, r = Lwt.wait () in
    let p =
      Lwt.backtrace_try_bind add_loc
        (fun () -> p)
        (fun _ -> Lwt.return ())
        (fun _ -> raise Exception)
    in
    Lwt.wakeup_exn r Exit;
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;
]
let tests = tests @ backtrace_try_bind_tests

let finalize_tests = [
  test "finalize: resolved" begin fun () ->
    let f'_ran = ref false in
    let p =
      Lwt.finalize
        (fun () -> Lwt.return "foo")
        (fun () -> f'_ran := true; Lwt.return ())
    in
    Lwt.return (!f'_ran && Lwt.state p = Lwt.Return "foo")
  end;

  test "finalize: resolved, f' fails" begin fun () ->
    let p =
      Lwt.finalize
        (fun () -> Lwt.return ())
        (fun () -> Lwt.fail Exception)
    in
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "finalize: resolved, f' raises" begin fun () ->
    try
      ignore @@ Lwt.finalize
        (fun () -> Lwt.return ())
        (fun () -> raise Exception);
      Lwt.return false
    with Exception ->
      Lwt.return true
  end;

  test "finalize: failed" begin fun () ->
    let f'_ran = ref false in
    let p =
      Lwt.finalize
        (fun () -> Lwt.fail Exception)
        (fun () -> f'_ran := true; Lwt.return ())
    in
    Lwt.return (!f'_ran && Lwt.state p = Lwt.Fail Exception)
  end;

  test "finalize: failed, f' fails" begin fun () ->
    let p =
      Lwt.finalize
        (fun () -> Lwt.fail Exit)
        (fun () -> Lwt.fail Exception)
    in
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "finalize: failed, f' raises" begin fun () ->
    try
      ignore @@ Lwt.finalize
        (fun () -> Lwt.fail Exit)
        (fun () -> raise Exception);
      Lwt.return false
    with Exception ->
      Lwt.return true
  end;

  test "finalize: pending" begin fun () ->
    let f'_ran = ref false in
    let p, _ = Lwt.wait () in
    let p =
      Lwt.finalize
        (fun () -> p)
        (fun () -> f'_ran := true; Lwt.return ())
    in
    Lwt.return (not !f'_ran && Lwt.state p = Lwt.Sleep)
  end;

  test "finalize: pending, resolves" begin fun () ->
    let f'_ran = ref false in
    let p, r = Lwt.wait () in
    let p =
      Lwt.finalize
        (fun () -> p)
        (fun () -> f'_ran := true; Lwt.return ())
    in
    Lwt.wakeup r "foo";
    Lwt.return (!f'_ran && Lwt.state p = Lwt.Return "foo")
  end;

  test "finalize: pending, resolves, f' fails" begin fun () ->
    let p, r = Lwt.wait () in
    let p =
      Lwt.finalize
        (fun () -> p)
        (fun () -> Lwt.fail Exception)
    in
    Lwt.wakeup r ();
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "finalize: pending, resolves, f' raises" begin fun () ->
    let p, r = Lwt.wait () in
    let p =
      Lwt.finalize
        (fun () -> p)
        (fun () -> raise Exception)
    in
    Lwt.wakeup r ();
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "finalize: pending, resolves, f' pending" begin fun () ->
    let p1, r1 = Lwt.wait () in
    let p2, r2 = Lwt.wait () in
    let p =
      Lwt.finalize
        (fun () -> p1)
        (fun () -> p2)
    in
    Lwt.wakeup r1 "foo";
    assert (Lwt.state p = Lwt.Sleep);
    Lwt.wakeup r2 ();
    Lwt.return (Lwt.state p = Lwt.Return "foo")
  end;

  test "finalize: pending, resolves, f' pending, fails" begin fun () ->
    let p1, r1 = Lwt.wait () in
    let p2, r2 = Lwt.wait () in
    let p =
      Lwt.finalize
        (fun () -> p1)
        (fun () -> p2)
    in
    Lwt.wakeup r1 ();
    assert (Lwt.state p = Lwt.Sleep);
    Lwt.wakeup_exn r2 Exception;
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "finalize: pending, fails" begin fun () ->
    let f'_ran = ref false in
    let p, r = Lwt.wait () in
    let p =
      Lwt.finalize
        (fun () -> p)
        (fun () -> f'_ran := true; Lwt.return ())
    in
    Lwt.wakeup_exn r Exception;
    Lwt.return (!f'_ran && Lwt.state p = Lwt.Fail Exception)
  end;

  test "finalize: pending, fails, f' fails" begin fun () ->
    let p, r = Lwt.wait () in
    let p =
      Lwt.finalize
        (fun () -> p)
        (fun () -> Lwt.fail Exception)
    in
    Lwt.wakeup_exn r Exit;
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "finalize: pending, fails, f' raises" begin fun () ->
    let p, r = Lwt.wait () in
    let p =
      Lwt.finalize
        (fun () -> p)
        (fun () -> raise Exception)
    in
    Lwt.wakeup_exn r Exit;
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "finalize: pending, fails, f' pending" begin fun () ->
    let p1, r1 = Lwt.wait () in
    let p2, r2 = Lwt.wait () in
    let p =
      Lwt.finalize
        (fun () -> p1)
        (fun () -> p2)
    in
    Lwt.wakeup_exn r1 Exception;
    assert (Lwt.state p = Lwt.Sleep);
    Lwt.wakeup r2 ();
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "finalize: pending, fails, f' pending, fails" begin fun () ->
    let p1, r1 = Lwt.wait () in
    let p2, r2 = Lwt.wait () in
    let p =
      Lwt.finalize
        (fun () -> p1)
        (fun () -> p2)
    in
    Lwt.wakeup_exn r1 Exit;
    assert (Lwt.state p = Lwt.Sleep);
    Lwt.wakeup_exn r2 Exception;
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;
]
let tests = tests @ finalize_tests

let backtrace_finalize_tests = [
  test "backtrace_finalize: resolved" begin fun () ->
    let f'_ran = ref false in
    let p =
      Lwt.backtrace_finalize add_loc
        (fun () -> Lwt.return "foo")
        (fun () -> f'_ran := true; Lwt.return ())
    in
    Lwt.return (!f'_ran && Lwt.state p = Lwt.Return "foo")
  end;

  test "backtrace_finalize: resolved, f' fails" begin fun () ->
    let p =
      Lwt.backtrace_finalize add_loc
        (fun () -> Lwt.return ())
        (fun () -> Lwt.fail Exception)
    in
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "backtrace_finalize: resolved, f' raises" begin fun () ->
    try
      ignore @@ Lwt.backtrace_finalize add_loc
        (fun () -> Lwt.return ())
        (fun () -> raise Exception);
      Lwt.return false
    with Exception ->
      Lwt.return true
  end;

  test "backtrace_finalize: failed" begin fun () ->
    let f'_ran = ref false in
    let p =
      Lwt.backtrace_finalize add_loc
        (fun () -> Lwt.fail Exception)
        (fun () -> f'_ran := true; Lwt.return ())
    in
    Lwt.return (!f'_ran && Lwt.state p = Lwt.Fail Exception)
  end;

  test "backtrace_finalize: failed, f' fails" begin fun () ->
    let p =
      Lwt.backtrace_finalize add_loc
        (fun () -> Lwt.fail Exit)
        (fun () -> Lwt.fail Exception)
    in
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "backtrace_finalize: failed, f' raises" begin fun () ->
    try
      ignore @@ Lwt.backtrace_finalize add_loc
        (fun () -> Lwt.fail Exit)
        (fun () -> raise Exception);
      Lwt.return false
    with Exception ->
      Lwt.return true
  end;

  test "backtrace_finalize: pending" begin fun () ->
    let f'_ran = ref false in
    let p, _ = Lwt.wait () in
    let p =
      Lwt.backtrace_finalize add_loc
        (fun () -> p)
        (fun () -> f'_ran := true; Lwt.return ())
    in
    Lwt.return (not !f'_ran && Lwt.state p = Lwt.Sleep)
  end;

  test "backtrace_finalize: pending, resolves" begin fun () ->
    let f'_ran = ref false in
    let p, r = Lwt.wait () in
    let p =
      Lwt.backtrace_finalize add_loc
        (fun () -> p)
        (fun () -> f'_ran := true; Lwt.return ())
    in
    Lwt.wakeup r "foo";
    Lwt.return (!f'_ran && Lwt.state p = Lwt.Return "foo")
  end;

  test "backtrace_finalize: pending, resolves, f' fails" begin fun () ->
    let p, r = Lwt.wait () in
    let p =
      Lwt.backtrace_finalize add_loc
        (fun () -> p)
        (fun () -> Lwt.fail Exception)
    in
    Lwt.wakeup r ();
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "backtrace_finalize: pending, resolves, f' raises" begin fun () ->
    let p, r = Lwt.wait () in
    let p =
      Lwt.backtrace_finalize add_loc
        (fun () -> p)
        (fun () -> raise Exception)
    in
    Lwt.wakeup r ();
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "backtrace_finalize: pending, fails" begin fun () ->
    let f'_ran = ref false in
    let p, r = Lwt.wait () in
    let p =
      Lwt.backtrace_finalize add_loc
        (fun () -> p)
        (fun () -> f'_ran := true; Lwt.return ())
    in
    Lwt.wakeup_exn r Exception;
    Lwt.return (!f'_ran && Lwt.state p = Lwt.Fail Exception)
  end;

  test "backtrace_finalize: pending, fails, f' fails" begin fun () ->
    let p, r = Lwt.wait () in
    let p =
      Lwt.backtrace_finalize add_loc
        (fun () -> p)
        (fun () -> Lwt.fail Exception)
    in
    Lwt.wakeup_exn r Exit;
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "backtrace_finalize: pending, fails, f' raises" begin fun () ->
    let p, r = Lwt.wait () in
    let p =
      Lwt.backtrace_finalize add_loc
        (fun () -> p)
        (fun () -> raise Exception)
    in
    Lwt.wakeup_exn r Exit;
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;
]
let tests = tests @ backtrace_finalize_tests

let on_success_tests = [
  test "on_success: resolved" begin fun () ->
    let f_ran = ref false in
    Lwt.on_success (Lwt.return ()) (fun () -> f_ran := true);
    Lwt.return !f_ran
  end;

  test "on_success: resolved, f raises" begin fun () ->
    let saw = ref None in
    with_async_exception_hook
      (fun exn -> saw := Some exn)
      (fun () -> Lwt.on_success (Lwt.return ()) (fun () -> raise Exception));
    Lwt.return (!saw = Some Exception)
  end;

  test "on_success: failed" begin fun () ->
    let f_ran = ref false in
    Lwt.on_success (Lwt.fail Exception) (fun () -> f_ran := true);
    Lwt.return (not !f_ran)
  end;

  test "on_success: pending" begin fun () ->
    let f_ran = ref false in
    Lwt.on_success (fst (Lwt.wait ())) (fun () -> f_ran := true);
    Lwt.return (not !f_ran)
  end;

  test "on_success: pending, resolves" begin fun () ->
    let f_ran = ref false in
    let p, r = Lwt.wait () in
    Lwt.on_success p (fun () -> f_ran := true);
    assert (not !f_ran);
    Lwt.wakeup r ();
    Lwt.return !f_ran
  end;

  test "on_success: pending, resolves, f raises" begin fun () ->
    let saw = ref None in
    let p, r = Lwt.wait () in
    Lwt.on_success p (fun () -> raise Exception);
    with_async_exception_hook
      (fun exn -> saw := Some exn)
      (fun () -> Lwt.wakeup r ());
    Lwt.return (!saw = Some Exception)
  end;

  test "on_success: pending, fails" begin fun () ->
    let f_ran = ref false in
    let p, r = Lwt.wait () in
    Lwt.on_success p (fun () -> f_ran := true);
    Lwt.wakeup_exn r Exception;
    Lwt.return (not !f_ran)
  end;
]
let tests = tests @ on_success_tests

let on_failure_tests = [
  test "on_failure: resolved" begin fun () ->
    let f_ran = ref false in
    Lwt.on_failure (Lwt.return ()) (fun _ -> f_ran := true);
    Lwt.return (not !f_ran)
  end;

  test "on_failure: failed" begin fun () ->
    let saw = ref None in
    Lwt.on_failure (Lwt.fail Exception) (fun exn -> saw := Some exn);
    Lwt.return (!saw = Some Exception)
  end;

  test "on_failure: failed, f raises" begin fun () ->
    let saw = ref None in
    with_async_exception_hook
      (fun exn -> saw := Some exn)
      (fun () -> Lwt.on_failure (Lwt.fail Exit) (fun _ -> raise Exception));
    Lwt.return (!saw = Some Exception)
  end;

  test "on_failure: pending" begin fun () ->
    let f_ran = ref false in
    Lwt.on_failure (fst (Lwt.wait ())) (fun _ -> f_ran := true);
    Lwt.return (not !f_ran)
  end;

  test "on_failure: pending, resolves" begin fun () ->
    let f_ran = ref false in
    let p, r = Lwt.wait () in
    Lwt.on_failure p (fun _ -> f_ran := true);
    Lwt.wakeup r ();
    Lwt.return (not !f_ran)
  end;

  test "on_failure: pending, fails" begin fun () ->
    let saw = ref None in
    let p, r = Lwt.wait () in
    Lwt.on_failure p (fun exn -> saw := Some exn);
    Lwt.wakeup_exn r Exception;
    Lwt.return (!saw = Some Exception)
  end;

  test "on_failure: pending, fails, f raises" begin fun () ->
    let saw = ref None in
    let p, r = Lwt.wait () in
    Lwt.on_failure p (fun _ -> raise Exception);
    with_async_exception_hook
      (fun exn -> saw := Some exn)
      (fun () -> Lwt.wakeup_exn r Exit);
    Lwt.return (!saw = Some Exception)
  end;
]
let tests = tests @ on_failure_tests

let on_termination_tests = [
  test "on_termination: resolved" begin fun () ->
    let f_ran = ref false in
    Lwt.on_termination (Lwt.return ()) (fun () -> f_ran := true);
    Lwt.return !f_ran
  end;

  test "on_termination: resolved, f raises" begin fun () ->
    let saw = ref None in
    with_async_exception_hook
      (fun exn -> saw := Some exn)
      (fun () ->
        Lwt.on_termination (Lwt.return ()) (fun () -> raise Exception));
    Lwt.return (!saw = Some Exception)
  end;

  test "on_termination: failed" begin fun () ->
    let f_ran = ref false in
    Lwt.on_termination (Lwt.fail Exception) (fun () -> f_ran := true);
    Lwt.return !f_ran
  end;

  test "on_termination: failed, f raises" begin fun () ->
    let saw = ref None in
    with_async_exception_hook
      (fun exn -> saw := Some exn)
      (fun () ->
        Lwt.on_termination (Lwt.fail Exit) (fun () -> raise Exception));
    Lwt.return (!saw = Some Exception)
  end;

  test "on_termination: pending" begin fun () ->
    let f_ran = ref false in
    Lwt.on_termination (fst (Lwt.wait ())) (fun () -> f_ran := true);
    Lwt.return (not !f_ran)
  end;

  test "on_termination: pending, resolves" begin fun () ->
    let f_ran = ref false in
    let p, r = Lwt.wait () in
    Lwt.on_termination p (fun () -> f_ran := true);
    assert (not !f_ran);
    Lwt.wakeup r ();
    Lwt.return !f_ran
  end;

  test "on_termination: pending, resolves, f raises" begin fun () ->
    let saw = ref None in
    let p, r = Lwt.wait () in
    Lwt.on_termination p (fun () -> raise Exception);
    with_async_exception_hook
      (fun exn -> saw := Some exn)
      (fun () -> Lwt.wakeup r ());
    Lwt.return (!saw = Some Exception)
  end;

  test "on_termination: pending, fails" begin fun () ->
    let f_ran = ref false in
    let p, r = Lwt.wait () in
    Lwt.on_termination p (fun () -> f_ran := true);
    assert (not !f_ran);
    Lwt.wakeup_exn r Exception;
    Lwt.return !f_ran
  end;

  test "on_termination: pending, fails, f raises" begin fun () ->
    let saw = ref None in
    let p, r = Lwt.wait () in
    Lwt.on_termination p (fun () -> raise Exception);
    with_async_exception_hook
      (fun exn -> saw := Some exn)
      (fun () -> Lwt.wakeup_exn r Exit);
    Lwt.return (!saw = Some Exception)
  end;
]
let tests = tests @ on_termination_tests

let on_any_tests = [
  test "on_any: resolved" begin fun () ->
    let f_ran = ref false in
    let g_ran = ref false in
    Lwt.on_any
      (Lwt.return ())
      (fun () -> f_ran := true)
      (fun _ -> g_ran := true);
    Lwt.return (!f_ran && not !g_ran)
  end;

  test "on_any: resolved, f raises" begin fun () ->
    let saw = ref None in
    with_async_exception_hook
      (fun exn -> saw := Some exn)
      (fun () -> Lwt.on_any (Lwt.return ()) (fun () -> raise Exception) ignore);
    Lwt.return (!saw = Some Exception)
  end;

  test "on_any: failed" begin fun () ->
    let saw = ref None in   (* f can't run due to parametricity. *)
    Lwt.on_any (Lwt.fail Exception) ignore (fun exn -> saw := Some exn);
    Lwt.return (!saw = Some Exception)
  end;

  test "on_any: failed, f raises" begin fun () ->
    let saw = ref None in
    with_async_exception_hook
      (fun exn -> saw := Some exn)
      (fun () -> Lwt.on_any (Lwt.fail Exit) ignore (fun _ -> raise Exception));
    Lwt.return (!saw = Some Exception)
  end;

  test "on_any: pending" begin fun () ->
    let g_ran = ref false in    (* f can't run due to parametricity. *)
    Lwt.on_any (fst (Lwt.wait ())) ignore (fun _ -> g_ran := true);
    Lwt.return (not !g_ran)
  end;

  test "on_any: pending, resolves" begin fun () ->
    let f_ran = ref false in
    let g_ran = ref false in
    let p, r = Lwt.wait () in
    Lwt.on_any p (fun () -> f_ran := true) (fun _ -> g_ran := true);
    Lwt.wakeup r ();
    Lwt.return (!f_ran && not !g_ran)
  end;

  test "on_any: pending, resolves, f raises" begin fun () ->
    let saw = ref None in
    let p, r = Lwt.wait () in
    Lwt.on_any p (fun () -> raise Exception) ignore;
    with_async_exception_hook
      (fun exn -> saw := Some exn)
      (fun () -> Lwt.wakeup r ());
    Lwt.return (!saw = Some Exception)
  end;

  test "on_any: pending, fails" begin fun () ->
    let saw = ref None in   (* f can't run due to parametricity. *)
    let p, r = Lwt.wait () in
    Lwt.on_any p ignore (fun exn -> saw := Some exn);
    Lwt.wakeup_exn r Exception;
    Lwt.return (!saw = Some Exception)
  end;

  test "on_any: pending, fails, g raises" begin fun () ->
    let saw = ref None in
    let p, r = Lwt.wait () in
    Lwt.on_any p ignore (fun _ -> raise Exception);
    with_async_exception_hook
      (fun exn -> saw := Some exn)
      (fun () -> Lwt.wakeup_exn r Exit);
    Lwt.return (!saw = Some Exception)
  end;
]
let tests = tests @ on_any_tests



(* Concurrent composition tests, not including cancelation and
   sequence-associated storage. Also not including [Lwt.pick] and [Lwt.npick],
   as those interact with cancelation. *)

let async_tests = [
  test "async: resolved" begin fun () ->
    let f_ran = ref false in
    Lwt.async (fun () -> f_ran := true; Lwt.return ());
    Lwt.return !f_ran
  end;

  test "async: f raises" begin fun () ->
    let saw = ref None in
    with_async_exception_hook
      (fun exn -> saw := Some exn)
      (fun () -> Lwt.async (fun () -> raise Exception));
    Lwt.return (!saw = Some Exception)
  end;

  test "async: failed" begin fun () ->
    let saw = ref None in
    with_async_exception_hook
      (fun exn -> saw := Some exn)
      (fun () -> Lwt.async (fun () -> Lwt.fail Exception));
    Lwt.return (!saw = Some Exception)
  end;

  test "async: pending, resolves" begin fun () ->
    let completed = ref false in
    let p, r = Lwt.wait () in
    Lwt.async (fun () ->
      Lwt.bind p (fun () ->
        completed := true;
        Lwt.return ()));
    assert (not !completed);
    Lwt.wakeup r ();
    Lwt.return !completed
  end;

  test "async: pending, fails" begin fun () ->
    let saw = ref None in
    let p, r = Lwt.wait () in
    Lwt.async (fun () -> p);
    with_async_exception_hook
      (fun exn -> saw := Some exn)
      (fun () -> Lwt.wakeup_exn r Exception);
    Lwt.return (!saw = Some Exception)
  end;
]
let tests = tests @ async_tests

let ignore_result_tests = [
  test "ignore_result: resolved" begin fun () ->
    Lwt.ignore_result (Lwt.return ());
    (* Reaching this without an exception is success. *)
    Lwt.return true
  end;

  test "ignore_result: failed" begin fun () ->
    try
      Lwt.ignore_result (Lwt.fail Exception);
      Lwt.return false
    with Exception ->
      Lwt.return true
  end;

  test "ignore_result: pending, resolves" begin fun () ->
    let p, r = Lwt.wait () in
    Lwt.ignore_result p;
    Lwt.wakeup r ();
    (* Reaching this without process termination is success. *)
    Lwt.return true
  end;

  test "ignore_result: pending, fails" begin fun () ->
    let saw = ref None in
    let p, r = Lwt.wait () in
    Lwt.ignore_result p;
    with_async_exception_hook
      (fun exn -> saw := Some exn)
      (fun () -> Lwt.wakeup_exn r Exception);
    Lwt.return (!saw = Some Exception)
  end;
]
let tests = tests @ ignore_result_tests

let join_tests = [
  test "join: empty" begin fun () ->
    let p = Lwt.join [] in
    Lwt.return (Lwt.state p = Lwt.Return ())
  end;

  test "join: all resolved" begin fun () ->
    let p = Lwt.join [Lwt.return (); Lwt.return ()] in
    Lwt.return (Lwt.state p = Lwt.Return ())
  end;

  test "join: all failed" begin fun () ->
    let p = Lwt.join [Lwt.fail Exception; Lwt.fail Exception] in
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "join: resolved and pending, resolves" begin fun () ->
    let p, r = Lwt.wait () in
    let p = Lwt.join [Lwt.return (); p] in
    assert (Lwt.state p = Lwt.Sleep);
    Lwt.wakeup r ();
    Lwt.return (Lwt.state p = Lwt.Return ())
  end;

  test "join: failed and pending, resolves" begin fun () ->
    let p, r = Lwt.wait () in
    let p = Lwt.join [Lwt.fail Exception; p] in
    assert (Lwt.state p = Lwt.Sleep);
    Lwt.wakeup r ();
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "join: resolved and pending, fails" begin fun () ->
    let p, r = Lwt.wait () in
    let p = Lwt.join [Lwt.return (); p] in
    Lwt.wakeup_exn r Exception;
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "join: failed and pending, fails" begin fun () ->
    let p, r = Lwt.wait () in
    let p = Lwt.join [Lwt.fail Exception; p] in
    assert (Lwt.state p = Lwt.Sleep);
    Lwt.wakeup_exn r Exit;
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "join: diamond" begin fun () ->
    let p, r = Lwt.wait () in
    let p = Lwt.join [p; p] in
    Lwt.wakeup r ();
    Lwt.return (Lwt.state p = Lwt.Return ())
  end;
]
let tests = tests @ join_tests

let choose_tests = [
  test "choose: empty" begin fun () ->
    let p = Lwt.choose [] in
    Lwt.return (Lwt.state p = Lwt.Sleep)
  end;

  test "choose: resolved" begin fun () ->
    let p = Lwt.choose [fst (Lwt.wait ()); Lwt.return "foo"] in
    Lwt.return (Lwt.state p = Lwt.Return "foo")
  end;

  test "choose: failed" begin fun () ->
    let p = Lwt.choose [fst (Lwt.wait ()); Lwt.fail Exception] in
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "choose: multiple completed" begin fun () ->
    (* This is run in a loop to exercise the internal PRNG. *)
    let outcomes = Array.make 3 0 in
    let rec repeat n =
      if n <= 0 then ()
      else
        let p = Lwt.choose
          [fst (Lwt.wait ());
           Lwt.return "foo";
           Lwt.fail Exception;
           Lwt.return "bar"]
        in
        begin match Lwt.state p with
        | Lwt.Return "foo" -> outcomes.(0) <- outcomes.(0) + 1
        | Lwt.Fail Exception -> outcomes.(1) <- outcomes.(1) + 1
        | Lwt.Return "bar" -> outcomes.(2) <- outcomes.(2) + 1
        | _ -> assert false
        end [@ocaml.warning "-4"];
        repeat (n - 1)
    in
    let count = 1000 in
    repeat count;
    Lwt.return
      (outcomes.(0) > 0 && outcomes.(1) > 0 && outcomes.(2) > 0 &&
       outcomes.(0) + outcomes.(1) + outcomes.(2) = count)
  end;

  test "choose: pending" begin fun () ->
    let p = Lwt.choose [fst (Lwt.wait ()); fst (Lwt.wait ())] in
    Lwt.return (Lwt.state p = Lwt.Sleep)
  end;

  test "choose: pending, resolves" begin fun () ->
    let p1, r1 = Lwt.wait () in
    let p2, r2 = Lwt.wait () in
    let p = Lwt.choose [p1; p2] in
    Lwt.wakeup r1 "foo";
    assert (Lwt.state p = Lwt.Return "foo");
    Lwt.wakeup r2 "bar";
    Lwt.return (Lwt.state p = Lwt.Return "foo")
  end;

  test "choose: diamond" begin fun () ->
    let p, r = Lwt.wait () in
    let p = Lwt.choose [p; p] in
    Lwt.wakeup r "foo";
    Lwt.return (Lwt.state p = Lwt.Return "foo")
  end;
]
let tests = tests @ choose_tests

let nchoose_tests = [
  test "nchoose: empty" begin fun () ->
    let p = Lwt.nchoose [] in
    Lwt.return (Lwt.state p = Lwt.Sleep)
  end;

  test "nchoose: all resolved" begin fun () ->
    let p = Lwt.nchoose [Lwt.return "foo"; Lwt.return "bar"] in
    Lwt.return (Lwt.state p = Lwt.Return ["foo"; "bar"])
  end;

  test "nchoose: resolved, failed" begin fun () ->
    let p = Lwt.nchoose [Lwt.return "foo"; Lwt.fail Exception] in
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "nchoose: failed, resolved" begin fun () ->
    let p = Lwt.nchoose [Lwt.fail Exception; Lwt.return "foo"] in
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "nchoose: some pending" begin fun () ->
    let p =
      Lwt.nchoose [Lwt.return "foo"; fst (Lwt.wait ()); Lwt.return "bar"] in
    Lwt.return (Lwt.state p = Lwt.Return ["foo"; "bar"])
  end;

  test "nchoose: pending, resolves" begin fun () ->
    let p, r = Lwt.wait () in
    let p = Lwt.nchoose [fst (Lwt.wait ()); p] in
    assert (Lwt.state p = Lwt.Sleep);
    Lwt.wakeup r "foo";
    Lwt.return (Lwt.state p = Lwt.Return ["foo"])
  end;

  test "nchoose: pending, fails" begin fun () ->
    let p, r = Lwt.wait () in
    let p = Lwt.nchoose [fst (Lwt.wait ()); p] in
    Lwt.wakeup_exn r Exception;
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "nchoose: diamond" begin fun () ->
    let p, r = Lwt.wait () in
    let p = Lwt.nchoose [p; p] in
    Lwt.wakeup r "foo";
    Lwt.return (Lwt.state p = Lwt.Return ["foo"; "foo"])
  end;

  test "nchoose: diamond, fails" begin fun () ->
    let p, r = Lwt.wait () in
    let p = Lwt.nchoose [p; p] in
    Lwt.wakeup_exn r Exception;
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;
]
let tests = tests @ nchoose_tests

let nchoose_split_tests = [
  test "nchoose_split: empty" begin fun () ->
    let p = Lwt.nchoose_split [] in
    Lwt.return (Lwt.state p = Lwt.Sleep)
  end;

  test "nchoose_split: some resolved" begin fun () ->
    let p =
      Lwt.nchoose_split
        [Lwt.return "foo"; fst (Lwt.wait ()); Lwt.return "bar"]
    in
    begin match Lwt.state p with
    | Lwt.Return (["foo"; "bar"], [_]) -> Lwt.return true
    | _ -> Lwt.return false
    end [@ocaml.warning "-4"]
  end;

  test "nchoose_split: resolved, failed" begin fun () ->
    let p = Lwt.nchoose_split [Lwt.return (); Lwt.fail Exception] in
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "nchoose_split: failed, resolved" begin fun () ->
    let p = Lwt.nchoose_split [Lwt.fail Exception; Lwt.return ()] in
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "nchoose_split: pending, failed" begin fun () ->
    let p = Lwt.nchoose_split [fst (Lwt.wait ()); Lwt.fail Exception] in
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "nchoose_split: pending, resolves" begin fun () ->
    let p, r = Lwt.wait () in
    let p = Lwt.nchoose_split [p; fst (Lwt.wait ())] in
    assert (Lwt.state p = Lwt.Sleep);
    Lwt.wakeup r "foo";
    begin match Lwt.state p with
    | Lwt.Return (["foo"], [_]) -> Lwt.return true
    | _ -> Lwt.return false
    end [@ocaml.warning "-4"]
  end;

  test "nchoose_split: pending, fails" begin fun () ->
    let p, r = Lwt.wait () in
    let p = Lwt.nchoose_split [p; fst (Lwt.wait ())] in
    Lwt.wakeup_exn r Exception;
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "nchoose_split: diamond" begin fun () ->
    let p, r = Lwt.wait () in
    let p = Lwt.nchoose_split [p; p; fst (Lwt.wait ())] in
    Lwt.wakeup r ();
    begin match Lwt.state p with
    | Lwt.Return ([(); ()], [_]) -> Lwt.return true
    | _ -> Lwt.return false
    end [@ocaml.warning "-4"]
  end;

  test "nchoose_split: diamond, fails" begin fun () ->
    let p, r = Lwt.wait () in
    let p = Lwt.nchoose_split [p; p; fst (Lwt.wait ())] in
    Lwt.wakeup_exn r Exception;
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;
]
let tests = nchoose_split_tests @ tests



(* Tests functions related to [Lwt.state]; [Lwt.state] itself is tested in the
   preceding sections. *)

let state_query_tests = [
  test "is_sleeping: resolved" begin fun () ->
    Lwt.return (not @@ Lwt.is_sleeping (Lwt.return ()))
  end;

  test "is_sleeping: failed" begin fun () ->
    Lwt.return (not @@ Lwt.is_sleeping (Lwt.fail Exception))
  end;

  test "is_sleeping: pending" begin fun () ->
    Lwt.return (Lwt.is_sleeping (fst (Lwt.wait ())))
  end;

  (* This tests an implementation detail. *)
  test "is_sleeping: proxy" begin fun () ->
    let p1, r = Lwt.wait () in
    let p2, _ = Lwt.wait () in
    Lwt.bind p1 (fun () -> p2) |> ignore;
    Lwt.wakeup r ();
    Lwt.return (Lwt.is_sleeping p2)
  end;

  (* This tests an internal API. *)
  test "poll: resolved" begin fun () ->
    Lwt.return (Lwt.poll (Lwt.return "foo") = Some "foo")
  end;

  test "poll: failed" begin fun () ->
    try
      Lwt.poll (Lwt.fail Exception) |> ignore;
      Lwt.return false
    with Exception ->
      Lwt.return true
  end;

  test "poll: pending" begin fun () ->
    Lwt.return (Lwt.poll (fst (Lwt.wait ())) = None)
  end;

  (* This tests an internal API on an implementation detail... *)
  test "poll: proxy" begin fun () ->
    let p1, r = Lwt.wait () in
    let p2, _ = Lwt.wait () in
    Lwt.bind p1 (fun () -> p2) |> ignore;
    Lwt.wakeup r ();
    Lwt.return (Lwt.poll p2 = None)
  end;
]
let tests = tests @ state_query_tests



(* Preceding tests exercised most of [Lwt.wakeup], but here are more checks. *)
let wakeup_tests = [
  test "wakeup_result: nested" begin fun () ->
    let f_ran = ref false in
    let p1, r1 = Lwt.wait () in
    let p2, r2 = Lwt.wait () in
    Lwt.on_success p2 (fun _ -> f_ran := true);
    Lwt.on_success p1 (fun s ->
      Lwt.wakeup_result r2 (Result.Ok (s ^ "bar"));
      assert (Lwt.state p2 = Lwt.Return "foobar");
      assert !f_ran);
    Lwt.wakeup_result r1 (Result.Ok "foo");
    Lwt.return (!f_ran && Lwt.state p2 = Lwt.Return "foobar")
  end;
]
let tests = tests @ wakeup_tests

let wakeup_later_tests = [
  test "wakeup_later_result: immediate" begin fun () ->
    let p, r = Lwt.wait () in
    let p = Lwt.bind p (fun s -> Lwt.return (s ^ "bar")) in
    Lwt.wakeup_later_result r (Result.Ok "foo");
    Lwt.return (Lwt.state p = Lwt.Return "foobar")
  end;

  test "wakeup_later_result: already resolved" begin fun () ->
    let _, r = Lwt.wait () in
    Lwt.wakeup r ();
    try
      Lwt.wakeup_later_result r (Result.Ok ());
      Lwt.return false;
    with Invalid_argument _ ->
      Lwt.return true
  end;

  test "wakeup_later_result: already failed" begin fun () ->
    let _, r = Lwt.wait () in
    Lwt.wakeup_exn r Exception;
    try
      Lwt.wakeup_later_result r (Result.Ok ());
      Lwt.return false
    with Invalid_argument _ ->
      Lwt.return true
  end;

  test "wakeup_later_result: nested" begin fun () ->
    let f_ran = ref false in
    let p1, r1 = Lwt.wait () in
    let p2, r2 = Lwt.wait () in
    Lwt.on_success p2 (fun _ -> f_ran := true);
    Lwt.on_success p1 (fun s ->
      Lwt.wakeup_later_result r2 (Result.Ok (s ^ "bar"));
      assert (Lwt.state p2 = Lwt.Return "foobar");
      assert (not !f_ran));
    Lwt.wakeup_later_result r1 (Result.Ok "foo");
    Lwt.return (!f_ran && Lwt.state p2 = Lwt.Return "foobar")
  end;

  (* Only basic tests for wakeup_later and wakeup_later_exn, as they are
     implemented in terms of wakeup_later_result. This isn't fully legitimate as
     a reason, but oh well. *)
  test "wakeup_later: basic" begin fun () ->
    let p, r = Lwt.wait () in
    Lwt.wakeup_later r "foo";
    Lwt.return (Lwt.state p = Lwt.Return "foo")
  end;

  test "wakeup_later_exn: basic" begin fun () ->
    let p, r = Lwt.wait () in
    Lwt.wakeup_later_exn r Exception;
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;
]
let tests = tests @ wakeup_later_tests



(* Cancelation and its interaction with the rest of the API. *)

let cancel_tests = [
  test "cancel: resolved" begin fun () ->
    let p = Lwt.return () in
    Lwt.cancel p;
    Lwt.return (Lwt.state p = Lwt.Return ())
  end;

  test "cancel: failed" begin fun () ->
    let p = Lwt.fail Exception in
    Lwt.cancel p;
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "cancel: wait" begin fun () ->
    let p, _ = Lwt.wait () in
    Lwt.cancel p;
    Lwt.return (Lwt.state p = Lwt.Sleep)
  end;

  test "cancel: task" begin fun () ->
    let p, _ = Lwt.task () in
    Lwt.cancel p;
    Lwt.return (Lwt.state p = Lwt.Fail Lwt.Canceled)
  end;

  test "cancel: trigger" begin fun () ->
    let saw = ref None in
    let p, _ = Lwt.task () in
    Lwt.on_failure p (fun exn -> saw := Some exn);
    Lwt.cancel p;
    Lwt.return (!saw = Some Lwt.Canceled)
  end;

  (* Behaves like wakeup rather than wakeup_later, even though that's probably
     wrong. Calling cancel in a (functional) loop will cause stack overflow. *)
  test "cancel: nested" begin fun () ->
    let f_ran = ref false in
    let p1, _ = Lwt.task () in
    let p2, _ = Lwt.task () in
    Lwt.on_failure p2 (fun _ -> f_ran := true);
    Lwt.on_failure p1 (fun _ ->
      Lwt.cancel p2;
      assert (Lwt.state p2 = Lwt.Fail Lwt.Canceled);
      assert !f_ran);
    Lwt.cancel p1;
    Lwt.return (!f_ran && Lwt.state p2 = Lwt.Fail Lwt.Canceled)
  end;
]
let tests = tests @ cancel_tests

let on_cancel_tests = [
 test "on_cancel: pending" begin fun () ->
    let f_ran = ref false in
    let p, _ = Lwt.task () in
    Lwt.on_cancel p (fun () -> f_ran := true);
    assert (not !f_ran);
    Lwt.cancel p;
    Lwt.return !f_ran
  end;

  test "on_cancel: multiple" begin fun () ->
    let f_ran = ref false in
    let g_ran = ref false in
    let h_ran = ref false in
    let p, _ = Lwt.task () in
    Lwt.on_cancel p (fun () -> f_ran := true);
    Lwt.on_cancel p (fun () -> g_ran := true);
    Lwt.on_cancel p (fun () -> h_ran := true);
    Lwt.cancel p;
    Lwt.return (!f_ran && !g_ran && !h_ran)
  end;

  test "on_cancel: ordering" begin fun () ->
    (* Two cancel triggers to make sure they both run before the ordinary
       trigger. *)
    let on_cancel_1_ran = ref false in
    let on_cancel_2_ran = ref false in
    let trigger_ran = ref false in
    let p, _ = Lwt.task () in
    Lwt.on_cancel p (fun () -> on_cancel_1_ran := true);
    Lwt.on_failure p (fun _ ->
      assert !on_cancel_1_ran;
      assert !on_cancel_2_ran;
      trigger_ran := true);
    Lwt.on_cancel p (fun () -> on_cancel_2_ran := true);
    Lwt.cancel p;
    Lwt.return !trigger_ran
  end;

  test "on_cancel: resolved" begin fun () ->
    let f_ran = ref false in
    Lwt.on_cancel (Lwt.return ()) (fun () -> f_ran := true);
    Lwt.return (not !f_ran)
  end;

  test "on_cancel: failed" begin fun () ->
    let f_ran = ref false in
    Lwt.on_cancel (Lwt.fail Exception) (fun () -> f_ran := true);
    Lwt.return (not !f_ran)
  end;

  test "on_cancel: already canceled" begin fun () ->
    let f_ran = ref false in
    Lwt.on_cancel (Lwt.fail Lwt.Canceled) (fun () -> f_ran := true);
    Lwt.return !f_ran
  end;

  (* More generally, this tests that failing with [Lwt.Canceled] is equivalent
     to calling [Lwt.cancel]. The difference is that [Lwt.cancel] can be called
     on promises without the need of a resolver. *)
  test "on_cancel: fail Canceled" begin fun () ->
    let f_ran = ref false in
    let p, r = Lwt.wait () in
    Lwt.on_cancel p (fun () -> f_ran := true);
    Lwt.wakeup_exn r Lwt.Canceled;
    Lwt.return !f_ran
  end;
]
let tests = tests @ on_cancel_tests

let protected_tests = [
  test "protected: resolved" begin fun () ->
    let p = Lwt.protected (Lwt.return ()) in
    (* If [p] starts resolved, it can't be canceled. *)
    Lwt.return (Lwt.state p = Lwt.Return ())
  end;

  test "protected: failed" begin fun () ->
    let p = Lwt.protected (Lwt.fail Exception) in
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "protected: pending" begin fun () ->
    let p, _ = Lwt.task () in
    let p' = Lwt.protected p in
    Lwt.return (Lwt.state p = Lwt.Sleep && Lwt.state p' = Lwt.Sleep)
  end;

  test "protected: pending, resolves" begin fun () ->
    let p, r = Lwt.task () in
    let p' = Lwt.protected p in
    Lwt.wakeup r "foo";
    Lwt.return (Lwt.state p' = Lwt.Return "foo")
  end;

  test "protected: pending, canceled" begin fun () ->
    let p, _ = Lwt.task () in
    let p' = Lwt.protected p in
    Lwt.cancel p';
    Lwt.return (Lwt.state p = Lwt.Sleep && Lwt.state p' = Lwt.Fail Lwt.Canceled)
  end;

  test "protected: pending, canceled, resolves" begin fun () ->
    let p, r = Lwt.task () in
    let p' = Lwt.protected p in
    Lwt.cancel p';
    Lwt.wakeup r "foo";
    Lwt.return
      (Lwt.state p = Lwt.Return "foo" && Lwt.state p' = Lwt.Fail Lwt.Canceled)
  end;

  (* Implementation detail: [p' = Lwt.protected _] can still be completed if it
     becomes a proxy. *)
  test "protected: pending, proxy" begin fun () ->
    let p1, r1 = Lwt.task () in
    let p2 = Lwt.protected p1 in

    (* Make p2 a proxy for p4; p3 is just needed to suspend the bind, in order
       to trigger the code that makes p2 a proxy. *)
    let p3, r3 = Lwt.wait () in
    let _ = Lwt.bind p3 (fun () -> p2) in
    Lwt.wakeup r3 ();

    (* It should now be possible to complete p2 by completing p1. *)
    Lwt.wakeup r1 "foo";
    Lwt.return (Lwt.state p2 = Lwt.Return "foo")
  end;
]
let tests = tests @ protected_tests

let no_cancel_tests = [
  test "no_cancel: resolved" begin fun () ->
    let p = Lwt.no_cancel (Lwt.return ()) in
    (* [p] starts resolved, so it can't be canceled. *)
    Lwt.return (Lwt.state p = Lwt.Return ())
  end;

  test "no_cancel: failed" begin fun () ->
    let p = Lwt.no_cancel (Lwt.fail Exception) in
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "no_cancel: pending" begin fun () ->
    let p, _ = Lwt.task () in
    let p' = Lwt.no_cancel p in
    Lwt.return (Lwt.state p = Lwt.Sleep && Lwt.state p' = Lwt.Sleep)
  end;

  test "no_cancel: pending, resolves" begin fun () ->
    let p, r = Lwt.task () in
    let p = Lwt.no_cancel p in
    Lwt.wakeup r "foo";
    Lwt.return (Lwt.state p = Lwt.Return "foo")
  end;

  test "no_cancel: pending, cancel attempt" begin fun () ->
    let p, _ = Lwt.task () in
    let p' = Lwt.no_cancel p in
    Lwt.cancel p';
    Lwt.return (Lwt.state p = Lwt.Sleep && Lwt.state p' = Lwt.Sleep)
  end;
]
let tests = tests @ no_cancel_tests

let complete_already_canceled_promise_tests = [
  test "wakeup: canceled" begin fun () ->
    let p, r = Lwt.task () in
    Lwt.cancel p;
    Lwt.wakeup r ();
    Lwt.return (Lwt.state p = Lwt.Fail Lwt.Canceled)
  end;

  (* This test can start falsely passing if the entire test is run inside an
     Lwt promise completion phase, e.g. inside an outer [Lwt.wakeup_later]. *)
  test "wakeup_later: canceled" begin fun () ->
    let p, r = Lwt.task () in
    Lwt.cancel p;
    Lwt.wakeup_later r ();
    Lwt.return (Lwt.state p = Lwt.Fail Lwt.Canceled)
  end;
]
let tests = tests @ complete_already_canceled_promise_tests

let pick_tests = [
  test "pick: empty" begin fun () ->
    let p = Lwt.pick [] in
    Lwt.return (Lwt.state p = Lwt.Sleep)
  end;

  test "pick: resolved" begin fun () ->
    let p1, _ = Lwt.task () in
    let p2 = Lwt.pick [p1; Lwt.return "foo"] in
    Lwt.return
      (Lwt.state p1 = Lwt.Fail Lwt.Canceled && Lwt.state p2 = Lwt.Return "foo")
  end;

  test "pick: failed" begin fun () ->
    let p1, _ = Lwt.task () in
    let p2 = Lwt.pick [p1; Lwt.fail Exception] in
    Lwt.return
      (Lwt.state p1 = Lwt.Fail Lwt.Canceled &&
       Lwt.state p2 = Lwt.Fail Exception)
  end;

  test "pick: multiple completed" begin fun () ->
    (* This is run in a loop to exercise the internal PRNG. *)
    let outcomes = Array.make 3 0 in
    let rec repeat n =
      if n <= 0 then ()
      else
        let p = Lwt.pick
          [fst (Lwt.wait ());
           Lwt.return "foo";
           Lwt.fail Exception;
           Lwt.return "bar"]
        in
        begin match Lwt.state p with
        | Lwt.Return "foo" -> outcomes.(0) <- outcomes.(0) + 1
        | Lwt.Fail Exception -> outcomes.(1) <- outcomes.(1) + 1
        | Lwt.Return "bar" -> outcomes.(2) <- outcomes.(2) + 1
        | _ -> assert false
        end [@ocaml.warning "-4"];
        repeat (n - 1)
    in
    let count = 1000 in
    repeat count;
    Lwt.return
      (outcomes.(0) > 0 && outcomes.(1) > 0 && outcomes.(2) > 0 &&
       outcomes.(0) + outcomes.(1) + outcomes.(2) = count)
  end;

  test "pick: pending" begin fun () ->
    let p = Lwt.pick [fst (Lwt.wait ()); fst (Lwt.wait ())] in
    Lwt.return (Lwt.state p = Lwt.Sleep)
  end;

  test "pick: pending, resolves" begin fun () ->
    let p1, r1 = Lwt.task () in
    let p2, _ = Lwt.task () in
    let p = Lwt.pick [p1; p2] in
    Lwt.wakeup r1 "foo";
    Lwt.return
      (Lwt.state p2 = Lwt.Fail Lwt.Canceled && Lwt.state p = Lwt.Return "foo")
  end;

  test "pick: diamond" begin fun () ->
    let p, r = Lwt.wait () in
    let p = Lwt.pick [p; p] in
    Lwt.wakeup r "foo";
    Lwt.return (Lwt.state p = Lwt.Return "foo")
  end;

  test "pick: pending, canceled" begin fun () ->
    let p1, _ = Lwt.task () in
    let p2, _ = Lwt.task () in
    let p = Lwt.pick [p1; p2] in
    Lwt.cancel p;
    Lwt.return
      (Lwt.state p1 = Lwt.Fail Lwt.Canceled &&
       Lwt.state p2 = Lwt.Fail Lwt.Canceled)
  end;
]
let tests = tests @ pick_tests

let npick_tests = [
  test "npick: empty" begin fun () ->
    let p = Lwt.npick [] in
    Lwt.return (Lwt.state p = Lwt.Sleep)
  end;

  test "npick: all resolved" begin fun () ->
    let p = Lwt.npick [Lwt.return "foo"; Lwt.return "bar"] in
    Lwt.return (Lwt.state p = Lwt.Return ["foo"; "bar"])
  end;

  test "npick: resolved, failed" begin fun () ->
    let p = Lwt.npick [Lwt.return "foo"; Lwt.fail Exception] in
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "npick: failed, resolved" begin fun () ->
    let p = Lwt.npick [Lwt.fail Exception; Lwt.return "foo"] in
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "npick: some pending" begin fun () ->
    let p1, _ = Lwt.task () in
    let p2 = Lwt.npick [Lwt.return "foo"; p1; Lwt.return "bar"] in
    Lwt.return
      (Lwt.state p1 = Lwt.Fail Lwt.Canceled &&
       Lwt.state p2 = Lwt.Return ["foo"; "bar"])
  end;

  (* The behavior of [p] tested here is a current bug in [Lwt.npick]. *)
  test "npick: pending, resolves" begin fun () ->
    let p1, _ = Lwt.task () in
    let p2, r = Lwt.task () in
    let p = Lwt.npick [p1; p2] in
    assert (Lwt.state p = Lwt.Sleep);
    Lwt.wakeup r "foo";
    Lwt.return
      (Lwt.state p1 = Lwt.Fail Lwt.Canceled &&
       Lwt.state p = Lwt.Fail Lwt.Canceled)
  end;

  (* This is the same bug as above. *)
  test "npick: pending, fails" begin fun () ->
    let p1, _ = Lwt.task () in
    let p2, r = Lwt.task () in
    let p = Lwt.npick [p1; p2] in
    Lwt.wakeup_exn r Exception;
    Lwt.return
      (Lwt.state p1 = Lwt.Fail Lwt.Canceled &&
       Lwt.state p = Lwt.Fail Lwt.Canceled)
  end;

  test "npick: diamond" begin fun () ->
    let p, r = Lwt.wait () in
    let p = Lwt.npick [p; p] in
    Lwt.wakeup r "foo";
    Lwt.return (Lwt.state p = Lwt.Return ["foo"; "foo"])
  end;

  test "npick: diamond, fails" begin fun () ->
    let p, r = Lwt.wait () in
    let p = Lwt.npick [p; p] in
    Lwt.wakeup_exn r Exception;
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "npick: pending, canceled" begin fun () ->
    let p1, _ = Lwt.task () in
    let p2, _ = Lwt.task () in
    let p = Lwt.npick [p1; p2] in
    Lwt.cancel p;
    Lwt.return
      (Lwt.state p1 = Lwt.Fail Lwt.Canceled &&
       Lwt.state p2 = Lwt.Fail Lwt.Canceled)
  end;
]
let tests = tests @ npick_tests

let cancel_bind_tests = [
  test "bind: wait, pending, canceled" begin fun () ->
    let f_ran = ref false in
    let p, _ = Lwt.wait () in
    let p' = Lwt.bind p (fun () -> f_ran := true; Lwt.return ()) in
    Lwt.cancel p';
    Lwt.return
      (not !f_ran && Lwt.state p = Lwt.Sleep && Lwt.state p' = Lwt.Sleep)
  end;

  test "bind: task, pending, canceled" begin fun () ->
    let f_ran = ref false in
    let p, _ = Lwt.task () in
    let p' = Lwt.bind p (fun () -> f_ran := true; Lwt.return ()) in
    Lwt.cancel p';
    Lwt.return
      (not !f_ran &&
       Lwt.state p = Lwt.Fail Lwt.Canceled &&
       Lwt.state p' = Lwt.Fail Lwt.Canceled)
  end;

  test "bind: pending, wait, canceled" begin fun () ->
    let p, r = Lwt.wait () in
    let p', _ = Lwt.wait () in
    let p'' = Lwt.bind p (fun () -> p') in
    Lwt.wakeup r ();
    (* [bind]'s [f] ran, and now [p'] and [p''] should share the same state. *)
    Lwt.cancel p'';
    Lwt.return (Lwt.state p' = Lwt.Sleep && Lwt.state p'' = Lwt.Sleep)
  end;

  test "bind: pending, task, canceled" begin fun () ->
    let p1, r = Lwt.wait () in
    let p2, _ = Lwt.task () in
    let p3 = Lwt.bind p1 (fun () -> p2) in
    Lwt.wakeup r ();
    Lwt.cancel p3;
    Lwt.return
      (Lwt.state p2 = Lwt.Fail Lwt.Canceled &&
       Lwt.state p3 = Lwt.Fail Lwt.Canceled &&
       p2 != p3)
  end;

  test "bind: pending, task, canceled, chain" begin fun () ->
    let p1, r = Lwt.wait () in
    let p2, _ = Lwt.task () in
    let p3 = Lwt.bind p1 (fun () -> p2) in
    let p4 = Lwt.bind p1 (fun () -> p3) in
    Lwt.wakeup r ();
    (* At this point, [p4] and [p3] share the same state, and canceling [p4]
       should chain to [p2], because [p3] is obtained by binding on [p2]. *)
    Lwt.cancel p4;
    Lwt.return
      (Lwt.state p2 = Lwt.Fail Lwt.Canceled &&
       Lwt.state p3 = Lwt.Fail Lwt.Canceled &&
       Lwt.state p4 = Lwt.Fail Lwt.Canceled)
  end;

  test "bind: pending, on_cancel triggers" begin fun () ->
    let f_ran = ref false in
    let g_ran = ref false in
    let p1, _ = Lwt.task () in
    let p2 = Lwt.bind (fst (Lwt.task ())) (fun () -> p1) in
    Lwt.on_cancel p1 (fun () -> f_ran := true);
    Lwt.on_cancel p2 (fun () -> g_ran := true);
    Lwt.cancel p2;
    (* Canceling [p2] doesn't cancel [p1], because the function passed to
       [Lwt.bind] never ran. *)
    Lwt.return (not !f_ran && !g_ran)
  end;

  test "bind: pending, resolves, on_cancel triggers" begin fun () ->
    let f_ran = ref false in
    let g_ran = ref false in
    let p1, r = Lwt.task () in
    let p2, _ = Lwt.task () in
    let p3 = Lwt.bind p1 (fun () -> p2) in
    Lwt.on_cancel p2 (fun () -> f_ran := true);
    Lwt.on_cancel p3 (fun () -> g_ran := true);
    Lwt.wakeup r ();
    Lwt.cancel p3;
    (* Canceling [p3] cancels [p2], because the function passed to [Lwt.bind]
       did run, and evaluated to [p2]. *)
    Lwt.return (!f_ran && !g_ran)
  end;
]
let tests = tests @ cancel_bind_tests

let cancel_map_tests = [
  test "map: wait, pending, canceled" begin fun () ->
    let f_ran = ref false in
    let p, _ = Lwt.wait () in
    let p' = Lwt.map (fun () -> f_ran := true) p in
    Lwt.cancel p';
    Lwt.return
      (not !f_ran && Lwt.state p = Lwt.Sleep && Lwt.state p' = Lwt.Sleep)
  end;

  test "map: task, pending, canceled" begin fun () ->
    let f_ran = ref false in
    let p, _ = Lwt.task () in
    let p' = Lwt.map (fun () -> f_ran := true) p in
    Lwt.cancel p';
    Lwt.return
      (not !f_ran &&
       Lwt.state p = Lwt.Fail Lwt.Canceled &&
       Lwt.state p' = Lwt.Fail Lwt.Canceled)
  end;
]
let tests = tests @ cancel_map_tests

let cancel_catch_tests = [
  (* In [p' = Lwt.catch (fun () -> p) f], if [p] is not cancelable, [p'] is also
     not cancelable. *)
  test "catch: wait, pending, canceled" begin fun () ->
    let f_ran = ref false in
    let p, _ = Lwt.wait () in
    let p' =
      Lwt.catch
        (fun () -> p)
        (fun _ -> f_ran := true; Lwt.return ())
    in
    Lwt.cancel p';
    Lwt.return
      (not !f_ran && Lwt.state p = Lwt.Sleep && Lwt.state p' = Lwt.Sleep)
  end;

  (* In [p' = Lwt.catch (fun () -> p) f], if [p] is cancelable, canceling [p']
     propagates to [p], and then the cancelation exception can be "intercepted"
     by [f], which can complete [p'] in an arbitrary way. *)
  test "catch: task, pending, canceled" begin fun () ->
    let saw = ref None in
    let p, _ = Lwt.task () in
    let p' =
      Lwt.catch
        (fun () -> p)
        (fun exn -> saw := Some exn; Lwt.return "foo")
    in
    Lwt.cancel p';
    Lwt.return
      (!saw = Some Lwt.Canceled &&
       Lwt.state p = Lwt.Fail Lwt.Canceled &&
       Lwt.state p' = Lwt.Return "foo")
  end;

  (* In [p' = Lwt.catch (fun () -> p) f], if [p] is cancelable, and cancel
     callbacks are added to both [p] and [p'], and [f] does not resolve [p']
     with [Lwt.Fail Lwt.Canceled], only the callback on [p] runs. *)
  test "catch: task, pending, canceled, on_cancel, intercepted" begin fun () ->
    let on_cancel_1_ran = ref false in
    let on_cancel_2_ran = ref false in
    let p, _ = Lwt.task () in
    let p' =
      Lwt.catch
        (fun () -> p)
        (fun _ ->
          assert (!on_cancel_1_ran && not !on_cancel_2_ran);
          Lwt.return "foo")
    in
    Lwt.on_cancel p (fun () -> on_cancel_1_ran := true);
    Lwt.on_cancel p' (fun () -> on_cancel_2_ran := true);
    Lwt.cancel p';
    Lwt.return
      (Lwt.state p = Lwt.Fail Lwt.Canceled &&
       Lwt.state p' = Lwt.Return "foo" &&
       not !on_cancel_2_ran)
  end;

  (* Same as above, except this time, cancelation is passed on to the outer
     promise, so we can expect both cancel callbacks to run. *)
  test "catch: task, pending, canceled, on_cancel, forwarded" begin fun () ->
    let on_cancel_2_ran = ref false in
    let p, _ = Lwt.task () in
    let p' = Lwt.catch (fun () -> p) Lwt.fail in
    Lwt.on_cancel p' (fun () -> on_cancel_2_ran := true);
    Lwt.cancel p';
    Lwt.return
      (Lwt.state p = Lwt.Fail Lwt.Canceled &&
       Lwt.state p' = Lwt.Fail Lwt.Canceled &&
       !on_cancel_2_ran)
  end;

  (* (2 tests) If the handler passed to [Lwt.catch] already ran, canceling the
     outer promise is the same as canceling the promise returned by the
     handler. *)
  test "catch: pending, wait, canceled" begin fun () ->
    let p1, r = Lwt.wait () in
    let p2, _ = Lwt.wait () in
    let p3 =
      Lwt.catch
        (fun () -> p1)
        (fun _ -> p2)
    in
    Lwt.wakeup_exn r Exception;
    Lwt.cancel p3;
    Lwt.return (Lwt.state p2 = Lwt.Sleep && Lwt.state p3 = Lwt.Sleep)
  end;

  test "catch: pending, task, canceled" begin fun () ->
    let p1, r = Lwt.wait () in
    let p2, _ = Lwt.task () in
    let p3 =
      Lwt.catch
        (fun () -> p1)
        (fun _ -> p2)
    in
    Lwt.wakeup_exn r Exception;
    Lwt.cancel p3;
    Lwt.return
      (Lwt.state p2 = Lwt.Fail Lwt.Canceled &&
       Lwt.state p3 = Lwt.Fail Lwt.Canceled)
  end;
]
let tests = tests @ cancel_catch_tests

let cancel_try_bind_tests = [
  test "try_bind: wait, pending, canceled" begin fun () ->
    let f_or_g_ran = ref false in
    let p, _ = Lwt.wait () in
    let p' =
      Lwt.try_bind
        (fun () -> p)
        (fun () -> f_or_g_ran := true; Lwt.return ())
        (fun _ -> f_or_g_ran := true; Lwt.return ())
    in
    Lwt.cancel p';
    Lwt.return
      (not !f_or_g_ran && Lwt.state p = Lwt.Sleep && Lwt.state p' = Lwt.Sleep)
  end;

  test "try_bind: task, pending, canceled" begin fun () ->
    let f_ran = ref false in
    let saw = ref None in
    let p, _ = Lwt.task () in
    let p' =
      Lwt.try_bind
        (fun () -> p)
        (fun () -> f_ran := true; Lwt.return "foo")
        (fun exn -> saw := Some exn; Lwt.return "bar")
    in
    Lwt.cancel p';
    Lwt.return
      (not !f_ran &&
       !saw = Some Lwt.Canceled &&
       Lwt.state p = Lwt.Fail Lwt.Canceled &&
       Lwt.state p' = Lwt.Return "bar")
  end;

  test "try_bind: pending, resolves, wait, canceled" begin fun () ->
    let p1, r = Lwt.wait () in
    let p2, _ = Lwt.wait () in
    let p3 =
      Lwt.try_bind
        (fun () -> p1)
        (fun () -> p2)
        (fun _ -> Lwt.return "foo")
    in
    Lwt.wakeup r ();
    Lwt.cancel p3;
    Lwt.return (Lwt.state p2 = Lwt.Sleep && Lwt.state p3 = Lwt.Sleep)
  end;

  test "try_bind: pending, resolves, task, canceled" begin fun () ->
    let p1, r = Lwt.wait () in
    let p2, _ = Lwt.task () in
    let p3 =
      Lwt.try_bind
        (fun () -> p1)
        (fun () -> p2)
        (fun _ -> Lwt.return "foo")
    in
    Lwt.wakeup r ();
    Lwt.cancel p3;
    Lwt.return
      (Lwt.state p2 = Lwt.Fail Lwt.Canceled &&
       Lwt.state p3 = Lwt.Fail Lwt.Canceled)
  end;

  test "try_bind: pending, fails, wait, canceled" begin fun () ->
    let p1, r = Lwt.wait () in
    let p2, _ = Lwt.wait () in
    let p3 =
      Lwt.try_bind
        (fun () -> p1)
        (fun () -> Lwt.return "foo")
        (fun _ -> p2)
    in
    Lwt.wakeup_exn r Exception;
    Lwt.cancel p3;
    Lwt.return (Lwt.state p2 = Lwt.Sleep && Lwt.state p3 = Lwt.Sleep)
  end;

  test "try_bind: pending, fails, task, canceled" begin fun () ->
    let p1, r = Lwt.wait () in
    let p2, _ = Lwt.task () in
    let p3 =
      Lwt.try_bind
        (fun () -> p1)
        (fun () -> Lwt.return "foo")
        (fun _ -> p2)
    in
    Lwt.wakeup_exn r Exception;
    Lwt.cancel p3;
    Lwt.return
      (Lwt.state p2 = Lwt.Fail Lwt.Canceled &&
       Lwt.state p3 = Lwt.Fail Lwt.Canceled)
  end;
]
let tests = tests @ cancel_try_bind_tests

let cancel_finalize_tests = [
  test "finalize: wait, pending, canceled" begin fun () ->
    let f_ran = ref false in
    let p, _ = Lwt.wait () in
    let p' =
      Lwt.finalize
        (fun () -> p)
        (fun () -> f_ran := true; Lwt.return ())
    in
    Lwt.cancel p';
    Lwt.return
      (not !f_ran && Lwt.state p = Lwt.Sleep && Lwt.state p' = Lwt.Sleep)
  end;

  test "finalize: task, pending, canceled" begin fun () ->
    let f_ran = ref false in
    let p, _ = Lwt.task () in
    let p' =
      Lwt.finalize
        (fun () -> p)
        (fun () -> f_ran := true; Lwt.return ())
    in
    Lwt.cancel p';
    Lwt.return
      (!f_ran &&
       Lwt.state p = Lwt.Fail Lwt.Canceled &&
       Lwt.state p' = Lwt.Fail Lwt.Canceled)
  end;

  test "finalize: pending, wait, canceled" begin fun () ->
    let p1, r = Lwt.wait () in
    let p2, _ = Lwt.wait () in
    let p3 =
      Lwt.finalize
        (fun () -> p1)
        (fun () -> p2)
    in
    Lwt.wakeup r ();
    Lwt.cancel p3;
    Lwt.return (Lwt.state p2 = Lwt.Sleep && Lwt.state p3 = Lwt.Sleep)
  end;

  test "finalize: pending, task, canceled" begin fun () ->
    let p1, r = Lwt.wait () in
    let p2, _ = Lwt.task () in
    let p3 =
      Lwt.finalize
        (fun () -> p1)
        (fun () -> p2)
    in
    Lwt.wakeup r ();
    Lwt.cancel p3;
    Lwt.return
      (Lwt.state p2 = Lwt.Fail Lwt.Canceled && Lwt.state p3 = Lwt.Fail Lwt.Canceled)
  end;
]
let tests = tests @ cancel_finalize_tests

let cancel_direct_handler_tests = [
  test "on_success: pending, canceled" begin fun () ->
    let f_ran = ref false in
    let p, _ = Lwt.task () in
    Lwt.on_success p (fun () -> f_ran := true);
    Lwt.cancel p;
    Lwt.return (not !f_ran)
  end;

  test "on_failure: pending, canceled" begin fun () ->
    let saw = ref None in
    let p, _ = Lwt.task () in
    Lwt.on_failure p (fun exn -> saw := Some exn);
    Lwt.cancel p;
    Lwt.return (!saw = Some Lwt.Canceled)
  end;

  test "on_termination: pending, canceled" begin fun () ->
    let f_ran = ref false in
    let p, _ = Lwt.task () in
    Lwt.on_termination p (fun () -> f_ran := true);
    Lwt.cancel p;
    Lwt.return !f_ran
  end;

  test "on_any: pending, canceled" begin fun () ->
    let f_ran = ref false in
    let saw = ref None in
    let p, _ = Lwt.task () in
    Lwt.on_any p (fun () -> f_ran := true) (fun exn -> saw := Some exn);
    Lwt.cancel p;
    Lwt.return (not !f_ran && !saw = Some Lwt.Canceled)
  end;
]
let tests = tests @ cancel_direct_handler_tests

let cancel_join_tests = [
  test "join: wait, pending, cancel" begin fun () ->
    let p1, _ = Lwt.wait () in
    let p2, _ = Lwt.wait () in
    let p3 = Lwt.join [p1; p2] in
    Lwt.cancel p3;
    Lwt.return
      (Lwt.state p1 = Lwt.Sleep &&
       Lwt.state p2 = Lwt.Sleep &&
       Lwt.state p3 = Lwt.Sleep)
  end;

  test "join: task, pending, cancel" begin fun () ->
    let p1, _ = Lwt.task () in
    let p2, _ = Lwt.task () in
    let p3 = Lwt.join [p1; p2] in
    Lwt.cancel p3;
    Lwt.return
      (Lwt.state p1 = Lwt.Fail Lwt.Canceled &&
       Lwt.state p2 = Lwt.Fail Lwt.Canceled &&
       Lwt.state p3 = Lwt.Fail Lwt.Canceled)
  end;

  test "join: wait and task, pending, cancel" begin fun () ->
    let p1, r = Lwt.wait () in
    let p2, _ = Lwt.task () in
    let p3 = Lwt.join [p1; p2] in
    Lwt.cancel p3;
    assert (Lwt.state p1 = Lwt.Sleep);
    assert (Lwt.state p2 = Lwt.Fail Lwt.Canceled);
    assert (Lwt.state p3 = Lwt.Sleep);
    Lwt.wakeup r ();
    Lwt.return
      (Lwt.state p1 = Lwt.Return () && Lwt.state p3 = Lwt.Fail Lwt.Canceled)
  end;

  (* In [p' = Lwt.join [p; p]], if [p'] is canceled, the cancel handler on [p]
     is called only once, even though it is reachable by two paths in the
     cancelation graph. *)
  test "join: cancel diamond" begin fun () ->
    let ran = ref 0 in
    let p, _ = Lwt.task () in
    let p' = Lwt.join [p; p] in
    Lwt.on_cancel p (fun () -> ran := !ran + 1);
    Lwt.cancel p';
    Lwt.return (!ran = 1)
  end;
]
let tests = tests @ cancel_join_tests

let cancel_choose_tests = [
  test "choose: wait, pending, cancel" begin fun () ->
    let p1, _ = Lwt.wait () in
    let p2, _ = Lwt.wait () in
    let p3 = Lwt.choose [p1; p2] in
    Lwt.cancel p3;
    Lwt.return
      (Lwt.state p1 = Lwt.Sleep &&
       Lwt.state p2 = Lwt.Sleep &&
       Lwt.state p3 = Lwt.Sleep)
  end;

  test "choose: wait and task, pending, cancel" begin fun () ->
    let p1, _ = Lwt.wait () in
    let p2, _ = Lwt.task () in
    let p3 = Lwt.choose [p1; p2] in
    Lwt.cancel p3;
    Lwt.return
      (Lwt.state p1 = Lwt.Sleep &&
       Lwt.state p2 = Lwt.Fail Lwt.Canceled &&
       Lwt.state p3 = Lwt.Fail Lwt.Canceled)
  end;
]
let tests = tests @ cancel_choose_tests

let cancel_pick_tests = [
  test "pick: wait, pending, cancel" begin fun () ->
    let p1, _ = Lwt.wait () in
    let p2, _ = Lwt.wait () in
    let p3 = Lwt.pick [p1; p2] in
    Lwt.cancel p3;
    Lwt.return
      (Lwt.state p1 = Lwt.Sleep &&
       Lwt.state p2 = Lwt.Sleep &&
       Lwt.state p3 = Lwt.Sleep)
  end;

  test "pick: wait and task, pending, cancel" begin fun () ->
    let p1, _ = Lwt.wait () in
    let p2, _ = Lwt.task () in
    let p3 = Lwt.pick [p1; p2] in
    Lwt.cancel p3;
    Lwt.return
      (Lwt.state p1 = Lwt.Sleep &&
       Lwt.state p2 = Lwt.Fail Lwt.Canceled &&
       Lwt.state p3 = Lwt.Fail Lwt.Canceled)
  end;
]
let tests = tests @ cancel_pick_tests

let cancel_nchoose_tests = [
  test "nchoose: wait, pending, cancel" begin fun () ->
    let p1, _ = Lwt.wait () in
    let p2, _ = Lwt.wait () in
    let p3 = Lwt.nchoose [p1; p2] in
    Lwt.cancel p3;
    Lwt.return
      (Lwt.state p1 = Lwt.Sleep &&
       Lwt.state p2 = Lwt.Sleep &&
       Lwt.state p3 = Lwt.Sleep)
  end;

  test "nchoose: wait and task, pending, cancel" begin fun () ->
    let p1, _ = Lwt.wait () in
    let p2, _ = Lwt.task () in
    let p3 = Lwt.nchoose [p1; p2] in
    Lwt.cancel p3;
    Lwt.return
      (Lwt.state p1 = Lwt.Sleep &&
       Lwt.state p2 = Lwt.Fail Lwt.Canceled &&
       Lwt.state p3 = Lwt.Fail Lwt.Canceled)
  end;
]
let tests = tests @ cancel_nchoose_tests

let cancel_npick_tests = [
  test "npick: wait, pending, cancel" begin fun () ->
    let p1, _ = Lwt.wait () in
    let p2, _ = Lwt.wait () in
    let p3 = Lwt.npick [p1; p2] in
    Lwt.cancel p3;
    Lwt.return
      (Lwt.state p1 = Lwt.Sleep &&
       Lwt.state p2 = Lwt.Sleep &&
       Lwt.state p3 = Lwt.Sleep)
  end;

  test "npick: wait and task, pending, cancel" begin fun () ->
    let p1, _ = Lwt.wait () in
    let p2, _ = Lwt.task () in
    let p3 = Lwt.npick [p1; p2] in
    Lwt.cancel p3;
    Lwt.return
      (Lwt.state p1 = Lwt.Sleep &&
       Lwt.state p2 = Lwt.Fail Lwt.Canceled &&
       Lwt.state p3 = Lwt.Fail Lwt.Canceled)
  end;
]
let tests = tests @ cancel_npick_tests

let cancel_nchoose_split_tests = [
  test "nchoose_split: wait, pending, cancel" begin fun () ->
    let p1, _ = Lwt.wait () in
    let p2, _ = Lwt.wait () in
    let p3 = Lwt.nchoose_split [p1; p2] in
    Lwt.cancel p3;
    Lwt.return
      (Lwt.state p1 = Lwt.Sleep &&
       Lwt.state p2 = Lwt.Sleep &&
       Lwt.state p3 = Lwt.Sleep)
  end;

  test "nchoose_split: wait and task, pending, cancel" begin fun () ->
    let p1, _ = Lwt.wait () in
    let p2, _ = Lwt.task () in
    let p3 = Lwt.nchoose_split [p1; p2] in
    Lwt.cancel p3;
    Lwt.return
      (Lwt.state p1 = Lwt.Sleep &&
       Lwt.state p2 = Lwt.Fail Lwt.Canceled &&
       Lwt.state p3 = Lwt.Fail Lwt.Canceled)
  end;
]
let tests = tests @ cancel_nchoose_split_tests



(* Sequence-associated storage, and its interaction with the rest of the API. *)

let storage_tests = [
  test "storage: initial" begin fun () ->
    let key = Lwt.new_key () in
    Lwt.return (Lwt.get key = None)
  end;

  test "storage: store, retrieve" begin fun () ->
    let key = Lwt.new_key () in
    Lwt.with_value key (Some 42) (fun () ->
      Lwt.return (Lwt.get key = Some 42))
  end;

  test "storage: store, restore" begin fun () ->
    let key = Lwt.new_key () in
    Lwt.with_value key (Some 42) ignore;
    Lwt.return (Lwt.get key = None)
  end;

  test "storage: store, f raises, restore" begin fun () ->
    let key = Lwt.new_key () in
    try
      Lwt.with_value key (Some 42) (fun () -> raise Exception) |> ignore;
      Lwt.return false
    with Exception ->
      Lwt.return (Lwt.get key = None)
  end;

  test "storage: store, overwrite, retrieve" begin fun () ->
    let key = Lwt.new_key () in
    Lwt.with_value key (Some 42) (fun () ->
      Lwt.with_value key (Some 1337) (fun () ->
        Lwt.return (Lwt.get key = Some 1337)))
  end;

  test "storage: store, blank, retrieve" begin fun () ->
    let key = Lwt.new_key () in
    Lwt.with_value key (Some 42) (fun () ->
      Lwt.with_value key None (fun () ->
        Lwt.return (Lwt.get key = None)))
  end;

  test "storage: distinct keys" begin fun () ->
    let key1 = Lwt.new_key () in
    let key2 = Lwt.new_key () in
    Lwt.with_value key1 (Some 42) (fun () ->
      Lwt.return (Lwt.get key2 = None))
  end;

  test "bind: storage" begin fun () ->
    let key = Lwt.new_key () in
    let f = fun () -> Lwt.return (Lwt.get key) in
    let p, r = Lwt.wait () in
    Lwt.with_value key (Some 42) (fun () ->
      let p' =
        Lwt.with_value key (Some 1337) (fun () ->
          Lwt.bind p f)
      in
      Lwt.wakeup r ();
      Lwt.return
        (Lwt.state p' = Lwt.Return (Some 1337) &&
         Lwt.get key = Some 42))
  end;

  test "map: storage" begin fun () ->
    let key = Lwt.new_key () in
    let f = fun () -> Lwt.get key in
    let p, r = Lwt.wait () in
    Lwt.with_value key (Some 42) (fun () ->
      let p' =
        Lwt.with_value key (Some 1337) (fun () ->
          Lwt.map f p)
      in
      Lwt.wakeup r ();
      Lwt.return
        (Lwt.state p' = Lwt.Return (Some 1337) &&
         Lwt.get key = Some 42))
  end;

  test "catch: storage" begin fun () ->
    let key = Lwt.new_key () in
    let f = fun _ -> Lwt.return (Lwt.get key) in
    let p, r = Lwt.wait () in
    Lwt.with_value key (Some 42) (fun () ->
      let p' =
        Lwt.with_value key (Some 1337) (fun () ->
          Lwt.catch (fun () -> p) f)
      in
      Lwt.wakeup_exn r Exception;
      Lwt.return
        (Lwt.state p' = Lwt.Return (Some 1337) &&
         Lwt.get key = Some 42))
  end;

  test "try_bind: storage, resolved" begin fun () ->
    let key = Lwt.new_key () in
    let f = fun () -> Lwt.return (Lwt.get key) in
    let p, r = Lwt.wait () in
    Lwt.with_value key (Some 42) (fun () ->
      let p' =
        Lwt.with_value key (Some 1337) (fun () ->
          Lwt.try_bind (fun () -> p) f Lwt.fail)
      in
      Lwt.wakeup r ();
      Lwt.return
        (Lwt.state p' = Lwt.Return (Some 1337) &&
         Lwt.get key = Some 42))
  end;

  test "try_bind: storage, failed" begin fun () ->
    let key = Lwt.new_key () in
    let f = fun _ -> Lwt.return (Lwt.get key) in
    let p, r = Lwt.wait () in
    Lwt.with_value key (Some 42) (fun () ->
      let p' =
        Lwt.with_value key (Some 1337) (fun () ->
          Lwt.try_bind (fun () -> p) Lwt.return f)
      in
      Lwt.wakeup_exn r Exception;
      Lwt.return
        (Lwt.state p' = Lwt.Return (Some 1337) &&
         Lwt.get key = Some 42))
  end;

  test "finalize: storage" begin fun () ->
    let key = Lwt.new_key () in
    let f = fun () -> assert (Lwt.get key = Some 1337); Lwt.return () in
    let p, r = Lwt.wait () in
    Lwt.with_value key (Some 42) (fun () ->
      Lwt.with_value key (Some 1337) (fun () ->
        Lwt.finalize (fun () -> p) f) |> ignore;
      Lwt.wakeup r ();
      Lwt.return (Lwt.get key = Some 42))
  end;

  test "on_success: storage" begin fun () ->
    let key = Lwt.new_key () in
    let f = fun () -> assert (Lwt.get key = Some 1337) in
    let p, r = Lwt.wait () in
    Lwt.with_value key (Some 42) (fun () ->
      Lwt.with_value key (Some 1337) (fun () ->
        Lwt.on_success p f);
      Lwt.wakeup r ();
      Lwt.return (Lwt.get key = Some 42))
  end;

  test "on_failure: storage" begin fun () ->
    let key = Lwt.new_key () in
    let f = fun _ -> assert (Lwt.get key = Some 1337) in
    let p, r = Lwt.wait () in
    Lwt.with_value key (Some 42) (fun () ->
      Lwt.with_value key (Some 1337) (fun () ->
        Lwt.on_failure p f);
      Lwt.wakeup_exn r Exception;
      Lwt.return (Lwt.get key = Some 42))
  end;

  test "on_termination: storage, resolved" begin fun () ->
    let key = Lwt.new_key () in
    let f = fun () -> assert (Lwt.get key = Some 1337) in
    let p, r = Lwt.wait () in
    Lwt.with_value key (Some 42) (fun () ->
      Lwt.with_value key (Some 1337) (fun () ->
        Lwt.on_termination p f);
      Lwt.wakeup r ();
      Lwt.return (Lwt.get key = Some 42))
  end;

  test "on_termination: storage, failed" begin fun () ->
    let key = Lwt.new_key () in
    let f = fun () -> assert (Lwt.get key = Some 1337) in
    let p, r = Lwt.wait () in
    Lwt.with_value key (Some 42) (fun () ->
      Lwt.with_value key (Some 1337) (fun () ->
        Lwt.on_termination p f);
      Lwt.wakeup_exn r Exception;
      Lwt.return (Lwt.get key = Some 42))
  end;

  test "on_any: storage, resolved" begin fun () ->
    let key = Lwt.new_key () in
    let f = fun () -> assert (Lwt.get key = Some 1337) in
    let p, r = Lwt.wait () in
    Lwt.with_value key (Some 42) (fun () ->
      Lwt.with_value key (Some 1337) (fun () ->
        Lwt.on_any p f ignore);
      Lwt.wakeup r ();
      Lwt.return (Lwt.get key = Some 42))
  end;

  test "on_any: storage, failed" begin fun () ->
    let key = Lwt.new_key () in
    let f = fun _ -> assert (Lwt.get key = Some 1337) in
    let p, r = Lwt.wait () in
    Lwt.with_value key (Some 42) (fun () ->
      Lwt.with_value key (Some 1337) (fun () ->
        Lwt.on_any p ignore f);
      Lwt.wakeup r ();
      Lwt.return (Lwt.get key = Some 42))
  end;

  test "on_cancel: storage" begin fun () ->
    let key = Lwt.new_key () in
    let f = fun () -> assert (Lwt.get key = Some 1337) in
    let p, _ = Lwt.task () in
    Lwt.with_value key (Some 42) (fun () ->
      Lwt.with_value key (Some 1337) (fun () ->
        Lwt.on_cancel p f);
      Lwt.cancel p;
      Lwt.return (Lwt.get key = Some 42))
  end;
]
let tests = tests @ storage_tests



(* These basically just test that the infix operators are exposed in the API,
   and are defined "more or less" as they should be. *)
let infix_operator_tests = [
  test ">>=" begin fun () ->
    let open Lwt.Infix in
    let p, r = Lwt.wait () in
    let p' = p >>= (fun s -> Lwt.return (s ^ "bar")) in
    Lwt.wakeup r "foo";
    Lwt.return (Lwt.state p' = Lwt.Return "foobar")
  end;

  test "=<<" begin fun () ->
    let open Lwt.Infix in
    let p, r = Lwt.wait () in
    let p' = (fun s -> Lwt.return (s ^ "bar")) =<< p in
    Lwt.wakeup r "foo";
    Lwt.return (Lwt.state p' = Lwt.Return "foobar")
  end;

  test ">|=" begin fun () ->
    let open Lwt.Infix in
    let p, r = Lwt.wait () in
    let p' = p >|= (fun s -> s ^ "bar") in
    Lwt.wakeup r "foo";
    Lwt.return (Lwt.state p' = Lwt.Return "foobar")
  end;

  test "=|<" begin fun () ->
    let open Lwt.Infix in
    let p, r = Lwt.wait () in
    let p' = (fun s -> s ^ "bar") =|< p in
    Lwt.wakeup r "foo";
    Lwt.return (Lwt.state p' = Lwt.Return "foobar")
  end;

  test "<&>" begin fun () ->
    let open Lwt.Infix in
    let p, r = Lwt.wait () in
    let p' = p <&> Lwt.return () in
    assert (Lwt.state p' = Lwt.Sleep);
    Lwt.wakeup r ();
    Lwt.return (Lwt.state p' = Lwt.Return ())
  end;

  test "<?>" begin fun () ->
    let open Lwt.Infix in
    let p1, r = Lwt.wait () in
    let p2, _ = Lwt.wait () in
    let p3 = p1 <?> p2 in
    assert (Lwt.state p3 = Lwt.Sleep);
    Lwt.wakeup r ();
    Lwt.return (Lwt.state p3 = Lwt.Return ())
  end;
]
let tests = tests @ infix_operator_tests



(* Tests for [Lwt.add_task_l] and [Lwt.add_task_r]. *)

let lwt_sequence_contains sequence list =
  let step item ((contains_so_far, list_tail) as state) =
    if not contains_so_far then
      state
    else
      match list_tail with
      | item'::rest -> item == item', rest
      | [] -> failwith "Sequence and list not of the same length"
  in
  fst (Lwt_sequence.fold_l step sequence (true, list))

let lwt_sequence_tests = [
  test "add_task_r" begin fun () ->
    let sequence = Lwt_sequence.create () in
    let p = Lwt.add_task_r sequence in
    let p' = Lwt.add_task_r sequence in
    assert (Lwt.state p = Lwt.Sleep);
    assert (lwt_sequence_contains sequence [Obj.magic p; Obj.magic p']);
    Lwt.cancel p;
    Lwt.return
      (Lwt.state p = Lwt.Fail Lwt.Canceled &&
       lwt_sequence_contains sequence [Obj.magic p'])
  end;

  test "add_task_l" begin fun () ->
    let sequence = Lwt_sequence.create () in
    let p = Lwt.add_task_l sequence in
    let p' = Lwt.add_task_l sequence in
    assert (Lwt.state p = Lwt.Sleep);
    assert (lwt_sequence_contains sequence [Obj.magic p'; Obj.magic p]);
    Lwt.cancel p;
    Lwt.return
      (Lwt.state p = Lwt.Fail Lwt.Canceled &&
       lwt_sequence_contains sequence [Obj.magic p'])
  end;
]
let tests = tests @ lwt_sequence_tests



let pause_tests = [
  test "pause" begin fun () ->
    let p = Lwt.pause () in
    Lwt.bind p (fun () -> Lwt.pause ()) |> ignore;
    let unpause = fun n ->
      assert (Lwt.paused_count () = 2);
      assert (Lwt.paused_count () = n);
      Lwt.register_pause_notifier ignore;
      Lwt.wakeup_paused ();
      assert (Lwt.paused_count () = 1);
    in
    Lwt.register_pause_notifier unpause;
    Lwt.pause () |> ignore;
    Lwt.return true
  end;
]
let tests = tests @ pause_tests



(* [Lwt.apply] and [Lwt.wrapN]. *)
let lift_tests = [
  test "apply" begin fun () ->
    let p = Lwt.apply (fun s -> Lwt.return (s ^ "bar")) "foo" in
    Lwt.return (Lwt.state p = Lwt.Return "foobar")
  end;

  test "apply: raises" begin fun () ->
    let p = Lwt.apply (fun () -> raise Exception) () in
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "wrap" begin fun () ->
    let p = Lwt.wrap (fun () -> "foo") in
    Lwt.return (Lwt.state p = Lwt.Return "foo")
  end;

  test "wrap: raises" begin fun () ->
    let p = Lwt.wrap (fun () -> raise Exception) in
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "wrap1" begin fun () ->
    let p = Lwt.wrap1 (fun x1 -> x1) 1 in
    Lwt.return (Lwt.state p = Lwt.Return 1)
  end;

  test "wrap1: raises" begin fun () ->
    let p = Lwt.wrap1 (fun _ -> raise Exception) () in
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "wrap2" begin fun () ->
    let p = Lwt.wrap2 (fun x1 x2 -> x1 + x2) 1 2 in
    Lwt.return (Lwt.state p = Lwt.Return 3)
  end;

  test "wrap2: raises" begin fun () ->
    let p = Lwt.wrap2 (fun _ _ -> raise Exception) () () in
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "wrap3" begin fun () ->
    let p = Lwt.wrap3 (fun x1 x2 x3 -> x1 + x2 + x3) 1 2 3 in
    Lwt.return (Lwt.state p = Lwt.Return 6)
  end;

  test "wrap3: raises" begin fun () ->
    let p = Lwt.wrap3 (fun _ _ _ -> raise Exception) () () () in
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "wrap4" begin fun () ->
    let p = Lwt.wrap4 (fun x1 x2 x3 x4 -> x1 + x2 + x3 + x4) 1 2 3 4 in
    Lwt.return (Lwt.state p = Lwt.Return 10)
  end;

  test "wrap4: raises" begin fun () ->
    let p = Lwt.wrap4 (fun _ _ _ _ -> raise Exception) () () () () in
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "wrap5" begin fun () ->
    let p =
      Lwt.wrap5 (fun x1 x2 x3 x4 x5 -> x1 + x2 + x3 + x4 + x5) 1 2 3 4 5 in
    Lwt.return (Lwt.state p = Lwt.Return 15)
  end;

  test "wrap5: raises" begin fun () ->
    let p = Lwt.wrap5 (fun _ _ _ _ _ -> raise Exception) () () () () () in
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "wrap6" begin fun () ->
    let p =
      Lwt.wrap6
        (fun x1 x2 x3 x4 x5 x6 -> x1 + x2 + x3 + x4 + x5 + x6) 1 2 3 4 5 6
    in
    Lwt.return (Lwt.state p = Lwt.Return 21)
  end;

  test "wrap6: raises" begin fun () ->
    let p = Lwt.wrap6 (fun _ _ _ _ _ _ -> raise Exception) () () () () () () in
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;

  test "wrap7" begin fun () ->
    let p =
      Lwt.wrap7
        (fun x1 x2 x3 x4 x5 x6 x7 -> x1 + x2 + x3 + x4 + x5 + x6 + x7)
        1 2 3 4 5 6 7
    in
    Lwt.return (Lwt.state p = Lwt.Return 28)
  end;

  test "wrap7: raises" begin fun () ->
    let p =
      Lwt.wrap7 (fun _ _ _ _ _ _ _ -> raise Exception) () () () () () () () in
    Lwt.return (Lwt.state p = Lwt.Fail Exception)
  end;
]
let tests = tests @ lift_tests



(* [Lwt.make_value] and [Lwt.make_error] are deprecated, but test them anyway,
   for good measure. *)
let make_value_and_error_tests = [
  test "make_value" begin fun () ->
    Lwt.return ((Lwt.make_value [@ocaml.warning "-3"]) 42 = Result.Ok 42)
  end;

  test "make_error" begin fun () ->
    Lwt.return
      ((Lwt.make_error [@ocaml.warning "-3"]) Exception =
        Result.Error Exception)
  end;
]
let tests = tests @ make_value_and_error_tests



(* These tests exercise the trigger cleanup mechanism of the Lwt core, which is
   an implementation detail. When a promise [p] is repeatedly used in fuctions
   such as [Lwt.choose], but remains pending, while other promises passed to
   [Lwt.choose] complete, [p] accumulates disabled trigger cells. They need to
   be occasionally cleaned up; in particular, this should happen every
   [trigger_cleanup_point] [Lwt.choose] operations.

   As an extra twist, if [f] in [p' = Lwt.bind _ f] returns a pending promise
   [p], that pending promise's trigger cells, including the disabled ones, are
   appended to the trigger cells of [p']. If the sum of [Lwt.choose] operations
   performed on [p] and [p'] is more than [trigger_cleanup_point], disabled
   trigger cells also need to be cleaned up on [p'].

   The tests below trigger the cleanup code, and make sure that non-disabled
   trigger cells survive the cleanup. *)

let trigger_cleanup_point = 42

let trigger_list_tests = [
  test "trigger cleanup: choose" begin fun () ->
    let p1, r1 = Lwt.wait () in
    let p2 = Lwt.bind p1 (fun s -> Lwt.return (s ^ "bar")) in
    let p3 = Lwt.choose [p1; fst (Lwt.wait ())] in
    let rec repeat = function
      | 0 -> ()
      | n ->
        let p4, r4 = Lwt.wait () in
        Lwt.choose [p1; p4] |> ignore;
        Lwt.wakeup r4 "";
        repeat (n - 1)
    in
    repeat (trigger_cleanup_point + 1);
    Lwt.wakeup r1 "foo";
    Lwt.return
      (Lwt.state p2 = Lwt.Return "foobar" && Lwt.state p3 = Lwt.Return "foo")
  end;

  test "trigger cleanup: bind" begin fun () ->
    let p1, r1 = Lwt.wait () in
    let p2, r2 = Lwt.wait () in
    let p3 = Lwt.bind p1 (fun () -> p2) in
    let p4 = Lwt.map ignore p2 in
    let p5 = Lwt.map ignore p3 in
    let rec repeat = function
      | 0 -> ()
      | n ->
        let p6, r6 = Lwt.wait () in
        Lwt.choose [p2; p3; p6] |> ignore;
        Lwt.wakeup r6 ();
        repeat (n - 1)
    in
    repeat ((trigger_cleanup_point / 2) + 1);
    Lwt.wakeup r1 ();
    Lwt.wakeup r2 ();
    Lwt.return (Lwt.state p4 = Lwt.Return () && Lwt.state p5 = Lwt.Return ())
  end;
]
let tests = tests @ trigger_list_tests



let suite = suite "lwt" tests
