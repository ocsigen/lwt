(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* [Lwt_sequence] is deprecated – we don't want users outside Lwt using it.
   However, it is still used internally by Lwt. So, briefly disable warning 3
   ("deprecated"), and create a local, non-deprecated alias for
   [Lwt_sequence] that can be referred to by the rest of the code in this
   module without triggering any more warnings. *)
[@@@ocaml.warning "-3"]

module Lwt_sequence = Lwt_sequence

[@@@ocaml.warning "+3"]

open Test

let state_is = Lwt.debug_state_is

(* When using JavaScript promises, this runs [f] on the next "tick." *)
let later f = Lwt.map f Lwt.return_unit

(* An exception type fresh to this testing module. *)
exception Exception

let set_async_exception_hook hook =
  let saved = !Lwt.async_exception_hook in
  let restore () = Lwt.async_exception_hook := saved in
  Lwt.async_exception_hook := hook;
  restore

(* An add_loc function for [Lwt.backtrace_bind], etc. This should be defined
   literally at each place it is used, and it should be tested that the location
   of the re-raise is added to the backtrace. However, I believe that backtraces
   are broken right now, so neither of these is done. *)
let add_loc exn = try raise exn with exn -> exn

(* The list of all the test suites in this file. This name is repeatedly
   shadowed as more and more test suites are defined. The purpose is to keep the
   aggregation of test suites local to their definition, instead of having to
   maintain a list in a separate location in the code. *)
let suites : Test.suite list = []

(* Tests for promises created with [Lwt.return], [Lwt.fail], and related
   functions, as well as state query (hard to test one without the other).
   These tests use assertions instead of relying on the correctness of a final
   [Lwt.return], not that it's particularly likely to be broken. *)

let trivial_promise_tests =
  suite "trivial promises"
    [
      test "return" (fun () -> state_is (Lwt.Return "foo") (Lwt.return "foo"));
      test "reject" (fun () ->
          state_is (Lwt.Fail Exception) (Lwt.fail Exception));
      test "of_result: fulfilled" (fun () ->
          state_is (Lwt.Return "foo") (Lwt.of_result (Result.Ok "foo")));
      test "of_result: rejected" (fun () ->
          state_is (Lwt.Fail Exception) (Lwt.of_result (Result.Error Exception)));
      test "return_unit" (fun () -> state_is (Lwt.Return ()) Lwt.return_unit);
      test "return_true" (fun () -> state_is (Lwt.Return true) Lwt.return_true);
      test "return_false" (fun () ->
          state_is (Lwt.Return false) Lwt.return_false);
      test "return_none" (fun () -> state_is (Lwt.Return None) Lwt.return_none);
      test "return_some" (fun () ->
          state_is (Lwt.Return (Some "foo")) (Lwt.return_some "foo"));
      test "return_ok" (fun () ->
          state_is (Lwt.Return (Result.Ok "foo")) (Lwt.return_ok "foo"));
      test "return_error" (fun () ->
          state_is (Lwt.Return (Result.Error "foo")) (Lwt.return_error "foo"));
      test "fail_with" (fun () ->
          state_is (Lwt.Fail (Failure "foo")) (Lwt.fail_with "foo"));
      test "fail_invalid_arg" (fun () ->
          state_is (Lwt.Fail (Invalid_argument "foo"))
            (Lwt.fail_invalid_arg "foo"));
    ]

let suites = suites @ [ trivial_promise_tests ]

(* Tests for promises created with [Lwt.wait] and [Lwt.task], not including
   tests for cancellation of the latter. Tests for double use of [Lwt.wakeup]
   and related functions are in a separated suite. So are tests for
   [Lwt.wakeup_later] and related functions. *)

let initial_promise_tests =
  suite "initial promises"
    [
      test "wait: pending" (fun () ->
          let p, _ = Lwt.wait () in
          state_is Lwt.Sleep p);
      test "task: pending" (fun () ->
          let p, _ = Lwt.task () in
          state_is Lwt.Sleep p);
      test "wait: fulfill" (fun () ->
          let p, r = Lwt.wait () in
          Lwt.wakeup r "foo";
          state_is (Lwt.Return "foo") p);
      test "task: fulfill" (fun () ->
          let p, r = Lwt.task () in
          Lwt.wakeup r "foo";
          state_is (Lwt.Return "foo") p);
      test "wait: reject" (fun () ->
          let p, r = Lwt.wait () in
          Lwt.wakeup_exn r Exception;
          state_is (Lwt.Fail Exception) p);
      test "task: reject" (fun () ->
          let p, r = Lwt.task () in
          Lwt.wakeup_exn r Exception;
          state_is (Lwt.Fail Exception) p);
      test "wait: resolve" (fun () ->
          let p, r = Lwt.wait () in
          Lwt.wakeup_result r (Result.Ok "foo");
          state_is (Lwt.Return "foo") p);
      test "task: resolve" (fun () ->
          let p, r = Lwt.task () in
          Lwt.wakeup_result r (Result.Ok "foo");
          state_is (Lwt.Return "foo") p);
      test "waiter_of_wakener" (fun () ->
          let p, r = Lwt.wait () in
          Lwt.return ((Lwt.waiter_of_wakener [@ocaml.warning "-3"]) r == p));
    ]

let suites = suites @ [ initial_promise_tests ]

let double_resolve_tests =
  suite "double resolve"
    [
      (test "wakeup: double use on wait" (fun () ->
           let _, r = Lwt.wait () in
           Lwt.wakeup r "foo";
           try
             Lwt.wakeup r "foo";
             Lwt.return false
           with Invalid_argument "Lwt.wakeup" -> Lwt.return true)
       [@ocaml.warning "-52"]);
      (test "wakeup: double use on task" (fun () ->
           let _, r = Lwt.task () in
           Lwt.wakeup r "foo";
           try
             Lwt.wakeup r "foo";
             Lwt.return false
           with Invalid_argument "Lwt.wakeup" -> Lwt.return true)
       [@ocaml.warning "-52"]);
      (test "wakeup_exn: double use on wait" (fun () ->
           let _, r = Lwt.wait () in
           Lwt.wakeup_exn r Exception;
           try
             Lwt.wakeup_exn r Exception;
             Lwt.return false
           with Invalid_argument "Lwt.wakeup_exn" -> Lwt.return true)
       [@ocaml.warning "-52"]);
      (test "wakeup_exn: double use on task" (fun () ->
           let _, r = Lwt.task () in
           Lwt.wakeup_exn r Exception;
           try
             Lwt.wakeup_exn r Exception;
             Lwt.return false
           with Invalid_argument "Lwt.wakeup_exn" -> Lwt.return true)
       [@ocaml.warning "-52"]);
      (test "wakeup_result: double use on wait" (fun () ->
           let _, r = Lwt.wait () in
           Lwt.wakeup_exn r Exception;
           try
             Lwt.wakeup_result r (Result.Ok ());
             Lwt.return false
           with Invalid_argument "Lwt.wakeup_result" -> Lwt.return true)
       [@ocaml.warning "-52"]);
      (test "wakeup_result: double use on task" (fun () ->
           let _, r = Lwt.task () in
           Lwt.wakeup_exn r Exception;
           try
             Lwt.wakeup_result r (Result.Ok ());
             Lwt.return false
           with Invalid_argument "Lwt.wakeup_result" -> Lwt.return true)
       [@ocaml.warning "-52"]);
    ]

let suites = suites @ [ double_resolve_tests ]

(* Tests for sequential composition functions, such as [Lwt.bind], but not
   including testing for interaction with cancellation and sequence-associated
   storage. Those tests come later. *)

let bind_tests =
  suite "bind"
    [
      test "already fulfilled" (fun () ->
          let p = Lwt.return "foo" in
          let p = Lwt.bind p (fun s -> Lwt.return (s ^ "bar")) in
          state_is (Lwt.Return "foobar") p);
      (* A somewhat surprising behavior of native [bind] is that if [p] is fulfilled
         and [f] raises before evaluating to a promise, [bind p f] raises, instead
         of evaluating to a promise. On the other hand, if [p] is pending, and [f]
         raises, the exception is folded into the promise resulting from [bind].
         See

           https://github.com/ocsigen/lwt/issues/329 *)
      test "already fulfilled, f raises" (fun () ->
          let p = Lwt.return "foo" in
          try
            Lwt.bind p (fun _ -> raise Exception) |> ignore;
            Lwt.return false
          with Exception -> Lwt.return true);
      test "already rejected" (fun () ->
          let p = Lwt.fail Exception in
          let p = Lwt.bind p (fun _ -> Lwt.return "foo") in
          state_is (Lwt.Fail Exception) p);
      test "pending" (fun () ->
          let f_ran = ref false in
          let p, _ = Lwt.wait () in
          let p =
            Lwt.bind p (fun _ ->
                f_ran := true;
                Lwt.return ())
          in
          Lwt.bind (state_is Lwt.Sleep p) (fun correct ->
              Lwt.return (correct && !f_ran = false)));
      test "pending, fulfilled" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.bind p (fun s -> Lwt.return (s ^ "bar")) in
          Lwt.wakeup r "foo";
          state_is (Lwt.Return "foobar") p);
      test "pending, fulfilled, f raises" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.bind p (fun _ -> raise Exception) in
          Lwt.wakeup r ();
          state_is (Lwt.Fail Exception) p);
      test "pending, rejected" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.bind p (fun _ -> Lwt.return "foo") in
          Lwt.wakeup_exn r Exception;
          state_is (Lwt.Fail Exception) p);
      test "chain" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2 = Lwt.bind p1 (fun s -> Lwt.return (s ^ "bar")) in
          let p3 = Lwt.bind p2 (fun s -> Lwt.return (s ^ "!!1")) in
          Lwt.wakeup r1 "foo";
          state_is (Lwt.Return "foobar!!1") p3);
      test "suspended chain" (fun () ->
          let p1, r = Lwt.wait () in
          let p2 = Lwt.return "foo" in
          let p3 = Lwt.bind p1 (fun () -> p2) in
          let p4 = Lwt.bind p1 (fun () -> p3) in
          Lwt.wakeup r ();
          state_is (Lwt.Return "foo") p4);
      test "fanout" (fun () ->
          let p1, r = Lwt.wait () in
          let p2 = Lwt.bind p1 (fun s -> Lwt.return (s ^ "bar")) in
          let p3 = Lwt.bind p1 (fun s -> Lwt.return (s ^ "!!1")) in
          let p4 = Lwt.bind p1 (fun s -> Lwt.return (s ^ "omg")) in
          Lwt.wakeup r "foo";
          Lwt.bind (state_is (Lwt.Return "foobar") p2) (fun p2_correct ->
              Lwt.bind (state_is (Lwt.Return "foo!!1") p3) (fun p3_correct ->
                  Lwt.bind (state_is (Lwt.Return "fooomg") p4)
                    (fun p4_correct ->
                      Lwt.return (p2_correct && p3_correct && p4_correct)))));
      test "double pending" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p = Lwt.bind p1 (fun _ -> p2) in
          Lwt.wakeup r1 "foo";
          assert (Lwt.state p = Lwt.Sleep);
          Lwt.wakeup r2 "bar";
          state_is (Lwt.Return "bar") p);
      test "same pending" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.bind p (fun _ -> p) in
          Lwt.wakeup r "foo";
          state_is (Lwt.Return "foo") p);
      test "nested" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p =
            Lwt.bind p1 (fun s -> Lwt.bind p2 (fun s' -> Lwt.return (s ^ s')))
          in
          Lwt.wakeup r1 "foo";
          Lwt.wakeup r2 "bar";
          state_is (Lwt.Return "foobar") p);
      (* This tests an implementation detail, namely the construction and flattening
         of a chain of proxy promises. *)
      test "proxy chain" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p3, r3 = Lwt.wait () in
          let p4 = Lwt.bind p1 (fun _ -> p3) in
          let p5 = Lwt.bind p2 (fun _ -> p4) in
          Lwt.wakeup r1 ();
          Lwt.wakeup r2 ();
          Lwt.wakeup r3 "bar";
          Lwt.bind (state_is (Lwt.Return "bar") p3) (fun p3_correct ->
              Lwt.bind (state_is (Lwt.Return "bar") p4) (fun p4_correct ->
                  Lwt.bind (state_is (Lwt.Return "bar") p5) (fun p5_correct ->
                      Lwt.return (p3_correct && p4_correct && p5_correct)))));
      (* This tests an implementation detail, namely that proxy promise chaining
         does not form cycles. It's only relevant for the native implementation. *)
      test "cycle" (fun () ->
          let p, r = Lwt.wait () in
          let p' = ref (Lwt.return ()) in
          p' := Lwt.bind p (fun _ -> !p');
          Lwt.wakeup r ();
          Lwt.return (Lwt.state !p' = Lwt.Sleep));
      (* This tests the effect of an implementation detail: if a promise is going to
         be resolved by a callback, but that promise becomes a proxy synchronously
         during that callback, everything still works. *)
      test "proxy during callback" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p3 =
            Lwt.bind p1 (fun () ->
                (* Synchronously resolve [p2]. Because of the [bind] below, [p3] will
                   become a proxy for [p4] while this callback is still running. We
                   then finish the callback by returning [true]. If [bind] is
                   implemented correctly, it will follow the [p3] proxy link to [p4]
                   only after the callback returns. In an earlier incorrect
                   implementation, this code could cause Lwt to hang forever, or crash
                   the process. *)
                Lwt.wakeup r2 ();
                Lwt.return true)
          in
          let p4 = Lwt.bind p2 (fun () -> p3) in
          Lwt.wakeup r1 ();
          p4);
    ]

let suites = suites @ [ bind_tests ]

let backtrace_bind_tests =
  suite "backtrace_bind"
    [
      test "fulfilled" (fun () ->
          let p = Lwt.return "foo" in
          let p =
            Lwt.backtrace_bind add_loc p (fun s -> Lwt.return @@ s ^ "bar")
          in
          state_is (Lwt.Return "foobar") p);
      test "rejected" (fun () ->
          let p = Lwt.fail Exception in
          let p = Lwt.backtrace_bind add_loc p (fun _ -> Lwt.return "foo") in
          state_is (Lwt.Fail Exception) p);
      test "pending, fulfilled" (fun () ->
          let p, r = Lwt.wait () in
          let p =
            Lwt.backtrace_bind add_loc p (fun s -> Lwt.return (s ^ "bar"))
          in
          Lwt.wakeup r "foo";
          state_is (Lwt.Return "foobar") p);
      test "pending, fulfilled, f raises" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.backtrace_bind add_loc p (fun () -> raise Exception) in
          Lwt.wakeup r ();
          state_is (Lwt.Fail Exception) p);
      test "pending, rejected" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.backtrace_bind add_loc p (fun _ -> Lwt.return "foo") in
          Lwt.wakeup_exn r Exception;
          state_is (Lwt.Fail Exception) p);
      (* See "proxy during callback" in [bind] tests. *)
      test "proxy during callback" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p3 =
            Lwt.backtrace_bind add_loc p1 (fun () ->
                Lwt.wakeup r2 ();
                Lwt.return true)
          in
          let p4 = Lwt.bind p2 (fun () -> p3) in
          Lwt.wakeup r1 ();
          p4);
    ]

let suites = suites @ [ backtrace_bind_tests ]

let map_tests =
  suite "map"
    [
      test "fulfilled" (fun () ->
          let p = Lwt.return "foo" in
          let p = Lwt.map (fun s -> s ^ "bar") p in
          state_is (Lwt.Return "foobar") p);
      test "fulfilled, f raises" (fun () ->
          let p = Lwt.return "foo" in
          let p = Lwt.map (fun _ -> raise Exception) p in
          state_is (Lwt.Fail Exception) p);
      test "rejected" (fun () ->
          let p = Lwt.fail Exception in
          let p = Lwt.map (fun _ -> "foo") p in
          state_is (Lwt.Fail Exception) p);
      test "pending" (fun () ->
          let f_ran = ref false in
          let p, _ = Lwt.wait () in
          let p = Lwt.map (fun _ -> f_ran := true) p in
          Lwt.bind (state_is Lwt.Sleep p) (fun correct ->
              Lwt.return (correct && !f_ran = false)));
      test "pending, fulfilled" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.map (fun s -> s ^ "bar") p in
          Lwt.wakeup r "foo";
          state_is (Lwt.Return "foobar") p);
      test "pending, fulfilled, f raises" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.map (fun () -> raise Exception) p in
          Lwt.wakeup r ();
          state_is (Lwt.Fail Exception) p);
      test "pending, rejected" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.map (fun _ -> Lwt.return "foo") p in
          Lwt.wakeup_exn r Exception;
          state_is (Lwt.Fail Exception) p);
      (* See "proxy during callback" in [bind] tests. *)
      test "proxy during callback" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p3 =
            Lwt.map
              (fun () ->
                Lwt.wakeup r2 ();
                true)
              p1
          in
          let p4 = Lwt.bind p2 (fun () -> p3) in
          Lwt.wakeup r1 ();
          p4);
    ]

let suites = suites @ [ map_tests ]

let catch_tests =
  suite "catch"
    [
      test "fulfilled" (fun () ->
          let p =
            Lwt.catch (fun () -> Lwt.return "foo") (fun _ -> Lwt.return "bar")
          in
          state_is (Lwt.Return "foo") p);
      test "f raises" (fun () ->
          let p =
            Lwt.catch (fun () -> raise Exception) (fun exn -> Lwt.return exn)
          in
          state_is (Lwt.Return Exception) p);
      test "rejected" (fun () ->
          let p =
            Lwt.catch (fun () -> Lwt.fail Exception) (fun exn -> Lwt.return exn)
          in
          state_is (Lwt.Return Exception) p);
      (* This is an analog of the "bind quirk," see

           https://github.com/ocsigen/lwt/issues/329 *)
      test "rejected, h raises" (fun () ->
          try
            ignore
            @@ Lwt.catch (fun () -> Lwt.fail Exit) (fun _ -> raise Exception);
            Lwt.return false
          with Exception -> Lwt.return true);
      test "pending" (fun () ->
          let h_ran = ref false in
          let p =
            Lwt.catch
              (fun () -> fst (Lwt.wait ()))
              (fun _ ->
                h_ran := true;
                Lwt.return ())
          in
          Lwt.bind (state_is Lwt.Sleep p) (fun correct ->
              Lwt.return (correct && !h_ran = false)));
      test "pending, fulfilled" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.catch (fun () -> p) (fun _ -> Lwt.return "bar") in
          Lwt.wakeup r "foo";
          state_is (Lwt.Return "foo") p);
      test "pending, rejected" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.catch (fun () -> p) (fun exn -> Lwt.return exn) in
          Lwt.wakeup_exn r Exception;
          state_is (Lwt.Return Exception) p);
      test "pending, rejected, h raises" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.catch (fun () -> p) (fun _ -> raise Exception) in
          Lwt.wakeup_exn r Exit;
          state_is (Lwt.Fail Exception) p);
      test "pending, rejected, h pending" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p = Lwt.catch (fun () -> p1) (fun _ -> p2) in
          Lwt.wakeup_exn r1 Exception;
          assert (Lwt.state p = Lwt.Sleep);
          Lwt.wakeup r2 "foo";
          state_is (Lwt.Return "foo") p);
      (* See "proxy during callback" in [bind] tests. *)
      test "proxy during callback" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p3 =
            Lwt.catch
              (fun () -> p1)
              (fun _exn ->
                Lwt.wakeup r2 ();
                Lwt.return true)
          in
          let p4 = Lwt.bind p2 (fun () -> p3) in
          Lwt.wakeup_exn r1 Exit;
          p4);
    ]

let suites = suites @ [ catch_tests ]

let backtrace_catch_tests =
  suite "backtrace_catch"
    [
      test "fulfilled" (fun () ->
          let p =
            Lwt.backtrace_catch add_loc
              (fun () -> Lwt.return "foo")
              (fun _ -> Lwt.return "bar")
          in
          state_is (Lwt.Return "foo") p);
      test "f raises" (fun () ->
          let p =
            Lwt.backtrace_catch add_loc
              (fun () -> raise Exception)
              (fun exn -> Lwt.return exn)
          in
          state_is (Lwt.Return Exception) p);
      test "rejected" (fun () ->
          let p =
            Lwt.backtrace_catch add_loc
              (fun () -> Lwt.fail Exception)
              (fun exn -> Lwt.return exn)
          in
          state_is (Lwt.Return Exception) p);
      test "pending" (fun () ->
          let h_ran = ref false in
          let p =
            Lwt.backtrace_catch add_loc
              (fun () -> fst (Lwt.wait ()))
              (fun _ ->
                h_ran := true;
                Lwt.return ())
          in
          state_is Lwt.Sleep p);
      test "pending, fulfilled" (fun () ->
          let p, r = Lwt.wait () in
          let p =
            Lwt.backtrace_catch add_loc
              (fun () -> p)
              (fun _ -> Lwt.return "bar")
          in
          Lwt.wakeup r "foo";
          state_is (Lwt.Return "foo") p);
      test "pending, rejected" (fun () ->
          let p, r = Lwt.wait () in
          let p =
            Lwt.backtrace_catch add_loc
              (fun () -> p)
              (fun exn -> Lwt.return exn)
          in
          Lwt.wakeup_exn r Exception;
          state_is (Lwt.Return Exception) p);
      test "pending, rejected, h raises" (fun () ->
          let p, r = Lwt.wait () in
          let p =
            Lwt.backtrace_catch add_loc (fun () -> p) (fun _ -> raise Exception)
          in
          Lwt.wakeup_exn r Exit;
          state_is (Lwt.Fail Exception) p);
      (* See "proxy during callback" in [bind] tests. *)
      test "proxy during callback" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p3 =
            Lwt.backtrace_catch add_loc
              (fun () -> p1)
              (fun _exn ->
                Lwt.wakeup r2 ();
                Lwt.return true)
          in
          let p4 = Lwt.bind p2 (fun () -> p3) in
          Lwt.wakeup_exn r1 Exit;
          p4);
    ]

let suites = suites @ [ backtrace_catch_tests ]

let try_bind_tests =
  suite "try_bind"
    [
      test "fulfilled" (fun () ->
          let p =
            Lwt.try_bind
              (fun () -> Lwt.return "foo")
              (fun s -> Lwt.return (s ^ "bar"))
              (fun _ -> Lwt.return "!!1")
          in
          state_is (Lwt.Return "foobar") p);
      (* An analog of the bind quirk. *)
      test "fulfilled, f' raises" (fun () ->
          try
            ignore
            @@ Lwt.try_bind
                 (fun () -> Lwt.return ())
                 (fun () -> raise Exception)
                 (fun _ -> Lwt.return ());
            Lwt.return false
          with Exception -> Lwt.return true);
      test "rejected" (fun () ->
          let p =
            Lwt.try_bind
              (fun () -> Lwt.fail Exception)
              (fun _ -> Lwt.return Exit)
              (fun exn -> Lwt.return exn)
          in
          state_is (Lwt.Return Exception) p);
      test "f raises" (fun () ->
          let p =
            Lwt.try_bind
              (fun () -> raise Exception)
              (fun _ -> Lwt.return Exit)
              (fun exn -> Lwt.return exn)
          in
          state_is (Lwt.Return Exception) p);
      (* Another analog of the bind quirk *)
      test "rejected, h raises" (fun () ->
          try
            ignore
            @@ Lwt.try_bind
                 (fun () -> Lwt.fail Exit)
                 (fun _ -> Lwt.return ())
                 (fun _ -> raise Exception);
            Lwt.return false
          with Exception -> Lwt.return true);
      test "pending" (fun () ->
          let f_ran = ref false in
          let p, _ = Lwt.wait () in
          let p =
            Lwt.try_bind
              (fun () -> p)
              (fun _ ->
                f_ran := true;
                Lwt.return ())
              (fun _ ->
                f_ran := true;
                Lwt.return ())
          in
          Lwt.bind (state_is Lwt.Sleep p) (fun correct ->
              Lwt.return (correct && not !f_ran)));
      test "pending, fulfilled" (fun () ->
          let p, r = Lwt.wait () in
          let p =
            Lwt.try_bind
              (fun () -> p)
              (fun s -> Lwt.return (s ^ "bar"))
              (fun _ -> Lwt.return "!!1")
          in
          Lwt.wakeup r "foo";
          state_is (Lwt.Return "foobar") p);
      test "pending, fulfilled, f' raises" (fun () ->
          let p, r = Lwt.wait () in
          let p =
            Lwt.try_bind
              (fun () -> p)
              (fun _ -> raise Exception)
              (fun _ -> Lwt.return ())
          in
          Lwt.wakeup r ();
          state_is (Lwt.Fail Exception) p);
      test "pending, fulfilled, f' pending" (fun () ->
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
          state_is (Lwt.Return "foo") p);
      test "pending, rejected" (fun () ->
          let p, r = Lwt.wait () in
          let p =
            Lwt.try_bind
              (fun () -> p)
              (fun _ -> Lwt.return Exit)
              (fun exn -> Lwt.return exn)
          in
          Lwt.wakeup_exn r Exception;
          state_is (Lwt.Return Exception) p);
      test "pending, rejected, h raises" (fun () ->
          let p, r = Lwt.wait () in
          let p =
            Lwt.try_bind
              (fun () -> p)
              (fun _ -> Lwt.return ())
              (fun _ -> raise Exception)
          in
          Lwt.wakeup_exn r Exit;
          state_is (Lwt.Fail Exception) p);
      test "pending, rejected, h pending" (fun () ->
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
          state_is (Lwt.Return "bar") p);
      (* See "proxy during callback" in [bind] tests. *)
      test "proxy during callback (fulfilled)" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p3 =
            Lwt.try_bind
              (fun () -> p1)
              (fun () ->
                Lwt.wakeup r2 ();
                Lwt.return true)
              (fun _exn -> Lwt.return false)
          in
          let p4 = Lwt.bind p2 (fun () -> p3) in
          Lwt.wakeup r1 ();
          p4);
      (* See "proxy during callback" in [bind] tests. *)
      test "proxy during callback (rejected)" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p3 =
            Lwt.try_bind
              (fun () -> p1)
              (fun () -> Lwt.return false)
              (fun _exn ->
                Lwt.wakeup r2 ();
                Lwt.return true)
          in
          let p4 = Lwt.bind p2 (fun () -> p3) in
          Lwt.wakeup_exn r1 Exit;
          p4);
    ]

let suites = suites @ [ try_bind_tests ]

let backtrace_try_bind_tests =
  suite "backtrace_try_bind"
    [
      test "fulfilled" (fun () ->
          let p =
            Lwt.backtrace_try_bind add_loc
              (fun () -> Lwt.return "foo")
              (fun s -> Lwt.return (s ^ "bar"))
              (fun _ -> Lwt.return "!!1")
          in
          state_is (Lwt.Return "foobar") p);
      test "rejected" (fun () ->
          let p =
            Lwt.backtrace_try_bind add_loc
              (fun () -> Lwt.fail Exception)
              (fun _ -> Lwt.return Exit)
              (fun exn -> Lwt.return exn)
          in
          state_is (Lwt.Return Exception) p);
      test "f raises" (fun () ->
          let p =
            Lwt.backtrace_try_bind add_loc
              (fun () -> raise Exception)
              (fun _ -> Lwt.return Exit)
              (fun exn -> Lwt.return exn)
          in
          state_is (Lwt.Return Exception) p);
      test "pending" (fun () ->
          let f_ran = ref false in
          let p, _ = Lwt.wait () in
          let p =
            Lwt.backtrace_try_bind add_loc
              (fun () -> p)
              (fun _ ->
                f_ran := true;
                Lwt.return ())
              (fun _ ->
                f_ran := true;
                Lwt.return ())
          in
          state_is Lwt.Sleep p);
      test "pending, fulfilled" (fun () ->
          let p, r = Lwt.wait () in
          let p =
            Lwt.backtrace_try_bind add_loc
              (fun () -> p)
              (fun s -> Lwt.return (s ^ "bar"))
              (fun _ -> Lwt.return "!!1")
          in
          Lwt.wakeup r "foo";
          state_is (Lwt.Return "foobar") p);
      test "pending, fulfilled, f' raises" (fun () ->
          let p, r = Lwt.wait () in
          let p =
            Lwt.backtrace_try_bind add_loc
              (fun () -> p)
              (fun _ -> raise Exception)
              (fun _ -> Lwt.return ())
          in
          Lwt.wakeup r ();
          state_is (Lwt.Fail Exception) p);
      test "pending, rejected" (fun () ->
          let p, r = Lwt.wait () in
          let p =
            Lwt.backtrace_try_bind add_loc
              (fun () -> p)
              (fun _ -> Lwt.return Exit)
              (fun exn -> Lwt.return exn)
          in
          Lwt.wakeup_exn r Exception;
          state_is (Lwt.Return Exception) p);
      test "pending, rejected, h raises" (fun () ->
          let p, r = Lwt.wait () in
          let p =
            Lwt.backtrace_try_bind add_loc
              (fun () -> p)
              (fun _ -> Lwt.return ())
              (fun _ -> raise Exception)
          in
          Lwt.wakeup_exn r Exit;
          state_is (Lwt.Fail Exception) p);
      (* See "proxy during callback" in [bind] tests. *)
      test "proxy during callback (fulfilled)" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p3 =
            Lwt.backtrace_try_bind add_loc
              (fun () -> p1)
              (fun () ->
                Lwt.wakeup r2 ();
                Lwt.return true)
              (fun _exn -> Lwt.return false)
          in
          let p4 = Lwt.bind p2 (fun () -> p3) in
          Lwt.wakeup r1 ();
          p4);
      (* See "proxy during callback" in [bind] tests. *)
      test "proxy during callback (rejected)" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p3 =
            Lwt.backtrace_try_bind add_loc
              (fun () -> p1)
              (fun () -> Lwt.return false)
              (fun _exn ->
                Lwt.wakeup r2 ();
                Lwt.return true)
          in
          let p4 = Lwt.bind p2 (fun () -> p3) in
          Lwt.wakeup_exn r1 Exit;
          p4);
    ]

let suites = suites @ [ backtrace_try_bind_tests ]

let finalize_tests =
  suite "finalize"
    [
      test "fulfilled" (fun () ->
          let f'_ran = ref false in
          let p =
            Lwt.finalize
              (fun () -> Lwt.return "foo")
              (fun () ->
                f'_ran := true;
                Lwt.return ())
          in
          Lwt.bind (state_is (Lwt.Return "foo") p) (fun correct ->
              Lwt.return (correct && !f'_ran = true)));
      test "fulfilled, f' rejected" (fun () ->
          let p =
            Lwt.finalize
              (fun () -> Lwt.return ())
              (fun () -> Lwt.fail Exception)
          in
          state_is (Lwt.Fail Exception) p);
      (* An instance of the bind quirk. *)
      test "fulfilled, f' raises" (fun () ->
          try
            ignore
            @@ Lwt.finalize
                 (fun () -> Lwt.return ())
                 (fun () -> raise Exception);
            Lwt.return false
          with Exception -> Lwt.return true);
      test "rejected" (fun () ->
          let f'_ran = ref false in
          let p =
            Lwt.finalize
              (fun () -> Lwt.fail Exception)
              (fun () ->
                f'_ran := true;
                Lwt.return ())
          in
          Lwt.bind (state_is (Lwt.Fail Exception) p) (fun correct ->
              Lwt.return (correct && !f'_ran = true)));
      test "rejected, f' rejected" (fun () ->
          let p =
            Lwt.finalize
              (fun () -> Lwt.fail Exit)
              (fun () -> Lwt.fail Exception)
          in
          state_is (Lwt.Fail Exception) p);
      (* An instance of the bind quirk. *)
      test "rejected, f' raises" (fun () ->
          try
            ignore
            @@ Lwt.finalize
                 (fun () -> Lwt.fail Exit)
                 (fun () -> raise Exception);
            Lwt.return false
          with Exception -> Lwt.return true);
      test "pending" (fun () ->
          let f'_ran = ref false in
          let p, _ = Lwt.wait () in
          let p =
            Lwt.finalize
              (fun () -> p)
              (fun () ->
                f'_ran := true;
                Lwt.return ())
          in
          Lwt.bind (state_is Lwt.Sleep p) (fun correct ->
              Lwt.return (correct && !f'_ran = false)));
      test "pending, fulfilled" (fun () ->
          let f'_ran = ref false in
          let p, r = Lwt.wait () in
          let p =
            Lwt.finalize
              (fun () -> p)
              (fun () ->
                f'_ran := true;
                Lwt.return ())
          in
          Lwt.wakeup r "foo";
          Lwt.bind (state_is (Lwt.Return "foo") p) (fun correct ->
              Lwt.return (correct && !f'_ran = true)));
      test "pending, fulfilled, f' rejected" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.finalize (fun () -> p) (fun () -> Lwt.fail Exception) in
          Lwt.wakeup r ();
          state_is (Lwt.Fail Exception) p);
      test "pending, fulfilled, f' raises" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.finalize (fun () -> p) (fun () -> raise Exception) in
          Lwt.wakeup r ();
          state_is (Lwt.Fail Exception) p);
      test "pending, fulfilled, f' pending" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p = Lwt.finalize (fun () -> p1) (fun () -> p2) in
          Lwt.wakeup r1 "foo";
          assert (Lwt.state p = Lwt.Sleep);
          Lwt.wakeup r2 ();
          state_is (Lwt.Return "foo") p);
      test "pending, fulfilled, f' pending, rejected" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p = Lwt.finalize (fun () -> p1) (fun () -> p2) in
          Lwt.wakeup r1 ();
          assert (Lwt.state p = Lwt.Sleep);
          Lwt.wakeup_exn r2 Exception;
          state_is (Lwt.Fail Exception) p);
      test "pending, rejected" (fun () ->
          let f'_ran = ref false in
          let p, r = Lwt.wait () in
          let p =
            Lwt.finalize
              (fun () -> p)
              (fun () ->
                f'_ran := true;
                Lwt.return ())
          in
          Lwt.wakeup_exn r Exception;
          Lwt.bind (state_is (Lwt.Fail Exception) p) (fun correct ->
              Lwt.return (correct && !f'_ran = true)));
      test "pending, rejected, f' rejected" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.finalize (fun () -> p) (fun () -> Lwt.fail Exception) in
          Lwt.wakeup_exn r Exit;
          state_is (Lwt.Fail Exception) p);
      test "pending, rejected, f' raises" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.finalize (fun () -> p) (fun () -> raise Exception) in
          Lwt.wakeup_exn r Exit;
          state_is (Lwt.Fail Exception) p);
      test "pending, rejected, f' pending" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p = Lwt.finalize (fun () -> p1) (fun () -> p2) in
          Lwt.wakeup_exn r1 Exception;
          assert (Lwt.state p = Lwt.Sleep);
          Lwt.wakeup r2 ();
          state_is (Lwt.Fail Exception) p);
      test "pending, rejected, f' pending, rejected" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p = Lwt.finalize (fun () -> p1) (fun () -> p2) in
          Lwt.wakeup_exn r1 Exit;
          assert (Lwt.state p = Lwt.Sleep);
          Lwt.wakeup_exn r2 Exception;
          state_is (Lwt.Fail Exception) p);
      (* See "proxy during callback" in [bind] tests. *)
      test "proxy during callback (fulfilled)" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p3 =
            Lwt.finalize
              (fun () -> p1)
              (fun () ->
                Lwt.wakeup r2 ();
                Lwt.return ())
          in
          let p4 = Lwt.bind p2 (fun () -> p3) in
          Lwt.wakeup r1 ();
          Lwt.bind p4 (fun () -> Lwt.return true));
      (* See "proxy during callback" in [bind] tests. *)
      test "proxy during callback (rejected)" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p3 =
            Lwt.finalize
              (fun () -> p1)
              (fun () ->
                Lwt.wakeup r2 ();
                Lwt.return ())
          in
          let p4 = Lwt.bind p2 (fun () -> p3) in
          Lwt.wakeup_exn r1 Exit;
          Lwt.catch (fun () -> p4) (fun _exn -> Lwt.return true));
    ]

let suites = suites @ [ finalize_tests ]

let backtrace_finalize_tests =
  suite "backtrace_finalize"
    [
      test "fulfilled" (fun () ->
          let f'_ran = ref false in
          let p =
            Lwt.backtrace_finalize add_loc
              (fun () -> Lwt.return "foo")
              (fun () ->
                f'_ran := true;
                Lwt.return ())
          in
          Lwt.bind (state_is (Lwt.Return "foo") p) (fun correct ->
              Lwt.return (correct && !f'_ran = true)));
      test "fulfilled, f' rejected" (fun () ->
          let p =
            Lwt.backtrace_finalize add_loc
              (fun () -> Lwt.return ())
              (fun () -> Lwt.fail Exception)
          in
          state_is (Lwt.Fail Exception) p);
      (* Instance of the bind quirk. *)
      test "fulfilled, f' raises" (fun () ->
          try
            ignore
            @@ Lwt.backtrace_finalize add_loc
                 (fun () -> Lwt.return ())
                 (fun () -> raise Exception);
            Lwt.return false
          with Exception -> Lwt.return true);
      test "rejected" (fun () ->
          let f'_ran = ref false in
          let p =
            Lwt.backtrace_finalize add_loc
              (fun () -> Lwt.fail Exception)
              (fun () ->
                f'_ran := true;
                Lwt.return ())
          in
          Lwt.bind (state_is (Lwt.Fail Exception) p) (fun correct ->
              Lwt.return (correct && !f'_ran = true)));
      test "rejected, f' rejected" (fun () ->
          let p =
            Lwt.backtrace_finalize add_loc
              (fun () -> Lwt.fail Exit)
              (fun () -> Lwt.fail Exception)
          in
          state_is (Lwt.Fail Exception) p);
      (* Instance of the bind quirk. *)
      test "rejected, f' raises" (fun () ->
          try
            ignore
            @@ Lwt.backtrace_finalize add_loc
                 (fun () -> Lwt.fail Exit)
                 (fun () -> raise Exception);
            Lwt.return false
          with Exception -> Lwt.return true);
      test "pending" (fun () ->
          let f'_ran = ref false in
          let p, _ = Lwt.wait () in
          let p =
            Lwt.backtrace_finalize add_loc
              (fun () -> p)
              (fun () ->
                f'_ran := true;
                Lwt.return ())
          in
          Lwt.bind (state_is Lwt.Sleep p) (fun correct ->
              Lwt.return (correct && !f'_ran = false)));
      test "pending, fulfilled" (fun () ->
          let f'_ran = ref false in
          let p, r = Lwt.wait () in
          let p =
            Lwt.backtrace_finalize add_loc
              (fun () -> p)
              (fun () ->
                f'_ran := true;
                Lwt.return ())
          in
          Lwt.wakeup r "foo";
          Lwt.bind (state_is (Lwt.Return "foo") p) (fun correct ->
              Lwt.return (correct && !f'_ran = true)));
      test "pending, fulfilled, f' rejected" (fun () ->
          let p, r = Lwt.wait () in
          let p =
            Lwt.backtrace_finalize add_loc
              (fun () -> p)
              (fun () -> Lwt.fail Exception)
          in
          Lwt.wakeup r ();
          state_is (Lwt.Fail Exception) p);
      test "pending, fulfilled, f' raises" (fun () ->
          let p, r = Lwt.wait () in
          let p =
            Lwt.backtrace_finalize add_loc
              (fun () -> p)
              (fun () -> raise Exception)
          in
          Lwt.wakeup r ();
          state_is (Lwt.Fail Exception) p);
      test "pending, rejected" (fun () ->
          let f'_ran = ref false in
          let p, r = Lwt.wait () in
          let p =
            Lwt.backtrace_finalize add_loc
              (fun () -> p)
              (fun () ->
                f'_ran := true;
                Lwt.return ())
          in
          Lwt.wakeup_exn r Exception;
          Lwt.bind (state_is (Lwt.Fail Exception) p) (fun correct ->
              Lwt.return (correct && !f'_ran = true)));
      test "pending, rejected, f' rejected" (fun () ->
          let p, r = Lwt.wait () in
          let p =
            Lwt.backtrace_finalize add_loc
              (fun () -> p)
              (fun () -> Lwt.fail Exception)
          in
          Lwt.wakeup_exn r Exit;
          state_is (Lwt.Fail Exception) p);
      test "pending, rejected, f' raises" (fun () ->
          let p, r = Lwt.wait () in
          let p =
            Lwt.backtrace_finalize add_loc
              (fun () -> p)
              (fun () -> raise Exception)
          in
          Lwt.wakeup_exn r Exit;
          state_is (Lwt.Fail Exception) p);
      (* See "proxy during callback" in [bind] tests. *)
      test "proxy during callback (fulfilled)" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p3 =
            Lwt.backtrace_finalize add_loc
              (fun () -> p1)
              (fun () ->
                Lwt.wakeup r2 ();
                Lwt.return ())
          in
          let p4 = Lwt.bind p2 (fun () -> p3) in
          Lwt.wakeup r1 ();
          Lwt.bind p4 (fun () -> Lwt.return true));
      (* See "proxy during callback" in [bind] tests. *)
      test "proxy during callback (rejected)" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p3 =
            Lwt.backtrace_finalize add_loc
              (fun () -> p1)
              (fun () ->
                Lwt.wakeup r2 ();
                Lwt.return ())
          in
          let p4 = Lwt.bind p2 (fun () -> p3) in
          Lwt.wakeup_exn r1 Exit;
          Lwt.catch (fun () -> p4) (fun _exn -> Lwt.return true));
    ]

let suites = suites @ [ backtrace_finalize_tests ]

let on_success_tests =
  suite "on_success"
    [
      test "fulfilled" (fun () ->
          let f_ran = ref false in
          Lwt.on_success (Lwt.return ()) (fun () -> f_ran := true);
          later (fun () -> !f_ran = true));
      test ~sequential:true "fulfilled, f raises" (fun () ->
          let saw = ref None in
          let restore = set_async_exception_hook (fun exn -> saw := Some exn) in
          Lwt.on_success (Lwt.return ()) (fun () -> raise Exception);
          later (fun () ->
              restore ();
              !saw = Some Exception));
      test "rejected" (fun () ->
          let f_ran = ref false in
          Lwt.on_success (Lwt.fail Exception) (fun () -> f_ran := true);
          later (fun () -> !f_ran = false));
      test "pending" (fun () ->
          let f_ran = ref false in
          Lwt.on_success (fst (Lwt.wait ())) (fun () -> f_ran := true);
          later (fun () -> !f_ran = false));
      test "pending, fulfilled" (fun () ->
          let f_ran = ref false in
          let p, r = Lwt.wait () in
          Lwt.on_success p (fun () -> f_ran := true);
          assert (!f_ran = false);
          Lwt.wakeup r ();
          later (fun () -> !f_ran = true));
      test ~sequential:true "pending, fulfilled, f raises" (fun () ->
          let saw = ref None in
          let p, r = Lwt.wait () in
          Lwt.on_success p (fun () -> raise Exception);
          let restore = set_async_exception_hook (fun exn -> saw := Some exn) in
          Lwt.wakeup r ();
          later (fun () ->
              restore ();
              !saw = Some Exception));
      test "pending, rejected" (fun () ->
          let f_ran = ref false in
          let p, r = Lwt.wait () in
          Lwt.on_success p (fun () -> f_ran := true);
          Lwt.wakeup_exn r Exception;
          later (fun () -> !f_ran = false));
    ]

let suites = suites @ [ on_success_tests ]

let on_failure_tests =
  suite "on_failure"
    [
      test "fulfilled" (fun () ->
          let f_ran = ref false in
          Lwt.on_failure (Lwt.return ()) (fun _ -> f_ran := true);
          later (fun () -> !f_ran = false));
      test "rejected" (fun () ->
          let saw = ref None in
          Lwt.on_failure (Lwt.fail Exception) (fun exn -> saw := Some exn);
          later (fun () -> !saw = Some Exception));
      test ~sequential:true "rejected, f raises" (fun () ->
          let saw = ref None in
          let restore = set_async_exception_hook (fun exn -> saw := Some exn) in
          Lwt.on_failure (Lwt.fail Exit) (fun _ -> raise Exception);
          later (fun () ->
              restore ();
              !saw = Some Exception));
      test "pending" (fun () ->
          let f_ran = ref false in
          Lwt.on_failure (fst (Lwt.wait ())) (fun _ -> f_ran := true);
          later (fun () -> !f_ran = false));
      test "pending, fulfilled" (fun () ->
          let f_ran = ref false in
          let p, r = Lwt.wait () in
          Lwt.on_failure p (fun _ -> f_ran := true);
          Lwt.wakeup r ();
          later (fun () -> !f_ran = false));
      test "pending, rejected" (fun () ->
          let saw = ref None in
          let p, r = Lwt.wait () in
          Lwt.on_failure p (fun exn -> saw := Some exn);
          Lwt.wakeup_exn r Exception;
          later (fun () -> !saw = Some Exception));
      test ~sequential:true "pending, rejected, f raises" (fun () ->
          let saw = ref None in
          let p, r = Lwt.wait () in
          Lwt.on_failure p (fun _ -> raise Exception);
          let restore = set_async_exception_hook (fun exn -> saw := Some exn) in
          Lwt.wakeup_exn r Exit;
          later (fun () ->
              restore ();
              !saw = Some Exception));
    ]

let suites = suites @ [ on_failure_tests ]

let on_termination_tests =
  suite "on_termination"
    [
      test "fulfilled" (fun () ->
          let f_ran = ref false in
          Lwt.on_termination (Lwt.return ()) (fun () -> f_ran := true);
          later (fun () -> !f_ran = true));
      test ~sequential:true "fulfilled, f raises" (fun () ->
          let saw = ref None in
          let restore = set_async_exception_hook (fun exn -> saw := Some exn) in
          Lwt.on_termination (Lwt.return ()) (fun () -> raise Exception);
          later (fun () ->
              restore ();
              !saw = Some Exception));
      test "rejected" (fun () ->
          let f_ran = ref false in
          Lwt.on_termination (Lwt.fail Exception) (fun () -> f_ran := true);
          later (fun () -> !f_ran = true));
      test ~sequential:true "rejected, f raises" (fun () ->
          let saw = ref None in
          let restore = set_async_exception_hook (fun exn -> saw := Some exn) in
          Lwt.on_termination (Lwt.fail Exit) (fun () -> raise Exception);
          later (fun () ->
              restore ();
              !saw = Some Exception));
      test "pending" (fun () ->
          let f_ran = ref false in
          Lwt.on_termination (fst (Lwt.wait ())) (fun () -> f_ran := true);
          later (fun () -> !f_ran = false));
      test "pending, fulfilled" (fun () ->
          let f_ran = ref false in
          let p, r = Lwt.wait () in
          Lwt.on_termination p (fun () -> f_ran := true);
          Lwt.wakeup r ();
          later (fun () -> !f_ran = true));
      test ~sequential:true "pending, fulfilled, f raises" (fun () ->
          let saw = ref None in
          let p, r = Lwt.wait () in
          Lwt.on_termination p (fun () -> raise Exception);
          let restore = set_async_exception_hook (fun exn -> saw := Some exn) in
          Lwt.wakeup r ();
          later (fun () ->
              restore ();
              !saw = Some Exception));
      test "pending, rejected" (fun () ->
          let f_ran = ref false in
          let p, r = Lwt.wait () in
          Lwt.on_termination p (fun () -> f_ran := true);
          Lwt.wakeup_exn r Exception;
          later (fun () -> !f_ran = true));
      test ~sequential:true "pending, rejected, f raises" (fun () ->
          let saw = ref None in
          let p, r = Lwt.wait () in
          Lwt.on_termination p (fun () -> raise Exception);
          let restore = set_async_exception_hook (fun exn -> saw := Some exn) in
          Lwt.wakeup_exn r Exit;
          later (fun () ->
              restore ();
              !saw = Some Exception));
    ]

let suites = suites @ [ on_termination_tests ]

let on_any_tests =
  suite "on_any"
    [
      test "fulfilled" (fun () ->
          let f_ran = ref false in
          let g_ran = ref false in
          Lwt.on_any (Lwt.return ())
            (fun () -> f_ran := true)
            (fun _ -> g_ran := true);
          later (fun () -> !f_ran = true && !g_ran = false));
      test ~sequential:true "fulfilled, f raises" (fun () ->
          let saw = ref None in
          let restore = set_async_exception_hook (fun exn -> saw := Some exn) in
          Lwt.on_any (Lwt.return ()) (fun () -> raise Exception) ignore;
          later (fun () ->
              restore ();
              !saw = Some Exception));
      test "rejected" (fun () ->
          let saw = ref None in
          (* f can't run due to parametricity. *)
          Lwt.on_any (Lwt.fail Exception) ignore (fun exn -> saw := Some exn);
          later (fun () -> !saw = Some Exception));
      test ~sequential:true "rejected, f raises" (fun () ->
          let saw = ref None in
          let restore = set_async_exception_hook (fun exn -> saw := Some exn) in
          Lwt.on_any (Lwt.fail Exit) ignore (fun _ -> raise Exception);
          later (fun () ->
              restore ();
              !saw = Some Exception));
      test "pending" (fun () ->
          let g_ran = ref false in
          (* f can't run due to parametricity. *)
          Lwt.on_any (fst (Lwt.wait ())) ignore (fun _ -> g_ran := true);
          later (fun () -> !g_ran = false));
      test "pending, fulfilled" (fun () ->
          let f_ran = ref false in
          let g_ran = ref false in
          let p, r = Lwt.wait () in
          Lwt.on_any p (fun () -> f_ran := true) (fun _ -> g_ran := true);
          Lwt.wakeup r ();
          later (fun () -> !f_ran = true && !g_ran = false));
      test ~sequential:true "pending, fulfilled, f raises" (fun () ->
          let saw = ref None in
          let p, r = Lwt.wait () in
          Lwt.on_any p (fun () -> raise Exception) ignore;
          let restore = set_async_exception_hook (fun exn -> saw := Some exn) in
          Lwt.wakeup r ();
          later (fun () ->
              restore ();
              !saw = Some Exception));
      test "pending, rejected" (fun () ->
          let saw = ref None in
          (* f can't run due to parametricity. *)
          let p, r = Lwt.wait () in
          Lwt.on_any p ignore (fun exn -> saw := Some exn);
          Lwt.wakeup_exn r Exception;
          later (fun () -> !saw = Some Exception));
      test ~sequential:true "pending, rejected, g raises" (fun () ->
          let saw = ref None in
          let p, r = Lwt.wait () in
          Lwt.on_any p ignore (fun _ -> raise Exception);
          let restore = set_async_exception_hook (fun exn -> saw := Some exn) in
          Lwt.wakeup_exn r Exit;
          later (fun () ->
              restore ();
              !saw = Some Exception));
    ]

let suites = suites @ [ on_any_tests ]

(* Concurrent composition tests, not including cancellation and
   sequence-associated storage. Also not including [Lwt.pick] and [Lwt.npick],
   as those interact with cancellation. *)

let async_tests =
  suite "async"
    [
      test "fulfilled" (fun () ->
          let f_ran = ref false in
          Lwt.async (fun () ->
              f_ran := true;
              Lwt.return ());
          later (fun () -> !f_ran = true));
      test ~sequential:true "f raises" (fun () ->
          let saw = ref None in
          let restore = set_async_exception_hook (fun exn -> saw := Some exn) in
          Lwt.async (fun () -> raise Exception);
          later (fun () ->
              restore ();
              !saw = Some Exception));
      test ~sequential:true "rejected" (fun () ->
          let saw = ref None in
          let restore = set_async_exception_hook (fun exn -> saw := Some exn) in
          Lwt.async (fun () -> Lwt.fail Exception);
          later (fun () ->
              restore ();
              !saw = Some Exception));
      test "pending, fulfilled" (fun () ->
          let resolved = ref false in
          let p, r = Lwt.wait () in
          Lwt.async (fun () ->
              Lwt.bind p (fun () ->
                  resolved := true;
                  Lwt.return ()));
          Lwt.wakeup r ();
          later (fun () -> !resolved = true));
      test ~sequential:true "pending, rejected" (fun () ->
          let saw = ref None in
          let p, r = Lwt.wait () in
          Lwt.async (fun () -> p);
          let restore = set_async_exception_hook (fun exn -> saw := Some exn) in
          Lwt.wakeup_exn r Exception;
          later (fun () ->
              restore ();
              !saw = Some Exception));
    ]

let suites = suites @ [ async_tests ]

let dont_wait_tests =
  suite "dont_wait"
    [
      test "fulfilled" (fun () ->
          let f_ran = ref false in
          Lwt.dont_wait
            (fun () ->
              f_ran := true;
              Lwt.return ())
            (fun _ -> ());
          later (fun () -> !f_ran = true));
      test "f raises" (fun () ->
          let saw = ref None in
          Lwt.dont_wait (fun () -> raise Exception) (fun exn -> saw := Some exn);
          later (fun () -> !saw = Some Exception));
      test "rejected" (fun () ->
          let saw = ref None in
          Lwt.dont_wait
            (fun () -> Lwt.fail Exception)
            (fun exn -> saw := Some exn);
          later (fun () -> !saw = Some Exception));
      test "pending, fulfilled" (fun () ->
          let resolved = ref false in
          let p, r = Lwt.wait () in
          Lwt.dont_wait
            (fun () ->
              Lwt.bind p (fun () ->
                  resolved := true;
                  Lwt.return ()))
            (fun _ -> ());
          Lwt.wakeup r ();
          later (fun () -> !resolved = true));
      test "pending, rejected" (fun () ->
          let saw = ref None in
          let p, r = Lwt.wait () in
          Lwt.dont_wait (fun () -> p) (fun exn -> saw := Some exn);
          Lwt.wakeup_exn r Exception;
          later (fun () -> !saw = Some Exception));
    ]

let suites = suites @ [ dont_wait_tests ]

let ignore_result_tests =
  suite "ignore_result"
    [
      test "fulfilled" (fun () ->
          Lwt.ignore_result (Lwt.return ());
          (* Reaching this without an exception is success. *)
          Lwt.return true);
      test "rejected" (fun () ->
          try
            Lwt.ignore_result (Lwt.fail Exception);
            Lwt.return false
          with Exception -> Lwt.return true);
      test "pending, fulfilled" (fun () ->
          let p, r = Lwt.wait () in
          Lwt.ignore_result p;
          Lwt.wakeup r ();
          (* Reaching this without process termination is success. *)
          Lwt.return true);
      test ~sequential:true "pending, rejected" (fun () ->
          let saw = ref None in
          let p, r = Lwt.wait () in
          Lwt.ignore_result p;
          let restore = set_async_exception_hook (fun exn -> saw := Some exn) in
          Lwt.wakeup_exn r Exception;
          restore ();
          Lwt.return (!saw = Some Exception));
    ]

let suites = suites @ [ ignore_result_tests ]

let join_tests =
  suite "join"
    [
      test "empty" (fun () ->
          let p = Lwt.join [] in
          state_is (Lwt.Return ()) p);
      test "all fulfilled" (fun () ->
          let p = Lwt.join [ Lwt.return (); Lwt.return () ] in
          state_is (Lwt.Return ()) p);
      test "all rejected" (fun () ->
          let p = Lwt.join [ Lwt.fail Exception; Lwt.fail Exception ] in
          state_is (Lwt.Fail Exception) p);
      test "fulfilled and pending, fulfilled" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.join [ Lwt.return (); p ] in
          Lwt.wakeup r ();
          state_is (Lwt.Return ()) p);
      test "rejected and pending, fulfilled" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.join [ Lwt.fail Exception; p ] in
          Lwt.wakeup r ();
          state_is (Lwt.Fail Exception) p);
      test "fulfilled and pending, rejected" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.join [ Lwt.return (); p ] in
          Lwt.wakeup_exn r Exception;
          state_is (Lwt.Fail Exception) p);
      test "rejected and pending, rejected" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.join [ Lwt.fail Exception; p ] in
          Lwt.wakeup_exn r Exit;
          state_is (Lwt.Fail Exception) p);
      test "diamond" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.join [ p; p ] in
          Lwt.wakeup r ();
          state_is (Lwt.Return ()) p);
    ]

let suites = suites @ [ join_tests ]
let list_init i f = Array.init i f |> Array.to_list

let all_tests =
  suite "all"
    [
      test "empty" (fun () ->
          let p = Lwt.all [] in
          state_is (Lwt.Return []) p);
      test "all fulfilled (one)" (fun () ->
          let p = Lwt.all [ Lwt.return 1 ] in
          state_is (Lwt.Return [ 1 ]) p);
      test "all fulfilled (two)" (fun () ->
          let p = Lwt.all [ Lwt.return 1; Lwt.return 2 ] in
          state_is (Lwt.Return [ 1; 2 ]) p);
      test "all fulfilled (three)" (fun () ->
          let p = Lwt.all [ Lwt.return 1; Lwt.return 2; Lwt.return 3 ] in
          state_is (Lwt.Return [ 1; 2; 3 ]) p);
      test "all fulfilled (long)" (fun () ->
          let p = Lwt.all (list_init 10 Lwt.return) in
          state_is (Lwt.Return (list_init 10 (fun i -> i))) p);
      test "all rejected" (fun () ->
          let p = Lwt.all [ Lwt.fail Exception; Lwt.fail Exception ] in
          state_is (Lwt.Fail Exception) p);
      test "fulfilled and pending, fulfilled" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.all [ Lwt.return 1; p ] in
          Lwt.wakeup r 2;
          state_is (Lwt.Return [ 1; 2 ]) p);
      test "pending twice physically equal, fulfilled" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.all [ p; p ] in
          Lwt.wakeup r 2;
          state_is (Lwt.Return [ 2; 2 ]) p);
      test "pending twice physically equal twice, fulfilled" (fun () ->
          let p, r = Lwt.wait () in
          let q, s = Lwt.wait () in
          let p = Lwt.all [ p; p; q; q ] in
          Lwt.wakeup r 2;
          Lwt.wakeup s 4;
          state_is (Lwt.Return [ 2; 2; 4; 4 ]) p);
      test "fulfilled and pending and fulfilled, fulfilled" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.all [ Lwt.return 1; p; Lwt.return 3 ] in
          Lwt.wakeup r 2;
          state_is (Lwt.Return [ 1; 2; 3 ]) p);
      test "fulfilled and pending, fulfilled (long)" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.all (list_init 10 Lwt.return @ [ p ]) in
          Lwt.wakeup r 10;
          state_is (Lwt.Return (list_init 11 (fun x -> x))) p);
      test "rejected and pending, fulfilled" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.all [ Lwt.fail Exception; p ] in
          Lwt.wakeup r 2;
          state_is (Lwt.Fail Exception) p);
      test "fulfilled and pending, rejected" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.all [ Lwt.return 1; p ] in
          Lwt.wakeup_exn r Exception;
          state_is (Lwt.Fail Exception) p);
      test "rejected and pending, rejected" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.all [ Lwt.fail Exception; p ] in
          Lwt.wakeup_exn r Exit;
          state_is (Lwt.Fail Exception) p);
      test "diamond" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.all [ p; p ] in
          Lwt.wakeup r 1;
          state_is (Lwt.Return [ 1; 1 ]) p);
    ]

let suites = suites @ [ all_tests ]

let both_tests =
  suite "both"
    [
      test "both fulfilled" (fun () ->
          let p = Lwt.both (Lwt.return 1) (Lwt.return 2) in
          state_is (Lwt.Return (1, 2)) p);
      test "both rejected" (fun () ->
          let p = Lwt.both (Lwt.fail Exception) (Lwt.fail Exit) in
          state_is (Lwt.Fail Exception) p);
      test "rejected, fulfilled" (fun () ->
          let p = Lwt.both (Lwt.fail Exception) (Lwt.return 2) in
          state_is (Lwt.Fail Exception) p);
      test "fulfilled, rejected" (fun () ->
          let p = Lwt.both (Lwt.return 1) (Lwt.fail Exception) in
          state_is (Lwt.Fail Exception) p);
      test "both pending" (fun () ->
          let p = Lwt.both (fst (Lwt.wait ())) (fst (Lwt.wait ())) in
          state_is Lwt.Sleep p);
      test "pending, fulfilled" (fun () ->
          let p = Lwt.both (fst (Lwt.wait ())) (Lwt.return 2) in
          state_is Lwt.Sleep p);
      test "pending, rejected" (fun () ->
          let p = Lwt.both (fst (Lwt.wait ())) (Lwt.fail Exception) in
          state_is Lwt.Sleep p);
      test "fulfilled, pending" (fun () ->
          let p = Lwt.both (Lwt.return 1) (fst (Lwt.wait ())) in
          state_is Lwt.Sleep p);
      test "rejected, pending" (fun () ->
          let p = Lwt.both (Lwt.fail Exception) (fst (Lwt.wait ())) in
          state_is Lwt.Sleep p);
      test "pending, fulfilled, then fulfilled" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p = Lwt.both p1 (Lwt.return 2) in
          Lwt.wakeup_later r1 1;
          state_is (Lwt.Return (1, 2)) p);
      test "pending, rejected, then fulfilled" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p = Lwt.both p1 (Lwt.fail Exception) in
          Lwt.wakeup_later r1 1;
          state_is (Lwt.Fail Exception) p);
      test "pending, fulfilled, then rejected" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p = Lwt.both p1 (Lwt.return 2) in
          Lwt.wakeup_later_exn r1 Exception;
          state_is (Lwt.Fail Exception) p);
      test "pending, rejected, then rejected" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p = Lwt.both p1 (Lwt.fail Exception) in
          Lwt.wakeup_later_exn r1 Exit;
          state_is (Lwt.Fail Exception) p);
      test "fulfilled, pending, then fulfilled" (fun () ->
          let p2, r2 = Lwt.wait () in
          let p = Lwt.both (Lwt.return 1) p2 in
          Lwt.wakeup_later r2 2;
          state_is (Lwt.Return (1, 2)) p);
      test "rejected, pending, then fulfilled" (fun () ->
          let p2, r2 = Lwt.wait () in
          let p = Lwt.both (Lwt.fail Exception) p2 in
          Lwt.wakeup_later r2 2;
          state_is (Lwt.Fail Exception) p);
      test "fulfilled, pending, then rejected" (fun () ->
          let p2, r2 = Lwt.wait () in
          let p = Lwt.both (Lwt.return 1) p2 in
          Lwt.wakeup_later_exn r2 Exception;
          state_is (Lwt.Fail Exception) p);
      test "rejected, pending, then rejected" (fun () ->
          let p2, r2 = Lwt.wait () in
          let p = Lwt.both (Lwt.fail Exception) p2 in
          Lwt.wakeup_later_exn r2 Exit;
          state_is (Lwt.Fail Exception) p);
      test "pending, then first fulfilled" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p = Lwt.both p1 (fst (Lwt.wait ())) in
          Lwt.wakeup_later r1 1;
          state_is Lwt.Sleep p);
      test "pending, then first rejected" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p = Lwt.both p1 (fst (Lwt.wait ())) in
          Lwt.wakeup_later_exn r1 Exception;
          state_is Lwt.Sleep p);
      test "pending, then second fulfilled" (fun () ->
          let p2, r2 = Lwt.wait () in
          let p = Lwt.both (fst (Lwt.wait ())) p2 in
          Lwt.wakeup_later r2 2;
          state_is Lwt.Sleep p);
      test "pending, then second rejected" (fun () ->
          let p2, r2 = Lwt.wait () in
          let p = Lwt.both (fst (Lwt.wait ())) p2 in
          Lwt.wakeup_later_exn r2 Exception;
          state_is Lwt.Sleep p);
      test "pending, then first fulfilled, then fulfilled" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p = Lwt.both p1 p2 in
          Lwt.wakeup_later r1 1;
          Lwt.wakeup_later r2 2;
          state_is (Lwt.Return (1, 2)) p);
      test "pending, then first fulfilled, then rejected" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p = Lwt.both p1 p2 in
          Lwt.wakeup_later r1 1;
          Lwt.wakeup_later_exn r2 Exception;
          state_is (Lwt.Fail Exception) p);
      test "pending, then first rejected, then fulfilled" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p = Lwt.both p1 p2 in
          Lwt.wakeup_later_exn r1 Exception;
          Lwt.wakeup_later r2 2;
          state_is (Lwt.Fail Exception) p);
      test "pending, then first rejected, then rejected" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p = Lwt.both p1 p2 in
          Lwt.wakeup_later_exn r1 Exception;
          Lwt.wakeup_later_exn r2 Exit;
          state_is (Lwt.Fail Exception) p);
      test "pending, then second fulfilled, then fulfilled" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p = Lwt.both p1 p2 in
          Lwt.wakeup_later r2 2;
          Lwt.wakeup_later r1 1;
          state_is (Lwt.Return (1, 2)) p);
      test "pending, then second fulfilled, then rejected" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p = Lwt.both p1 p2 in
          Lwt.wakeup_later r2 2;
          Lwt.wakeup_later_exn r1 Exception;
          state_is (Lwt.Fail Exception) p);
      test "pending, then second rejected, then fulfilled" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p = Lwt.both p1 p2 in
          Lwt.wakeup_later_exn r2 Exception;
          Lwt.wakeup_later r1 1;
          state_is (Lwt.Fail Exception) p);
      test "pending, then second rejected, then rejected" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p = Lwt.both p1 p2 in
          Lwt.wakeup_later_exn r2 Exception;
          Lwt.wakeup_later_exn r1 Exit;
          state_is (Lwt.Fail Exception) p);
      test "diamond" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p = Lwt.both p1 p1 in
          Lwt.bind (state_is Lwt.Sleep p) (fun was_pending ->
              Lwt.wakeup_later r1 1;
              Lwt.bind
                (state_is (Lwt.Return (1, 1)) p)
                (fun is_fulfilled -> Lwt.return (was_pending && is_fulfilled))));
    ]

let suites = suites @ [ both_tests ]

let choose_tests =
  suite "choose"
    [
      (test "empty" (fun () ->
           try
             ignore (Lwt.choose []);
             Lwt.return false
           with
           | Invalid_argument
               "Lwt.choose [] would return a promise that is pending forever"
           ->
             Lwt.return true) [@ocaml.warning "-52"]);
      test "fulfilled" (fun () ->
          let p = Lwt.choose [ fst (Lwt.wait ()); Lwt.return "foo" ] in
          state_is (Lwt.Return "foo") p);
      test "rejected" (fun () ->
          let p = Lwt.choose [ fst (Lwt.wait ()); Lwt.fail Exception ] in
          state_is (Lwt.Fail Exception) p);
      test "multiple resolved" (fun () ->
          (* This is run in a loop to exercise the internal PRNG. *)
          let outcomes = Array.make 3 0 in
          let rec repeat n =
            if n <= 0 then ()
            else
              let p =
                Lwt.choose
                  [
                    fst (Lwt.wait ());
                    Lwt.return "foo";
                    Lwt.fail Exception;
                    Lwt.return "bar";
                  ]
              in
              (match[@ocaml.warning "-4"] Lwt.state p with
              | Lwt.Return "foo" -> outcomes.(0) <- outcomes.(0) + 1
              | Lwt.Fail Exception -> outcomes.(1) <- outcomes.(1) + 1
              | Lwt.Return "bar" -> outcomes.(2) <- outcomes.(2) + 1
              | _ -> assert false);
              repeat (n - 1)
          in
          let count = 1000 in
          repeat count;
          Lwt.return
            (outcomes.(0) > 0
            && outcomes.(1) > 0
            && outcomes.(2) > 0
            && outcomes.(0) + outcomes.(1) + outcomes.(2) = count));
      test "pending" (fun () ->
          let p = Lwt.choose [ fst (Lwt.wait ()); fst (Lwt.wait ()) ] in
          state_is Lwt.Sleep p);
      test "pending, fulfilled" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p = Lwt.choose [ p1; p2 ] in
          Lwt.wakeup r1 "foo";
          Lwt.wakeup r2 "bar";
          state_is (Lwt.Return "foo") p);
      test "diamond" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.choose [ p; p ] in
          Lwt.wakeup r "foo";
          state_is (Lwt.Return "foo") p);
    ]

let suites = suites @ [ choose_tests ]

let nchoose_tests =
  suite "nchoose"
    [
      (test "empty" (fun () ->
           try
             ignore (Lwt.nchoose []);
             Lwt.return false
           with
           | Invalid_argument
               "Lwt.nchoose [] would return a promise that is pending forever"
           ->
             Lwt.return true) [@ocaml.warning "-52"]);
      test "all fulfilled" (fun () ->
          let p = Lwt.nchoose [ Lwt.return "foo"; Lwt.return "bar" ] in
          Lwt.return (Lwt.state p = Lwt.Return [ "foo"; "bar" ]));
      test "fulfilled, rejected" (fun () ->
          let p = Lwt.nchoose [ Lwt.return "foo"; Lwt.fail Exception ] in
          Lwt.return (Lwt.state p = Lwt.Fail Exception));
      test "rejected, fulfilled" (fun () ->
          let p = Lwt.nchoose [ Lwt.fail Exception; Lwt.return "foo" ] in
          Lwt.return (Lwt.state p = Lwt.Fail Exception));
      test "some pending" (fun () ->
          let p =
            Lwt.nchoose
              [ Lwt.return "foo"; fst (Lwt.wait ()); Lwt.return "bar" ]
          in
          Lwt.return (Lwt.state p = Lwt.Return [ "foo"; "bar" ]));
      test "pending, fulfilled" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.nchoose [ fst (Lwt.wait ()); p ] in
          assert (Lwt.state p = Lwt.Sleep);
          Lwt.wakeup r "foo";
          Lwt.return (Lwt.state p = Lwt.Return [ "foo" ]));
      test "pending, rejected" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.nchoose [ fst (Lwt.wait ()); p ] in
          Lwt.wakeup_exn r Exception;
          Lwt.return (Lwt.state p = Lwt.Fail Exception));
      test "diamond" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.nchoose [ p; p ] in
          Lwt.wakeup r "foo";
          Lwt.return (Lwt.state p = Lwt.Return [ "foo"; "foo" ]));
      test "diamond, rejected" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.nchoose [ p; p ] in
          Lwt.wakeup_exn r Exception;
          Lwt.return (Lwt.state p = Lwt.Fail Exception));
    ]

let suites = suites @ [ nchoose_tests ]

let nchoose_split_tests =
  suite "nchoose_split"
    [
      (test "empty" (fun () ->
           try
             ignore (Lwt.nchoose_split []);
             Lwt.return false
           with
           | Invalid_argument
               "Lwt.nchoose_split [] would return a promise that is pending \
                forever"
           ->
             Lwt.return true) [@ocaml.warning "-52"]);
      test "some fulfilled" (fun () ->
          let p =
            Lwt.nchoose_split
              [ Lwt.return "foo"; fst (Lwt.wait ()); Lwt.return "bar" ]
          in
          match[@ocaml.warning "-4"] Lwt.state p with
          | Lwt.Return ([ "foo"; "bar" ], [ _ ]) -> Lwt.return true
          | _ -> Lwt.return false);
      test "fulfilled, rejected" (fun () ->
          let p = Lwt.nchoose_split [ Lwt.return (); Lwt.fail Exception ] in
          Lwt.return (Lwt.state p = Lwt.Fail Exception));
      test "rejected, fulfilled" (fun () ->
          let p = Lwt.nchoose_split [ Lwt.fail Exception; Lwt.return () ] in
          Lwt.return (Lwt.state p = Lwt.Fail Exception));
      test "pending, rejected" (fun () ->
          let p = Lwt.nchoose_split [ fst (Lwt.wait ()); Lwt.fail Exception ] in
          Lwt.return (Lwt.state p = Lwt.Fail Exception));
      test "pending, fulfilled" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.nchoose_split [ p; fst (Lwt.wait ()) ] in
          assert (Lwt.state p = Lwt.Sleep);
          Lwt.wakeup r "foo";
          match[@ocaml.warning "-4"] Lwt.state p with
          | Lwt.Return ([ "foo" ], [ _ ]) -> Lwt.return true
          | _ -> Lwt.return false);
      test "pending, rejected 2" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.nchoose_split [ p; fst (Lwt.wait ()) ] in
          Lwt.wakeup_exn r Exception;
          Lwt.return (Lwt.state p = Lwt.Fail Exception));
      test "diamond" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.nchoose_split [ p; p; fst (Lwt.wait ()) ] in
          Lwt.wakeup r ();
          match[@ocaml.warning "-4"] Lwt.state p with
          | Lwt.Return ([ (); () ], [ _ ]) -> Lwt.return true
          | _ -> Lwt.return false);
      test "diamond, rejected" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.nchoose_split [ p; p; fst (Lwt.wait ()) ] in
          Lwt.wakeup_exn r Exception;
          Lwt.return (Lwt.state p = Lwt.Fail Exception));
    ]

let suites = suites @ [ nchoose_split_tests ]

(* Tests functions related to [Lwt.state]; [Lwt.state] itself is tested in the
   preceding sections. *)

let state_query_tests =
  suite "state query"
    [
      test "is_sleeping: fulfilled" (fun () ->
          Lwt.return (not @@ Lwt.is_sleeping (Lwt.return ())));
      test "is_sleeping: rejected" (fun () ->
          Lwt.return (not @@ Lwt.is_sleeping (Lwt.fail Exception)));
      test "is_sleeping: pending" (fun () ->
          Lwt.return (Lwt.is_sleeping (fst (Lwt.wait ()))));
      (* This tests an implementation detail. *)
      test "is_sleeping: proxy" (fun () ->
          let p1, r = Lwt.wait () in
          let p2, _ = Lwt.wait () in
          Lwt.bind p1 (fun () -> p2) |> ignore;
          Lwt.wakeup r ();
          Lwt.return (Lwt.is_sleeping p2));
      (* This tests an internal API. *)
      test "poll: fulfilled" (fun () ->
          Lwt.return (Lwt.poll (Lwt.return "foo") = Some "foo"));
      test "poll: rejected" (fun () ->
          try
            Lwt.poll (Lwt.fail Exception) |> ignore;
            Lwt.return false
          with Exception -> Lwt.return true);
      test "poll: pending" (fun () ->
          Lwt.return (Lwt.poll (fst (Lwt.wait ())) = None));
      (* This tests an internal API on an implementation detail... *)
      test "poll: proxy" (fun () ->
          let p1, r = Lwt.wait () in
          let p2, _ = Lwt.wait () in
          Lwt.bind p1 (fun () -> p2) |> ignore;
          Lwt.wakeup r ();
          Lwt.return (Lwt.poll p2 = None));
    ]

let suites = suites @ [ state_query_tests ]

(* Preceding tests exercised most of [Lwt.wakeup], but here are more checks. *)
let wakeup_tests =
  suite "wakeup"
    [
      test "wakeup_result: nested" (fun () ->
          let f_ran = ref false in
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          Lwt.on_success p2 (fun _ -> f_ran := true);
          Lwt.on_success p1 (fun s ->
              Lwt.wakeup_result r2 (Result.Ok (s ^ "bar"));
              assert (Lwt.state p2 = Lwt.Return "foobar");
              assert (!f_ran = true));
          Lwt.wakeup_result r1 (Result.Ok "foo");
          Lwt.return (!f_ran = true && Lwt.state p2 = Lwt.Return "foobar"));
    ]

let suites = suites @ [ wakeup_tests ]

let wakeup_later_tests =
  suite "wakeup_later"
    [
      test "wakeup_later_result: immediate" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.bind p (fun s -> Lwt.return (s ^ "bar")) in
          Lwt.wakeup_later_result r (Result.Ok "foo");
          state_is (Lwt.Return "foobar") p);
      (test "wakeup_later: double use on wait" (fun () ->
           let _, r = Lwt.wait () in
           Lwt.wakeup r ();
           try
             Lwt.wakeup_later r ();
             Lwt.return false
           with Invalid_argument "Lwt.wakeup_later" -> Lwt.return true)
       [@ocaml.warning "-52"]);
      (test "wakeup_later: double use on task" (fun () ->
           let _, r = Lwt.task () in
           Lwt.wakeup r ();
           try
             Lwt.wakeup_later r ();
             Lwt.return false
           with Invalid_argument "Lwt.wakeup_later" -> Lwt.return true)
       [@ocaml.warning "-52"]);
      (test "wakeup_later_result: double use on wait" (fun () ->
           let _, r = Lwt.wait () in
           Lwt.wakeup r ();
           try
             Lwt.wakeup_later_result r (Result.Ok ());
             Lwt.return false
           with Invalid_argument "Lwt.wakeup_later_result" -> Lwt.return true)
       [@ocaml.warning "-52"]);
      (test "wakeup_later_result: double use on task" (fun () ->
           let _, r = Lwt.task () in
           Lwt.wakeup r ();
           try
             Lwt.wakeup_later_result r (Result.Ok ());
             Lwt.return false
           with Invalid_argument "Lwt.wakeup_later_result" -> Lwt.return true)
       [@ocaml.warning "-52"]);
      (test "wakeup_later_exn: double use on wait" (fun () ->
           let _, r = Lwt.wait () in
           Lwt.wakeup r ();
           try
             Lwt.wakeup_later_exn r Exception;
             Lwt.return false
           with Invalid_argument "Lwt.wakeup_later_exn" -> Lwt.return true)
       [@ocaml.warning "-52"]);
      (test "wakeup_later_exn: double use on task" (fun () ->
           let _, r = Lwt.task () in
           Lwt.wakeup r ();
           try
             Lwt.wakeup_later_exn r Exception;
             Lwt.return false
           with Invalid_argument "Lwt.wakeup_later_exn" -> Lwt.return true)
       [@ocaml.warning "-52"]);
      test "wakeup_later_result: nested" (fun () ->
          let f_ran = ref false in
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          Lwt.on_success p2 (fun _ -> f_ran := true);
          Lwt.on_success p1 (fun s ->
              Lwt.wakeup_later_result r2 (Result.Ok (s ^ "bar"));
              assert (Lwt.state p2 = Lwt.Return "foobar");
              assert (!f_ran = false));
          Lwt.wakeup_later_result r1 (Result.Ok "foo");
          Lwt.return (!f_ran = true && Lwt.state p2 = Lwt.Return "foobar"));
      (* Only basic tests for wakeup_later and wakeup_later_exn, as they are
         implemented in terms of wakeup_later_result. This isn't fully legitimate as
         a reason, but oh well. *)
      test "wakeup_later: basic" (fun () ->
          let p, r = Lwt.wait () in
          Lwt.wakeup_later r "foo";
          state_is (Lwt.Return "foo") p);
      test "wakeup_later_exn: basic" (fun () ->
          let p, r = Lwt.wait () in
          Lwt.wakeup_later_exn r Exception;
          state_is (Lwt.Fail Exception) p);
    ]

let suites = suites @ [ wakeup_later_tests ]

(* Cancellation and its interaction with the rest of the API. *)

let cancel_tests =
  suite "cancel"
    [
      test "fulfilled" (fun () ->
          let p = Lwt.return () in
          Lwt.cancel p;
          Lwt.return (Lwt.state p = Lwt.Return ()));
      test "rejected" (fun () ->
          let p = Lwt.fail Exception in
          Lwt.cancel p;
          Lwt.return (Lwt.state p = Lwt.Fail Exception));
      test "wait" (fun () ->
          let p, _ = Lwt.wait () in
          Lwt.cancel p;
          Lwt.return (Lwt.state p = Lwt.Sleep));
      test "task" (fun () ->
          let p, _ = Lwt.task () in
          Lwt.cancel p;
          Lwt.return (Lwt.state p = Lwt.Fail Lwt.Canceled));
      test "callback" (fun () ->
          let saw = ref None in
          let p, _ = Lwt.task () in
          Lwt.on_failure p (fun exn -> saw := Some exn);
          Lwt.cancel p;
          Lwt.return (!saw = Some Lwt.Canceled));
      (* Behaves like wakeup rather than wakeup_later, even though that's probably
         wrong. Calling cancel in a (functional) loop will cause stack overflow. *)
      test "nested" (fun () ->
          let f_ran = ref false in
          let p1, _ = Lwt.task () in
          let p2, _ = Lwt.task () in
          Lwt.on_failure p2 (fun _ -> f_ran := true);
          Lwt.on_failure p1 (fun _ ->
              Lwt.cancel p2;
              assert (Lwt.state p2 = Lwt.Fail Lwt.Canceled);
              assert (!f_ran = true));
          Lwt.cancel p1;
          Lwt.return (!f_ran = true && Lwt.state p2 = Lwt.Fail Lwt.Canceled));
    ]

let suites = suites @ [ cancel_tests ]

let on_cancel_tests =
  suite "on_cancel"
    [
      test "pending" (fun () ->
          let f_ran = ref false in
          let p, _ = Lwt.task () in
          Lwt.on_cancel p (fun () -> f_ran := true);
          assert (!f_ran = false);
          Lwt.cancel p;
          Lwt.return (!f_ran = true));
      test "multiple" (fun () ->
          let f_ran = ref false in
          let g_ran = ref false in
          let h_ran = ref false in
          let p, _ = Lwt.task () in
          Lwt.on_cancel p (fun () -> f_ran := true);
          Lwt.on_cancel p (fun () -> g_ran := true);
          Lwt.on_cancel p (fun () -> h_ran := true);
          Lwt.cancel p;
          Lwt.return (!f_ran = true && !g_ran = true && !h_ran = true));
      test "ordering" (fun () ->
          (* Two cancel callbacks to make sure they both run before the ordinary
             callback. *)
          let on_cancel_1_ran = ref false in
          let on_cancel_2_ran = ref false in
          let callback_ran = ref false in
          let p, _ = Lwt.task () in
          Lwt.on_cancel p (fun () -> on_cancel_1_ran := true);
          Lwt.on_failure p (fun _ ->
              assert (!on_cancel_1_ran = true);
              assert (!on_cancel_2_ran = true);
              callback_ran := true);
          Lwt.on_cancel p (fun () -> on_cancel_2_ran := true);
          Lwt.cancel p;
          Lwt.return (!callback_ran = true));
      test "fulfilled" (fun () ->
          let f_ran = ref false in
          Lwt.on_cancel (Lwt.return ()) (fun () -> f_ran := true);
          Lwt.return (!f_ran = false));
      test "rejected" (fun () ->
          let f_ran = ref false in
          Lwt.on_cancel (Lwt.fail Exception) (fun () -> f_ran := true);
          Lwt.return (!f_ran = false));
      test "already canceled" (fun () ->
          let f_ran = ref false in
          Lwt.on_cancel (Lwt.fail Lwt.Canceled) (fun () -> f_ran := true);
          Lwt.return (!f_ran = true));
      (* More generally, this tests that rejecting with [Lwt.Canceled] is equivalent
         to calling [Lwt.cancel]. The difference is that [Lwt.cancel] can be called
         on promises without the need of a resolver. *)
      test "reject with Canceled" (fun () ->
          let f_ran = ref false in
          let p, r = Lwt.wait () in
          Lwt.on_cancel p (fun () -> f_ran := true);
          Lwt.wakeup_exn r Lwt.Canceled;
          Lwt.return (!f_ran = true));
    ]

let suites = suites @ [ on_cancel_tests ]

let protected_tests =
  suite "protected"
    [
      test "fulfilled" (fun () ->
          let p = Lwt.protected (Lwt.return ()) in
          (* If [p] starts fulfilled, it can't be canceled. *)
          Lwt.return (Lwt.state p = Lwt.Return ()));
      test "rejected" (fun () ->
          let p = Lwt.protected (Lwt.fail Exception) in
          Lwt.return (Lwt.state p = Lwt.Fail Exception));
      test "pending" (fun () ->
          let p, _ = Lwt.task () in
          let p' = Lwt.protected p in
          Lwt.return (Lwt.state p = Lwt.Sleep && Lwt.state p' = Lwt.Sleep));
      test "pending, fulfilled" (fun () ->
          let p, r = Lwt.task () in
          let p' = Lwt.protected p in
          Lwt.wakeup r "foo";
          Lwt.return (Lwt.state p' = Lwt.Return "foo"));
      test "pending, canceled" (fun () ->
          let p, _ = Lwt.task () in
          let p' = Lwt.protected p in
          Lwt.cancel p';
          Lwt.return
            (Lwt.state p = Lwt.Sleep && Lwt.state p' = Lwt.Fail Lwt.Canceled));
      test "pending, canceled, fulfilled" (fun () ->
          let p, r = Lwt.task () in
          let p' = Lwt.protected p in
          Lwt.cancel p';
          Lwt.wakeup r "foo";
          Lwt.return
            (Lwt.state p = Lwt.Return "foo"
            && Lwt.state p' = Lwt.Fail Lwt.Canceled));
      (* Implementation detail: [p' = Lwt.protected _] can still be resolved if it
         becomes a proxy. *)
      test "pending, proxy" (fun () ->
          let p1, r1 = Lwt.task () in
          let p2 = Lwt.protected p1 in

          (* Make p2 a proxy for p4; p3 is just needed to suspend the bind, in order
             to callback the code that makes p2 a proxy. *)
          let p3, r3 = Lwt.wait () in
          let _ = Lwt.bind p3 (fun () -> p2) in
          Lwt.wakeup r3 ();

          (* It should now be possible to resolve p2 by resolving p1. *)
          Lwt.wakeup r1 "foo";
          Lwt.return (Lwt.state p2 = Lwt.Return "foo"));
    ]

let suites = suites @ [ protected_tests ]

let cancelable_tests =
  suite "wrap_in_cancelable"
    [
      test "fulfilled" (fun () ->
          let p = Lwt.wrap_in_cancelable (Lwt.return ()) in
          Lwt.return (Lwt.state p = Lwt.Return ()));
      test "rejected" (fun () ->
          let p = Lwt.wrap_in_cancelable (Lwt.fail Exception) in
          Lwt.return (Lwt.state p = Lwt.Fail Exception));
      test "pending(task)" (fun () ->
          let p, _ = Lwt.task () in
          let p' = Lwt.wrap_in_cancelable p in
          Lwt.return (Lwt.state p = Lwt.Sleep && Lwt.state p' = Lwt.Sleep));
      test "pending(task), fulfilled" (fun () ->
          let p, r = Lwt.task () in
          let p' = Lwt.wrap_in_cancelable p in
          Lwt.wakeup r "foo";
          Lwt.return
            (Lwt.state p = Lwt.Return "foo" && Lwt.state p' = Lwt.Return "foo"));
      test "pending(task), canceled" (fun () ->
          let p, _ = Lwt.task () in
          let p' = Lwt.wrap_in_cancelable p in
          Lwt.cancel p';
          Lwt.return
            (Lwt.state p = Lwt.Fail Lwt.Canceled
            && Lwt.state p' = Lwt.Fail Lwt.Canceled));
      test "pending(wait)" (fun () ->
          let p, _ = Lwt.wait () in
          let p' = Lwt.wrap_in_cancelable p in
          Lwt.return (Lwt.state p = Lwt.Sleep && Lwt.state p' = Lwt.Sleep));
      test "pending(wait), fulfilled" (fun () ->
          let p, r = Lwt.wait () in
          let p' = Lwt.wrap_in_cancelable p in
          Lwt.wakeup r "foo";
          Lwt.return
            (Lwt.state p = Lwt.Return "foo" && Lwt.state p' = Lwt.Return "foo"));
      test "pending(wait), canceled" (fun () ->
          let p, _ = Lwt.wait () in
          let p' = Lwt.wrap_in_cancelable p in
          Lwt.cancel p';
          Lwt.return
            (Lwt.state p = Lwt.Sleep && Lwt.state p' = Lwt.Fail Lwt.Canceled));
      test "pending(task), canceled, fulfilled" (fun () ->
          let p, r = Lwt.task () in
          let p' = Lwt.wrap_in_cancelable p in
          Lwt.cancel p';
          Lwt.wakeup r "foo";
          Lwt.return
            (Lwt.state p = Lwt.Fail Lwt.Canceled
            && Lwt.state p' = Lwt.Fail Lwt.Canceled));
      test "pending(wait), canceled, fulfilled" (fun () ->
          let p, r = Lwt.wait () in
          let p' = Lwt.wrap_in_cancelable p in
          Lwt.cancel p';
          Lwt.wakeup r "foo";
          Lwt.return
            (Lwt.state p = Lwt.Return "foo"
            && Lwt.state p' = Lwt.Fail Lwt.Canceled));
      (* Implementation detail: [p' = Lwt.wrap_in_cancelable _] can still be
         resolved if it becomes a proxy. *)
      test "pending, proxy" (fun () ->
          let p1, r1 = Lwt.task () in
          let p2 = Lwt.wrap_in_cancelable p1 in

          (* Make p2 a proxy for p4; p3 is just needed to suspend the bind, in order
             to callback the code that makes p2 a proxy. *)
          let p3, r3 = Lwt.wait () in
          let _ = Lwt.bind p3 (fun () -> p2) in
          Lwt.wakeup r3 ();

          (* It should now be possible to resolve p2 by resolving p1. *)
          Lwt.wakeup r1 "foo";
          Lwt.return (Lwt.state p2 = Lwt.Return "foo"));
    ]

let suites = suites @ [ cancelable_tests ]

let no_cancel_tests =
  suite "no_cancel"
    [
      test "fulfilled" (fun () ->
          let p = Lwt.no_cancel (Lwt.return ()) in
          (* [p] starts fulfilled, so it can't be canceled. *)
          Lwt.return (Lwt.state p = Lwt.Return ()));
      test "rejected" (fun () ->
          let p = Lwt.no_cancel (Lwt.fail Exception) in
          Lwt.return (Lwt.state p = Lwt.Fail Exception));
      test "pending" (fun () ->
          let p, _ = Lwt.task () in
          let p' = Lwt.no_cancel p in
          Lwt.return (Lwt.state p = Lwt.Sleep && Lwt.state p' = Lwt.Sleep));
      test "pending, fulfilled" (fun () ->
          let p, r = Lwt.task () in
          let p = Lwt.no_cancel p in
          Lwt.wakeup r "foo";
          Lwt.return (Lwt.state p = Lwt.Return "foo"));
      test "pending, cancel attempt" (fun () ->
          let p, _ = Lwt.task () in
          let p' = Lwt.no_cancel p in
          Lwt.cancel p';
          Lwt.return (Lwt.state p = Lwt.Sleep && Lwt.state p' = Lwt.Sleep));
    ]

let suites = suites @ [ no_cancel_tests ]

let resolve_already_canceled_promise_tests =
  suite "resolve canceled"
    [
      test "wakeup: canceled" (fun () ->
          let p, r = Lwt.task () in
          Lwt.cancel p;
          Lwt.wakeup r ();
          Lwt.return (Lwt.state p = Lwt.Fail Lwt.Canceled));
      (* This test can start falsely passing if the entire test is run inside an
         Lwt promise resolution phase, e.g. inside an outer [Lwt.wakeup_later]. *)
      test "wakeup_later: canceled" (fun () ->
          let p, r = Lwt.task () in
          Lwt.cancel p;
          Lwt.wakeup_later r ();
          Lwt.return (Lwt.state p = Lwt.Fail Lwt.Canceled));
    ]

let suites = suites @ [ resolve_already_canceled_promise_tests ]

let pick_tests =
  suite "pick"
    [
      (test "empty" (fun () ->
           try
             ignore (Lwt.pick []);
             Lwt.return false
           with
           | Invalid_argument
               "Lwt.pick [] would return a promise that is pending forever"
           ->
             Lwt.return true) [@ocaml.warning "-52"]);
      test "fulfilled" (fun () ->
          let p1, _ = Lwt.task () in
          let p2 = Lwt.pick [ p1; Lwt.return "foo" ] in
          Lwt.return
            (Lwt.state p1 = Lwt.Fail Lwt.Canceled
            && Lwt.state p2 = Lwt.Return "foo"));
      test "rejected" (fun () ->
          let p1, _ = Lwt.task () in
          let p2 = Lwt.pick [ p1; Lwt.fail Exception ] in
          Lwt.return
            (Lwt.state p1 = Lwt.Fail Lwt.Canceled
            && Lwt.state p2 = Lwt.Fail Exception));
      test "multiple resolved" (fun () ->
          (* This is run in a loop to exercise the internal PRNG. *)
          let outcomes = Array.make 3 0 in
          let rec repeat n =
            if n <= 0 then ()
            else
              let p =
                Lwt.pick
                  [
                    fst (Lwt.wait ());
                    Lwt.return "foo";
                    Lwt.fail Exception;
                    Lwt.return "bar";
                  ]
              in
              (match[@ocaml.warning "-4"] Lwt.state p with
              | Lwt.Return "foo" -> outcomes.(0) <- outcomes.(0) + 1
              | Lwt.Fail Exception -> outcomes.(1) <- outcomes.(1) + 1
              | Lwt.Return "bar" -> outcomes.(2) <- outcomes.(2) + 1
              | _ -> assert false);
              repeat (n - 1)
          in
          let count = 1000 in
          repeat count;
          Lwt.return
            (outcomes.(0) > 0
            && outcomes.(1) > 0
            && outcomes.(2) > 0
            && outcomes.(0) + outcomes.(1) + outcomes.(2) = count));
      test "pending" (fun () ->
          let p = Lwt.pick [ fst (Lwt.wait ()); fst (Lwt.wait ()) ] in
          Lwt.return (Lwt.state p = Lwt.Sleep));
      test "pending, fulfilled" (fun () ->
          let p1, r1 = Lwt.task () in
          let p2, _ = Lwt.task () in
          let p = Lwt.pick [ p1; p2 ] in
          Lwt.wakeup r1 "foo";
          Lwt.return
            (Lwt.state p2 = Lwt.Fail Lwt.Canceled
            && Lwt.state p = Lwt.Return "foo"));
      test "diamond" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.pick [ p; p ] in
          Lwt.wakeup r "foo";
          Lwt.return (Lwt.state p = Lwt.Return "foo"));
      test "pending, canceled" (fun () ->
          let p1, _ = Lwt.task () in
          let p2, _ = Lwt.task () in
          let p = Lwt.pick [ p1; p2 ] in
          Lwt.cancel p;
          Lwt.return
            (Lwt.state p1 = Lwt.Fail Lwt.Canceled
            && Lwt.state p2 = Lwt.Fail Lwt.Canceled));
      test "cancellation/resolution order" (fun () ->
          let a = [| 0; 0 |] in
          let i = ref 0 in
          let p1, r1 = Lwt.task () in
          let p2, _ = Lwt.task () in
          let p3 = Lwt.pick [ p1; p2 ] in
          let _ =
            Lwt.catch
              (fun () -> p2)
              (fun _ ->
                a.(!i) <- 1;
                i := 1;
                Lwt.return ())
          in
          let _ =
            Lwt.bind p3 (fun _ ->
                a.(!i) <- 2;
                i := 1;
                Lwt.return ())
          in
          Lwt.wakeup_later r1 ();
          Lwt.return (a.(0) = 1 && a.(1) = 2));
    ]

let suites = suites @ [ pick_tests ]

let npick_tests =
  suite "npick"
    [
      (test "empty" (fun () ->
           try
             ignore (Lwt.npick []);
             Lwt.return false
           with
           | Invalid_argument
               "Lwt.npick [] would return a promise that is pending forever"
           ->
             Lwt.return true) [@ocaml.warning "-52"]);
      test "all fulfilled" (fun () ->
          let p = Lwt.npick [ Lwt.return "foo"; Lwt.return "bar" ] in
          Lwt.return (Lwt.state p = Lwt.Return [ "foo"; "bar" ]));
      test "fulfilled, rejected" (fun () ->
          let p = Lwt.npick [ Lwt.return "foo"; Lwt.fail Exception ] in
          Lwt.return (Lwt.state p = Lwt.Fail Exception));
      test "rejected, fulfilled" (fun () ->
          let p = Lwt.npick [ Lwt.fail Exception; Lwt.return "foo" ] in
          Lwt.return (Lwt.state p = Lwt.Fail Exception));
      test "some pending" (fun () ->
          let p1, _ = Lwt.task () in
          let p2 = Lwt.npick [ Lwt.return "foo"; p1; Lwt.return "bar" ] in
          Lwt.return
            (Lwt.state p1 = Lwt.Fail Lwt.Canceled
            && Lwt.state p2 = Lwt.Return [ "foo"; "bar" ]));
      test "pending" (fun () ->
          let p = Lwt.npick [ fst (Lwt.task ()); fst (Lwt.task ()) ] in
          Lwt.return (Lwt.state p = Lwt.Sleep));
      test "pending, fulfilled" (fun () ->
          let p1, _ = Lwt.task () in
          let p2, r = Lwt.task () in
          let p = Lwt.npick [ p1; p2 ] in
          Lwt.wakeup r "foo";
          Lwt.return
            (Lwt.state p1 = Lwt.Fail Lwt.Canceled
            && Lwt.state p = Lwt.Return [ "foo" ]));
      test "pending, rejected" (fun () ->
          let p1, _ = Lwt.task () in
          let p2, r = Lwt.task () in
          let p = Lwt.npick [ p1; p2 ] in
          Lwt.wakeup_exn r Exception;
          Lwt.return
            (Lwt.state p1 = Lwt.Fail Lwt.Canceled
            && Lwt.state p = Lwt.Fail Exception));
      test "diamond" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.npick [ p; p ] in
          Lwt.wakeup r "foo";
          Lwt.return (Lwt.state p = Lwt.Return [ "foo"; "foo" ]));
      test "diamond, rejected" (fun () ->
          let p, r = Lwt.wait () in
          let p = Lwt.npick [ p; p ] in
          Lwt.wakeup_exn r Exception;
          Lwt.return (Lwt.state p = Lwt.Fail Exception));
      test "pending, canceled" (fun () ->
          let p1, _ = Lwt.task () in
          let p2, _ = Lwt.task () in
          let p = Lwt.npick [ p1; p2 ] in
          Lwt.cancel p;
          Lwt.return
            (Lwt.state p1 = Lwt.Fail Lwt.Canceled
            && Lwt.state p2 = Lwt.Fail Lwt.Canceled));
      test "cancellation/resolution order" (fun () ->
          let a = [| 0; 0 |] in
          let i = ref 0 in
          let p1, r1 = Lwt.task () in
          let p2, _ = Lwt.task () in
          let p3 = Lwt.npick [ p1; p2 ] in
          let _ =
            Lwt.catch
              (fun () -> p2)
              (fun _ ->
                a.(!i) <- 1;
                i := 1;
                Lwt.return ())
          in
          let _ =
            Lwt.bind p3 (fun _ ->
                a.(!i) <- 2;
                i := 1;
                Lwt.return ())
          in
          Lwt.wakeup_later r1 ();
          Lwt.return (a.(0) = 1 && a.(1) = 2));
    ]

let suites = suites @ [ npick_tests ]

let cancel_bind_tests =
  suite "cancel bind"
    [
      test "wait, pending, canceled" (fun () ->
          let f_ran = ref false in
          let p, _ = Lwt.wait () in
          let p' =
            Lwt.bind p (fun () ->
                f_ran := true;
                Lwt.return ())
          in
          Lwt.cancel p';
          Lwt.return
            (!f_ran = false
            && Lwt.state p = Lwt.Sleep
            && Lwt.state p' = Lwt.Sleep));
      test "task, pending, canceled" (fun () ->
          let f_ran = ref false in
          let p, _ = Lwt.task () in
          let p' =
            Lwt.bind p (fun () ->
                f_ran := true;
                Lwt.return ())
          in
          Lwt.cancel p';
          Lwt.return
            (!f_ran = false
            && Lwt.state p = Lwt.Fail Lwt.Canceled
            && Lwt.state p' = Lwt.Fail Lwt.Canceled));
      test "pending, wait, canceled" (fun () ->
          let p, r = Lwt.wait () in
          let p', _ = Lwt.wait () in
          let p'' = Lwt.bind p (fun () -> p') in
          Lwt.wakeup r ();
          (* [bind]'s [f] ran, and now [p'] and [p''] should share the same state. *)
          Lwt.cancel p'';
          Lwt.return (Lwt.state p' = Lwt.Sleep && Lwt.state p'' = Lwt.Sleep));
      test "pending, task, canceled" (fun () ->
          let p1, r = Lwt.wait () in
          let p2, _ = Lwt.task () in
          let p3 = Lwt.bind p1 (fun () -> p2) in
          Lwt.wakeup r ();
          Lwt.cancel p3;
          Lwt.return
            (Lwt.state p2 = Lwt.Fail Lwt.Canceled
            && Lwt.state p3 = Lwt.Fail Lwt.Canceled
            && p2 != p3));
      test "pending, task, canceled, chain" (fun () ->
          let p1, r = Lwt.wait () in
          let p2, _ = Lwt.task () in
          let p3 = Lwt.bind p1 (fun () -> p2) in
          let p4 = Lwt.bind p1 (fun () -> p3) in
          Lwt.wakeup r ();
          (* At this point, [p4] and [p3] share the same state, and canceling [p4]
             should chain to [p2], because [p3] is obtained by binding on [p2]. *)
          Lwt.cancel p4;
          Lwt.return
            (Lwt.state p2 = Lwt.Fail Lwt.Canceled
            && Lwt.state p3 = Lwt.Fail Lwt.Canceled
            && Lwt.state p4 = Lwt.Fail Lwt.Canceled));
      test "pending, on_cancel callbacks" (fun () ->
          let f_ran = ref false in
          let g_ran = ref false in
          let p1, _ = Lwt.task () in
          let p2 = Lwt.bind (fst (Lwt.task ())) (fun () -> p1) in
          Lwt.on_cancel p1 (fun () -> f_ran := true);
          Lwt.on_cancel p2 (fun () -> g_ran := true);
          Lwt.cancel p2;
          (* Canceling [p2] doesn't cancel [p1], because the function passed to
             [Lwt.bind] never ran. *)
          Lwt.return (!f_ran = false && !g_ran = true));
      test "pending, fulfilled, on_cancel callbacks" (fun () ->
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
          Lwt.return (!f_ran = true && !g_ran = true));
    ]

let suites = suites @ [ cancel_bind_tests ]

let cancel_map_tests =
  suite "cancel map"
    [
      test "wait, pending, canceled" (fun () ->
          let f_ran = ref false in
          let p, _ = Lwt.wait () in
          let p' = Lwt.map (fun () -> f_ran := true) p in
          Lwt.cancel p';
          Lwt.return
            (!f_ran = false
            && Lwt.state p = Lwt.Sleep
            && Lwt.state p' = Lwt.Sleep));
      test "task, pending, canceled" (fun () ->
          let f_ran = ref false in
          let p, _ = Lwt.task () in
          let p' = Lwt.map (fun () -> f_ran := true) p in
          Lwt.cancel p';
          Lwt.return
            (!f_ran = false
            && Lwt.state p = Lwt.Fail Lwt.Canceled
            && Lwt.state p' = Lwt.Fail Lwt.Canceled));
    ]

let suites = suites @ [ cancel_map_tests ]

let cancel_catch_tests =
  suite "cancel catch"
    [
      (* In [p' = Lwt.catch (fun () -> p) f], if [p] is not cancelable, [p'] is also
         not cancelable. *)
      test "wait, pending, canceled" (fun () ->
          let f_ran = ref false in
          let p, _ = Lwt.wait () in
          let p' =
            Lwt.catch
              (fun () -> p)
              (fun _ ->
                f_ran := true;
                Lwt.return ())
          in
          Lwt.cancel p';
          Lwt.return
            (!f_ran = false
            && Lwt.state p = Lwt.Sleep
            && Lwt.state p' = Lwt.Sleep));
      (* In [p' = Lwt.catch (fun () -> p) f], if [p] is cancelable, canceling [p']
         propagates to [p], and then the cancellation exception can be "intercepted"
         by [f], which can resolve [p'] in an arbitrary way. *)
      test "task, pending, canceled" (fun () ->
          let saw = ref None in
          let p, _ = Lwt.task () in
          let p' =
            Lwt.catch
              (fun () -> p)
              (fun exn ->
                saw := Some exn;
                Lwt.return "foo")
          in
          Lwt.cancel p';
          Lwt.return
            (!saw = Some Lwt.Canceled
            && Lwt.state p = Lwt.Fail Lwt.Canceled
            && Lwt.state p' = Lwt.Return "foo"));
      (* In [p' = Lwt.catch (fun () -> p) f], if [p] is cancelable, and cancel
         callbacks are added to both [p] and [p'], and [f] does not resolve [p']
         with [Lwt.Fail Lwt.Canceled], only the callback on [p] runs. *)
      test "task, pending, canceled, on_cancel, intercepted" (fun () ->
          let on_cancel_1_ran = ref false in
          let on_cancel_2_ran = ref false in
          let p, _ = Lwt.task () in
          let p' =
            Lwt.catch
              (fun () -> p)
              (fun _ ->
                assert (!on_cancel_1_ran = true && !on_cancel_2_ran = false);
                Lwt.return "foo")
          in
          Lwt.on_cancel p (fun () -> on_cancel_1_ran := true);
          Lwt.on_cancel p' (fun () -> on_cancel_2_ran := true);
          Lwt.cancel p';
          Lwt.return
            (Lwt.state p = Lwt.Fail Lwt.Canceled
            && Lwt.state p' = Lwt.Return "foo"
            && !on_cancel_2_ran = false));
      (* Same as above, except this time, cancellation is passed on to the outer
         promise, so we can expect both cancel callbacks to run. *)
      test "task, pending, canceled, on_cancel, forwarded" (fun () ->
          let on_cancel_2_ran = ref false in
          let p, _ = Lwt.task () in
          let p' = Lwt.catch (fun () -> p) Lwt.fail in
          Lwt.on_cancel p' (fun () -> on_cancel_2_ran := true);
          Lwt.cancel p';
          Lwt.return
            (Lwt.state p = Lwt.Fail Lwt.Canceled
            && Lwt.state p' = Lwt.Fail Lwt.Canceled
            && !on_cancel_2_ran = true));
      (* (2 tests) If the handler passed to [Lwt.catch] already ran, canceling the
         outer promise is the same as canceling the promise returned by the
         handler. *)
      test "pending, wait, canceled" (fun () ->
          let p1, r = Lwt.wait () in
          let p2, _ = Lwt.wait () in
          let p3 = Lwt.catch (fun () -> p1) (fun _ -> p2) in
          Lwt.wakeup_exn r Exception;
          Lwt.cancel p3;
          Lwt.return (Lwt.state p2 = Lwt.Sleep && Lwt.state p3 = Lwt.Sleep));
      test "pending, task, canceled" (fun () ->
          let p1, r = Lwt.wait () in
          let p2, _ = Lwt.task () in
          let p3 = Lwt.catch (fun () -> p1) (fun _ -> p2) in
          Lwt.wakeup_exn r Exception;
          Lwt.cancel p3;
          Lwt.return
            (Lwt.state p2 = Lwt.Fail Lwt.Canceled
            && Lwt.state p3 = Lwt.Fail Lwt.Canceled));
    ]

let suites = suites @ [ cancel_catch_tests ]

let cancel_try_bind_tests =
  suite "cancel try_bind"
    [
      test "wait, pending, canceled" (fun () ->
          let f_or_g_ran = ref false in
          let p, _ = Lwt.wait () in
          let p' =
            Lwt.try_bind
              (fun () -> p)
              (fun () ->
                f_or_g_ran := true;
                Lwt.return ())
              (fun _ ->
                f_or_g_ran := true;
                Lwt.return ())
          in
          Lwt.cancel p';
          Lwt.return
            (!f_or_g_ran = false
            && Lwt.state p = Lwt.Sleep
            && Lwt.state p' = Lwt.Sleep));
      test "task, pending, canceled" (fun () ->
          let f_ran = ref false in
          let saw = ref None in
          let p, _ = Lwt.task () in
          let p' =
            Lwt.try_bind
              (fun () -> p)
              (fun () ->
                f_ran := true;
                Lwt.return "foo")
              (fun exn ->
                saw := Some exn;
                Lwt.return "bar")
          in
          Lwt.cancel p';
          Lwt.return
            (!f_ran = false
            && !saw = Some Lwt.Canceled
            && Lwt.state p = Lwt.Fail Lwt.Canceled
            && Lwt.state p' = Lwt.Return "bar"));
      test "pending, fulfilled, wait, canceled" (fun () ->
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
          Lwt.return (Lwt.state p2 = Lwt.Sleep && Lwt.state p3 = Lwt.Sleep));
      test "pending, fulfilled, task, canceled" (fun () ->
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
            (Lwt.state p2 = Lwt.Fail Lwt.Canceled
            && Lwt.state p3 = Lwt.Fail Lwt.Canceled));
      test "pending, rejected, wait, canceled" (fun () ->
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
          Lwt.return (Lwt.state p2 = Lwt.Sleep && Lwt.state p3 = Lwt.Sleep));
      test "pending, rejected, task, canceled" (fun () ->
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
            (Lwt.state p2 = Lwt.Fail Lwt.Canceled
            && Lwt.state p3 = Lwt.Fail Lwt.Canceled));
    ]

let suites = suites @ [ cancel_try_bind_tests ]

let cancel_finalize_tests =
  suite "cancel finalize"
    [
      test "wait, pending, canceled" (fun () ->
          let f_ran = ref false in
          let p, _ = Lwt.wait () in
          let p' =
            Lwt.finalize
              (fun () -> p)
              (fun () ->
                f_ran := true;
                Lwt.return ())
          in
          Lwt.cancel p';
          Lwt.return
            (!f_ran = false
            && Lwt.state p = Lwt.Sleep
            && Lwt.state p' = Lwt.Sleep));
      test "task, pending, canceled" (fun () ->
          let f_ran = ref false in
          let p, _ = Lwt.task () in
          let p' =
            Lwt.finalize
              (fun () -> p)
              (fun () ->
                f_ran := true;
                Lwt.return ())
          in
          Lwt.cancel p';
          Lwt.return
            (!f_ran = true
            && Lwt.state p = Lwt.Fail Lwt.Canceled
            && Lwt.state p' = Lwt.Fail Lwt.Canceled));
      test "task, canceled, cancel exception replaced" (fun () ->
          let p, _ = Lwt.task () in
          let p' = Lwt.finalize (fun () -> p) (fun () -> Lwt.fail Exception) in
          Lwt.cancel p;
          Lwt.return (Lwt.state p' = Lwt.Fail Exception));
      test "pending, wait, canceled" (fun () ->
          let p1, r = Lwt.wait () in
          let p2, _ = Lwt.wait () in
          let p3 = Lwt.finalize (fun () -> p1) (fun () -> p2) in
          Lwt.wakeup r ();
          Lwt.cancel p3;
          Lwt.return (Lwt.state p2 = Lwt.Sleep && Lwt.state p3 = Lwt.Sleep));
      test "pending, task, canceled" (fun () ->
          let p1, r = Lwt.wait () in
          let p2, _ = Lwt.task () in
          let p3 = Lwt.finalize (fun () -> p1) (fun () -> p2) in
          Lwt.wakeup r ();
          Lwt.cancel p3;
          Lwt.return
            (Lwt.state p2 = Lwt.Fail Lwt.Canceled
            && Lwt.state p3 = Lwt.Fail Lwt.Canceled));
    ]

let suites = suites @ [ cancel_finalize_tests ]

let cancel_direct_handler_tests =
  suite "cancel with direct handler"
    [
      test "on_success: pending, canceled" (fun () ->
          let f_ran = ref false in
          let p, _ = Lwt.task () in
          Lwt.on_success p (fun () -> f_ran := true);
          Lwt.cancel p;
          Lwt.return (!f_ran = false));
      test "on_failure: pending, canceled" (fun () ->
          let saw = ref None in
          let p, _ = Lwt.task () in
          Lwt.on_failure p (fun exn -> saw := Some exn);
          Lwt.cancel p;
          Lwt.return (!saw = Some Lwt.Canceled));
      test "on_termination: pending, canceled" (fun () ->
          let f_ran = ref false in
          let p, _ = Lwt.task () in
          Lwt.on_termination p (fun () -> f_ran := true);
          Lwt.cancel p;
          Lwt.return (!f_ran = true));
      test "on_any: pending, canceled" (fun () ->
          let f_ran = ref false in
          let saw = ref None in
          let p, _ = Lwt.task () in
          Lwt.on_any p (fun () -> f_ran := true) (fun exn -> saw := Some exn);
          Lwt.cancel p;
          Lwt.return (!f_ran = false && !saw = Some Lwt.Canceled));
    ]

let suites = suites @ [ cancel_direct_handler_tests ]

let cancel_join_tests =
  suite "cancel join"
    [
      test "wait, pending, cancel" (fun () ->
          let p1, _ = Lwt.wait () in
          let p2, _ = Lwt.wait () in
          let p3 = Lwt.join [ p1; p2 ] in
          Lwt.cancel p3;
          Lwt.return
            (Lwt.state p1 = Lwt.Sleep
            && Lwt.state p2 = Lwt.Sleep
            && Lwt.state p3 = Lwt.Sleep));
      test "task, pending, cancel" (fun () ->
          let p1, _ = Lwt.task () in
          let p2, _ = Lwt.task () in
          let p3 = Lwt.join [ p1; p2 ] in
          Lwt.cancel p3;
          Lwt.return
            (Lwt.state p1 = Lwt.Fail Lwt.Canceled
            && Lwt.state p2 = Lwt.Fail Lwt.Canceled
            && Lwt.state p3 = Lwt.Fail Lwt.Canceled));
      test "wait and task, pending, cancel" (fun () ->
          let p1, r = Lwt.wait () in
          let p2, _ = Lwt.task () in
          let p3 = Lwt.join [ p1; p2 ] in
          Lwt.cancel p3;
          assert (Lwt.state p1 = Lwt.Sleep);
          assert (Lwt.state p2 = Lwt.Fail Lwt.Canceled);
          assert (Lwt.state p3 = Lwt.Sleep);
          Lwt.wakeup r ();
          Lwt.return
            (Lwt.state p1 = Lwt.Return ()
            && Lwt.state p3 = Lwt.Fail Lwt.Canceled));
      (* In [p' = Lwt.join [p; p]], if [p'] is canceled, the cancel handler on [p]
         is called only once, even though it is reachable by two paths in the
         cancellation graph. *)
      test "cancel diamond" (fun () ->
          let ran = ref 0 in
          let p, _ = Lwt.task () in
          let p' = Lwt.join [ p; p ] in
          Lwt.on_cancel p (fun () -> ran := !ran + 1);
          Lwt.cancel p';
          Lwt.return (!ran = 1));
    ]

let suites = suites @ [ cancel_join_tests ]

let cancel_choose_tests =
  suite "cancel choose"
    [
      test "wait, pending, cancel" (fun () ->
          let p1, _ = Lwt.wait () in
          let p2, _ = Lwt.wait () in
          let p3 = Lwt.choose [ p1; p2 ] in
          Lwt.cancel p3;
          Lwt.return
            (Lwt.state p1 = Lwt.Sleep
            && Lwt.state p2 = Lwt.Sleep
            && Lwt.state p3 = Lwt.Sleep));
      test "wait and task, pending, cancel" (fun () ->
          let p1, _ = Lwt.wait () in
          let p2, _ = Lwt.task () in
          let p3 = Lwt.choose [ p1; p2 ] in
          Lwt.cancel p3;
          Lwt.return
            (Lwt.state p1 = Lwt.Sleep
            && Lwt.state p2 = Lwt.Fail Lwt.Canceled
            && Lwt.state p3 = Lwt.Fail Lwt.Canceled));
    ]

let suites = suites @ [ cancel_choose_tests ]

let cancel_pick_tests =
  suite "cancel pick"
    [
      test "wait, pending, cancel" (fun () ->
          let p1, _ = Lwt.wait () in
          let p2, _ = Lwt.wait () in
          let p3 = Lwt.pick [ p1; p2 ] in
          Lwt.cancel p3;
          Lwt.return
            (Lwt.state p1 = Lwt.Sleep
            && Lwt.state p2 = Lwt.Sleep
            && Lwt.state p3 = Lwt.Sleep));
      test "wait and task, pending, cancel" (fun () ->
          let p1, _ = Lwt.wait () in
          let p2, _ = Lwt.task () in
          let p3 = Lwt.pick [ p1; p2 ] in
          Lwt.cancel p3;
          Lwt.return
            (Lwt.state p1 = Lwt.Sleep
            && Lwt.state p2 = Lwt.Fail Lwt.Canceled
            && Lwt.state p3 = Lwt.Fail Lwt.Canceled));
    ]

let suites = suites @ [ cancel_pick_tests ]

let cancel_nchoose_tests =
  suite "cancel nchoose"
    [
      test "wait, pending, cancel" (fun () ->
          let p1, _ = Lwt.wait () in
          let p2, _ = Lwt.wait () in
          let p3 = Lwt.nchoose [ p1; p2 ] in
          Lwt.cancel p3;
          Lwt.return
            (Lwt.state p1 = Lwt.Sleep
            && Lwt.state p2 = Lwt.Sleep
            && Lwt.state p3 = Lwt.Sleep));
      test "wait and task, pending, cancel" (fun () ->
          let p1, _ = Lwt.wait () in
          let p2, _ = Lwt.task () in
          let p3 = Lwt.nchoose [ p1; p2 ] in
          Lwt.cancel p3;
          Lwt.return
            (Lwt.state p1 = Lwt.Sleep
            && Lwt.state p2 = Lwt.Fail Lwt.Canceled
            && Lwt.state p3 = Lwt.Fail Lwt.Canceled));
    ]

let suites = suites @ [ cancel_nchoose_tests ]

let cancel_npick_tests =
  suite "cancel npick"
    [
      test "wait, pending, cancel" (fun () ->
          let p1, _ = Lwt.wait () in
          let p2, _ = Lwt.wait () in
          let p3 = Lwt.npick [ p1; p2 ] in
          Lwt.cancel p3;
          Lwt.return
            (Lwt.state p1 = Lwt.Sleep
            && Lwt.state p2 = Lwt.Sleep
            && Lwt.state p3 = Lwt.Sleep));
      test "wait and task, pending, cancel" (fun () ->
          let p1, _ = Lwt.wait () in
          let p2, _ = Lwt.task () in
          let p3 = Lwt.npick [ p1; p2 ] in
          Lwt.cancel p3;
          Lwt.return
            (Lwt.state p1 = Lwt.Sleep
            && Lwt.state p2 = Lwt.Fail Lwt.Canceled
            && Lwt.state p3 = Lwt.Fail Lwt.Canceled));
    ]

let suites = suites @ [ cancel_npick_tests ]

let cancel_nchoose_split_tests =
  suite "cancel nchoose_split"
    [
      test "wait, pending, cancel" (fun () ->
          let p1, _ = Lwt.wait () in
          let p2, _ = Lwt.wait () in
          let p3 = Lwt.nchoose_split [ p1; p2 ] in
          Lwt.cancel p3;
          Lwt.return
            (Lwt.state p1 = Lwt.Sleep
            && Lwt.state p2 = Lwt.Sleep
            && Lwt.state p3 = Lwt.Sleep));
      test "wait and task, pending, cancel" (fun () ->
          let p1, _ = Lwt.wait () in
          let p2, _ = Lwt.task () in
          let p3 = Lwt.nchoose_split [ p1; p2 ] in
          Lwt.cancel p3;
          Lwt.return
            (Lwt.state p1 = Lwt.Sleep
            && Lwt.state p2 = Lwt.Fail Lwt.Canceled
            && Lwt.state p3 = Lwt.Fail Lwt.Canceled));
    ]

let suites = suites @ [ cancel_nchoose_split_tests ]

(* Sequence-associated storage, and its interaction with the rest of the API. *)

let storage_tests =
  suite "storage"
    [
      test "initial" (fun () ->
          let key = Lwt.new_key () in
          Lwt.return (Lwt.get key = None));
      test "store, retrieve" (fun () ->
          let key = Lwt.new_key () in
          Lwt.with_value key (Some 42) (fun () ->
              Lwt.return (Lwt.get key = Some 42)));
      test "store, restore" (fun () ->
          let key = Lwt.new_key () in
          Lwt.with_value key (Some 42) ignore;
          Lwt.return (Lwt.get key = None));
      test "store, f raises, restore" (fun () ->
          let key = Lwt.new_key () in
          try
            Lwt.with_value key (Some 42) (fun () -> raise Exception) |> ignore;
            Lwt.return false
          with Exception -> Lwt.return (Lwt.get key = None));
      test "store, overwrite, retrieve" (fun () ->
          let key = Lwt.new_key () in
          Lwt.with_value key (Some 42) (fun () ->
              Lwt.with_value key (Some 1337) (fun () ->
                  Lwt.return (Lwt.get key = Some 1337))));
      test "store, blank, retrieve" (fun () ->
          let key = Lwt.new_key () in
          Lwt.with_value key (Some 42) (fun () ->
              Lwt.with_value key None (fun () ->
                  Lwt.return (Lwt.get key = None))));
      test "distinct keys" (fun () ->
          let key1 = Lwt.new_key () in
          let key2 = Lwt.new_key () in
          Lwt.with_value key1 (Some 42) (fun () ->
              Lwt.return (Lwt.get key2 = None)));
      test "bind" (fun () ->
          let key = Lwt.new_key () in
          let f () = Lwt.return (Lwt.get key) in
          let p, r = Lwt.wait () in
          Lwt.with_value key (Some 42) (fun () ->
              let p' =
                Lwt.with_value key (Some 1337) (fun () -> Lwt.bind p f)
              in
              Lwt.wakeup r ();
              Lwt.return
                (Lwt.state p' = Lwt.Return (Some 1337) && Lwt.get key = Some 42)));
      test "map" (fun () ->
          let key = Lwt.new_key () in
          let f () = Lwt.get key in
          let p, r = Lwt.wait () in
          Lwt.with_value key (Some 42) (fun () ->
              let p' = Lwt.with_value key (Some 1337) (fun () -> Lwt.map f p) in
              Lwt.wakeup r ();
              Lwt.return
                (Lwt.state p' = Lwt.Return (Some 1337) && Lwt.get key = Some 42)));
      test "catch" (fun () ->
          let key = Lwt.new_key () in
          let f _ = Lwt.return (Lwt.get key) in
          let p, r = Lwt.wait () in
          Lwt.with_value key (Some 42) (fun () ->
              let p' =
                Lwt.with_value key (Some 1337) (fun () ->
                    Lwt.catch (fun () -> p) f)
              in
              Lwt.wakeup_exn r Exception;
              Lwt.return
                (Lwt.state p' = Lwt.Return (Some 1337) && Lwt.get key = Some 42)));
      test "try_bind, fulfilled" (fun () ->
          let key = Lwt.new_key () in
          let f () = Lwt.return (Lwt.get key) in
          let p, r = Lwt.wait () in
          Lwt.with_value key (Some 42) (fun () ->
              let p' =
                Lwt.with_value key (Some 1337) (fun () ->
                    Lwt.try_bind (fun () -> p) f Lwt.fail)
              in
              Lwt.wakeup r ();
              Lwt.return
                (Lwt.state p' = Lwt.Return (Some 1337) && Lwt.get key = Some 42)));
      test "try_bind, rejected" (fun () ->
          let key = Lwt.new_key () in
          let f _ = Lwt.return (Lwt.get key) in
          let p, r = Lwt.wait () in
          Lwt.with_value key (Some 42) (fun () ->
              let p' =
                Lwt.with_value key (Some 1337) (fun () ->
                    Lwt.try_bind (fun () -> p) Lwt.return f)
              in
              Lwt.wakeup_exn r Exception;
              Lwt.return
                (Lwt.state p' = Lwt.Return (Some 1337) && Lwt.get key = Some 42)));
      test "finalize" (fun () ->
          let key = Lwt.new_key () in
          let f () =
            assert (Lwt.get key = Some 1337);
            Lwt.return ()
          in
          let p, r = Lwt.wait () in
          Lwt.with_value key (Some 42) (fun () ->
              Lwt.with_value key (Some 1337) (fun () ->
                  Lwt.finalize (fun () -> p) f)
              |> ignore;
              Lwt.wakeup r ();
              Lwt.return (Lwt.get key = Some 42)));
      test "on_success" (fun () ->
          let key = Lwt.new_key () in
          let f () = assert (Lwt.get key = Some 1337) in
          let p, r = Lwt.wait () in
          Lwt.with_value key (Some 42) (fun () ->
              Lwt.with_value key (Some 1337) (fun () -> Lwt.on_success p f);
              Lwt.wakeup r ();
              Lwt.return (Lwt.get key = Some 42)));
      test "on_failure" (fun () ->
          let key = Lwt.new_key () in
          let f _ = assert (Lwt.get key = Some 1337) in
          let p, r = Lwt.wait () in
          Lwt.with_value key (Some 42) (fun () ->
              Lwt.with_value key (Some 1337) (fun () -> Lwt.on_failure p f);
              Lwt.wakeup_exn r Exception;
              Lwt.return (Lwt.get key = Some 42)));
      test "on_termination, fulfilled" (fun () ->
          let key = Lwt.new_key () in
          let f () = assert (Lwt.get key = Some 1337) in
          let p, r = Lwt.wait () in
          Lwt.with_value key (Some 42) (fun () ->
              Lwt.with_value key (Some 1337) (fun () -> Lwt.on_termination p f);
              Lwt.wakeup r ();
              Lwt.return (Lwt.get key = Some 42)));
      test "on_termination, rejected" (fun () ->
          let key = Lwt.new_key () in
          let f () = assert (Lwt.get key = Some 1337) in
          let p, r = Lwt.wait () in
          Lwt.with_value key (Some 42) (fun () ->
              Lwt.with_value key (Some 1337) (fun () -> Lwt.on_termination p f);
              Lwt.wakeup_exn r Exception;
              Lwt.return (Lwt.get key = Some 42)));
      test "on_any, fulfilled" (fun () ->
          let key = Lwt.new_key () in
          let f () = assert (Lwt.get key = Some 1337) in
          let p, r = Lwt.wait () in
          Lwt.with_value key (Some 42) (fun () ->
              Lwt.with_value key (Some 1337) (fun () -> Lwt.on_any p f ignore);
              Lwt.wakeup r ();
              Lwt.return (Lwt.get key = Some 42)));
      test "on_any, rejected" (fun () ->
          let key = Lwt.new_key () in
          let f _ = assert (Lwt.get key = Some 1337) in
          let p, r = Lwt.wait () in
          Lwt.with_value key (Some 42) (fun () ->
              Lwt.with_value key (Some 1337) (fun () -> Lwt.on_any p ignore f);
              Lwt.wakeup r ();
              Lwt.return (Lwt.get key = Some 42)));
      test "on_cancel" (fun () ->
          let key = Lwt.new_key () in
          let f () = assert (Lwt.get key = Some 1337) in
          let p, _ = Lwt.task () in
          Lwt.with_value key (Some 42) (fun () ->
              Lwt.with_value key (Some 1337) (fun () -> Lwt.on_cancel p f);
              Lwt.cancel p;
              Lwt.return (Lwt.get key = Some 42)));
    ]

let suites = suites @ [ storage_tests ]

(* These basically just test that the infix operators are exposed in the API,
   and are defined "more or less" as they should be. *)
let infix_operator_tests =
  suite "infix operators"
    [
      test ">>=" (fun () ->
          let open Lwt.Infix in
          let p, r = Lwt.wait () in
          let p' = p >>= fun s -> Lwt.return (s ^ "bar") in
          Lwt.wakeup r "foo";
          state_is (Lwt.Return "foobar") p');
      test "=<<" (fun () ->
          let open Lwt.Infix in
          let p, r = Lwt.wait () in
          let p' = (fun s -> Lwt.return (s ^ "bar")) =<< p in
          Lwt.wakeup r "foo";
          state_is (Lwt.Return "foobar") p');
      test ">|=" (fun () ->
          let open Lwt.Infix in
          let p, r = Lwt.wait () in
          let p' = p >|= fun s -> s ^ "bar" in
          Lwt.wakeup r "foo";
          state_is (Lwt.Return "foobar") p');
      test "=|<" (fun () ->
          let open Lwt.Infix in
          let p, r = Lwt.wait () in
          let p' = (fun s -> s ^ "bar") =|< p in
          Lwt.wakeup r "foo";
          state_is (Lwt.Return "foobar") p');
      test "<&>" (fun () ->
          let open Lwt.Infix in
          let p, r = Lwt.wait () in
          let p' = p <&> Lwt.return () in
          Lwt.wakeup r ();
          state_is (Lwt.Return ()) p');
      test "<?>" (fun () ->
          let open Lwt.Infix in
          let p1, r = Lwt.wait () in
          let p2, _ = Lwt.wait () in
          let p3 = p1 <?> p2 in
          Lwt.wakeup r ();
          state_is (Lwt.Return ()) p3);
    ]

let suites = suites @ [ infix_operator_tests ]

(* Like the infix operator tests, these just check that the necessary functions
   exist in Lwt.Infix.Let_syntax, and do roughly what they should. We are not
   testing the full syntax to avoid large dependencies for the test suite. *)
let ppx_let_tests =
  suite "ppx_let"
    [
      test "return" (fun () ->
          let p = Lwt.Let_syntax.Let_syntax.return () in
          state_is (Lwt.Return ()) p);
      test "map" (fun () ->
          let p =
            Lwt.Let_syntax.Let_syntax.map (Lwt.return 1) ~f:(fun x -> x + 1)
          in
          state_is (Lwt.Return 2) p);
      test "bind" (fun () ->
          let p =
            Lwt.Let_syntax.Let_syntax.bind (Lwt.return 1) ~f:(fun x ->
                Lwt.return (x + 1))
          in
          state_is (Lwt.Return 2) p);
      test "both" (fun () ->
          let p = Lwt.both (Lwt.return 1) (Lwt.return 2) in
          state_is (Lwt.Return (1, 2)) p);
      test "Open_on_rhs" (fun () ->
          let module Local = struct
            module type Empty = sig end
          end in
          let x : (module Local.Empty) =
            (module Lwt.Let_syntax.Let_syntax.Open_on_rhs)
          in
          ignore x;
          Lwt.return true);
    ]

let suites = suites @ [ ppx_let_tests ]

let let_syntax_tests =
  suite "let syntax"
    [
      test "let*" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p' =
            let open Lwt.Syntax in
            let* s1 = p1 in
            let* s2 = p2 in
            Lwt.return (s1 ^ s2)
          in
          Lwt.wakeup r1 "foo";
          Lwt.wakeup r2 "bar";
          state_is (Lwt.Return "foobar") p');
      test "and*" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p' =
            let open Lwt.Syntax in
            let* s1 = p1 and* s2 = p2 in
            Lwt.return (s1 ^ s2)
          in
          Lwt.wakeup r1 "foo";
          Lwt.wakeup r2 "bar";
          state_is (Lwt.Return "foobar") p');
      test "let+/and+" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p' =
            let open Lwt.Syntax in
            let+ s1 = p1 and+ s2 = p2 in
            s1 ^ s2
          in
          Lwt.wakeup r1 "foo";
          Lwt.wakeup r2 "bar";
          state_is (Lwt.Return "foobar") p');
    ]

let suites = suites @ [ let_syntax_tests ]

(* Tests for [Lwt.add_task_l] and [Lwt.add_task_r]. *)

let lwt_sequence_contains sequence list =
  let step item ((contains_so_far, list_tail) as state) =
    if not contains_so_far then state
    else
      match list_tail with
      | item' :: rest -> (item == item', rest)
      | [] -> failwith "Sequence and list not of the same length"
  in
  fst (Lwt_sequence.fold_l step sequence (true, list))

let lwt_sequence_tests =
  suite "add_task_l and add_task_r"
    [
      test "add_task_r" (fun () ->
          let sequence = Lwt_sequence.create () in
          let p = (Lwt.add_task_r [@ocaml.warning "-3"]) sequence in
          let p' = (Lwt.add_task_r [@ocaml.warning "-3"]) sequence in
          assert (Lwt.state p = Lwt.Sleep);
          assert (lwt_sequence_contains sequence [ Obj.magic p; Obj.magic p' ]);
          Lwt.cancel p;
          Lwt.return
            (Lwt.state p = Lwt.Fail Lwt.Canceled
            && lwt_sequence_contains sequence [ Obj.magic p' ]));
      test "add_task_l" (fun () ->
          let sequence = Lwt_sequence.create () in
          let p = (Lwt.add_task_l [@ocaml.warning "-3"]) sequence in
          let p' = (Lwt.add_task_l [@ocaml.warning "-3"]) sequence in
          assert (Lwt.state p = Lwt.Sleep);
          assert (lwt_sequence_contains sequence [ Obj.magic p'; Obj.magic p ]);
          Lwt.cancel p;
          Lwt.return
            (Lwt.state p = Lwt.Fail Lwt.Canceled
            && lwt_sequence_contains sequence [ Obj.magic p' ]));
    ]

let suites = suites @ [ lwt_sequence_tests ]

let pause_tests =
  suite "pause"
    [
      test "initial state" (fun () -> Lwt.return (Lwt.paused_count () = 0));
      test "one promise" (fun () ->
          let p = Lwt.pause () in
          assert (Lwt.paused_count () = 1);
          Lwt.bind (state_is Lwt.Sleep p) (fun initial_state_correct ->
              Lwt.wakeup_paused ();
              assert (Lwt.paused_count () = 0);
              Lwt.bind (state_is (Lwt.Return ()) p) (fun final_state_correct ->
                  Lwt.return (initial_state_correct && final_state_correct))));
      test "multiple promises" (fun () ->
          let p1 = Lwt.pause () in
          let p2 = Lwt.pause () in
          assert (Lwt.paused_count () = 2);
          Lwt.bind (state_is Lwt.Sleep p1) (fun initial_state_correct_1 ->
              Lwt.bind (state_is Lwt.Sleep p2) (fun initial_state_correct_2 ->
                  Lwt.wakeup_paused ();
                  assert (Lwt.paused_count () = 0);
                  Lwt.bind (state_is (Lwt.Return ()) p1)
                    (fun final_state_correct_1 ->
                      Lwt.bind (state_is (Lwt.Return ()) p2)
                        (fun final_state_correct_2 ->
                          Lwt.return
                            (initial_state_correct_1
                            && initial_state_correct_2
                            && final_state_correct_1
                            && final_state_correct_2))))));
      test "wakeup with no promises" (fun () ->
          assert (Lwt.paused_count () = 0);
          Lwt.wakeup_paused ();
          assert (Lwt.paused_count () = 0);
          Lwt.return true);
      test "pause notifier" (fun () ->
          let seen = ref None in
          Lwt.register_pause_notifier (fun count -> seen := Some count);
          Lwt.pause () |> ignore;
          assert (Lwt.paused_count () = 1);
          assert (!seen = Some 1);
          Lwt.wakeup_paused ();
          Lwt.register_pause_notifier ignore;
          Lwt.return true);
      test "pause in unpause" (fun () ->
          let p1 = Lwt.pause () in
          (* let p2 = ref return_unit in *)
          Lwt.bind p1 (fun () -> Lwt.pause ()) |> ignore;
          assert (Lwt.paused_count () = 1);
          Lwt.wakeup_paused ();
          later (fun () ->
              assert (Lwt.paused_count () = 1);
              Lwt.wakeup_paused ();
              true));
      test "recursive pause in notifier" (fun () ->
          Lwt.register_pause_notifier (fun _count ->
              (* This will be called in response to a call to [Lwt.pause ()], so we can
                 expect one paused promise to already be in the queue. *)
              assert (Lwt.paused_count () = 1);
              Lwt.register_pause_notifier ignore;
              Lwt.pause () |> ignore);
          Lwt.pause () |> ignore;
          assert (Lwt.paused_count () = 2);
          Lwt.wakeup_paused ();
          Lwt.return true);
      test "unpause in pause" (fun () ->
          Lwt.register_pause_notifier (fun _count ->
              assert (Lwt.paused_count () = 1);
              Lwt.wakeup_paused ());
          Lwt.pause () |> ignore;
          assert (Lwt.paused_count () = 0);
          Lwt.register_pause_notifier ignore;
          Lwt.return true);
    ]

let suites = suites @ [ pause_tests ]

(* [Lwt.apply] and [Lwt.wrapN]. *)
let lift_tests =
  suite "apply and wrap"
    [
      test "apply" (fun () ->
          let p = Lwt.apply (fun s -> Lwt.return (s ^ "bar")) "foo" in
          state_is (Lwt.Return "foobar") p);
      test "apply: raises" (fun () ->
          let p = Lwt.apply (fun () -> raise Exception) () in
          state_is (Lwt.Fail Exception) p);
      test "wrap" (fun () ->
          let p = Lwt.wrap (fun () -> "foo") in
          state_is (Lwt.Return "foo") p);
      test "wrap: raises" (fun () ->
          let p = Lwt.wrap (fun () -> raise Exception) in
          state_is (Lwt.Fail Exception) p);
      test "wrap1" (fun () ->
          let p = Lwt.wrap1 (fun x1 -> x1) 1 in
          state_is (Lwt.Return 1) p);
      test "wrap1: raises" (fun () ->
          let p = Lwt.wrap1 (fun _ -> raise Exception) () in
          state_is (Lwt.Fail Exception) p);
      test "wrap2" (fun () ->
          let p = Lwt.wrap2 (fun x1 x2 -> x1 + x2) 1 2 in
          state_is (Lwt.Return 3) p);
      test "wrap2: raises" (fun () ->
          let p = Lwt.wrap2 (fun _ _ -> raise Exception) () () in
          state_is (Lwt.Fail Exception) p);
      test "wrap3" (fun () ->
          let p = Lwt.wrap3 (fun x1 x2 x3 -> x1 + x2 + x3) 1 2 3 in
          state_is (Lwt.Return 6) p);
      test "wrap3: raises" (fun () ->
          let p = Lwt.wrap3 (fun _ _ _ -> raise Exception) () () () in
          state_is (Lwt.Fail Exception) p);
      test "wrap4" (fun () ->
          let p = Lwt.wrap4 (fun x1 x2 x3 x4 -> x1 + x2 + x3 + x4) 1 2 3 4 in
          state_is (Lwt.Return 10) p);
      test "wrap4: raises" (fun () ->
          let p = Lwt.wrap4 (fun _ _ _ _ -> raise Exception) () () () () in
          state_is (Lwt.Fail Exception) p);
      test "wrap5" (fun () ->
          let p =
            Lwt.wrap5 (fun x1 x2 x3 x4 x5 -> x1 + x2 + x3 + x4 + x5) 1 2 3 4 5
          in
          state_is (Lwt.Return 15) p);
      test "wrap5: raises" (fun () ->
          let p = Lwt.wrap5 (fun _ _ _ _ _ -> raise Exception) () () () () () in
          state_is (Lwt.Fail Exception) p);
      test "wrap6" (fun () ->
          let p =
            Lwt.wrap6
              (fun x1 x2 x3 x4 x5 x6 -> x1 + x2 + x3 + x4 + x5 + x6)
              1 2 3 4 5 6
          in
          state_is (Lwt.Return 21) p);
      test "wrap6: raises" (fun () ->
          let p =
            Lwt.wrap6 (fun _ _ _ _ _ _ -> raise Exception) () () () () () ()
          in
          state_is (Lwt.Fail Exception) p);
      test "wrap7" (fun () ->
          let p =
            Lwt.wrap7
              (fun x1 x2 x3 x4 x5 x6 x7 -> x1 + x2 + x3 + x4 + x5 + x6 + x7)
              1 2 3 4 5 6 7
          in
          state_is (Lwt.Return 28) p);
      test "wrap7: raises" (fun () ->
          let p =
            Lwt.wrap7
              (fun _ _ _ _ _ _ _ -> raise Exception)
              () () () () () () ()
          in
          state_is (Lwt.Fail Exception) p);
    ]

let suites = suites @ [ lift_tests ]

(* [Lwt.make_value] and [Lwt.make_error] are deprecated, but test them anyway,
   for good measure. *)
let make_value_and_error_tests =
  suite "make_value and make_error"
    [
      test "make_value" (fun () ->
          Lwt.return ((Lwt.make_value [@ocaml.warning "-3"]) 42 = Result.Ok 42));
      test "make_error" (fun () ->
          Lwt.return
            ((Lwt.make_error [@ocaml.warning "-3"]) Exception
            = Result.Error Exception));
    ]

let suites = suites @ [ make_value_and_error_tests ]

(* These tests exercise the callback cleanup mechanism of the Lwt core, which is
   an implementation detail. When a promise [p] is repeatedly used in functions
   such as [Lwt.choose], but remains pending, while other promises passed to
   [Lwt.choose] resolve, [p] accumulates disabled callback cells. They need to
   be occasionally cleaned up; in particular, this should happen every
   [callback_cleanup_point] [Lwt.choose] operations.

   As an extra twist, if [f] in [p' = Lwt.bind _ f] returns a pending promise
   [p], that pending promise's callback cells, including the disabled ones, are
   appended to the callback cells of [p']. If the sum of [Lwt.choose] operations
   performed on [p] and [p'] is more than [callback_cleanup_point], disabled
   callback cells also need to be cleaned up on [p'].

   The tests below callback the cleanup code, and make sure that non-disabled
   callback cells survive the cleanup. *)

let callback_cleanup_point = 42

let callback_list_tests =
  suite "callback cleanup"
    [
      test "choose" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2 = Lwt.bind p1 (fun s -> Lwt.return (s ^ "bar")) in
          let p3 = Lwt.choose [ p1; fst (Lwt.wait ()) ] in
          let rec repeat = function
            | 0 -> ()
            | n ->
                let p4, r4 = Lwt.wait () in
                Lwt.choose [ p1; p4 ] |> ignore;
                Lwt.wakeup r4 "";
                repeat (n - 1)
          in
          repeat (callback_cleanup_point + 1);
          Lwt.wakeup r1 "foo";
          Lwt.return
            (Lwt.state p2 = Lwt.Return "foobar"
            && Lwt.state p3 = Lwt.Return "foo"));
      test "bind" (fun () ->
          let p1, r1 = Lwt.wait () in
          let p2, r2 = Lwt.wait () in
          let p3 = Lwt.bind p1 (fun () -> p2) in
          let p4 = Lwt.map ignore p2 in
          let p5 = Lwt.map ignore p3 in
          let rec repeat = function
            | 0 -> ()
            | n ->
                let p6, r6 = Lwt.wait () in
                Lwt.choose [ p2; p3; p6 ] |> ignore;
                Lwt.wakeup r6 ();
                repeat (n - 1)
          in
          repeat ((callback_cleanup_point / 2) + 1);
          Lwt.wakeup r1 ();
          Lwt.wakeup r2 ();
          Lwt.return
            (Lwt.state p4 = Lwt.Return () && Lwt.state p5 = Lwt.Return ()));
    ]

let suites = suites @ [ callback_list_tests ]

(*

   Lwt should preserve tail-recursion. When a recursive function [f]
   lives in the [Lwt] monad, a programmer must assume that [f] is only
   call in tail position in [Lwt.bind].

   Given that robustness to stack overflows cannot be ignored by OCaml
   programmers, this property should be documented, be part of
   [Lwt] specification, and probably never be broken in the future.

   This test tries to ensure that [Lwt.bind x f] indeed calls [f] only
   in tail position.

*)

let tailrec_tests =
  suite "tailrec"
    [
      test "tailrec" (fun () ->
          let rec aux f accu n =
            if n = 0 then Lwt.return accu
            else Lwt.bind (f n) (fun s -> aux f (s + accu) (n - 1))
          in
          let f n = Lwt.return n in
          try
            ignore (aux f 0 10000000);
            Lwt.return true
          with _ -> Lwt.return false);
    ]

let suites = suites @ [ tailrec_tests ]
