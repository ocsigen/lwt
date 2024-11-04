(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



open Test
open Lwt.Infix
open Lwt.Syntax

module Retry = Lwt_retry

let pp = Retry.pp_error ~retry:Format.pp_print_float ~fatal:Format.pp_print_int

let suite = suite "lwt_retry" [
    test_direct "can format retries outcomes"
      (fun () ->
         Format.asprintf "%a" pp (`Retry 3.0) = "`Retry 3.");

    test_direct "can format fatal outcomes"
      (fun () ->
         Format.asprintf "%a" pp (`Fatal 42) = "`Fatal 42");

    test_direct "can format with default printer"
      (fun () ->
         Format.asprintf "%a" (fun x -> Retry.pp_error x) (`Fatal 42)
         =
         "`Fatal <opaque>");

    test "success without retry"
      (fun () ->
        let strm =
          Retry.on_error (fun () -> Lwt.return_ok 42)
        in
        let* actual = Lwt_stream.next strm in
        assert (actual = Ok 42);
        (* ensure the post condition of an empty stream *)
        Lwt_stream.is_empty strm);

    test "does not run extra attempts"
      (fun () ->
         let count = ref 0 in
         let strm =
           Retry.on_error (fun () ->
               incr count;
               Lwt.return_ok 42)
         in
         let* actual = Lwt_stream.next strm in
         assert (actual = Ok 42);
         (* Force another attempt on the stream *)
         let+ _ = Lwt_stream.is_empty strm in
         (* We should have run 1 and only 1 attempt,
            or else the execution logic is wrong. *)
         !count = 1);

    test "just retries" (fun () ->
        let strm =
          Retry.on_error (fun () -> Lwt.return_error (`Retry ()))
        in
        let retry_attempts = 5 in
        let expected_retries = List.init retry_attempts (fun i -> Error (`Retry (), i + 1)) in
        let+ actual_retries = Lwt_stream.nget retry_attempts strm in
        actual_retries = expected_retries);

    test "retries before fatal error" (fun () ->
        let retries_before_fatal = 3 in
        let i = ref 0 in
        let strm = Retry.on_error
            (fun () ->
               if !i < retries_before_fatal then (
                 incr i;
                 Lwt.return_error (`Retry ())
               ) else
                 Lwt.return_error (`Fatal ()))
        in
        let* n_retry_errors = Lwt_stream.nget retries_before_fatal strm >|= List.length in
        assert (n_retry_errors = retries_before_fatal);
        let* fatal_error = Lwt_stream.next strm in
        assert (fatal_error = Error (`Fatal (), retries_before_fatal + 1));
        (* ensure the post condition of an empty stream *)
        Lwt_stream.is_empty strm);

    test "retries before success" (fun () ->
        let retries_before_fatal = 3 in
        let i = ref 0 in
        let strm = Retry.on_error (fun () ->
            if !i < retries_before_fatal then (
              incr i;
              Lwt.return_error (`Retry ())
            ) else
              Lwt.return_ok ()
          )
        in
        let* n_retry_errors = Lwt_stream.nget retries_before_fatal strm >|= List.length in
        assert (n_retry_errors = retries_before_fatal);
        let* success = Lwt_stream.next strm in
        assert (success = Ok ());
        (* ensure the post condition of an empty stream *)
        Lwt_stream.is_empty strm);

    test "[n_times 0] runs one attempt" (fun () ->
        let operation () = Lwt.return_error (`Retry ()) in
        let+ attempt = Retry.(operation |> on_error |> n_times 0) in
        attempt = Error (`Retry (), 1));

    test "n_times gives up on a fatal error" (fun () ->
        let i = ref 0 in
        let operation () =
          if !i < 3 then (
            incr i;
            Lwt.return_error (`Retry ())
          ) else
            Lwt.return_error (`Fatal ())
        in
        let+ fatal_error = Retry.(operation |> on_error |> n_times 5) in
        fatal_error = Error (`Fatal (), 4));

    test "n_times gives a retry error when exhausted"  (fun () ->
        let retries = 5 in
        let operation () = Lwt.return_error (`Retry ()) in
        let+ result = Retry.(operation |> on_error |> n_times retries) in
        result = Error (`Retry (), retries + 1));

    test "n_times is ok on success" (fun () ->
        let i = ref 0 in
        let operation () =
          if !i < 3 then (
            incr i;
            Lwt.return_error (`Retry ())
          ) else
            Lwt.return_ok ()
        in
        let+ success = Retry.(operation |> on_error |> n_times 5) in
        success = Ok ());

    test_direct "n_times on negative raises Invalid_argument"  (fun () ->
        let invalid_negative_retries = -5 in
        let operation () = Lwt.return_error (`Retry ()) in
        let attempts = Retry.(operation |> on_error) in
        try
          let _ = Retry.(attempts |> n_times invalid_negative_retries) in
          false (* We failed to raise the invalid argument exception *)
        with
          Invalid_argument _ -> true);

    (* test that the sleeps actually throttle computations as desired *)
    test "with_sleep really does sleep"  (fun ()  ->
      let duration _ = 0.01 in
      let operation () = Lwt.return_error (`Retry ()) in
      (* If [with_sleep] is removed the test fails, as expected *)
      let retries = Retry.(operation |> on_error |> with_sleep ~duration |> n_times 5) in
      (* We will expect the [racing_operation] to complete before the retries with_sleep *)
      let racing_operation = Lwt_unix.sleep (duration ()) >|= Result.ok in
      let+ actual = Lwt.choose [racing_operation; retries] in
      actual = Ok ());
  ]

let () =
  Test.run "retry" [suite]
