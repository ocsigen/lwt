(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(** Utilities for retrying Lwt computations

    These utilities are useful for dealing with failure-prone computations that
    are expected to succeed after some number of repeated attempts. E.g.,

    {[
      let flaky_computation () = match try_to_get_resource () with
        | Flaky_error msg -> Error (`Retry msg)
        | Fatal_error err -> Error (`Fatal err)
        | Success  result -> Ok result

      let error_tolerant_computation () =
        Lwt_retry.(flaky_computation
                   |> on_error    (* Retry when [`Retry]able results are produced. *)
                   |> with_sleep  (* Add a delay between attempts, with an exponential backoff. *)
                   |> n_times 10  (* Try up to 10 times, so long as errors are retryable. *)
                  )
    ]}

    This library provides a few combinators, but retry attempts are produced on
    demand in an {!type:Lwt_stream.t}, and they can be consumed and traversed
    using the {!module:Lwt_stream} functions directly. *)

type ('retry, 'fatal) error =
  [ `Retry of 'retry
  | `Fatal of 'fatal
  ]
(** The type of errors that a retryable computation can produce.

    - [`Retry r] when [r] represents an error that can be retried.
    - [`Fatal f] when [f] represents an error that cannot be retried. *)

type ('ok, 'retry, 'fatal) attempt = ('ok, ('retry, 'fatal) error * int) result
(** A [('ok, 'retry, 'fatal) attempt] is the [result] of a retryable computation,
    with its the erroneous results enumerated.

    - [Ok v] is a successfully computed value [v]
    - [Error (err, n)] is the {!type:error} [err] produced on the [n]th
      attempt

    The enumeration of attempts is 1-based, because making 0 attempts means
    making no attempts all, making 1 attempt means {i trying} once, and (when
    [i>0]) making [n] attempts means trying once and then {i retrying} up to
    [n-1] times. *)

val pp_error :
  ?retry:(Format.formatter -> 'retry -> unit) ->
  ?fatal:(Format.formatter -> 'fatal -> unit) ->
  Format.formatter -> ('retry, 'fatal) error -> unit
(** [pp_error ~retry ~fatal] is a pretty printer for {!type:error}s that formats
    fatal and retryable errors according to the provided printers.

    If a printers is not provided, values of the type are represented as
    ["<opaque>"]. *)

val equal_error :
  retry:('retry -> 'retry -> bool) ->
  fatal:('fatal -> 'fatal -> bool) ->
  ('retry, 'fatal) error ->
  ('retry, 'fatal) error ->
  bool

val on_error :
  (unit -> ('ok, ('retry, 'fatal) error) result Lwt.t) ->
  ('ok, 'retry, 'fatal) attempt Lwt_stream.t
(** [Lwt_retry.on_error f] is a stream of attempts to compute [f], with attempts
    made on demand. Attempts will be added to the stream when results are
    requested until the computation either succeeds or produces a fatal error.

    Examples

    {[
      # let success () = Lwt.return_ok ();;
      val success : unit -> (unit, 'a) result Lwt.t = <fun>
      # Lwt_retry.(success |> on_error) |> Lwt_stream.to_list;;
      - : (unit, 'a, 'b) Lwt_retry.attempt list = [Ok ()]

      # let fatal_failure () = Lwt.return_error (`Fatal ());;
      val fatal_failure : unit -> ('a, [> `Fatal of unit ]) result Lwt.t = <fun>
      # Lwt_retry.(fatal_failure |> on_error) |> Lwt_stream.to_list;;
      - : ('a, 'b, unit) Lwt_retry.attempt list = [Error (`Fatal (), 1)]

      # let retryable_error () = Lwt.return_error (`Retry ());;
      val retryable_error : unit -> ('a, [> `Retry of unit ]) result Lwt.t = <fun>
      # Lwt_retry.(retryable_error |> on_error) |> Lwt_stream.nget 3;;
      - : ('a, unit, 'b) Lwt_retry.attempt list =
      [Error (`Retry (), 1); Error (`Retry (), 2); Error (`Retry (), 3)]
    ]}*)

val with_sleep :
  ?duration:(int -> float) ->
  ('ok, 'retry, 'fatal) attempt Lwt_stream.t ->
  ('ok, 'retry, 'fatal) attempt Lwt_stream.t
(** [with_sleep ~duration attempts] is the stream of [attempts] with a sleep of
    [duration n] seconds added before computing each [n]th retryable attempt.

    @param duration the optional sleep duration calculation, defaulting to
    {!val:default_sleep_duration}.

    Examples

    {[
      # let f () = Lwt.return_error (`Retry ());;
      # let attempts_with_sleeps = Lwt_retry.(f |> on_error |> with_sleep);;

      # Lwt_stream.get attempts_with_sleeps;;
      (* computed immediately *)
      Some (Error (`Retry (), 1))

      # Lwt_stream.get attempts_with_sleeps;;
      (* computed after 3 seconds *)
      Some (Error (`Retry (), 2))

      # Lwt_stream.get attempts_with_sleeps;;
      (* computed after 9 seconds *)
      Some (Error (`Retry (), 3))

      (* a stream with a constant 1s sleep between attempts *)
      # let attempts_with_constant_sleeps =
          Lwt_retry.(f |> on_error |> with_sleep ~duration:(fun _ -> 1.0));;
    ]} *)

val default_sleep_duration : int -> float
(** [default_sleep_duration n] is an exponential backoff computed as [n] * 2 *
    (2 ^ [n]), which gives the sequence [ [0.; 4.; 16.; 48.; 128.; 320.; 768.;
    1792.; ...] ]. *)

val n_times :
  int ->
  ('ok, 'retry, 'fatal) attempt Lwt_stream.t ->
  ('ok, 'retry, 'fatal) attempt Lwt.t
(** [n_times n attempts] is [Ok v] if one of the [attempts] succeeds within [n]
    retries (or [n+1] attempts), [Error (`Fatal f, n+1)] if any of the attempts
    results in the fatal error, or [Error (`Retry r, n+1)] if all [n] retries are
    exhausted and the [n+1]th attempt results in a retry error.

    In particular [n_times 0 attempts] will *try* 1 attempt but *re-try* 0, so
    it is guaranteed to produce some result.

    [n_times] forces up to [n] elements of the on-demand stream of attempts.

    Examples

    {[
      # let f () =
          let i = ref 0 in
          fun () -> Lwt.return_error (if !i < 3 then (incr i; `Retry ()) else `Fatal "error!");;
      # Lwt_retry.(f () |> on_error |> n_times 0);;
      Error (`Retry (), 1)
      # Lwt_retry.(f () |> on_error |> n_times 4);;
      Error (`Fatal "error!", 3)
    ]} *)
