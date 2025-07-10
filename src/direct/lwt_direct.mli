(** Direct style control flow for Lwt. *)

val run : (unit -> 'a) -> 'a Lwt.t
(** [run f] runs the function [f ()] in a task within
    the [Lwt_unix] event loop. [f ()] can create [Lwt]
    promises and use {!await} to wait for them. Like any promise
    in Lwt, [f ()] can starve the event loop if it runs long computations
    without yielding to the event loop.

    When [f ()] terminates (successfully or not), the promise
    [run f] is resolved with [f ()]'s result, or the exception
    raised by [f ()]. *)

val run_in_the_background :
  ?on_uncaught_exn:(exn -> Printexc.raw_backtrace -> unit) ->
  (unit -> unit) ->
  unit
(** [run_in_the_background f] is similar to [ignore (run f)].
    The computation [f()] runs in the background in the event loop
    and returns no result.
    @param on_uncaught_exn if provided, this is called when [f()]
    raises an exception. *)

val yield : unit -> unit
(** Yield to the event loop.
    calling [yield] outside of {!run} or {!run_in_the_background} will raise an exception,
    crash your program, or otherwise cause errors. It is a programming error to do so. *)

val await : 'a Lwt.t -> 'a
(** [await prom] returns the result of [prom], or re-raises the
    exception with which [prom] failed if it failed.
    If [prom] is not resolved yet, [await prom] will suspend the
    current task and resume it when [prom] is resolved.
    calling [yield] outside of {!run} or {!run_in_the_background} will raise an exception,
    crash your program, or otherwise cause errors. It is a programming error to do so. *)
