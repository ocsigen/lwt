(** Direct style control flow for Lwt.

    This module relies on OCaml 5's
    {{:https://ocaml.org/manual/5.3/effects.html} effect handlers}.
    Instead of chaining promises using {!Lwt.bind} and {!Lwt.map}
    and other combinators, it becomes possible to start
    lightweight "tasks" using [Lwt_direct.run (fun () -> ...)].
    The body of such a task is written in direct-style code,
    using OCaml's standard control flow structures such as loops,
    higher-order functions, exception handlers, [match], etc.

    Interactions with the rest of lwt can be done using [await],
    for example:

    {[
    Lwt_direct.run (fun () ->
      let continue = ref true in
      while !continue do
        match Lwt_io.read_line in_channel |> Lwt_direct.await with
        | exception End_of_file -> continue := false
        | line ->
          let uppercase_line = String.uppercase_ascii line in
          Lwt_io.write_line out_channel uppercase_line |> Lwt_direct.await
      done)
    ]}

    This code snippet contains a simple "task" that repeatedly reads
    a line from a [Lwt_io] channel, uppercases it, and writes the
    uppercase version to another channel.

    This task is itself a [unit Lwt.t], which is resolved when the function
    returns. It is possible to use
    {!Lwt_direct.run_in_the_background} to ignore the result and
    let the task run in the background instead.

    *)

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
  ?on_uncaught_exn:(exn -> unit) ->
  (unit -> unit) ->
  unit
(** [run_in_the_background f] is similar to [ignore (run f)].
    The computation [f()] runs in the background in the event loop
    and returns no result.
    @param on_uncaught_exn if provided, this is called when [f()]
    raises an exception. *)

val yield : unit -> unit
(** Yield to the event loop.

    Calling [yield] outside of {!run} or {!run_in_the_background} will raise an exception,
    crash your program, or otherwise cause errors. It is a programming error to do so. *)

val await : 'a Lwt.t -> 'a
(** [await prom] returns the result of [prom], or re-raises the
    exception with which [prom] failed if it failed.
    If [prom] is not resolved yet, [await prom] will suspend the
    current task and resume it when [prom] is resolved.

    Calling [await] outside of {!run} or {!run_in_the_background} will raise an exception,
    crash your program, or otherwise cause errors. It is a programming error to do so. *)

(** Local storage.

    This storage is the same as the one described with {!Lwt.key},
    except that it is usable from the inside of {!run} or
    {!run_in_the_background}.

    Each task has its own storage, independent from other tasks or promises. *)
module Storage : sig
  type 'a key = 'a Lwt.key
  val new_key : unit -> 'a key
  (** Alias to {!Lwt.new_key} *)

  val get : 'a key -> 'a option
  (** get the value associated with this key in local storage, or [None] *)

  val set : 'a key -> 'a -> unit
  (** [set k v] sets the key to the value for the rest of the task. *)

  val remove : 'a key -> unit
  (** Remove the value associated with this key, if any *)
end
