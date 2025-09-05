(** Direct style control flow for Lwt.

    Using this module you can write code in direct style (using loops,
    exceptions handlers, etc.) in an Lwt codebase. Your direct-style sections
    must be enclosed in a call to {!spawn} and they may {!await} on promises.
    For example:

    {[
    open Lwt_direct
    spawn (fun () ->
      let continue = ref true in
      while !continue do
        match await @@ Lwt_io.read_line in_channel with
        | exception End_of_file -> continue := false
        | line ->
          let uppercase_line = String.uppercase_ascii line in
          await @@ Lwt_io.write_line out_channel uppercase_line
      done)
    ]}

    In this code snippet, the [while]-loop repeats a simple task of reading from
    an {!Lwt_io.channel}, modifying it, and writing it to a different channel.
    The code is in direct-style: the control structures are standard OCaml
    without any Lwt primitives.

    The code-snippet as a whole is a [unit Lwt.t] promise. It becomes resovled
    when the function returns. Conversely, the promises inside the snippet are
    wrapped in {!await}, turning them into regular plain (non-Lwt) values
    (although values that are not available immediately).

    The [Lwt_direct] module is implemented using OCaml 5's
    {{:https://ocaml.org/manual/5.3/effects.html} effects and effect handlers}.
    This allows the kind of scheduling where a promise is turned into a regular
    value and vice-versa. *)

val spawn : (unit -> 'a) -> 'a Lwt.t
(** [spawn f] runs the function [f ()], it also returns a promise [p] which is
    resolved when the call to [f ()] returns a value. If [f ()] throws an
    exception, the promise [p] is rejected.

    The function [f] can create Lwt promises (e.g., by calling functions from
    [Lwt_io], [Lwt_unix], or third-party libraries) and use {!await} to wait for
    them. These promises are evaluated in the Lwt event loop.

    Like any promise in Lwt, [f ()] can starve the event loop if it runs long
    computations without yielding to the event loop.

    Cancelling the promise returned by [spawn] has no effect: the execution of
    [f ()] continues and the promise is not cancelled.

    When [f ()] terminates (successfully or not), the promise
    [spawn f] is resolved with [f ()]'s result, or the exception
    raised by [f ()]. *)

val spawn_in_the_background :
  (unit -> unit) ->
  unit
(** [spawn_in_the_background f] is similar to [ignore (spawn f)].
    The computation [f ()] runs in the background in the event loop
    and returns no result.

    If [f()] raises an exception, {!Lwt.async_exception_hook} is called. *)

val yield : unit -> unit
(** Yield to the event loop.

    This is similar to [await (Lwt.pause ())], using less indirection internally
    and fewer characters to write.

    Calling [yield] outside of {!spawn} or {!spawn_in_the_background} will raise
    an exception, crash your program, or otherwise cause errors. It is a
    programming error to do so. *)

val await : 'a Lwt.t -> 'a
(** [await p] returns the result of [p] (or raises the exception with which [p]
    was rejected.

    If [p] is not resolved yet, [await p] will suspend the current task (i.e.,
    the computation started by the surrounding {!spawn}) and resume it when [p]
    is resolved.

    Calling [await] outside of {!spawn} or {!spawn_in_the_background} will raise
    an exception, crash your program, or otherwise cause errors. It is a
    programming error to do so. *)

(** Local storage.

    This storage is the same as the one described with {!Lwt.key},
    except that it is usable from the inside of {!spawn} or
    {!spawn_in_the_background}.

    Each task has its own storage, independent from other tasks or promises.

    NOTE: it is recommended to use [Lwt_direct.Storage] functions rather than
    [Lwt.key] functions from {!Lwt}. The latter is deprecated. *)
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
