(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(** Main loop and event queue *)

(** This module controls the ``main-loop'' of Lwt. *)

val run : 'a Lwt.t -> 'a
  (** [Lwt_main.run p] calls the Lwt scheduler, performing I/O until [p]
      resolves. [Lwt_main.run p] returns the value in [p] if [p] is fulfilled.
      If [p] is rejected with an exception instead, [Lwt_main.run p] raises that
      exception.

      Every native and bytecode program that uses Lwt should call this function
      at its top level. It implements the Lwt main loop.

      Example:
      {[
let main () = Lwt_io.write_line Lwt_io.stdout "hello world"

let () = Lwt_main.run (main ())
      ]}

      [Lwt_main.run] is not available when targeting JavaScript, because the
      environment (such as Node.js or the browser's script engine) implements
      the I/O loop.

      On Unix, calling [Lwt_main.run] installs a [SIGCHLD] handler, which is
      needed for the implementations of {!Lwt_unix.waitpid} and
      {!Lwt_unix.wait4}. As a result, programs that call [Lwt_main.run] and also
      use non-Lwt system calls need to handle those system calls failing with
      [EINTR].

      Nested calls to [Lwt_main.run] are not allowed. That is, do not call
      [Lwt_main.run] in a callback triggered by a promise that is resolved by
      an outer invocation of [Lwt_main.run]. If your program makes such a call,
      [Lwt_main.run] will raise [Failure]. This should be considered a logic
      error (i.e., code making such a call is inherently broken).

      In addition, note that if you have set the exception filter to let runtime
      exceptions bubble up (via
      [Lwt.Exception_filter.(set handle_all_except_runtime)])
      then Lwt does not attempt to catch exceptions thrown by the OCaml runtime.
      Specifically, in this case, Lwt lets [Out_of_memory] and [Stack_overflow]
      exceptions traverse all of its functions and bubble up to the caller of
      [Lwt_main.run]. Moreover because these exceptions are left to traverse the
      call stack, they leave the internal data-structures in an inconsistent
      state. For this reason, calling [Lwt_main.run] again after such an
      exception will raise [Failure].

      It is not safe to call [Lwt_main.run] in a function registered with
      [Stdlib.at_exit], use {!Lwt_main.at_exit} instead. *)

val yield : unit -> unit Lwt.t [@@deprecated "Use Lwt.pause instead"]
  (** [yield ()] is a pending promise that is fulfilled after Lwt finishes
      calling all currently ready callbacks, i.e. it is fulfilled on the next
      “tick.”

      @deprecated Since 5.5.0 [yield] is deprecated in favor of the more general
      {!Lwt.pause} in order to avoid discrepancies in resolution (see below) and
      stay compatible with other execution environments such as js_of_ocaml. *)

val abandon_yielded_and_paused : unit -> unit [@@deprecated "Use Lwt.abandon_paused instead"]
(** Causes promises created with {!Lwt.pause} and {!Lwt_main.yield} to remain
    forever pending.

    (Note that [yield] is deprecated in favor of the more general {!Lwt.pause}.)

    This is meant for use with {!Lwt_unix.fork}, as a way to “abandon” more
    promise chains that are pending in your process.

    @deprecated Since 5.7 [abandon_yielded_and_paused] is deprecated in favour
    of [Lwt.abandon_paused]. *)



(** Hook sequences. Each module of this type is a set of hooks, to be run by Lwt
    at certain points during execution.

    Hooks are added for the current domain. If you are calling the Hook
    functions from a domain where Lwt is not running a scheduler then some
    unspecified error may occur. If you need to set some Hooks to/from a
    different domain, you can use [Lwt_preemptive.run_in_domain].

    See modules {!Enter_iter_hooks}, {!Leave_iter_hooks}, and {!Exit_hooks}. *)
module type Hooks =
sig
  type 'return_value kind
  (** Hooks are functions of either type [unit -> unit] or [unit -> unit Lwt.t];
      this type constructor is used only to express both possibilities in one
      signature. *)

  type hook
  (** Values of type [hook] represent hooks that have been added, so that they
      can be removed later (if needed). *)

  val add_first : (unit -> unit kind) -> hook
  (** Adds a hook to the hook sequence underlying this module, to be run
      {e first}, before any other hooks already added. *)

  val add_last : (unit -> unit kind) -> hook
  (** Adds a hook to the hook sequence underlying this module, to be run
      {e last}, after any other hooks already added. *)

  val remove : hook -> unit
  (** Removes a hook added by {!add_first} or {!add_last}. *)

  val remove_all : unit -> unit
  (** Removes all hooks from the hook sequence underlying this module. *)
end

(** Hooks, of type [unit -> unit], that are called before each iteration of the
    Lwt main loop.

    @since 4.2.0 *)
module Enter_iter_hooks :
  Hooks with type 'return_value kind = 'return_value

(** Hooks, of type [unit -> unit], that are called after each iteration of the
    Lwt main loop.

    @since 4.2.0 *)
module Leave_iter_hooks :
  Hooks with type 'return_value kind = 'return_value

(** Promise-returning hooks, of type [unit -> unit Lwt.t], that are called at
    process exit. Exceptions raised by these hooks are ignored.

    @since 4.2.0 *)
module Exit_hooks :
  Hooks with type 'return_value kind = 'return_value Lwt.t

val at_exit : (unit -> unit Lwt.t) -> unit
(** [Lwt_main.at_exit hook] is the same as
    [ignore (Lwt_main.Exit_hooks.add_first hook)]. *)
