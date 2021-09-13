(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(** This module allows to mix multicore parallelism with [Lwt]
    cooperative threads. It maintains an extensible pool of domains
    to which you can detach computations. *)

    val detach : ('a -> 'b) -> 'a -> 'b Lwt.t
    (** [detach f x] runs the computation [f x] in a separate domain in parallel.
        [detach] evaluates to an Lwt promise, which is pending until the
        domain completes execution.
        [detach] calls {!simple_init} internally, which means that the number of
        domains is capped by default at four. If you would like a
        higher limit, call {!init} or {!set_bounds} directly.
        Note that Lwt thread-local storage (i.e., {!Lwt.with_value}) cannot be
        safely used from within [f]. The same goes for most of the rest of Lwt. If
        you need to run an Lwt thread in [f], use {!run_in_main}. *)

  val run_in_main : (unit -> 'a Lwt.t) -> 'a
    (** [run_in_main f] can be called from a detached computation to execute
        [f ()] in the parent domain, i.e. the one executing
        {!Lwt_main.run}. [run_in_main f] blocks until [f ()] completes, then
        returns its result. If [f ()] raises an exception, [run_in_main f] raises
        the same exception.
        {!Lwt.with_value} may be used inside [f ()]. {!Lwt.get} can correctly
        retrieve values set this way inside [f ()], but not values set using
        {!Lwt.with_value} outside [f ()]. *)

  val init : int -> (string -> unit) -> unit
    (** [init num log] initialises this module. i.e. it launches the
        num number of domains and starts the {b
        dispatcher}.
        @param min is the minimum number of domains
        @param max is the maximum number of domains
        @param log is used to log error messages
        If {!Lwt_preemptive} has already been initialised, this call
        only modify bounds and the log function. *)

  val simple_init : unit -> unit
  (** [simple_init ()] checks if the library is not yet initialized, and if not,
      does a {i simple initialization}. The number of domains is set to four,
      and the log function is left unchanged, i.e. the default built-in
      logging function is used. See {!Lwt_preemptive.init}. Note: this
      function is automatically called by {!detach}. *)

  val get_bounds : unit -> int
    (** [get_bounds ()] returns the number of domains. *)

  val set_bounds : int -> unit
    (** [set_bounds num] sets the number of domains. *)

  (**/**)
  val nbdomains : unit -> int