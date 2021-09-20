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
        higher limit, call {!set_num_domains} directly.
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

  val set_num_domains : int -> unit
  (** [set_num_domains n] initializes the library if not done, sets the number
      of domains to [n]. It shuts down any previously created domains. Hence, it
      should be used infrequently; ideally once in a program. *)

  val get_num_domains : unit -> int
    (** [get_num_domains ()] returns the number of domains. *)

  (**/**)
  val nbdomains : unit -> int