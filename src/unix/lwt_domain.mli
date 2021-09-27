(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(** This module provides the necessary function ({!detach}) to schedule some
    computations to be ran in parallel in a separate domain. The result of such
    a computation is exposed to the caller of {!detach} as a promise. Thus, this
    module allows to mix multicore parallelism with the concurrent-only
    scheduling of the rest of Lwt. *)

    val detach : ('a -> 'b) -> 'a -> 'b Lwt.t
    (** [detach f x] runs the computation [f x] in a separate domain in
        parallel.

        [detach f x] evaluates to an Lwt promise which is pending until the
        domain completes the execution of [f x] at which point it becomes
        resolved. If [f x] raises an exception, then the promise is rejected.

        If the task pool has not been initialised yet (see {!setup_pool}),
        then [detach] initializes it. The default number of domains is four (4).
        It is recommended you initialise the task pool using
        {!setup_pool} with a number of domains equal to the number of
        physical cores.

        Note that the function [f] passed to [detach] cannot safely use {!Lwt}.
        This is true even for implicit callback arguments (i.e.,
        {!Lwt.with_value}). If you need to use {!Lwt} or interact with promises,
        you must use {!run_in_main}. *)

  val run_in_main : (unit -> 'a Lwt.t) -> 'a
    (** [run_in_main f] can be called from a detached computation to execute [f
        ()] in the parent domain, i.e. the one executing {!Lwt_main.run}.

        [run_in_main f] blocks until [f ()] completes, then it returns its
        result. If [f ()] raises an exception, [run_in_main f] raises the same
        exception. The whole of {!Lwt} can be safely used from within [f].
        However, note that implicit callback arguments are local to [f]. I.e.,
        {!Lwt.get} can only retrieve values set inside of [f], and not those set
        inside the promise that called [detach] that called [run_in_main].

        Note that the calling domain will be idle until [f ()] completes
        execution and returns the result. Thus, heavy use of [run_in_main] may
        lead to most or all domains being frozen. It's also possible to create a
        dead-lock when [run_in_main] is called (thus freezing a domain) with a
        function that calls [detach] (thus needing a domain). Consequently, it
        is recommended to use this function sparingly. *)

  val setup_pool : int -> unit
  (** [setup_pool n] initializes the task pool with [n] domains. If it is called
      again, or if the task pool was already initialised by [detach], it
      raises an exception.

      It is recommended to use this function once before calling [Lwt_main.run]
      and to not call it again afterwards. To resize the pool, call
      [teardown_pool ()] first before calling [setup_pool] again. Multiple calls
      to [resize] the domain pool are safe but costly.

      For more details about task pool, please refer:
      https://github.com/ocaml-multicore/domainslib/blob/master/lib/task.mli

      @raise Invalid_argument if given number of domains [n] is smaller than [1]
      *)

  val teardown_pool : unit -> unit
  (** [teardown_pool ()] shuts down the task pool if it was initialized. Raises
      an exception if the task pool was not initialized.*)

  val get_num_domains : unit -> int
    (** [get_num_domains ()] returns the number of domains in the current task
        pool. *)

  (**/**)
  val nbdomains : unit -> int
