(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(** Interaction with the garbage collector *)

(** This module offers a convenient way to add a finaliser launching a
    thread to a value, without having to use [Lwt_unix.run] in the
    finaliser. *)

val finalise : ?domain:Domain.id -> ('a -> unit Lwt.t) -> 'a -> unit
  (** [finalise f x] ensures [f x] is evaluated after [x] has been
      garbage collected. If [f x] yields, then Lwt will wait for its
      termination at the end of the program.

      Note that [f x] is not called at garbage collection time, but
      later in the main loop.

      If [domain] is provided, then [f x] is evaluated in the corresponding
      domain. Otherwise it is evaluated in the domain calling [finalise]. If
      Lwt is not running in the domain set to run the finaliser, an
      unspecified error occurs at an unspecified time or the finaliser doesn't
      run or some other bad thing happens. *)

val finalise_or_exit : ('a -> unit Lwt.t) -> 'a -> unit
  (** [finalise_or_exit f x] call [f x] when [x] is garbage collected
      or (exclusively) when the program exits.

      The finaliser [f] is called in the same domain that called
      [finalise_or_exit]. If there is no Lwt scheduler running in this domain an
      unspecified error occurs. You can use [Lwt_preemptive.run_in_domain] to
      bypass the same-domain limitation. *)
