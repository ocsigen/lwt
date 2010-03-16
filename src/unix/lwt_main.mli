(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_main
 * Copyright (C) 2009 Jérémie Dimino
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exceptions;
 * either version 2.1 of the License, or (at your option) any later
 * version. See COPYING file for details.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *)

(** Main loop and event queue *)

(** This module controls the ``main-loop'' of Lwt. *)

val run : 'a Lwt.t -> 'a
  (** [run t] calls the Lwt scheduler repeatedly until [t] terminates,
      then returns the value returned by the thread. It [t] fails with
      an exception, this exception is raised.

      Note that you should avoid using [run] inside threads
      - The calling threads will not resume before [run]
        returns.
      - Successive invocations of [run] are serialized: an
        invocation of [run] will not terminate before all
        subsequent invocations are terminated. *)

val exit_hooks : (unit -> unit Lwt.t) Lwt_sequence.t
  (** Sets of functions executed just before the program exit.

      Notes:
      - each hook is called exactly one time
      - exceptions raised by hooks are ignored *)

val at_exit : (unit -> unit Lwt.t) -> unit
  (** [at_exit hook] adds hook at the left of [exit_hooks]*)

(** {6 Low-level control of event loop} *)

(** The rest of this file is for advanced users. You must have a good
    understanding on how Lwt works to use it.

    This part has two purpose:
    - integration of external libraries which are not ``Lwt-aware''
      but are ``main-loop'' aware, or more generally which support
      asynchronous IOs.
    - replace the default main-loop implementation (which is just
      a wrapper around select) by another one.

    Note that {!Lwt.wakeup_paused} must be called before collecting
    sources of event and before doing the blocking select or
    equivalent.
*)

type fd_set = Unix.file_descr list
    (** A set of file-descriptors *)

type current_time = float Lazy.t
    (** The current time. It is a lazy value to avoid a call to
        [Unix.gettimeofday] if not needed. *)

type select = fd_set -> fd_set -> fd_set -> float option -> current_time * fd_set * fd_set * fd_set
  (** Type of a select-like function *)

val select_filters : (current_time -> select -> select) Lwt_sequence.t
  (** The set of all select filters. *)

val apply_filters : select -> select
  (** [apply_filters select] apply all the filters on the given select
      function. *)

val default_select : select
  (** The default select function. It uses [Unix.select]. *)

val default_iteration : unit -> unit
  (** The default iteration function. It just apply all select filters
      on [default_select], the, call the resulting select function. *)

val main_loop_iteration : (unit -> unit) ref
  (** The function doing one step of the main loop. *)

(** {6 Utils} *)

val min_timeout : float option -> float option -> float option
  (** [min_timeout a b] returns the smallest timeout of two
      timeouts. *)
