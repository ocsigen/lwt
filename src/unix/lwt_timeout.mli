(* OCaml promise library
 * http://www.ocsigen.org/lwt
 * Copyright (C) 2005-2008 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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

(** Cancelable timeouts. *)

type t

val create : int -> (unit -> unit) -> t
(** [Lwt_timeout.create n f] creates a new timeout object with duration [n]
    seconds. [f] is the {e action}, a function to be called once the timeout
    expires. [f] should not raise exceptions.

    The timeout is not started until {!Lwt_timeout.start} is called on it. *)

val start : t -> unit
(** Starts the given timeout.

    Starting a timeout that has already been started has the same effect as
    stopping it, and then restarting it with its original duration. So,
    suppose you have [timeout] with a duration of three seconds, which was
    started two seconds ago. The next call to its action is scheduled for one
    second in the future. Calling [Lwt_timeout.start timeout] at this point
    cancels this upcoming action call, and schedules a call three seconds from
    now. *)

val stop : t -> unit
(** Stops (cancels) the given timeout. *)

val change : t -> int -> unit
(** Changes the duration of the given timeout.

    If the timeout has already been started, it is stopped, and restarted with
    its new duration. This is similar to how {!Lwt_timeout.start} works on a
    timeout that has already been started. *)

val set_exn_handler : (exn -> unit) -> unit
(** [Lwt_timeout.set_exn_handler f] sets the handler to be used for exceptions
    raised by timeout actions. Recall that actions are not allowed to raise
    exceptions. If they do raise an exception [exn] despite this, [f exn] is
    called.

    The default behavior of [f exn], set by [Lwt_timeout] on program startup, is
    to pass [exn] to [!]{!Lwt.async_exception_hook}. The default behavior of
    {e that} is to terminate the process. *)
