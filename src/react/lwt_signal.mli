(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Interface Lwt_event
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

(** Signals utilities *)

(** {6 Utilities} *)

val limit : ?eq : ('a -> 'a -> bool) -> (unit -> unit Lwt.t) -> 'a React.signal -> 'a React.signal
  (** [limit f signal] limits the rate of [signal] update with [f].

      For example, to limit it to 1 per second, you can use: [limit
      (fun () -> Lwt_unix.sleep 1.0) signal]. *)

val map_s : ?eq : ('b -> 'b -> bool) -> ('a -> 'b Lwt.t) -> 'b -> 'a React.signal -> 'b React.signal
  (** [map_s ?eq f i s] is the signal [s] transformed by the function
      [f], which may yield. [i] is the initial value of the returned
      signal. *)

(** {6 Notification} *)

type notifier
  (** Type of signal notifiers *)

val disable : notifier -> unit
  (** [disable notif] stops the corresponding signal to be
      monitored *)

val notify : ('a -> unit) -> 'a React.signal -> notifier
  (** [notify f s] calls [f] each time the value of [s] change *)

val notify_p : ('a -> unit Lwt.t) -> 'a React.signal -> notifier
  (** [notify_p f s] is the same as [notify] except that [f x] is a
      thread. Calls to [f] are made in parallel. *)

val notify_s : ('a -> unit Lwt.t) -> 'a React.signal -> notifier
  (** [notify_s f s] is the same as [notify] except that [f x] is a
      thread. Calls to [f] are serialized. *)

val always_notify : ('a -> unit) -> 'a React.signal -> unit
  (** Same as [notify] but does not return a notifier *)

val always_notify_p : ('a -> unit Lwt.t) -> 'a React.signal -> unit
  (** Same as [notify_p] but does not return a notifier *)

val always_notify_s : ('a -> unit Lwt.t) -> 'a React.signal -> unit
  (** Same as [notify_s] but does not return a notifier *)
