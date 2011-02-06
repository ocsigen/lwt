(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Interface Lwt_engine
 * Copyright (C) 2011 Jérémie Dimino
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

(** Lwt unix main loop engine *)

(** {6 Events} *)

type event
  (** Type of events. An event represent a callback registered to be
      called when some event occurs. *)

val make_event : (unit -> unit) -> event
  (** [make_event stop] creates a new event. [stop] is the function
      used to stop the event and free its associated resources. [stop]
      is guaranteed to be called at most one time. *)

val stop_event : event -> unit
  (** [stop_event event] stops the given event. *)

val fake_event : event
  (** Event which does nothing when stopped. *)

(** {6 Engines} *)

(** An engine represent a set of functions used to register different
    kind of callbacks for different kind of events. *)

(** Type of engines. *)
type t = {
  on_readable : Unix.file_descr -> (unit -> unit) -> event;
  (** [on_readable fd f] musts call [f] each time [fd] becomes
      readable. *)

  on_writable : Unix.file_descr -> (unit -> unit) -> event;
  (** [on_readable fd f] musts call [f] each time [fd] becomes
      writable. *)

  on_timer : float -> (unit -> unit) -> event;
  (** [on_timer delay f] musts call [f] one time after [delay]
      seconds. *)

  on_signal : int -> (unit -> unit) -> event;
  (** [on_signal signum f] musts call [f] each time the signal with
      number [signum] is received by the process. *)

  on_suspend : (unit -> unit) -> event;
  (** [on_suspend f] musts call [f] each time is about to goes into
      idle by suspending itself. *)

  on_resume : (unit -> unit) -> event;
  (** [on_resume f] musts call [f] each time the process resume from
      idle. *)
}

(** {6 The current engine} *)

val get : unit -> t
  (** [get ()] returns the engine currently in use. *)

val set : t -> unit
  (** [set engine] replaces the current engine by the given one. *)

(** The following functions are for accessing the current engine. *)

val on_readable : Unix.file_descr -> (unit -> unit) -> event
val on_writable : Unix.file_descr -> (unit -> unit) -> event
val on_timer : float -> (unit -> unit) -> event
val on_signal : int -> (unit -> unit) -> event
val on_suspend : (unit -> unit) -> event
val on_resume : (unit -> unit) -> event
