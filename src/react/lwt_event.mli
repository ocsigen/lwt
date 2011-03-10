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

(** Events utilities *)

(** This module is deprecated, you should use {!Lwt_react.E}
    instead. *)

open React

val with_finaliser : (unit -> unit) -> 'a event -> 'a event
val next : 'a event -> 'a Lwt.t
val limit : (unit -> unit Lwt.t) -> 'a event -> 'a event
val from : (unit -> 'a Lwt.t) -> 'a event
val to_stream : 'a event -> 'a Lwt_stream.t
val of_stream : 'a Lwt_stream.t -> 'a event
val delay : 'a event Lwt.t -> 'a event
val app_s : ('a -> 'b Lwt.t) event -> 'a event -> 'b event
val app_p : ('a -> 'b Lwt.t) event -> 'a event -> 'b event
val map_s : ('a -> 'b Lwt.t) -> 'a event -> 'b event
val map_p: ('a -> 'b Lwt.t) -> 'a event -> 'b event
val filter_s : ('a -> bool Lwt.t) -> 'a event -> 'a event
val filter_p : ('a -> bool Lwt.t) -> 'a event -> 'a event
val fmap_s : ('a -> 'b option Lwt.t) -> 'a event -> 'b event
val fmap_p : ('a -> 'b option Lwt.t) -> 'a event -> 'b event
val diff_s : ('a -> 'a -> 'b Lwt.t) -> 'a event -> 'b event
val accum_s : ('a -> 'a Lwt.t) event -> 'a -> 'a event
val fold_s : ('a -> 'b -> 'a Lwt.t) -> 'a -> 'b event -> 'a event
val merge_s : ('a -> 'b -> 'a Lwt.t) -> 'a -> 'b event list -> 'a event
val run_s : 'a Lwt.t event -> 'a event
val run_p : 'a Lwt.t event -> 'a event
type notifier
val disable : notifier -> unit
val notify : ('a -> unit) -> 'a event -> notifier
val notify_p : ('a -> unit Lwt.t) -> 'a event -> notifier
val notify_s : ('a -> unit Lwt.t) -> 'a event -> notifier
val always_notify : ('a -> unit) -> 'a event -> unit
val always_notify_p : ('a -> unit Lwt.t) -> 'a event -> unit
val always_notify_s : ('a -> unit Lwt.t) -> 'a event -> unit
