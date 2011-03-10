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

(** This module is deprecated, you should use {!Lwt_react.S}
    instead. *)

open React
val return : 'a -> 'a signal
val bind : ?eq : ('b -> 'b -> bool) -> 'a signal -> ('a -> 'b signal) -> 'b signal
val with_finaliser : (unit -> unit) -> 'a signal -> 'a signal
val limit : ?eq : ('a -> 'a -> bool) -> (unit -> unit Lwt.t) -> 'a signal -> 'a signal
val delay : 'a signal Lwt.t -> 'a event
val app_s : ?eq : ('b -> 'b -> bool) -> ('a -> 'b Lwt.t) signal -> 'b -> 'a signal -> 'b signal
val map_s : ?eq : ('b -> 'b -> bool) -> ('a -> 'b Lwt.t) -> 'b -> 'a signal -> 'b signal
val filter_s : ?eq : ('a -> 'a -> bool) -> ('a -> bool Lwt.t) -> 'a -> 'a signal -> 'a signal
val fmap_s : ?eq:('b -> 'b -> bool) -> ('a -> 'b option Lwt.t) -> 'b -> 'a signal -> 'b signal
val diff_s : ('a -> 'a -> 'b Lwt.t) -> 'a signal -> 'b event
val sample_s : ('b -> 'a -> 'c Lwt.t) -> 'b event -> 'a signal -> 'c event
val accum_s : ?eq : ('a -> 'a -> bool) -> ('a -> 'a Lwt.t) event -> 'a -> 'a signal
val fold_s : ?eq : ('a -> 'a -> bool) -> ('a -> 'b -> 'a Lwt.t) -> 'a -> 'b event -> 'a signal
val merge_s : ?eq : ('a -> 'a -> bool) -> ('a -> 'b -> 'a Lwt.t) -> 'a -> 'b signal list -> 'a signal
val l1_s : ?eq : ('b -> 'b -> bool) -> ('a -> 'b Lwt.t) -> 'b -> ('a signal -> 'b signal)
val l2_s : ?eq : ('c -> 'c -> bool) -> ('a -> 'b -> 'c Lwt.t) -> 'c -> ('a signal -> 'b signal -> 'c signal)
val l3_s : ?eq : ('d -> 'd -> bool) -> ('a -> 'b -> 'c -> 'd Lwt.t) -> 'd -> ('a signal -> 'b signal -> 'c signal -> 'd signal)
val l4_s : ?eq : ('e -> 'e -> bool) -> ('a -> 'b -> 'c -> 'd -> 'e Lwt.t) -> 'e -> ('a signal -> 'b signal -> 'c signal -> 'd signal -> 'e signal)
val l5_s : ?eq : ('f -> 'f -> bool) -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f Lwt.t) -> 'f -> ('a signal -> 'b signal -> 'c signal -> 'd signal -> 'e signal -> 'f signal)
val l6_s : ?eq : ('g -> 'g -> bool) -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g Lwt.t) -> 'g -> ('a signal -> 'b signal -> 'c signal -> 'd signal -> 'e signal -> 'f signal -> 'g signal)
val run_s : ?eq : ('a -> 'a -> bool) -> 'a -> 'a Lwt.t signal -> 'a signal
type notifier
val disable : notifier -> unit
val notify : ('a -> unit) -> 'a signal -> notifier
val notify_p : ('a -> unit Lwt.t) -> 'a signal -> notifier
val notify_s : ('a -> unit Lwt.t) -> 'a signal -> notifier
val always_notify : ('a -> unit) -> 'a signal -> unit
val always_notify_p : ('a -> unit Lwt.t) -> 'a signal -> unit
val always_notify_s : ('a -> unit Lwt.t) -> 'a signal -> unit
