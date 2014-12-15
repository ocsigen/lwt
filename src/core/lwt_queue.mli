(* Lightweight thread library for OCaml
 * http://www.ocsigen.org/lwt
 * Module Lwt_stream
 * Copyright (C) 2014 Simon Cruanes
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

(** {1 Blocking queue}
This queue can be used for message passing between threads *)

type 'a t
(** Queue containing elements of type 'a *)

val create : unit -> 'a t
(** New queue with unbounded capacity *)

val create_bounded : int -> 'a t
(** New queue with bounded capacity (must be a strictly positive int) *)

val is_empty : 'a t -> bool
(** Is the queue currently empty? *)

val push : 'a t -> 'a -> unit Lwt.t
(** Potentially blocking push (in case the queue is full). *)

val pop : 'a t -> 'a Lwt.t
(** Extract the first value of the queue. Blocks if the queue is empty *)
