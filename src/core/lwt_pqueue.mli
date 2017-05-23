(* OCaml promise library
 * http://www.ocsigen.org/lwt
 * Copyright (C) 1999-2004 Jérôme Vouillon
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

(** Functional priority queues (deprecated).

    A priority queue maintains, in the abstract sense, a set of elements in
    order, and supports fast lookup and removal of the first ("minimum")
    element. This is used in Lwt for organizing threads that are waiting for
    timeouts.

    The priority queues in this module preserve "duplicates": elements that
    compare equal in their order.

    @deprecated This module is an internal implementation detail of Lwt, and may
    be removed from the API at some point in the future. For alternatives, see,
    for example: {{: https://www.lri.fr/~filliatr/software.en.html#heap} Heaps}
    by Jean-Cristophe Filliatre,
    {{: http://cedeela.fr/~simon/software/containers/CCHeap.html} containers},
    or
    {{: http://ocaml-batteries-team.github.io/batteries-included/hdoc2/BatHeap.html}
    Batteries}. *)

[@@@ocaml.deprecated
" This module is an implementation detail of Lwt. See
   http://ocsigen.org/lwt/dev/api/Lwt_pqueue"]

(** Signature pairing an element type with an ordering function. *)
module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

(** Signature of priority queues. *)
module type S =
  sig
    type elt
    (** Type of elements contained in the priority queue. *)

    type t
    (** Type of priority queues. *)

    val empty: t
    (** The empty priority queue. Contains no elements. *)

    val is_empty: t -> bool
    (** [is_empty q] evaluates to [true] iff [q] is empty. *)

    val add: elt -> t -> t
    (** [add e q] evaluates to a new priority queue, which contains all the
        elements of [q], and the additional element [e]. *)

    val union: t -> t -> t
    (** [union q q'] evaluates to a new priority queue, which contains all the
        elements of both [q] and [q']. *)

    val find_min: t -> elt
    (** [find_min q] evaluates to the minimum element of [q] if it is not empty,
        and raises [Not_found] otherwise. *)

    val lookup_min: t -> elt option
    (** [lookup_min q] evaluates to [Some e], where [e] is the minimum element
        of [q], if [q] is not empty, and evaluates to [None] otherwise. *)

    val remove_min: t -> t
    (** [remove_min q] evaluates to a new priority queue, which contains all the
        elements of [q] except for its minimum element. Raises [Not_found] if
        [q] is empty. *)

    val size: t -> int
    (** [size q] evaluates to the number of elements in [q]. *)
  end

(** Generates priority queue types from ordered types. *)
module Make(Ord: OrderedType) : S with type elt = Ord.t
