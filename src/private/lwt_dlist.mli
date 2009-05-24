(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Interface Lwt_dlist
 * Copyright (C) 2009 Jérémie Dimino
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

(** Doubly-linked list *)

(** A node in a doubly linked list: *)
type 'a node = {
  mutable prev : 'a node;
  data : 'a;
  mutable next : 'a node;
}

val make : 'a -> 'a node
  (** [make data] create a new node connected to itself *)

val data : 'a node -> 'a
  (** [data node] returns the data contained in [node] *)

val remove : 'a node -> unit
  (** Removes a node from any doubly-linked it is part of *)

val junk : 'a node -> unit
  (** Same as {!remove}, but a bit more efficient. However the node
      must not be reused after that. *)

val next : 'a node -> 'a node
  (** [next node] *)

val prev : 'a node -> 'a node
  (** [prev node] *)

val link : 'a node -> 'a node -> unit
  (** [link node1 node2] links two node together.

      Here is an example of linking:


      {[
            node1 node2
        O---->O     O---->O
        ^     |     ^     |
        |     |     |     |
        |     v     |     v
        O<----O     O<----O

      ]}

      result:

      {[
            node1
        O---->O---->O---->O
        ^                 |
        |                 |
        |                 v
        O<----O<----O<----O
      ]}

      Where [O1---->O2] means that [O2] is the next of [O1] and [O1] is
      the previous of [O2].
  *)

val append_data : 'a -> 'a node -> 'a node
  (** [append_data data node] creates a new node, insert it after
      [node], and return it. *)

val prepend_data : 'a -> 'a node -> 'a node
  (** [prepend_data data node] creates a new node, attach it before
      [node], and return it. *)

val iter : ('a -> unit) -> 'a node -> unit
  (** [iter f node] applies [f] on all nodes connected to [node] *)

val is_alone : 'a node -> bool
  (** [is_alone node] returns whether [node] is alone or not *)

val count : 'a node -> int
  (** [count node] returns the number of threads connected to
      [node] *)
