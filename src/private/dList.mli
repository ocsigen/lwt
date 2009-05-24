(*
 * dList.mli
 * ---------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lwt.
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
