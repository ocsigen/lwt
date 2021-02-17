(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(** @since 5.5.0 *)

type 'a t = unit -> 'a node Lwt.t
(** The type of delayed lists containing elements of type ['a].
  Note that the concrete list node ['a node] is delayed under a closure,
  not a [lazy] block, which means it might be recomputed every time
  we access it. *)

and +'a node = Nil | Cons of 'a * 'a t
(** A fully-evaluated list node, either empty or containing an element
  and a delayed tail. *)

val empty : 'a t
(** The empty sequence, containing no elements. *)

val return : 'a -> 'a t
(** The singleton sequence containing only the given element. *)

val cons : 'a -> 'a t -> 'a t
(** [cons x xs] is the sequence containing the element [x] followed by
  the sequence [xs] *)

val append : 'a t -> 'a t -> 'a t
(** [append xs ys] is the sequence [xs] followed by the sequence [ys] *)

val map : ('a -> 'b Lwt.t) -> 'a t -> 'b t
(** [map f seq] returns a new sequence whose elements are the elements of
  [seq], transformed by [f].
  This transformation is lazy, it only applies when the result is traversed. *)

val filter : ('a -> bool Lwt.t) -> 'a t -> 'a t
(** Remove from the sequence the elements that do not satisfy the
  given predicate.
  This transformation is lazy, it only applies when the result is
  traversed. *)

val filter_map : ('a -> 'b option Lwt.t) -> 'a t -> 'b t
(** Apply the function to every element; if [f x = None] then [x] is dropped;
  if [f x = Some y] then [y] is returned.
  This transformation is lazy, it only applies when the result is
  traversed. *)

val flat_map : ('a -> 'b t Lwt.t) -> 'a t -> 'b t
(** Map each element to a subsequence, then return each element of this
  sub-sequence in turn.
  This transformation is lazy, it only applies when the result is
  traversed. *)

val fold_left : ('a -> 'b -> 'a Lwt.t) -> 'a -> 'b t -> 'a Lwt.t
(** Traverse the sequence from left to right, combining each element with the
  accumulator using the given function.
  The traversal happens immediately and will not terminate (i.e., the promise
  will not resolve) on infinite sequences. *)

val iter : ('a -> unit Lwt.t) -> 'a t -> unit Lwt.t
(** Iterate on the sequence, calling the (imperative) function on every element.

  The sequence's next node is evaluated only once the function has finished
  processing the current element. More formally: the promise for the [n+1]th
  node of the sequence is created only once the promise returned by [f] on the
  [n]th element of the sequence has resolved.

  The traversal happens immediately and will not terminate (i.e., the promise
  will not resolve) on infinite sequences. *)

val unfold : ('b -> ('a * 'b) option Lwt.t) -> 'b -> 'a t
(** Build a sequence from a step function and an initial value.
  [unfold f u] returns [empty] if the promise [f u] resolves to [None],
  or [fun () -> Lwt.return (Cons (x, unfold f y))] if the promise [f u] resolves
  to [Some (x, y)]. *)

val to_list : 'a t -> 'a list Lwt.t
(** Convert a sequence to a list, preserving order.
  The traversal happens immediately and will not terminate (i.e., the promise
  will not resolve) on infinite sequences. *)

val of_list : 'a list -> 'a t
(** Convert a list to a sequence, preserving order. *)

val of_seq : 'a Seq.t -> 'a t
(** Convert from ['a Stdlib.Seq.t] to ['a Lwt_seq.t].
  This transformation is lazy, it only applies when the result is
  traversed. *)

val of_seq_lwt : 'a Lwt.t Seq.t -> 'a t Lwt.t
(** Convert from ['a Lwt.t Stdlib.Seq.t] to ['a Lwt_seq.t].
  This transformation is lazy, it only applies when the result is
  traversed. *)
