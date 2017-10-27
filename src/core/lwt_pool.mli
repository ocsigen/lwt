(* Lwt
 * http://www.ocsigen.org
 * Copyright (C) 2008 Jérôme Vouillon
 *               2012 Jérémie Dimino
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exceptions;
 * either version 2.1 of the License, or (at your option) any later version.
 * See COPYING file for details.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(** External resource pools.

    For example, instead of creating a new database connection each time you
    need one, keep a pool of opened connections and reuse ones that are free.
    The pool also provides a limit on the number of connections that can
    simultaneously be open.

    If you want to have a pool of {e system} threads, consider using
    [Lwt_preemptive]. *)

type 'a t
  (** Pools containing elements of type ['a]. *)

val create :
  int ->
  ?validate : ('a -> bool Lwt.t) ->
  ?check : ('a -> (bool -> unit) -> unit) ->
  ?dispose : ('a -> unit Lwt.t) ->
  (unit -> 'a Lwt.t) -> 'a t
  (** [create n ?check ?validate ?dispose f] creates a new pool with at most
      [n] elements. [f] is used to create a new pool element.  Elements are
      created on demand and re-used until disposed of.

      @param validate is called each time a pool element is accessed by {!use},
      before the element is provided to {!use}'s callback.  If
      [validate element] resolves to [true] the element is considered valid and
      is passed to the callback for use as-is.  If [validate element] resolves
      to [false] the tested pool element is passed to [dispose] then dropped,
      with a new one is created to take [element]'s place in the pool.

      @param check is called after the resolution of {!use}'s callback when the
      resolution is a failed promise.  [check element is_ok] must call [is_ok]
      exactly once with [true] if [element] is still valid and [false]
      otherwise.  If [check] calls [is_ok false] then [dispose] will be run
      on [element] and the element will not be returned to the pool.

      @param dispose is used as described above and by {!clear} to dispose of
      all elements in a pool.  [dispose] is {b not} guaranteed to be called on
      the elements in a pool when the pool is garbage collected.  {!clear}
      should be used if the elements of the pool need to be explicitly disposed
      of. *)

val use : 'a t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
  (** [use p f] takes one free element of the pool [p] and gives it to
      the function [f]. The element is put back into the pool after the
      promise created by [f] completes. *)

val clear : 'a t -> unit Lwt.t
  (** [clear p] will clear all elements in [p], calling the [dispose] function
      associated with [p] on each of the cleared elements.  Any elements from
      [p] which are currently in use will be disposed of once they are
      released.

      The next call to [use p] after [clear p] guarantees a freshly created
      pool element.

      Disposals are performed sequentially in an undefined order. *)

val wait_queue_length : _ t -> int
  (** [wait_queue_length p] returns the number of threads currently
      waiting for an element of the pool [p] to become available. *)
