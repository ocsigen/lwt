(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_stream
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

(** Data streams *)

type 'a t
  (** Type of a stream holding values of type ['a] *)

(** Naming convention: in this module all function taking a function
    which is applied to all element of the streams are suffixed by:

    - [_s] when the function returns a thread and calls are serialised
    - [_p] when the function returns a thread and calls are parallelised
*)

(** {6 Construction} *)

val from : (unit -> 'a option Lwt.t) -> 'a t
  (** [from f] creates an stream from the given input function. [f] is
      called each time more input is needed, and the stream ends when
      [f] returns [None]. *)

val from_direct : (unit -> 'a option) -> 'a t
  (** [from_direct f] does the same as {!from} but with a function
      that does not return a thread. It is better than wrapping [f]
      into a function which return a thread. *)

exception Closed
  (** Exception raised by the push function of a push-stream when
      pushing an element after the end of stream ([= None]) have been
      pushed. *)

val create : unit -> 'a t * ('a option -> unit)
  (** [create ()] returns a new stream and a push function. *)

val create_with_reference : unit -> 'a t * ('a option -> unit) * ('b -> unit)
  (** [create_with_reference ()] returns a new stream and a push
      function. The last function allows to set a reference to an
      external source. This prevent the external source from being
      garbage collected.

      For example, to convert a reactive event to a stream:

      {[
        let stream, push, set_ref = Lwt_stream.create_with_reference () in
        set_ref (map_event push event)
      ]}
  *)

exception Full
  (** Exception raised by the push function of a bounded push-stream
      when the stream queue is full and a thread is already waiting to
      push an element. *)

(** Type of sources for bounded push-streams. *)
class type ['a] bounded_push = object
  method size : int
    (** Size of the stream. *)

  method resize : int -> unit
    (** Change the size of the stream queue. Note that the new size
        can smaller than the current stream queue size.

        It raises [Invalid_argument] if [size < 0]. *)

  method push : 'a -> unit Lwt.t
    (** Pushes a new element to the stream. If the stream is full then
        it will block until one element is consumed. If another thread
        is already blocked on {!push}, it raises {!Full}. *)

  method close : unit
    (** Closes the stream. Any thread currently blocked on {!push}
        will fail with {!Closed}. *)

  method count : int
    (** Number of elements in the stream queue. *)

  method blocked : bool
    (** Is a thread is blocked on {!push} ? *)

  method closed : bool
    (** Is the stream closed ? *)

  method set_reference : 'a. 'a -> unit
    (** Set the reference to an external source. *)
end

val create_bounded : int -> 'a t * 'a bounded_push
  (** [create_bounded size] returns a new stream and a bounded push
      source. The stream can hold a maximum of [size] elements.  When
      this limit is reached, pushing a new element will block until
      one is consumed.

      Note that you cannot clone or parse (with {!parse}) a bounded
      stream. These functions will raise [Invalid_argument] if you try
      to do so.

      It raises [Invalid_argument] if [size < 0]. *)

val of_list : 'a list -> 'a t
  (** [of_list l] creates a stream returning all elements of [l] *)

val of_array : 'a array -> 'a t
  (** [of_array a] creates a stream returning all elements of [a] *)

val of_string : string -> char t
  (** [of_string str] creates a stream returning all characters of
      [str] *)

val clone : 'a t -> 'a t
  (** [clone st] clone the given stream. Operations on each stream
      will not affect the other.

      For example:

      {[
        # let st1 = Lwt_stream.of_list [1; 2; 3];;
        val st1 : int Lwt_stream.t = <abstr>
        # let st2 = Lwt_stream.clone st1;;
        val st2 : int Lwt_stream.t = <abstr>
        # lwt x = Lwt_stream.next st1;;
        val x : int = 1
        # lwt y = Lwt_stream.next st2;;
        val y : int = 1
      ]}

      It raises [Invalid_argument] if [st] is a bounded
      push-stream. *)

(** {6 Destruction} *)

val to_list : 'a t -> 'a list Lwt.t
  (** Returns the list of elements of the given stream *)

val to_string : char t -> string Lwt.t
  (** Returns the word composed of all characters of the given
      stream *)

(** {6 Data retreival} *)

exception Empty
  (** Exception raised when trying to retreive data from an empty
      stream. *)

val peek : 'a t -> 'a option Lwt.t
  (** [peek st] returns the first element of the stream, if any,
      without removing it. *)

val npeek : int -> 'a t -> 'a list Lwt.t
  (** [npeek n st] returns at most the first [n] elements of [st],
      without removing them. *)

val get : 'a t -> 'a option Lwt.t
  (** [get st] remove and returns the first element of the stream, if
      any. *)

val nget : int -> 'a t -> 'a list Lwt.t
  (** [nget n st] remove and returns at most the first [n] elements of
      [st]. *)

val get_while : ('a -> bool) -> 'a t -> 'a list Lwt.t
val get_while_s : ('a -> bool Lwt.t) -> 'a t -> 'a list Lwt.t
  (** [get_while f st] returns the longest prefix of [st] where all
      elements satisfy [f]. *)

val next : 'a t -> 'a Lwt.t
  (** [next st] remove and returns the next element of the stream, of
      fail with {!Empty} if the stream is empty. *)

val last_new : 'a t -> 'a Lwt.t
  (** [last_new st] returns the last element that can be obtained
      without sleepping, or wait for one if no one is already
      available.

      If fails with {!Empty} if the stream has no more elements *)

val junk : 'a t -> unit Lwt.t
  (** [junk st] remove the first element of [st]. *)

val njunk : int -> 'a t -> unit Lwt.t
  (** [njunk n st] removes at most the first [n] elements of the
      stream. *)

val junk_while : ('a -> bool) -> 'a t -> unit Lwt.t
val junk_while_s : ('a -> bool Lwt.t) -> 'a t -> unit Lwt.t
  (** [junk_while f st] removes all elements at the beginning of the
      streams which satisfy [f]. *)

val junk_old : 'a t -> unit Lwt.t
  (** [junk_old st] removes all elements that are ready to be read
      without yeilding from [st].

      For example the [read_password] function of [Lwt_read_line] use
      that to junk key previously typed by the user.
  *)

val get_available : 'a t -> 'a list
  (** [get_available l] returns all available elements of [l] without
      blocking *)

val get_available_up_to : int -> 'a t -> 'a list
  (** [get_available_up_to l n] returns up to [n] elements of [l]
      without blocking *)

val is_empty : 'a t -> bool Lwt.t
  (** [is_empty enum] returns wether the given stream is empty *)

(** {6 Stream transversal} *)

(** Note: all the following functions are destructive.

    For example:

    {[
      # let st1 = Lwt_stream.of_list [1; 2; 3];;
      val st1 : int Lwt_stream.t = <abstr>
      # let st2 = Lwt_stream.map string_of_int st1;;
      val st2 : string Lwt_stream.t = <abstr>
      # lwt x = Lwt_stream.next st1;;
      val x : int = 1
      # lwt y = Lwt_stream.next st2;;
      val y : string = "2"
    ]}
*)

val choose : 'a t list -> 'a t
  (** [choose l] creates an stream from a list of streams. The
      resulting stream will returns elements returned by any stream of
      [l] in an unspecified order. *)

val map : ('a -> 'b) -> 'a t -> 'b t
val map_s : ('a -> 'b Lwt.t) -> 'a t -> 'b t
  (** [map f st] maps the value returned by [st] with [f] *)

val filter : ('a -> bool) -> 'a t -> 'a t
val filter_s : ('a -> bool Lwt.t) -> 'a t -> 'a t
  (** [filter f st] keeps only value [x] such that [f x] is [true] *)

val filter_map : ('a -> 'b option) -> 'a t -> 'b t
val filter_map_s : ('a -> 'b option Lwt.t) -> 'a t -> 'b t
  (** [filter_map f st] filter and map [st] at the same time *)

val map_list : ('a -> 'b list) -> 'a t -> 'b t
val map_list_s : ('a -> 'b list Lwt.t) -> 'a t -> 'b t
  (** [map_list f st] applies [f] on each element of [st] and flattens
      the lists returned *)

val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b Lwt.t
val fold_s : ('a -> 'b -> 'b Lwt.t) -> 'a t -> 'b -> 'b Lwt.t
  (** [fold f s x] fold_like function for streams. *)

val iter : ('a -> unit) -> 'a t -> unit Lwt.t
val iter_p : ('a -> unit Lwt.t) -> 'a t -> unit Lwt.t
val iter_s : ('a -> unit Lwt.t) -> 'a t -> unit Lwt.t
  (** [iter f s] iterates over all elements of the stream *)

val find : ('a -> bool) -> 'a t -> 'a option Lwt.t
val find_s : ('a -> bool Lwt.t) -> 'a t -> 'a option Lwt.t
  (** [find f s] find an element in a stream. *)

val find_map : ('a -> 'b option) -> 'a t -> 'b option Lwt.t
val find_map_s : ('a -> 'b option Lwt.t) -> 'a t -> 'b option Lwt.t
  (** [find f s] find and map at the same time. *)

val combine : 'a t -> 'b t -> ('a * 'b) t
  (** [combine s1 s2] combine two streams. The stream will ends when
      the first stream ends. *)

val append : 'a t -> 'a t -> 'a t
  (** [append s1 s2] returns a stream which returns all elements of
      [s1], then all elements of [s2] *)

val concat : 'a t t -> 'a t
  (** [concat st] returns the concatenation of all streams of [st]. *)

val flatten : 'a list t -> 'a t
  (** [flatten st = map_list (fun l -> l) st] *)

(** A value or an error. *)
type 'a result =
  | Value of 'a
  | Error of exn

val map_exn : 'a t -> 'a result t
  (** [map_exn s] returns a stream that captures all exceptions raised
      by the source of the stream (the function passed to {!from}).

      Note that for push-streams (as returned by {!create}) all
      elements of the mapped streams are values. *)

(** {6 Parsing} *)

val parse : 'a t -> ('a t -> 'b Lwt.t) -> 'b Lwt.t
  (** [parse st f] parses [st] with [f]. If [f] raise an exception,
      [st] is restored to its previous state.

      It raises [Invalid_argument] if [st] is a bounded
      push-stream. *)

(** {6 Misc} *)

val hexdump : char t -> string t
  (** [hexdump byte_stream] returns a stream which is the same as the
      output of [hexdump -C].

      Basically, here is a simple implementation of [hexdump -C]:

      {[
        let () = Lwt_main.run (Lwt_io.write_lines Lwt_io.stdout (Lwt_stream.hexdump (Lwt_io.read_lines Lwt_io.stdin)))
      ]}
  *)
