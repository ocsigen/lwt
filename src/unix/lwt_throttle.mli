(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(** Rate limiters *)

(** This module defines rate limiters. A rate limiter is parametrized
    by its limit and a maximum waiting time. The [wait] function will
    collaboratively hang for a delay necessary to respect the
    limit. If that delay exceeds the maximum waiting time, [wait]
    returns [false]; otherwise it returns [true]. *)

module type S = sig
  type key
  type t

  val create : rate:int -> max:int -> n:int -> t
    (**
       @param rate maximum number of connections per second
       @param max maximum waiting time (in seconds)
       @param n initial size of the hash table
    *)

  val wait : t -> key -> bool Lwt.t
    (** @return [false] if maximum reached, [true] else *)
end

module Make (H : Hashtbl.HashedType) : S with type key = H.t
