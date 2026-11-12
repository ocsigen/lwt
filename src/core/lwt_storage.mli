(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(** The storage behind {!Lwt.key}: an immutable map from keys to values, each
    key having its own value type. Built on [Type.Id] from OCaml 5.1, on a fresh
    constructor of an extensible type per key before. *)

type 'v key

val new_key : unit -> 'v key
(** [new_key ()] is a key distinct from all the keys created before it, in any
    domain. *)

type t
(** A storage. *)

val empty : t
(** [empty] binds no key. *)

val find : 'v key -> t -> 'v option
(** [find k s] is the value of [k] in [s], or [None] if [s] does not bind
    [k]. *)

val set : 'v key -> 'v option -> t -> t
(** [set k (Some v) s] binds [k] to [v] in [s], replacing its previous value.
    [set k None s] removes the binding of [k]. *)
