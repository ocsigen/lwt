(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(** The universal type behind {!Lwt.key}: a value of {!t} holds a value of any
    key's type, and only the key that injected it can project it back. Built on
    [Type.Id] from OCaml 5.1, on a fresh constructor of an extensible type per
    key before.

    A key carries a ['v option], so that a lookup returns the stored option
    without allocating one. *)

type t
type 'v key

val new_key : unit -> 'v key
(** [new_key ()] is a key distinct from all the keys created before it, in any
    domain. *)

val id : 'v key -> int
(** [id k] is an integer identifying [k]: two distinct keys have distinct
    ids. *)

val inject : 'v key -> 'v option -> t
(** [inject k v] is [v], seen as a value of the universal type. *)

val project : 'v key -> t -> 'v option
(** [project k u] is [v] if [u] is [inject k v], and [None] if [u] was injected
    by another key. *)
