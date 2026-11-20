(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(** A per-domain slot, and the identity of the current domain. This is the
    only module of the core that knows about domains. On OCaml 5, a slot is a
    [Domain.DLS] key; on 4.14, a plain cell.

    Internal to the Lwt packages. The library is [wrapped false], so only the
    alert keeps users away. *)

[@@@alert trespassing "Lwt_dls is internal to the Lwt packages, keep away."]

type 'a t

val new_key : (unit -> 'a) -> 'a t
(** [new_key init] creates a slot. On OCaml 5, [init ()] runs on the first
    access from each domain. On 4.14 it runs once, at creation. Callers must
    depend only on the value it produces, not on when it runs. *)

val get : 'a t -> 'a
val set : 'a t -> 'a -> unit

type token = private int
(** The identity of a domain: [Domain.self ()], unique among all the domains of
    a program, unlike [Domain.self_index], which is reused. *)

val self_token : unit -> token
(** The calling domain's identity. Always the same value on 4.14. *)
