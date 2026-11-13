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

val check_owner : string -> token -> unit
(** [check_owner name owner] raises [Invalid_argument], mentioning [name],
    unless [owner] is the calling domain's token. *)

val is_main_domain : unit -> bool
(** Always [true] on 4.14, where there is one domain and it is the main one. *)

val at_domain_exit : (unit -> unit) -> unit
(** [at_domain_exit f] runs [f] when the current domain exits. On the main
    domain, that is at process exit, before every [Stdlib.at_exit] callback,
    so a callback that must run after the user's own [Stdlib.at_exit]
    callbacks is registered with [Stdlib.at_exit] instead. On 4.14 it is
    [Stdlib.at_exit]. *)
