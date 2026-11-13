(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

module Impl = struct
  type 'a t = 'a Domain.DLS.key

  let new_key init = Domain.DLS.new_key init
  let get = Domain.DLS.get
  let set = Domain.DLS.set

  (* The token is [Domain.self ()]: comparing it is much cheaper than reading
     a token from a slot. *)
  type token = int

  let self_token () = (Domain.self () :> int)
  let is_main_domain () = Domain.is_main_domain ()
  let at_domain_exit f = Domain.at_exit f
end
[@@if ocaml_version >= (5, 0, 0)]

module Impl = struct
  (* One domain, so a slot is a cell, initialized at once. *)
  type 'a t = 'a ref

  let new_key init = ref (init ())
  let get k = !k
  let set k v = k := v

  (* One domain, so one token. *)
  type token = int

  let self_token () = 0

  (* One domain, the main one. [at_domain_exit] is [Stdlib.at_exit] here, so
     that callers need no version test. *)
  let is_main_domain () = true
  let at_domain_exit f = Stdlib.at_exit f
end
[@@if ocaml_version < (5, 0, 0)]

include Impl

(* For Lwt_mutex, Lwt_condition, Lwt_mvar, Lwt_pool, Lwt_stream and
   Lwt_switch, whose values belong to the domain that created them. *)

let[@inline never] foreign name =
  invalid_arg (name ^ ": this value belongs to another domain")

let[@inline] check_owner name owner =
  if owner <> self_token () then foreign name
