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
end
[@@if ocaml_version < (5, 0, 0)]

include Impl
