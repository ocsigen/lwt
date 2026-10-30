(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* The universal type: [inject k v] holds a value of any key's type, and only
   [k] can project it back. A key carries a ['v option], so that a lookup
   returns the stored option without allocating one. *)
module Univ = struct
  type 'v key = 'v option Type.Id.t
  type t = Binding : 'v key * 'v option -> t

  let new_key () = Type.Id.make ()

  (* Unique across domains: the runtime hands extension constructor ids out by
     chunks taken from an atomic counter. *)
  let id = Type.Id.uid
  let inject k v = Binding (k, v)

  let project (type v) (k : v key) (Binding (k', v)) : v option =
    match Type.Id.provably_equal k k' with Some Type.Equal -> v | None -> None
end
[@@if ocaml_version >= (5, 1, 0)]

module Univ = struct
  (* Each key owns a fresh constructor of an extensible type. *)

  type t = ..
  type 'v key = {id : int; inject : 'v option -> t; project : t -> 'v option}

  (* Atomic, so that keys created at once on two domains get distinct ids. *)
  let next_key_id = Atomic.make 0

  let new_key (type v) () : v key =
    let module M = struct
      type t += E of v option
    end in
    let id = Atomic.fetch_and_add next_key_id 1 in
    { id
    ; inject = (fun v -> M.E v)
    ; project = (function M.E v -> v | _ -> None) }

  let id k = k.id
  let inject k v = k.inject v
  let project k u = k.project u
end
[@@if ocaml_version < (5, 1, 0)]

type 'v key = 'v Univ.key

let new_key = Univ.new_key

module Int_map = Map.Make (Int)

type t = Univ.t Int_map.t

let empty = Int_map.empty

let find key storage =
  match Int_map.find_opt (Univ.id key) storage with
  | Some u -> Univ.project key u
  | None -> None

let set key value storage =
  match value with
  | Some _ -> Int_map.add (Univ.id key) (Univ.inject key value) storage
  | None -> Int_map.remove (Univ.id key) storage
