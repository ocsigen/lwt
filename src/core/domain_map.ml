module Domain_map : Map.S with type key = Domain.id = Map.Make(struct
  type t = Domain.id
  let compare d1 d2 = Int.compare (d1 : Domain.id :> int) (d2 : Domain.id :> int)
end)

(* Protected domain map reference with per-reference mutex *)
type 'a protected_map = {
  mutex : Mutex.t;
  mutable map : 'a Domain_map.t;
}

let create_protected_map () = {
  mutex = Mutex.create ();
  map = Domain_map.empty;
}

let with_lock protected_map f =
  Mutex.lock protected_map.mutex;
  Fun.protect f ~finally:(fun () -> Mutex.unlock protected_map.mutex)

let update_map protected_map f =
  with_lock protected_map (fun () ->
    let old_map = protected_map.map in
    let new_map = f old_map in
    protected_map.map <- new_map)

let add protected_map key value =
  update_map protected_map (Domain_map.add key value)

let remove protected_map key =
  update_map protected_map (Domain_map.remove key)

let update protected_map key f =
  update_map protected_map (Domain_map.update key f)

let find protected_map key =
  with_lock protected_map (fun () -> Domain_map.find_opt key protected_map.map)

let extract protected_map key =
  with_lock protected_map (fun () ->
    match Domain_map.find_opt key protected_map.map with
    | None -> None
    | Some v ->
        protected_map.map <- Domain_map.remove key protected_map.map;
        Some v)

let size protected_map =
  with_lock protected_map (fun () -> Domain_map.cardinal protected_map.map)

let init protected_map key init_value =
  with_lock protected_map (fun () ->
    match Domain_map.find_opt key protected_map.map with
    | Some existing -> existing
    | None ->
        let new_value = init_value () in
        protected_map.map <- Domain_map.add key new_value protected_map.map;
        new_value)
