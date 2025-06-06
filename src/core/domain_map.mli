(** Domain-indexed maps with thread-safe operations *)

(** Thread-safe wrapper for domain maps *)
type 'a protected_map

(** Create a new protected map with an empty map *)
val create_protected_map : unit -> 'a protected_map

(** Add a key-value binding to the map *)
val add : 'a protected_map -> Domain.id -> 'a -> unit

(** Remove a key from the map *)
val remove : 'a protected_map -> Domain.id -> unit

(** Update a binding using the underlying map's update function *)
val update : 'a protected_map -> Domain.id -> ('a option -> 'a option) -> unit

(** Find a value by key, returning None if not found *)
val find : 'a protected_map -> Domain.id -> 'a option

(** Find + remove but hit the mutex only once *)
val extract : 'a protected_map -> Domain.id -> 'a option

(** Get the number of bindings in the map *)
val size : 'a protected_map -> int

(** Initialize a key with a value if it doesn't exist, return existing or new value *)
val init : 'a protected_map -> Domain.id -> (unit -> 'a) -> 'a
