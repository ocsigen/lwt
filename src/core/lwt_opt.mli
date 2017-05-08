(** Helpers around the [option] type *)

val may : ('a -> unit Lwt.t) -> 'a option -> unit Lwt.t

val map : ('a -> 'b Lwt.t) -> 'a option -> 'b option Lwt.t

val lift : 'a Lwt.t option -> 'a option Lwt.t
