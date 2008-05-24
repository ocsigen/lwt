
type t

val create : unit -> t
val lock : t -> unit Lwt.t
val unlock : t -> unit
