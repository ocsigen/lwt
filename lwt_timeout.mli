
type t

val set_exn_handler : (exn -> unit) -> unit
(** set the default handler for exception occurring after a timeout.
    The function lauched after a timeout should not raise any exception.
    That's why the default handler will exit the program.
*)

val create : int -> (unit -> unit) -> t
(** [create n f] defines a new timeout with [n] seconds duration. [f] is
    the function to be called after the timeout.
    That function must not raise any exception.
*)

val start : t -> unit
(** starts a timeout. *)

val stop : t -> unit
(** stops a timeout. *)

val change : t -> int -> unit
(** changes the duration of a timeout. *)
