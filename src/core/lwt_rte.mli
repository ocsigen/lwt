(** All the functions below are wrappers around the ones found in
    [Lwt_runtime_events]. The wrappers conditionally call the correspoding
    wrapped function or do nothing, depending if the [Lwt_runtime_events] library
    is available.

    Note that [Lwt_runtime_events] is only installable with OCaml 5.1+ *)

val emit_paused_count : int -> unit
val emit_sch_call_begin : unit -> unit
val emit_sch_call_end : unit -> unit
val emit_sch_lap : unit -> unit
val emit_job_count : int -> unit
type span = Begin | End
val emit_trace : span -> string option -> string -> int -> unit
