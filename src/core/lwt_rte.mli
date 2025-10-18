val emit_paused_count : int -> unit
val emit_sch_call_begin : unit -> unit
val emit_sch_call_end : unit -> unit
val emit_sch_lap : unit -> unit
val emit_job_count : int -> unit
type span = Begin | End
val emit_trace : span -> string -> int -> unit
