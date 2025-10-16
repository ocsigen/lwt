type Runtime_events.User.tag += Paused_count
val paused_count : int Runtime_events.User.t
val emit_paused_count : int -> unit

type Runtime_events.User.tag += Scheduler_call
val sch_call : Runtime_events.Type.span Runtime_events.User.t
val emit_sch_call_begin : unit -> unit
val emit_sch_call_end : unit -> unit

type Runtime_events.User.tag += Scheduler_lap
val sch_lap : unit Runtime_events.User.t
val emit_sch_lap : unit -> unit

type Runtime_events.User.tag += Unix_job_count
val unix_job_count : int Runtime_events.User.t
val emit_job_count : int -> unit

type Runtime_events.User.tag += Trace
val ss : string Runtime_events.Type.t
val trace : string Runtime_events.User.t
val emit_trace : string -> unit
