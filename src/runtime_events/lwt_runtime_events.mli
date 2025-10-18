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

module Trace : sig
  type t = { kind: Runtime_events.Type.span; context: string option; count: int; filename: string; line: int; }
  val t : t Runtime_events.Type.t

  type Runtime_events.User.tag += T
  val span : t Runtime_events.User.t
  val emit : t -> unit
end
