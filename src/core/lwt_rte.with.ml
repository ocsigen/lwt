let emit_paused_count v = Lwt_runtime_events.emit_paused_count v
let emit_sch_call_begin () = Lwt_runtime_events.emit_sch_call_begin ()
let emit_sch_call_end () = Lwt_runtime_events.emit_sch_call_end ()
let emit_sch_lap v = Lwt_runtime_events.emit_sch_lap v
let emit_job_count v = Lwt_runtime_events.emit_job_count v
type span = Runtime_events.Type.span = Begin | End
let emit_trace kind context filename line = Lwt_runtime_events.Trace.emit {kind; context; filename; line}
