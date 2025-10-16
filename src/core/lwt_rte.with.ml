let emit_paused_count v = Lwt_runtime_events.emit_paused_count v
let emit_sch_call_begin () = Lwt_runtime_events.emit_sch_call_begin ()
let emit_sch_call_end () = Lwt_runtime_events.emit_sch_call_end ()
let emit_sch_lap v = Lwt_runtime_events.emit_sch_lap v
let emit_job_count v = Lwt_runtime_events.emit_job_count v
let emit_trace f l n = Lwt_runtime_events.emit_trace (Printf.sprintf "%s:%d: %s" f l n)
