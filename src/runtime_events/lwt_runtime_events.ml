type Runtime_events.User.tag += Paused_count
let paused_count = Runtime_events.User.register "lwt-paused-count" Paused_count Runtime_events.Type.int
let emit_paused_count v = Runtime_events.User.write paused_count v

type Runtime_events.User.tag += Scheduler_call
let sch_call = Runtime_events.User.register "lwt-sch-call" Scheduler_call Runtime_events.Type.span
let emit_sch_call_begin () = Runtime_events.User.write sch_call Runtime_events.Type.Begin
let emit_sch_call_end () = Runtime_events.User.write sch_call Runtime_events.Type.End

type Runtime_events.User.tag += Scheduler_lap
let sch_lap = Runtime_events.User.register "lwt-sch-lap" Scheduler_lap Runtime_events.Type.unit
let emit_sch_lap v = Runtime_events.User.write sch_lap v

type Runtime_events.User.tag += Unix_job_count
let unix_job_count = Runtime_events.User.register "lwt-unix-job-count" Unix_job_count Runtime_events.Type.int
let emit_job_count v = Runtime_events.User.write unix_job_count v

(* TODO: optimise to not require allocation of the string *)
type Runtime_events.User.tag += Trace
let ss = Runtime_events.Type.register
  ~encode:(fun b s ->
    let l = min (String.length s) (Bytes.length b) in
    Bytes.blit_string s 0 b 0 l;
    l)
  ~decode:(fun b i -> Bytes.sub_string b 0 i)
let trace = Runtime_events.User.register "lwt-trace" Trace ss
let emit_trace s = Runtime_events.User.write trace s
