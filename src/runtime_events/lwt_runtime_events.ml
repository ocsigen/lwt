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

module Trace = struct

  type labelled_span = Runtime_events.Type.span * string
  let labelled_span : labelled_span Runtime_events.Type.t =
    Runtime_events.Type.register
      ~encode:(fun b (k, s) ->
        let () = match k with
          | Runtime_events.Type.Begin -> Bytes.set b 0 'B'
          | End -> Bytes.set b 0 'E'
        in
        let l = min (String.length s) (Bytes.length b - 1) in
        Bytes.blit_string s 0 b 1 l;
        (l + 1))
      ~decode:(fun b i ->
        if i < 1 then failwith "unreadable tag for labelled_span";
        let k = match Bytes.get b 0 with
          | 'B' -> Runtime_events.Type.Begin
          | 'E' -> End
          | _ -> failwith "unreadable tag for labelled_span";
        in
        let s = Bytes.sub_string b 1 (i - 1) in
        (k, s))

  type Runtime_events.User.tag += LabelledSpan
  let span = Runtime_events.User.register "lwt-trace" LabelledSpan labelled_span
  let emit_span labelled_span = Runtime_events.User.write span labelled_span

end
