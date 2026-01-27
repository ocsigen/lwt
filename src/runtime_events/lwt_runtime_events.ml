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

  type t =
    { kind: Runtime_events.Type.span;
      context: string option;
      filename: string;
      line: int; }

  let decode b i =
    let offset = 0 in
    (* BEGIN|END *)
    let kind = match Bytes.get b offset with
      | 'B' -> Runtime_events.Type.Begin
      | 'E' -> End
      | _ -> failwith "unreadable tag for labelled_span";
    in
    let offset = offset + 1 in
    (* context *)
    let context_size = Bytes.get_uint8 b offset in
    let offset = offset + 1 in
    let context = if context_size = 0 then None else Some (Bytes.sub_string b offset context_size) in
    let offset = offset + context_size in
    (* line *)
    let line = Bytes.get_uint16_be b offset in
    let offset = offset + 2 in
    (* fname *)
    let filename_size = Bytes.get_uint8 b offset in
    let offset = offset + 1 in
    let filename = Bytes.sub_string b offset filename_size in
    let offset = offset + filename_size in
    assert (offset = i);
    { kind; context; filename; line }

  let encode b { kind; context; filename; line } =
    let offset = 0 in
    (* BEGIN|END *)
    Bytes.set b offset (match kind with Begin -> 'B' | End -> 'E');
    let offset = offset + 1 in
    (* context *)
    let offset = 
      match context with
      | None -> Bytes.set_uint8 b offset 0; offset + 1
      | Some context ->
          Bytes.set_uint8 b offset (String.length context);
          let offset = offset + 1 in
          Bytes.blit_string context 0 b offset (String.length context);
          offset + String.length context
    in
    (* line *)
    Bytes.set_uint16_be b offset line;
    let offset = offset + 2 in
    (* filename *)
    let filename_truncated_length = min (String.length filename) (Bytes.length b - offset) in
    Bytes.set_uint8 b offset filename_truncated_length;
    let offset = offset + 1 in
    Bytes.blit_string filename 0 b offset filename_truncated_length;
    let offset = offset + filename_truncated_length in
    assert (offset <= Bytes.length b);
    offset

  let t : t Runtime_events.Type.t = Runtime_events.Type.register ~encode ~decode

  type Runtime_events.User.tag += T
  let span = Runtime_events.User.register "lwt-trace" T t
  let emit t = Runtime_events.User.write span t

end
