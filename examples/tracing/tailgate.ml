let tmpdir = Sys.argv.(1)
let command = Sys.argv.(2)

let () =
  if Sys.file_exists tmpdir then () else
  Unix.mkdir tmpdir 0o777
let pid =
  Unix.create_process_env command [||]
    [|"OCAML_RUNTIME_EVENTS_DIR="^tmpdir|]
    Unix.stdin Unix.stdout Unix.stderr

let () = Printf.printf "started %s (pid: %d)\n" command pid

let () = Unix.sleepf 0.001
let cursor =
  let rec get n =
    if n < 0 then exit 2 else
      try Runtime_events.create_cursor (Some (tmpdir, pid))
      with _ -> begin Unix.sleepf 0.005; get (n - 1) end
  in
  get 100

let (code, _) = Unix.waitpid [WUNTRACED] pid
let () = Printf.printf "crash! (%d)\n" code

let simplify_fname fname =
  String.split_on_char '/' fname
  |> List.rev
  |> function
    | [] -> assert false
    | hd :: tl ->
        (hd :: "/" :: List.map (fun s -> String.make 1 s.[0]) tl)
    |> List.rev
    |> String.concat ""

let name_of { Lwt_runtime_events.Trace.context; filename; line; kind=_ } =
  let shortloc = simplify_fname filename ^ ":" ^ string_of_int line in
  match context with
  | Some c -> c ^ "@" ^ shortloc
  | None -> shortloc

let ts_to_us ts = Int64.(div ts (of_int 1000))

let oc = open_out_bin (Printf.sprintf "%s/tailtrace-%d.json" tmpdir pid)
let () =
  Printf.fprintf oc "[";
  let cb =
    Runtime_events.Callbacks.create ()
    |> Runtime_events.Callbacks.add_user_event
        Lwt_runtime_events.Trace.t
        (fun rid t u x ->
          match Runtime_events.User.tag u with
            | Lwt_runtime_events.Trace.T -> begin
                let { Lwt_runtime_events.Trace.kind; context=_; filename; line } = x in
                    Printf.fprintf oc
                      "{\"name\": \"%s\", \"cat\": \"PERF\", \"ph\":\"%s\", \"ts\":%Ld, \"pid\": %d, \"tid\": %d, \"args\": {\"location\": \"%s:%d\"}},\n"

                      (name_of x)
                      (match kind with Begin -> "B" | End -> "E")
                      (ts_to_us (Runtime_events.Timestamp.to_int64 t))
                      rid
                      rid
                      filename
                      line
            end
          | _ -> ())
    |> Runtime_events.Callbacks.add_user_event
        Runtime_events.Type.unit
        (fun rid t e () ->
          match Runtime_events.User.tag e with
          | Lwt_runtime_events.Scheduler_lap ->
              Printf.fprintf oc
                "{\"name\": \"%s\", \"cat\": \"PERF\", \"ph\":\"%s\", \"ts\":%Ld, \"pid\": %d, \"tid\": %d},\n"

                "lap"
                "i"
                (ts_to_us (Runtime_events.Timestamp.to_int64 t))
                rid
                rid
          | _ -> ())
    |> Runtime_events.Callbacks.add_user_event
        Runtime_events.Type.int
        (fun rid t u x ->
          match Runtime_events.User.tag u with
          | Lwt_runtime_events.Paused_count ->
                    Printf.fprintf oc
                      "{\"name\": \"%s\", \"cat\": \"PERF\", \"ph\":\"%s\", \"ts\":%Ld, \"pid\": %d, \"tid\": %d, \"args\": {\"%s\": %d}},\n"
                      "paused"
                      "C"
                     (ts_to_us (Runtime_events.Timestamp.to_int64 t))
                      rid
                      rid
                      "paused"
                      x
          | Lwt_runtime_events.Unix_job_count ->
                    Printf.fprintf oc
                      "{\"name\": \"%s\", \"cat\": \"PERF\", \"ph\":\"%s\", \"ts\":%Ld, \"pid\": %d, \"tid\": %d, \"args\": {\"%s\": %d}},\n"
                      "jobs"
                      "C"
                      (ts_to_us (Runtime_events.Timestamp.to_int64 t))
                      rid
                      rid
                      "jobs"
                      x
          | _ -> ())
  in
  let _ : int = Runtime_events.read_poll cursor cb None in
  Printf.fprintf oc "]";
  ()

let () = Printf.printf "writing trace file %s/%d.tail\n" tmpdir pid
let () = flush_all ()
let () = exit 0
