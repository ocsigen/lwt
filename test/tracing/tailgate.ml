let tmpdir = Sys.argv.(1)
let command = Sys.argv.(2)

let () =
  if Sys.file_exists tmpdir then () else
  Unix.mkdir tmpdir 0o777
let pid =
  Unix.create_process_env command [||]
    [|"OCAML_RUNTIME_EVENTS_DIR="^tmpdir|]
    Unix.stdin Unix.stdout Unix.stderr

let cursor =
  let rec get n =
    if n < 0 then exit 2 else
      try Runtime_events.create_cursor (Some (tmpdir, pid))
      with _ -> begin Unix.sleepf 0.005; get (n - 1) end
  in
  get 10

let (_, _) = Unix.waitpid [WUNTRACED] pid

let () = Printf.printf "crash! writing trace file %s/%d.tail\n" tmpdir pid
let () =
  let buf_pool = Trace_fuchsia.Buf_pool.create () in
  let buf = Trace_fuchsia.Buf_chain.create ~sharded:false ~buf_pool () in
  let oc = open_out_bin (Printf.sprintf "%s/%d.tail" tmpdir pid) in
  let { Trace_fuchsia.Exporter.write_bufs; flush; close } as exporter = Trace_fuchsia.Exporter.of_out_channel ~close_channel:true oc in
  let subscriber = Trace_fuchsia.Subscriber.create ~buf_pool ~pid:0 ~exporter () in
  Trace_fuchsia.Subscriber.Callbacks.on_init subscriber ~time_ns:0L;
  let cb =
    Runtime_events.Callbacks.create ()
    |> Runtime_events.Callbacks.add_user_event
        Lwt_runtime_events.ss
        (fun _ t u x ->
          match Runtime_events.User.tag u with
          | Lwt_runtime_events.Trace ->
              Trace_fuchsia.Writer.Event.Instant.encode buf ~name:"bind"
              ~t_ref:(Ref 1) ~time_ns:(Runtime_events.Timestamp.to_int64 t) ~args:["location",A_string x] ()
          | _ -> ())
  in
  let _ : int = Runtime_events.read_poll cursor cb None in
  Trace_fuchsia.Buf_chain.ready_all_non_empty buf;
  Trace_fuchsia.Buf_chain.pop_ready ~f:write_bufs buf;
  flush ();
  Trace_fuchsia.Subscriber.close subscriber;
  close ();
  ()

let () = exit 0
