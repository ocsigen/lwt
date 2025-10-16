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

let () = Printf.printf "crash!\n"
let () =
  let cb =
    Runtime_events.Callbacks.create ()
    |> Runtime_events.Callbacks.add_user_event
        Lwt_runtime_events.ss
        (fun _ t u x ->
          match Runtime_events.User.tag u with
          | Lwt_runtime_events.Trace ->
              Printf.printf "[%Ld] %s\n" (Runtime_events.Timestamp.to_int64 t) x
          | _ -> ())
  in
  let _ : int = Runtime_events.read_poll cursor cb None in
  ()

let () = flush stdout
let () = exit 0
