let () = Runtime_events.start ()

let rec stall d =
  let open Lwt.Syntax in
  Unix.sleepf d;
  let* () = Lwt.pause () in
  stall (d+.0.01)

let last_lap = ref 0L
let started = ref false


let detect () =
  let cursor = Runtime_events.create_cursor None in
  let cb_stall =
    Runtime_events.Callbacks.create ()
    |> Runtime_events.Callbacks.add_user_event
        Runtime_events.Type.unit
        (fun _ t e () ->
          match Runtime_events.User.tag e with
          | Lwt_main.Scheduler_lap ->
          let delta = Int64.sub (Runtime_events.Timestamp.to_int64 t) !last_lap in
          if delta > 1_000_000_000L (* 1 second *) then begin
            Printf.printf "ALARM: stall 1s+ CRASHING\n"; flush stdout;
            exit 0
          end;
          if delta > 500_000_000L (* 0.5 second *) then begin
            Printf.printf "ALARM: stall .5s+\n"; flush stdout
          end
          | _ -> ())
  in
  let rec detect_stall () =
      Unix.sleepf 0.01;
      let _ : int = Runtime_events.read_poll cursor cb_stall None in
      detect_stall ()
  in
  let cb_pre_start =
    Runtime_events.Callbacks.create ()
    |> Runtime_events.Callbacks.add_user_event
        Runtime_events.Type.span
        (fun _ t e ev ->
          match Runtime_events.User.tag e, ev with
          | Lwt_main.Scheduler_call, Runtime_events.Type.Begin ->
              last_lap := Runtime_events.Timestamp.to_int64 t;
              started := true
          | Lwt_main.Scheduler_call, Runtime_events.Type.End ->
              failwith "NO"
          | _ -> failwith "NOPE")
  in
  let rec detect_start () =
    if not !started then begin
      Unix.sleepf 0.001;
      let _ : int = Runtime_events.read_poll cursor cb_pre_start None in
      detect_start ()
      end else
      detect_stall ()
  in
  detect_start ()

let _ = Domain.spawn (fun () -> detect ())

let () = Lwt_main.run (stall 0.)
