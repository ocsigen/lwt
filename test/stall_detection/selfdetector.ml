let () = Runtime_events.start ()

let rec stall d =
  let open Lwt.Syntax in
  Printf.printf "stalling for %dns\n" (int_of_float (d *. 1_000_000_000.));
  flush stdout;
  Unix.sleepf d;
  let* () = Lwt.pause () in
  stall (1.5*.(d+.0.01))

(* set to maxint to avoid the first hit being a false positive *)
let last_lap = ref Int64.max_int

let cursor = Runtime_events.create_cursor None
let cb =
  Runtime_events.Callbacks.create ()
  |> Runtime_events.Callbacks.add_user_event
      Runtime_events.Type.unit
      (fun _ t e () ->
        match Runtime_events.User.tag e with
        | Lwt_main.Scheduler_lap ->
        let delta = Int64.sub (Runtime_events.Timestamp.to_int64 t) !last_lap in
        last_lap := Runtime_events.Timestamp.to_int64 t;
        if delta > 1_000_000_000L (* 1 second *) then begin
          Printf.printf "ALARM: stall 1s+ CRASHING\n"; flush stdout;
          exit 0
        end;
        if delta > 500_000_000L (* 0.5 second *) then begin
          Printf.printf "ALARM: stall .5s+\n"; flush stdout
        end
        | _ -> ())
let rec detect () =
  Unix.sleepf 0.01;
  let _ : int = Runtime_events.read_poll cursor cb None in
  detect ()

let _ : _ Domain.t = Domain.spawn detect

let () =
  Printf.printf "start\n"; flush stdout;
  Lwt_main.run (stall 0.)
