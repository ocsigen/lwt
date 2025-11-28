(* STALLING *)

let () = Random.self_init ()

let to_s_ns f =
  let ns = int_of_float (f *. 1_000_000_000.) in
  (ns / 1_000_000_000 , ns mod 1_000_000_000)

let rec stall d =
  let open Lwt.Syntax in
  let s, ns = to_s_ns d in
  Printf.printf "stalling for %d.%09d\n" s ns;
  flush stdout;
  Unix.sleepf d;
  let* () = Lwt.pause () in
  if d > 3. then exit 2 else stall (1.2*.(d+. Random.float 0.05))


(* DETECTING *)

(* set to maxint to avoid the first hit being a false positive *)
let last_lap = ref Int64.max_int
let alarmed = ref false

let detect ?ringopt () =
  begin match ringopt with
  | None -> Printf.printf "starting detection on self (%d)\n" (Unix.getpid ())
  | Some (path, pid) -> Printf.printf "starting detection on %s/%d.events\n" path pid
  end;
  flush stdout;
  let cursor = Runtime_events.create_cursor ringopt in
  let is_stall t =
    let delta = Int64.sub (Runtime_events.Timestamp.to_int64 t) !last_lap in
    if delta > 1_000_000_000L (* 1 second *) then begin
      Printf.printf "ALARM: stall 1s+ CRASHING\n"; flush stdout;
      (match ringopt with
      | None -> exit 1
      | Some (_, pid) -> Unix.kill pid Sys.sigkill)
    end;
    if not !alarmed && delta > 500_000_000L (* 0.5 second *) then begin
      alarmed := true;
      Printf.printf "ALARM: stall .5s+\n"; flush stdout
    end
  in
  let cb =
    Runtime_events.Callbacks.create ()
    |> Runtime_events.Callbacks.add_user_event
        Runtime_events.Type.unit
        (fun _ t e () ->
          match Runtime_events.User.tag e with
          | Lwt_runtime_events.Scheduler_lap ->
              alarmed := false;
              last_lap := Runtime_events.Timestamp.to_int64 t
          | _ -> ())
  in
  let rec detect () =
    Unix.sleepf 0.01;
    let _ : int = Runtime_events.read_poll cursor cb None in
    is_stall (Runtime_events.Timestamp.get_current ());
    detect ()
  in
  detect ()
