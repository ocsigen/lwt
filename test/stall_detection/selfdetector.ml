let () = Runtime_events.start ()

let _ : _ Domain.t = Domain.spawn (fun () -> Stallerlib.detect ())

let () =
  Printf.printf "start\n"; flush stdout;
  Lwt_main.run (Stallerlib.stall 0.)
