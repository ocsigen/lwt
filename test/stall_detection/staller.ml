let () = Runtime_events.start ()

let () = Unix.sleep 2
let () = Lwt_main.run (Stallerlib.stall 0.)
