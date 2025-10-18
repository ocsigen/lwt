let () = Runtime_events.start ()

let rec ping () =
  let p = Lwt.pause () in
  let%lwt _ping = Lwt_unix.sleep 0.01 in
  pong p
and pong p =
  let%lwt _ = p in
  let%lwt _pong = Lwt_unix.sleep 0.01 in
  ping ()
;;

let rec eventually_crash n =
  if n < 0 then raise Exit else
    let%lwt do_some_work = Lwt_unix.sleep 0.004 in
    ignore do_some_work;
    eventually_crash (n - 1)
;;

let protek f =
  try%lwt f () with Exit -> exit 1

let () = Lwt_main.run begin
  let%lwt () = Lwt.pause () in
  Lwt.pick [ping (); protek (fun () -> eventually_crash 20)]
end
