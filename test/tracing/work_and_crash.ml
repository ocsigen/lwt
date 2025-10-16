let () = Runtime_events.start ()

let rec ping () =
  let%lwt foo = Lwt_unix.sleep 0.01 in
  pong foo
and pong () =
  let%lwt bar = Lwt_unix.sleep 0.01 in
  pong bar
;;

let rec eventually_crash n =
  if n < 0 then exit 1 else
    let%lwt do_some_work = Lwt_unix.sleep 0.004 in
    ignore do_some_work;
    eventually_crash (n - 1)
;;

let () = Lwt_main.run begin
  let%lwt () = Lwt.pause () in
  Lwt.pick [ping (); eventually_crash 20]
end
