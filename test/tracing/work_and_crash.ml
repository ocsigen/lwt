let () = Runtime_events.start ()

let rec ping () =
  let p = Lwt.pause () in
  let%lwt _ping = Lwt_unix.sleep 0.01
  and _ping2 = Lwt_unix.sleep 0.01 in
  pong p
and pong p =
  let%lwt _ = p in
  let%lwt _pong = Lwt_unix.sleep 0.01 in
  ping ()
;;

let fibobo () =
  let rec fibobo n =
    if n <= 0 then Lwt.return 1 else
    let%lwt left =
      Lwt.with_value Lwt.tracing_context (Some "left") (fun () ->
        Lwt.bind (Lwt_unix.sleep 0.001) (fun () -> Lwt.bind (Lwt.pause ()) (fun () -> fibobo (n - 1))))
    and right =
      Lwt.with_value Lwt.tracing_context (Some "right") (fun () ->
        Lwt.bind (Lwt_unix.sleep 0.002) (fun () -> Lwt.bind (Lwt.pause ()) (fun () -> fibobo (n - 2))))
    in
    Lwt.return (left + right)
  in
  let%lwt () = Lwt_unix.sleep 0.02 in
  let%lwt _ = Lwt_list.map_s fibobo (List.init 6 (fun x -> x+2)) in
  fst (Lwt.task ()) (* never resolve *)
;;

let rec eventually_crash n =
  if n < 0 then raise Exit else
    let%lwt do_some_work = Lwt_unix.sleep 0.004 in
    ignore do_some_work;
    eventually_crash (n - 1)
;;

let eventually_crash n =
  Lwt.with_value Lwt.tracing_context (Some "crrrrash") (fun () -> eventually_crash n)

let protek f =
  try%lwt
    Lwt.with_value Lwt.tracing_context (Some "protekted") f
  with Exit -> exit 1

let () = Lwt_main.run begin
  let%lwt () = Lwt.pause () in
  Lwt.pick [
    (Lwt.with_value Lwt.tracing_context (Some "pingpong") ping);
    (Lwt.with_value Lwt.tracing_context (Some "fib") fibobo);
    protek (fun () -> eventually_crash 20)
  ]
end
