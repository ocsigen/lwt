let () = Unix.sleepf 0.05
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
  let%lwt store = Lwt_io.create_temp_dir ~parent:"/tmp" ~prefix:"fibobo" () in
  let rec fibobo n =
    if n <= 0 then Lwt.return 1 else
    match%lwt
      let%lwt fd = Lwt_unix.openfile (store ^ "/" ^ (string_of_int n)) [O_RDONLY; O_NONBLOCK] 0o775 in
      let ic = Lwt_io.of_fd ~mode:Input fd in
      let%lwt v = Lwt_io.read ic in
      let%lwt () = Lwt_unix.close fd in
      Lwt.return (int_of_string v)
    with
    | v -> Lwt.return v
    | exception _ ->
        let%lwt left =
          let open Lwt.Syntax in
          let* () = Lwt_unix.sleep 0.002 in
          let* () = Lwt.pause () in
          fibobo (n - 1)
        and right =
          let open Lwt.Syntax in
          let* () = Lwt_unix.sleep 0.0001 in
          let* () = Lwt.pause () in
          fibobo (n - 2)
        in
        let v = left + right in
        let%lwt fd = Lwt_unix.openfile (store ^ "/" ^ (string_of_int n)) [O_WRONLY; O_CREAT; O_TRUNC] 0o775 in
        let oc = Lwt_io.of_fd ~mode:Output fd in
        let%lwt () = Lwt_io.write oc (string_of_int v) in
        Lwt.return v
  in
  let%lwt () = Lwt_unix.sleep 0.02 in
  let%lwt _ = Lwt_list.map_s fibobo (List.init 12 (fun x -> 6*x+6)) in
  fst (Lwt.task ()) (* never resolve *)
;;

let eventually_crash n =
  for%lwt i = 0 to n do
    let%lwt do_some_work = Lwt_unix.sleep 0.004 in
    ignore do_some_work;
    Lwt.return ()
  done;%lwt
  raise Exit
;;

let eventually_crash n =
  Lwt.with_tracing_context "crrrrash" (fun () -> eventually_crash n)

let protek f =
  try%lwt
    Lwt.with_tracing_context "protekted" f
  with Exit -> exit 1

let () = Lwt_main.run begin
  let%lwt () = Lwt.pause () in
  Lwt.pick [
    (Lwt.with_tracing_context "pingpong" ping);
    (Lwt.with_tracing_context "fib" fibobo);
    protek (fun () -> eventually_crash 50)
  ]
end
