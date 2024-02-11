

let main ~port ~n ~n_conn () : unit Lwt.t =
  let remaining = ref n in
  let all_done = ref 0 in

  let fut_exit, prom_exit = Lwt.wait () in

  Printf.printf "connecting to port %d\n%!" port;
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in

  let rec run_task () : unit Lwt.t =
    let n = !remaining in
    Printf.printf "running task %d\n%!" n;
    decr remaining;
    if n > 0 then (
      Lwt_io.with_connection addr (fun (ic,oc) ->
        let buf = Bytes.create 32 in

        for _j = 1 to 10 do
          Printf.printf "  t %d: writing\n%!" n;
          Lwt.await @@ Lwt_io.write oc "hello";
          Lwt.await @@ Lwt_io.flush oc;

          (* read back something *)
          let _n = Lwt.await @@ Lwt_io.read_into ic buf 0 (Bytes.length buf) in
          Printf.printf "  t %d: read back\n%!" n;
          ()
        done;
        Printf.printf "  t %d: closing connection\n%!" n;
        Lwt.return ()) |> Lwt.await;
      Printf.printf "  t %d: done with connection\n%!" n;

      (* run another task *)
      Lwt.async run_task;
    ) else (
      (* if we're the last to exit, resolve the promise *)
      let n_already_done = !all_done in
      incr all_done;

      if n_already_done = n_conn - 1 then (
        Printf.printf "all done\n%!";
        Lwt.wakeup prom_exit ()
      )
    );
    Printf.printf "exiting task %d\n%!" n;
    Lwt.return ()
  in

  (* start the first [n_conn] tasks *)
  for _i = 1 to n_conn do
    Lwt.async run_task
  done;

  (* exit when [fut_exit] is resolved *)
  fut_exit

let () =
  let port = ref 0 in
  let j = ref 4 in
  let n_conn = ref 100 in
  let n = ref 50_000 in

  let opts =
    [
      "-p", Arg.Set_int port, " port";
      "j", Arg.Set_int j, " number of threads";
      "-n", Arg.Set_int n, " total number of connections";
      "--n-conn", Arg.Set_int n_conn, " number of parallel connections";
    ]
    |> Arg.align
  in
  Arg.parse opts ignore "echo client";

  Lwt_engine.set @@ new Lwt_engine.libev ();
  Lwt_main.run @@ main ~port:!port ~n:!n ~n_conn:!n_conn ()
