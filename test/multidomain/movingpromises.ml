open Lwt.Syntax

let rec worker ongoing_tasks recv_task f =
  let* task = Lwt_stream.get recv_task in
  match task with
  | None ->
      let () = Printf.printf "worker(%d) received interrupt\n" (Domain.self () :> int); flush_all() in
      Lwt.join ongoing_tasks
  | Some (idx, data, resolver) ->
      let task =
        let () = Printf.printf "worker(%d) received task(%d)\n" (Domain.self () :> int) idx; flush_all() in
        let* data in
        let () = Printf.printf "worker(%d) received task(%d) data(%S)\n" (Domain.self () :> int) idx data; flush_all() in
        let* result = f data in
        Lwt.wakeup resolver result;
        let () = Printf.printf "worker(%d) sent result(%d) for task(%d)\n" (Domain.self () :> int) result idx; flush_all() in
        Lwt.return ()
      in
      let* () = Lwt.pause () in
      worker (task :: ongoing_tasks) recv_task f

let spawn_domain_worker f =
  let recv_task, send_task = Lwt_stream.create () in
  let dw =
    Domain.spawn (fun () ->
      Lwt_main.run (
        let* () = Lwt.pause () in
        worker [] recv_task f
      )
    )
  in
  send_task, dw

let simulate_work data =
  let simulated_work_duration = String.length data in
  let* () = Lwt_unix.sleep (0.01 *. float_of_int simulated_work_duration) in
  Lwt.return (String.length data)

let simulate_input data =
  let simulated_work_duration = max 1 (10 - String.length data) in
  let* () = Lwt_unix.sleep (0.01 *. float_of_int simulated_work_duration) in
  Lwt.return data

let main () =
  let send_task1, dw1 = spawn_domain_worker simulate_work in
  let send_task2, dw2 = spawn_domain_worker simulate_work in
  let l =
    Lwt_main.run (
      let* () = Lwt.pause () in
      let inputs = List.map simulate_input
        [""; "adsf"; "lkjh"; "lkjahsdflkjahdlfkjha"; "0"; ""; ""; ""; ""; ""; "adf"; "ASDSKJLHDAS"; "WPOQIEU"; "DSFALKHJ"; ""; ""; ""; ""; "SD"; "SD"; "SAD; SD;SD"; "ad"; "...."]
      in
      let* lengths =
        Lwt_list.mapi_p
        (fun idx s ->
          let (p, r) = Lwt.task () in
          begin if idx mod 3 = 0 then send_task1 (Some (idx, s, r)) else send_task2 (Some (idx, s, r)) end;
          p)
        inputs
      in
      let* () = Lwt.pause () in
      send_task1 None;
      send_task2 None;
      let lengths = lengths |> List.map string_of_int |> String.concat "," in
      Lwt.return lengths
    )
  in
  let () = Domain.join dw1 in
  let () = Domain.join dw2 in
  Printf.printf "lengths: %s\n" l;
  flush_all ();
  exit 0

let () = main ()
