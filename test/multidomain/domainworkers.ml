open Lwt.Syntax

let rec worker recv_task f send_result =
  let* task = Lwt_stream.get recv_task in
  match task with
  | None ->
(*       let () = Printf.printf "worker(%d) received interrupt\n" (Domain.self () :> int); flush_all() in *)
      send_result None;
      Lwt.return ()
  | Some data ->
(*       let () = Printf.printf "worker(%d) received task (%S)\n" (Domain.self () :> int) data; flush_all() in *)
      let* result = f data in
      send_result (Some result);
(*       let () = Printf.printf "worker(%d) sent result (%d)\n" (Domain.self () :> int) result; flush_all() in *)
      let* () = Lwt.pause () in
      worker recv_task f send_result

let spawn_domain_worker f =
  let recv_task, send_task = Lwt_stream.create () in
  let recv_result, send_result = Lwt_stream.create () in
  let dw =
    Domain.spawn (fun () ->
      Lwt_unix.init_domain ();
      Lwt_main.run (
        let* () = Lwt.pause () in
        worker recv_task f send_result
      )
    )
  in
  send_task, dw, recv_result

let simulate_work data =
  let simulated_work_duration = String.length data in
  let* () = Lwt_unix.sleep (0.01 *. float_of_int simulated_work_duration) in
  Lwt.return (String.length data)

let input = [""; "adsf"; "lkjh"; "lkjahsdflkjahdlfkjha"; "0"; ""; ""; ""; ""; ""; "adf"; "ASDSKJLHDAS"; "WPOQIEU"; "DSFALKHJ"; ""; ""; ""; ""; "SD"; "SD"; "SAD; SD;SD"; "ad"; "...."]
let expected_result = List.fold_left (fun acc s -> acc + String.length s) 0 input

let main () =
  let send_task1, dw1, recv_result1 = spawn_domain_worker simulate_work in
  let send_task2, dw2, recv_result2 = spawn_domain_worker simulate_work in
  let l =
    Lwt_unix.init_domain ();
    Lwt_main.run (
      let* () = Lwt.pause () in
      let () = (* push work *)
        List.iteri
        (fun idx s -> if idx mod 3 = 0 then send_task1 (Some s) else send_task2 (Some s))
        input
      in
      send_task1 None;
      send_task2 None;
      let* lengths1 = Lwt_stream.fold (+) recv_result1 0
      and* lengths2 = Lwt_stream.fold (+) recv_result2 0
      in
      Lwt.return (lengths1 + lengths2)
    )
  in
  let () = Domain.join dw1 in
  let () = Domain.join dw2 in
  let code =
    if l = expected_result then begin
      Printf.printf "domain-workers: ✓\n";
      0
    end else begin
      Printf.printf "domain-workers: ×\n";
      1
    end
  in
  flush_all ();
  exit code

let () = main ()
