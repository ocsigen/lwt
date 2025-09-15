open Lwt.Syntax


let input = ["adsf"; "lkjahsdflkjahdlfkjhaadslfhlasfdasdf"; "0"; ""; "ahlsdfjk"]
let simulate_work data =
  let simulated_work_duration = String.length data in
  let () =
    (* each bit of work is blocking and will use preemptive *)
    Unix.sleepf (0.001 *. float_of_int simulated_work_duration)
  in
  String.length data

let () = Lwt_unix.init_domain ()

let domain_go_brrrrrrr input = Domain.spawn (fun () ->
  flush_all ();
  Lwt_unix.init_domain ();
  let v = Lwt_main.run (
      let* () = Lwt.pause () in
      (* detach blocking work *)
      Lwt_list.map_p (Lwt_preemptive.detach simulate_work) input
    )
  in
  Lwt_preemptive.kill_all ();
  v
)

let () =
  let rec go acc = function
    | [_] | [] ->
        acc
    | (_ :: more) as wrk ->
        let expected = List.map String.length wrk in
        let acc = (expected, domain_go_brrrrrrr wrk) :: acc in
        go acc more
  in
  let results = go [] input in
  let success =
    List.for_all
      (fun (expected, d) -> List.for_all2 Int.equal expected (Domain.join d))
      results
  in
  let code =
    if success then begin
      Printf.printf "preempting: ✓\n";
      0
    end else begin
      Printf.printf "preempting: ×\n";
      1
    end
  in
  flush_all ();
  exit code
