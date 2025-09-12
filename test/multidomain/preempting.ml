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

(* atomic just for debugging: to record when domains are finished *)
let x = Atomic.make 0

let domain_go_brrrrrrr n input = Domain.spawn (fun () ->
  flush_all ();
  Lwt_unix.init_domain ();
  let v = Lwt_main.run (
      let* () = Lwt.pause () in
      (* detach blocking work *)
      Lwt_list.map_p (Lwt_preemptive.detach simulate_work) input
    )
  in
  (* printing just for debug: to see when different domains are finished *)
  Printf.printf "domain #%d %d scheduler returned\n" n (Domain.self () :> int);
  flush_all ();
  Atomic.incr x;
  v
)

let () =
  let rec go n acc = function
    | [_] | [] ->
        Printf.printf "all domain started\n"; flush_all ();
        acc
    | (_ :: more) as wrk ->
        let expected = List.map String.length wrk in
        let acc = (n, expected, domain_go_brrrrrrr n wrk) :: acc in
        go (n + 1) acc more
  in
  let results = go 1 [] input in
  Unix.sleepf 5.; (* sleeping for debug: to observe that atomic is at max value *)
  Printf.printf "done debug-sleeping, about to join all domains (atomic=%d)\n" (Atomic.get x); flush_all ();
  let results = List.map (fun (n, e, d) ->
    Printf.printf "joining domain #%d (atomic=%d)\n" n (Atomic.get x); flush_all ();
    let d = Domain.join d in
    Printf.printf "joined domain #%d (atomic=%d)\n" n (Atomic.get x); flush_all ();
    (e, d)) results
  in
  let success =
    List.for_all
      (fun (expected, d) -> List.for_all2 Int.equal expected d)
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
