open Lwt.Syntax

(* we don't call run in the root domain so we initialise by hand *)
let () = Lwt_unix.init_domain ()

let p_one, w_one = Lwt.wait ()
let v_one = 3
let p_two, w_two = Lwt.wait ()
let v_two = 2

let d_mult = Domain.spawn (fun () ->
  (* domain one: wait for value from domain two then work and then send a value *)
  Lwt_main.run (
    let* () = Lwt_unix.sleep 0.01 in
    let* v_two = p_two in
(*     Printf.printf "d%d  received %d\n" (Domain.self () :> int) v_two; *)
    let* () = Lwt_unix.sleep 0.1 in
    Lwt.wakeup w_one v_one;
(*     Printf.printf "d%d  sent %d\n" (Domain.self () :> int) v_one; *)
    Lwt.return (v_two * v_one)
  )
)
let d_sum = Domain.spawn (fun () ->
  Lwt_main.run (
    let () =
      (* concurrent thread within domain "two" send a value and then work and
         then wait for a value from domain one *)
      Lwt.dont_wait (fun () ->
        let* () = Lwt_unix.sleep 0.1 in
(*         Printf.printf "d%d  slept\n" (Domain.self () :> int); *)
        Lwt.wakeup w_two v_two;
(*         Printf.printf "d%d  sent %d\n" (Domain.self () :> int) v_two; *)
        Lwt.return ()
      )
      (fun _ -> exit 1)
    in
    let* v_one = p_one in
    Lwt.return (v_two + v_one)
  )
)


let mult = Domain.join d_mult
let sum = Domain.join d_sum

let () =
  if mult = v_one * v_two && sum = v_one + v_two then begin
    Printf.printf "basic: ✓\n";
    exit 0
  end else begin
    Printf.printf "basic: ×\n";
    exit 1
  end
