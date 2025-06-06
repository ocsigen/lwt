open Lwt.Syntax

(* we don't call run in the root domain so we initialise by hand *)
let () = Lwt_unix.init_domain ()

let p_one, w_one = Lwt.wait ()
let p_two, w_two = Lwt.wait ()

let d_one = Domain.spawn (fun () ->
  (* domain one: wait for value from domain two then work and then send a value *)
  Lwt_main.run (
    let* () = Lwt_unix.sleep 0.01 in
    let* v_two = p_two in
    Printf.printf "d%d  received %d\n" (Domain.self () :> int) v_two;
    flush_all ();
    let* () = Lwt_unix.sleep 0.1 in
    flush_all ();
    let v_one = 3 in
    Lwt.wakeup w_one v_one;
    Printf.printf "d%d  sent %d\n" (Domain.self () :> int) v_one;
    flush_all ();
    let* v_two = p_two and* v_one = p_one in
    Lwt.return (v_two * v_one)
  )
)
let d_two = Domain.spawn (fun () ->
  Lwt_main.run (
    let () =
      (* concurrent thread within domain "two" send a value and then work and
         then wait for a value from domain one *)
      Lwt.dont_wait (fun () ->
        let* () = Lwt_unix.sleep 0.1 in
        Printf.printf "d%d  slept\n" (Domain.self () :> int);
        flush_all ();
        let v_two = 2 in
        Lwt.wakeup w_two v_two;
        Printf.printf "d%d  sent %d\n" (Domain.self () :> int) v_two;
        flush_all ();
        let* from_one = p_one in
        Printf.printf "d%d  received %d\n" (Domain.self () :> int) from_one;
        flush_all ();
        Lwt.return ()
      )
      (fun _ -> exit 1)
    in
    let* v_two = p_two and* v_one = p_one in
    Lwt.return (v_two + v_one)
  )
)


let one = Domain.join d_one
let two = Domain.join d_two

let () = Printf.printf "product: %d, sum: %d\n" one two
let () = flush_all ()
