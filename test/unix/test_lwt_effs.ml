(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

let pool = Lwt_domain.setup_pool 4

type _ Effect.t +=
  | Strlen : string -> int Effect.t
  | Pause : unit Lwt.t Effect.t
  | Sleep : float -> unit Lwt.t Effect.t
  | Par : (unit -> int) -> int Lwt.t Effect.t

let effect_handler =
  let effc
  : type a b. b Effect.t -> ((b, a) Effect.Deep.continuation -> a) option
  = function
    | Strlen s -> Some (fun k -> Effect.Deep.continue k (String.length s))
    | Pause -> Some (fun k -> Effect.Deep.continue k (Lwt.pause ()))
    | Sleep f -> Some (fun k -> Effect.Deep.continue k (Lwt_unix.sleep f))
    | Par f -> Some (fun k ->
        let p = Lwt_domain.detach pool f () in
        Effect.Deep.continue k p)
    | _ -> None
  in
  { Effect.Deep.effc }

let rec inneficient_fib n =
  if n < 0 then raise (Invalid_argument "inneficient_fib");
  if n = 0 then 0 else if n = 1 then 1 else
  let a = inneficient_fib (n - 1) in
  let b = inneficient_fib (n - 2) in
  a + b

let test_perform_after_pauses () =
  let open Lwt.Syntax in
  let* () = Lwt.pause () in
  let n = Effect.perform (Strlen "") in
  assert (n = 0);
  let* () = Lwt.pause () in
  let n = Effect.perform (Strlen "foo") in
  assert (n = 3);
  let* () = Effect.perform Pause in
  let n = Effect.perform (Strlen "buzz") in
  assert (n = 4);
  let* () = Effect.perform Pause in
  let n = Effect.perform (Strlen "x") in
  assert (n = 1);
  let* () = Effect.perform Pause in
  let* u = Effect.perform (Par (fun () -> inneficient_fib 45))
  and* _ = Effect.perform (Par (fun () -> inneficient_fib 45))
  and* _ = Effect.perform (Par (fun () -> inneficient_fib 45))
  and* _ = Effect.perform (Par (fun () -> inneficient_fib 45))
  and* _ = Effect.perform (Par (fun () -> inneficient_fib 45))
  and* _ = Effect.perform (Par (fun () -> inneficient_fib 45))
  and* () = Effect.perform (Sleep 1.0)
  in
  Printf.printf "%d\n" u ;
  assert (u = 1134903170);
  Lwt.return_unit


let run () =
  Lwt_main.run
    ~effect_handler
    (test_perform_after_pauses ())

let () =
  Printf.printf "Testing effects\n";
  run ();
  Printf.printf "Tested effects: ✓\n";
  ()
