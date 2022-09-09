(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

open Test

type _ Effect.t +=
  | Strlen : string -> int Effect.t
  | Pause : unit Lwt.t Effect.t


let effect_handler =
  let effc
  : type a b. b Effect.t -> ((b, a) Effect.Deep.continuation -> a) option
  = function
    | Strlen s -> Some (fun k -> Effect.Deep.continue k (String.length s))
    | Pause -> Some (fun k -> Effect.Deep.continue k (Lwt.pause ()))
    | _ -> None
  in
  { Effect.Deep.effc }

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
