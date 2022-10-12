(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

open Lwt.Syntax

let test () =

  begin
  try
    let () = Lwt_main.run (
      let* () = Lwt.pause () in
      if true then raise Out_of_memory else Lwt.return_unit
    ) in
    Printf.eprintf "Test run+raise failure\n";
    Stdlib.exit 1
  with
    | Out_of_memory -> ()
  end;

  begin
  try
    let () = Lwt_main.run (
      let* () = Lwt_unix.sleep 0.001 in
      if true then raise Out_of_memory else Lwt.return_unit
    ) in
    Printf.eprintf "Test run+raise failure\n";
    Stdlib.exit 1
  with
    | Out_of_memory -> ()
  end;

  begin
  try
    let () = Lwt_main.run (
      let* () = Lwt.pause () in
      Lwt.choose [
        (let* () = Lwt.pause () in raise Out_of_memory);
        Lwt_unix.sleep 2.;
      ]
    ) in
    Printf.eprintf "Test run+raise failure\n";
    Stdlib.exit 1
  with
    | Out_of_memory -> ()
  end;

  begin
  try
    let () = Lwt_main.run (
      let* () = Lwt.pause () in
      Lwt.catch
        (fun () -> raise Out_of_memory)
        (fun _ -> Lwt.return_unit)
    ) in
    Printf.eprintf "Test run+raise failure\n";
    Stdlib.exit 1
  with
    | Out_of_memory -> ()
  end;

  begin
  try
    let () = Lwt_main.run (
      let* () = Lwt.pause () in
      Lwt.try_bind
        (fun () -> raise Out_of_memory)
        (fun () -> Lwt.return_unit)
        (fun _ -> Lwt.return_unit)
    ) in
    Printf.eprintf "Test run+raise failure\n";
    Stdlib.exit 1
  with
    | Out_of_memory -> ()
  end;

  begin
  try
    let () = Lwt_main.run (
      let* () = Lwt.pause () in
      let _ =
        Lwt.async
          (fun () -> let* () = Lwt.pause () in raise Out_of_memory)
      in
      Lwt_unix.sleep 0.5
    ) in
    Printf.eprintf "Test run+raise failure\n";
    Stdlib.exit 1
  with
    | Out_of_memory -> ()
  end;

  begin
  try
    let () = Lwt_main.run (
      let* () = Lwt.pause () in
      let _ =
        Lwt.dont_wait
          (fun () -> let* () = Lwt.pause () in raise Out_of_memory)
          (fun _ -> ())
      in
      Lwt_unix.sleep 0.5
    ) in
    Printf.eprintf "Test run+raise failure\n";
    Stdlib.exit 1
  with
    | Out_of_memory -> ()
  end;

  Printf.printf "Test run+raise ok\n";
  ()
