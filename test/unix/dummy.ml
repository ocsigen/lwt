(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



let test_input_str = "the quick brown fox jumps over the lazy dog"
let test_input = Bytes.of_string test_input_str
let test_input_len = Bytes.length test_input

let read () =
  let buf = Bytes.create test_input_len in
  let rec aux n =
    let i = Unix.read Unix.stdin buf n (Bytes.length buf - n) in
    if i = 0 || n + i = test_input_len then
      Bytes.equal buf test_input
    else aux (n + i)
  in
  if aux 0 then
    (* make sure there's nothing more to read *)
    0 = Unix.read Unix.stdin buf 0 1
  else false

let write fd =
  assert (test_input_len = Unix.write fd test_input 0 test_input_len)

let printenv () =
  (* stdout is in text mode by default, which converts \n to \r\n on Windows.
    switch to binary mode to prevent this, so the output is the same across
    platforms. *)
  set_binary_mode_out stdout true;
  Array.iter (Printf.printf "%s\n") (Unix.unsafe_environment ());
  flush stdout

let () =
  match Sys.argv.(1) with
  | "read" -> exit @@ if read () then 0 else 1
  | "write" -> write Unix.stdout
  | "errwrite" -> write Unix.stderr
  | "printenv" -> printenv ()
  | _ -> invalid_arg "Sys.argv"
