(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* set the exception filter being tested *)
let () = Lwt.Exception_filter.(set handle_all_except_runtime)

(* OCaml runtime exceptions (out-of-memory, stack-overflow) are fatal in a
   different way than other exceptions and they leave the Lwt main loop in an
   inconsistent state where it cannot be restarted. Indeed, attempting to call
   [Lwt_main.run] again after it has crashed with a runtime exception causes a
   "Nested calls to Lwt_main.run are not allowed" error.

   For this reason, we run this test as its own executable rather than as part
   of a larger suite. *)

open Lwt.Syntax

let test () =
  try
    let () = Lwt_main.run (
      let* () = Lwt.pause () in
      if true then raise Out_of_memory else Lwt.return_unit
    ) in
    Printf.eprintf "Test run+raise failure\n";
    Stdlib.exit 1
  with
    | Out_of_memory -> ()

let () = test ()
