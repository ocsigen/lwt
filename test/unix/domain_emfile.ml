(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* Creating a loop's notification channel with no descriptor left must fail
   with EMFILE, and the next loop must be created once descriptors are back.
   A failure that raised while holding the table's mutex would hang every
   later loop. The test re-runs itself through a shell that lowers the
   descriptor limit. Needs a second domain, hence OCaml 5. *)

let failures = ref 0

let check name b =
  if not b
  then (
    Printf.eprintf "FAILED: %s\n" name;
    incr failures)

let check_under_low_limit () =
  (* Deadlocks must show as a failure, not as a hang. *)
  ignore (Unix.alarm 10);
  let held = ref [] in
  (try
     while true do
       held := Unix.openfile "/dev/null" [Unix.O_RDONLY] 0 :: !held
     done
   with Unix.Unix_error (Unix.EMFILE, _, _) -> ());
  let first =
    Domain.join
      (Domain.spawn (fun () ->
         match Lwt_unix.make_notification (fun () -> ()) with
         | _ -> `Created
         | exception Unix.Unix_error (Unix.EMFILE, _, _) -> `Emfile
         | exception _ -> `Other))
  in
  check "with no descriptor left, creating a loop fails with EMFILE"
    (first = `Emfile);
  List.iter Unix.close !held;
  let second =
    Domain.join
      (Domain.spawn (fun () ->
         match Lwt_unix.make_notification (fun () -> ()) with
         | _ -> true
         | exception _ -> false))
  in
  check "and once descriptors are back, the next loop is created" second;
  if !failures > 0 then exit 1

let () =
  match Sys.argv with
  | [|_; "--under-low-limit"|] -> check_under_low_limit ()
  | _ ->
      let child =
        Unix.create_process "sh"
          [| "sh"
           ; "-c"
           ; "ulimit -n 64 && exec \"$0\" --under-low-limit"
           ; Sys.executable_name |]
          Unix.stdin Unix.stdout Unix.stderr
      in
      let _, status = Unix.waitpid [] child in
      if status <> Unix.WEXITED 0 then exit 1;
      print_endline "loop creation under EMFILE: ok"
