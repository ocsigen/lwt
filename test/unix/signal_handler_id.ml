(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* Two handlers of one signal, installed with on_signal_full. The second
   disables itself with the id it receives: the first must keep running. *)

let () =
  if Sys.win32
  then print_endline "the id passed to a signal handler: skipped on Windows"
  else begin
    let first = ref 0 and second = ref 0 in
    let h1 = Lwt_unix.on_signal_full Sys.sigusr2 (fun _ _ -> incr first) in
    let _h2 =
      Lwt_unix.on_signal_full Sys.sigusr2 (fun id _ ->
        incr second;
        Lwt_unix.disable_signal_handler id)
    in
    (* Runs the loop until [!first] reaches [n], or for at most 5 seconds. *)
    let wait_for n =
      let rec loop tries =
        if !first < n && tries > 0
        then begin
          Lwt_main.run (Lwt_unix.sleep 0.01);
          loop (tries - 1)
        end
      in
      loop 500
    in
    Unix.kill (Unix.getpid ()) Sys.sigusr2;
    wait_for 1;
    Unix.kill (Unix.getpid ()) Sys.sigusr2;
    wait_for 2;
    Lwt_unix.disable_signal_handler h1;
    if !first <> 2 || !second <> 1
    then begin
      Printf.eprintf
        "FAILED: the first handler ran %d times, the second %d times, not 2 and 1\n"
        !first !second;
      exit 1
    end;
    print_endline "the id passed to a signal handler: ok"
  end
