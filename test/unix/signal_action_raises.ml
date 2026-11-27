(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* Two handlers of one signal, the first raising: the second must run anyway,
   and the exception must go to [Lwt.async_exception_hook]. *)

exception Handler_failed

let () =
  if Sys.win32
  then print_endline "a signal handler that raises: skipped on Windows"
  else begin
    let hooked = ref [] in
    (Lwt.async_exception_hook := fun exn -> hooked := exn :: !hooked);
    let second_ran, second = Lwt.wait () in
    let h1 = Lwt_unix.on_signal Sys.sigusr1 (fun _ -> raise Handler_failed) in
    let h2 =
      Lwt_unix.on_signal Sys.sigusr1 (fun _ -> Lwt.wakeup_later second true)
    in
    Unix.kill (Unix.getpid ()) Sys.sigusr1;
    let ok =
      match
        Lwt_main.run
          (Lwt.pick [second_ran; Lwt.map (fun () -> false) (Lwt_unix.sleep 5.)])
      with
      | ok -> ok
      | exception Handler_failed -> false
    in
    Lwt_unix.disable_signal_handler h1;
    Lwt_unix.disable_signal_handler h2;
    if not ok
    then begin
      prerr_endline "FAILED: a signal handler that raises skips the next ones";
      exit 1
    end;
    if !hooked <> [Handler_failed]
    then begin
      prerr_endline "FAILED: the exception does not go to async_exception_hook";
      exit 1
    end;
    print_endline "a signal handler that raises: ok"
  end
