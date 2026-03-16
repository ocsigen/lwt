(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)


(* An instance of the tester for the wait/waitpid tests. *)
let () =
  match Sys.argv with
  | [|_; "--child"|] ->
    exit 42
  | _ ->
    ()

let wait_tests = [
  Test.test "wait" ~sequential:true ~only_if:(fun () -> not Sys.win32) begin fun () ->
    let open Lwt.Infix in
    match Lwt_unix.fork () with
    | 0 ->
      Unix.execv Sys.argv.(0) [|""; "--child"|]
    | child_pid ->
      Lwt_unix.wait () >|= function
      | exited_pid, Lwt_unix.WEXITED 42 when exited_pid = child_pid -> true
      | _ -> false
  end;

  Test.test "waitpid" ~sequential:true ~only_if:(fun () -> not Sys.win32)
      begin fun () ->
    let open Lwt.Infix in
    match Lwt_unix.fork () with
    | 0 ->
      Unix.execv Sys.argv.(0) [|""; "--child"|]
    | child_pid ->
      Lwt_unix.waitpid [] child_pid >|= function
      | exited_pid, Lwt_unix.WEXITED 42 when exited_pid = child_pid -> true
      | _ -> false
  end;

  Test.test "waitpid: any child" ~sequential:true ~only_if:(fun () -> not Sys.win32)
      begin fun () ->
    let open Lwt.Infix in
    match Lwt_unix.fork () with
    | 0 ->
      Unix.execv Sys.argv.(0) [|""; "--child"|]
    | child_pid ->
      Lwt_unix.waitpid [] 0 >|= function
      | exited_pid, Lwt_unix.WEXITED 42 when exited_pid = child_pid -> true
      | _ -> false
  end;

  Test.test "wait4" ~sequential:true ~only_if:(fun () -> not Sys.win32)
      begin fun () ->
    let open Lwt.Infix in
    match Lwt_unix.fork () with
    | 0 ->
      Unix.execv Sys.argv.(0) [|""; "--child"|]
    | child_pid ->
      Lwt_unix.wait4 [] child_pid >|= function
      | exited_pid, Lwt_unix.WEXITED 42, _ when exited_pid = child_pid -> true
      | _ -> false
  end;

  Test.test "wait4: any child" ~sequential:true ~only_if:(fun () -> not Sys.win32)
      begin fun () ->
    let open Lwt.Infix in
    match Lwt_unix.fork () with
    | 0 ->
      Unix.execv Sys.argv.(0) [|""; "--child"|]
    | child_pid ->
      Lwt_unix.wait4 [] 0 >|= function
      | exited_pid, Lwt_unix.WEXITED 42, _ when exited_pid = child_pid -> true
      | _ -> false
  end;
]

let () =
  Test.concurrent "unix.wait" [
    Test.suite "lwt_unix.wait" wait_tests
  ]
