(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* The CLOEXEC tests use execv(2) to execute this code, by passing --cloexec to
   the copy of the tester in the child process. This is a module side effect
   that interprets that --cloexec argument. *)
let () =
  let is_fd_open fd =
    let fd  = (Obj.magic (int_of_string fd) : Unix.file_descr) in
    let buf = Bytes.create 1 in
    try
      ignore (Unix.read fd buf 0 1);
      true
    with Unix.Unix_error (Unix.EBADF, _, _) ->
      false
  in

  match Sys.argv with
  | [|_; "--cloexec"; fd; "--open"|] ->
    if is_fd_open fd then
      exit 0
    else
      exit 1
  | [|_; "--cloexec"; fd; "--closed"|] ->
    if is_fd_open fd then
      exit 1
    else
      exit 0
  | _ ->
    ()

let test_cloexec ~closed flags =
  let open Lwt.Infix in
  Lwt_unix.openfile "/dev/zero" (Unix.O_RDONLY :: flags) 0o644 >>= fun fd ->
  match Lwt_unix.fork () with
  | 0 ->
    let fd = string_of_int (Obj.magic (Lwt_unix.unix_file_descr fd)) in
    let expected_status = if closed then "--closed" else "--open" in
    (* There's no portable way to obtain the tester executable name (which may
       even no longer exist at this point), but argv[0] fortunately has the
       right value when the tests are run in the Lwt dev environment. *)
    Unix.execv Sys.argv.(0) [|""; "--cloexec"; fd; expected_status|]
  | n ->
    Lwt_unix.close fd >>= fun () ->
    Lwt_unix.waitpid [] n >>= function
    | _, Unix.WEXITED 0 -> Lwt.return_true
    | _, (Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _) ->
      Lwt.return_false

let openfile_tests = [
  Test.test "openfile: O_CLOEXEC" ~only_if:(fun () -> not Sys.win32)
    (fun () -> test_cloexec ~closed:true [Unix.O_CLOEXEC]);

  Test.test "openfile: O_CLOEXEC not given" ~only_if:(fun () -> not Sys.win32)
    (fun () -> test_cloexec ~closed:false []);

  Test.test "openfile: O_KEEPEXEC" ~only_if:(fun () -> not Sys.win32)
    (fun () -> test_cloexec ~closed:false [Unix.O_KEEPEXEC]);

  Test.test "openfile: O_CLOEXEC, O_KEEPEXEC" ~only_if:(fun () -> not Sys.win32)
    (fun () -> test_cloexec ~closed:true [Unix.O_CLOEXEC; Unix.O_KEEPEXEC]);

  Test.test "openfile: O_KEEPEXEC, O_CLOEXEC" ~only_if:(fun () -> not Sys.win32)
    (fun () -> test_cloexec ~closed:true [Unix.O_KEEPEXEC; Unix.O_CLOEXEC]);
]


let () =
  Test.concurrent "unix.cloexec" [
    Test.suite "lwt_unix.cloexec" openfile_tests
  ]
