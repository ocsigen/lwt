(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

open Test
open Lwt.Infix

let expected_str = "the quick brown fox jumps over the lazy dog"
let expected = Bytes.of_string expected_str
let expected_len = Bytes.length expected

let check_status ?(status = ( = ) 0) = function
  | Unix.WEXITED n when status n -> Lwt.return_true
  | Unix.WEXITED n ->
      Printf.eprintf "exited with code %d" n;
      Lwt.return_false
  | Unix.WSIGNALED x ->
      Printf.eprintf "failed with signal %d" x;
      Lwt.return_false
  | Unix.WSTOPPED x ->
      Printf.eprintf "stopped with signal %d" x;
      Lwt.return_false

let pwrite ?cwd ~stdin pout =
  let args = [|"dummy.exe"; "read"|] in
  let proc = Lwt_process.exec ?cwd ~stdin ("./dummy.exe", args) in
  let write =
    Lwt.finalize
      (fun () -> Lwt_unix.write pout expected 0 expected_len)
      (fun () -> Lwt_unix.close pout)
  in
  proc >>= fun r ->
  write >>= fun n ->
  assert (n = expected_len);
  check_status r

let pread ?cwd ?stdout ?stderr pin =
  let buf = Bytes.create expected_len in
  let proc =
    match stdout, stderr with
    | Some stdout, None ->
        let args = [|"dummy.exe"; "write"|] in
        Lwt_process.exec ?cwd ~stdout ("./dummy.exe", args)
    | None, Some stderr ->
        let args = [|"dummy.exe"; "errwrite"|] in
        Lwt_process.exec ?cwd ~stderr ("./dummy.exe", args)
    | _ -> assert false
  in
  let read = Lwt_unix.read pin buf 0 expected_len in
  proc >>= fun r ->
  read >>= fun n ->
  assert (n = expected_len);
  assert (Bytes.equal buf expected);
  Lwt_unix.read pin buf 0 1 >>= fun n ->
  assert (n = 0);
  check_status r

let process_suite cwd =
  (* When passing cwd, the implementation of process spawning changes (from spawn to fork+execvp).
     We run the suite once with `?cwd` to `None` and once to `Some "."`. *)
  [ (* The sleep command is not available on Win32. *)
    test "lazy_undefined"
      ~only_if:(fun () -> not Sys.win32)
      (fun () ->
         Lwt_process.with_process_in ?cwd ~timeout:1.
           ("sleep", [|"sleep"; "2"|])
           (fun p ->
              Lwt.catch
                (fun () -> Lwt_io.read p#stdout)
                (fun _ -> Lwt.return ""))
         >>= fun _ -> Lwt.return_true)
  ; test "subproc stdout can be redirected to null" (fun () ->
      let args = [|"dummy.exe"; "write"|] in
      Lwt_process.exec ?cwd ~stdout:`Dev_null ("./dummy.exe", args)
      >>= check_status)
  ; test "subproc stderr can be redirected to null" (fun () ->
      let args = [|"dummy.exe"; "errwrite"|] in
      Lwt_process.exec ?cwd ~stderr:`Dev_null ("./dummy.exe", args)
      >>= check_status)
  ; test "subproc cannot write on closed stdout" (fun () ->
      let args = [|"dummy.exe"; "write"|] in
      (* mask subproc stderr *)
      let stderr = `Dev_null in
      Lwt_process.exec ?cwd ~stdout:`Close ~stderr ("./dummy.exe", args)
      >>= check_status ~status:(( <> ) 0))
  ; test "subproc cannot write on closed stderr" (fun () ->
      let args = [|"dummy.exe"; "errwrite"|] in
      Lwt_process.exec ?cwd ~stderr:`Close ("./dummy.exe", args)
      >>= check_status ~status:(( <> ) 0))
  ; test "can write to subproc stdin" (fun () ->
      let pin, pout = Lwt_unix.pipe_out ~cloexec:true () in
      pwrite ~stdin:(`FD_move pin) pout)
  ; test "can read from subproc stdout" (fun () ->
      let pin, pout = Lwt_unix.pipe_in ~cloexec:true () in
      pread ~stdout:(`FD_move pout) pin)
  ; test "can read from subproc stderr" (fun () ->
      let pin, perr = Lwt_unix.pipe_in ~cloexec:true () in
      pread ~stderr:(`FD_move perr) pin)
  ; test "missing executable exits with 127"
      ~only_if:(fun () -> not Sys.win32)
      (fun () ->
         Lwt_process.exec ?cwd ("./does-not-exist.exe", [|"does-not-exist.exe"|])
         >>= check_status ~status:(( = ) 127))
  ; (* Sequential: a child forked meanwhile by a concurrent test holds a copy of
       the pipe's write end until it calls exec, and the read below would not
       see the end of file. *)
    test "failed spawn closes moved fds" ~sequential:true
      ~only_if:(fun () -> not Sys.win32)
      (fun () ->
         let r, w = Unix.pipe ~cloexec:true () in
         Lwt_process.exec ?cwd ~stdout:(`FD_move w)
           ("./does-not-exist.exe", [|"does-not-exist.exe"|])
         >>= fun status ->
         (* End of file once every write end is closed. On macOS and the BSDs,
            the child that posix_spawnp created before exec failed may still
            hold a copy for a moment after posix_spawnp returns, since it is
            not waited for. So the test waits for the end of file. If the
            parent kept its copy, it never comes, and the timeout fails the
            test. *)
         let r = Lwt_unix.of_unix_file_descr ~blocking:false r in
         Lwt.pick
           [ (Lwt_unix.read r (Bytes.create 1) 0 1 >|= fun n -> n = 0)
           ; (Lwt_unix.sleep 5. >|= fun () -> false) ]
         >>= fun closed ->
         Lwt_unix.close r >>= fun () ->
         if not closed then prerr_string "the write end stayed open; ";
         check_status ~status:(( = ) 127) status >|= fun ok -> ok && closed) ]

let suite_cwd = suite "lwt_process(cwd)" (process_suite (Some "."))
let suite = suite "lwt_process" (process_suite None)
