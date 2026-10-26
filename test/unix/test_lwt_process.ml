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

let pwrite ~stdin pout =
  let args = [|"dummy.exe"; "read"|] in
  let proc = Lwt_process.exec ~stdin ("./dummy.exe", args) in
  let write =
    Lwt.finalize
      (fun () -> Lwt_unix.write pout expected 0 expected_len)
      (fun () -> Lwt_unix.close pout)
  in
  proc >>= fun r ->
  write >>= fun n ->
  assert (n = expected_len);
  check_status r

let pread ?stdout ?stderr pin =
  let buf = Bytes.create expected_len in
  let proc =
    match stdout, stderr with
    | Some stdout, None ->
        let args = [|"dummy.exe"; "write"|] in
        Lwt_process.exec ~stdout ("./dummy.exe", args)
    | None, Some stderr ->
        let args = [|"dummy.exe"; "errwrite"|] in
        Lwt_process.exec ~stderr ("./dummy.exe", args)
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

let suite =
  suite "lwt_process"
    [ (* The sleep command is not available on Win32. *)
      test "lazy_undefined"
        ~only_if:(fun () -> not Sys.win32)
        (fun () ->
           Lwt_process.with_process_in ~timeout:1.
             ("sleep", [|"sleep"; "2"|])
             (fun p ->
                Lwt.catch
                  (fun () -> Lwt_io.read p#stdout)
                  (fun _ -> Lwt.return ""))
           >>= fun _ -> Lwt.return_true)
    ; test "subproc stdout can be redirected to null" (fun () ->
        let args = [|"dummy.exe"; "write"|] in
        Lwt_process.exec ~stdout:`Dev_null ("./dummy.exe", args)
        >>= check_status)
    ; test "subproc stderr can be redirected to null" (fun () ->
        let args = [|"dummy.exe"; "errwrite"|] in
        Lwt_process.exec ~stderr:`Dev_null ("./dummy.exe", args)
        >>= check_status)
    ; test "subproc cannot write on closed stdout" (fun () ->
        let args = [|"dummy.exe"; "write"|] in
        (* mask subproc stderr *)
        let stderr = `Dev_null in
        Lwt_process.exec ~stdout:`Close ~stderr ("./dummy.exe", args)
        >>= check_status ~status:(( <> ) 0))
    ; test "subproc cannot write on closed stderr" (fun () ->
        let args = [|"dummy.exe"; "errwrite"|] in
        Lwt_process.exec ~stderr:`Close ("./dummy.exe", args)
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
    ; (* posix_spawnp does not run scripts without a shebang through /bin/sh,
         unlike execvp; Lwt_process falls back to fork for them. *)
      test "script without shebang is run by /bin/sh"
        ~only_if:(fun () -> not Sys.win32)
        (fun () ->
           let script = Filename.temp_file "lwt_process" ".sh" in
           let oc = open_out script in
           output_string oc "exit 3\n";
           close_out oc;
           Unix.chmod script 0o755;
           Lwt.finalize
             (fun () ->
                Lwt_process.exec (script, [|script|])
                >>= check_status ~status:(( = ) 3))
             (fun () -> Sys.remove script; Lwt.return_unit))
    ; test "missing executable exits with 127"
        ~only_if:(fun () -> not Sys.win32)
        (fun () ->
           Lwt_process.exec ("./does-not-exist.exe", [|"does-not-exist.exe"|])
           >>= check_status ~status:(( = ) 127))
    ; test "failed spawn closes moved fds"
        ~only_if:(fun () -> not Sys.win32)
        (fun () ->
           let r, w = Unix.pipe ~cloexec:true () in
           Unix.set_nonblock r;
           Lwt_process.exec ~stdout:(`FD_move w)
             ("./does-not-exist.exe", [|"does-not-exist.exe"|])
           >>= fun status ->
           (* End of file only once every write end is closed; EAGAIN otherwise. *)
           let closed =
             try Unix.read r (Bytes.create 1) 0 1 = 0
             with Unix.Unix_error (Unix.EAGAIN, _, _) -> false
           in
           Unix.close r;
           Lwt.return (status = Unix.WEXITED 127 && closed)) ]
