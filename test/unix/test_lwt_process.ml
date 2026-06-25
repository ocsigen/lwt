(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



open Test
open Lwt.Infix

let expected_str = "the quick brown fox jumps over the lazy dog"
let expected = Bytes.of_string expected_str

let check_status ?(status=(=) 0) = function
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

let pwrite ~stdin pout expected =
  let expected_len = Bytes.length expected in
  let args = [|"dummy.exe"; "read"|] in
  let proc = Lwt_process.exec ~stdin ("./dummy.exe", args) in
  let write = Lwt.finalize
                (fun () -> Lwt_unix.write pout expected 0 expected_len)
                (fun () -> Lwt_unix.close pout) in
  proc >>= fun r ->
  write >>= fun n ->
  assert (n = expected_len);
  check_status r

let read_all ic buf ofs len =
  let rec loop ic buf ofs len =
    Lwt_unix.read ic buf ofs len >>= function
    | 0 ->
        Lwt.return ofs
    | n ->
        let ofs = ofs + n in
        let len = len - n in
        if len = 0 then
          Lwt.return ofs
        else
          loop ic buf ofs len
  in
  loop ic buf ofs len

let pread ?env ?stdout ?stderr pin cmd expected =
  (match stdout, stderr with
    | Some _, None
    | None, Some _ ->
      ()
    | _ -> assert false);
  let expected_len = Bytes.length expected in
  let buf = Bytes.create expected_len in
  let args = [|"dummy.exe"; cmd|] in
  let proc = Lwt_process.exec ?env ?stdout ?stderr ("./dummy.exe", args) in
  let read = read_all pin buf 0 expected_len in
  proc >>= fun r ->
  read >>= fun n ->
  (if n <> expected_len then Printf.ksprintf failwith "expected %d bytes, got %d" expected_len n);
  assert (Bytes.equal buf expected);
  Lwt_unix.read pin buf 0 1 >>= fun n ->
  if n <> 0 then Printf.ksprintf failwith "expected 0 bytes remaining, got %d" n;
  check_status r

let bytes_of_env env =
  env
  |> Array.map (Printf.sprintf "%s\n")
  |> Array.to_list
  |> String.concat ""
  |> Bytes.of_string

let suite = suite "lwt_process" [
  (* The sleep command is not available on Win32. *)
  test "lazy_undefined" ~only_if:(fun () -> not Sys.win32)
    (fun () ->
      Lwt_process.with_process_in
        ~timeout:1. ("sleep", [| "sleep"; "2" |])
          (fun p ->
            Lwt.catch
              (fun () -> Lwt_io.read p#stdout)
              (fun _ -> Lwt.return ""))
        >>= fun _ -> Lwt.return_true);

  test "subproc stdout can be redirected to null"
    (fun () ->
      let args = [|"dummy.exe"; "write"|] in
      Lwt_process.exec ~stdout:`Dev_null ("./dummy.exe", args)
      >>= check_status);

  test "subproc stderr can be redirected to null"
    (fun () ->
      let args = [|"dummy.exe"; "errwrite"|] in
      Lwt_process.exec ~stderr:`Dev_null ("./dummy.exe", args)
      >>= check_status);

  test "subproc cannot write on closed stdout"
    (fun () ->
      let args = [|"dummy.exe"; "write"|] in
      let stderr = `Dev_null    (* mask subproc stderr *) in
      Lwt_process.exec ~stdout:`Close ~stderr ("./dummy.exe", args)
      >>= check_status ~status:((<>) 0));

  test "subproc cannot write on closed stderr"
    (fun () ->
      let args = [|"dummy.exe"; "errwrite"|] in
      Lwt_process.exec ~stderr:`Close ("./dummy.exe", args)
      >>= check_status ~status:((<>) 0));

  test "can write to subproc stdin"
    (fun () ->
      let pin, pout = Lwt_unix.pipe_out ~cloexec:true () in
      pwrite ~stdin:(`FD_move pin) pout expected);

  test "can read from subproc stdout"
    (fun () ->
      let pin, pout = Lwt_unix.pipe_in ~cloexec:true () in
      pread ~stdout:(`FD_move pout) pin "write" expected);

  test "can read from subproc stderr"
    (fun () ->
      let pin, perr = Lwt_unix.pipe_in ~cloexec:true () in
      pread ~stderr:(`FD_move perr) pin "errwrite" expected);

  test "overrides env"
    (fun () ->
      let env = [| "FOO=1" |] in
      let expected = Bytes.of_string "FOO=1\n" in
      let pin, pout = Lwt_unix.pipe_in ~cloexec:true () in
      pread ~env ~stdout:(`FD_move pout) pin "printenv" expected);

  test "passes env"
    (fun () ->
      let env = Unix.unsafe_environment () in
      let expected = bytes_of_env env in
      let pin, pout = Lwt_unix.pipe_in ~cloexec:true () in
      pread ~env ~stdout:(`FD_move pout) pin "printenv" expected);

  test "inherits env"
    (fun () ->
      let env = Unix.unsafe_environment () in
      let expected = bytes_of_env env in
      let pin, pout = Lwt_unix.pipe_in ~cloexec:true () in
      pread ?env:None ~stdout:(`FD_move pout) pin "printenv" expected);
]
