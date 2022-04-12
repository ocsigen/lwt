(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



open Test
open Lwt.Infix

let expected = "the quick brown fox jumps over the lazy dog"

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

  test "pread"
    (fun () ->
      let args = [|"dummy.exe"; "write"|] in
      Lwt_process.pread ~stdin:`Close ~stderr:`Close ("./dummy.exe", args)
      >|= fun actual ->
      actual = expected);

  test "pread keep"
    (fun () ->
      let args = [|"dummy.exe"; "write"|] in
      Lwt_process.pread ~stdin:`Keep ~stderr:`Keep ("./dummy.exe", args)
      >|= fun actual ->
      actual = expected);

  test "pread nul"
    (fun () ->
      let args = [|"dummy.exe"; "write"|] in
      Lwt_process.pread ~stdin:`Dev_null ~stderr:`Dev_null ("./dummy.exe", args)
      >|= fun actual ->
      actual = expected);

  test "pwrite"
    (fun () ->
      let args = [|"dummy.exe"; "read"|] in
      Lwt_process.pwrite ~stdout:`Close ~stderr:`Close ("./dummy.exe", args) expected
      >>= fun () -> Lwt.return_true);

  test "pwrite keep"
    (fun () ->
      let args = [|"dummy.exe"; "read"|] in
      Lwt_process.pwrite ~stdout:`Keep ~stderr:`Keep ("./dummy.exe", args) expected
      >>= fun () -> Lwt.return_true);

  test "pwrite nul"
    (fun () ->
      let args = [|"dummy.exe"; "read"|] in
      Lwt_process.pwrite ~stdout:`Dev_null ~stderr:`Dev_null ("./dummy.exe", args) expected
      >>= fun () -> Lwt.return_true);
]
