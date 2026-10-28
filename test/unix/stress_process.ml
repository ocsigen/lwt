(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* Stress test for Lwt_process: many concurrent, short-lived and randomly chosen
   process operations, with bounded concurrency so memory stays small.

   Usage: stress_process.exe [ITERATIONS] [CONCURRENCY]

   At the end, checks that no file descriptor leaked and no child is left
   un-reaped. Run it with a timeout and a memory limit, e.g.
   [bash -c 'ulimit -v 2097152; exec timeout 300 ./stress_process.exe 5000'] *)

open Lwt.Infix

let iterations = try int_of_string Sys.argv.(1) with _ -> 2000
let concurrency = try int_of_string Sys.argv.(2) with _ -> 32
let failures = ref 0

let fail fmt =
  Printf.ksprintf
    (fun s ->
       incr failures;
       prerr_endline ("FAIL: " ^ s))
    fmt

let expect_status what expected = function
  | st when st = expected -> ()
  | Unix.WEXITED n -> fail "%s: exited %d" what n
  | Unix.WSIGNALED n -> fail "%s: signaled %d" what n
  | Unix.WSTOPPED n -> fail "%s: stopped %d" what n

let open_fds () =
  match Sys.readdir "/proc/self/fd" with
  | a -> Some (Array.length a)
  | exception Sys_error _ -> None

let payload n = String.init n (fun i -> Char.chr (Char.code 'a' + (i mod 26)))

(* Each operation takes a random state and returns unit Lwt.t; it records
   failures through [fail] rather than raising. *)

let op_true _ = Lwt_process.exec ("true", [|"true"|]) >|= expect_status "true" (Unix.WEXITED 0)

let op_exit_code rs =
  let n = Random.State.int rs 200 in
  Lwt_process.exec (Lwt_process.shell (Printf.sprintf "exit %d" n))
  >|= expect_status "exit code" (Unix.WEXITED n)

let op_missing _ =
  Lwt_process.exec ("./no-such-binary", [|"no-such-binary"|])
  >|= expect_status "missing binary" (Unix.WEXITED 127)

let op_empty_prog _ =
  (* [prog = ""] means "use args.(0)". *)
  Lwt_process.pread_line ("", [|"echo"; "hi"|]) >|= fun l ->
  if l <> "hi" then fail "empty prog: got %S" l

let op_env rs =
  let v = string_of_int (Random.State.bits rs) in
  Lwt_process.pread_line
    ~env:[|"LWT_STRESS=" ^ v|]
    ("printenv", [|"printenv"; "LWT_STRESS"|])
  >|= fun l -> if l <> v then fail "env: got %S, wanted %S" l v

let op_empty_env _ =
  Lwt_process.pread ~env:[||] ("printenv", [|"printenv"|]) >|= fun s ->
  if s <> "" then fail "empty env: got %S" s

(* Round trip through [cat]; large payloads exercise pipe back-pressure, so
   reading and writing must be interleaved. *)
let op_cat rs =
  let n = [|0; 1; 100; 5000; 70_000; 200_000|].(Random.State.int rs 6) in
  let data = payload n in
  Lwt_process.with_process ("cat", [|"cat"|]) (fun p ->
    let write =
      Lwt_io.write p#stdin data >>= fun () -> Lwt_io.close p#stdin
    in
    let read = Lwt_io.read p#stdout in
    Lwt.both write read >>= fun ((), out) ->
    if out <> data then fail "cat %d: wrong output (%d bytes)" n (String.length out);
    p#status >|= expect_status "cat" (Unix.WEXITED 0))

let op_full rs =
  let out = payload (Random.State.int rs 3000)
  and err = payload (Random.State.int rs 3000) in
  let script = Printf.sprintf "printf %%s %s; printf %%s %s >&2; exit 7" out err in
  Lwt_process.with_process_full (Lwt_process.shell script) (fun p ->
    Lwt_io.close p#stdin >>= fun () ->
    Lwt.both (Lwt_io.read p#stdout) (Lwt_io.read p#stderr) >>= fun (o, e) ->
    if o <> out then fail "full: wrong stdout";
    if e <> err then fail "full: wrong stderr";
    p#status >|= expect_status "full" (Unix.WEXITED 7))

let op_redirections rs =
  let pick () =
    match Random.State.int rs 3 with
    | 0 -> `Dev_null
    | 1 -> `Close
    | _ -> `Keep
  in
  (* [true] never touches its standard streams, so any combination works. *)
  let stdin = match pick () with `Keep -> `Dev_null | r -> r in
  Lwt_process.exec ~stdin ~stdout:`Dev_null ~stderr:`Dev_null
    ("true", [|"true"|])
  >|= expect_status "redirections" (Unix.WEXITED 0)

(* stdout and stderr both pointing to the same pipe: [`FD_copy] keeps [w] open
   for the second redirection, [`FD_move] closes it in the parent. *)
let op_fd_copy _ =
  let r, w = Lwt_unix.pipe_in ~cloexec:true () in
  let script = "printf 1; printf 2 >&2" in
  let proc =
    Lwt_process.exec ~stdin:`Dev_null ~stdout:(`FD_copy w) ~stderr:(`FD_move w)
      (Lwt_process.shell script)
  in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.input r in
  Lwt.both proc (Lwt_io.read ic) >>= fun (st, out) ->
  Lwt_io.close ic >|= fun () ->
  expect_status "fd_copy" (Unix.WEXITED 0) st;
  if out <> "12" then fail "fd_copy: got %S" out

(* Kill a long-running process; both through [terminate] and the timeout. *)
let op_kill rs =
  if Random.State.bool rs
  then
    Lwt_process.with_process_none ("sleep", [|"sleep"; "30"|]) (fun p ->
      Lwt_unix.sleep 0.001 >>= fun () ->
      p#terminate;
      p#status >|= function
      | Unix.WSIGNALED s when s = Sys.sigkill -> ()
      | _ -> fail "terminate: not killed by SIGKILL")
  else
    Lwt_process.with_process_none ~timeout:0.01 ("sleep", [|"sleep"; "30"|])
      (fun p ->
         p#status >|= function
         | Unix.WSIGNALED s when s = Sys.sigkill -> ()
         | _ -> fail "timeout: not killed by SIGKILL")

(* Closing the pipe before the child is done: the child must still be reaped. *)
let op_early_close _ =
  Lwt_process.with_process_in ("yes", [|"yes"|]) (fun p ->
    Lwt_io.read_char p#stdout >>= fun _ ->
    Lwt_io.close p#stdout >>= fun () ->
    p#terminate;
    p#status >|= fun _ -> ())

let ops =
  [| op_true; op_exit_code; op_missing; op_empty_prog; op_env; op_empty_env
   ; op_cat; op_full; op_redirections; op_fd_copy; op_kill; op_early_close |]

let run_one rs =
  let op = ops.(Random.State.int rs (Array.length ops)) in
  Lwt.catch
    (fun () -> op rs)
    (fun exn -> fail "uncaught exception: %s" (Printexc.to_string exn); Lwt.return_unit)

let rec worker rs remaining =
  if !remaining <= 0
  then Lwt.return_unit
  else (
    decr remaining;
    run_one rs >>= fun () -> worker rs remaining)

let () =
  Random.self_init ();
  let seed = Random.bits () in
  Printf.printf "stress_process: %d iterations, %d workers, seed %d\n%!"
    iterations concurrency seed;
  let remaining = ref iterations in
  let before = open_fds () in
  Lwt_main.run
    (Lwt.join
       (List.init concurrency (fun i ->
          worker (Random.State.make [|seed; i|]) remaining)));
  (* Every child must have been reaped by Lwt_process. *)
  (match Unix.waitpid [Unix.WNOHANG] (-1) with
   | exception Unix.Unix_error (Unix.ECHILD, _, _) -> ()
   | _ -> fail "an un-reaped child (zombie) is left");
  (match before, open_fds () with
   | Some b, Some a when a <> b -> fail "fd leak: %d open before, %d after" b a
   | _ -> ());
  if !failures > 0
  then (Printf.printf "%d failure(s)\n" !failures; exit 1)
  else print_endline "ok"
