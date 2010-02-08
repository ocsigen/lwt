(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_process
 * Copyright (C) 2009 Jérémie Dimino
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exceptions;
 * either version 2.1 of the License, or (at your option) any later
 * version. See COPYING file for details.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *)

open Lwt

type command = string * string array

let shell cmd = ("/bin/sh", [| "/bin/sh"; "-c"; cmd |])

(* +-----------------------------------------------------------------+
   | Spawing commands                                                |
   +-----------------------------------------------------------------+ *)

let fd_move fd1 fd2 =
  Unix.dup2 fd1 fd2;
  Unix.close fd1

let spawn (prog, args) env stdin stdout stderr toclose =
  try
    match Unix.fork() with
      | 0 ->
          if stdin <> Unix.stdin then fd_move stdin Unix.stdin;
          if stdout <> Unix.stdout then fd_move stdout Unix.stdout;
          if stderr <> Unix.stdout then fd_move stderr Unix.stderr;
          List.iter Unix.close toclose;
          begin
            try
              match env with
                | None ->
                    Unix.execvp prog args
                | Some env ->
                    Unix.execvpe prog args env
            with _ ->
              exit 127
          end
      | id ->
          if stdin <> Unix.stdin then Unix.close stdin;
          if stdout <> Unix.stdout then Unix.close stdout;
          if stderr <> Unix.stderr then Unix.close stderr;
          id
  with Unix.Unix_error(error, _, _) ->
    raise (Sys_error(Unix.error_message error))

type state =
  | Running
  | Exited of Unix.process_status

let status (pid, status, rusage) = status
let rusage (pid, status, rusage) = rusage

class virtual common timeout (w : (int * Unix.process_status * Lwt_unix.resource_usage) Lwt.t) pid =
  let status = lazy(w >|= status) and rusage = lazy(w >|= rusage) in
object(self)

  method virtual close : Unix.process_status Lwt.t

  method pid = pid

  method state = match Lwt.poll w with
    | None -> Running
    | Some(pid, status, rusage) -> Exited status

  method kill signum = match Lwt.poll w with
    | None -> Unix.kill pid signum
    | Some _ -> ()

  method status = Lazy.force status
  method rusage = Lazy.force rusage

  initializer
    match timeout with
      | None ->
          ()
      | Some dt ->
          Lwt.ignore_result begin
            try_lwt
              lwt _ = Lwt.select [Lwt_unix.timeout dt; w] in
              return ()
            with
              | Lwt_unix.Timeout ->
                  (try Unix.kill Sys.sigkill pid with _ -> ());
                  (try_lwt self#close >> return () with _ -> return ())
              | _ ->
                  return ()
          end
end

class process_none ?timeout ?env cmd =
  let pid = spawn cmd env Unix.stdin Unix.stdout Unix.stderr [] in
  let w = Lwt_unix.wait4 [] pid in
  let close = lazy(w >|= status) in
object
  inherit common timeout w pid
  method close = Lazy.force close
end

class process_in ?timeout ?env cmd =
  let stdout_r, stdout_w = Unix.pipe () in
  let pid = spawn cmd env Unix.stdin stdout_w Unix.stderr [stdout_r] in
  let stdout = Lwt_io.of_unix_fd ~mode:Lwt_io.input stdout_r
  and w = Lwt_unix.wait4 [] pid in
  let close = lazy(Lwt_io.close stdout >> w >|= status) in
object
  inherit common timeout w pid
  method close = Lazy.force close
  method stdout = stdout
end

class process_out ?timeout ?env cmd =
  let stdin_r, stdin_w = Unix.pipe () in
  let pid = spawn cmd env stdin_r Unix.stdout Unix.stderr [stdin_w] in
  let stdin = Lwt_io.of_unix_fd ~mode:Lwt_io.output stdin_w
  and w = Lwt_unix.wait4 [] pid in
  let close = lazy (Lwt_io.close stdin >> w >|= status) in
object
  inherit common timeout w pid
  method close = Lazy.force close
  method stdin = stdin
end

class process ?timeout ?env cmd =
  let stdin_r, stdin_w = Unix.pipe ()
  and stdout_r, stdout_w = Unix.pipe () in
  let pid = spawn cmd env stdin_r stdout_w Unix.stderr [stdin_w; stdout_r] in
  let stdin = Lwt_io.of_unix_fd ~mode:Lwt_io.output stdin_w
  and stdout = Lwt_io.of_unix_fd ~mode:Lwt_io.input stdout_r
  and w = Lwt_unix.wait4 [] pid in
  let close = lazy(Lwt_io.close stdin <&> Lwt_io.close stdout >> w >|= status) in
object
  inherit common timeout w pid
  method close = Lazy.force close
  method stdin = stdin
  method stdout = stdout
end

class process_full ?timeout ?env cmd =
  let stdin_r, stdin_w = Unix.pipe ()
  and stdout_r, stdout_w = Unix.pipe ()
  and stderr_r, stderr_w = Unix.pipe () in
  let pid = spawn cmd env stdin_r stdout_w stderr_w [stdin_w; stdout_r; stderr_r] in
  let stdin = Lwt_io.of_unix_fd ~mode:Lwt_io.output stdin_w
  and stdout = Lwt_io.of_unix_fd ~mode:Lwt_io.input stdout_r
  and stderr = Lwt_io.of_unix_fd ~mode:Lwt_io.input stderr_r
  and w = Lwt_unix.wait4 [] pid in
  let close = lazy(join [Lwt_io.close stdin; Lwt_io.close stdout; Lwt_io.close stderr] >> w >|= status) in
object
  inherit common timeout w pid
  method close = Lazy.force close
  method stdin = stdin
  method stdout = stdout
  method stderr = stderr
end

let open_process_none ?timeout ?env cmd = new process_none ?timeout ?env cmd
let open_process_in ?timeout ?env cmd = new process_in ?timeout ?env cmd
let open_process_out ?timeout ?env cmd = new process_out ?timeout ?env cmd
let open_process ?timeout ?env cmd = new process ?timeout ?env cmd
let open_process_full ?timeout ?env cmd = new process_full ?timeout ?env cmd

let make_with backend ?timeout ?env cmd f =
  let process = backend ?timeout ?env cmd in
  try_lwt
    f process
  finally
    lwt _ = process#close in
    return ()

let with_process_none ?timeout ?env cmd f = make_with open_process_none ?timeout ?env cmd f
let with_process_in ?timeout ?env cmd f = make_with open_process_in ?timeout ?env cmd f
let with_process_out ?timeout ?env cmd f = make_with open_process_out ?timeout ?env cmd f
let with_process ?timeout ?env cmd f = make_with open_process ?timeout ?env cmd f
let with_process_full ?timeout ?env cmd f = make_with open_process_full ?timeout ?env cmd f

(* +-----------------------------------------------------------------+
   | High-level functions                                            |
   +-----------------------------------------------------------------+ *)

let exec ?timeout ?env cmd = (open_process_none ?timeout ?env cmd)#close

let ingore_close ch =
  ignore (Lwt_io.close ch)

let recv_chars pr =
  let ic = pr#stdout in
  Gc.finalise ingore_close ic;
  Lwt_stream.from (fun _ ->
                     lwt x = Lwt_io.read_char_opt ic in
                     if x = None then begin
                       lwt () = Lwt_io.close ic in
                       return x
                     end else
                       return x)

let recv_lines pr =
  let ic = pr#stdout in
  Gc.finalise ingore_close ic;
  Lwt_stream.from (fun _ ->
                     lwt x = Lwt_io.read_line_opt ic in
                     if x = None then begin
                       lwt () = Lwt_io.close ic in
                       return x
                     end else
                       return x)

let recv pr =
  let ic = pr#stdout in
  try_lwt
    Lwt_io.read ic
  finally
    Lwt_io.close ic >> return ()

let recv_line pr =
  let ic = pr#stdout in
  try_lwt
    Lwt_io.read_line ic
  finally
    Lwt_io.close ic

(* Receiving *)

let pread ?timeout ?env cmd =
  recv (open_process_in ?timeout ?env cmd)

let pread_chars ?timeout ?env cmd =
  recv_chars (open_process_in ?timeout ?env cmd)

let pread_line ?timeout ?env cmd =
  recv_line (open_process_in ?timeout ?env cmd)

let pread_lines ?timeout ?env cmd =
  recv_lines (open_process_in ?timeout ?env cmd)

(* Sending *)

let send f pr data =
  let oc = pr#stdin in
  try_lwt
    f oc data
  finally
    Lwt_io.close oc

let pwrite ?timeout ?env cmd text =
  send Lwt_io.write (open_process_out ?timeout ?env cmd) text

let pwrite_chars ?timeout ?env cmd chars =
  send Lwt_io.write_chars (open_process_out ?timeout ?env cmd) chars

let pwrite_line ?timeout ?env cmd line =
  send Lwt_io.write_line (open_process_out ?timeout ?env cmd) line

let pwrite_lines ?timeout ?env cmd lines =
  send Lwt_io.write_lines (open_process_out ?timeout ?env cmd) lines

(* Mapping *)

(* Dump something to a command: *)
let dump f pr data =
  let oc = pr#stdin in
  ignore_result (try_lwt
                   f oc data
                 finally
                   Lwt_io.close oc)

let pmap ?timeout ?env cmd text =
  let pr = open_process ?timeout ?env cmd in
  dump Lwt_io.write pr text;
  recv pr

let pmap_chars ?timeout ?env cmd chars =
  let pr = open_process ?timeout ?env cmd in
  dump Lwt_io.write_chars pr chars;
  recv_chars pr

let pmap_line ?timeout ?env cmd line =
  let pr = open_process ?timeout ?env cmd in
  dump Lwt_io.write_line pr line;
  recv_line pr

let pmap_lines ?timeout ?env cmd lines =
  let pr = open_process ?timeout ?env cmd in
  dump Lwt_io.write_lines pr lines;
  recv_lines pr
