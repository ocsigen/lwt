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

type redirection =
    [ `Keep
    | `Dev_null
    | `Close
    | `FD_copy of Unix.file_descr
    | `FD_move of Unix.file_descr ]

(* +-----------------------------------------------------------------+
   | Spawing commands                                                |
   +-----------------------------------------------------------------+ *)

let redirect fd redirection = match redirection with
  | `Keep ->
      ()
  | `Dev_null ->
      Unix.close fd;
      let dev_null = Unix.openfile "/dev/null" [Unix.O_RDWR] 0o666 in
      if fd <> dev_null then begin
        Unix.dup2 dev_null fd;
        Unix.close dev_null
      end
  | `Close ->
      Unix.close fd
  | `FD_copy fd' ->
      Unix.dup2 fd' fd
  | `FD_move fd' ->
      Unix.dup2 fd' fd;
      Unix.close fd'

let spawn (prog, args) env ?(stdin:redirection=`Keep) ?(stdout:redirection=`Keep) ?(stderr:redirection=`Keep) toclose =
  match Unix.fork() with
    | 0 ->
        redirect Unix.stdin stdin;
        redirect Unix.stdout stdout;
        redirect Unix.stderr stderr;
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
        let close = function
          | `FD_move fd ->
              Unix.close fd
          | _ ->
              ()
        in
        close stdin;
        close stdout;
        close stderr;
        id

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
              lwt _ = Lwt.pick [Lwt_unix.timeout dt; w] in
              return ()
            with
              | Lwt_unix.Timeout ->
                  (try Unix.kill pid Sys.sigkill with _ -> ());
                  (try_lwt self#close >> return () with _ -> return ())
              | _ ->
                  return ()
          end
end

class process_none ?timeout ?env ?stdin ?stdout ?stderr cmd =
  let pid = spawn cmd env ?stdin ?stdout ?stderr [] in
  let w = Lwt_unix.wait4 [] pid in
  let close = lazy(w >|= status) in
object
  inherit common timeout w pid
  method close = Lazy.force close
end

class process_in ?timeout ?env ?stdin ?stderr cmd =
  let stdout_r, stdout_w = Unix.pipe () in
  let pid = spawn cmd env ?stdin ~stdout:(`FD_move stdout_w) ?stderr [stdout_r] in
  let stdout = Lwt_io.of_unix_fd ~mode:Lwt_io.input stdout_r
  and w = Lwt_unix.wait4 [] pid in
  let close = lazy(Lwt_io.close stdout >> w >|= status) in
object
  inherit common timeout w pid
  method close = Lazy.force close
  method stdout = stdout
end

class process_out ?timeout ?env ?stdout ?stderr cmd =
  let stdin_r, stdin_w = Unix.pipe () in
  let pid = spawn cmd env ~stdin:(`FD_move stdin_r) ?stdout ?stderr [stdin_w] in
  let stdin = Lwt_io.of_unix_fd ~mode:Lwt_io.output stdin_w
  and w = Lwt_unix.wait4 [] pid in
  let close = lazy (Lwt_io.close stdin >> w >|= status) in
object
  inherit common timeout w pid
  method close = Lazy.force close
  method stdin = stdin
end

class process ?timeout ?env ?stderr cmd =
  let stdin_r, stdin_w = Unix.pipe ()
  and stdout_r, stdout_w = Unix.pipe () in
  let pid = spawn cmd env ~stdin:(`FD_move stdin_r) ~stdout:(`FD_move stdout_w) ?stderr [stdin_w; stdout_r] in
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
  let pid = spawn cmd env ~stdin:(`FD_move stdin_r) ~stdout:(`FD_move stdout_w) ~stderr:(`FD_move stderr_w) [stdin_w; stdout_r; stderr_r] in
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

let open_process_none ?timeout ?env ?stdin ?stdout ?stderr cmd = new process_none ?timeout ?env ?stdin ?stdout ?stderr cmd
let open_process_in ?timeout ?env ?stdin ?stderr cmd = new process_in ?timeout ?env ?stdin ?stderr cmd
let open_process_out ?timeout ?env ?stdout ?stderr cmd = new process_out ?timeout ?env ?stdout ?stderr cmd
let open_process ?timeout ?env ?stderr cmd = new process ?timeout ?env ?stderr cmd
let open_process_full ?timeout ?env cmd = new process_full ?timeout ?env cmd

let make_with backend ?timeout ?env cmd f =
  let process = backend ?timeout ?env cmd in
  try_lwt
    f process
  finally
    lwt _ = process#close in
    return ()

let with_process_none ?timeout ?env ?stdin ?stdout ?stderr cmd f = make_with (open_process_none ?stdin ?stdout ?stderr) ?timeout ?env cmd f
let with_process_in ?timeout ?env ?stdin ?stderr cmd f = make_with (open_process_in ?stdin ?stderr) ?timeout ?env cmd f
let with_process_out ?timeout ?env ?stdout ?stderr cmd f = make_with (open_process_out ?stdout ?stderr) ?timeout ?env cmd f
let with_process ?timeout ?env ?stderr cmd f = make_with (open_process ?stderr) ?timeout ?env cmd f
let with_process_full ?timeout ?env cmd f = make_with open_process_full ?timeout ?env cmd f

(* +-----------------------------------------------------------------+
   | High-level functions                                            |
   +-----------------------------------------------------------------+ *)

let exec ?timeout ?env ?stdin ?stdout ?stderr cmd = (open_process_none ?timeout ?env ?stdin ?stdout ?stderr cmd)#close

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

let pread ?timeout ?env ?stdin ?stderr cmd =
  recv (open_process_in ?timeout ?env ?stdin ?stderr cmd)

let pread_chars ?timeout ?env ?stdin ?stderr cmd =
  recv_chars (open_process_in ?timeout ?env ?stdin ?stderr cmd)

let pread_line ?timeout ?env ?stdin ?stderr cmd =
  recv_line (open_process_in ?timeout ?env ?stdin ?stderr cmd)

let pread_lines ?timeout ?env ?stdin ?stderr cmd =
  recv_lines (open_process_in ?timeout ?env ?stdin ?stderr cmd)

(* Sending *)

let send f pr data =
  let oc = pr#stdin in
  try_lwt
    f oc data
  finally
    Lwt_io.close oc

let pwrite ?timeout ?env ?stdout ?stderr cmd text =
  send Lwt_io.write (open_process_out ?timeout ?env ?stdout ?stderr cmd) text

let pwrite_chars ?timeout ?env ?stdout ?stderr cmd chars =
  send Lwt_io.write_chars (open_process_out ?timeout ?env ?stdout ?stderr cmd) chars

let pwrite_line ?timeout ?env ?stdout ?stderr cmd line =
  send Lwt_io.write_line (open_process_out ?timeout ?env ?stdout ?stderr cmd) line

let pwrite_lines ?timeout ?env ?stdout ?stderr cmd lines =
  send Lwt_io.write_lines (open_process_out ?timeout ?env ?stdout ?stderr cmd) lines

(* Mapping *)

(* Dump something to a command: *)
let dump f pr data =
  let oc = pr#stdin in
  ignore_result (try_lwt
                   f oc data
                 finally
                   Lwt_io.close oc)

let pmap ?timeout ?env ?stderr cmd text =
  let pr = open_process ?timeout ?env ?stderr cmd in
  dump Lwt_io.write pr text;
  recv pr

let pmap_chars ?timeout ?env ?stderr cmd chars =
  let pr = open_process ?timeout ?env ?stderr cmd in
  dump Lwt_io.write_chars pr chars;
  recv_chars pr

let pmap_line ?timeout ?env ?stderr cmd line =
  let pr = open_process ?timeout ?env ?stderr cmd in
  dump Lwt_io.write_line pr line;
  recv_line pr

let pmap_lines ?timeout ?env ?stderr cmd lines =
  let pr = open_process ?timeout ?env ?stderr cmd in
  dump Lwt_io.write_lines pr lines;
  recv_lines pr
