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

(* +------------------+
   | Spawing commands |
   +------------------+ *)

let fd_move fd1 fd2 =
  Unix.dup2 fd1 fd2;
  Unix.close fd1

let spawn (prog, args) env stdin stdout stderr toclose =
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
          with
              _ -> exit 127
        end
    | id ->
        if stdin <> Unix.stdin then Unix.close stdin;
        if stdout <> Unix.stdout then Unix.close stdout;
        if stderr <> Unix.stderr then Unix.close stderr;
        id

class process_none ?env cmd =
  let pid = spawn cmd env Unix.stdin Unix.stdout Unix.stderr [] in
  let w = Lwt_unix.waitpid [] pid >|= snd in
object
  method pid = pid
  method close = w
end

class process_in ?env cmd =
  let stdout_r, stdout_w = Unix.pipe () in
  let pid = spawn cmd env Unix.stdin stdout_w Unix.stderr [stdout_r] in
  let stdout = Lwt_io.of_unix_fd ~mode:Lwt_io.input stdout_r
  and w = Lwt_unix.waitpid [] pid in
  let close = lazy(Lwt_io.close stdout >> w >|= snd) in
object
  method pid = pid
  method close = Lazy.force close
  method stdout = stdout
end

class process_out ?env cmd =
  let stdin_r, stdin_w = Unix.pipe () in
  let pid = spawn cmd env stdin_r Unix.stdout Unix.stderr [stdin_w] in
  let stdin = Lwt_io.of_unix_fd ~mode:Lwt_io.output stdin_w
  and w = Lwt_unix.waitpid [] pid in
  let close = lazy (Lwt_io.close stdin >> w >|= snd) in
object
  method pid = pid
  method close = Lazy.force close
  method stdin = stdin
end

class process ?env cmd =
  let stdin_r, stdin_w = Unix.pipe ()
  and stdout_r, stdout_w = Unix.pipe () in
  let pid = spawn cmd env stdin_r stdout_w Unix.stderr [stdin_w; stdout_r] in
  let stdin = Lwt_io.of_unix_fd ~mode:Lwt_io.output stdin_w
  and stdout = Lwt_io.of_unix_fd ~mode:Lwt_io.input stdout_r
  and w = Lwt_unix.waitpid [] pid in
  let close = lazy(Lwt_io.close stdin <&> Lwt_io.close stdout >> w >|= snd) in
object
  method pid = pid
  method close = Lazy.force close
  method stdin = stdin
  method stdout = stdout
end

class process_full ?env cmd =
  let stdin_r, stdin_w = Unix.pipe ()
  and stdout_r, stdout_w = Unix.pipe ()
  and stderr_r, stderr_w = Unix.pipe () in
  let pid = spawn cmd env stdin_r stdout_w stderr_w [stdin_w; stdout_r; stderr_r] in
  let stdin = Lwt_io.of_unix_fd ~mode:Lwt_io.output stdin_w
  and stdout = Lwt_io.of_unix_fd ~mode:Lwt_io.input stdout_r
  and stderr = Lwt_io.of_unix_fd ~mode:Lwt_io.input stderr_r
  and w = Lwt_unix.waitpid [] pid in
  let close = lazy(Lwt_io.close stdin <&> Lwt_io.close stdout >> w >|= snd) in
object
  method pid = pid
  method close = Lazy.force close
  method stdin = stdin
  method stdout = stdout
  method stderr = stderr
end

let process_none ?env cmd = new process_none ?env cmd
let process_in ?env cmd = new process_in ?env cmd
let process_out ?env cmd = new process_out ?env cmd
let process ?env cmd = new process ?env cmd
let process_full ?env cmd = new process_full ?env cmd

let make_with backend ?env cmd f =
  let process = backend ?env cmd in
  try_lwt
    f process
  finally
    process#close >> return ()

let with_process_none ?env cmd f = make_with process_none ?env cmd f
let with_process_in ?env cmd f = make_with process_in ?env cmd f
let with_process_out ?env cmd f = make_with process_out ?env cmd f
let with_process ?env cmd f = make_with process ?env cmd f
let with_process_full ?env cmd f = make_with process_full ?env cmd f

let get_status (prog, args) =
  match Unix.fork () with
    | 0 ->
        begin try
          Unix.execv prog args
        with _ ->
          exit 127
        end
    | id ->
        Lwt_unix.waitpid [] id >>= fun (pid, status) -> Lwt.return status

let get_status_output cmd =
  let buf = Buffer.create 1024 in
  let p = process_in cmd in
  let rec loop () =
    Lwt_io.direct_access (p#stdout)
      (fun str ofs len ->
         Buffer.add_substring buf str ofs len;
         return (len, len))
    >>= function
      | 0 ->
          return ()
      | _ ->
          loop ()
  in
  loop () >>
  lwt status = p#close in
  return (status, Buffer.contents buf)

let get_output cmd = get_status_output cmd >|= snd

(* +----------------------+
   | High-level functions |
   +----------------------+ *)

let exec ?env cmd = (process_none ?env cmd)#close

(* Make a stream from the output of a command: *)
let make_stream f pr =
  let ic = pr#stdout in
  Lwt_stream.from (fun _ ->
                     try_lwt
                       f ic >|= fun x -> Some x
                     with
                       | End_of_file ->
                           Lwt_io.close ic >> return None)

let recv_bytes pr = make_stream Lwt_io.get_byte pr
let recv_chars pr = make_stream Lwt_io.read_char pr
let recv_lines pr = make_stream Lwt_io.read_line pr

let recv_byte_array pr =
  try_lwt
    Lwt_io.get_byte_array pr#stdout
  finally
    Lwt_io.close pr#stdout >> return ()

let recv_line pr =
  try_lwt
    Lwt_io.read_line pr#stdout
  finally
    Lwt_io.close pr#stdout >> return ()

let recv_text pr =
  try_lwt
    Lwt_io.read pr#stdout
  finally
    Lwt_io.close pr#stdout >> return ()

(* Receiving *)

let pget_byte_array ?env cmd =
  recv_byte_array (process_in ?env cmd)

let pget_bytes ?env cmd =
  recv_bytes (process_in ?env cmd)

let pread ?env cmd =
  recv_text (process_in ?env cmd)

let pread_chars ?env cmd =
  recv_chars (process_in ?env cmd)

let pread_line ?env cmd =
  recv_line (process_in ?env cmd)

let pread_lines ?env cmd =
  recv_lines (process_in ?env cmd)

(* Sending *)

let send f pr data =
  try_lwt
    f pr#stdin data >> pr#close
  finally
    Lwt_io.close pr#stdin >> return ()

let pput_byte_array ?env cmd byte_array =
  send Lwt_io.put_byte_array (process_out ?env cmd) byte_array

let pput_bytes ?env cmd byte_stream =
  send Lwt_io.put_bytes (process_out ?env cmd) byte_stream

let pwrite ?env cmd text =
  send Lwt_io.write (process_out ?env cmd) text

let pwrite_chars ?env cmd chars =
  send Lwt_io.write_chars (process_out ?env cmd) chars

let pwrite_line ?env cmd line =
  send Lwt_io.write_line (process_out ?env cmd) line

let pwrite_lines ?env ?sep cmd lines =
  send (Lwt_io.write_lines ?sep) (process_out ?env cmd) lines

(* Mapping *)

(* Dump something to a command: *)
let dump f pr data =
  let oc = pr#stdin in
  ignore_result (try_lwt
                   f oc data
                 finally
                   Lwt_io.close oc)

let pmap_byte_array ?env cmd byte_array =
  let pr = process ?env cmd in
  dump Lwt_io.put_byte_array pr byte_array;
  recv_byte_array pr

let pmap_bytes ?env cmd byte_stream =
  let pr = process ?env cmd in
  dump Lwt_io.put_bytes pr byte_stream;
  recv_bytes pr

let pmap ?env cmd text =
  let pr = process ?env cmd in
  dump Lwt_io.write pr text;
  recv_text pr

let pmap_chars ?env cmd chars =
  let pr = process ?env cmd in
  dump Lwt_io.write_chars pr chars;
  recv_chars pr

let pmap_line ?env cmd line =
  let pr = process ?env cmd in
  dump Lwt_io.write_line pr line;
  recv_line pr

let pmap_lines ?env ?sep cmd lines =
  let pr = process ?env cmd in
  dump (Lwt_io.write_lines ?sep) pr lines;
  recv_lines pr
