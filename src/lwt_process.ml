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

let open_process_none ?env cmd = new process_none ?env cmd
let open_process_in ?env cmd = new process_in ?env cmd
let open_process_out ?env cmd = new process_out ?env cmd
let open_process ?env cmd = new process ?env cmd
let open_process_full ?env cmd = new process_full ?env cmd

let make_with backend ?env cmd f =
  let process = backend ?env cmd in
  try_lwt
    f process
  finally
    process#close >> return ()

let with_process_none ?env cmd f = make_with open_process_none ?env cmd f
let with_process_in ?env cmd f = make_with open_process_in ?env cmd f
let with_process_out ?env cmd f = make_with open_process_out ?env cmd f
let with_process ?env cmd f = make_with open_process ?env cmd f
let with_process_full ?env cmd f = make_with open_process_full ?env cmd f

(* +-----------------------------------------------------------------+
   | High-level functions                                            |
   +-----------------------------------------------------------------+ *)

let exec ?env cmd = (open_process_none ?env cmd)#close

let recv_bytes pr =
  let ic = pr#stdout in
  Lwt_stream.from (fun _ ->
                     try_lwt
                       Lwt_io.read_byte ic >|= fun x -> Some x
                     with
                       | End_of_file ->
                           Lwt_io.close ic >> return None)

let recv_chars pr =
  let ic = Lwt_text.make pr#stdout in
  Lwt_stream.from (fun _ ->
                     try_lwt
                       Lwt_text.read_char ic >|= fun x -> Some x
                     with
                       | End_of_file ->
                           Lwt_text.close ic >> return None)

let recv_lines pr =
  let ic = Lwt_text.make pr#stdout in
  Lwt_stream.from (fun _ ->
                     try_lwt
                       Lwt_text.read_line ic >|= fun x -> Some x
                     with
                       | End_of_file ->
                           Lwt_text.close ic >> return None)

let recv_byte_array pr =
  let ic = pr#stdout in
  try_lwt
    Lwt_io.read_byte_array ic
  finally
    Lwt_io.close ic >> return ()

let recv_line pr =
  let ic = Lwt_text.make pr#stdout in
  try_lwt
    Lwt_text.read_line ic
  finally
    Lwt_text.close ic >> return ()

let recv_text pr =
  let ic = Lwt_text.make pr#stdout in
  try_lwt
    Lwt_text.read ic
  finally
    Lwt_text.close ic >> return ()

(* Receiving *)

let pread_byte_array ?env cmd =
  recv_byte_array (open_process_in ?env cmd)

let pread_bytes ?env cmd =
  recv_bytes (open_process_in ?env cmd)

let pread ?env cmd =
  recv_text (open_process_in ?env cmd)

let pread_chars ?env cmd =
  recv_chars (open_process_in ?env cmd)

let pread_line ?env cmd =
  recv_line (open_process_in ?env cmd)

let pread_lines ?env cmd =
  recv_lines (open_process_in ?env cmd)

(* Sending *)

let sendb f pr data =
  let oc = pr#stdin in
  try_lwt
    f oc data
  finally
    Lwt_io.close oc >> return ()

let sendt f pr data =
  let oc = Lwt_text.make pr#stdin in
  try_lwt
    f oc data
  finally
    Lwt_text.close oc >> return ()

let pwrite_byte_array ?env cmd byte_array =
  sendb Lwt_io.write_byte_array (open_process_out ?env cmd) byte_array

let pwrite_bytes ?env cmd byte_stream =
  sendb Lwt_io.write_bytes (open_process_out ?env cmd) byte_stream

let pwrite ?env cmd text =
  sendt Lwt_text.write (open_process_out ?env cmd) text

let pwrite_chars ?env cmd chars =
  sendt Lwt_text.write_chars (open_process_out ?env cmd) chars

let pwrite_line ?env cmd line =
  sendt Lwt_text.write_line (open_process_out ?env cmd) line

let pwrite_lines ?env cmd lines =
  sendt Lwt_text.write_lines (open_process_out ?env cmd) lines

(* Mapping *)

(* Dump something to a command: *)
let dumpb f pr data =
  let oc = pr#stdin in
  ignore_result (try_lwt
                   f oc data
                 finally
                   Lwt_io.close oc)

let dumpt f pr data =
  let oc = Lwt_text.make pr#stdin in
  ignore_result (try_lwt
                   f oc data
                 finally
                   Lwt_text.close oc)

let pmap_byte_array ?env cmd byte_array =
  let pr = open_process ?env cmd in
  dumpb Lwt_io.write_byte_array pr byte_array;
  recv_byte_array pr

let pmap_bytes ?env cmd byte_stream =
  let pr = open_process ?env cmd in
  dumpb Lwt_io.write_bytes pr byte_stream;
  recv_bytes pr

let pmap ?env cmd text =
  let pr = open_process ?env cmd in
  dumpt Lwt_text.write pr text;
  recv_text pr

let pmap_chars ?env cmd chars =
  let pr = open_process ?env cmd in
  dumpt Lwt_text.write_chars pr chars;
  recv_chars pr

let pmap_line ?env cmd line =
  let pr = open_process ?env cmd in
  dumpt Lwt_text.write_line pr line;
  recv_line pr

let pmap_lines ?env cmd lines =
  let pr = open_process ?env cmd in
  dumpt Lwt_text.write_lines pr lines;
  recv_lines pr
