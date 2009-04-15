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

let sh cmd = ("/bin/sh", [| "/bin/sh"; "-c"; cmd |])

class type process_common = object
  method pid : int
  method close : Unix.process_status Lwt.t
end

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

class process_in ?env cmd =
  let stdout_r, stdout_w = Unix.pipe () in
  let pid = spawn cmd env Unix.stdin stdout_w Unix.stderr [stdout_r] in
  let stdout = Lwt_io.of_unix_fd ~mode:Lwt_io.input stdout_r
  and w = Lwt_unix.waitpid [] pid in
  let close = lazy
    (perform
       Lwt_io.close stdout;
       (pid, ret) <-- w;
       return ret) in
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
  let close = lazy
    (perform
       Lwt_io.close stdin;
       (pid, ret) <-- w;
       return ret) in
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
  let close = lazy
    (perform
       Lwt_io.close stdin;
       Lwt_io.close stdout;
       (pid, ret) <-- w;
       return ret) in
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
  let close = lazy
    (perform
       Lwt_io.close stdin;
       Lwt_io.close stdout;
       (pid, ret) <-- w;
       return ret) in
object
  method pid = pid
  method close = Lazy.force close
  method stdin = stdin
  method stdout = stdout
  method stderr = stderr
end

let process_in ?env cmd = new process_in ?env cmd
let process_out ?env cmd = new process_out ?env cmd
let process ?env cmd = new process ?env cmd
let process_full ?env cmd = new process_full ?env cmd

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
  (perform
     loop ();
     status <-- p#close;
     return (status, Buffer.contents buf))

let get_output cmd =
  (perform
     (status, output) <-- get_status_output cmd;
     return output)
