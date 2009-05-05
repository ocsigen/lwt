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

let read_all_bytes ic =
  let buf = Buffer.create 512 in
  let rec loop _ =
    lwt b = Lwt_io.get_byte ic in
    Buffer.add_char buf b;
    loop ()
  in
  try
    loop ()
  with
    | End_of_file ->
        return (Buffer.contents buf)

let read_all_text ic =
  let buf = Buffer.create 512 in
  let rec loop _ =
    lwt ch = Lwt_io.read_char ic in
    Buffer.add_string buf ch;
    loop ()
  in
  try
    loop ()
  with
    | End_of_file ->
        return (Buffer.contents buf)

let recv_bytes ?env cmd =
  let pr = process_in ?env cmd in
  try_lwt
    read_all_bytes pr#stdout
  finally
    pr#close >> return ()

let recv_text ?env cmd =
  let pr = process_in ?env cmd in
  try_lwt
    read_all_text pr#stdout
  finally
    pr#close >> return ()

let recv_line ?env cmd =
  let pr = process_in ?env cmd in
  try_lwt
    Lwt_io.read_line pr#stdout
  finally
    pr#close >> return ()

let recv_lines ?env cmd =
  let pr = process_in ?env cmd in
  Lwt_stream.from (fun _ -> Lwt_io.peek_line pr#stdout >>= function
                     | None ->
                         pr#close >> return None
                     | some ->
                         return some)

let send_bytes ?env cmd bytes =
  let pr = process_out ?env cmd in
  try_lwt
    Lwt_io.put_bytes pr#stdin bytes >> pr#close
  finally
    pr#close >> return ()

let send_text ?env cmd text =
  let pr = process_out ?env cmd in
  try_lwt
    Lwt_io.write_text pr#stdin text >> pr#close
  finally
    pr#close >> return ()

let send_line ?env cmd line =
  let pr = process_out ?env cmd in
  try_lwt
    Lwt_io.write_line pr#stdin line >> pr#close
  finally
    pr#close >> return ()

let send_lines ?env ?sep cmd lines =
  let pr = process_out ?env cmd in
  try_lwt
    Lwt_io.write_lines ?sep pr#stdin lines >> pr#close
  finally
    pr#close >> return ()

let map_bytes ?env cmd bytes =
  let pr = process ?env cmd in
  try_lwt
    Lwt_io.put_bytes pr#stdin bytes >> Lwt_io.close pr#stdin >> read_all_bytes pr#stdout
  finally
    pr#close >> return ()

let map_text ?env cmd text =
  let pr = process ?env cmd in
  try_lwt
    Lwt_io.write_text pr#stdin text >> Lwt_io.close pr#stdin >> read_all_text pr#stdout
  finally
    pr#close >> return ()

let map_line ?env cmd line =
  let pr = process ?env cmd in
  try_lwt
    Lwt_io.write_line pr#stdin line >> Lwt_io.close pr#stdin >> Lwt_io.read_line pr#stdout
  finally
    pr#close >> return ()

let map_lines ?env ?sep cmd lines =
  let pr = process ?env cmd in
  ignore_result (try_lwt
                   Lwt_io.write_lines ?sep pr#stdin lines
                 finally
                   pr#close >> return ());
  Lwt_stream.from (fun _ -> Lwt_io.peek_line pr#stdout >>= function
                     | None ->
                         pr#close >> return None
                     | some ->
                         return some)
