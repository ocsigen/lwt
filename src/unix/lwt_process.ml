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

#include "src/unix/lwt_config.ml"

open Lwt

type command = string * string array

#if windows
let shell cmd = ("", [|"cmd.exe"; "/c"; "\000" ^ cmd|])
#else
let shell cmd = ("", [|"/bin/sh"; "-c"; cmd|])
#endif

type redirection =
    [ `Keep
    | `Dev_null
    | `Close
    | `FD_copy of Unix.file_descr
    | `FD_move of Unix.file_descr ]

(* +-----------------------------------------------------------------+
   | OS-depentent command spawning                                   |
   +-----------------------------------------------------------------+ *)

type proc = {
  id : int;
  (* The process id. *)
  fd : Unix.file_descr;
  (* A handle on windows, and a dummy value of Unix. *)
}

#if windows

let get_fd fd redirection =
  match redirection with
    | `Keep ->
        Some fd
    | `Dev_null ->
        Some (Unix.openfile "nul" [Unix.O_RDWR] 0o666)
    | `Close ->
        None
    | `FD_copy fd' ->
        Some fd'
    | `FD_move fd' ->
        Some fd'

external create_process : string option -> string -> string option -> (Unix.file_descr option * Unix.file_descr option * Unix.file_descr option) -> proc = "lwt_process_create_process"

let quote arg =
  if String.length arg > 0 && arg.[0] = '\000' then
    String.sub arg 1 (String.length arg - 1)
  else
    Filename.quote arg

let spawn (prog, args) env ?(stdin:redirection=`Keep) ?(stdout:redirection=`Keep) ?(stderr:redirection=`Keep) toclose =
  let cmdline = String.concat " " (List.map quote (Array.to_list args)) in
  let env =
    match env with
      | None ->
          None
      | Some env ->
          let len = Array.fold_left (fun len str -> String.length str + len + 1) 1 env in
          let res = String.create len in
          let ofs =
            Array.fold_left
              (fun ofs str ->
                 let len = String.length str in
                 String.blit str 0 res ofs len;
                 res.[ofs + len] <- '\000';
                 ofs + len + 1)
              0 env
          in
          res.[ofs] <- '\000';
          Some res
  in
  List.iter Unix.set_close_on_exec toclose;
  let proc = create_process (if prog = "" then None else Some prog) cmdline env (get_fd Unix.stdin stdin, get_fd Unix.stdout stdout, get_fd Unix.stderr stderr) in
  let close = function
    | `FD_move fd ->
        Unix.close fd
    | _ ->
        ()
  in
  close stdin;
  close stdout;
  close stderr;
  proc

external wait_job : Unix.file_descr -> int Lwt_unix.job  = "lwt_process_wait_job"

let waitproc proc =
  lwt code = Lwt_unix.run_job (wait_job proc.fd) in
  return (proc.id, Lwt_unix.WEXITED code, { Lwt_unix.ru_utime = 0.; Lwt_unix.ru_stime = 0. })

external terminate_process : Unix.file_descr -> int -> unit = "lwt_process_terminate_process"

let terminate proc =
  terminate_process proc.fd 1

#else

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

let rec need_inline args idx =
  if idx = Array.length args then
    false
  else
    let arg = args.(idx) in
    (String.length arg > 0 && arg.[0] = '\000') || need_inline args (idx + 1)

let split arg =
  let rec search_start i =
    if i = String.length arg then
      []
    else
      match arg.[i] with
        | ' ' | '\t' ->
            search_start (i + 1)
        | _ ->
            loop_word i (i + 1)
  and loop_word i j =
    if j = String.length arg then
      [String.sub arg i (j - i)]
    else
      match arg.[i] with
        | ' ' | '\t' ->
            String.sub arg i (j - i) :: search_start (j + 1)
        | _ ->
            loop_word i (j + 1)
  in
  search_start 0

let inline arg =
  if String.length arg > 0 && arg.[0] = '\000' then
    split arg
  else
    [arg]

let map_args args =
  if need_inline args 0 then begin
    Array.of_list (List.flatten (List.map inline (Array.to_list args)))
  end else
    args

let spawn (prog, args) env ?(stdin:redirection=`Keep) ?(stdout:redirection=`Keep) ?(stderr:redirection=`Keep) toclose =
  let prog = if prog = "" && Array.length args > 0 then args.(0) else prog in
  match Lwt_unix.fork () with
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
            (* Prevent exit hooks from running, they are not supposed
               to be executed here. *)
            Lwt_sequence.iter_node_l Lwt_sequence.remove Lwt_main.exit_hooks;
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
        { id; fd = Unix.stdin }

let waitproc proc = Lwt_unix.wait4 [] proc.id

let terminate proc =
  Unix.kill proc.id Sys.sigkill

#endif

(* +-----------------------------------------------------------------+
   | Objects                                                         |
   +-----------------------------------------------------------------+ *)

type state =
  | Running
  | Exited of Unix.process_status

let status (pid, status, rusage) = status
let rusage (pid, status, rusage) = rusage

external cast_chan : 'a Lwt_io.channel -> unit Lwt_io.channel = "%identity"
  (* Transform a channel into a channel that only support closing. *)

let ignore_close chan = ignore (Lwt_io.close chan)

class virtual common timeout proc channels =
  let wait = waitproc proc in
  let close = lazy(join (List.map Lwt_io.close channels) >> wait) in
object(self)

  method pid = proc.id

  method state =
    match Lwt.poll wait with
      | None -> Running
      | Some (pid, status, rusage) -> Exited status

  method kill signum =
    if Lwt.state wait = Lwt.Sleep then
      Unix.kill proc.id signum

  method terminate =
    if Lwt.state wait = Lwt.Sleep then
      terminate proc

  method close = Lwt.protected (Lazy.force close) >|= status
  method status = Lwt.protected wait >|= status
  method rusage = Lwt.protected wait >|= rusage

  initializer
    (* Ensure channels are closed when no longer used. *)
    List.iter (Gc.finalise ignore_close) channels;
    (* Handle timeout. *)
    match timeout with
      | None ->
          ()
      | Some dt ->
          ignore (
            (* Ignore errors since they can be obtained by
               self#close. *)
            Lwt.try_bind
              (fun () ->
                 Lwt.choose [Lwt_unix.sleep dt >> return false;
                             wait >> return true])
              (function
                 | true ->
                     return_unit
                 | false ->
                     self#terminate;
                     Lazy.force close >> Lwt.return_unit)
              (fun exn ->
                 (* The exception is dropped because it can be
                    obtained with self#close. *)
                 Lwt.return_unit)
          )
end

class process_none ?timeout ?env ?stdin ?stdout ?stderr cmd =
  let proc = spawn cmd env ?stdin ?stdout ?stderr [] in
object
  inherit common timeout proc []
end

class process_in ?timeout ?env ?stdin ?stderr cmd =
  let stdout_r, stdout_w = Unix.pipe () in
  let proc = spawn cmd env ?stdin ~stdout:(`FD_move stdout_w) ?stderr [stdout_r] in
  let stdout = Lwt_io.of_unix_fd ~mode:Lwt_io.input stdout_r in
object
  inherit common timeout proc [cast_chan stdout]
  method stdout = stdout
end

class process_out ?timeout ?env ?stdout ?stderr cmd =
  let stdin_r, stdin_w = Unix.pipe () in
  let proc = spawn cmd env ~stdin:(`FD_move stdin_r) ?stdout ?stderr [stdin_w] in
  let stdin = Lwt_io.of_unix_fd ~mode:Lwt_io.output stdin_w in
object
  inherit common timeout proc [cast_chan stdin]
  method stdin = stdin
end

class process ?timeout ?env ?stderr cmd =
  let stdin_r, stdin_w = Unix.pipe ()
  and stdout_r, stdout_w = Unix.pipe () in
  let proc = spawn cmd env ~stdin:(`FD_move stdin_r) ~stdout:(`FD_move stdout_w) ?stderr [stdin_w; stdout_r] in
  let stdin = Lwt_io.of_unix_fd ~mode:Lwt_io.output stdin_w
  and stdout = Lwt_io.of_unix_fd ~mode:Lwt_io.input stdout_r in
object
  inherit common timeout proc [cast_chan stdin; cast_chan stdout]
  method stdin = stdin
  method stdout = stdout
end

class process_full ?timeout ?env cmd =
  let stdin_r, stdin_w = Unix.pipe ()
  and stdout_r, stdout_w = Unix.pipe ()
  and stderr_r, stderr_w = Unix.pipe () in
  let proc = spawn cmd env ~stdin:(`FD_move stdin_r) ~stdout:(`FD_move stdout_w) ~stderr:(`FD_move stderr_w) [stdin_w; stdout_r; stderr_r] in
  let stdin = Lwt_io.of_unix_fd ~mode:Lwt_io.output stdin_w
  and stdout = Lwt_io.of_unix_fd ~mode:Lwt_io.input stdout_r
  and stderr = Lwt_io.of_unix_fd ~mode:Lwt_io.input stderr_r in
object
  inherit common timeout proc [cast_chan stdin; cast_chan stdout; cast_chan stderr]
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

let read_opt read ic =
  try_lwt
    read ic >|= fun x -> Some x
  with Unix.Unix_error (Unix.EPIPE, _, _) | End_of_file ->
    return None

let recv_chars pr =
  let ic = pr#stdout in
  Gc.finalise ingore_close ic;
  Lwt_stream.from (fun _ ->
                     lwt x = read_opt Lwt_io.read_char ic in
                     if x = None then begin
                       lwt () = Lwt_io.close ic in
                       return x
                     end else
                       return x)

let recv_lines pr =
  let ic = pr#stdout in
  Gc.finalise ingore_close ic;
  Lwt_stream.from (fun _ ->
                     lwt x = read_opt Lwt_io.read_line ic in
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
    Lwt_io.close ic

let recv_line pr =
  let ic = pr#stdout in
  try_lwt
    Lwt_io.read_line ic
  finally
    Lwt_io.close ic

let send f pr data =
  let oc = pr#stdin in
  try_lwt
    f oc data
  finally
    Lwt_io.close oc

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

let pwrite ?timeout ?env ?stdout ?stderr cmd text =
  send Lwt_io.write (open_process_out ?timeout ?env ?stdout ?stderr cmd) text

let pwrite_chars ?timeout ?env ?stdout ?stderr cmd chars =
  send Lwt_io.write_chars (open_process_out ?timeout ?env ?stdout ?stderr cmd) chars

let pwrite_line ?timeout ?env ?stdout ?stderr cmd line =
  send Lwt_io.write_line (open_process_out ?timeout ?env ?stdout ?stderr cmd) line

let pwrite_lines ?timeout ?env ?stdout ?stderr cmd lines =
  send Lwt_io.write_lines (open_process_out ?timeout ?env ?stdout ?stderr cmd) lines

(* Mapping *)

type 'a map_state =
  | Init
  | Save of 'a option Lwt.t
  | Done

(* Monitor the thread [sender] in the stream [st] so write errors are
   reported. *)
let monitor sender st =
  let sender = sender >|= fun () -> None in
  let state = ref Init in
  Lwt_stream.from
    (fun () ->
       match !state with
         | Init ->
             let getter = Lwt.apply Lwt_stream.get st in
             let result _ =
               match Lwt.state sender with
                 | Lwt.Sleep ->
                     (* The sender is still sleeping, behave as the
                        getter. *)
                     getter
                 | Lwt.Return _ ->
                     (* The sender terminated successfully, we are
                        done monitoring it. *)
                     state := Done;
                     getter
                 | Lwt.Fail _ ->
                     (* The sender failed, behave as the sender for
                        this element and save current getter. *)
                     state := Save getter;
                     sender
             in
             Lwt.try_bind (fun () -> Lwt.choose [sender; getter]) result result
         | Save t ->
             state := Done;
             t
         | Done ->
             Lwt_stream.get st)

let pmap ?timeout ?env ?stderr cmd text =
  let pr = open_process ?timeout ?env ?stderr cmd in
  (* Start the sender and getter at the same time. *)
  let sender = send Lwt_io.write pr text in
  let getter = recv pr in
  try_lwt
    (* Wait for both to terminate, returning the result of the
       getter. *)
    sender >> getter
  with Lwt.Canceled as exn ->
    (* Cancel the getter if the sender was canceled. *)
    Lwt.cancel getter;
    raise_lwt exn

let pmap_chars ?timeout ?env ?stderr cmd chars =
  let pr = open_process ?timeout ?env ?stderr cmd in
  let sender = send Lwt_io.write_chars pr chars in
  monitor sender (recv_chars pr)

let pmap_line ?timeout ?env ?stderr cmd line =
  let pr = open_process ?timeout ?env ?stderr cmd in
  (* Start the sender and getter at the same time. *)
  let sender = send Lwt_io.write_line pr line in
  let getter = recv_line pr in
  try_lwt
    (* Wait for both to terminate, returning the result of the
       getter. *)
    sender >> getter
  with Lwt.Canceled as exn ->
    (* Cancel the getter if the sender was canceled. *)
    Lwt.cancel getter;
    raise_lwt exn

let pmap_lines ?timeout ?env ?stderr cmd lines =
  let pr = open_process ?timeout ?env ?stderr cmd in
  let sender = send Lwt_io.write_lines pr lines in
  monitor sender (recv_lines pr)
