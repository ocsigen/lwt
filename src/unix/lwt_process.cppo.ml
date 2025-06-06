(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



open Lwt.Infix

type command = string * string array

let shell =
  if Sys.win32 then
    fun cmd -> ("", [|"cmd.exe"; "/c"; "\000" ^ cmd|])
  else
    fun cmd -> ("", [|"/bin/sh"; "-c"; cmd|])

type redirection =
  [ `Keep
  | `Dev_null
  | `Close
  | `FD_copy of Unix.file_descr
  | `FD_move of Unix.file_descr ]

(* +-----------------------------------------------------------------+
   | OS-dependent command spawning                                   |
   +-----------------------------------------------------------------+ *)

type proc = {
  id : int;
  (* The process id. *)
  fd : Unix.file_descr;
  (* A handle on windows, and a dummy value of Unix. *)
}

let win32_get_fd fd redirection =
  match redirection with
  | `Keep ->
    Some fd
  | `Dev_null ->
    Some (Unix.openfile "nul" [Unix.O_RDWR; Unix.O_KEEPEXEC] 0o666)
  | `Close ->
    None
  | `FD_copy fd' ->
    Some fd'
  | `FD_move fd' ->
    Some fd'

external win32_create_process :
  string option -> string -> string option -> string option ->
  (Unix.file_descr option * Unix.file_descr option * Unix.file_descr option) ->
    proc = "lwt_process_create_process"

let win32_quote arg =
  if String.length arg > 0 && arg.[0] = '\000' then
    String.sub arg 1 (String.length arg - 1)
  else
    Filename.quote arg

let win32_spawn
    ?cwd
    ?(stdin:redirection=`Keep)
    ?(stdout:redirection=`Keep)
    ?(stderr:redirection=`Keep)
    (prog, args) env
  =
  let cmdline = String.concat " " (List.map win32_quote (Array.to_list args)) in
  let env =
    match env with
    | None ->
      None
    | Some env ->
      let len =
        Array.fold_left (fun len str -> String.length str + len + 1) 1 env in
      let res = Bytes.create len in
      let ofs =
        Array.fold_left
          (fun ofs str ->
             let len = String.length str in
             String.blit str 0 res ofs len;
             Bytes.set res (ofs + len) '\000';
             ofs + len + 1)
          0 env
      in
      Bytes.set res ofs '\000';
      Some (Bytes.unsafe_to_string res)
  in
  let stdin_fd  = win32_get_fd Unix.stdin stdin
  and stdout_fd = win32_get_fd Unix.stdout stdout
  and stderr_fd = win32_get_fd Unix.stderr stderr in
  let proc =
    win32_create_process
      (if prog = "" then None else Some prog) cmdline env cwd
      (stdin_fd, stdout_fd, stderr_fd)
  in
  let close fd fd' =
    match fd with
    | `FD_move _ | `Dev_null ->
      Unix.close (match fd' with Some fd' -> fd' | _ -> assert false)
    | _ -> ()
  in
  close stdin stdin_fd;
  close stdout stdout_fd;
  close stderr stderr_fd;
  proc

external win32_wait_job : Unix.file_descr -> int Lwt_unix.job =
  "lwt_process_wait_job"

let win32_waitproc proc =
  Lwt_unix.run_job (win32_wait_job proc.fd) >>= fun code ->
  Lwt.return
    (proc.id,
     Lwt_unix.WEXITED code,
     {Lwt_unix.ru_utime = 0.; Lwt_unix.ru_stime = 0.})

external win32_terminate_process : Unix.file_descr -> int -> unit =
  "lwt_process_terminate_process"

let win32_terminate proc =
  win32_terminate_process proc.fd 1

let unix_redirect fd redirection = match redirection with
  | `Keep ->
    ()
  | `Dev_null ->
    let dev_null = Unix.openfile "/dev/null" [Unix.O_RDWR; Unix.O_KEEPEXEC] 0o666 in
    Unix.dup2 ~cloexec:false dev_null fd;
    Unix.close dev_null
  | `Close ->
    Unix.close fd
  | `FD_copy fd' ->
    Unix.dup2 ~cloexec:false fd' fd
  | `FD_move fd' ->
    Unix.dup2 ~cloexec:false fd' fd;
    Unix.close fd'

#if OCAML_VERSION >= (5, 0, 0)
external unix_exit : int -> 'a = "caml_unix_exit"
#else
external unix_exit : int -> 'a = "unix_exit"
#endif

let unix_spawn
    ?cwd
    ?(stdin:redirection=`Keep)
    ?(stdout:redirection=`Keep)
    ?(stderr:redirection=`Keep)
    (prog, args) env
  =
  let prog = if prog = "" && Array.length args > 0 then args.(0) else prog in
  match Lwt_unix.fork () with
  | 0 ->
    unix_redirect Unix.stdin stdin;
    unix_redirect Unix.stdout stdout;
    unix_redirect Unix.stderr stderr;
    begin
      try
        begin match cwd with
          | None -> ()
          | Some dir ->
            Sys.chdir dir
        end;
        match env with
        | None ->
          Unix.execvp prog args
        | Some env ->
          Unix.execvpe prog args env
      with _ ->
        (* Do not run at_exit hooks *)
        unix_exit 127
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
    {id; fd = Unix.stdin}

let unix_waitproc proc = Lwt_unix.wait4 [] proc.id

let unix_terminate proc =
  Unix.kill proc.id Sys.sigkill

let spawn     = if Sys.win32 then win32_spawn     else unix_spawn
let waitproc  = if Sys.win32 then win32_waitproc  else unix_waitproc
let terminate = if Sys.win32 then win32_terminate else unix_terminate

(* +-----------------------------------------------------------------+
   | Objects                                                         |
   +-----------------------------------------------------------------+ *)

type state =
  | Running
  | Exited of Unix.process_status

let status (_pid, status, _rusage) = status
let rusage (_pid, _status, rusage) = rusage

external cast_chan : 'a Lwt_io.channel -> unit Lwt_io.channel = "%identity"
(* Transform a channel into a channel that only support closing. *)

let ignore_close chan = ignore (Lwt_io.close chan)

class virtual common timeout proc channels =
  let wait = waitproc proc in
  object(self)
    val mutable closed = false

    method pid = proc.id

    method state =
      match Lwt.poll wait with
      | None -> Running
      | Some (_pid, status, _rusage) -> Exited status

    method kill signum =
      if Lwt.state wait = Lwt.Sleep then
        Unix.kill proc.id signum

    method terminate =
      if Lwt.state wait = Lwt.Sleep then
        terminate proc

    method close =
      if closed then self#status
      else (
        closed <- true;
        Lwt.protected (Lwt.join (List.map Lwt_io.close channels))
        >>= fun () -> self#status
      )
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
               Lwt.choose [(Lwt_unix.sleep dt >>= fun () -> Lwt.return_false);
                           (wait >>= fun _ -> Lwt.return_true)])
            (function
              | true ->
                Lwt.return_unit
              | false ->
                self#terminate;
                self#close >>= fun _ -> Lwt.return_unit)
            (fun _ ->
               (* The exception is dropped because it can be
                  obtained with self#close. *)
               Lwt.return_unit)
        )
  end

class process_none ?timeout ?env ?cwd ?stdin ?stdout ?stderr cmd =
  let proc = spawn cmd env ?cwd ?stdin ?stdout ?stderr in
  object
    inherit common timeout proc []
  end

class process_in ?timeout ?env ?cwd ?stdin ?stderr cmd =
  let stdout_r, stdout_w = Lwt_unix.pipe_in ~cloexec:true () in
  let proc = spawn cmd env ?cwd ?stdin ~stdout:(`FD_move stdout_w) ?stderr in
  let stdout = Lwt_io.of_fd ~mode:Lwt_io.input stdout_r in
  object
    inherit common timeout proc [cast_chan stdout]
    method stdout = stdout
  end

class process_out ?timeout ?env ?cwd ?stdout ?stderr cmd =
  let stdin_r, stdin_w = Lwt_unix.pipe_out ~cloexec:true () in
  let proc = spawn cmd env ?cwd ~stdin:(`FD_move stdin_r) ?stdout ?stderr in
  let stdin = Lwt_io.of_fd ~mode:Lwt_io.output stdin_w in
  object
    inherit common timeout proc [cast_chan stdin]
    method stdin = stdin
  end

class process ?timeout ?env ?cwd ?stderr cmd =
  let stdin_r, stdin_w = Lwt_unix.pipe_out ~cloexec:true ()
  and stdout_r, stdout_w = Lwt_unix.pipe_in ~cloexec:true () in
  let proc =
    spawn
      cmd env ?cwd ~stdin:(`FD_move stdin_r) ~stdout:(`FD_move stdout_w) ?stderr
  in
  let stdin = Lwt_io.of_fd ~mode:Lwt_io.output stdin_w
  and stdout = Lwt_io.of_fd ~mode:Lwt_io.input stdout_r in
  object
    inherit common timeout proc [cast_chan stdin; cast_chan stdout]
    method stdin = stdin
    method stdout = stdout
  end

class process_full ?timeout ?env ?cwd cmd =
  let stdin_r, stdin_w = Lwt_unix.pipe_out ~cloexec:true ()
  and stdout_r, stdout_w = Lwt_unix.pipe_in ~cloexec:true ()
  and stderr_r, stderr_w = Lwt_unix.pipe_in ~cloexec:true () in
  let proc =
    spawn
      cmd env ?cwd
      ~stdin:(`FD_move stdin_r)
      ~stdout:(`FD_move stdout_w)
      ~stderr:(`FD_move stderr_w)
  in
  let stdin = Lwt_io.of_fd ~mode:Lwt_io.output stdin_w
  and stdout = Lwt_io.of_fd ~mode:Lwt_io.input stdout_r
  and stderr = Lwt_io.of_fd ~mode:Lwt_io.input stderr_r in
  object
    inherit
      common timeout proc [cast_chan stdin; cast_chan stdout; cast_chan stderr]
    method stdin = stdin
    method stdout = stdout
    method stderr = stderr
  end

let open_process_none ?timeout ?env ?cwd ?stdin ?stdout ?stderr cmd =
  new process_none ?timeout ?env ?cwd ?stdin ?stdout ?stderr cmd

let open_process_in ?timeout ?env ?cwd ?stdin ?stderr cmd =
  new process_in ?timeout ?env ?cwd ?stdin ?stderr cmd

let open_process_out ?timeout ?env ?cwd ?stdout ?stderr cmd =
  new process_out ?timeout ?env ?cwd ?stdout ?stderr cmd

let open_process ?timeout ?env ?cwd ?stderr cmd =
  new process ?timeout ?env ?cwd ?stderr cmd

let open_process_full ?timeout ?env ?cwd cmd =
  new process_full ?timeout ?env ?cwd cmd

let make_with backend ?timeout ?env ?cwd cmd f =
  let process = backend ?timeout ?env ?cwd cmd in
  Lwt.finalize
    (fun () -> f process)
    (fun () ->
       process#close >>= fun _ ->
       Lwt.return_unit)

let with_process_none ?timeout ?env ?cwd ?stdin ?stdout ?stderr cmd f =
  make_with (open_process_none ?stdin ?stdout ?stderr) ?timeout ?env ?cwd cmd f

let with_process_in ?timeout ?env ?cwd ?stdin ?stderr cmd f =
  make_with (open_process_in ?stdin ?stderr) ?timeout ?env ?cwd cmd f

let with_process_out ?timeout ?env ?cwd ?stdout ?stderr cmd f =
  make_with (open_process_out ?stdout ?stderr) ?timeout ?env ?cwd cmd f

let with_process ?timeout ?env ?cwd ?stderr cmd f =
  make_with (open_process ?stderr) ?timeout ?env ?cwd cmd f

let with_process_full ?timeout ?env ?cwd cmd f =
  make_with open_process_full ?timeout ?env ?cwd cmd f

(* +-----------------------------------------------------------------+
   | High-level functions                                            |
   +-----------------------------------------------------------------+ *)

let exec ?timeout ?env ?cwd ?stdin ?stdout ?stderr cmd =
  (open_process_none ?timeout ?env ?cwd ?stdin ?stdout ?stderr cmd)#close

let ignore_close ch =
  ignore (Lwt_io.close ch)

let read_opt read ic =
  Lwt.catch
    (fun () -> read ic >|= fun x -> Some x)
    (function
      | Unix.Unix_error (Unix.EPIPE, _, _) | End_of_file ->
        Lwt.return_none
      | exn -> Lwt.reraise exn)

let recv_chars pr =
  let ic = pr#stdout in
  Gc.finalise ignore_close ic;
  Lwt_stream.from (fun _ ->
    read_opt Lwt_io.read_char ic >>= fun x ->
    if x = None then begin
      Lwt_io.close ic >>= fun () ->
      Lwt.return x
    end else
      Lwt.return x)

let recv_lines pr =
  let ic = pr#stdout in
  Gc.finalise ignore_close ic;
  Lwt_stream.from (fun _ ->
    read_opt Lwt_io.read_line ic >>= fun x ->
    if x = None then begin
      Lwt_io.close ic >>= fun () ->
      Lwt.return x
    end else
      Lwt.return x)

let recv pr =
  let ic = pr#stdout in
  Lwt.finalize
    (fun () -> Lwt_io.read ic)
    (fun () -> Lwt_io.close ic)

let recv_line pr =
  let ic = pr#stdout in
  Lwt.finalize
    (fun () -> Lwt_io.read_line ic)
    (fun () -> Lwt_io.close ic)

let send f pr data =
  let oc = pr#stdin in
  Lwt.finalize
    (fun () -> f oc data)
    (fun () -> Lwt_io.close oc)

(* Receiving *)

let pread ?timeout ?env ?cwd ?stdin ?stderr cmd =
  recv (open_process_in ?timeout ?env ?cwd ?stdin ?stderr cmd)

let pread_chars ?timeout ?env ?cwd ?stdin ?stderr cmd =
  recv_chars (open_process_in ?timeout ?env ?cwd ?stdin ?stderr cmd)

let pread_line ?timeout ?env ?cwd ?stdin ?stderr cmd =
  recv_line (open_process_in ?timeout ?env ?cwd ?stdin ?stderr cmd)

let pread_lines ?timeout ?env ?cwd ?stdin ?stderr cmd =
  recv_lines (open_process_in ?timeout ?env ?cwd ?stdin ?stderr cmd)

(* Sending *)

let pwrite ?timeout ?env ?cwd ?stdout ?stderr cmd text =
  send Lwt_io.write (open_process_out ?timeout ?env ?cwd ?stdout ?stderr cmd) text

let pwrite_chars ?timeout ?env ?cwd ?stdout ?stderr cmd chars =
  send
    Lwt_io.write_chars
    (open_process_out ?timeout ?env ?cwd ?stdout ?stderr cmd)
    chars

let pwrite_line ?timeout ?env ?cwd ?stdout ?stderr cmd line =
  send
    Lwt_io.write_line
    (open_process_out ?timeout ?env ?cwd ?stdout ?stderr cmd)
    line

let pwrite_lines ?timeout ?env ?cwd ?stdout ?stderr cmd lines =
  send
    Lwt_io.write_lines
    (open_process_out ?timeout ?env ?cwd ?stdout ?stderr cmd)
    lines

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

let pmap ?timeout ?env ?cwd ?stderr cmd text =
  let pr = open_process ?timeout ?env ?cwd ?stderr cmd in
  (* Start the sender and getter at the same time. *)
  let sender = send Lwt_io.write pr text in
  let getter = recv pr in
  Lwt.catch
    (fun () ->
       (* Wait for both to terminate, returning the result of the
          getter. *)
       sender >>= fun () -> getter)
    (function
      | Lwt.Canceled as exn ->
        (* Cancel the getter if the sender was canceled. *)
        Lwt.cancel getter;
        Lwt.reraise exn
      | exn -> Lwt.reraise exn)

let pmap_chars ?timeout ?env ?cwd ?stderr cmd chars =
  let pr = open_process ?timeout ?env ?cwd ?stderr cmd in
  let sender = send Lwt_io.write_chars pr chars in
  monitor sender (recv_chars pr)

let pmap_line ?timeout ?env ?cwd ?stderr cmd line =
  let pr = open_process ?timeout ?env ?cwd ?stderr cmd in
  (* Start the sender and getter at the same time. *)
  let sender = send Lwt_io.write_line pr line in
  let getter = recv_line pr in
  Lwt.catch
    (fun () ->
       (* Wait for both to terminate, returning the result of the
          getter. *)
       sender >>= fun () -> getter)
    (function
      | Lwt.Canceled as exn ->
        (* Cancel the getter if the sender was canceled. *)
        Lwt.cancel getter;
        Lwt.reraise exn
      | exn -> Lwt.reraise exn)

let pmap_lines ?timeout ?env ?cwd ?stderr cmd lines =
  let pr = open_process ?timeout ?env ?cwd ?stderr cmd in
  let sender = send Lwt_io.write_lines pr lines in
  monitor sender (recv_lines pr)
