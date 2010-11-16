(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_unix
 * Copyright (C) 2005-2008 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *                    2009 Jérémie Dimino
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

module Int_map = Map.Make(struct type t = int let compare = compare end)

let windows_hack = Sys.os_type <> "Unix"

(* +-----------------------------------------------------------------+
   | Configuration                                                   |
   +-----------------------------------------------------------------+ *)

type async_method =
  | Async_none
  | Async_thread

let default_async_method_var = ref Async_thread

let () =
  try
    match Sys.getenv "LWT_ASYNC_METHOD" with
      | "none" ->
          default_async_method_var := Async_none
      | "thread" ->
          default_async_method_var := Async_thread
      | str ->
          Printf.eprintf
            "%s: invalid lwt async method: '%s', must be 'none' or 'thread'\n%!"
            (Filename.basename Sys.executable_name) str
  with Not_found ->
    ()

let default_async_method () = !default_async_method_var
let set_default_async_method am = default_async_method_var := am

let async_method_key = Lwt.new_key ()

let async_method () =
  match Lwt.get async_method_key with
    | Some am -> am
    | None -> !default_async_method_var

let set_async_method am =
  Lwt.set async_method_key (Some am)

(* +-----------------------------------------------------------------+
   | Notifications management                                        |
   +-----------------------------------------------------------------+ *)

(* Informations about a notifier *)
type notifier = {
  notify_handler : unit -> unit;
  (* The callback *)

  notify_once : bool;
  (* Whether to remove the notifier after the reception of the first
     notification *)
}

let notifiers = ref Int_map.empty

let current_notification_id = ref 0

let make_notification ?(once=false) f =
  incr current_notification_id;
  while Int_map.mem !current_notification_id !notifiers do
    incr current_notification_id
  done;
  notifiers := Int_map.add !current_notification_id { notify_once = once; notify_handler = f } !notifiers;
  !current_notification_id

let stop_notification id =
  notifiers := Int_map.remove id !notifiers

let set_notification id f =
  let notifier = Int_map.find id !notifiers in
  notifiers := Int_map.add id { notifier with notify_handler = f } !notifiers

(* +-----------------------------------------------------------------+
   | libev suff                                                      |
   +-----------------------------------------------------------------+ *)

type io_event = Read | Write

type ev_io
type ev_signal
type ev_timer
type ev_child

external ev_io_init : Unix.file_descr -> io_event -> (ev_io -> unit) -> ev_io = "lwt_libev_io_init"
external ev_io_stop : ev_io -> unit = "lwt_libev_io_stop"
external ev_signal_init : int -> (ev_signal -> unit) -> ev_signal = "lwt_libev_signal_init"
external ev_signal_stop : ev_signal -> unit  = "lwt_libev_signal_stop"
external ev_timer_init : float -> (ev_timer -> unit) -> ev_timer = "lwt_libev_timer_init"
external ev_timer_stop : ev_timer -> unit  = "lwt_libev_timer_stop"

(* +-----------------------------------------------------------------+
   | Sleepers                                                        |
   +-----------------------------------------------------------------+ *)

let sleep d =
  let waiter, wakener = Lwt.task () in
  let ev = ev_timer_init d (fun ev ->
                              ev_timer_stop ev;
                              Lwt.wakeup wakener ()) in
  Lwt.on_cancel waiter (fun () -> ev_timer_stop ev);
  waiter

let yield = Lwt_main.yield

let auto_yield timeout =
  let limit = ref (Unix.gettimeofday () +. timeout) in
  fun () ->
    let current = Unix.gettimeofday () in
    if current >= !limit then begin
      limit := current +. timeout;
      yield ();
    end else
      return ()

exception Timeout

let timeout d = sleep d >> Lwt.fail Timeout

let with_timeout d f = Lwt.pick [timeout d; Lwt.apply f ()]

(* +-----------------------------------------------------------------+
   | File descriptor wrappers                                        |
   +-----------------------------------------------------------------+ *)

type state = Opened | Closed | Aborted of exn

type file_descr = {
  fd : Unix.file_descr;
  (* The underlying unix file descriptor *)

  mutable state: state;
  (* The state of the file descriptor *)

  mutable blocking : bool;
  (* Is the file descriptor in blocking or non-blocking mode *)
}

let mk_ch ?(blocking=false) ?(set_flags=true) fd =
  if set_flags then begin
    if blocking then
      Unix.clear_nonblock fd
    else
      Unix.set_nonblock fd
  end;
  { fd = fd; state = Opened; blocking = blocking }

let rec check_descriptor ch =
  match ch.state with
    | Opened ->
        ()
    | Aborted e ->
        raise e
    | Closed ->
        raise (Unix.Unix_error (Unix.EBADF, "check_descriptor", ""))

let state ch = ch.state

let blocking ch =
  check_descriptor ch;
  ch.blocking

let set_blocking ?(set_flags=true) ch blocking =
  check_descriptor ch;
  ch.blocking <- blocking;
  if set_flags then begin
    if blocking then
      Unix.clear_nonblock ch.fd
    else
      Unix.set_nonblock ch.fd
  end

external readable : Unix.file_descr -> bool = "lwt_unix_readable" "noalloc"
external writable : Unix.file_descr -> bool = "lwt_unix_writable" "noalloc"

let set_state ch st =
  ch.state <- st

let abort ch e =
  if ch.state <> Closed then
    set_state ch (Aborted e)

let unix_file_descr ch = ch.fd

let of_unix_file_descr = mk_ch

let stdin = of_unix_file_descr ~blocking:true Unix.stdin
let stdout = of_unix_file_descr ~blocking:true Unix.stdout
let stderr = of_unix_file_descr ~blocking:true Unix.stderr

(* +-----------------------------------------------------------------+
   | Actions on file descriptors                                     |
   +-----------------------------------------------------------------+ *)

exception Retry
exception Retry_write
exception Retry_read

type 'a outcome =
  | Success of 'a
  | Exn of exn
  | Requeued of io_event

(* Retry a queued syscall, [cont] is the thread to wakeup
   if the action succeed: *)
let rec retry_syscall ev event ch wakener action =
  let res =
    try
      check_descriptor ch;
      Success(action ())
    with
      | Retry
      | Unix.Unix_error((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _)
      | Sys_blocked_io ->
          (* EINTR because we are catching SIG_CHLD hence the system
             call might be interrupted to handle the signal; this lets
             us restart the system call eventually. *)
          Requeued event
      | Retry_read ->
          Requeued Read
      | Retry_write ->
          Requeued Write
      | e ->
          Exn e
  in
  match res with
    | Success v ->
        ev_io_stop ev;
        Lwt.wakeup wakener v
    | Exn e ->
        ev_io_stop ev;
        Lwt.wakeup_exn wakener e
    | Requeued event' ->
        if event <> event' then begin
          ev_io_stop ev;
          ignore (ev_io_init ch.fd event' (fun ev -> retry_syscall ev event' ch wakener action))
        end

let register_action event ch action =
  let waiter, wakener = Lwt.task () in
  let ev = ev_io_init ch.fd event (fun ev -> retry_syscall ev event ch wakener action) in
  Lwt.on_cancel waiter (fun () -> ev_io_stop ev);
  waiter

(* Wraps a system call *)
let wrap_syscall event ch action =
  try
    check_descriptor ch;
    if not ch.blocking || (event = Read && readable ch.fd) || (event == Write && writable ch.fd) then
      return (action ())
    else
      register_action event ch action
  with
    | Retry
    | Unix.Unix_error((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _)
    | Sys_blocked_io ->
        (* The action could not be completed immediatly, register it: *)
        register_action event ch action
    | Retry_read ->
        register_action Read ch action
    | Retry_write ->
        register_action Write ch action
    | e ->
        raise_lwt e

(* +-----------------------------------------------------------------+
   | Jobs                                                            |
   +-----------------------------------------------------------------+ *)

type 'a job

external start_job : 'a job -> int -> async_method -> unit = "lwt_unix_start_job"
    (* Starts the given job with given parameters. *)

external check_job : 'a job -> bool = "lwt_unix_check_job" "noalloc"
    (* Check whether that a job has terminated or not. If it has not
       yet terminated, it is marked so it will send a notification
       when it finishes. *)

external cancel_job : 'a job -> unit = "lwt_unix_cancel_job" "noalloc"
    (* Cancel the thread of the given job. *)

let execute_job ?async_method ~job ~result ~free =
  let async_method =
    match async_method with
      | Some am -> am
      | None ->
          match Lwt.get async_method_key with
            | Some am -> am
            | None -> !default_async_method_var
  in
  (* Create the notification for asynchronous wakeup. *)
  let id = make_notification ~once:true ignore in
  (* Starts the job. *)
  start_job job id async_method;
  lwt () =
    match async_method with
      | Async_none ->
          return ()
      | Async_thread ->
          try_lwt
            (* Give some time to the job before we fallback to
               asynchronous notification. *)
            pause ()
          with Canceled as exn ->
            cancel_job job;
            (* Free resources for when the job terminates. *)
            if check_job job then begin
              stop_notification id;
              free job
            end else
              set_notification id (fun () -> free job);
            raise_lwt exn
  in
  if check_job job then begin
    (* The job has already terminated, no need for asynchronous
       notification. *)
    stop_notification id;
    (* Read and return the result immediatly. *)
    let thread =
      try
        return (result job)
      with exn ->
        fail exn
    in
    free job;
    thread
  end else begin
    (* The job has not terminated, setup the notification for the
       asynchronous wakeup. *)
    let waiter, wakener = task () in
    set_notification id
      (fun () ->
         begin
           try
             wakeup wakener (result job);
           with exn ->
             wakeup_exn wakener exn
         end;
         free job);
    on_cancel waiter
      (fun () ->
         cancel_job job;
         set_notification id (fun () -> free job));
    waiter
  end

(* +-----------------------------------------------------------------+
   | System calls                                                    |
   +-----------------------------------------------------------------+ *)

let wait_read ch =
  try_lwt
    check_descriptor ch;
    if readable ch.fd then
      return ()
    else
      register_action Read ch ignore

external stub_read : Unix.file_descr -> string -> int -> int -> int = "lwt_unix_read"
external read_job : Unix.file_descr -> int -> [ `unix_read ] job = "lwt_unix_read_job"
external read_result : [ `unix_read ] job -> string -> int -> int = "lwt_unix_read_result" "noalloc"
external read_free : [ `unix_read ] job -> unit = "lwt_unix_read_free" "noalloc"

let read ch buf pos len =
  if pos < 0 || len < 0 || pos > String.length buf - len then
    invalid_arg "Lwt_unix.read"
  else if windows_hack then begin
    check_descriptor ch;
    register_action Read ch (fun () -> Unix.read ch.fd buf pos len)
  end else if ch.blocking then
    lwt () = wait_read ch in
    execute_job (read_job ch.fd len) (fun job -> read_result job buf pos) read_free
  else
    wrap_syscall Read ch (fun () -> stub_read ch.fd buf pos len)

let wait_write ch =
  try_lwt
    check_descriptor ch;
    if writable ch.fd then
      return ()
    else
      register_action Write ch ignore

external stub_write : Unix.file_descr -> string -> int -> int -> int = "lwt_unix_write"
external write_job : Unix.file_descr -> string -> int -> int -> [ `unix_write ] job = "lwt_unix_write_job"
external write_result : [ `unix_write ] job -> int = "lwt_unix_write_result" "noalloc"
external write_free : [ `unix_write ] job -> unit = "lwt_unix_write_free" "noalloc"

let write ch buf pos len =
  if pos < 0 || len < 0 || pos > String.length buf - len then
    invalid_arg "Lwt_unix.write"
  else if windows_hack then begin
    check_descriptor ch;
    register_action Read ch (fun () -> Unix.write ch.fd buf pos len)
  end else if ch.blocking then
    lwt () = wait_write ch in
    execute_job (write_job ch.fd buf pos len) write_result write_free
  else
    wrap_syscall Write ch (fun () -> stub_write ch.fd buf pos len)

external stub_recv : Unix.file_descr -> string -> int -> int -> Unix.msg_flag list -> int = "lwt_unix_recv"

let recv ch buf pos len flags =
  if pos < 0 || len < 0 || pos > String.length buf - len then
    invalid_arg "Lwt_unix.recv"
  else if windows_hack then
    wrap_syscall Read ch (fun () -> Unix.recv ch.fd buf pos len flags)
  else
    wrap_syscall Read ch (fun () -> stub_recv ch.fd buf pos len flags)

external stub_send : Unix.file_descr -> string -> int -> int -> Unix.msg_flag list -> int = "lwt_unix_send"

let send ch buf pos len flags =
  if pos < 0 || len < 0 || pos > String.length buf - len then
    invalid_arg "Lwt_unix.send"
  else if windows_hack then
    wrap_syscall Write ch (fun () -> Unix.send ch.fd buf pos len flags)
  else
    wrap_syscall Write ch (fun () -> stub_send ch.fd buf pos len flags)

let recvfrom ch buf ofs len flags =
  wrap_syscall Read ch (fun _ -> Unix.recvfrom ch.fd buf ofs len flags)

let sendto ch buf ofs len flags addr =
  wrap_syscall Write ch (fun _ -> Unix.sendto ch.fd buf ofs len flags addr)

type io_vector = {
  iov_buffer : string;
  iov_offset : int;
  iov_length : int;
}

let io_vector ~buffer ~offset ~length = {
  iov_buffer = buffer;
  iov_offset = offset;
  iov_length = length;
}

external stub_recv_msg : Unix.file_descr -> int -> io_vector list -> int * Unix.file_descr list = "lwt_unix_recv_msg"
external stub_send_msg : Unix.file_descr -> int -> io_vector list -> int -> Unix.file_descr list -> int = "lwt_unix_send_msg"

let check_io_vectors func_name iovs =
  List.iter (fun iov ->
               if iov.iov_offset < 0
                 || iov.iov_length < 0
                 || iov.iov_offset > String.length iov.iov_buffer - iov.iov_length then
                   invalid_arg func_name) iovs

let recv_msg ~socket ~io_vectors =
  check_io_vectors "Lwt_unix.recv_msg" io_vectors;
  let n_iovs = List.length io_vectors in
  wrap_syscall Read socket
    (fun () ->
       stub_recv_msg socket.fd n_iovs io_vectors)

let send_msg ~socket ~io_vectors ~fds =
  check_io_vectors "Lwt_unix.send_msg" io_vectors;
  let n_iovs = List.length io_vectors and n_fds = List.length fds in
  wrap_syscall Write socket
    (fun () ->
       stub_send_msg socket.fd n_iovs io_vectors n_fds fds)

type credentials = {
  cred_pid : int;
  cred_uid : int;
  cred_gid : int;
}

external stub_get_credentials : Unix.file_descr -> credentials = "lwt_unix_get_credentials"

let get_credentials ch = stub_get_credentials ch.fd

let pipe () =
  let (out_fd, in_fd) = Unix.pipe() in
  (mk_ch out_fd, mk_ch in_fd)

let pipe_in () =
  let (out_fd, in_fd) = Unix.pipe() in
  (mk_ch out_fd, in_fd)

let pipe_out () =
  let (out_fd, in_fd) = Unix.pipe() in
  (out_fd, mk_ch in_fd)

let socket dom typ proto =
  let s = Unix.socket dom typ proto in
  mk_ch s

let shutdown ch shutdown_command =
  check_descriptor ch;
  Unix.shutdown ch.fd shutdown_command

let socketpair dom typ proto =
  let (s1, s2) = Unix.socketpair dom typ proto in
  (mk_ch s1, mk_ch s2)

let accept ch =
  wrap_syscall Read ch (fun _ -> let (fd, addr) = Unix.accept ch.fd in (mk_ch fd, addr))

let accept_n ch n =
  let l = ref [] in
  try_lwt
    wrap_syscall Read ch begin fun () ->
      begin
        try
          for i = 1 to n do
            if ch.blocking && not (readable ch.fd) then raise Retry;
            let fd, addr = Unix.accept ch.fd in
            l := (mk_ch fd, addr) :: !l
          done
        with
          | (Unix.Unix_error((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _) | Retry) when !l <> [] ->
              (* Ignore blocking errors if we have at least one file-descriptor: *)
              ()
      end;
      (List.rev !l, None)
    end
  with exn ->
    return (List.rev !l, Some exn)

let connect ch addr =
  (* [in_progress] tell wether connection has started but not
     terminated: *)
  let in_progress = ref false in
  wrap_syscall Write ch begin fun _ ->
    if !in_progress then
      (* If the connection is in progress, [getsockopt_error] tells
         wether it succceed: *)
      match Unix.getsockopt_error ch.fd with
        | None ->
            (* The socket is connected *)
            ()
        | Some err ->
            (* An error happened: *)
            raise (Unix.Unix_error(err, "connect", ""))
    else
      try
        (* We should pass only one time here, unless the system call
           is interrupted by a signal: *)
        Unix.connect ch.fd addr
      with
        | Unix.Unix_error(Unix.EINPROGRESS, _, _) ->
            in_progress := true;
            raise Retry
  end

let close ch =
  if ch.state = Closed then check_descriptor ch;
  set_state ch Closed;
  Unix.close ch.fd

let setsockopt ch opt v =
  check_descriptor ch;
  Unix.setsockopt ch.fd opt v

let bind ch addr =
  check_descriptor ch;
  Unix.bind ch.fd addr

let listen ch cnt =
  check_descriptor ch;
  Unix.listen ch.fd cnt

let set_close_on_exec ch =
  check_descriptor ch;
  Unix.set_close_on_exec ch.fd

let clear_close_on_exec ch =
  check_descriptor ch;
  Unix.clear_close_on_exec ch.fd

external open_job : string -> Unix.open_flag list -> int -> [ `unix_open ] job = "lwt_unix_open_job"
external open_result : [ `unix_open ] job -> Unix.file_descr * bool = "lwt_unix_open_result"
external open_free : [ `unix_open ] job -> unit = "lwt_unix_open_free" "noalloc"

let openfile name flags perms =
  lwt fd, blocking =
    execute_job
      (open_job name flags perms)
      open_result
      open_free
  in
  return (of_unix_file_descr ~blocking fd)

let lseek ch offset whence =
  check_descriptor ch;
  Unix.lseek ch.fd offset whence

let ftruncate ch size =
  check_descriptor ch;
  Unix.ftruncate ch.fd size

let fstat ch =
  check_descriptor ch;
  Unix.fstat ch.fd

module LargeFile =
struct
  let lseek ch offset whence =
    check_descriptor ch;
    Unix.LargeFile.lseek ch.fd offset whence

  let ftruncate ch size =
    check_descriptor ch;
    Unix.LargeFile.ftruncate ch.fd size

  let fstat ch =
    check_descriptor ch;
    Unix.LargeFile.fstat ch.fd
end

let isatty ch =
  check_descriptor ch;
  Unix.isatty ch.fd

let fchmod ch perms =
  check_descriptor ch;
  Unix.fchmod ch.fd perms

let fchown ch perms =
  check_descriptor ch;
  Unix.fchown ch.fd perms

let dup ch =
  check_descriptor ch;
  of_unix_file_descr (Unix.dup ch.fd)

let dup2 ch1 ch2 =
  check_descriptor ch1;
  Unix.dup2 ch1.fd ch2.fd

let lockf ch cmd size =
  check_descriptor ch;
  Unix.lockf ch.fd cmd size

let getpeername ch =
  check_descriptor ch;
  Unix.getpeername ch.fd

let getsockname ch =
  check_descriptor ch;
  Unix.getsockname ch.fd

let getsockopt ch opt =
  check_descriptor ch;
  Unix.getsockopt ch.fd opt

let setsockopt ch opt x =
  check_descriptor ch;
  Unix.setsockopt ch.fd opt x

let getsockopt_int ch opt =
  check_descriptor ch;
  Unix.getsockopt_int ch.fd opt

let setsockopt_int ch opt x =
  check_descriptor ch;
  Unix.setsockopt_int ch.fd opt x

let getsockopt_optint ch opt =
  check_descriptor ch;
  Unix.getsockopt_optint ch.fd opt

let setsockopt_optint ch opt x =
  check_descriptor ch;
  Unix.setsockopt_optint ch.fd opt x

let getsockopt_float ch opt =
  check_descriptor ch;
  Unix.getsockopt_float ch.fd opt

let setsockopt_float ch opt x =
  check_descriptor ch;
  Unix.setsockopt_float ch.fd opt x

let getsockopt_error ch =
  check_descriptor ch;
  Unix.getsockopt_error ch.fd

let tcgetattr ch =
  check_descriptor ch;
  Unix.tcgetattr ch.fd

let tcsetattr ch when_ attr =
  check_descriptor ch;
  Unix.tcsetattr ch.fd when_ attr

let tcdrain ch =
  check_descriptor ch;
  Unix.tcdrain ch.fd

let tcflush ch q =
  check_descriptor ch;
  Unix.tcflush ch.fd q

let tcflow ch f =
  check_descriptor ch;
  Unix.tcflow ch.fd f

(* +-----------------------------------------------------------------+
   | Reading notifications                                           |
   +-----------------------------------------------------------------+ *)

(* Buffer used to receive notifications: *)
let notification_buffer = String.create 4

external init_notification : Unix.file_descr -> unit = "lwt_unix_init_notification"
external send_notification : int -> unit = "lwt_unix_send_notification_stub"

(* Pipe used to send/receive notifications *)
let notification_fd_reader, notification_fd_writer = pipe_in ()

let () =
  (* Send the writing side of the pipe to the C code: *)
  init_notification notification_fd_writer;

  set_close_on_exec notification_fd_reader;
  Unix.set_close_on_exec notification_fd_writer

(* Read one notification *)
let rec read_notification offset =
  if offset = 4 then
    return (Char.code notification_buffer.[0]
            lor (Char.code notification_buffer.[1] lsl 8)
            lor (Char.code notification_buffer.[2] lsl 16)
            lor (Char.code notification_buffer.[3] lsl 24))
  else
    read notification_fd_reader notification_buffer offset (4 - offset) >>= function
      | 0 ->
          raise_lwt End_of_file
      | n ->
          read_notification (offset + n)

(* Read continuously notifications *)
let rec read_notifications () =
  lwt id = read_notification 0 in
  match try Some(Int_map.find id !notifiers) with Not_found -> None with
    | Some notifier ->
        if notifier.notify_once then
          stop_notification id;
        notifier.notify_handler ();
        read_notifications ()
    | None ->
        read_notifications ()

let _ = read_notifications ()

(* +-----------------------------------------------------------------+
   | Signals                                                         |
   +-----------------------------------------------------------------+ *)

type signal_handler_id = unit Lazy.t

let on_signal signum handler =
  let ev = ev_signal_init signum (fun ev -> handler signum) in
  lazy(ev_signal_stop ev)

let disable_signal_handler stop =
  Lazy.force stop

(* +-----------------------------------------------------------------+
   | Processes                                                       |
   +-----------------------------------------------------------------+ *)

type resource_usage = { ru_utime : float; ru_stime : float }

external stub_wait4 : Unix.wait_flag list -> int -> int * Unix.process_status * resource_usage = "lwt_unix_wait4"
external stub_has_wait4 : unit -> bool = "lwt_unix_has_wait4"

let has_wait4 = stub_has_wait4 ()

let real_wait4 =
  if has_wait4 then
    stub_wait4
  else
    (fun flags pid ->
       let pid, status = Unix.waitpid flags pid in
       (pid, status, { ru_utime = 0.0; ru_stime = 0.0 }))

type child_waiter = {
  cw_notify_id : int;
  cw_flags : Unix.wait_flag list;
  cw_pid : int;
  mutable cw_result : (int * Unix.process_status * resource_usage) Lwt.t;
}

let wait_children = Lwt_sequence.create ()

let () =
  if not windows_hack then
    Sys.set_signal Sys.sigchld
      (Sys.Signal_handle
         (fun signum ->
            Lwt_sequence.iter_node_l
              (fun node ->
                 let cw = Lwt_sequence.get node in
                 try
                   let (pid, _, _) as result = real_wait4 cw.cw_flags cw.cw_pid in
                   if pid <> 0 then begin
                     Lwt_sequence.remove node;
                     cw.cw_result <- return result;
                     send_notification cw.cw_notify_id
                   end
                 with exn ->
                   Lwt_sequence.remove node;
                   cw.cw_result <- fail exn;
                   send_notification cw.cw_notify_id)
              wait_children))

let safe_waitpid flags pid =
  try_lwt
    return (Unix.waitpid flags pid)

let waitpid flags pid =
  if List.mem Unix.WNOHANG flags || windows_hack then
    safe_waitpid flags pid
  else
    let flags = Unix.WNOHANG :: flags in
    lwt (pid', _) as result = safe_waitpid flags pid in
    if pid' <> 0 then
      return result
    else begin
      let waiter, wakener = Lwt.task () in
      let child_waiter = {
        cw_notify_id = make_notification ~once:true (wakeup wakener);
        cw_flags = flags;
        cw_pid = pid;
        cw_result = fail Exit;
      } in
      let node = Lwt_sequence.add_l child_waiter wait_children in
      Lwt.on_cancel waiter (fun () -> Lwt_sequence.remove node);
      lwt () = waiter in
      lwt pid, status, _ = child_waiter.cw_result in
      return (pid, status)
    end

let safe_wait4 flags pid =
  try_lwt
    return (real_wait4 flags pid)

let wait4 flags pid =
  if List.mem Unix.WNOHANG flags || windows_hack then
    safe_wait4 flags pid
  else
    let flags = Unix.WNOHANG :: flags in
    lwt (pid', _, _) as result = safe_wait4 flags pid in
    if pid' <> 0 then
      return result
    else begin
      let waiter, wakener = Lwt.task () in
      let child_waiter = {
        cw_notify_id = make_notification ~once:true (wakeup wakener);
        cw_flags = flags;
        cw_pid = pid;
        cw_result = fail Exit;
      } in
      let node = Lwt_sequence.add_l child_waiter wait_children in
      Lwt.on_cancel waiter (fun () -> Lwt_sequence.remove node);
      lwt () = waiter in
      child_waiter.cw_result
    end

let wait () = waitpid [] (-1)

let system cmd =
  match Unix.fork () with
    | 0 ->
        begin try
          Unix.execv "/bin/sh" [| "/bin/sh"; "-c"; cmd |]
        with _ ->
          exit 127
        end
    | id ->
        waitpid [] id >|= snd

(* +-----------------------------------------------------------------+
   | Directories                                                     |
   +-----------------------------------------------------------------+ *)

type dir_handle = {
  dir_handle : Unix.dir_handle;
  auto_yield : unit -> unit Lwt.t;
}

let opendir name = {
  dir_handle = Unix.opendir name;
  auto_yield = auto_yield 0.05;
}

let readdir dh =
  lwt () = dh.auto_yield () in
  try
    return (Unix.readdir dh.dir_handle)
  with exn ->
    raise_lwt exn

let rewinddir dh = Unix.rewinddir dh.dir_handle
let closedir dh = Unix.closedir dh.dir_handle

(* +-----------------------------------------------------------------+
   | Misc                                                            |
   +-----------------------------------------------------------------+ *)

let run = Lwt_main.run

let handle_unix_error f x =
  try_lwt
    f x
  with exn ->
    Unix.handle_unix_error (fun () -> raise exn) ()
