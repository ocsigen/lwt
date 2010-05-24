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

let windows_hack = Sys.os_type <> "Unix"

(* +-----------------------------------------------------------------+
   | Sleepers                                                        |
   +-----------------------------------------------------------------+ *)

type sleep = {
  time : float;
  mutable canceled : bool;
  thread : unit Lwt.u;
}

module SleepQueue =
  Lwt_pqueue.Make (struct
                     type t = sleep
                     let compare { time = t1 } { time = t2 } = compare t1 t2
                   end)

(* Threads waiting for a timeout to expire: *)
let sleep_queue = ref SleepQueue.empty

(* Sleepers added since the last iteration of the main loop:

   They are not added immediatly to the main sleep queue in order to
   prevent them from being wakeup immediatly by [restart_threads].
*)
let new_sleeps = ref []

let sleep d =
  let (res, w) = Lwt.task () in
  let t = if d <= 0. then 0. else Unix.gettimeofday () +. d in
  let sleeper = { time = t; canceled = false; thread = w } in
  new_sleeps := sleeper :: !new_sleeps;
  Lwt.on_cancel res (fun _ -> sleeper.canceled <- true);
  res

let yield () = sleep 0.

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

let in_the_past now t =
  t = 0. || t <= Lazy.force now

(* We use a lazy-value for [now] to avoid one system call if not
   needed: *)
let rec restart_threads now =
  match SleepQueue.lookup_min !sleep_queue with
    | Some{ canceled = true } ->
        sleep_queue := SleepQueue.remove_min !sleep_queue;
        restart_threads now
    | Some{ time = time; thread = thread } when in_the_past now time ->
        sleep_queue := SleepQueue.remove_min !sleep_queue;
        Lwt.wakeup thread ();
        restart_threads now
    | _ ->
        ()

(* +-----------------------------------------------------------------+
   | File descriptor wrappers                                        |
   +-----------------------------------------------------------------+ *)

type state = Open | Closed | Aborted of exn

type file_descr = {
  fd : Unix.file_descr;
  (* The underlying unix file descriptor *)

  mutable state: state;
  (* The state of the file descriptor *)

  mutable blocking : bool;
  (* Is the file descriptor in blocking or non-blocking mode *)
}

let mk_ch fd =
  if windows_hack then
    match (Unix.fstat fd).Unix.st_kind  with
      | Unix.S_SOCK ->
          Unix.set_nonblock fd;
          { fd = fd; state = Open; blocking = false }
      | _ ->
          { fd = fd; state = Open; blocking = true }
  else begin
    Unix.set_nonblock fd;
    { fd = fd; state = Open; blocking = false }
  end

let rec check_descriptor ch =
  match ch.state with
    | Open ->
        ()
    | Aborted e ->
        raise e
    | Closed ->
        raise (Unix.Unix_error (Unix.EBADF, "check_descriptor", ""))

let state ch = ch.state

let blocking ch =
  check_descriptor ch;
  ch.blocking

let set_blocking ch blocking =
  check_descriptor ch;
  ch.blocking <- blocking;
  if blocking then
    Unix.clear_nonblock ch.fd
  else
    Unix.set_nonblock ch.fd

let readable fd = Lwt_select.select [fd] [] [] 0.0 <> ([], [], [])
let writable fd = Lwt_select.select [] [fd] [] 0.0 <> ([], [], [])

(* +-----------------------------------------------------------------+
   | Actions on file descriptors                                     |
   +-----------------------------------------------------------------+ *)

module FdMap =
  Map.Make (struct type t = Unix.file_descr let compare = compare end)

type watchers = (file_descr * (unit -> unit) Lwt_sequence.t) FdMap.t ref

let inputs = ref FdMap.empty
let outputs = ref FdMap.empty

exception Retry
exception Retry_write
exception Retry_read

type 'a outcome =
  | Success of 'a
  | Exn of exn
  | Requeued of watchers

let get_actions ch set =
  try
    snd (FdMap.find ch.fd !set)
  with Not_found ->
    let seq = Lwt_sequence.create () in
    set := FdMap.add ch.fd (ch, seq) !set;
    seq

(* Retry a queued syscall, [cont] is the thread to wakeup
   if the action succeed: *)
let rec retry_syscall set ch cont action =
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
          Requeued set
      | Retry_read ->
          Requeued inputs
      | Retry_write ->
          Requeued outputs
      | e ->
          Exn e
  in
  match res with
    | Success v ->
        Lwt.wakeup cont v
    | Exn e ->
        Lwt.wakeup_exn cont e
    | Requeued set ->
        ignore (Lwt_sequence.add_r (fun _ -> retry_syscall set ch cont action) (get_actions ch set))

let register_action set ch action =
  let (res, w) = Lwt.task () in
  let actions = get_actions ch set in
  let node = Lwt_sequence.add_r (fun _ -> retry_syscall set ch w action) actions in
  (* Unregister the action on cancel: *)
  Lwt.on_cancel res begin fun _ ->
    Lwt_sequence.remove node;
    if Lwt_sequence.is_empty actions then
      (* If there is no other action queued on the file-descriptor,
         remove it: *)
      set := FdMap.remove ch.fd !set
  end;
  res

(* Wraps a system call *)
let wrap_syscall set ch action =
  try
    check_descriptor ch;
    if not ch.blocking || (set == inputs && readable ch.fd) || (set == outputs && writable ch.fd) then
      return (action ())
    else
      register_action set ch action
  with
    | Retry
    | Unix.Unix_error((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _)
    | Sys_blocked_io ->
        (* The action could not be completed immediatly, register it: *)
        register_action set ch action
    | Retry_read ->
        register_action inputs ch action
    | Retry_write ->
        register_action outputs ch action
    | e ->
        fail e

(* Performs all registered actions on [fd]: *)
let perform_actions set fd =
  try
    let (ch, actions) = FdMap.find fd !set in
    set := FdMap.remove fd !set;
    List.iter (fun f -> f ()) (Lwt_sequence.fold_l (fun x l -> x :: l) actions [])
  with Not_found ->
    ()

let active_descriptors set acc =
  FdMap.fold (fun key _ acc -> key :: acc) !set acc

let blocked_thread_count set =
  FdMap.fold (fun key (_, l) c -> Lwt_sequence.fold_l (fun _ x -> x + 1) l 0 + c) !set 0

(* +-----------------------------------------------------------------+
   | System calls                                                    |
   +-----------------------------------------------------------------+ *)

let set_state ch st =
  ch.state <- st;
  perform_actions inputs ch.fd;
  perform_actions outputs ch.fd

let abort ch e =
  if ch.state <> Closed then
    set_state ch (Aborted e)

let unix_file_descr ch = ch.fd

let of_unix_file_descr fd = mk_ch fd

let of_unix_file_descr_blocking fd = { fd = fd; state = Open; blocking = true }

let stdin = of_unix_file_descr_blocking Unix.stdin
let stdout = of_unix_file_descr_blocking Unix.stdout
let stderr = of_unix_file_descr_blocking Unix.stderr

external lwt_unix_read : Unix.file_descr -> string -> int -> int -> int = "lwt_unix_read"
external lwt_unix_write : Unix.file_descr -> string -> int -> int -> int = "lwt_unix_write"
external lwt_unix_recv : Unix.file_descr -> string -> int -> int -> Unix.msg_flag list -> int = "lwt_unix_recv"
external lwt_unix_send : Unix.file_descr -> string -> int -> int -> Unix.msg_flag list -> int = "lwt_unix_send"

let read ch buf pos len =
  if pos < 0 || len < 0 || pos > String.length buf - len then
    invalid_arg "Lwt_unix.read"
  else if windows_hack then begin
    check_descriptor ch;
    register_action inputs ch (fun () -> Unix.read ch.fd buf pos len)
  end else
    wrap_syscall inputs ch (fun () -> lwt_unix_read ch.fd buf pos len)

let write ch buf pos len =
  if pos < 0 || len < 0 || pos > String.length buf - len then
    invalid_arg "Lwt_unix.write"
  else if windows_hack then
    wrap_syscall outputs ch (fun () -> Unix.write ch.fd buf pos len)
  else
    wrap_syscall outputs ch (fun () -> lwt_unix_write ch.fd buf pos len)

let recv ch buf pos len flags =
  if pos < 0 || len < 0 || pos > String.length buf - len then
    invalid_arg "Lwt_unix.recv"
  else if windows_hack then
    wrap_syscall inputs ch (fun () -> Unix.recv ch.fd buf pos len flags)
  else
    wrap_syscall inputs ch (fun () -> lwt_unix_recv ch.fd buf pos len flags)

let send ch buf pos len flags =
  if pos < 0 || len < 0 || pos > String.length buf - len then
    invalid_arg "Lwt_unix.send"
  else if windows_hack then
    wrap_syscall outputs ch (fun () -> Unix.send ch.fd buf pos len flags)
  else
    wrap_syscall outputs ch (fun () -> lwt_unix_send ch.fd buf pos len flags)

let recvfrom ch buf ofs len flags =
  wrap_syscall inputs ch (fun _ -> Unix.recvfrom ch.fd buf ofs len flags)

let sendto ch buf ofs len flags addr =
  wrap_syscall outputs ch (fun _ -> Unix.sendto ch.fd buf ofs len flags addr)

let wait_read ch =
  try_lwt
    check_descriptor ch;
    if readable ch.fd then
      return ()
    else
      register_action inputs ch ignore

let wait_write ch =
  try_lwt
    check_descriptor ch;
    if writable ch.fd then
      return ()
    else
      register_action outputs ch ignore

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

external lwt_unix_recv_msg : Unix.file_descr -> int -> io_vector list -> int * Unix.file_descr list = "lwt_unix_recv_msg"
external lwt_unix_send_msg : Unix.file_descr -> int -> io_vector list -> int -> Unix.file_descr list -> int = "lwt_unix_send_msg"

let check_io_vectors func_name iovs =
  List.iter (fun iov ->
               if iov.iov_offset < 0
                 || iov.iov_length < 0
                 || iov.iov_offset > String.length iov.iov_buffer - iov.iov_length then
                   invalid_arg func_name) iovs

let recv_msg ~socket ~io_vectors =
  check_io_vectors "Lwt_unix.recv_msg" io_vectors;
  let n_iovs = List.length io_vectors in
  wrap_syscall inputs socket
    (fun () ->
       lwt_unix_recv_msg socket.fd n_iovs io_vectors)

let send_msg ~socket ~io_vectors ~fds =
  check_io_vectors "Lwt_unix.send_msg" io_vectors;
  let n_iovs = List.length io_vectors and n_fds = List.length fds in
  wrap_syscall outputs socket
    (fun () ->
       lwt_unix_send_msg socket.fd n_iovs io_vectors n_fds fds)

type credentials = {
  cred_pid : int;
  cred_uid : int;
  cred_gid : int;
}

external lwt_unix_get_credentials : Unix.file_descr -> credentials = "lwt_unix_get_credentials"

let get_credentials ch = lwt_unix_get_credentials ch.fd

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
  wrap_syscall inputs ch (fun _ -> let (fd, addr) = Unix.accept ch.fd in (mk_ch fd, addr))

let accept_n ch n =
  let l = ref [] in
  try_lwt
    wrap_syscall inputs ch begin fun () ->
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
  wrap_syscall outputs ch begin fun _ ->
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

let openfile name flags perms =
  of_unix_file_descr (Unix.openfile name flags perms)

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
   | Signals                                                         |
   +-----------------------------------------------------------------+ *)

module SignalMap = Map.Make(struct type t = int let compare = compare end)

type signal_handler = { sh_signum : int; sh_node : (int -> unit) Lwt_sequence.node }
type signal_handler_id = signal_handler option ref

(* The set of all monitored signals: *)
let signals : (int -> unit) Lwt_sequence.t SignalMap.t ref = ref SignalMap.empty

(* The list of pending signals *)
let pending_signals = Queue.create ()

let disable_signal_handler id = match !id with
  | Some sh -> begin
      id := None;
      Lwt_sequence.remove sh.sh_node;
      try
        if Lwt_sequence.is_empty (SignalMap.find sh.sh_signum !signals) then begin
          signals := SignalMap.remove sh.sh_signum !signals;
          Sys.set_signal sh.sh_signum Sys.Signal_default
        end
      with Not_found ->
        ()
    end
  | None ->
      ()

(* Pipe used to tell the main loop that a signal has been caught *)
let signal_fd_reader, signal_fd_writer = pipe_in ()
let () =
  set_close_on_exec signal_fd_reader;
  Unix.set_close_on_exec signal_fd_writer

let wakeup_pending_signals () =
  while not (Queue.is_empty pending_signals) do
    let signum = Queue.pop pending_signals in
    Lwt_sequence.iter_l (fun f -> f signum) (SignalMap.find signum !signals)
  done

(* Read and dispatch signals *)
let rec read_signals () =
  read signal_fd_reader (String.create 1) 0 1 >>= function
    | 0 ->
        return ()
    | 1 ->
        wakeup_pending_signals ();
        read_signals ()
    | _ ->
        assert false

(* Start listenning for signals *)
let _ = read_signals ()

let handle_signal signum =
  Queue.push signum pending_signals;
  ignore (Unix.write signal_fd_writer " " 0 1)

let on_signal signum f =
  match try Some(SignalMap.find signum !signals) with Not_found -> None with
    | Some seq ->
        ref (Some{ sh_signum = signum; sh_node = Lwt_sequence.add_r f seq })
    | None ->
        let seq = Lwt_sequence.create () in
        let id = ref (Some{ sh_signum = signum; sh_node = Lwt_sequence.add_r f seq }) in
        signals := SignalMap.add signum seq !signals;
        Sys.set_signal signum (Sys.Signal_handle handle_signal);
        id

(* +-----------------------------------------------------------------+
   | Processes                                                       |
   +-----------------------------------------------------------------+ *)

type resource_usage = { ru_utime : float; ru_stime : float }

external lwt_unix_wait4 : Unix.wait_flag list -> int -> int * Unix.process_status * resource_usage = "lwt_unix_wait4"
external lwt_unix_has_wait4 : unit -> bool = "lwt_unix_has_wait4"

let has_wait4 = lwt_unix_has_wait4 ()

let real_wait4 =
  if has_wait4 then
    lwt_unix_wait4
  else
    (fun flags pid ->
       let pid, status = Unix.waitpid flags pid in
       (pid, status, { ru_utime = 0.0; ru_stime = 0.0 }))

let wait_children = Lwt_sequence.create ()

let () =
  if not windows_hack then
    ignore begin
      on_signal Sys.sigchld
        (fun _ ->
           Lwt_sequence.iter_node_l begin fun node ->
             let cont, flags, pid = Lwt_sequence.get node in
             try
               let (pid', _, _) as v = lwt_unix_wait4 flags pid in
               if pid' <> 0 then begin
                 Lwt_sequence.remove node;
                 Lwt.wakeup cont v
               end
             with e ->
               Lwt_sequence.remove node;
               Lwt.wakeup_exn cont e
           end wait_children)
    end

let _waitpid flags pid =
  try_lwt
    return (Unix.waitpid flags pid)

let waitpid flags pid =
  if List.mem Unix.WNOHANG flags || windows_hack then
    _waitpid flags pid
  else
    let flags = Unix.WNOHANG :: flags in
    lwt ((pid', _) as res) = _waitpid flags pid in
    if pid' <> 0 then
      return res
    else begin
      let (res, w) = Lwt.task () in
      let node = Lwt_sequence.add_l (w, flags, pid) wait_children in
      Lwt.on_cancel res (fun _ -> Lwt_sequence.remove node);
      lwt (pid, status, _) = res in
      return (pid, status)
    end

let _wait4 flags pid =
  try_lwt
    return (lwt_unix_wait4 flags pid)

let wait4 flags pid =
  if List.mem Unix.WNOHANG flags || windows_hack then
    _wait4 flags pid
  else
    let flags = Unix.WNOHANG :: flags in
    lwt (pid', _, _) as res = _wait4 flags pid in
    if pid' <> 0 then
      return res
    else begin
      let (res, w) = Lwt.task () in
      let node = Lwt_sequence.add_l (w, flags, pid) wait_children in
      Lwt.on_cancel res (fun _ -> Lwt_sequence.remove node);
      res
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
   | Event loop                                                      |
   +-----------------------------------------------------------------+ *)

let run = Lwt_main.run

let rec get_next_timeout now timeout =
  match SleepQueue.lookup_min !sleep_queue with
    | Some{ canceled = true } ->
        sleep_queue := SleepQueue.remove_min !sleep_queue;
        get_next_timeout now timeout
    | Some{ time = time } ->
        Lwt_main.min_timeout timeout (Some(if time = 0. then 0. else max 0. (time -. (Lazy.force now))))
    | None ->
        timeout

let select_filter now select set_r set_w set_e timeout =
  (* Transfer all sleepers added since the last iteration to the main
     sleep queue: *)
  sleep_queue :=
    List.fold_left
      (fun q e -> SleepQueue.add e q) !sleep_queue !new_sleeps;
  new_sleeps := [];
  let (now, set_r, set_w, set_e) as result =
    select
      (active_descriptors inputs set_r)
      (active_descriptors outputs set_w)
      set_e
      (get_next_timeout now timeout)
  in
  (* Restart threads waiting for a timeout: *)
  restart_threads now;
  (* Restart threads waiting on a file descriptors: *)
  List.iter (fun fd -> perform_actions inputs fd) set_r;
  List.iter (fun fd -> perform_actions outputs fd) set_w;
  (* Wakeup signals is not needed here but this make signal delivering
     faster: *)
  wakeup_pending_signals ();
  result

let _ = Lwt_sequence.add_l select_filter Lwt_main.select_filters

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
    fail exn

let rewinddir dh = Unix.rewinddir dh.dir_handle
let closedir dh = Unix.closedir dh.dir_handle

(* +-----------------------------------------------------------------+
   | Misc                                                            |
   +-----------------------------------------------------------------+ *)

let handle_unix_error f x =
  try_lwt
    f x
  with exn ->
    Unix.handle_unix_error (fun () -> raise exn) ()

(* Monitoring functions *)
let inputs_length () = blocked_thread_count inputs
let outputs_length () = blocked_thread_count outputs
let wait_children_length () = Lwt_sequence.fold_l (fun _ x -> succ x) wait_children 0
let get_new_sleeps () = List.length !new_sleeps
let sleep_queue_size () = SleepQueue.size !sleep_queue
