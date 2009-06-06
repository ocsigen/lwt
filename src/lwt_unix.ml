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

(* +-----------------------------------------------------------------+
   | Sleepers                                                        |
   +-----------------------------------------------------------------+ *)

type sleep = {
  time : float;
  mutable canceled : bool;
  thread : unit Lwt.t;
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
  let res = Lwt.task () in
  let t = if d <= 0. then 0. else Unix.gettimeofday () +. d in
  let sleeper = { time = t; canceled = false; thread = res } in
  new_sleeps := sleeper :: !new_sleeps;
  Lwt.on_cancel res (fun _ -> sleeper.canceled <- true);
  res

let yield () = sleep 0.

exception Timeout

let timeout d = sleep d >> Lwt.fail Timeout

let with_timeout d f = Lwt.select [timeout d; Lwt.apply f ()]

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
  Unix.set_nonblock fd;
  { fd = fd; state = Open; blocking = false }

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

let readable fd = Unix.select [fd] [] [] 0.0 <> ([], [], [])
let writable fd = Unix.select [] [fd] [] 0.0 <> ([], [], [])

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
  let res = Lwt.task () in
  let actions = get_actions ch set in
  let node = Lwt_sequence.add_r (fun _ -> retry_syscall set ch res action) actions in
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

let real_read, real_write =
  if Sys.os_type = "Unix" then
    lwt_unix_read, lwt_unix_write
  else
    Unix.read, Unix.write

let read ch buf pos len =
  if pos < 0 || len < 0 || pos > String.length buf - len then
    invalid_arg "Lwt_unix.read"
  else
    wrap_syscall inputs ch (fun _ -> real_read ch.fd buf pos len)

let write ch buf pos len =
  if pos < 0 || len < 0 || pos > String.length buf - len then
    invalid_arg "Lwt_unix.write"
  else
    wrap_syscall outputs ch (fun _ -> real_write ch.fd buf pos len)

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
  wrap_syscall inputs ch begin fun _ ->
    let l = ref [] in
    begin
      try
        for i = 1 to n do
          if ch.blocking && not (readable ch.fd) then raise Exit;
          let fd, addr = Unix.accept ch.fd in
          l := (mk_ch fd, addr) :: !l
        done
      with
        | (Unix.Unix_error((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _) | Exit) when !l <> [] ->
            (* Ignore blocking errors if we have at least one file-descriptor: *)
            ()
    end;
    List.rev !l
  end

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
            in_progress := true
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

(* +-----------------------------------------------------------------+
   | Signals                                                         |
   +-----------------------------------------------------------------+ *)

(* Signal file descriptor. Used only if available. *)
module SignalFD =
struct
  external available : unit -> bool = "lwt_signalfd_available"
  external add : int -> unit = "lwt_signalfd_add"
  external del : int -> unit = "lwt_signalfd_del"
  external size : unit -> int = "lwt_signalfd_size"
  external init : unit -> Unix.file_descr = "lwt_signalfd_init"
  external read : string -> int = "lwt_signalfd_read"
end

module SignalMap = Map.Make(struct type t = int let compare = compare end)

(* Information about a signal being monitored: *)
type signal_info = {
  event : int React.event;
  (* The event which occurs when the signal is received. *)

  send : int -> unit;
  (* The function to send a new event *)

  mutable ref_count : int;
  (* Number of object created for this signal *)
}

(* The set of all monitored signals: *)
let signals : signal_info SignalMap.t ref = ref SignalMap.empty

(* Read [signalfd_siginfo] structure continously from the signal file
   descriptor and dispatch signals to event: *)
let rec loop_signalfd fd buf len =
  read fd buf 0 len >>= function
    | 0 ->
        return ()
    | _ ->
        let signum = SignalFD.read buf in
        begin try
          (SignalMap.find signum !signals).send signum
        with Not_found ->
          ()
        end;
        loop_signalfd fd buf len

(* List of received signals, for the classic mode: *)
let received_signals = Queue.create ()

type signal_mode = Signal_not_initialised | Signal_fd | Signal_classic

let signal_mode = ref Signal_not_initialised

let init_signals = lazy(
  if SignalFD.available () then begin
    let fd = SignalFD.init () and len = SignalFD.size () in
    ignore (loop_signalfd (of_unix_file_descr fd) (String.create len) len);
    signal_mode := Signal_fd
  end else
    signal_mode := Signal_classic
)

(* Handle the reception of a signal in classic mode *)
let handle_signal signum =
  Queue.add signum received_signals

(* Add a reference to the given signal and return a new handle: *)
let signal_ref signum info =
  info.ref_count <- info.ref_count + 1;
  let stop = lazy(
    info.ref_count <- info.ref_count - 1;
    if info.ref_count = 0 then begin
      signals := SignalMap.remove signum !signals;
      match !signal_mode with
        | Signal_fd ->
            SignalFD.del signum
        | Signal_classic ->
            Sys.set_signal signum Sys.Signal_default
        | Signal_not_initialised ->
            assert false
    end
  ) in
  (object
     method event = info.event
     method stop = Lazy.force stop
   end)

let signal signum =
  Lazy.force init_signals;
  match try Some(SignalMap.find signum !signals) with Not_found -> None with
    | Some info ->
        signal_ref signum info
    | None ->
        begin match !signal_mode with
          | Signal_fd ->
              SignalFD.add signum
          | Signal_classic ->
              Sys.set_signal signum (Sys.Signal_handle handle_signal)
          | Signal_not_initialised ->
              assert false
        end;
        let event, send = React.E.create () in
        let info = { send = send; event = event; ref_count = 0 } in
        signals := SignalMap.add signum info !signals;
        signal_ref signum info

(* Wakeup signals, for the classic mode: *)
let wakeup_signals () =
  try
    while true do
      let signum = Queue.take received_signals in
      begin try
        (SignalMap.find signum !signals).send signum
      with Not_found ->
        ()
      end
    done
  with Queue.Empty ->
    ()

(* +-----------------------------------------------------------------+
   | Processes                                                       |
   +-----------------------------------------------------------------+ *)

let wait_children = Lwt_sequence.create ()

let init_wait_pid = lazy(
  React.E.map begin fun _ ->
    Lwt_sequence.iter_node_l begin fun node ->
      let cont, flags, pid = Lwt_sequence.get node in
      try
        let (pid', _) as v = Unix.waitpid flags pid in
        if pid' <> 0 then begin
          Lwt_sequence.remove node;
          Lwt.wakeup cont v
        end
      with e ->
        Lwt_sequence.remove node;
        Lwt.wakeup_exn cont e
    end wait_children
  end (signal Sys.sigchld)#event
)

let _waitpid flags pid =
  try_lwt
    return (Unix.waitpid flags pid)

let waitpid flags pid =
  if List.mem Unix.WNOHANG flags then
    _waitpid flags pid
  else
    let flags = Unix.WNOHANG :: flags in
    lwt ((pid', _) as res) = _waitpid flags pid in
    if pid' <> 0 then
      return res
    else begin
      ignore (Lazy.force init_wait_pid);
      let res = Lwt.task () in
      let node = Lwt_sequence.add_l (res, flags, pid) wait_children in
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
  (* Restart threads waiting for a signal before doing the select,
     because there may be blocked signal that need to be handled
     now: *)
  if !signal_mode = Signal_classic then wakeup_signals ();
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
  (* Restart threads waiting for a signal: *)
  if !signal_mode = Signal_classic then wakeup_signals ();
  result

let _ = Lwt_sequence.add_l select_filter Lwt_main.select_filters

(* +-----------------------------------------------------------------+
   | Misc                                                            |
   +-----------------------------------------------------------------+ *)

(* Monitoring functions *)
let inputs_length () = blocked_thread_count inputs
let outputs_length () = blocked_thread_count outputs
let wait_children_length () = Lwt_sequence.fold_l (fun _ x -> succ x) wait_children 0
let get_new_sleeps () = List.length !new_sleeps
let sleep_queue_size () = SleepQueue.size !sleep_queue
