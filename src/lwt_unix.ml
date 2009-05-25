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
  Lwt.on_cancel res (fun _ -> sleeper.canceled <- false);
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

type file_descr = { fd : Unix.file_descr; mutable state: state }

let mk_ch fd =
  Unix.set_nonblock fd;
  { fd = fd; state = Open }

let check_descriptor ch =
  match ch.state with
    | Open ->
        ()
    | Aborted e ->
        raise e
    | Closed ->
        raise (Unix.Unix_error (Unix.EBADF, "check_descriptor", ""))

let state ch = ch.state

(* +-----------------------------------------------------------------+
   | Actions on file descriptors                                     |
   +-----------------------------------------------------------------+ *)

module FdMap =
  Map.Make (struct type t = Unix.file_descr let compare = compare end)

type watchers = (file_descr * (unit -> unit) Lwt_dlist.node) FdMap.t ref

let inputs = ref FdMap.empty
let outputs = ref FdMap.empty

exception Retry
exception Retry_write
exception Retry_read

type 'a outcome =
  | Success of 'a
  | Exn of exn
  | Requeued of watchers

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
        ignore (add_action set ch cont action)

(* Add an action on a file descriptor: *)
and add_action set ch cont action =
  try
    let _, actions = FdMap.find ch.fd !set in
    Lwt_dlist.prepend_data (fun _ -> retry_syscall set ch cont action) actions
  with Not_found ->
    let node = Lwt_dlist.make (fun _ -> retry_syscall set ch cont action) in
    set := FdMap.add ch.fd (ch, node) !set;
    node

let register_action set ch action =
  let res = Lwt.task () in
  let node = add_action set ch res action in
  (* Unregister the action on cancel: *)
  Lwt.on_cancel res begin fun _ ->
    if Lwt_dlist.is_alone node then
      (* If there is no other action queued on the file-descriptor,
         remove it: *)
      set := FdMap.remove ch.fd !set
    else
      (* Otherwise, just remove the action: *)
      Lwt_dlist.junk node
  end;
  res

(* Wraps a system call *)
let wrap_syscall set ch action =
  try
    check_descriptor ch;
    return (action ())
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
    Lwt_dlist.iter (fun f -> f ()) actions
  with Not_found ->
    ()

let active_descriptors set acc =
  FdMap.fold (fun key _ acc -> key :: acc) !set acc

let blocked_thread_count set =
  FdMap.fold (fun key (_, l) c -> Lwt_dlist.count l + c) !set 0

(* +-----------------------------------------------------------------+
   | Event loop                                                      |
   +-----------------------------------------------------------------+ *)

let run = Lwt_main.run

let wait_children = ref []

let child_exited = ref false
let _ =
  Sys.signal Sys.sigchld
    (Sys.Signal_handle (fun _ -> child_exited := true))

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
  (* Restart threads waiting for a child to exit: *)
  if !child_exited then begin
    child_exited := false;
    let l = !wait_children in
    wait_children := [];
    List.iter
      (fun ((cont, flags, pid) as e) ->
         try
           let (pid', _) as v = Unix.waitpid flags pid in
           if pid' = 0 then
             wait_children := e :: !wait_children
           else
             Lwt.wakeup cont v
         with e ->
           Lwt.wakeup_exn cont e)
      l
  end;
  result

let _ = Lwt_main.add_hook (ref select_filter) Lwt_main.select_filters

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

(* Try to create a file descriptor from a unix file descriptor, but
   does not fail if set_nonblock fail: *)
let of_fd_maybe fd =
  try
    of_unix_file_descr fd
  with _ ->
    { fd = fd; state = Closed }

let stdin = of_fd_maybe Unix.stdin
let stdout = of_fd_maybe Unix.stdout
let stderr = of_fd_maybe Unix.stderr

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

let wait_read ch = register_action inputs ch ignore
let wait_write ch = register_action outputs ch ignore

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
          let fd, addr = Unix.accept ch.fd in
          l := (mk_ch fd, addr) :: !l
        done
      with
        | Unix.Unix_error((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _) when !l <> [] ->
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
    else
      let res = Lwt.wait () in
      wait_children := (res, flags, pid) :: !wait_children;
      res

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
   | Misc                                                            |
   +-----------------------------------------------------------------+ *)

(* Monitoring functions *)
let inputs_length () = blocked_thread_count inputs
let outputs_length () = blocked_thread_count outputs
let wait_children_length () = List.length !wait_children
let get_new_sleeps () = List.length !new_sleeps
let sleep_queue_size () = SleepQueue.size !sleep_queue
