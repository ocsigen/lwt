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

(* +-----------------------------------------------------------------+
   | Sleepers                                                        |
   +-----------------------------------------------------------------+ *)

module SleepQueue =
  Pqueue.Make (struct
    type t = float * unit Lwt.t
    let compare (t, _) (t', _) = compare t t'
  end)

(* Threads waiting for a timeout to expire: *)
let sleep_queue = ref SleepQueue.empty

(* Sleepers added since the last iteration of the main loop:

   They are not added immediatly to the main sleep queue in order to
   prevent them from being wakeup immediatly by [restart_threads].
*)
let new_sleeps = ref []

let sleep d =
  let res = Lwt.wait () in
  let t = if d <= 0. then 0. else Unix.gettimeofday () +. d in
  new_sleeps := (t, res) :: !new_sleeps;
  res

let yield () = sleep 0.

let in_the_past now t =
  t = 0. || t <= Lazy.force now

(* We use a lazy-value for [now] to avoid one system call if not
   needed: *)
let rec restart_threads now =
  match SleepQueue.lookup_min !sleep_queue with
    | Some(time, thr) when in_the_past now time ->
        sleep_queue := SleepQueue.remove_min !sleep_queue;
        Lwt.wakeup thr ();
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
    Open ->
      ()
  | Aborted e ->
      raise e
  | Closed ->
      raise (Unix.Unix_error (Unix.EBADF, "check_descriptor", ""))

(* +-----------------------------------------------------------------+
   | Actions on file descriptors                                     |
   +-----------------------------------------------------------------+ *)

module FdMap =
  Map.Make (struct type t = Unix.file_descr let compare = compare end)

type watchers = (file_descr * (unit -> unit) list ref) FdMap.t ref

let inputs = ref FdMap.empty
let outputs = ref FdMap.empty

exception Retry
exception Retry_write
exception Retry_read

let find_actions set ch =
  try
    FdMap.find ch.fd !set
  with Not_found ->
    let res = (ch, ref []) in
    set := FdMap.add ch.fd res !set;
    res

type 'a outcome =
    Success of 'a
  | Exn of exn
  | Requeued

let rec wrap_syscall set ch cont action =
  let res =
    try
      check_descriptor ch;
      Success (action ())
    with
      Retry
    | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _)
    | Sys_blocked_io ->
        (* EINTR because we are catching SIG_CHLD hence the system call
           might be interrupted to handle the signal; this lets us restart
           the system call eventually. *)
        add_action set ch cont action;
        Requeued
    | Retry_read ->
        add_action inputs ch cont action;
        Requeued
    | Retry_write ->
        add_action outputs ch cont action;
        Requeued
    | e ->
        Exn e
  in
  match res with
    Success v ->
      Lwt.wakeup cont v
  | Exn e ->
      Lwt.wakeup_exn cont e
  | Requeued ->
      ()

and add_action set ch cont action =
  assert (ch.state = Open);
  let (_, actions) = find_actions set ch in
  actions := (fun () -> wrap_syscall set ch cont action) :: !actions

let register_action set ch action =
  let cont = Lwt.wait () in
  add_action set ch cont action;
  cont

let perform_actions set fd =
  try
    let (ch, actions) = FdMap.find fd !set in
    set := FdMap.remove fd !set;
    List.iter (fun f -> f ()) !actions
  with Not_found ->
    ()

let active_descriptors set acc = FdMap.fold (fun key _ acc -> key :: acc) !set acc

let blocked_thread_count set =
  FdMap.fold (fun key (_, l) c -> List.length !l + c) !set 0

(* +-----------------------------------------------------------------+
   | Event loop                                                      |
   +-----------------------------------------------------------------+ *)

let run = Lwt_main.run

let wait_children = ref []

let child_exited = ref false
let _ =
  Sys.signal Sys.sigchld
    (Sys.Signal_handle (fun _ -> child_exited := true))

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
      (Lwt_main.min_timeout
         timeout
         (match SleepQueue.lookup_min !sleep_queue with
            | None -> None
            | Some(0.0, _) -> Some 0.0
            | Some(time, _) -> Some(max 0.0 (time -. (Lazy.force now)))))
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

let stdin = of_unix_file_descr Unix.stdin
let stdout = of_unix_file_descr Unix.stdout
let stderr = of_unix_file_descr Unix.stderr

external lwt_unix_read : Unix.file_descr -> string -> int -> int -> int = "lwt_unix_read"
external lwt_unix_write : Unix.file_descr -> string -> int -> int -> int = "lwt_unix_write"

let read ch buf pos len =
  if pos < 0 || len < 0 || pos > String.length buf - len then
    invalid_arg "Lwt_unix.read"
  else
    try
      check_descriptor ch;
      Lwt.return (lwt_unix_read ch.fd buf pos len)
    with
        Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
          register_action inputs ch (fun () -> lwt_unix_read ch.fd buf pos len)
      | e ->
          Lwt.fail e

let write ch buf pos len =
  if pos < 0 || len < 0 || pos > String.length buf - len then
    invalid_arg "Lwt_unix.write"
  else
    try
      check_descriptor ch;
      Lwt.return (lwt_unix_write ch.fd buf pos len)
    with
        Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
          register_action outputs ch (fun () -> lwt_unix_write ch.fd buf pos len)
      | e ->
          Lwt.fail e

let wait_read ch = register_action inputs ch (fun () -> ())
let wait_write ch = register_action outputs ch (fun () -> ())

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
  try
    check_descriptor ch;
    register_action inputs ch
      (fun () ->
         let (s, addr) = Unix.accept ch.fd in
         (mk_ch s, addr))
  with e ->
    Lwt.fail e

let accept_n ch n =
  let rec iter_accept acc = function
    | 0 ->
        List.rev acc
    | n ->
        match
          try
            Some(Unix.accept ch.fd)
          with
            | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK | Unix.EINTR), _, _) ->
                None
        with
          | Some(s, addr) ->
              iter_accept ((mk_ch s, addr) :: acc) (n - 1)
          | None ->
              List.rev acc
  in
  try
    check_descriptor ch;
    register_action inputs ch
      (fun () -> iter_accept [] n)
  with e ->
    Lwt.fail e

let check_socket ch =
  register_action outputs ch
    (fun () ->
       try ignore (Unix.getpeername ch.fd) with
         Unix.Unix_error (Unix.ENOTCONN, _, _) ->
           (* Get the socket error *)
           ignore (lwt_unix_read ch.fd " " 0 1))

let connect ch addr =
  try
    check_descriptor ch;
    Unix.connect ch.fd addr;
    Lwt.return ()
  with
    Unix.Unix_error
      ((Unix.EINPROGRESS | Unix.EWOULDBLOCK | Unix.EAGAIN), _, _) ->
        check_socket ch
  | e ->
      Lwt.fail e

let _waitpid flags pid =
  try
    Lwt.return (Unix.waitpid flags pid)
  with e ->
    Lwt.fail e

let waitpid flags pid =
  if List.mem Unix.WNOHANG flags then
    _waitpid flags pid
  else
    let flags = Unix.WNOHANG :: flags in
    Lwt.bind (_waitpid flags pid) (fun ((pid', _) as res) ->
    if pid' <> 0 then
      Lwt.return res
    else
      let res = Lwt.wait () in
      wait_children := (res, flags, pid) :: !wait_children;
      res)

let wait () = waitpid [] (-1)

let system cmd =
  match Unix.fork () with
     0 -> begin try
            Unix.execv "/bin/sh" [| "/bin/sh"; "-c"; cmd |]
          with _ ->
            exit 127
          end
  | id -> Lwt.bind (waitpid [] id) (fun (pid, status) -> Lwt.return status)

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
