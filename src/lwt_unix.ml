(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_unix
 * Copyright (C) 2005-2008 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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

(*
Non-blocking I/O and select does not (fully) work under Windows.
The libray therefore does not use them under Windows, and will
therefore have the following limitations:
- No read will be performed while there are some threads ready to run
  or waiting to write;
- When a read is pending, everything else will be blocked: [sleep]
  will not terminate and other reads will not be performed before
  this read terminates;
- A write on a socket or a pipe can block the execution of the program
  if the data are never consumed at the other end of the connection.
  In particular, if both ends use this library and write at the same
  time, this could result in a dead-lock.
- [connect] is blocking
*)
let windows_hack = Sys.os_type <> "Unix"

module SleepQueue =
  Pqueue.Make (struct
    type t = float * unit Lwt.t
    let compare (t, _) (t', _) = compare t t'
  end)
let sleep_queue = ref SleepQueue.empty
let new_sleeps = ref []

let sleep d =
  let res = Lwt.wait () in
  let t = if d <= 0. then 0. else Unix.gettimeofday () +. d in
  new_sleeps := (t, res) :: !new_sleeps;
  res

let yield () = sleep 0.

let get_time t =
  if !t = -1. then t := Unix.gettimeofday ();
  !t

let in_the_past now t =
  t = 0. || t <= get_time now

let rec restart_threads now =
  match
    try Some (SleepQueue.find_min !sleep_queue) with Not_found -> None
  with
    Some (time, thr) when in_the_past now time ->
      sleep_queue := SleepQueue.remove_min !sleep_queue;
      Lwt.wakeup thr ();
      restart_threads now
  | _ ->
      ()

(****)

type state = Open | Closed | Aborted of exn

type file_descr = { fd : Unix.file_descr; mutable state: state }

let mk_ch fd =
  if not windows_hack then Unix.set_nonblock fd;
  { fd = fd; state = Open }

let check_descriptor ch =
  match ch.state with
    Open ->
      ()
  | Aborted e ->
      raise e
  | Closed ->
      raise (Unix.Unix_error (Unix.EBADF, "check_descriptor", ""))

(****)

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

let active_descriptors set = FdMap.fold (fun key _ l -> key :: l) !set []

let blocked_thread_count set =
  FdMap.fold (fun key (_, l) c -> List.length !l + c) !set 0

(****)

let wait_children = ref []

let child_exited = ref false
let _ =
  if not windows_hack then
    ignore (Sys.signal Sys.sigchld
              (Sys.Signal_handle (fun _ -> child_exited := true)))

let bad_fd fd =
  try ignore (Unix.LargeFile.fstat fd); false with
    Unix.Unix_error (_, _, _) ->
      true

let rec run thread =
  match Lwt.poll thread with
    Some v ->
      v
  | None ->
      sleep_queue :=
        List.fold_left
          (fun q e -> SleepQueue.add e q) !sleep_queue !new_sleeps;
      new_sleeps := [];
      let next_event =
        try
          let (time, _) = SleepQueue.find_min !sleep_queue in Some time
        with Not_found ->
          None
      in
      let now = ref (-1.) in
      let delay =
        match next_event with
          None      -> -1.
        | Some 0.   -> 0.
        | Some time -> max 0. (time -. get_time now)
      in
      let infds = active_descriptors inputs in
      let outfds = active_descriptors outputs in
      let (readers, writers, _) =
        if windows_hack then
          let writers = outfds in
          let readers =
            if delay = 0. || writers <> [] then [] else infds in
          (readers, writers, [])
        else if infds = [] && outfds = [] && delay = 0. then
          ([], [], [])
        else
          try
            let (readers, writers, _) as res =
              Unix.select infds outfds [] delay in
            if delay > 0. && !now <> -1. && readers = [] && writers = [] then
              now := !now +. delay;
            res
          with
            Unix.Unix_error (Unix.EINTR, _, _) ->
              ([], [], [])
          | Unix.Unix_error (Unix.EBADF, _, _) ->
              (List.filter bad_fd infds, List.filter bad_fd outfds, [])
      in
      restart_threads now;
      List.iter (fun fd -> perform_actions inputs fd) readers;
      List.iter (fun fd -> perform_actions outputs fd) writers;
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
      run thread

(****)

let set_state ch st =
  ch.state <- st;
  perform_actions inputs ch.fd;
  perform_actions outputs ch.fd

let abort ch e =
  if ch.state <> Closed then
    set_state ch (Aborted e)

let unix_file_descr ch = ch.fd

let of_unix_file_descr fd = mk_ch fd

let stdin = lazy(of_unix_file_descr Unix.stdin)
let stdout = lazy(of_unix_file_descr Unix.stdout)
let stderr = lazy(of_unix_file_descr Unix.stderr)

external lwt_unix_read : Unix.file_descr -> string -> int -> int -> int = "lwt_unix_read"
external lwt_unix_write : Unix.file_descr -> string -> int -> int -> int = "lwt_unix_write"

let read ch buf pos len =
  if pos < 0 || len < 0 || pos > String.length buf - len then
    invalid_arg "Lwt_unix.read"
  else
    try
      check_descriptor ch;
      if windows_hack then raise (Unix.Unix_error (Unix.EAGAIN, "", ""));
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
  if List.mem Unix.WNOHANG flags || windows_hack then
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

(****)

(* Monitoring functions *)
let inputs_length () = blocked_thread_count inputs
let outputs_length () = blocked_thread_count outputs
let wait_children_length () = List.length !wait_children
let get_new_sleeps () = List.length !new_sleeps
let sleep_queue_size () = SleepQueue.size !sleep_queue
