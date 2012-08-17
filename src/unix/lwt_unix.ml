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

#include "src/unix/lwt_config.ml"

open Lwt

(* +-----------------------------------------------------------------+
   | Configuration                                                   |
   +-----------------------------------------------------------------+ *)

type async_method =
  | Async_none
  | Async_detach
  | Async_switch

let default_async_method_var = ref Async_detach

let () =
  try
    match Sys.getenv "LWT_ASYNC_METHOD" with
      | "none" ->
          default_async_method_var := Async_none
      | "detach" ->
          default_async_method_var := Async_detach
      | "switch" ->
          default_async_method_var := Async_switch
      | str ->
          Printf.eprintf
            "%s: invalid lwt async method: '%s', must be 'none', 'detach' or 'switch'\n%!"
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

let with_async_none f =
  with_value async_method_key (Some Async_none) f

let with_async_detach f =
  with_value async_method_key (Some Async_detach) f

let with_async_switch f =
  with_value async_method_key (Some Async_switch) f

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

module Notifiers = Hashtbl.Make(struct
                                  type t = int
                                  let equal (x : int) (y : int) = x = y
                                  let hash (x : int) = x
                                end)

let notifiers = Notifiers.create 1024

let current_notification_id = ref 0

let rec find_free_id id =
  if Notifiers.mem notifiers id then
    find_free_id (id + 1)
  else
    id

let make_notification ?(once=false) f =
  let id = find_free_id (!current_notification_id + 1) in
  current_notification_id := id;
  Notifiers.add notifiers id { notify_once = once; notify_handler = f };
  id

let stop_notification id =
  Notifiers.remove notifiers id

let set_notification id f =
  let notifier = Notifiers.find notifiers id in
  Notifiers.replace notifiers id { notifier with notify_handler = f }

let call_notification id =
  match try Some(Notifiers.find notifiers id) with Not_found -> None with
    | Some notifier ->
        if notifier.notify_once then
          stop_notification id;
        notifier.notify_handler ()
    | None ->
        ()

(* +-----------------------------------------------------------------+
   | Sleepers                                                        |
   +-----------------------------------------------------------------+ *)

let sleep delay =
  let waiter, wakener = Lwt.task () in
  let ev = Lwt_engine.on_timer delay false (fun ev -> Lwt_engine.stop_event ev; Lwt.wakeup wakener ()) in
  Lwt.on_cancel waiter (fun () -> Lwt_engine.stop_event ev);
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
   | Jobs                                                            |
   +-----------------------------------------------------------------+ *)

type 'a job

external start_job : 'a job -> async_method -> bool = "lwt_unix_start_job"
    (* Starts the given job with given parameters. It returns [true]
       if the job is already terminated. *)

external check_job : 'a job -> int -> bool = "lwt_unix_check_job" "noalloc"
    (* Check whether that a job has terminated or not. If it has not
       yet terminated, it is marked so it will send a notification
       when it finishes. *)

(* For all running job, a waiter and a function to abort it. *)
let jobs = Lwt_sequence.create ()

let rec abort_jobs exn =
  match Lwt_sequence.take_opt_l jobs with
    | Some (w, f) -> f exn; abort_jobs exn
    | None -> ()

let cancel_jobs () = abort_jobs Lwt.Canceled

let wait_for_jobs () =
  join (Lwt_sequence.fold_l (fun (w, f) l -> w :: l) jobs [])

let wrap_result f x =
  try
    Lwt.make_value (f x)
  with exn ->
    Lwt.make_error exn

let run_job_aux async_method job result =
  (* Starts the job. *)
  if start_job job async_method then
    (* The job has already terminated, read and return the result
       immediatly. *)
    Lwt.of_result (result job)
  else begin
    (* Thread for the job. *)
    let waiter, wakener = wait () in
    (* Add the job to the sequence of all jobs. *)
    let node = Lwt_sequence.add_l (waiter >> return (), fun exn -> if state waiter = Sleep then wakeup_exn wakener exn) jobs in
    ignore begin
      (* Create the notification for asynchronous wakeup. *)
      let id =
        make_notification ~once:true
          (fun () ->
             Lwt_sequence.remove node;
             let result = result job in
             if state waiter = Sleep then Lwt.wakeup_result wakener result)
      in
      (* Give the job some time before we fallback to asynchronous
         notification. *)
      lwt () = pause () in
      (* The job has terminated, send the result immediatly. *)
      if check_job job id then call_notification id;
      return ()
    end;
    waiter
  end

let choose_async_method = function
  | Some async_method ->
      async_method
  | None ->
      match Lwt.get async_method_key with
        | Some am -> am
        | None -> !default_async_method_var

let execute_job ?async_method ~job ~result ~free =
  let async_method = choose_async_method async_method in
  run_job_aux async_method job (fun job -> let x = wrap_result result job in free job; x)

external self_result : 'a job -> 'a = "lwt_unix_self_result"
      (* Returns the result of a job using the [result] field of the C
         job structure. *)

external run_job_sync : 'a job -> 'a = "lwt_unix_run_job_sync"
      (* Exeuctes a job synchronously and returns its result. *)

let self_result job =
  try
    Lwt.make_value (self_result job)
  with exn ->
    Lwt.make_error exn

let run_job ?async_method job =
  let async_method = choose_async_method async_method in
  if async_method = Async_none then
    try
      return (run_job_sync job)
    with exn ->
      fail exn
  else
    run_job_aux async_method job self_result

(* +-----------------------------------------------------------------+
   | File descriptor wrappers                                        |
   +-----------------------------------------------------------------+ *)

type state = Opened | Closed | Aborted of exn

type file_descr = {
  fd : Unix.file_descr;
  (* The underlying unix file descriptor *)

  mutable state: state;
  (* The state of the file descriptor *)

  mutable set_flags : bool;
  (* Whether to set file flags *)

  mutable blocking : bool Lwt.t Lazy.t;
  (* Is the file descriptor in blocking or non-blocking mode *)

  mutable event_readable : Lwt_engine.event option;
  (* The event used to check the file descriptor for readability. *)

  mutable event_writable : Lwt_engine.event option;
  (* The event used to check the file descriptor for writability. *)

  hooks_readable : (unit -> unit) Lwt_sequence.t;
  (* Hooks to call when the file descriptor becomes readable. *)

  hooks_writable : (unit -> unit) Lwt_sequence.t;
  (* Hooks to call when the file descriptor becomes writable. *)
}

#if windows

external is_socket : Unix.file_descr -> bool = "lwt_unix_is_socket" "noalloc"

let is_blocking ?blocking ?(set_flags=true) fd =
  if is_socket fd then
    match blocking, set_flags with
      | Some state, false ->
          lazy(return state)
      | Some true, true ->
          Unix.clear_nonblock fd;
          lazy(return true)
      | Some false, true ->
          Unix.set_nonblock fd;
          lazy(return false)
      | None, false ->
          lazy(return false)
      | None, true ->
          Unix.set_nonblock fd;
          lazy(return false)
  else
    match blocking with
      | Some state ->
          lazy(return state)
      | None ->
          lazy(return true)

#else

external guess_blocking_job : Unix.file_descr -> bool job = "lwt_unix_guess_blocking_job"

let guess_blocking fd =
  run_job (guess_blocking_job fd)

let is_blocking ?blocking ?(set_flags=true) fd =
    match blocking, set_flags with
      | Some state, false ->
          lazy(return state)
      | Some true, true ->
          Unix.clear_nonblock fd;
          lazy(return true)
      | Some false, true ->
          Unix.set_nonblock fd;
          lazy(return false)
      | None, false ->
          lazy(guess_blocking fd)
      | None, true ->
          lazy(guess_blocking fd >>= function
                 | true ->
                     Unix.clear_nonblock fd;
                     return true
                 | false ->
                     Unix.set_nonblock fd;
                     return false)

#endif

let mk_ch ?blocking ?(set_flags=true) fd = {
  fd = fd;
  state = Opened;
  set_flags = set_flags;
  blocking = is_blocking ?blocking ~set_flags fd;
  event_readable = None;
  event_writable = None;
  hooks_readable = Lwt_sequence.create ();
  hooks_writable = Lwt_sequence.create ();
}

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
  Lazy.force ch.blocking

let set_blocking ?(set_flags=true) ch blocking =
  check_descriptor ch;
  ch.set_flags <- set_flags;
  ch.blocking <- is_blocking ~blocking ~set_flags ch.fd

#if windows

let unix_stub_readable fd = Unix.select [fd] [] [] 0.0 <> ([], [], [])
let unix_stub_writable fd = Unix.select [] [fd] [] 0.0 <> ([], [], [])

#else

external unix_stub_readable : Unix.file_descr -> bool = "lwt_unix_readable"
external unix_stub_writable : Unix.file_descr -> bool = "lwt_unix_writable"

#endif

let rec unix_readable fd =
  try
    unix_stub_readable fd
  with Unix.Unix_error (Unix.EINTR, _, _) ->
    unix_readable fd

let rec unix_writable fd =
  try
    unix_stub_writable fd
  with Unix.Unix_error (Unix.EINTR, _, _) ->
    unix_writable fd

let readable ch =
  check_descriptor ch;
  unix_readable ch.fd

let writable ch =
  check_descriptor ch;
  unix_writable ch.fd

let set_state ch st =
  ch.state <- st

let clear_events ch =
  Lwt_sequence.iter_node_l (fun node -> Lwt_sequence.remove node; Lwt_sequence.get node ()) ch.hooks_readable;
  Lwt_sequence.iter_node_l (fun node -> Lwt_sequence.remove node; Lwt_sequence.get node ()) ch.hooks_writable;
  begin
    match ch.event_readable with
      | Some ev ->
          ch.event_readable <- None;
          Lwt_engine.stop_event ev
      | None ->
          ()
  end;
  begin
    match ch.event_writable with
      | Some ev ->
          ch.event_writable <- None;
          Lwt_engine.stop_event ev
      | None ->
          ()
  end

let abort ch e =
  if ch.state <> Closed then begin
    set_state ch (Aborted e);
    clear_events ch
  end

let unix_file_descr ch = ch.fd

let of_unix_file_descr = mk_ch

let stdin = of_unix_file_descr ~set_flags:false ~blocking:true Unix.stdin
let stdout = of_unix_file_descr ~set_flags:false ~blocking:true Unix.stdout
let stderr = of_unix_file_descr ~set_flags:false ~blocking:true Unix.stderr

(* +-----------------------------------------------------------------+
   | Actions on file descriptors                                     |
   +-----------------------------------------------------------------+ *)

type io_event = Read | Write

exception Retry
exception Retry_write
exception Retry_read

type 'a outcome =
  | Success of 'a
  | Exn of exn
  | Requeued of io_event

(* Wait a bit, then stop events that are no more used. *)
let stop_events ch =
  on_success
    (pause ())
    (fun () ->
       if Lwt_sequence.is_empty ch.hooks_readable then begin
         match ch.event_readable with
           | Some ev ->
               ch.event_readable <- None;
               Lwt_engine.stop_event ev
           | None ->
               ()
       end;
       if Lwt_sequence.is_empty ch.hooks_writable then begin
         match ch.event_writable with
           | Some ev ->
               ch.event_writable <- None;
               Lwt_engine.stop_event ev
           | None ->
               ()
       end)

let register_readable ch =
  if ch.event_readable = None then
    ch.event_readable <- Some(Lwt_engine.on_readable ch.fd (fun _ -> Lwt_sequence.iter_l (fun f -> f ()) ch.hooks_readable))

let register_writable ch =
  if ch.event_writable = None then
    ch.event_writable <- Some(Lwt_engine.on_writable ch.fd (fun _ -> Lwt_sequence.iter_l (fun f -> f ()) ch.hooks_writable))

(* Retry a queued syscall, [wakener] is the thread to wakeup if the
   action succeeds: *)
let rec retry_syscall node event ch wakener action =
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
        Lwt_sequence.remove !node;
        stop_events ch;
        Lwt.wakeup wakener v
    | Exn e ->
        Lwt_sequence.remove !node;
        stop_events ch;
        Lwt.wakeup_exn wakener e
    | Requeued event' ->
        if event <> event' then begin
          Lwt_sequence.remove !node;
          stop_events ch;
          match event' with
            | Read ->
                node := Lwt_sequence.add_r (fun () -> retry_syscall node Read ch wakener action) ch.hooks_readable ;
                register_readable ch
            | Write ->
                node := Lwt_sequence.add_r (fun () -> retry_syscall node Write ch wakener action) ch.hooks_writable;
                register_writable ch
        end

let dummy = Lwt_sequence.add_r ignore (Lwt_sequence.create ())

let register_action event ch action =
  let waiter, wakener = Lwt.task () in
  match event with
    | Read ->
        let node = ref dummy in
        node := Lwt_sequence.add_r (fun () -> retry_syscall node Read ch wakener action) ch.hooks_readable;
        on_cancel waiter (fun () -> Lwt_sequence.remove !node; stop_events ch);
        register_readable ch;
        waiter
    | Write ->
        let node = ref dummy in
        node := Lwt_sequence.add_r (fun () -> retry_syscall node Write ch wakener action) ch.hooks_writable;
        on_cancel waiter (fun () -> Lwt_sequence.remove !node; stop_events ch);
        register_writable ch;
        waiter

(* Wraps a system call *)
let wrap_syscall event ch action =
  check_descriptor ch;
  lwt blocking = Lazy.force ch.blocking in
  try
    if not blocking || (event = Read && unix_readable ch.fd) || (event = Write && unix_writable ch.fd) then
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
   | Generated jobs                                                  |
   +-----------------------------------------------------------------+ *)

module Jobs = Lwt_unix_jobs_generated.Make(struct type 'a t = 'a job end)

(* +-----------------------------------------------------------------+
   | Basic file input/output                                         |
   +-----------------------------------------------------------------+ *)

type open_flag =
    Unix.open_flag =
  | O_RDONLY
  | O_WRONLY
  | O_RDWR
  | O_NONBLOCK
  | O_APPEND
  | O_CREAT
  | O_TRUNC
  | O_EXCL
  | O_NOCTTY
  | O_DSYNC
  | O_SYNC
  | O_RSYNC
#if ocaml_version >= (3, 13)
  | O_SHARE_DELETE
#endif

#if windows

let openfile name flags perms =
  return (of_unix_file_descr (Unix.openfile name flags perms))

#else

external open_job : string -> Unix.open_flag list -> int -> (Unix.file_descr * bool) job = "lwt_unix_open_job"

let openfile name flags perms =
  lwt fd, blocking = run_job (open_job name flags perms) in
  return (of_unix_file_descr ~blocking fd)

#endif

#if windows

let close ch =
  if ch.state = Closed then check_descriptor ch;
  set_state ch Closed;
  clear_events ch;
  return (Unix.close ch.fd)

#else

let close ch =
  if ch.state = Closed then check_descriptor ch;
  set_state ch Closed;
  clear_events ch;
  run_job (Jobs.close_job ch.fd)

#endif

let wait_read ch =
  try_lwt
    if readable ch then
      return ()
    else
      register_action Read ch ignore

external stub_read : Unix.file_descr -> string -> int -> int -> int = "lwt_unix_read"
external read_job : Unix.file_descr -> string -> int -> int -> int job = "lwt_unix_read_job"

let read ch buf pos len =
  if pos < 0 || len < 0 || pos > String.length buf - len then
    invalid_arg "Lwt_unix.read"
  else
    Lazy.force ch.blocking >>= function
      | true ->
          lwt () = wait_read ch in
          run_job (read_job ch.fd buf pos len)
      | false ->
          wrap_syscall Read ch (fun () -> stub_read ch.fd buf pos len)

let wait_write ch =
  try_lwt
    if writable ch then
      return ()
    else
      register_action Write ch ignore

external stub_write : Unix.file_descr -> string -> int -> int -> int = "lwt_unix_write"
external write_job : Unix.file_descr -> string -> int -> int -> int job = "lwt_unix_write_job"

let write ch buf pos len =
  if pos < 0 || len < 0 || pos > String.length buf - len then
    invalid_arg "Lwt_unix.write"
  else
    Lazy.force ch.blocking >>= function
      | true ->
          lwt () = wait_write ch in
          run_job (write_job ch.fd buf pos len)
      | false ->
          wrap_syscall Write ch (fun () -> stub_write ch.fd buf pos len)

(* +-----------------------------------------------------------------+
   | Seeking and truncating                                          |
   +-----------------------------------------------------------------+ *)

type seek_command =
    Unix.seek_command =
  | SEEK_SET
  | SEEK_CUR
  | SEEK_END

#if windows

let lseek ch offset whence =
  check_descriptor ch;
  return (Unix.lseek ch.fd offset whence)

#else

let lseek ch offset whence =
  check_descriptor ch;
  run_job (Jobs.lseek_job ch.fd offset whence)

#endif

#if windows

let truncate name offset =
  return (Unix.truncate name offset)

#else

let truncate name offset =
  run_job (Jobs.truncate_job name offset)

#endif

#if windows

let ftruncate ch offset =
  check_descriptor ch;
  return (Unix.ftruncate ch.fd offset)

#else

let ftruncate ch offset =
  check_descriptor ch;
  run_job (Jobs.ftruncate_job ch.fd offset)

#endif

(* +-----------------------------------------------------------------+
   | File system synchronisation                                     |
   +-----------------------------------------------------------------+ *)

let fdatasync ch =
  check_descriptor ch;
  run_job (Jobs.fdatasync_job ch.fd)

let fsync ch =
  check_descriptor ch;
  run_job (Jobs.fsync_job ch.fd)

(* +-----------------------------------------------------------------+
   | File status                                                     |
   +-----------------------------------------------------------------+ *)

type file_perm = Unix.file_perm

type file_kind =
    Unix.file_kind =
  | S_REG
  | S_DIR
  | S_CHR
  | S_BLK
  | S_LNK
  | S_FIFO
  | S_SOCK

type stats =
    Unix.stats =
    {
      st_dev : int;
      st_ino : int;
      st_kind : file_kind;
      st_perm : file_perm;
      st_nlink : int;
      st_uid : int;
      st_gid : int;
      st_rdev : int;
      st_size : int;
      st_atime : float;
      st_mtime : float;
      st_ctime : float;
    }

#if windows

let stat name =
  return (Unix.stat name)

#else

external stat_job : string -> Unix.stats job = "lwt_unix_stat_job"

let stat name =
  run_job (stat_job name)

#endif

#if windows

let lstat name =
  return (Unix.lstat name)

#else

external lstat_job : string -> Unix.stats job = "lwt_unix_lstat_job"

let lstat name =
  run_job (lstat_job name)

#endif

#if windows

let fstat ch =
  check_descriptor ch;
  return (Unix.fstat ch.fd)

#else

external fstat_job : Unix.file_descr -> Unix.stats job = "lwt_unix_fstat_job"

let fstat ch =
  check_descriptor ch;
  run_job (fstat_job ch.fd)

#endif

#if windows

let isatty ch =
  check_descriptor ch;
  return (Unix.isatty ch.fd)

#else

external isatty_job : Unix.file_descr -> bool job = "lwt_unix_isatty_job"

let isatty ch =
  check_descriptor ch;
  run_job (isatty_job ch.fd)

#endif

(* +-----------------------------------------------------------------+
   | File operations on large files                                  |
   +-----------------------------------------------------------------+ *)

module LargeFile =
struct

  type stats =
      Unix.LargeFile.stats =
      {
        st_dev : int;
        st_ino : int;
        st_kind : file_kind;
        st_perm : file_perm;
        st_nlink : int;
        st_uid : int;
        st_gid : int;
        st_rdev : int;
        st_size : int64;
        st_atime : float;
        st_mtime : float;
        st_ctime : float;
      }

#if windows

  let lseek ch offset whence =
    check_descriptor ch;
    return (Unix.LargeFile.lseek ch.fd offset whence)

#else

  let lseek ch offset whence =
    check_descriptor ch;
    run_job (Jobs.lseek_64_job ch.fd offset whence)

#endif

#if windows

  let truncate name offset =
    return (Unix.LargeFile.truncate name offset)

#else

  let truncate name offset =
    run_job (Jobs.truncate_64_job name offset)

#endif

#if windows

  let ftruncate ch offset =
    check_descriptor ch;
    return (Unix.LargeFile.ftruncate ch.fd offset)

#else

  let ftruncate ch offset =
    check_descriptor ch;
    run_job (Jobs.ftruncate_64_job ch.fd offset)

#endif

#if windows

  let stat name =
    return (Unix.LargeFile.stat name)

#else

  external stat_job : string -> Unix.LargeFile.stats job = "lwt_unix_stat_64_job"

  let stat name =
    run_job (stat_job name)

#endif

#if windows

  let lstat name =
    return (Unix.LargeFile.lstat name)

#else

  external lstat_job : string -> Unix.LargeFile.stats job = "lwt_unix_lstat_64_job"

  let lstat name =
    run_job (lstat_job name)

#endif

#if windows

  let fstat ch =
    check_descriptor ch;
    return (Unix.LargeFile.fstat ch.fd)

#else

  external fstat_job : Unix.file_descr -> Unix.LargeFile.stats job = "lwt_unix_fstat_64_job"

  let fstat ch =
    check_descriptor ch;
    run_job (fstat_job ch.fd)

#endif

end

(* +-----------------------------------------------------------------+
   | Operations on file names                                        |
   +-----------------------------------------------------------------+ *)

#if windows

let unlink name =
  return (Unix.unlink name)

#else

let unlink name =
  run_job (Jobs.unlink_job name)

#endif

#if windows

let rename name1 name2 =
  return (Unix.rename name1 name2)

#else

let rename name1 name2 =
  run_job (Jobs.rename_job name1 name2)

#endif

#if windows

let link name1 name2 =
  return (Unix.link name1 name2)

#else

let link oldpath newpath =
  run_job (Jobs.link_job oldpath newpath)

#endif

(* +-----------------------------------------------------------------+
   | File permissions and ownership                                  |
   +-----------------------------------------------------------------+ *)

#if windows

let chmod name perms =
  return (Unix.chmod name perms)

#else

let chmod path mode =
  run_job (Jobs.chmod_job path mode)

#endif

#if windows

let fchmod ch perms =
  check_descriptor ch;
  return (Unix.fchmod ch.fd perms)

#else

let fchmod ch mode =
  check_descriptor ch;
  run_job (Jobs.fchmod_job ch.fd mode)

#endif

#if windows

let chown name uid gid =
  return (Unix.chown name uid gid)

#else

let chown path ower group =
  run_job (Jobs.chown_job path ower group)

#endif

#if windows

let fchown ch uid gid =
  check_descriptor ch;
  return (Unix.fchown ch.fd uid gid)

#else

let fchown ch ower group =
  check_descriptor ch;
  run_job (Jobs.fchown_job ch.fd ower group)

#endif

type access_permission =
    Unix.access_permission =
  | R_OK
  | W_OK
  | X_OK
  | F_OK

#if windows

let access name perms =
  return (Unix.access name perms)

#else

let access path mode =
  run_job (Jobs.access_job path mode)

#endif

(* +-----------------------------------------------------------------+
   | Operations on file descriptors                                  |
   +-----------------------------------------------------------------+ *)

let dup ch =
  check_descriptor ch;
  let fd = Unix.dup ch.fd in
  {
    fd = fd;
    state = Opened;
    set_flags = ch.set_flags;
    blocking =
      if ch.set_flags then
        lazy(Lazy.force ch.blocking >>= function
               | true ->
                   Unix.clear_nonblock fd;
                   return true
               | false ->
                   Unix.set_nonblock fd;
                   return false)
      else
        ch.blocking;
    event_readable = None;
    event_writable = None;
    hooks_readable = Lwt_sequence.create ();
    hooks_writable = Lwt_sequence.create ();
  }

let dup2 ch1 ch2 =
  check_descriptor ch1;
  Unix.dup2 ch1.fd ch2.fd;
  ch2.set_flags <- ch1.set_flags;
  ch2.blocking <- (
    if ch2.set_flags then
      lazy(Lazy.force ch1.blocking >>= function
             | true ->
                 Unix.clear_nonblock ch2.fd;
                 return true
             | false ->
                 Unix.set_nonblock ch2.fd;
                 return false)
    else
      ch1.blocking
  )

let set_close_on_exec ch =
  check_descriptor ch;
  Unix.set_close_on_exec ch.fd

let clear_close_on_exec ch =
  check_descriptor ch;
  Unix.clear_close_on_exec ch.fd

(* +-----------------------------------------------------------------+
   | Directories                                                     |
   +-----------------------------------------------------------------+ *)

#if windows

let mkdir name perms =
  return (Unix.mkdir name perms)

#else

let mkdir name perms =
  run_job (Jobs.mkdir_job name perms)

#endif

#if windows

let rmdir name =
  return (Unix.rmdir name)

#else

let rmdir name =
  run_job (Jobs.rmdir_job name)

#endif

#if windows

let chdir name =
  return (Unix.chdir name)

#else

let chdir path =
  run_job (Jobs.chdir_job path)

#endif

#if windows

let chroot name =
  return (Unix.chroot name)

#else

let chroot path =
  run_job (Jobs.chroot_job path)

#endif

type dir_handle = Unix.dir_handle

#if windows

let opendir name =
  return (Unix.opendir name)

#else

external opendir_job : string -> Unix.dir_handle job = "lwt_unix_opendir_job"

let opendir name =
  run_job (opendir_job name)

#endif

#if windows

let readdir handle =
  return (Unix.readdir handle)

#else

external readdir_job : Unix.dir_handle -> string job = "lwt_unix_readdir_job"

let readdir handle =
  run_job (readdir_job handle)

#endif

#if windows

let readdir_n handle count =
  if count < 0 then
    fail (Invalid_argument "Lwt_uinx.readdir_n")
  else
    let array = Array.make count "" in
    let rec fill i =
      if i = count then
        return array
      else
        match try array.(i) <- Unix.readdir handle; true with End_of_file -> false with
          | true ->
              fill (i + 1)
          | false ->
              return (Array.sub array 0 i)
    in
    fill 0

#else

external readdir_n_job : Unix.dir_handle -> int -> string array job = "lwt_unix_readdir_n_job"

let readdir_n handle count =
  if count < 0 then
    fail (Invalid_argument "Lwt_uinx.readdir_n")
  else
    run_job (readdir_n_job handle count)

#endif

#if windows

let rewinddir handle =
  return (Unix.rewinddir handle)

#else

external rewinddir_job : Unix.dir_handle -> unit job = "lwt_unix_rewinddir_job"

let rewinddir handle =
  run_job (rewinddir_job handle)

#endif

#if windows

let closedir handle =
  return (Unix.closedir handle)

#else

external closedir_job : Unix.dir_handle -> unit job = "lwt_unix_closedir_job"

let closedir handle =
  run_job (closedir_job handle)

#endif

type list_directory_state  =
  | LDS_not_started
  | LDS_listing of Unix.dir_handle
  | LDS_done

let cleanup_dir_handle state =
  match !state with
    | LDS_listing handle ->
        ignore (closedir handle)
    | LDS_not_started | LDS_done ->
        ()

let files_of_directory path =
  let state = ref LDS_not_started in
  Lwt_stream.concat
    (Lwt_stream.from
       (fun () ->
          match !state with
            | LDS_not_started ->
                lwt handle = opendir path in
                lwt entries =
                  try_lwt
                    readdir_n handle 1024
                  with exn ->
                    lwt () = closedir handle in
                    raise exn
                in
                if Array.length entries < 1024 then begin
                  state := LDS_done;
                  lwt () = closedir handle in
                  return (Some(Lwt_stream.of_array entries))
                end else begin
                  state := LDS_listing handle;
                  Gc.finalise cleanup_dir_handle state;
                  return (Some(Lwt_stream.of_array entries))
                end
            | LDS_listing handle ->
                lwt entries =
                  try_lwt
                    readdir_n handle 1024
                  with exn ->
                    lwt () = closedir handle in
                    raise exn
                in
                if Array.length entries < 1024 then begin
                  state := LDS_done;
                  lwt () = closedir handle in
                  return (Some(Lwt_stream.of_array entries))
                end else
                  return (Some(Lwt_stream.of_array entries))
            | LDS_done ->
                return None))

(* +-----------------------------------------------------------------+
   | Pipes and redirections                                          |
   +-----------------------------------------------------------------+ *)

let pipe () =
  let (out_fd, in_fd) = Unix.pipe() in
  (mk_ch ~blocking:Lwt_sys.windows out_fd, mk_ch ~blocking:Lwt_sys.windows in_fd)

let pipe_in () =
  let (out_fd, in_fd) = Unix.pipe() in
  (mk_ch ~blocking:Lwt_sys.windows out_fd, in_fd)

let pipe_out () =
  let (out_fd, in_fd) = Unix.pipe() in
  (out_fd, mk_ch ~blocking:Lwt_sys.windows in_fd)

#if windows

let mkfifo name perms =
  return (Unix.mkfifo name perms)

#else

let mkfifo name perms =
  run_job (Jobs.mkfifo_job name perms)

#endif

(* +-----------------------------------------------------------------+
   | Symbolic links                                                  |
   +-----------------------------------------------------------------+ *)

#if windows

let symlink name1 name2 =
  return (Unix.symlink name1 name2)

#else

let symlink name1 name2 =
  run_job (Jobs.symlink_job name1 name2)

#endif

#if windows

let readlink name =
  return (Unix.readlink name)

#else

external readlink_job : string -> string job = "lwt_unix_readlink_job"

let readlink name =
  run_job (readlink_job name)

#endif

(* +-----------------------------------------------------------------+
   | Locking                                                         |
   +-----------------------------------------------------------------+ *)

type lock_command =
    Unix.lock_command =
  | F_ULOCK
  | F_LOCK
  | F_TLOCK
  | F_TEST
  | F_RLOCK
  | F_TRLOCK

#if windows

let lockf ch cmd size =
  check_descriptor ch;
  return (Unix.lockf ch.fd cmd size)

#else

external lockf_job : Unix.file_descr -> Unix.lock_command -> int -> unit job = "lwt_unix_lockf_job"

let lockf ch cmd size =
  check_descriptor ch;
  run_job (lockf_job ch.fd cmd size)

#endif

(* +-----------------------------------------------------------------+
   | User id, group id                                               |
   +-----------------------------------------------------------------+ *)

type passwd_entry =
    Unix.passwd_entry =
  {
    pw_name : string;
    pw_passwd : string;
    pw_uid : int;
    pw_gid : int;
    pw_gecos : string;
    pw_dir : string;
    pw_shell : string
  }

type group_entry =
    Unix.group_entry =
  {
    gr_name : string;
    gr_passwd : string;
    gr_gid : int;
    gr_mem : string array
  }

#if windows

let getlogin () =
  return (Unix.getlogin ())

#else

external getlogin_job : unit -> string job = "lwt_unix_getlogin_job"

let getlogin () =
  run_job (getlogin_job ())

#endif

#if windows

let getpwnam name =
  return (Unix.getpwnam name)

#else

external getpwnam_job : string -> Unix.passwd_entry job = "lwt_unix_getpwnam_job"

let getpwnam name =
  run_job (getpwnam_job name)

#endif

#if windows

let getgrnam name =
  return (Unix.getgrnam name)

#else

external getgrnam_job : string -> Unix.group_entry job = "lwt_unix_getgrnam_job"

let getgrnam name =
  run_job (getgrnam_job name)

#endif

#if windows

let getpwuid uid =
  return (Unix.getpwuid uid)

#else

external getpwuid_job : int -> Unix.passwd_entry job = "lwt_unix_getpwuid_job"

let getpwuid uid =
  run_job (getpwuid_job uid)

#endif

#if windows

let getgrgid gid =
  return (Unix.getgrgid gid)

#else

external getgrgid_job : int -> Unix.group_entry job = "lwt_unix_getgrgid_job"

let getgrgid gid =
  run_job (getgrgid_job gid)

#endif

(* +-----------------------------------------------------------------+
   | Sockets                                                         |
   +-----------------------------------------------------------------+ *)

type msg_flag =
    Unix.msg_flag =
  | MSG_OOB
  | MSG_DONTROUTE
  | MSG_PEEK

#if windows
let stub_recv = Unix.recv
#else
external stub_recv : Unix.file_descr -> string -> int -> int -> Unix.msg_flag list -> int = "lwt_unix_recv"
#endif

let recv ch buf pos len flags =
  if pos < 0 || len < 0 || pos > String.length buf - len then
    invalid_arg "Lwt_unix.recv"
  else
    wrap_syscall Read ch (fun () -> stub_recv ch.fd buf pos len flags)

#if windows
let stub_send = Unix.send
#else
external stub_send : Unix.file_descr -> string -> int -> int -> Unix.msg_flag list -> int = "lwt_unix_send"
#endif

let send ch buf pos len flags =
  if pos < 0 || len < 0 || pos > String.length buf - len then
    invalid_arg "Lwt_unix.send"
  else
    wrap_syscall Write ch (fun () -> stub_send ch.fd buf pos len flags)

#if windows
let stub_recvfrom = Unix.recvfrom
#else
external stub_recvfrom : Unix.file_descr -> string -> int -> int -> Unix.msg_flag list -> int * Unix.sockaddr = "lwt_unix_recvfrom"
#endif

let recvfrom ch buf pos len flags =
  if pos < 0 || len < 0 || pos > String.length buf - len then
    invalid_arg "Lwt_unix.recvfrom"
  else
    wrap_syscall Read ch (fun () -> stub_recvfrom ch.fd buf pos len flags)

#if windows
let stub_sendto = Unix.sendto
#else
external stub_sendto : Unix.file_descr -> string -> int -> int -> Unix.msg_flag list -> Unix.sockaddr -> int = "lwt_unix_sendto_byte" "lwt_unix_sendto"
#endif

let sendto ch buf pos len flags addr =
  if pos < 0 || len < 0 || pos > String.length buf - len then
    invalid_arg "Lwt_unix.sendto"
  else
    wrap_syscall Write ch (fun () -> stub_sendto ch.fd buf pos len flags addr)

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

let check_io_vectors func_name iovs =
  List.iter (fun iov ->
               if iov.iov_offset < 0
                 || iov.iov_length < 0
                 || iov.iov_offset > String.length iov.iov_buffer - iov.iov_length then
                   invalid_arg func_name) iovs

#if windows

let recv_msg ~socket ~io_vectors =
  raise (Lwt_sys.Not_available "recv_msg")

#else

external stub_recv_msg : Unix.file_descr -> int -> io_vector list -> int * Unix.file_descr list = "lwt_unix_recv_msg"

let recv_msg ~socket ~io_vectors =
  check_io_vectors "Lwt_unix.recv_msg" io_vectors;
  let n_iovs = List.length io_vectors in
  wrap_syscall Read socket
    (fun () ->
       stub_recv_msg socket.fd n_iovs io_vectors)

#endif

#if windows

let send_msg ~socket ~io_vectors ~fds =
  raise (Lwt_sys.Not_available "send_msg")

#else

external stub_send_msg : Unix.file_descr -> int -> io_vector list -> int -> Unix.file_descr list -> int = "lwt_unix_send_msg"

let send_msg ~socket ~io_vectors ~fds =
  check_io_vectors "Lwt_unix.send_msg" io_vectors;
  let n_iovs = List.length io_vectors and n_fds = List.length fds in
  wrap_syscall Write socket
    (fun () ->
       stub_send_msg socket.fd n_iovs io_vectors n_fds fds)

#endif

type inet_addr = Unix.inet_addr

type socket_domain =
    Unix.socket_domain =
  | PF_UNIX
  | PF_INET
  | PF_INET6

type socket_type =
    Unix.socket_type =
  | SOCK_STREAM
  | SOCK_DGRAM
  | SOCK_RAW
  | SOCK_SEQPACKET

type sockaddr = Unix.sockaddr = ADDR_UNIX of string | ADDR_INET of inet_addr * int

let socket dom typ proto =
  let s = Unix.socket dom typ proto in
  mk_ch ~blocking:false s

type shutdown_command =
    Unix.shutdown_command =
  | SHUTDOWN_RECEIVE
  | SHUTDOWN_SEND
  | SHUTDOWN_ALL

let shutdown ch shutdown_command =
  check_descriptor ch;
  Unix.shutdown ch.fd shutdown_command

#if windows

external socketpair_stub : socket_domain -> socket_type -> int -> Unix.file_descr * Unix.file_descr = "lwt_unix_socketpair_stub"

#else

let socketpair_stub = Unix.socketpair

#endif

let socketpair dom typ proto =
  let (s1, s2) = socketpair_stub dom typ proto in
  (mk_ch ~blocking:false s1, mk_ch ~blocking:false s2)

let accept ch =
  wrap_syscall Read ch (fun _ -> let (fd, addr) = Unix.accept ch.fd in (mk_ch ~blocking:false fd, addr))

let accept_n ch n =
  let l = ref [] in
  lwt blocking = Lazy.force ch.blocking in
  try_lwt
    wrap_syscall Read ch begin fun () ->
      begin
        try
          for i = 1 to n do
            if blocking && not (unix_readable ch.fd) then raise Retry;
            let fd, addr = Unix.accept ch.fd in
            l := (mk_ch ~blocking:false fd, addr) :: !l
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

#if windows

let connect ch addr =
  (* [in_progress] tell wether connection has started but not
     terminated: *)
  let in_progress = ref false in
  wrap_syscall Write ch begin fun () ->
    if !in_progress then
      (* Nothing works without this test and i have no idea why... *)
      if writable ch then
        try
          Unix.connect ch.fd addr
        with
          | Unix.Unix_error (Unix.EISCONN, _, _) ->
              (* This is the windows way of telling that the connection
                 has completed. *)
              ()
      else
        raise Retry
    else
      try
        Unix.connect ch.fd addr
      with
        | Unix.Unix_error (Unix.EWOULDBLOCK, _, _) ->
            in_progress := true;
            raise Retry
  end

#else

let connect ch addr =
  (* [in_progress] tell wether connection has started but not
     terminated: *)
  let in_progress = ref false in
  wrap_syscall Write ch begin fun () ->
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
        | Unix.Unix_error (Unix.EINPROGRESS, _, _) ->
            in_progress := true;
            raise Retry
  end

#endif

let setsockopt ch opt v =
  check_descriptor ch;
  Unix.setsockopt ch.fd opt v

let bind ch addr =
  check_descriptor ch;
  Unix.bind ch.fd addr

let listen ch cnt =
  check_descriptor ch;
  Unix.listen ch.fd cnt

let getpeername ch =
  check_descriptor ch;
  Unix.getpeername ch.fd

let getsockname ch =
  check_descriptor ch;
  Unix.getsockname ch.fd

type credentials = {
  cred_pid : int;
  cred_uid : int;
  cred_gid : int;
}

#if HAVE_GET_CREDENTIALS

external stub_get_credentials : Unix.file_descr -> credentials = "lwt_unix_get_credentials"

let get_credentials ch =
  check_descriptor ch;
  stub_get_credentials ch.fd

#else

let get_credentials ch =
  raise (Lwt_sys.Not_available "get_credentials")

#endif

(* +-----------------------------------------------------------------+
   | Socket options                                                  |
   +-----------------------------------------------------------------+ *)

type socket_bool_option =
    Unix.socket_bool_option =
  | SO_DEBUG
  | SO_BROADCAST
  | SO_REUSEADDR
  | SO_KEEPALIVE
  | SO_DONTROUTE
  | SO_OOBINLINE
  | SO_ACCEPTCONN
  | TCP_NODELAY
  | IPV6_ONLY

type socket_int_option =
    Unix.socket_int_option =
  | SO_SNDBUF
  | SO_RCVBUF
  | SO_ERROR
  | SO_TYPE
  | SO_RCVLOWAT
  | SO_SNDLOWAT

type socket_optint_option = Unix.socket_optint_option = SO_LINGER

type socket_float_option =
    Unix.socket_float_option =
  | SO_RCVTIMEO
  | SO_SNDTIMEO

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

(* +-----------------------------------------------------------------+
   | Host and protocol databases                                     |
   +-----------------------------------------------------------------+ *)

type host_entry =
    Unix.host_entry =
    {
      h_name : string;
      h_aliases : string array;
      h_addrtype : socket_domain;
      h_addr_list : inet_addr array
    }

type protocol_entry =
    Unix.protocol_entry =
    {
      p_name : string;
      p_aliases : string array;
      p_proto : int
    }

type service_entry =
    Unix.service_entry =
    {
      s_name : string;
      s_aliases : string array;
      s_port : int;
      s_proto : string
    }

#if windows

let gethostname () =
  return (Unix.gethostname ())

#else

external gethostname_job : unit -> string job = "lwt_unix_gethostname_job"

let gethostname () =
  run_job (gethostname_job ())

#endif

#if windows

let gethostbyname name =
  return (Unix.gethostbyname name)

#else

external gethostbyname_job : string -> Unix.host_entry job = "lwt_unix_gethostbyname_job"

let gethostbyname name =
  run_job (gethostbyname_job name)

#endif

#if windows

let gethostbyaddr addr =
  return (Unix.gethostbyaddr addr)

#else

external gethostbyaddr_job : Unix.inet_addr -> Unix.host_entry job = "lwt_unix_gethostbyaddr_job"

let gethostbyaddr addr =
  run_job (gethostbyaddr_job addr)

#endif

#if windows

let getprotobyname name =
  return (Unix.getprotobyname name)

#else

external getprotobyname_job : string -> Unix.protocol_entry job = "lwt_unix_getprotobyname_job"

let getprotobyname name =
  run_job (getprotobyname_job name)

#endif

#if windows

let getprotobynumber number =
  return (Unix.getprotobynumber number)

#else

external getprotobynumber_job : int -> Unix.protocol_entry job = "lwt_unix_getprotobynumber_job"

let getprotobynumber number =
  run_job (getprotobynumber_job number)

#endif

#if windows

let getservbyname name x =
  return (Unix.getservbyname name x)

#else

external getservbyname_job : string -> string -> Unix.service_entry job = "lwt_unix_getservbyname_job"

let getservbyname name x =
  run_job (getservbyname_job name x)

#endif

#if windows

let getservbyport port x =
  return (Unix.getservbyport port x)

#else

external getservbyport_job : int -> string -> Unix.service_entry job = "lwt_unix_getservbyport_job"

let getservbyport port x =
  run_job (getservbyport_job port x)

#endif

type addr_info =
    Unix.addr_info =
    {
      ai_family : socket_domain;
      ai_socktype : socket_type;
      ai_protocol : int;
      ai_addr : sockaddr;
      ai_canonname : string;
    }

type getaddrinfo_option =
    Unix.getaddrinfo_option =
  | AI_FAMILY of socket_domain
  | AI_SOCKTYPE of socket_type
  | AI_PROTOCOL of int
  | AI_NUMERICHOST
  | AI_CANONNAME
  | AI_PASSIVE

#if windows

let getaddrinfo host service opts =
  return (Unix.getaddrinfo host service opts)

#else

external getaddrinfo_job : string -> string -> Unix.getaddrinfo_option list -> Unix.addr_info list job = "lwt_unix_getaddrinfo_job"

let getaddrinfo host service opts =
  run_job (getaddrinfo_job host service opts) >>= fun l ->
  return (List.rev l)

#endif

type name_info =
    Unix.name_info =
    {
      ni_hostname : string;
      ni_service : string;
    }

type getnameinfo_option =
    Unix.getnameinfo_option =
  | NI_NOFQDN
  | NI_NUMERICHOST
  | NI_NAMEREQD
  | NI_NUMERICSERV
  | NI_DGRAM

#if windows

let getnameinfo addr opts =
  return (Unix.getnameinfo addr opts)

#else

external getnameinfo_job : Unix.sockaddr -> Unix.getnameinfo_option list -> Unix.name_info job = "lwt_unix_getnameinfo_job"

let getnameinfo addr opts =
  run_job (getnameinfo_job addr opts)

#endif

(* +-----------------------------------------------------------------+
   | Terminal interface                                              |
   +-----------------------------------------------------------------+ *)

type terminal_io =
    Unix.terminal_io =
    {
      mutable c_ignbrk : bool;
      mutable c_brkint : bool;
      mutable c_ignpar : bool;
      mutable c_parmrk : bool;
      mutable c_inpck : bool;
      mutable c_istrip : bool;
      mutable c_inlcr : bool;
      mutable c_igncr : bool;
      mutable c_icrnl : bool;
      mutable c_ixon : bool;
      mutable c_ixoff : bool;
      mutable c_opost : bool;
      mutable c_obaud : int;
      mutable c_ibaud : int;
      mutable c_csize : int;
      mutable c_cstopb : int;
      mutable c_cread : bool;
      mutable c_parenb : bool;
      mutable c_parodd : bool;
      mutable c_hupcl : bool;
      mutable c_clocal : bool;
      mutable c_isig : bool;
      mutable c_icanon : bool;
      mutable c_noflsh : bool;
      mutable c_echo : bool;
      mutable c_echoe : bool;
      mutable c_echok : bool;
      mutable c_echonl : bool;
      mutable c_vintr : char;
      mutable c_vquit : char;
      mutable c_verase : char;
      mutable c_vkill : char;
      mutable c_veof : char;
      mutable c_veol : char;
      mutable c_vmin : int;
      mutable c_vtime : int;
      mutable c_vstart : char;
      mutable c_vstop : char;
    }

type setattr_when =
    Unix.setattr_when =
  | TCSANOW
  | TCSADRAIN
  | TCSAFLUSH

type flush_queue =
    Unix.flush_queue =
  | TCIFLUSH
  | TCOFLUSH
  | TCIOFLUSH

type flow_action =
    Unix.flow_action =
  | TCOOFF
  | TCOON
  | TCIOFF
  | TCION

#if windows

let tcgetattr ch =
  check_descriptor ch;
  return (Unix.tcgetattr ch.fd)

#else

external tcgetattr_job : Unix.file_descr -> Unix.terminal_io job = "lwt_unix_tcgetattr_job"

let tcgetattr ch =
  check_descriptor ch;
  run_job (tcgetattr_job ch.fd)

#endif

#if windows

let tcsetattr ch when_ attrs =
  check_descriptor ch;
  return (Unix.tcsetattr ch.fd when_ attrs)

#else

external tcsetattr_job : Unix.file_descr -> Unix.setattr_when -> Unix.terminal_io -> unit job = "lwt_unix_tcsetattr_job"

let tcsetattr ch when_ attrs =
  check_descriptor ch;
  run_job (tcsetattr_job ch.fd when_ attrs)

#endif

#if windows

let tcsendbreak ch delay =
  check_descriptor ch;
  return (Unix.tcsendbreak ch.fd delay)

#else

let tcsendbreak ch delay =
  check_descriptor ch;
  run_job (Jobs.tcsendbreak_job ch.fd delay)

#endif

#if windows

let tcdrain ch =
  check_descriptor ch;
  return (Unix.tcdrain ch.fd)

#else

let tcdrain ch =
  check_descriptor ch;
  run_job (Jobs.tcdrain_job ch.fd)

#endif

#if windows

let tcflush ch q =
  check_descriptor ch;
  return (Unix.tcflush ch.fd q)

#else

let tcflush ch q =
  check_descriptor ch;
  run_job (Jobs.tcflush_job ch.fd q)

#endif

#if windows

let tcflow ch act =
  check_descriptor ch;
  return (Unix.tcflow ch.fd act)

#else

let tcflow ch act =
  check_descriptor ch;
  run_job (Jobs.tcflow_job ch.fd act)

#endif

(* +-----------------------------------------------------------------+
   | Reading notifications                                           |
   +-----------------------------------------------------------------+ *)

(* Buffer used to receive notifications: *)
let notification_buffer = String.create 4

external init_notification : unit -> Unix.file_descr = "lwt_unix_init_notification"
external send_notification : int -> unit = "lwt_unix_send_notification_stub"
external recv_notifications : unit -> int array = "lwt_unix_recv_notifications"

let rec handle_notifications ev =
  (* Process available notifications. *)
  Array.iter call_notification (recv_notifications ())

let event_notifications = ref (Lwt_engine.on_readable (init_notification ()) handle_notifications)

(* +-----------------------------------------------------------------+
   | Signals                                                         |
   +-----------------------------------------------------------------+ *)

external set_signal : int -> int -> unit = "lwt_unix_set_signal"
external remove_signal : int -> unit = "lwt_unix_remove_signal"
external init_signals : unit -> unit = "lwt_unix_init_signals"

let () = init_signals ()

module Signal_map = Map.Make(struct type t = int let compare a b = a - b end)

let signals = ref Signal_map.empty
let signal_count () =
  Signal_map.fold
    (fun signum (id, actions) len -> len + Lwt_sequence.length actions)
    !signals
    0

type signal_handler_id = unit Lazy.t

let on_signal signum handler =
  let notification, actions =
    try
      Signal_map.find signum !signals
    with Not_found ->
      let actions = Lwt_sequence.create () in
      let notification = make_notification (fun () -> Lwt_sequence.iter_l (fun f -> f signum) actions) in
      (try
         set_signal signum notification
       with exn ->
         stop_notification notification;
         raise exn);
      signals := Signal_map.add signum (notification, actions) !signals;
      (notification, actions)
  in
  let node = Lwt_sequence.add_r handler actions in
  lazy(Lwt_sequence.remove node;
       if Lwt_sequence.is_empty actions then begin
         remove_signal signum;
         signals := Signal_map.remove signum !signals;
         stop_notification notification
       end)

let disable_signal_handler = Lazy.force

let reinstall_signal_handler signum =
  match try Some (Signal_map.find signum !signals) with Not_found -> None with
    | Some (notification, actions) ->
        set_signal signum notification
    | None ->
        ()

(* +-----------------------------------------------------------------+
   | Processes                                                       |
   +-----------------------------------------------------------------+ *)

external reset_after_fork : unit -> unit = "lwt_unix_reset_after_fork"

let fork () =
  match Unix.fork () with
    | 0 ->
        (* Reset threading. *)
        reset_after_fork ();
        (* Stop the old event for notifications. *)
        Lwt_engine.stop_event !event_notifications;
        (* Reinitialise the notification system. *)
        event_notifications := Lwt_engine.on_readable (init_notification ()) handle_notifications;
        (* Collect all pending jobs. *)
        let l = Lwt_sequence.fold_l (fun (w, f) l -> f :: l) jobs [] in
        (* Remove them all. *)
        Lwt_sequence.iter_node_l Lwt_sequence.remove jobs;
        (* And cancel them all. We yield first so that if the program
           do an exec just after, it won't be executed. *)
        on_termination (Lwt_main.yield ()) (fun () -> List.iter (fun f -> f Lwt.Canceled) l);
        0
    | pid ->
        pid

type process_status =
    Unix.process_status =
  | WEXITED of int
  | WSIGNALED of int
  | WSTOPPED of int

type wait_flag =
    Unix.wait_flag =
  | WNOHANG
  | WUNTRACED

let has_wait4 = not Lwt_sys.windows

type resource_usage = { ru_utime : float; ru_stime : float }

#if windows

let stub_wait4 flags pid =
  let pid, status = Unix.waitpid flags pid in
  (pid, status, { ru_utime = 0.0; ru_stime = 0.0 })

#else

external stub_wait4 : Unix.wait_flag list -> int -> int * Unix.process_status * resource_usage = "lwt_unix_wait4"

#endif

let wait_children = Lwt_sequence.create ()
let wait_count () = Lwt_sequence.length wait_children

#if not windows
let () =
  ignore begin
    on_signal Sys.sigchld
      (fun _ ->
         Lwt_sequence.iter_node_l begin fun node ->
           let wakener, flags, pid = Lwt_sequence.get node in
           try
             let (pid', _, _) as v = stub_wait4 flags pid in
             if pid' <> 0 then begin
               Lwt_sequence.remove node;
               Lwt.wakeup wakener v
             end
           with e ->
             Lwt_sequence.remove node;
             Lwt.wakeup_exn wakener e
         end wait_children)
  end
#endif

let _waitpid flags pid =
  try_lwt
    return (Unix.waitpid flags pid)

#if windows

let waitpid = _waitpid

#else

let waitpid flags pid =
  if List.mem Unix.WNOHANG flags then
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

#endif

let _wait4 flags pid =
  try_lwt
    return (stub_wait4 flags pid)

#if windows

let wait4 = _wait4

#else

let wait4 flags pid =
  if List.mem Unix.WNOHANG flags then
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

#endif

let wait () = waitpid [] (-1)

#if windows

external system_job : string -> int job = "lwt_unix_system_job"

let system cmd =
  lwt code = run_job (system_job ("cmd.exe /c " ^ cmd)) in
  return (Unix.WEXITED code)

#else

let system cmd =
  match fork () with
    | 0 ->
        begin try
          Unix.execv "/bin/sh" [| "/bin/sh"; "-c"; cmd |]
        with _ ->
          (* Prevent exit hooks from running, they are not supposed to
             be executed here. *)
          Lwt_sequence.iter_node_l Lwt_sequence.remove Lwt_main.exit_hooks;
          exit 127
        end
    | id ->
        waitpid [] id >|= snd

#endif

(* +-----------------------------------------------------------------+
   | Misc                                                            |
   +-----------------------------------------------------------------+ *)

let run = Lwt_main.run

let handle_unix_error f x =
  try_lwt
    f x
  with exn ->
    Unix.handle_unix_error (fun () -> raise exn) ()

(* +-----------------------------------------------------------------+
   | System thread pool                                              |
   +-----------------------------------------------------------------+ *)

external pool_size : unit -> int = "lwt_unix_pool_size" "noalloc"
external set_pool_size : int -> unit = "lwt_unix_set_pool_size" "noalloc"
external thread_count : unit -> int = "lwt_unix_thread_count" "noalloc"
external thread_waiting_count : unit -> int = "lwt_unix_thread_waiting_count" "noalloc"

(* +-----------------------------------------------------------------+
   | CPUs                                                            |
   +-----------------------------------------------------------------+ *)

#if HAVE_GETCPU
external get_cpu : unit -> int = "lwt_unix_get_cpu"
#else
let get_cpu () = raise (Lwt_sys.Not_available "get_cpu")
#endif

#if HAVE_AFFINITY

external stub_get_affinity : int -> int list = "lwt_unix_get_affinity"
external stub_set_affinity : int -> int list -> unit = "lwt_unix_set_affinity"

let get_affinity ?(pid=0) () = stub_get_affinity pid
let set_affinity ?(pid=0) l = stub_set_affinity pid l

#else

let get_affinity ?pid () = raise (Lwt_sys.Not_available "get_affinity")
let set_affinity ?pid l = raise (Lwt_sys.Not_available "set_affinity")

#endif

(* +-----------------------------------------------------------------+
   | Error printing                                                  |
   +-----------------------------------------------------------------+ *)

let () =
  Printexc.register_printer
    (function
       | Unix.Unix_error(error, func, arg) ->
           let error =
             match error with
               | Unix.E2BIG -> "E2BIG"
               | Unix.EACCES -> "EACCES"
               | Unix.EAGAIN -> "EAGAIN"
               | Unix.EBADF -> "EBADF"
               | Unix.EBUSY -> "EBUSY"
               | Unix.ECHILD -> "ECHILD"
               | Unix.EDEADLK -> "EDEADLK"
               | Unix.EDOM -> "EDOM"
               | Unix.EEXIST -> "EEXIST"
               | Unix.EFAULT -> "EFAULT"
               | Unix.EFBIG -> "EFBIG"
               | Unix.EINTR -> "EINTR"
               | Unix.EINVAL -> "EINVAL"
               | Unix.EIO -> "EIO"
               | Unix.EISDIR -> "EISDIR"
               | Unix.EMFILE -> "EMFILE"
               | Unix.EMLINK -> "EMLINK"
               | Unix.ENAMETOOLONG -> "ENAMETOOLONG"
               | Unix.ENFILE -> "ENFILE"
               | Unix.ENODEV -> "ENODEV"
               | Unix.ENOENT -> "ENOENT"
               | Unix.ENOEXEC -> "ENOEXEC"
               | Unix.ENOLCK -> "ENOLCK"
               | Unix.ENOMEM -> "ENOMEM"
               | Unix.ENOSPC -> "ENOSPC"
               | Unix.ENOSYS -> "ENOSYS"
               | Unix.ENOTDIR -> "ENOTDIR"
               | Unix.ENOTEMPTY -> "ENOTEMPTY"
               | Unix.ENOTTY -> "ENOTTY"
               | Unix.ENXIO -> "ENXIO"
               | Unix.EPERM -> "EPERM"
               | Unix.EPIPE -> "EPIPE"
               | Unix.ERANGE -> "ERANGE"
               | Unix.EROFS -> "EROFS"
               | Unix.ESPIPE -> "ESPIPE"
               | Unix.ESRCH -> "ESRCH"
               | Unix.EXDEV -> "EXDEV"
               | Unix.EWOULDBLOCK -> "EWOULDBLOCK"
               | Unix.EINPROGRESS -> "EINPROGRESS"
               | Unix.EALREADY -> "EALREADY"
               | Unix.ENOTSOCK -> "ENOTSOCK"
               | Unix.EDESTADDRREQ -> "EDESTADDRREQ"
               | Unix.EMSGSIZE -> "EMSGSIZE"
               | Unix.EPROTOTYPE -> "EPROTOTYPE"
               | Unix.ENOPROTOOPT -> "ENOPROTOOPT"
               | Unix.EPROTONOSUPPORT -> "EPROTONOSUPPORT"
               | Unix.ESOCKTNOSUPPORT -> "ESOCKTNOSUPPORT"
               | Unix.EOPNOTSUPP -> "EOPNOTSUPP"
               | Unix.EPFNOSUPPORT -> "EPFNOSUPPORT"
               | Unix.EAFNOSUPPORT -> "EAFNOSUPPORT"
               | Unix.EADDRINUSE -> "EADDRINUSE"
               | Unix.EADDRNOTAVAIL -> "EADDRNOTAVAIL"
               | Unix.ENETDOWN -> "ENETDOWN"
               | Unix.ENETUNREACH -> "ENETUNREACH"
               | Unix.ENETRESET -> "ENETRESET"
               | Unix.ECONNABORTED -> "ECONNABORTED"
               | Unix.ECONNRESET -> "ECONNRESET"
               | Unix.ENOBUFS -> "ENOBUFS"
               | Unix.EISCONN -> "EISCONN"
               | Unix.ENOTCONN -> "ENOTCONN"
               | Unix.ESHUTDOWN -> "ESHUTDOWN"
               | Unix.ETOOMANYREFS -> "ETOOMANYREFS"
               | Unix.ETIMEDOUT -> "ETIMEDOUT"
               | Unix.ECONNREFUSED -> "ECONNREFUSED"
               | Unix.EHOSTDOWN -> "EHOSTDOWN"
               | Unix.EHOSTUNREACH -> "EHOSTUNREACH"
               | Unix.ELOOP -> "ELOOP"
               | Unix.EOVERFLOW -> "EOVERFLOW"
               | Unix.EUNKNOWNERR n -> Printf.sprintf "EUNKNOWNERR %d" n
           in
           Some(Printf.sprintf "Unix.Unix_error(Unix.%s, %S, %S)" error func arg)
       | _ ->
           None)
