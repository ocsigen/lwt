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
  (* Starts the job. *)
  let job_done = start_job job async_method in
  lwt status =
    if job_done then
      return None
    else
      (* Create the notification for asynchronous wakeup. *)
      let id = make_notification ~once:true ignore in
      try_lwt
        (* Give some time to the job before we fallback to
           asynchronous notification. *)
        lwt () = pause () in
        if check_job job id then begin
          stop_notification id;
          return None
        end else
          return (Some id)
      with Canceled as exn ->
        cancel_job job;
        (* Free resources when the job terminates. *)
        if check_job job id then begin
          stop_notification id;
          free job
        end else
          set_notification id (fun () -> free job);
        raise_lwt exn
  in
  match status with
    | None ->
        (* The job has already terminated, read and return the result
           immediatly. *)
        let thread =
          try
            return (result job)
          with exn ->
            fail exn
        in
        free job;
        thread
    | Some id ->
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

let is_blocking ?blocking ?(set_flags=true) fd =
    match (Unix.fstat fd).Unix.st_kind with
      | Unix.S_SOCK -> begin
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
        end
      | _ -> begin
          match blocking with
            | Some state ->
                lazy(return state)
            | None ->
                lazy(return true)
        end

#else

external guess_blocking_job : Unix.file_descr -> [ `unix_guess_blocking ] job = "lwt_unix_guess_blocking_job"
external guess_blocking_result : [ `unix_guess_blocking ] job -> bool = "lwt_unix_guess_blocking_result" "noalloc"
external guess_blocking_free : [ `unix_guess_blocking ] job -> unit = "lwt_unix_guess_blocking_free" "noalloc"

let guess_blocking fd =
  execute_job (guess_blocking_job fd) guess_blocking_result guess_blocking_free

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

let stub_readable fd = Unix.select [fd] [] [] (-1.0) <> ([], [], [])
let stub_writable fd = Unix.select [] [fd] [] (-1.0) <> ([], [], [])

#else

external stub_readable : Unix.file_descr -> bool = "lwt_unix_readable"
external stub_writable : Unix.file_descr -> bool = "lwt_unix_writable"

#endif

let readable ch =
  check_descriptor ch;
  stub_readable ch.fd

let writable ch =
  check_descriptor ch;
  stub_writable ch.fd

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
  try
    check_descriptor ch;
    lwt blocking = Lazy.force ch.blocking in
    if not blocking || (event = Read && stub_readable ch.fd) || (event = Write && stub_writable ch.fd) then
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

#if windows

let openfile name flags perms =
  return (of_unix_file_descr (Unix.openfile name flags perms))

#else

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

#endif

#if windows

let close ch =
  if ch.state = Closed then check_descriptor ch;
  set_state ch Closed;
  clear_events ch;
  return (Unix.close ch.fd)

#else

external close_job : Unix.file_descr -> [ `unix_close ] job = "lwt_unix_close_job"
external close_result : [ `unix_close ] job -> unit = "lwt_unix_close_result"
external close_free : [ `unix_close ] job -> unit = "lwt_unix_close_free" "noalloc"

let close ch =
  if ch.state = Closed then check_descriptor ch;
  set_state ch Closed;
  clear_events ch;
  execute_job (close_job ch.fd) close_result close_free

#endif

let wait_read ch =
  try_lwt
    if readable ch then
      return ()
    else
      register_action Read ch ignore

external stub_read : Unix.file_descr -> string -> int -> int -> int = "lwt_unix_read"
external read_job : Unix.file_descr -> int -> [ `unix_read ] job = "lwt_unix_read_job"
external read_result : [ `unix_read ] job -> string -> int -> int = "lwt_unix_read_result"
external read_free : [ `unix_read ] job -> unit = "lwt_unix_read_free" "noalloc"

let read ch buf pos len =
  if pos < 0 || len < 0 || pos > String.length buf - len then
    invalid_arg "Lwt_unix.read"
  else
    Lazy.force ch.blocking >>= function
      | true ->
          lwt () = wait_read ch in
          execute_job (read_job ch.fd len) (fun job -> read_result job buf pos) read_free
      | false ->
          wrap_syscall Read ch (fun () -> stub_read ch.fd buf pos len)

let wait_write ch =
  try_lwt
    if writable ch then
      return ()
    else
      register_action Write ch ignore

external stub_write : Unix.file_descr -> string -> int -> int -> int = "lwt_unix_write"
external write_job : Unix.file_descr -> string -> int -> int -> [ `unix_write ] job = "lwt_unix_write_job"
external write_result : [ `unix_write ] job -> int = "lwt_unix_write_result"
external write_free : [ `unix_write ] job -> unit = "lwt_unix_write_free" "noalloc"

let write ch buf pos len =
  if pos < 0 || len < 0 || pos > String.length buf - len then
    invalid_arg "Lwt_unix.write"
  else
    Lazy.force ch.blocking >>= function
      | true ->
          lwt () = wait_write ch in
          execute_job (write_job ch.fd buf pos len) write_result write_free
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

external lseek_job : Unix.file_descr -> int -> Unix.seek_command -> [ `unix_lseek ] job = "lwt_unix_lseek_job"
external lseek_result : [ `unix_lseek ] job -> int = "lwt_unix_lseek_result"
external lseek_free : [ `unix_lseek ] job -> unit = "lwt_unix_lseek_free"

let lseek ch offset whence =
  check_descriptor ch;
  execute_job (lseek_job ch.fd offset whence) lseek_result lseek_free

#endif

#if windows

let truncate name offset =
  return (Unix.truncate name offset)

#else

external truncate_job : string -> int -> [ `unix_truncate ] job = "lwt_unix_truncate_job"
external truncate_result : [ `unix_truncate ] job -> unit = "lwt_unix_truncate_result"
external truncate_free : [ `unix_truncate ] job -> unit = "lwt_unix_truncate_free"

let truncate name offset =
  execute_job (truncate_job name offset) truncate_result truncate_free

#endif

#if windows

let ftruncate ch offset =
  check_descriptor ch;
  return (Unix.ftruncate ch.fd offset)

#else

external ftruncate_job : Unix.file_descr -> int -> [ `unix_ftruncate ] job = "lwt_unix_ftruncate_job"
external ftruncate_result : [ `unix_ftruncate ] job -> unit = "lwt_unix_ftruncate_result"
external ftruncate_free : [ `unix_ftruncate ] job -> unit = "lwt_unix_ftruncate_free"

let ftruncate ch offset =
  check_descriptor ch;
  execute_job (ftruncate_job ch.fd offset) ftruncate_result ftruncate_free

#endif

(* +-----------------------------------------------------------------+
   | Syncing                                                         |
   +-----------------------------------------------------------------+ *)

external fsync_job : Unix.file_descr -> [ `unix_fsync ] job = "lwt_unix_fsync_job"
external fsync_result : [ `unix_fsync ] job -> unit = "lwt_unix_fsync_result"
external fsync_free : [ `unix_fsync ] job -> unit = "lwt_unix_fsync_free"

let fsync ch =
  check_descriptor ch;
  execute_job (fsync_job ch.fd) fsync_result fsync_free

#if HAVE_FDATASYNC

external fdatasync_job : Unix.file_descr -> [ `unix_fdatasync ] job = "lwt_unix_fdatasync_job"
external fdatasync_result : [ `unix_fdatasync ] job -> unit = "lwt_unix_fdatasync_result"
external fdatasync_free : [ `unix_fdatasync ] job -> unit = "lwt_unix_fdatasync_free"

let fdatasync ch =
  check_descriptor ch;
  execute_job (fdatasync_job ch.fd) fdatasync_result fdatasync_free

#else

let fdatasync ch =
  fail (Lwt_sys.Not_available "fdatasync")

#endif

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

external stat_job : string -> [ `unix_stat ] job = "lwt_unix_stat_job"
external stat_result : [ `unix_stat ] job -> Unix.stats = "lwt_unix_stat_result"
external stat_free : [ `unix_stat ] job -> unit = "lwt_unix_stat_free"

let stat name =
  execute_job (stat_job name) stat_result stat_free

#endif

#if windows

let lstat name =
  return (Unix.lstat name)

#else

external lstat_job : string -> [ `unix_lstat ] job = "lwt_unix_lstat_job"
external lstat_result : [ `unix_lstat ] job -> Unix.stats = "lwt_unix_lstat_result"
external lstat_free : [ `unix_lstat ] job -> unit = "lwt_unix_lstat_free"

let lstat name =
  execute_job (lstat_job name) lstat_result lstat_free

#endif

#if windows

let fstat ch =
  check_descriptor ch;
  return (Unix.fstat ch.fd)

#else

external fstat_job : Unix.file_descr -> [ `unix_fstat ] job = "lwt_unix_fstat_job"
external fstat_result : [ `unix_fstat ] job -> Unix.stats = "lwt_unix_fstat_result"
external fstat_free : [ `unix_fstat ] job -> unit = "lwt_unix_fstat_free"

let fstat ch =
  check_descriptor ch;
  execute_job (fstat_job ch.fd) fstat_result fstat_free

#endif

#if windows

let isatty ch =
  check_descriptor ch;
  return (Unix.isatty ch.fd)

#else

external isatty_job : Unix.file_descr -> [ `unix_isatty ] job = "lwt_unix_isatty_job"
external isatty_result : [ `unix_isatty ] job -> bool = "lwt_unix_isatty_result"
external isatty_free : [ `unix_isatty ] job -> unit = "lwt_unix_isatty_free"

let isatty ch =
  check_descriptor ch;
  execute_job (isatty_job ch.fd) isatty_result isatty_free

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

  external lseek_job : Unix.file_descr -> int64 -> Unix.seek_command -> [ `unix_lseek ] job = "lwt_unix_lseek_64_job"
  external lseek_result : [ `unix_lseek ] job -> int64 = "lwt_unix_lseek_64_result"
  external lseek_free : [ `unix_lseek ] job -> unit = "lwt_unix_lseek_64_free"

  let lseek ch offset whence =
    check_descriptor ch;
    execute_job (lseek_job ch.fd offset whence) lseek_result lseek_free

#endif

#if windows

  let truncate name offset =
    return (Unix.LargeFile.truncate name offset)

#else

  external truncate_job : string -> int64 -> [ `unix_truncate ] job = "lwt_unix_truncate_64_job"
  external truncate_result : [ `unix_truncate ] job -> unit = "lwt_unix_truncate_64_result"
  external truncate_free : [ `unix_truncate ] job -> unit = "lwt_unix_truncate_64_free"

  let truncate name offset =
    execute_job (truncate_job name offset) truncate_result truncate_free

#endif

#if windows

  let ftruncate ch offset =
    check_descriptor ch;
    return (Unix.LargeFile.ftruncate ch.fd offset)

#else

  external ftruncate_job : Unix.file_descr -> int64 -> [ `unix_ftruncate ] job = "lwt_unix_ftruncate_64_job"
  external ftruncate_result : [ `unix_ftruncate ] job -> unit = "lwt_unix_ftruncate_64_result"
  external ftruncate_free : [ `unix_ftruncate ] job -> unit = "lwt_unix_ftruncate_64_free"

  let ftruncate ch offset =
    check_descriptor ch;
    execute_job (ftruncate_job ch.fd offset) ftruncate_result ftruncate_free

#endif

#if windows

  let stat name =
    return (Unix.LargeFile.stat name)

#else

  external stat_job : string -> [ `unix_stat ] job = "lwt_unix_stat_64_job"
  external stat_result : [ `unix_stat ] job -> Unix.LargeFile.stats = "lwt_unix_stat_64_result"
  external stat_free : [ `unix_stat ] job -> unit = "lwt_unix_stat_64_free"

  let stat name =
    execute_job (stat_job name) stat_result stat_free

#endif

#if windows

  let lstat name =
    return (Unix.LargeFile.lstat name)

#else

  external lstat_job : string -> [ `unix_lstat ] job = "lwt_unix_lstat_64_job"
  external lstat_result : [ `unix_lstat ] job -> Unix.LargeFile.stats = "lwt_unix_lstat_64_result"
  external lstat_free : [ `unix_lstat ] job -> unit = "lwt_unix_lstat_64_free"

  let lstat name =
    execute_job (lstat_job name) lstat_result lstat_free

#endif

#if windows

  let fstat ch =
    check_descriptor ch;
    return (Unix.LargeFile.fstat ch.fd)

#else

  external fstat_job : Unix.file_descr -> [ `unix_fstat ] job = "lwt_unix_fstat_64_job"
  external fstat_result : [ `unix_fstat ] job -> Unix.LargeFile.stats = "lwt_unix_fstat_64_result"
  external fstat_free : [ `unix_fstat ] job -> unit = "lwt_unix_fstat_64_free"

  let fstat ch =
    check_descriptor ch;
    execute_job (fstat_job ch.fd) fstat_result fstat_free

#endif

end

(* +-----------------------------------------------------------------+
   | Operations on file names                                        |
   +-----------------------------------------------------------------+ *)

#if windows

let unlink name =
  return (Unix.unlink name)

#else

external unlink_job : string -> [ `unix_unlink ] job = "lwt_unix_unlink_job"
external unlink_result : [ `unix_unlink ] job -> unit = "lwt_unix_unlink_result"
external unlink_free : [ `unix_unlink ] job -> unit = "lwt_unix_unlink_free"

let unlink name =
  execute_job (unlink_job name) unlink_result unlink_free

#endif

#if windows

let rename name1 name2 =
  return (Unix.rename name1 name2)

#else

external rename_job : string -> string -> [ `unix_rename ] job = "lwt_unix_rename_job"
external rename_result : [ `unix_rename ] job -> unit = "lwt_unix_rename_result"
external rename_free : [ `unix_rename ] job -> unit = "lwt_unix_rename_free"

let rename name1 name2 =
  execute_job (rename_job name1 name2) rename_result rename_free

#endif

#if windows

let link name1 name2 =
  return (Unix.link name1 name2)

#else

external link_job : string -> string -> [ `unix_link ] job = "lwt_unix_link_job"
external link_result : [ `unix_link ] job -> unit = "lwt_unix_link_result"
external link_free : [ `unix_link ] job -> unit = "lwt_unix_link_free"

let link name1 name2 =
  execute_job (link_job name1 name2) link_result link_free

#endif

(* +-----------------------------------------------------------------+
   | File permissions and ownership                                  |
   +-----------------------------------------------------------------+ *)

#if windows

let chmod name perms =
  return (Unix.chmod name perms)

#else

external chmod_job : string -> Unix.file_perm -> [ `unix_chmod ] job = "lwt_unix_chmod_job"
external chmod_result : [ `unix_chmod ] job -> unit = "lwt_unix_chmod_result"
external chmod_free : [ `unix_chmod ] job -> unit = "lwt_unix_chmod_free"

let chmod name perms =
  execute_job (chmod_job name perms) chmod_result chmod_free

#endif

#if windows

let fchmod ch perms =
  check_descriptor ch;
  return (Unix.fchmod ch.fd perms)

#else

external fchmod_job : Unix.file_descr -> Unix.file_perm -> [ `unix_fchmod ] job = "lwt_unix_fchmod_job"
external fchmod_result : [ `unix_fchmod ] job -> unit = "lwt_unix_fchmod_result"
external fchmod_free : [ `unix_fchmod ] job -> unit = "lwt_unix_fchmod_free"

let fchmod ch perms =
  check_descriptor ch;
  execute_job (fchmod_job ch.fd perms) fchmod_result fchmod_free

#endif

#if windows

let chown name uid gid =
  return (Unix.chown name uid gid)

#else

external chown_job : string -> int -> int -> [ `unix_chown ] job = "lwt_unix_chown_job"
external chown_result : [ `unix_chown ] job -> unit = "lwt_unix_chown_result"
external chown_free : [ `unix_chown ] job -> unit = "lwt_unix_chown_free"

let chown name uid gid =
  execute_job (chown_job name uid gid) chown_result chown_free

#endif

#if windows

let fchown ch uid gid =
  check_descriptor ch;
  return (Unix.fchown ch.fd uid gid)

#else

external fchown_job : Unix.file_descr -> int -> int -> [ `unix_fchown ] job = "lwt_unix_fchown_job"
external fchown_result : [ `unix_fchown ] job -> unit = "lwt_unix_fchown_result"
external fchown_free : [ `unix_fchown ] job -> unit = "lwt_unix_fchown_free"

let fchown ch uid gid =
  check_descriptor ch;
  execute_job (fchown_job ch.fd uid gid) fchown_result fchown_free

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

external access_job : string -> Unix.access_permission list -> [ `unix_access ] job = "lwt_unix_access_job"
external access_result : [ `unix_access ] job -> unit = "lwt_unix_access_result"
external access_free : [ `unix_access ] job -> unit = "lwt_unix_access_free"

let access name perms =
  execute_job (access_job name perms) access_result access_free

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

external mkdir_job : string -> Unix.file_perm -> [ `unix_mkdir ] job = "lwt_unix_mkdir_job"
external mkdir_result : [ `unix_mkdir ] job -> unit = "lwt_unix_mkdir_result"
external mkdir_free : [ `unix_mkdir ] job -> unit = "lwt_unix_mkdir_free"

let mkdir name perms =
  execute_job (mkdir_job name perms) mkdir_result mkdir_free

#endif

#if windows

let rmdir name =
  return (Unix.rmdir name)

#else

external rmdir_job : string -> [ `unix_rmdir ] job = "lwt_unix_rmdir_job"
external rmdir_result : [ `unix_rmdir ] job -> unit = "lwt_unix_rmdir_result"
external rmdir_free : [ `unix_rmdir ] job -> unit = "lwt_unix_rmdir_free"

let rmdir name =
  execute_job (rmdir_job name) rmdir_result rmdir_free

#endif

#if windows

let chdir name =
  return (Unix.chdir name)

#else

external chdir_job : string -> [ `unix_chdir ] job = "lwt_unix_chdir_job"
external chdir_result : [ `unix_chdir ] job -> unit = "lwt_unix_chdir_result"
external chdir_free : [ `unix_chdir ] job -> unit = "lwt_unix_chdir_free"

let chdir name =
  execute_job (chdir_job name) chdir_result chdir_free

#endif

#if windows

let chroot name =
  return (Unix.chroot name)

#else

external chroot_job : string -> [ `unix_chroot ] job = "lwt_unix_chroot_job"
external chroot_result : [ `unix_chroot ] job -> unit = "lwt_unix_chroot_result"
external chroot_free : [ `unix_chroot ] job -> unit = "lwt_unix_chroot_free"

let chroot name =
  execute_job (chroot_job name) chroot_result chroot_free

#endif

type dir_handle = Unix.dir_handle

#if windows

let opendir name =
  return (Unix.opendir name)

#else

external opendir_job : string -> [ `unix_opendir ] job = "lwt_unix_opendir_job"
external opendir_result : [ `unix_opendir ] job -> Unix.dir_handle = "lwt_unix_opendir_result"
external opendir_free : [ `unix_opendir ] job -> unit = "lwt_unix_opendir_free"

let opendir name =
  execute_job (opendir_job name) opendir_result opendir_free

#endif

#if windows

let readdir handle =
  return (Unix.readdir handle)

#else

external readdir_job : Unix.dir_handle -> [ `unix_readdir ] job = "lwt_unix_readdir_job"
external readdir_result : [ `unix_readdir ] job -> string = "lwt_unix_readdir_result"
external readdir_free : [ `unix_readdir ] job -> unit = "lwt_unix_readdir_free"

let readdir handle =
  execute_job (readdir_job handle) readdir_result readdir_free

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

external readdir_n_job : Unix.dir_handle -> int -> [ `unix_readdir_n ] job = "lwt_unix_readdir_n_job"
external readdir_n_result : [ `unix_readdir_n ] job -> string array = "lwt_unix_readdir_n_result"
external readdir_n_free : [ `unix_readdir_n ] job -> unit = "lwt_unix_readdir_n_free"

let readdir_n handle count =
  if count < 0 then
    fail (Invalid_argument "Lwt_uinx.readdir_n")
  else
    execute_job (readdir_n_job handle count) readdir_n_result readdir_n_free

#endif

#if windows

let rewinddir handle =
  return (Unix.rewinddir handle)

#else

external rewinddir_job : Unix.dir_handle -> [ `unix_rewinddir ] job = "lwt_unix_rewinddir_job"
external rewinddir_result : [ `unix_rewinddir ] job -> unit = "lwt_unix_rewinddir_result"
external rewinddir_free : [ `unix_rewinddir ] job -> unit = "lwt_unix_rewinddir_free"

let rewinddir handle =
  execute_job (rewinddir_job handle) rewinddir_result rewinddir_free

#endif

#if windows

let closedir handle =
  return (Unix.closedir handle)

#else

external closedir_job : Unix.dir_handle -> [ `unix_closedir ] job = "lwt_unix_closedir_job"
external closedir_result : [ `unix_closedir ] job -> unit = "lwt_unix_closedir_result"
external closedir_free : [ `unix_closedir ] job -> unit = "lwt_unix_closedir_free"

let closedir handle =
  execute_job (closedir_job handle) closedir_result closedir_free

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

external mkfifo_job : string -> Unix.file_perm -> [ `unix_mkfifo ] job = "lwt_unix_mkfifo_job"
external mkfifo_result : [ `unix_mkfifo ] job -> unit = "lwt_unix_mkfifo_result"
external mkfifo_free : [ `unix_mkfifo ] job -> unit = "lwt_unix_mkfifo_free"

let mkfifo name perms =
  execute_job (mkfifo_job name perms) mkfifo_result mkfifo_free

#endif

(* +-----------------------------------------------------------------+
   | Symbolic links                                                  |
   +-----------------------------------------------------------------+ *)

#if windows

let symlink name1 name2 =
  return (Unix.symlink name1 name2)

#else

external symlink_job : string -> string -> [ `unix_symlink ] job = "lwt_unix_symlink_job"
external symlink_result : [ `unix_symlink ] job -> unit = "lwt_unix_symlink_result"
external symlink_free : [ `unix_symlink ] job -> unit = "lwt_unix_symlink_free"

let symlink name1 name2 =
  execute_job (symlink_job name1 name2) symlink_result symlink_free

#endif

#if windows

let readlink name =
  return (Unix.readlink name)

#else

external readlink_job : string -> [ `unix_readlink ] job = "lwt_unix_readlink_job"
external readlink_result : [ `unix_readlink ] job -> string = "lwt_unix_readlink_result"
external readlink_free : [ `unix_readlink ] job -> unit = "lwt_unix_readlink_free"

let readlink name =
  execute_job (readlink_job name) readlink_result readlink_free

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

external lockf_job : Unix.file_descr -> Unix.lock_command -> int -> [ `unix_lockf ] job = "lwt_unix_lockf_job"
external lockf_result : [ `unix_lockf ] job -> unit = "lwt_unix_lockf_result"
external lockf_free : [ `unix_lockf ] job -> unit = "lwt_unix_lockf_free"

let lockf ch cmd size =
  check_descriptor ch;
  execute_job (lockf_job ch.fd cmd size) lockf_result lockf_free

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

external getlogin_job : unit -> [ `unix_getlogin ] job = "lwt_unix_getlogin_job"
external getlogin_result : [ `unix_getlogin ] job -> string = "lwt_unix_getlogin_result"
external getlogin_free : [ `unix_getlogin ] job -> unit = "lwt_unix_getlogin_free"

let getlogin () =
  execute_job (getlogin_job ()) getlogin_result getlogin_free

#endif

#if windows

let getpwnam name =
  return (Unix.getpwnam name)

#else

external getpwnam_job : string -> [ `unix_getpwnam ] job = "lwt_unix_getpwnam_job"
external getpwnam_result : [ `unix_getpwnam ] job -> Unix.passwd_entry = "lwt_unix_getpwnam_result"
external getpwnam_free : [ `unix_getpwnam ] job -> unit = "lwt_unix_getpwnam_free"

let getpwnam name =
  execute_job (getpwnam_job name) getpwnam_result getpwnam_free

#endif

#if windows

let getgrnam name =
  return (Unix.getgrnam name)

#else

external getgrnam_job : string -> [ `unix_getgrnam ] job = "lwt_unix_getgrnam_job"
external getgrnam_result : [ `unix_getgrnam ] job -> Unix.group_entry = "lwt_unix_getgrnam_result"
external getgrnam_free : [ `unix_getgrnam ] job -> unit = "lwt_unix_getgrnam_free"

let getgrnam name =
  execute_job (getgrnam_job name) getgrnam_result getgrnam_free

#endif

#if windows

let getpwuid uid =
  return (Unix.getpwuid uid)

#else

external getpwuid_job : int -> [ `unix_getpwuid ] job = "lwt_unix_getpwuid_job"
external getpwuid_result : [ `unix_getpwuid ] job -> Unix.passwd_entry = "lwt_unix_getpwuid_result"
external getpwuid_free : [ `unix_getpwuid ] job -> unit = "lwt_unix_getpwuid_free"

let getpwuid uid =
  execute_job (getpwuid_job uid) getpwuid_result getpwuid_free

#endif

#if windows

let getgrgid gid =
  return (Unix.getgrgid gid)

#else

external getgrgid_job : int -> [ `unix_getgrgid ] job = "lwt_unix_getgrgid_job"
external getgrgid_result : [ `unix_getgrgid ] job -> Unix.group_entry = "lwt_unix_getgrgid_result"
external getgrgid_free : [ `unix_getgrgid ] job -> unit = "lwt_unix_getgrgid_free"

let getgrgid gid =
  execute_job (getgrgid_job gid) getgrgid_result getgrgid_free

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

let socketpair dom typ proto =
  let (s1, s2) = Unix.socketpair dom typ proto in
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
            if blocking && not (stub_readable ch.fd) then raise Retry;
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

external gethostname_job : unit -> [ `unix_gethostname ] job = "lwt_unix_gethostname_job"
external gethostname_result : [ `unix_gethostname ] job -> string = "lwt_unix_gethostname_result"
external gethostname_free : [ `unix_gethostname ] job -> unit = "lwt_unix_gethostname_free"

let gethostname () =
  execute_job (gethostname_job ()) gethostname_result gethostname_free

#endif

#if windows

let gethostbyname name =
  return (Unix.gethostbyname name)

#else

external gethostbyname_job : string -> [ `unix_gethostbyname ] job = "lwt_unix_gethostbyname_job"
external gethostbyname_result : [ `unix_gethostbyname ] job -> Unix.host_entry = "lwt_unix_gethostbyname_result"
external gethostbyname_free : [ `unix_gethostbyname ] job -> unit = "lwt_unix_gethostbyname_free"

let gethostbyname name =
  execute_job (gethostbyname_job name) gethostbyname_result gethostbyname_free

#endif

#if windows

let gethostbyaddr addr =
  return (Unix.gethostbyaddr addr)

#else

external gethostbyaddr_job : Unix.inet_addr -> [ `unix_gethostbyaddr ] job = "lwt_unix_gethostbyaddr_job"
external gethostbyaddr_result : [ `unix_gethostbyaddr ] job -> Unix.host_entry = "lwt_unix_gethostbyaddr_result"
external gethostbyaddr_free : [ `unix_gethostbyaddr ] job -> unit = "lwt_unix_gethostbyaddr_free"

let gethostbyaddr addr =
  execute_job (gethostbyaddr_job addr) gethostbyaddr_result gethostbyaddr_free

#endif

#if windows

let getprotobyname name =
  return (Unix.getprotobyname name)

#else

external getprotobyname_job : string -> [ `unix_getprotobyname ] job = "lwt_unix_getprotobyname_job"
external getprotobyname_result : [ `unix_getprotobyname ] job -> Unix.protocol_entry = "lwt_unix_getprotobyname_result"
external getprotobyname_free : [ `unix_getprotobyname ] job -> unit = "lwt_unix_getprotobyname_free"

let getprotobyname name =
  execute_job (getprotobyname_job name) getprotobyname_result getprotobyname_free

#endif

#if windows

let getprotobynumber number =
  return (Unix.getprotobynumber number)

#else

external getprotobynumber_job : int -> [ `unix_getprotobynumber ] job = "lwt_unix_getprotobynumber_job"
external getprotobynumber_result : [ `unix_getprotobynumber ] job -> Unix.protocol_entry = "lwt_unix_getprotobynumber_result"
external getprotobynumber_free : [ `unix_getprotobynumber ] job -> unit = "lwt_unix_getprotobynumber_free"

let getprotobynumber number =
  execute_job (getprotobynumber_job number) getprotobynumber_result getprotobynumber_free

#endif

#if windows

let getservbyname name x =
  return (Unix.getservbyname name x)

#else

external getservbyname_job : string -> string -> [ `unix_getservbyname ] job = "lwt_unix_getservbyname_job"
external getservbyname_result : [ `unix_getservbyname ] job -> Unix.service_entry = "lwt_unix_getservbyname_result"
external getservbyname_free : [ `unix_getservbyname ] job -> unit = "lwt_unix_getservbyname_free"

let getservbyname name x =
  execute_job (getservbyname_job name x) getservbyname_result getservbyname_free

#endif

#if windows

let getservbyport port x =
  return (Unix.getservbyport port x)

#else

external getservbyport_job : int -> string -> [ `unix_getservbyport ] job = "lwt_unix_getservbyport_job"
external getservbyport_result : [ `unix_getservbyport ] job -> Unix.service_entry = "lwt_unix_getservbyport_result"
external getservbyport_free : [ `unix_getservbyport ] job -> unit = "lwt_unix_getservbyport_free"

let getservbyport port x =
  execute_job (getservbyport_job port x) getservbyport_result getservbyport_free

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

external getaddrinfo_job : string -> string -> Unix.getaddrinfo_option list -> [ `unix_getaddrinfo ] job = "lwt_unix_getaddrinfo_job"
external getaddrinfo_result : [ `unix_getaddrinfo ] job -> Unix.addr_info list = "lwt_unix_getaddrinfo_result"
external getaddrinfo_free : [ `unix_getaddrinfo ] job -> unit = "lwt_unix_getaddrinfo_free"

let getaddrinfo host service opts =
  execute_job (getaddrinfo_job host service opts) getaddrinfo_result getaddrinfo_free

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

external getnameinfo_job : Unix.sockaddr -> Unix.getnameinfo_option list -> [ `unix_getnameinfo ] job = "lwt_unix_getnameinfo_job"
external getnameinfo_result : [ `unix_getnameinfo ] job -> Unix.name_info = "lwt_unix_getnameinfo_result"
external getnameinfo_free : [ `unix_getnameinfo ] job -> unit = "lwt_unix_getnameinfo_free"

let getnameinfo addr opts =
  execute_job (getnameinfo_job addr opts) getnameinfo_result getnameinfo_free

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

external tcgetattr_job : Unix.file_descr -> [ `unix_tcgetattr ] job = "lwt_unix_tcgetattr_job"
external tcgetattr_result : [ `unix_tcgetattr ] job -> Unix.terminal_io = "lwt_unix_tcgetattr_result"
external tcgetattr_free : [ `unix_tcgetattr ] job -> unit = "lwt_unix_tcgetattr_free"

let tcgetattr ch =
  check_descriptor ch;
  execute_job (tcgetattr_job ch.fd) tcgetattr_result tcgetattr_free

#endif

#if windows

let tcsetattr ch when_ attrs =
  check_descriptor ch;
  return (Unix.tcsetattr ch.fd when_ attrs)

#else

external tcsetattr_job : Unix.file_descr -> Unix.setattr_when -> Unix.terminal_io -> [ `unix_tcsetattr ] job = "lwt_unix_tcsetattr_job"
external tcsetattr_result : [ `unix_tcsetattr ] job -> unit = "lwt_unix_tcsetattr_result"
external tcsetattr_free : [ `unix_tcsetattr ] job -> unit = "lwt_unix_tcsetattr_free"

let tcsetattr ch when_ attrs =
  check_descriptor ch;
  execute_job (tcsetattr_job ch.fd when_ attrs) tcsetattr_result tcsetattr_free

#endif

#if windows

let tcsendbreak ch delay =
  check_descriptor ch;
  return (Unix.tcsendbreak ch.fd delay)

#else

external tcsendbreak_job : Unix.file_descr -> int -> [ `unix_tcsendbreak ] job = "lwt_unix_tcsendbreak_job"
external tcsendbreak_result : [ `unix_tcsendbreak ] job -> unit = "lwt_unix_tcsendbreak_result"
external tcsendbreak_free : [ `unix_tcsendbreak ] job -> unit = "lwt_unix_tcsendbreak_free"

let tcsendbreak ch delay =
  check_descriptor ch;
  execute_job (tcsendbreak_job ch.fd delay) tcsendbreak_result tcsendbreak_free

#endif

#if windows

let tcdrain ch =
  check_descriptor ch;
  return (Unix.tcdrain ch.fd)

#else

external tcdrain_job : Unix.file_descr -> [ `unix_tcdrain ] job = "lwt_unix_tcdrain_job"
external tcdrain_result : [ `unix_tcdrain ] job -> unit = "lwt_unix_tcdrain_result"
external tcdrain_free : [ `unix_tcdrain ] job -> unit = "lwt_unix_tcdrain_free"

let tcdrain ch =
  check_descriptor ch;
  execute_job (tcdrain_job ch.fd) tcdrain_result tcdrain_free

#endif

#if windows

let tcflush ch q =
  check_descriptor ch;
  return (Unix.tcflush ch.fd q)

#else

external tcflush_job : Unix.file_descr -> Unix.flush_queue -> [ `unix_tcflush ] job = "lwt_unix_tcflush_job"
external tcflush_result : [ `unix_tcflush ] job -> unit = "lwt_unix_tcflush_result"
external tcflush_free : [ `unix_tcflush ] job -> unit = "lwt_unix_tcflush_free"

let tcflush ch q =
  check_descriptor ch;
  execute_job (tcflush_job ch.fd q) tcflush_result tcflush_free

#endif

#if windows

let tcflow ch act =
  check_descriptor ch;
  return (Unix.tcflow ch.fd act)

#else

external tcflow_job : Unix.file_descr -> Unix.flow_action -> [ `unix_tcflow ] job = "lwt_unix_tcflow_job"
external tcflow_result : [ `unix_tcflow ] job -> unit = "lwt_unix_tcflow_result"
external tcflow_free : [ `unix_tcflow ] job -> unit = "lwt_unix_tcflow_free"

let tcflow ch act =
  check_descriptor ch;
  execute_job (tcflow_job ch.fd act) tcflow_result tcflow_free

#endif

(* +-----------------------------------------------------------------+
   | Reading notifications                                           |
   +-----------------------------------------------------------------+ *)

(* Buffer used to receive notifications: *)
let notification_buffer = String.create 4

external init_notification : unit -> Unix.file_descr = "lwt_unix_init_notification"
external send_notification : int -> unit = "lwt_unix_send_notification_stub"
external recv_notifications : unit -> int array = "lwt_unix_recv_notifications"

let handle_notification id =
  match try Some(Notifiers.find notifiers id) with Not_found -> None with
    | Some notifier ->
        if notifier.notify_once then
          stop_notification id;
        notifier.notify_handler ()
    | None ->
        ()

let rec handle_notifications ev =
  (* Process available notifications. *)
  Array.iter handle_notification (recv_notifications ())

let event_notifications = ref (Lwt_engine.on_readable (init_notification ()) handle_notifications)

(* +-----------------------------------------------------------------+
   | Signals                                                         |
   +-----------------------------------------------------------------+ *)

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
         Sys.set_signal signum (Sys.Signal_handle (fun signum -> send_notification notification))
       with exn ->
         stop_notification notification;
         raise exn);
      signals := Signal_map.add signum (notification, actions) !signals;
      (notification, actions)
  in
  let node = Lwt_sequence.add_r handler actions in
  lazy(Lwt_sequence.remove node;
       if Lwt_sequence.is_empty actions then begin
         stop_notification notification;
         Sys.set_signal signum Sys.Signal_default
       end)

let disable_signal_handler = Lazy.force

(* +-----------------------------------------------------------------+
   | Processes                                                       |
   +-----------------------------------------------------------------+ *)

let fork () =
  match Unix.fork () with
    | 0 ->
        Lwt_engine.stop_event !event_notifications;
        (* Reinitialise the notification system. *)
        event_notifications := Lwt_engine.on_readable (init_notification ()) handle_notifications;
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
