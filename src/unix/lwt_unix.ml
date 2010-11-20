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
external ev_fake_io : Unix.file_descr -> unit = "lwt_libev_fake_io"

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
      | Async_detach | Async_switch ->
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
}

external guess_blocking_job : Unix.file_descr -> [ `unix_guess_blocking ] job = "lwt_unix_guess_blocking_job"
external guess_blocking_result : [ `unix_guess_blocking ] job -> bool = "lwt_unix_guess_blocking_result" "noalloc"
external guess_blocking_free : [ `unix_guess_blocking ] job -> unit = "lwt_unix_guess_blocking_free" "noalloc"

let guess_blocking fd =
  if windows_hack then
    return (match (Unix.fstat fd).Unix.st_kind with
              | Unix.S_SOCK | Unix.S_FIFO -> false
              | _ -> true)
  else
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

let mk_ch ?blocking ?(set_flags=true) fd =
  { fd = fd;
    state = Opened;
    set_flags = set_flags;
    blocking = is_blocking ?blocking ~set_flags fd }

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

let set_blocking ?set_flags ch blocking =
  check_descriptor ch;
  ch.blocking <- is_blocking ~blocking ?set_flags ch.fd

external stub_readable : Unix.file_descr -> bool = "lwt_unix_readable" "noalloc"
external stub_writable : Unix.file_descr -> bool = "lwt_unix_writable" "noalloc"

let readable ch =
  check_descriptor ch;
  stub_readable ch.fd

let writable ch =
  check_descriptor ch;
  stub_writable ch.fd

let set_state ch st =
  ch.state <- st

let abort ch e =
  if ch.state <> Closed then begin
    set_state ch (Aborted e);
    ev_fake_io ch.fd
  end

let unix_file_descr ch = ch.fd

let of_unix_file_descr = mk_ch

let stdin = of_unix_file_descr ~set_flags:false ~blocking:true Unix.stdin
let stdout = of_unix_file_descr ~set_flags:false ~blocking:true Unix.stdout
let stderr = of_unix_file_descr ~set_flags:false ~blocking:true Unix.stderr

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
    lwt blocking = Lazy.force ch.blocking in
    if not blocking || (event = Read && stub_readable ch.fd) || (event == Write && stub_writable ch.fd) then
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

external open_job : string -> Unix.open_flag list -> int -> [ `unix_open ] job = "lwt_unix_open_job"
external open_result : [ `unix_open ] job -> Unix.file_descr * bool = "lwt_unix_open_result"
external open_free : [ `unix_open ] job -> unit = "lwt_unix_open_free" "noalloc"

let openfile name flags perms =
  if windows_hack then
    return (of_unix_file_descr (Unix.openfile name flags perms))
  else
    lwt fd, blocking =
      execute_job
        (open_job name flags perms)
        open_result
        open_free
    in
    return (of_unix_file_descr ~blocking fd)

external close_job : Unix.file_descr -> [ `unix_close ] job = "lwt_unix_close_job"
external close_result : [ `unix_close ] job -> unit = "lwt_unix_close_result"
external close_free : [ `unix_close ] job -> unit = "lwt_unix_close_free" "noalloc"

let close ch =
  if ch.state = Closed then check_descriptor ch;
  set_state ch Closed;
  if windows_hack then begin
    Unix.close ch.fd;
    return ()
  end else
    execute_job (close_job ch.fd) close_result close_free

let wait_read ch =
  try_lwt
    if readable ch then
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
  end else
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
external write_result : [ `unix_write ] job -> int = "lwt_unix_write_result" "noalloc"
external write_free : [ `unix_write ] job -> unit = "lwt_unix_write_free" "noalloc"

let write ch buf pos len =
  if pos < 0 || len < 0 || pos > String.length buf - len then
    invalid_arg "Lwt_unix.write"
  else if windows_hack then begin
    check_descriptor ch;
    register_action Read ch (fun () -> Unix.write ch.fd buf pos len)
  end else
    Lazy.force ch.blocking >>= function
      | true ->
          lwt () = wait_write ch in
          execute_job (write_job ch.fd buf pos len) write_result write_free
      | false ->
          wrap_syscall Write ch (fun () -> stub_write ch.fd buf pos len)

(* +-----------------------------------------------------------------+
   | Seeking and truncating                                          |
   +-----------------------------------------------------------------+ *)

external lseek_job : Unix.file_descr -> int -> Unix.seek_command -> [ `unix_lseek ] job = "lwt_unix_lseek_job"
external lseek_result : [ `unix_lseek ] job -> int = "lwt_unix_lseek_result"
external lseek_free : [ `unix_lseek ] job -> unit = "lwt_unix_lseek_free"

let lseek ch offset whence =
  check_descriptor ch;
  if windows_hack then
    return (Unix.lseek ch.fd offset whence)
  else
    execute_job (lseek_job ch.fd offset whence) lseek_result lseek_free

external truncate_job : string -> int -> [ `unix_truncate ] job = "lwt_unix_truncate_job"
external truncate_result : [ `unix_truncate ] job -> unit = "lwt_unix_truncate_result"
external truncate_free : [ `unix_truncate ] job -> unit = "lwt_unix_truncate_free"

let truncate name offset =
  if windows_hack then
    return (Unix.truncate name offset)
  else
    execute_job (truncate_job name offset) truncate_result truncate_free

external ftruncate_job : Unix.file_descr -> int -> [ `unix_ftruncate ] job = "lwt_unix_ftruncate_job"
external ftruncate_result : [ `unix_ftruncate ] job -> unit = "lwt_unix_ftruncate_result"
external ftruncate_free : [ `unix_ftruncate ] job -> unit = "lwt_unix_ftruncate_free"

let ftruncate ch offset =
  check_descriptor ch;
  if windows_hack then
    return (Unix.ftruncate ch.fd offset)
  else
    execute_job (ftruncate_job ch.fd offset) ftruncate_result ftruncate_free

(* +-----------------------------------------------------------------+
   | File status                                                     |
   +-----------------------------------------------------------------+ *)

external stat_job : string -> [ `unix_stat ] job = "lwt_unix_stat_job"
external stat_result : [ `unix_stat ] job -> Unix.stats = "lwt_unix_stat_result"
external stat_free : [ `unix_stat ] job -> unit = "lwt_unix_stat_free"

let stat name =
  if windows_hack then
    return (Unix.stat name)
  else
    execute_job (stat_job name) stat_result stat_free

external lstat_job : string -> [ `unix_lstat ] job = "lwt_unix_lstat_job"
external lstat_result : [ `unix_lstat ] job -> Unix.stats = "lwt_unix_lstat_result"
external lstat_free : [ `unix_lstat ] job -> unit = "lwt_unix_lstat_free"

let lstat name =
  if windows_hack then
    return (Unix.lstat name)
  else
    execute_job (lstat_job name) lstat_result lstat_free

external fstat_job : Unix.file_descr -> [ `unix_fstat ] job = "lwt_unix_fstat_job"
external fstat_result : [ `unix_fstat ] job -> Unix.stats = "lwt_unix_fstat_result"
external fstat_free : [ `unix_fstat ] job -> unit = "lwt_unix_fstat_free"

let fstat ch =
  check_descriptor ch;
  if windows_hack then
    return (Unix.fstat ch.fd)
  else
    execute_job (fstat_job ch.fd) fstat_result fstat_free

external isatty_job : Unix.file_descr -> [ `unix_isatty ] job = "lwt_unix_isatty_job"
external isatty_result : [ `unix_isatty ] job -> bool = "lwt_unix_isatty_result"
external isatty_free : [ `unix_isatty ] job -> unit = "lwt_unix_isatty_free"

let isatty ch =
  check_descriptor ch;
  if windows_hack then
    return (Unix.isatty ch.fd)
  else
    execute_job (isatty_job ch.fd) isatty_result isatty_free

(* +-----------------------------------------------------------------+
   | File operations on large files                                  |
   +-----------------------------------------------------------------+ *)

module LargeFile =
struct
  external lseek_job : Unix.file_descr -> int64 -> Unix.seek_command -> [ `unix_lseek ] job = "lwt_unix_lseek_64_job"
  external lseek_result : [ `unix_lseek ] job -> int64 = "lwt_unix_lseek_64_result"
  external lseek_free : [ `unix_lseek ] job -> unit = "lwt_unix_lseek_64_free"

  let lseek ch offset whence =
    check_descriptor ch;
    if windows_hack then
      return (Unix.LargeFile.lseek ch.fd offset whence)
    else
      execute_job (lseek_job ch.fd offset whence) lseek_result lseek_free

  external truncate_job : string -> int64 -> [ `unix_truncate ] job = "lwt_unix_truncate_64_job"
  external truncate_result : [ `unix_truncate ] job -> unit = "lwt_unix_truncate_64_result"
  external truncate_free : [ `unix_truncate ] job -> unit = "lwt_unix_truncate_64_free"

  let truncate name offset =
    if windows_hack then
      return (Unix.LargeFile.truncate name offset)
    else
      execute_job (truncate_job name offset) truncate_result truncate_free

  external ftruncate_job : Unix.file_descr -> int64 -> [ `unix_ftruncate ] job = "lwt_unix_ftruncate_64_job"
  external ftruncate_result : [ `unix_ftruncate ] job -> unit = "lwt_unix_ftruncate_64_result"
  external ftruncate_free : [ `unix_ftruncate ] job -> unit = "lwt_unix_ftruncate_64_free"

  let ftruncate ch offset =
    check_descriptor ch;
    if windows_hack then
      return (Unix.LargeFile.ftruncate ch.fd offset)
    else
      execute_job (ftruncate_job ch.fd offset) ftruncate_result ftruncate_free


  external stat_job : string -> [ `unix_stat ] job = "lwt_unix_stat_64_job"
  external stat_result : [ `unix_stat ] job -> Unix.LargeFile.stats = "lwt_unix_stat_64_result"
  external stat_free : [ `unix_stat ] job -> unit = "lwt_unix_stat_64_free"

  let stat name =
    if windows_hack then
      return (Unix.LargeFile.stat name)
    else
      execute_job (stat_job name) stat_result stat_free

  external lstat_job : string -> [ `unix_lstat ] job = "lwt_unix_lstat_64_job"
  external lstat_result : [ `unix_lstat ] job -> Unix.LargeFile.stats = "lwt_unix_lstat_64_result"
  external lstat_free : [ `unix_lstat ] job -> unit = "lwt_unix_lstat_64_free"

  let lstat name =
    if windows_hack then
      return (Unix.LargeFile.lstat name)
    else
      execute_job (lstat_job name) lstat_result lstat_free

  external fstat_job : Unix.file_descr -> [ `unix_fstat ] job = "lwt_unix_fstat_64_job"
  external fstat_result : [ `unix_fstat ] job -> Unix.LargeFile.stats = "lwt_unix_fstat_64_result"
  external fstat_free : [ `unix_fstat ] job -> unit = "lwt_unix_fstat_64_free"

  let fstat ch =
    check_descriptor ch;
    if windows_hack then
      return (Unix.LargeFile.fstat ch.fd)
    else
      execute_job (fstat_job ch.fd) fstat_result fstat_free
end

(* +-----------------------------------------------------------------+
   | Operations on file names                                        |
   +-----------------------------------------------------------------+ *)

external unlink_job : string -> [ `unix_unlink ] job = "lwt_unix_unlink_job"
external unlink_result : [ `unix_unlink ] job -> unit = "lwt_unix_unlink_result"
external unlink_free : [ `unix_unlink ] job -> unit = "lwt_unix_unlink_free"

let unlink name =
  if windows_hack then
    return (Unix.unlink name)
  else
    execute_job (unlink_job name) unlink_result unlink_free

external rename_job : string -> string -> [ `unix_rename ] job = "lwt_unix_rename_job"
external rename_result : [ `unix_rename ] job -> unit = "lwt_unix_rename_result"
external rename_free : [ `unix_rename ] job -> unit = "lwt_unix_rename_free"

let rename name1 name2 =
  if windows_hack then
    return (Unix.rename name1 name2)
  else
    execute_job (rename_job name1 name2) rename_result rename_free

external link_job : string -> string -> [ `unix_link ] job = "lwt_unix_link_job"
external link_result : [ `unix_link ] job -> unit = "lwt_unix_link_result"
external link_free : [ `unix_link ] job -> unit = "lwt_unix_link_free"

let link name1 name2 =
  if windows_hack then
    return (Unix.link name1 name2)
  else
    execute_job (link_job name1 name2) link_result link_free

(* +-----------------------------------------------------------------+
   | File permissions and ownership                                  |
   +-----------------------------------------------------------------+ *)

external chmod_job : string -> Unix.file_perm -> [ `unix_chmod ] job = "lwt_unix_chmod_job"
external chmod_result : [ `unix_chmod ] job -> unit = "lwt_unix_chmod_result"
external chmod_free : [ `unix_chmod ] job -> unit = "lwt_unix_chmod_free"

let chmod name perms =
  if windows_hack then
    return (Unix.chmod name perms)
  else
    execute_job (chmod_job name perms) chmod_result chmod_free

external fchmod_job : Unix.file_descr -> Unix.file_perm -> [ `unix_fchmod ] job = "lwt_unix_fchmod_job"
external fchmod_result : [ `unix_fchmod ] job -> unit = "lwt_unix_fchmod_result"
external fchmod_free : [ `unix_fchmod ] job -> unit = "lwt_unix_fchmod_free"

let fchmod ch perms =
  check_descriptor ch;
  if windows_hack then
    return (Unix.fchmod ch.fd perms)
  else
    execute_job (fchmod_job ch.fd perms) fchmod_result fchmod_free

external chown_job : string -> int -> int -> [ `unix_chown ] job = "lwt_unix_chown_job"
external chown_result : [ `unix_chown ] job -> unit = "lwt_unix_chown_result"
external chown_free : [ `unix_chown ] job -> unit = "lwt_unix_chown_free"

let chown name uid gid =
  if windows_hack then
    return (Unix.chown name uid gid)
  else
    execute_job (chown_job name uid gid) chown_result chown_free

external fchown_job : Unix.file_descr -> int -> int -> [ `unix_fchown ] job = "lwt_unix_fchown_job"
external fchown_result : [ `unix_fchown ] job -> unit = "lwt_unix_fchown_result"
external fchown_free : [ `unix_fchown ] job -> unit = "lwt_unix_fchown_free"

let fchown ch uid gid =
  check_descriptor ch;
  if windows_hack then
    return (Unix.fchown ch.fd uid gid)
  else
    execute_job (fchown_job ch.fd uid gid) fchown_result fchown_free

external access_job : string -> Unix.access_permission list -> [ `unix_access ] job = "lwt_unix_access_job"
external access_result : [ `unix_access ] job -> unit = "lwt_unix_access_result"
external access_free : [ `unix_access ] job -> unit = "lwt_unix_access_free"

let access name perms =
  if windows_hack then
    return (Unix.access name perms)
  else
    execute_job (access_job name perms) access_result access_free

(* +-----------------------------------------------------------------+
   | Operations on file descriptors                                  |
   +-----------------------------------------------------------------+ *)

let dup ch =
  check_descriptor ch;
  let fd = Unix.dup ch.fd in
  { fd = fd;
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
        ch.blocking }

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

external mkdir_job : string -> Unix.file_perm -> [ `unix_mkdir ] job = "lwt_unix_mkdir_job"
external mkdir_result : [ `unix_mkdir ] job -> unit = "lwt_unix_mkdir_result"
external mkdir_free : [ `unix_mkdir ] job -> unit = "lwt_unix_mkdir_free"

let mkdir name perms =
  if windows_hack then
    return (Unix.mkdir name perms)
  else
    execute_job (mkdir_job name perms) mkdir_result mkdir_free

external rmdir_job : string -> [ `unix_rmdir ] job = "lwt_unix_rmdir_job"
external rmdir_result : [ `unix_rmdir ] job -> unit = "lwt_unix_rmdir_result"
external rmdir_free : [ `unix_rmdir ] job -> unit = "lwt_unix_rmdir_free"

let rmdir name =
  if windows_hack then
    return (Unix.rmdir name)
  else
    execute_job (rmdir_job name) rmdir_result rmdir_free

external chdir_job : string -> [ `unix_chdir ] job = "lwt_unix_chdir_job"
external chdir_result : [ `unix_chdir ] job -> unit = "lwt_unix_chdir_result"
external chdir_free : [ `unix_chdir ] job -> unit = "lwt_unix_chdir_free"

let chdir name =
  if windows_hack then
    return (Unix.chdir name)
  else
    execute_job (chdir_job name) chdir_result chdir_free

external chroot_job : string -> [ `unix_chroot ] job = "lwt_unix_chroot_job"
external chroot_result : [ `unix_chroot ] job -> unit = "lwt_unix_chroot_result"
external chroot_free : [ `unix_chroot ] job -> unit = "lwt_unix_chroot_free"

let chroot name =
  if windows_hack then
    return (Unix.chroot name)
  else
    execute_job (chroot_job name) chroot_result chroot_free

external opendir_job : string -> [ `unix_opendir ] job = "lwt_unix_opendir_job"
external opendir_result : [ `unix_opendir ] job -> Unix.dir_handle = "lwt_unix_opendir_result"
external opendir_free : [ `unix_opendir ] job -> unit = "lwt_unix_opendir_free"

let opendir name =
  if windows_hack then
    return (Unix.opendir name)
  else
    execute_job (opendir_job name) opendir_result opendir_free

external readdir_job : Unix.dir_handle -> [ `unix_readdir ] job = "lwt_unix_readdir_job"
external readdir_result : [ `unix_readdir ] job -> string = "lwt_unix_readdir_result"
external readdir_free : [ `unix_readdir ] job -> unit = "lwt_unix_readdir_free"

let readdir handle =
  if windows_hack then
    return (Unix.readdir handle)
  else
    execute_job (readdir_job handle) readdir_result readdir_free

external rewinddir_job : Unix.dir_handle -> [ `unix_rewinddir ] job = "lwt_unix_rewinddir_job"
external rewinddir_result : [ `unix_rewinddir ] job -> unit = "lwt_unix_rewinddir_result"
external rewinddir_free : [ `unix_rewinddir ] job -> unit = "lwt_unix_rewinddir_free"

let rewinddir handle =
  if windows_hack then
    return (Unix.rewinddir handle)
  else
    execute_job (rewinddir_job handle) rewinddir_result rewinddir_free

external closedir_job : Unix.dir_handle -> [ `unix_closedir ] job = "lwt_unix_closedir_job"
external closedir_result : [ `unix_closedir ] job -> unit = "lwt_unix_closedir_result"
external closedir_free : [ `unix_closedir ] job -> unit = "lwt_unix_closedir_free"

let closedir handle =
  if windows_hack then
    return (Unix.closedir handle)
  else
    execute_job (closedir_job handle) closedir_result closedir_free

(* +-----------------------------------------------------------------+
   | Pipes and redirections                                          |
   +-----------------------------------------------------------------+ *)

let pipe () =
  let (out_fd, in_fd) = Unix.pipe() in
  (mk_ch ~blocking:false out_fd, mk_ch ~blocking:false in_fd)

let pipe_in () =
  let (out_fd, in_fd) = Unix.pipe() in
  (mk_ch ~blocking:false out_fd, in_fd)

let pipe_out () =
  let (out_fd, in_fd) = Unix.pipe() in
  (out_fd, mk_ch ~blocking:false in_fd)

external mkfifo_job : string -> Unix.file_perm -> [ `unix_mkfifo ] job = "lwt_unix_mkfifo_job"
external mkfifo_result : [ `unix_mkfifo ] job -> unit = "lwt_unix_mkfifo_result"
external mkfifo_free : [ `unix_mkfifo ] job -> unit = "lwt_unix_mkfifo_free"

let mkfifo name perms =
  if windows_hack then
    return (Unix.mkfifo name perms)
  else
    execute_job (mkfifo_job name perms) mkfifo_result mkfifo_free

(* +-----------------------------------------------------------------+
   | Symbolic links                                                  |
   +-----------------------------------------------------------------+ *)

external symlink_job : string -> string -> [ `unix_symlink ] job = "lwt_unix_symlink_job"
external symlink_result : [ `unix_symlink ] job -> unit = "lwt_unix_symlink_result"
external symlink_free : [ `unix_symlink ] job -> unit = "lwt_unix_symlink_free"

let symlink name1 name2 =
  if windows_hack then
    return (Unix.symlink name1 name2)
  else
    execute_job (symlink_job name1 name2) symlink_result symlink_free

external readlink_job : string -> [ `unix_readlink ] job = "lwt_unix_readlink_job"
external readlink_result : [ `unix_readlink ] job -> string = "lwt_unix_readlink_result"
external readlink_free : [ `unix_readlink ] job -> unit = "lwt_unix_readlink_free"

let readlink name =
  if windows_hack then
    return (Unix.readlink name)
  else
    execute_job (readlink_job name) readlink_result readlink_free

(* +-----------------------------------------------------------------+
   | Locking                                                         |
   +-----------------------------------------------------------------+ *)

external lockf_job : Unix.file_descr -> Unix.lock_command -> int -> [ `unix_lockf ] job = "lwt_unix_lockf_job"
external lockf_result : [ `unix_lockf ] job -> unit = "lwt_unix_lockf_result"
external lockf_free : [ `unix_lockf ] job -> unit = "lwt_unix_lockf_free"

let lockf ch cmd size =
  check_descriptor ch;
  if windows_hack then
    return (Unix.lockf ch.fd cmd size)
  else
    execute_job (lockf_job ch.fd cmd size) lockf_result lockf_free

(* +-----------------------------------------------------------------+
   | User id, group id                                               |
   +-----------------------------------------------------------------+ *)

external getlogin_job : unit -> [ `unix_getlogin ] job = "lwt_unix_getlogin_job"
external getlogin_result : [ `unix_getlogin ] job -> string = "lwt_unix_getlogin_result"
external getlogin_free : [ `unix_getlogin ] job -> unit = "lwt_unix_getlogin_free"

let getlogin () =
  if windows_hack then
    return (Unix.getlogin ())
  else
    execute_job (getlogin_job ()) getlogin_result getlogin_free

external getpwnam_job : string -> [ `unix_getpwnam ] job = "lwt_unix_getpwnam_job"
external getpwnam_result : [ `unix_getpwnam ] job -> Unix.passwd_entry = "lwt_unix_getpwnam_result"
external getpwnam_free : [ `unix_getpwnam ] job -> unit = "lwt_unix_getpwnam_free"

let getpwnam name =
  if windows_hack then
    return (Unix.getpwnam name)
  else
    execute_job (getpwnam_job name) getpwnam_result getpwnam_free

external getgrnam_job : string -> [ `unix_getgrnam ] job = "lwt_unix_getgrnam_job"
external getgrnam_result : [ `unix_getgrnam ] job -> Unix.group_entry = "lwt_unix_getgrnam_result"
external getgrnam_free : [ `unix_getgrnam ] job -> unit = "lwt_unix_getgrnam_free"

let getgrnam name =
  if windows_hack then
    return (Unix.getgrnam name)
  else
    execute_job (getgrnam_job name) getgrnam_result getgrnam_free

external getpwuid_job : int -> [ `unix_getpwuid ] job = "lwt_unix_getpwuid_job"
external getpwuid_result : [ `unix_getpwuid ] job -> Unix.passwd_entry = "lwt_unix_getpwuid_result"
external getpwuid_free : [ `unix_getpwuid ] job -> unit = "lwt_unix_getpwuid_free"

let getpwuid uid =
  if windows_hack then
    return (Unix.getpwuid uid)
  else
    execute_job (getpwuid_job uid) getpwuid_result getpwuid_free

external getgrgid_job : int -> [ `unix_getgrgid ] job = "lwt_unix_getgrgid_job"
external getgrgid_result : [ `unix_getgrgid ] job -> Unix.group_entry = "lwt_unix_getgrgid_result"
external getgrgid_free : [ `unix_getgrgid ] job -> unit = "lwt_unix_getgrgid_free"

let getgrgid gid =
  if windows_hack then
    return (Unix.getgrgid gid)
  else
    execute_job (getgrgid_job gid) getgrgid_result getgrgid_free

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

let check_io_vectors func_name iovs =
  List.iter (fun iov ->
               if iov.iov_offset < 0
                 || iov.iov_length < 0
                 || iov.iov_offset > String.length iov.iov_buffer - iov.iov_length then
                   invalid_arg func_name) iovs

external stub_recv_msg : Unix.file_descr -> int -> io_vector list -> int * Unix.file_descr list = "lwt_unix_recv_msg"

let recv_msg ~socket ~io_vectors =
  check_io_vectors "Lwt_unix.recv_msg" io_vectors;
  let n_iovs = List.length io_vectors in
  wrap_syscall Read socket
    (fun () ->
       stub_recv_msg socket.fd n_iovs io_vectors)

external stub_send_msg : Unix.file_descr -> int -> io_vector list -> int -> Unix.file_descr list -> int = "lwt_unix_send_msg"

let send_msg ~socket ~io_vectors ~fds =
  check_io_vectors "Lwt_unix.send_msg" io_vectors;
  let n_iovs = List.length io_vectors and n_fds = List.length fds in
  wrap_syscall Write socket
    (fun () ->
       stub_send_msg socket.fd n_iovs io_vectors n_fds fds)

let socket dom typ proto =
  let s = Unix.socket dom typ proto in
  mk_ch ~blocking:false s

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

external stub_get_credentials : Unix.file_descr -> credentials = "lwt_unix_get_credentials"

let get_credentials ch = stub_get_credentials ch.fd

(* +-----------------------------------------------------------------+
   | Socket options                                                  |
   +-----------------------------------------------------------------+ *)

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

external gethostname_job : unit -> [ `unix_gethostname ] job = "lwt_unix_gethostname_job"
external gethostname_result : [ `unix_gethostname ] job -> string = "lwt_unix_gethostname_result"
external gethostname_free : [ `unix_gethostname ] job -> unit = "lwt_unix_gethostname_free"

let gethostname () =
  if windows_hack then
    return (Unix.gethostname ())
  else
    execute_job (gethostname_job ()) gethostname_result gethostname_free

external gethostbyname_job : string -> [ `unix_gethostbyname ] job = "lwt_unix_gethostbyname_job"
external gethostbyname_result : [ `unix_gethostbyname ] job -> Unix.host_entry = "lwt_unix_gethostbyname_result"
external gethostbyname_free : [ `unix_gethostbyname ] job -> unit = "lwt_unix_gethostbyname_free"

let gethostbyname name =
  if windows_hack then
    return (Unix.gethostbyname name)
  else
    execute_job (gethostbyname_job name) gethostbyname_result gethostbyname_free

external gethostbyaddr_job : Unix.inet_addr -> [ `unix_gethostbyaddr ] job = "lwt_unix_gethostbyaddr_job"
external gethostbyaddr_result : [ `unix_gethostbyaddr ] job -> Unix.host_entry = "lwt_unix_gethostbyaddr_result"
external gethostbyaddr_free : [ `unix_gethostbyaddr ] job -> unit = "lwt_unix_gethostbyaddr_free"

let gethostbyaddr addr =
  if windows_hack then
    return (Unix.gethostbyaddr addr)
  else
    execute_job (gethostbyaddr_job addr) gethostbyaddr_result gethostbyaddr_free

external getprotobyname_job : string -> [ `unix_getprotobyname ] job = "lwt_unix_getprotobyname_job"
external getprotobyname_result : [ `unix_getprotobyname ] job -> Unix.protocol_entry = "lwt_unix_getprotobyname_result"
external getprotobyname_free : [ `unix_getprotobyname ] job -> unit = "lwt_unix_getprotobyname_free"

let getprotobyname name =
  if windows_hack then
    return (Unix.getprotobyname name)
  else
    execute_job (getprotobyname_job name) getprotobyname_result getprotobyname_free

external getprotobynumber_job : int -> [ `unix_getprotobynumber ] job = "lwt_unix_getprotobynumber_job"
external getprotobynumber_result : [ `unix_getprotobynumber ] job -> Unix.protocol_entry = "lwt_unix_getprotobynumber_result"
external getprotobynumber_free : [ `unix_getprotobynumber ] job -> unit = "lwt_unix_getprotobynumber_free"

let getprotobynumber number =
  if windows_hack then
    return (Unix.getprotobynumber number)
  else
    execute_job (getprotobynumber_job number) getprotobynumber_result getprotobynumber_free

external getservbyname_job : string -> string -> [ `unix_getservbyname ] job = "lwt_unix_getservbyname_job"
external getservbyname_result : [ `unix_getservbyname ] job -> Unix.service_entry = "lwt_unix_getservbyname_result"
external getservbyname_free : [ `unix_getservbyname ] job -> unit = "lwt_unix_getservbyname_free"

let getservbyname name x =
  if windows_hack then
    return (Unix.getservbyname name x)
  else
    execute_job (getservbyname_job name x) getservbyname_result getservbyname_free

external getservbyport_job : int -> string -> [ `unix_getservbyport ] job = "lwt_unix_getservbyport_job"
external getservbyport_result : [ `unix_getservbyport ] job -> Unix.service_entry = "lwt_unix_getservbyport_result"
external getservbyport_free : [ `unix_getservbyport ] job -> unit = "lwt_unix_getservbyport_free"

let getservbyport port x =
  if windows_hack then
    return (Unix.getservbyport port x)
  else
    execute_job (getservbyport_job port x) getservbyport_result getservbyport_free

external getaddrinfo_job : string -> string -> Unix.getaddrinfo_option list -> [ `unix_getaddrinfo ] job = "lwt_unix_getaddrinfo_job"
external getaddrinfo_result : [ `unix_getaddrinfo ] job -> Unix.addr_info list = "lwt_unix_getaddrinfo_result"
external getaddrinfo_free : [ `unix_getaddrinfo ] job -> unit = "lwt_unix_getaddrinfo_free"

let getaddrinfo host service opts =
  if windows_hack then
    return (Unix.getaddrinfo host service opts)
  else
    execute_job (getaddrinfo_job host service opts) getaddrinfo_result getaddrinfo_free

external getnameinfo_job : Unix.sockaddr -> Unix.getnameinfo_option list -> [ `unix_getnameinfo ] job = "lwt_unix_getnameinfo_job"
external getnameinfo_result : [ `unix_getnameinfo ] job -> Unix.name_info = "lwt_unix_getnameinfo_result"
external getnameinfo_free : [ `unix_getnameinfo ] job -> unit = "lwt_unix_getnameinfo_free"

let getnameinfo addr opts =
  if windows_hack then
    return (Unix.getnameinfo addr opts)
  else
    execute_job (getnameinfo_job addr opts) getnameinfo_result getnameinfo_free

(* +-----------------------------------------------------------------+
   | Terminal interface                                              |
   +-----------------------------------------------------------------+ *)

external tcgetattr_job : Unix.file_descr -> [ `unix_tcgetattr ] job = "lwt_unix_tcgetattr_job"
external tcgetattr_result : [ `unix_tcgetattr ] job -> Unix.terminal_io = "lwt_unix_tcgetattr_result"
external tcgetattr_free : [ `unix_tcgetattr ] job -> unit = "lwt_unix_tcgetattr_free"

let tcgetattr ch =
  check_descriptor ch;
  if windows_hack then
    return (Unix.tcgetattr ch.fd)
  else
    execute_job (tcgetattr_job ch.fd) tcgetattr_result tcgetattr_free

external tcsetattr_job : Unix.file_descr -> Unix.setattr_when -> Unix.terminal_io -> [ `unix_tcsetattr ] job = "lwt_unix_tcsetattr_job"
external tcsetattr_result : [ `unix_tcsetattr ] job -> unit = "lwt_unix_tcsetattr_result"
external tcsetattr_free : [ `unix_tcsetattr ] job -> unit = "lwt_unix_tcsetattr_free"

let tcsetattr ch when_ attrs =
  check_descriptor ch;
  if windows_hack then
    return (Unix.tcsetattr ch.fd when_ attrs)
  else
    execute_job (tcsetattr_job ch.fd when_ attrs) tcsetattr_result tcsetattr_free

external tcsendbreak_job : Unix.file_descr -> int -> [ `unix_tcsendbreak ] job = "lwt_unix_tcsendbreak_job"
external tcsendbreak_result : [ `unix_tcsendbreak ] job -> unit = "lwt_unix_tcsendbreak_result"
external tcsendbreak_free : [ `unix_tcsendbreak ] job -> unit = "lwt_unix_tcsendbreak_free"

let tcsendbreak ch delay =
  check_descriptor ch;
  if windows_hack then
    return (Unix.tcsendbreak ch.fd delay)
  else
    execute_job (tcsendbreak_job ch.fd delay) tcsendbreak_result tcsendbreak_free

external tcdrain_job : Unix.file_descr -> [ `unix_tcdrain ] job = "lwt_unix_tcdrain_job"
external tcdrain_result : [ `unix_tcdrain ] job -> unit = "lwt_unix_tcdrain_result"
external tcdrain_free : [ `unix_tcdrain ] job -> unit = "lwt_unix_tcdrain_free"

let tcdrain ch =
  check_descriptor ch;
  if windows_hack then
    return (Unix.tcdrain ch.fd)
  else
    execute_job (tcdrain_job ch.fd) tcdrain_result tcdrain_free

external tcflush_job : Unix.file_descr -> Unix.flush_queue -> [ `unix_tcflush ] job = "lwt_unix_tcflush_job"
external tcflush_result : [ `unix_tcflush ] job -> unit = "lwt_unix_tcflush_result"
external tcflush_free : [ `unix_tcflush ] job -> unit = "lwt_unix_tcflush_free"

let tcflush ch q =
  check_descriptor ch;
  if windows_hack then
    return (Unix.tcflush ch.fd q)
  else
    execute_job (tcflush_job ch.fd q) tcflush_result tcflush_free

external tcflow_job : Unix.file_descr -> Unix.flow_action -> [ `unix_tcflow ] job = "lwt_unix_tcflow_job"
external tcflow_result : [ `unix_tcflow ] job -> unit = "lwt_unix_tcflow_result"
external tcflow_free : [ `unix_tcflow ] job -> unit = "lwt_unix_tcflow_free"

let tcflow ch act =
  check_descriptor ch;
  if windows_hack then
    return (Unix.tcflow ch.fd act)
  else
    execute_job (tcflow_job ch.fd act) tcflow_result tcflow_free

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

external pool_size : unit -> int = "lwt_unix_pool_size"
