(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Interface Lwt_unix
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

(** Cooperative system calls *)

(** This modules redefine system calls, as in the [Unix] module of the
    standard library, but mapped into cooperative ones, which will not
    block the program, letting other threads run.

    The semantic of all operations is the following: if the action
    (for example reading from a {b file descriptor}) can be performed
    immediatly, it is done and returns immediatly, otherwise it
    returns a sleeping threads which is waked up when the operation
    completes.

    Moreover all sleeping threads returned by function of this modules
    are {b cancelable}, this means that you can cancel them with
    {!Lwt.cancel}. For example if you want to read something from a {b
    file descriptor} with a timeout, you can cancel the action after
    the timeout and the reading will not be performed if not already
    done.

    More precisely, assuming that you have two {b file descriptor}
    [fd1] and [fd2] and you want to read something from [fd1] or
    exclusively from [fd2], and fail with an exception if a timeout of
    1 second expires, without reading anything from [fd1] and [fd2],
    even if they become readable in the future.

    Then you can do:

    {[
      Lwt.select [Lwt_unix.timeout 1.0; read fd1 buf1 ofs1 len1; read fd2 buf2 ofs2 len2]
    ]}

    In this case it is guaranteed that exactly one of the three
    operations will completes, and other will just be cancelled.
*)

val handle_unix_error : ('a -> 'b Lwt.t) -> 'a -> 'b Lwt.t
  (** Same as [Unix.handle_unix_error] but catches lwt-level
      exceptions *)

(** {6 Sleeping} *)

val sleep : float -> unit Lwt.t
  (** [sleep d] is a threads which remain suspended for [d] seconds
      and then terminates. *)

val yield : unit -> unit Lwt.t
  (** [yield ()] is a threads which suspends itself and then resumes
      as soon as possible and terminates. *)

val auto_yield : float -> (unit -> unit Lwt.t)
  (** [auto_yield timeout] returns a function [f] which will yield
      every [timeout] seconds. *)

exception Timeout
  (** Exception raised by timeout operations *)

val timeout : float -> 'a Lwt.t
  (** [timeout d] is a threads which remain suspended for [d] seconds
      then fail with {!Timeout} *)

val with_timeout : float -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [with_timeout d f] is a short-hand for:

      {[
        Lwt.select [Lwt_unix.timeout d; f ()]
      ]}
  *)

(** {6 Operation on file-descriptors} *)

type file_descr
  (** The abstract type for {b file descriptor}s. A Lwt {b file
      descriptor} is a pair of a unix {b file descriptor} (of type
      [Unix.file_descr]) and a {b state}.

      A {b file descriptor} may be:

      - {b opened}, in which case it is fully usable
      - {b closed} or {b aborted}, in which case it is no longer
      usable *)

(** State of a {b file descriptor} *)
type state =
  | Open
      (** The {b file descriptor} is opened *)
  | Closed
      (** The {b file descriptor} has been closed by {!close}. It must
          not be used for any operation. *)
  | Aborted of exn
      (** The {b file descriptor} has been aborted, the only operation
          possible is {!close}, all others will fail. *)

val state : file_descr -> state
  (** [state fd] returns the state of [fd] *)

val openfile : string -> Unix.open_flag list -> Unix.file_perm -> file_descr
  (** Wrapper for [Unix.openfile] *)

val unix_file_descr : file_descr -> Unix.file_descr
  (** Returns the underlying unix {b file descriptor}. It always
      succeed, even if the {b file descriptor}'s state is not
      {!Open}. *)

val of_unix_file_descr : Unix.file_descr -> file_descr
  (** Creates a lwt {b file descriptor} from a unix one. It has the
      side effect of putting it into non-blocking mode *)

val of_unix_file_descr_blocking : Unix.file_descr -> file_descr
  (** Normally [Lwt_unix] uses file descriptors in non-blocking mode,
      but in certain cases, like for standard descriptors ({!stdin},
      {!stdout} and {!stderr}) we do not want that.

      This function do not modify the {b file descritpor} flags but
      other operations involving it may be a bit less efficient, since
      [Lwt_unix] will always check that the {b file descriptor} is
      ready before using it.

      Note: this is not 100% safe to use file descriptors in blocking
      mode, so you should avoid doing it. *)

val blocking : file_descr -> bool
  (** [blocking fd] returns whether [fd] is used in blocking or
      non-blocking mode. *)

val set_blocking : file_descr -> bool -> unit
  (** [set_blocking fd b] puts [fd] in blocking or non-blocking
      mode. *)

val abort : file_descr -> exn -> unit
  (** [abort fd exn] makes all current and further uses of the file
      descriptor fail with the given exception. This put the {b file
      descriptor} into the {!Aborted} state.

      If the {b file descrptor} is closed, this does nothing, if it is
      aborted, this replace the abort exception by [exn]. *)

val close : file_descr -> unit
  (** Close a {b file descriptor}. This close the underlying unix {b
      file descriptor} and set its state to {!Closed} *)

val set_close_on_exec : file_descr -> unit
  (** Wrapper for [Unix.set_close_on_exec] *)

val clear_close_on_exec : file_descr -> unit
  (** Wrapper for [Unix.clear_close_on_exec] *)

val fchmod : file_descr -> Unix.file_perm -> unit
  (** Wrapper for [Unix.fchmod] *)

val fchown : file_descr -> int -> int -> unit
  (** Wrapper for [Unix.fchown] *)

val dup : file_descr -> file_descr
  (** Wrapper for [Unix.dup] *)

val dup2 : file_descr -> file_descr -> unit
  (** Wrapper for [Unix.dup2] *)

val lockf : file_descr -> Unix.lock_command -> int -> unit
  (** Wrapper for [Unix.lockf] *)

(** {8 Standard instances} *)

val stdin : file_descr
  (** The standard {b file descriptor} for input. This one is usually
      a terminal is the program is started from a terminal. *)

val stdout : file_descr
  (** The standard {b file descriptor} for output *)

val stderr : file_descr
  (** The standard {b file descriptor} for printing error messages *)

(** {8 Reading/writing} *)

val read : file_descr -> string -> int -> int -> int Lwt.t
  (** [read fd buf ofs len] has the same semantic as [Unix.read], but
      is cooperative *)

val write : file_descr -> string -> int -> int -> int Lwt.t
  (** [read fd buf ofs len] has the same semantic as [Unix.write], but
      is cooperative *)

val wait_read : file_descr -> unit Lwt.t
  (** waits (without blocking other threads) until there is something
      to read on the file descriptor *)

val wait_write : file_descr -> unit Lwt.t
  (** waits (without blocking other threads) until it is possible to
      write on the file descriptor *)

(** {8 Pipes} *)

val pipe : unit -> file_descr * file_descr
  (** [pipe ()] creates pipe using [Unix.pipe] and returns two lwt {b
      file descriptor}s created from unix {b file_descriptor} *)

val pipe_in : unit -> file_descr * Unix.file_descr
  (** [pipe_in ()] is the same as {!pipe} but maps only the unix {b
      file descriptor} for reading into a lwt one. The second is not
      put into non-blocking mode. You usually want to use this before
      forking to receive data from the child process. *)

val pipe_out : unit -> Unix.file_descr * file_descr
  (** [pipe_out ()] is the inverse of {!pipe_in}. You usually want to
      use this before forking to send data to the child process *)

(** {8 Random access} *)

val lseek : file_descr -> int -> Unix.seek_command -> int
  (** Wrapper for [Unix.lseek] *)

val ftruncate : file_descr -> int -> unit
  (** Wrapper for [Unix.ftruncate] *)

(** {6 Miscellaneous wrappers} *)

val fstat : file_descr -> Unix.stats
  (** Wrapper for [Unix.fstat] *)

val isatty : file_descr -> bool
  (** Wrapper for [Unix.isatty] *)

module LargeFile : sig
  val lseek : file_descr -> int64 -> Unix.seek_command -> int64
  val ftruncate : file_descr -> int64 -> unit
  val fstat : file_descr -> Unix.LargeFile.stats
end

(** {6 Signals} *)

type signal_handler_id
  (** Id of a signal handler, used to cancel it *)

val on_signal : int -> (int -> unit) -> signal_handler_id
  (** [on_signal signum f] calls [f] each time the signal with numnber
      [signum] is received by the process. It returns a signal handler
      identifier which can be used to stop monitoring [signum]. *)

val disable_signal_handler : signal_handler_id -> unit
  (** Stops receiving this signal *)

(** {6 Directories} *)

type dir_handle
  (** The type of descriptors over opened directories. *)

val opendir : string -> dir_handle
  (** Open a descriptor on a directory *)

val readdir : dir_handle -> string Lwt.t
  (** Return the next entry in a directory.

      @raise End_of_file when the end of the directory has been reached. *)

val rewinddir : dir_handle -> unit
  (** Reposition the descriptor to the beginning of the directory *)

val closedir : dir_handle -> unit
  (** Close a directory descriptor. *)

(** {6 Processes} *)

(** Resource usages *)
type resource_usage = {
  ru_utime : float;
  (** User time used *)

  ru_stime : float;
  (** System time used *)
}

val wait : unit -> (int * Unix.process_status) Lwt.t
  (** Wrapper for [Unix.wait] *)

val waitpid : Unix.wait_flag list -> int -> (int * Unix.process_status) Lwt.t
  (** Wrapper for [Unix.waitpid] *)

val wait4 : Unix.wait_flag list -> int -> (int * Unix.process_status * resource_usage) Lwt.t
  (** [wait4 flags pid] returns [(pid, status, rusage)] where [(pid,
      status)] is the same result as [Unix.waitpid flags pid], and
      [rusage] contains accounting information about the child. *)

val has_wait4 : bool
  (** Whether the [wait4] system call is available on this system. If
      it is not, them [wait4] will always returns [{ utime = 0.0;
      stime = 0.0 }] as resource usages. *)

val system : string -> Unix.process_status Lwt.t
  (** Wrapper for [Unix.system] *)

(** {6 Sockets} *)

val socket : Unix.socket_domain -> Unix.socket_type -> int -> file_descr
  (** [socket domain type proto] is the same as [Unix.socket] but maps
      the result into a lwt {b file descriptor} *)

val socketpair : Unix.socket_domain -> Unix.socket_type -> int -> file_descr * file_descr
  (** Wrapper for [Unix.socketpair] *)

val bind : file_descr -> Unix.sockaddr -> unit
  (** Wrapper for [Unix.bind] *)

val listen : file_descr -> int -> unit
  (** Wrapper for [Unix.listen] *)

val accept : file_descr -> (file_descr * Unix.sockaddr) Lwt.t
  (** Wrapper for [Unix.accept] *)

val accept_n : file_descr -> int -> ((file_descr * Unix.sockaddr) list * exn option) Lwt.t
  (** [accept_n fd count] accepts up to [count] connection in one time.

      - if no connection is available right now, it returns a sleeping
      thread

      - if more that 1 and less than [count] are available, it returns
      all of them

      - if more that [count] are available, it returns the next
      [count] of them

      - if an error happen, it returns the connections that have been
      successfully accepted so far and the error

      [accept_n] has the advantage of improving performances. If you
      want a more detailed description, you can have a look at:

      {{:http://portal.acm.org/citation.cfm?id=1247435}Acceptable strategies for improving web server performance} *)

val connect : file_descr -> Unix.sockaddr -> unit Lwt.t
  (** Wrapper for [Unix.connect] *)

val shutdown : file_descr -> Unix.shutdown_command -> unit
  (** Wrapper for [Unix.shutdown] *)

val getsockname : file_descr -> Unix.sockaddr
  (** Wrapper for [Unix.getsockname] *)

val getpeername : file_descr -> Unix.sockaddr
  (** Wrapper for [Unix.getpeername] *)

(** {8 Socket options} *)

val getsockopt : file_descr -> Unix.socket_bool_option -> bool
  (** Wrapper for [Unix.getsockopt] *)

val setsockopt : file_descr -> Unix.socket_bool_option -> bool -> unit
  (** Wrapper for [Unix.setsockopt] *)

val getsockopt_int : file_descr -> Unix.socket_int_option -> int
  (** Wrapper for [Unix.getsockopt_int] *)

val setsockopt_int : file_descr -> Unix.socket_int_option -> int -> unit
  (** Wrapper for [Unix.setsockopt_int] *)

val getsockopt_optint : file_descr -> Unix.socket_optint_option -> int option
  (** Wrapper for [Unix.getsockopt_optint] *)

val setsockopt_optint : file_descr -> Unix.socket_optint_option -> int option -> unit
  (** Wrapper for [Unix.setsockopt_optint] *)

val getsockopt_float : file_descr -> Unix.socket_float_option -> float
  (** Wrapper for [Unix.getsockopt_float] *)

val setsockopt_float : file_descr -> Unix.socket_float_option -> float -> unit
  (** Wrapper for [Unix.setsockopt_float] *)

val getsockopt_error : file_descr -> Unix.error option
  (** Wrapper for [Unix.getsockopt_error] *)

type credentials = {
  cred_pid : int;
  cred_uid : int;
  cred_gid : int;
}

val get_credentials : file_descr -> credentials
  (** [get_credentials fd] returns credential informations from the
      given socket. *)

(** {8 receive/send messages} *)

val recv : file_descr -> string -> int -> int -> Unix.msg_flag list -> int Lwt.t
  (** Wrapper for [Unix.recv] *)

val recvfrom : file_descr -> string -> int -> int -> Unix.msg_flag list -> (int * Unix.sockaddr) Lwt.t
  (** Wrapper for [Unix.recvfrom] *)

val send : file_descr -> string -> int -> int -> Unix.msg_flag list -> int Lwt.t
  (** Wrapper for [Unix.send] *)

val sendto : file_descr -> string -> int -> int -> Unix.msg_flag list -> Unix.sockaddr -> int Lwt.t
  (** Wrapper for [Unix.sendto] *)

(** An io-vector. Used by {!recv_msg} and {!send_msg}. *)
type io_vector = {
  iov_buffer : string;
  iov_offset : int;
  iov_length : int;
}

val io_vector : buffer : string -> offset : int -> length : int -> io_vector
  (** Creates an io-vector *)

val recv_msg : socket : file_descr -> io_vectors : io_vector list -> (int * Unix.file_descr list) Lwt.t
  (** [recv_msg ~socket ~io_vectors] receives data into a list of
      io-vectors, plus any file-descriptors that may accompany the
      message. *)

val send_msg : socket : file_descr -> io_vectors : io_vector list -> fds : Unix.file_descr list -> int Lwt.t
  (** [send_msg ~socket ~io_vectors ~fds] sends data from a list of
      io-vectors, accompanied with a list of file-descriptor. *)

(** {6 Terminal interface} *)

val tcgetattr : file_descr -> Unix.terminal_io
  (** Wrapper for [Unix.tcgetattr] *)

val tcsetattr : file_descr -> Unix.setattr_when -> Unix.terminal_io -> unit
  (** Wrapper for [Unix.tcsetattr] *)

val tcdrain : file_descr -> unit
  (** Wrapper for [Unix.tcdrain] *)

val tcflush : file_descr -> Unix.flush_queue -> unit
  (** Wrapper for [Unix.tcflush] *)

val tcflow : file_descr -> Unix.flow_action -> unit
  (** Wrapper for [Unix.tcflow] *)

(** {6 Low-level interaction} *)

type watchers
  (** Type of a set of watchers, i.e. a set of action waiting for a {b
      file descriptor} to be readable/writable. *)

exception Retry
  (** If an action raises {!Retry}, it will be requeued until the {b
      file descriptor} becomes readable/writable again. *)

exception Retry_read
  (** If an action raises {!Retry_read}, it will be requeued until the
      {b file descriptor} becomes readable. *)

exception Retry_write
  (** If an action raises {!Retry_read}, it will be requeued until the
      {b file descriptor} becomes writables. *)

val inputs : watchers
  (** The set of action waiting for a {b file descriptor} to become
      readable *)

val outputs : watchers
  (** The set of action waiting for a {b file descriptor} to become
      writable *)

val wrap_syscall : watchers -> file_descr -> (unit -> 'a) -> 'a Lwt.t
  (** [wrap_syscall set fd action] wrap an action on a {b file
      descriptor}. It tries to execture action, and if it can not be
      performed immediately without blocking, it is registered for
      latter.

      In the latter case, if the thread is canceled, [action] is
      removed from [set]. *)

val check_descriptor : file_descr -> unit
  (** [check_descriptor fd] raise an exception if [fd] is not in the
      state {!Open} *)

val register_action : watchers -> file_descr -> (unit -> 'a) -> 'a Lwt.t
  (** [register_action set fd action] registers [action] on [fd]. When
      [fd] becomes [readable]/[writable] [action] is called.

      Note:

      - you must call [check_descriptor fd] before calling
      [register_action]

      - you should prefer using {!wrap_syscall}
  *)

(**/**)

val inputs_length : unit -> int
val outputs_length : unit -> int
val wait_children_length : unit -> int
val get_new_sleeps : unit -> int
val sleep_queue_size : unit -> int

val run : 'a Lwt.t -> 'a
  (* Same as {!Lwt_main.run} *)
