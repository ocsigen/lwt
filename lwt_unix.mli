(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Interface Lwt_unix
 * Copyright (C) 2005-2008 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later
 * version.
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

(** Module [Lwt_unix]: thread-compatible system calls *)

val sleep : float -> unit Lwt.t
      (** [sleep d] is a threads which remain suspended for [d] seconds
          (letting other threads run) and then terminates. *)
val yield : unit -> unit Lwt.t
      (** [yield ()] is a threads which suspends itself (letting other
          thread run) and then resumes as soon as possible and
          terminates. *)

val run : 'a Lwt.t -> 'a
      (** [run t] lets the thread [t] run until it terminates.  It
          evaluates to the return value of [t], or raise the exception
          associated to [t] if [t] fails.

          You should avoid using [run] inside threads:
          - The calling threads will not resume before [run]
            returns.
          - Successive invocations of [run] are serialized: an
            invocation of [run] will not terminate before all
            subsequent invocations are terminated. *)

(****)

(** These functions behave as their [Unix] counterparts, but let other
    threads run while waiting for the completion of the system call. *)

type file_descr

val read : file_descr -> string -> int -> int -> int Lwt.t
val write : file_descr -> string -> int -> int -> int Lwt.t

val wait_read : file_descr -> unit Lwt.t
(** waits (without blocking other threads)
    until there is something to read on the file descriptor *)

val wait_write : file_descr -> unit Lwt.t
(** waits (without blocking other threads)
    until it is possible to write on the file descriptor *)

val pipe : unit -> file_descr * file_descr
val pipe_in : unit -> file_descr * Unix.file_descr
val pipe_out : unit -> Unix.file_descr * file_descr
val socket :
  Unix.socket_domain -> Unix.socket_type -> int -> file_descr
val socketpair :
  Unix.socket_domain -> Unix.socket_type -> int -> file_descr * file_descr
val bind : file_descr -> Unix.sockaddr -> unit
val listen : file_descr -> int -> unit
val accept : file_descr -> (file_descr * Unix.sockaddr) Lwt.t
val connect : file_descr -> Unix.sockaddr -> unit Lwt.t
val shutdown : file_descr -> Unix.shutdown_command -> unit
val close : file_descr -> unit

val setsockopt : file_descr -> Unix.socket_bool_option -> bool -> unit
val set_close_on_exec : file_descr -> unit

val wait : unit -> (int * Unix.process_status) Lwt.t
val waitpid : Unix.wait_flag list -> int -> (int * Unix.process_status) Lwt.t

val system : string -> Unix.process_status Lwt.t

(****)

(** Aborting a connection *)

val abort : file_descr -> exn -> unit
      (** Makes all current and further uses of the file descriptor
          fail with the given exception *)

(****)

(** File descriptor wrappings/unwrappings *)

(* [of_unix_file_descr] has the side-effect of putting the file
   descriptor in non-blocking mode. *)

val unix_file_descr : file_descr -> Unix.file_descr
val of_unix_file_descr : Unix.file_descr -> file_descr

(**/**)

type watchers

exception Retry
exception Retry_read
exception Retry_write

val inputs : watchers
val outputs : watchers
val register_action : watchers -> file_descr -> (unit -> 'a) -> 'a Lwt.t
val check_descriptor : file_descr -> unit
(** [check_descriptor] must be called before any system call involving
    the file descriptor and before calling [register_action]. *)

(****)

(* Monitoring *)

val inputs_length : unit -> int
val outputs_length : unit -> int
val wait_children_length : unit -> int
val get_new_sleeps : unit -> int
val sleep_queue_size : unit -> int

(*XXX*)
(*
val open_process_in: string -> Lwt_chan.in_channel Lwt.t
val open_process_out: string -> out_channel Lwt.t
val open_process: string -> (in_channel * out_channel) Lwt.t
val open_process_full:
  string -> string array ->
  (in_channel * out_channel * in_channel) Lwt.t
val close_process_in: in_channel -> Unix.process_status Lwt.t
val close_process_out: out_channel -> Unix.process_status Lwt.t
val close_process:
  in_channel * out_channel -> Unix.process_status Lwt.t
val close_process_full:
  in_channel * out_channel * in_channel ->
  Unix.process_status Lwt.t
*)
