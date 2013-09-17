(* Lightweight thread library for OCaml
 * http://www.ocsigen.org/lwt
 * Interface Lwt_ssl
 * Copyright (C) 2005-2008 J�r�me Vouillon
 * Laboratoire PPS - CNRS Universit� Paris Diderot
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

(** OCaml-SSL integration *)

type socket
  (** Wrapper for SSL sockets.

      It is either a plain socket, either a real SSL socket. *)

val ssl_socket : socket -> Ssl.socket option
(** Returns the underlying SSL socket used for this wrapper. If it is
      a plain socket it returns [None]. *)

val is_ssl : socket -> bool
(** Are we using an SSL socket? *)

val ssl_accept : Lwt_unix.file_descr -> Ssl.context -> socket Lwt.t
val ssl_connect : Lwt_unix.file_descr -> Ssl.context -> socket Lwt.t
val plain : Lwt_unix.file_descr -> socket
val embed_socket : Lwt_unix.file_descr -> Ssl.context -> socket

val read : socket -> string -> int -> int -> int Lwt.t
val write : socket -> string -> int -> int -> int Lwt.t

val read_bytes : socket -> Lwt_bytes.t -> int -> int -> int Lwt.t
val write_bytes : socket -> Lwt_bytes.t -> int -> int -> int Lwt.t

(* Really wait on a plain socket, just yield over SSL *)
val wait_read : socket -> unit Lwt.t
val wait_write : socket -> unit Lwt.t

val shutdown : socket -> Unix.shutdown_command -> unit
val close : socket -> unit Lwt.t

val in_channel_of_descr : socket -> Lwt_io.input_channel
val out_channel_of_descr : socket -> Lwt_io.output_channel

val ssl_shutdown : socket -> unit Lwt.t

val abort : socket -> exn -> unit

val get_fd : socket -> Lwt_unix.file_descr

val get_unix_fd : socket -> Unix.file_descr

val getsockname : socket -> Unix.sockaddr

val getpeername : socket -> Unix.sockaddr
