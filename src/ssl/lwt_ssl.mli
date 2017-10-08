(* OCaml promise library
 * http://www.ocsigen.org/lwt
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

(** OCaml-SSL integration

    This module is provided by OPAM package [lwt_ssl]. Link with ocamlfind
    package [lwt_ssl]. *)

type socket
  (** Wrapper for SSL sockets.

      It is either a plain socket, either a real SSL socket. *)

type uninitialized_socket
  (** Wrapper for SSL sockets that have not yet performed the SSL
      handshake. *)

val ssl_socket : socket -> Ssl.socket option
(** Returns the underlying SSL socket used for this wrapper. If it is
      a plain socket it returns [None]. *)

val ssl_socket_of_uninitialized_socket : uninitialized_socket -> Ssl.socket
(** Returns the underlying SSL socket used for this wrapper. *)

val is_ssl : socket -> bool
(** Are we using an SSL socket? *)

val ssl_accept : Lwt_unix.file_descr -> Ssl.context -> socket Lwt.t
val ssl_connect : Lwt_unix.file_descr -> Ssl.context -> socket Lwt.t
val plain : Lwt_unix.file_descr -> socket
val embed_socket : Lwt_unix.file_descr -> Ssl.context -> socket

val embed_uninitialized_socket :
  Lwt_unix.file_descr -> Ssl.context -> uninitialized_socket

val ssl_perform_handshake : uninitialized_socket -> socket Lwt.t
(** Initiate a SSL/TLS handshake on the specified socket (used by clients). *)

val ssl_accept_handshake  : uninitialized_socket -> socket Lwt.t
(** Await a SSL/TLS handshake on the specified socket (used by servers). *)

val read : socket -> bytes -> int -> int -> int Lwt.t
val write : socket -> bytes -> int -> int -> int Lwt.t

val read_bytes : socket -> Lwt_bytes.t -> int -> int -> int Lwt.t
val write_bytes : socket -> Lwt_bytes.t -> int -> int -> int Lwt.t

(* Really wait on a plain socket, just yield over SSL *)
val wait_read : socket -> unit Lwt.t
val wait_write : socket -> unit Lwt.t

val shutdown : socket -> Unix.shutdown_command -> unit
val close : socket -> unit Lwt.t

val in_channel_of_descr : ?buffer:Lwt_bytes.t -> socket -> Lwt_io.input_channel
val out_channel_of_descr : ?buffer:Lwt_bytes.t -> socket -> Lwt_io.output_channel

val ssl_shutdown : socket -> unit Lwt.t

val abort : socket -> exn -> unit

val get_fd : socket -> Lwt_unix.file_descr

val get_unix_fd : socket -> Unix.file_descr

val getsockname : socket -> Unix.sockaddr

val getpeername : socket -> Unix.sockaddr
