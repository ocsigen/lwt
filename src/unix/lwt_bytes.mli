(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Interface Lwt_unix
 * Copyright (C) 2010 Jérémie Dimino
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

(** Byte arrays *)

type t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
    (** Type of array of bytes. *)

val create : int -> t
  (** Creates a new byte array of the given size. *)

val length : t -> int
  (** Returns the length of the given byte array. *)

external get : t -> int -> char = "%caml_ba_ref_1"
external set : t -> int -> char -> unit = "%caml_ba_set_1"

external unsafe_get : t -> int -> char = "%caml_ba_unsafe_ref_1"
external unsafe_set : t -> int -> char -> unit = "%caml_ba_unsafe_set_1"

val of_string : string -> t
val to_string : t -> string

val blit : t -> int -> t -> int -> int -> unit
val blit_string_bytes : string -> int -> t -> int -> int -> unit
val blit_bytes_string : t -> int -> string -> int -> int -> unit

external unsafe_blit : t -> int -> t -> int -> int -> unit = "lwt_unix_blit_bytes_bytes" "noalloc"
external unsafe_blit_string_bytes : string -> int -> t -> int -> int -> unit = "lwt_unix_blit_string_bytes" "noalloc"
external unsafe_blit_bytes_string : t -> int -> string -> int -> int -> unit = "lwt_unix_blit_bytes_string" "noalloc"

val fill : t -> int -> int -> char -> unit

external unsafe_fill : t -> int -> int -> char -> unit = "lwt_unix_fill_bytes" "noalloc"

(** {6 IOs} *)

(** The following functions does the same as the functions in
    {!Lwt_unix} except that they use byte arrays instead of
    strings. *)

val read : Lwt_unix.file_descr -> t -> int -> int -> int Lwt.t
val write : Lwt_unix.file_descr -> t -> int -> int -> int Lwt.t

val recv : Lwt_unix.file_descr -> t -> int -> int -> Unix.msg_flag list -> int Lwt.t
val send : Lwt_unix.file_descr -> t -> int -> int -> Unix.msg_flag list -> int Lwt.t

type io_vector = {
  iov_buffer : t;
  iov_offset : int;
  iov_length : int;
}

val io_vector : buffer : t -> offset : int -> length : int -> io_vector

val recv_msg : socket : Lwt_unix.file_descr -> io_vectors : io_vector list -> (int * Unix.file_descr list) Lwt.t
val send_msg : socket : Lwt_unix.file_descr -> io_vectors : io_vector list -> fds : Unix.file_descr list -> int Lwt.t

