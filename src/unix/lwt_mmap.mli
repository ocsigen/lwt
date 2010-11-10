(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_mmap
 * Copyright (C) 2010 Pierre Chambart
 *               2010 Jérémie Dimino
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

(** Cooperative disk inputs *)

(** Note: on 32bits systems the functions in this module can't work
    with files bigger than 2Go *)

type t
  (** Type of memory-maped file. *)

type byte_array = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
    (** Type of array of bytes used in this module. *)

(** {6 Creation/closing} *)

val of_unix_fd : Unix.file_descr -> t option
  (** Creates a mmaped file from a unix file descriptor. If the given
      file descriptor is not mapable, it returns [None]. *)

val close : t -> unit
  (** Close the given mmaped file, i.e. unmap it. Note that the
      underlying file descriptor is not closed by this function. *)

(** {6 Informations} *)

val size : t -> int
  (** Returns the size of the mapped file. *)

(** {6 Reading data} *)

val read : t -> int -> string -> int -> int -> int Lwt.t
  (** [read input position buf offset length] reads at maximum
      [length] bytes from [input] at [position] into [buf] starting at
      position [offset]. It returns the number of bytes effectively
      read. *)

(** {6 Misc} *)

val sendfile : Unix.file_descr -> Lwt_unix.file_descr -> int -> int -> unit Lwt.t
  (** [sendfile filename output offset length] sends a file throught
      [output] cooperatively *)

val max_read_size : int
  (** The maximum size Lwt will advise the kernel to prefetch. *)
