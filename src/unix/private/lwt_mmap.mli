(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_mmap
 * Copyright (C) 2010 Pierre Chambart
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

(** Note: on 32bits systems the functions in this module can't work with
    files bigger than 2Go *)

val sendfile : string -> Lwt_unix.file_descr -> int -> int -> unit Lwt.t
(** [sendfile filename output offset length] sends a file throught
    [output] cooperatively *)

type t

val of_unix_fd : Unix.file_descr -> t option

val size : t -> int

val read : t -> int -> string -> int -> int -> int Lwt.t
(** [read input position buf offset length] reads at maximum [length]
    bytes from [input] at [position] into [buf] starting at position [offset].
    It returns the number of bytes effectively read. *)

val max_read_size : int
