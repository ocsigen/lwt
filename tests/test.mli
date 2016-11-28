(* Lightweight thread library for OCaml
 * http://www.ocsigen.org/lwt
 * Interface Test
 * Copyright (C) 2009 Jérémie Dimino
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

(** Helpers for test *)

type t
  (** Type of a test *)

type suite
  (** Type of a suite of tests *)

val test_direct : string -> ?only_if:(unit -> bool) -> (unit -> bool) -> t
  (** Defines a test. [run] must returns [true] if the test succeeded
      and [false] otherwise. [only_if] is used to conditionally skip the
      test. *)

val test : string -> ?only_if:(unit -> bool) -> (unit -> bool Lwt.t) -> t
  (** Like [test_direct], but defines a test which runs a thread. *)

val suite : string -> t list -> suite
  (** Defines a suite of tests *)

val run : string -> suite list -> unit
  (** Run all the given tests and exit the program with an exit code
      of [0] if all tests succeeded and with [1] otherwise. *)

val temp_name : unit -> string
(** Generates the name of a temporary file (or directory) in [_build/]. Note
    that a file at the path may already exist. *)

val temp_file : unit -> string
(** Creates a temporary file in [_build/] and evaluates to its path. *)

val temp_directory : unit -> string
(** Creates a temporary directory in [build/] and evaluates to its path. *)
