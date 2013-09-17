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

val test_direct : name : string -> run : (unit -> bool) -> t
  (** Defines a test. [run] must returns [true] if the test succeeded
      and [false] otherwise. *)

val test : name : string -> run : (unit -> bool Lwt.t) -> t
  (** Defines a test which returns a thread. *)

val suite : name : string -> tests : t list -> suite
  (** Defines a suite of tests *)

val run : name : string -> suites : suite list -> unit
  (** Run all the given tests and exit the program with an exit code
      of [0] if all tests succeeded and with [1] otherwise. *)
