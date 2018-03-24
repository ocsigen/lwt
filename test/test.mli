(* OCaml promise library
 * http://www.ocsigen.org/lwt
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



(** Helpers for tests. *)

type test
(** Type of a test *)

type suite
(** Type of a suite of tests *)

exception Skip
(** In some tests, it is only clear that the test should be skipped after it has
    started running (for example, after an attempted system call raises a
    certain exception, indicating it is not supported).

    Such tests should raise [Test.Skip], or reject their final promise with
    [Test.Skip]. *)

val test_direct : string -> ?only_if:(unit -> bool) -> (unit -> bool) -> test
(** Defines a test. [run] must returns [true] if the test succeeded
    and [false] otherwise. [only_if] is used to conditionally skip the
    test. *)

val test : string -> ?only_if:(unit -> bool) -> (unit -> bool Lwt.t) -> test
(** Like [test_direct], but defines a test which runs a thread. *)

val suite : string -> ?only_if:(unit -> bool) -> test list -> suite
(** Defines a suite of tests *)

val run : string -> suite list -> unit
(** Run all the given tests and exit the program with an exit code
    of [0] if all tests succeeded and with [1] otherwise. *)
