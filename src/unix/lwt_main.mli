(* OCaml promise library
 * http://www.ocsigen.org/lwt
 * Copyright (C) 2009-2011 Jérémie Dimino
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

(** Main loop and event queue *)

(** This module controls the ``main-loop'' of Lwt. *)

val run : 'a Lwt.t -> 'a
  (** [run p] calls the Lwt scheduler repeatedly until [p] resolves,
      and returns the value of [p] if it is fulfilled. If [p] is rejected with
      an exception, that exception is raised.

      Every native or bytecode program that uses Lwt should always use
      this function for evaluating a promise at the top level
      (such as its main function or main loop),
      otherwise promises that depend on I/O operations will not be resolved.

      Example:
      {[
let main () = Lwt_io.write_line Lwt_io.stdout "hello world"

let () = Lwt_main.run @@ main ()
      ]}

      When targeting JavaScript, [Lwt_main.run] is not available,
      but neither it's necessary since
      the JS environment automatically takes care of the I/O considerations.


      Note that you should avoid using [run] inside threads
      - The calling threads will not resume before [run]
        returns.
      - Successive invocations of [run] are serialized: an
        invocation of [run] will not terminate before all
        subsequent invocations are terminated.

      Note also that it is not safe to call [run] in a function
      registered with [Pervasives.at_exit], use the {!at_exit}
      function of this module instead. *)

val yield : unit -> unit Lwt.t
  (** [yield ()] is a threads which suspends itself and then resumes
      as soon as possible and terminates. *)

[@@@ocaml.warning "-3"]

val enter_iter_hooks : (unit -> unit) Lwt_sequence.t
  (** Functions that are called before the main iteration. *)

val leave_iter_hooks : (unit -> unit) Lwt_sequence.t
  (** Functions that are called after the main iteration. *)

val exit_hooks : (unit -> unit Lwt.t) Lwt_sequence.t
  (** Sets of functions executed just before the program exit.

      Notes:
      - each hook is called exactly one time
      - exceptions raised by hooks are ignored *)

[@@@ocaml.warning "+3"]

val at_exit : (unit -> unit Lwt.t) -> unit
  (** [at_exit hook] adds hook at the left of [exit_hooks]*)
