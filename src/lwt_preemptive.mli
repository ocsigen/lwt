(* Ocsigen
 * http://www.ocsigen.org
 * Module lwt_preemptive.ml
 * Copyright (C) 2005 Nataliya Guts, Vincent Balat, Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(** This module allows to mix preemptive threads with [Lwt]
   cooperative threads. It maintains an extensible pool of preemptive
   threads to with you can detach computations.
 *)

(** detaches a computation to a preemptive thread. *)
val detach : ('a -> 'b) -> 'a -> 'b Lwt.t


val init : int -> int -> (string -> unit) -> 'a Lwt.t
(** Should be called only once at the begining of the process.
    Arguments are: minimum number of threads, maximum number of threads
    and the function to log errors.
*)

val set_max_number_of_threads_queued : int -> unit
(** Sets the size of the waiting queue, if no more threads are available *)

val get_max_number_of_threads_queued : unit -> int
(** Returns the size of the waiting queue, if no more threads are available *)

(**/**)
val nbthreads : unit -> int
val nbthreadsbusy : unit -> int
val nbthreadsqueued : unit -> int


