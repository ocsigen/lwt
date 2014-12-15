(* Lightweight thread library for OCaml
 * http://www.ocsigen.org/lwt
 * Module Lwt_stream
 * Copyright (C) 2014 Simon Cruanes
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

(** {1 Bocking queue} *)

let (>>=) = Lwt.(>>=)

type 'a t = {
  queue : 'a Queue.t;
  max : int option;
  cond_push : unit Lwt_condition.t;  (* on push *)
  cond_pop : unit Lwt_condition.t; (* on pop *)
}

let create () =
  { queue = Queue.create ();
    max = None;
    cond_push = Lwt_condition.create ();
    cond_pop = Lwt_condition.create ();
  }

let create_bounded max =
  if max <= 0 then invalid_arg "Lwt_queue.create_bounded";
  { queue = Queue.create ();
    max = Some max;
    cond_push = Lwt_condition.create ();
    cond_pop = Lwt_condition.create ();
  }

let is_empty q =
  Queue.is_empty q.queue

let rec push q x =
  match q.max with
  | Some m when Queue.length q.queue >= m ->
      (* wait and retry *)
      Lwt_condition.wait q.cond_pop >>= fun () ->
      push q x
  | _ ->
    Queue.push x q.queue;
    Lwt_condition.signal q.cond_push ();
    Lwt.return_unit

let rec pop q =
  if Queue.is_empty q.queue
    then
      (* wait and retry *)
      Lwt_condition.wait q.cond_push >>= fun () ->
      pop q
    else (
      let x = Queue.pop q.queue in
      Lwt_condition.signal q.cond_pop ();
      Lwt.return x
    )
