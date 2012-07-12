(* Lwt
 * http://www.ocsigen.org
 * Copyright (C) 2008 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exceptions;
 * either version 2.1 of the License, or (at your option) any later version.
 * See COPYING file for details.
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

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

(*
XXX Close after some timeout
...
*)

type 'a t =
  { create : unit -> 'a Lwt.t;
    check : 'a -> (bool -> unit) -> unit;
    validate : 'a -> bool Lwt.t;
    max : int;
    mutable count : int;
    list : 'a Queue.t;
    waiters : 'a Lwt.u Lwt_sequence.t }

let create m ?(check = fun _ f -> f true) ?(validate = fun _ -> Lwt.return_true) create =
  { max = m;
    create = create;
    validate = validate;
    check = check;
    count = 0;
    list = Queue.create ();
    waiters = Lwt_sequence.create () }

let create_member p =
  Lwt.catch
    (fun () ->
       p.count <- p.count + 1; (* must be done before p.create *)
       p.create ())
    (fun exn ->
       (* create failed, so don't increment count *)
       p.count <- p.count - 1;
       Lwt.fail exn)

let release p c =
  try
    Lwt.wakeup_later (Lwt_sequence.take_l p.waiters) c
  with Lwt_sequence.Empty ->
    Queue.push c p.list

let replace_acquired p =
  Lwt.ignore_result (
    p.count <- p.count - 1;
    create_member p >>= fun c ->
    release p c;
    Lwt.return_unit
  )

let acquire p =
  if Queue.is_empty p.list then
    if p.count < p.max then
      create_member p
    else
      Lwt.add_task_r p.waiters
  else
    let c = Queue.take p.list in
    Lwt.try_bind
      (fun () ->
         p.validate c)
      (function
         | true ->
             Lwt.return c
         | false ->
             p.count <- p.count - 1;
             create_member p)
      (fun e ->
         replace_acquired p;
         Lwt.fail e)

let checked_release p c =
  p.check c begin fun ok ->
    if ok then
      release p c
    else
      replace_acquired p
  end

let use p f =
  acquire p >>= fun c ->
  Lwt.catch
    (fun () ->
       let t = f c in
       t >>= fun _ ->
       release p c;
       t)
    (fun e ->
       checked_release p c;
       Lwt.fail e)
