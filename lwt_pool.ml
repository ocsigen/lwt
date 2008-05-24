(* Lwt
 * http://www.ocsigen.org
 * Copyright (C) 2008 Jérôme Vouillon
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

(*
XXX What if create fails
XXX Close after some timeout
...
*)

let (>>=) = Lwt.bind

type 'a t =
  { create : unit -> 'a Lwt.t;
    check : 'a -> (bool -> unit) -> unit;
    max : int;
    mutable count : int;
    list : 'a Queue.t;
    waiters : 'a Lwt.t Queue.t }

let create m ?(check = fun _ f -> f true) create =
  { max = m;
    create = create;
    check = check;
    count = 0;
    list = Queue.create ();
    waiters = Queue.create () }

let acquire p =
  try
    Lwt.return (Queue.take p.list)
  with Queue.Empty ->
    if p.count < p.max then begin
      p.count <- p.count + 1;
      p.create ()
    end else begin
      let r = Lwt.wait () in
      Queue.push r p.waiters;
      r
    end

let release p c =
  try
    Lwt.wakeup (Queue.take p.waiters) c
  with Queue.Empty ->
    Queue.push c p.list

let checked_release p c =
  p.check c (fun ok ->
  if ok then release p c else
  ignore (p.create () >>= fun c -> release p c; Lwt.return ()))

let use p f =
  acquire p >>= fun c ->
  Lwt.catch
    (fun () -> f c >>= fun r -> release p c; Lwt.return r)
    (fun e -> checked_release p c; Lwt.fail e)
