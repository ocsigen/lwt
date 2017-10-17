(* Lwt
 * http://www.ocsigen.org
 * Copyright (C) 2008 Jérôme Vouillon
 *               2012 Jérémie Dimino
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

open Lwt.Infix

(*
XXX Close after some timeout
...
*)

type 'a t = {
  create : unit -> 'a Lwt.t;
  (* Create a new pool member. *)
  check : 'a -> (bool -> unit) -> unit;
  (* Check a member when its use failed. *)
  validate : 'a -> bool Lwt.t;
  (* Validate old pool members. *)
  dispose : 'a -> unit Lwt.t;
  (* Dispose of a pool member. *)
  max : int;
  (* Size of the pool. *)
  mutable count : int;
  (* Number of elements in the pool. *)
  list : 'a Queue.t;
  (* Available pool members. *)
  waiters : 'a Lwt.u Lwt_sequence.t;
  (* Threads waiting for a member. *)
}

let create m ?(check = fun _ f -> f true) ?(validate = fun _ -> Lwt.return_true) ?(dispose = fun _ -> Lwt.return_unit) create =
  { max = m;
    create = create;
    validate = validate;
    check = check;
    dispose = dispose;
    count = 0;
    list = Queue.create ();
    waiters = Lwt_sequence.create () }

let create_member p =
  Lwt.catch
    (fun () ->
       (* Must be done before p.create to prevent other threads from
          creating new members if the limit is reached. *)
       p.count <- p.count + 1;
       p.create ())
    (fun exn ->
       (* Creation failed, so don't increment count. *)
       p.count <- p.count - 1;
       Lwt.fail exn)

(* Release a pool member. *)
let release p c =
  match Lwt_sequence.take_opt_l p.waiters with
  | Some wakener ->
    (* A thread is waiting, give it the pool member. *)
    Lwt.wakeup_later wakener c
  | None ->
    (* No one is waiting, queue it. *)
    Queue.push c p.list

(* Create a new member when one is thrown away. *)
let replace_acquired p =
  match Lwt_sequence.take_opt_l p.waiters with
  | None ->
    (* No one is waiting, do not create a new member to avoid
       loosing an error if creation fails. *)
    p.count <- p.count - 1
  | Some wakener ->
    Lwt.on_any
      (Lwt.apply p.create ())
      (fun c ->
         Lwt.wakeup_later wakener c)
      (fun exn ->
         (* Creation failed, notify the waiter of the failure. *)
         p.count <- p.count - 1;
         Lwt.wakeup_later_exn wakener exn)

(* Verify a member is still valid before releasing it; try to replace
   if it's invalid. *)
let check_elt p c =
  Lwt.try_bind
      (fun () ->
         p.validate c)
      (function
        | true ->
          Lwt.return c
        | false ->
          (* Remove this member and create a new one. *)
          p.count <- p.count - 1;
          p.dispose c >>= fun () ->
          create_member p)
      (fun e ->
         (* Validation failed: create a new member if at least one
            thread is waiting. *)
         p.dispose c >>= fun () ->
         replace_acquired p;
         Lwt.fail e)

let acquire p =
  if Queue.is_empty p.list then
    (* No more available member. *)
    if p.count < p.max then
      (* Limit not reached: create a new one. *)
      create_member p
    else
      (* Limit reached: wait for a free one. *)
      Lwt.add_task_r p.waiters >>= check_elt p
  else
    (* Take the first free member and validate it. *)
    let c = Queue.take p.list in
    check_elt p c

(* Release a member when its use failed. *)
let checked_release p c =
  let ok = ref false in
  p.check c (fun result -> ok := result);
  if !ok then (
    (* Element is ok - release it back to the pool *)
    release p c;
    Lwt.return_unit
  )
  else (
    (* Element is not ok - dispose of it and replace with a new one *)
    p.dispose c >>= fun () ->
    replace_acquired p;
    Lwt.return_unit
  )

let use p f =
  acquire p >>= fun c ->
  Lwt.catch
    (fun () ->
       let t = f c in
       t >>= fun _ ->
       release p c;
       t)
    (fun e ->
       checked_release p c >>= fun () ->
       Lwt.fail e)

let clear p =
  let elements = Queue.fold (fun l element -> element :: l) [] p.list in
  Queue.clear p.list;
  Lwt_list.iter_s p.dispose elements
