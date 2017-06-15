(* OCaml promise library
 * http://www.ocsigen.org/lwt
 * Copyright (C) 2008 Stéphane Glondu
 * Laboratoire PPS - CNRS Université Paris Diderot
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

open Lwt.Infix

module type S = sig
  type key
  type t

  val create : rate:int -> max:int -> n:int -> t
  val wait : t -> key -> bool Lwt.t
end

let section = Lwt_log.Section.make "Lwt_throttle"

module Make (H : Hashtbl.HashedType) : (S with type key = H.t) = struct
  module MH = Hashtbl.Make(H)

  type key = H.t
  type elt = {
    mutable consumed : int;
    queue : bool Lwt.u Queue.t;
  }

  type t = {
    rate : int;
    max : int; (* maximum number of waiting threads *)
    mutable waiting : int;
    table : elt MH.t;
    mutable cleaning : unit Lwt.t option;
  }

  let create ~rate ~max ~n =
    if rate < 1 || max < 1 || n < 0 then
      invalid_arg "Lwt_throttle.S.create"
    else {
      rate = rate;
      max = max;
      waiting = 0;
      table = MH.create n;
      cleaning = None;
    }

  let update_key t key elt (old_waiting,to_run) =
    let rec update to_run = function
      | 0 -> 0, Queue.length elt.queue, to_run
      | i ->
        try
          let to_run = (Queue.take elt.queue)::to_run in
          update to_run (i-1)
        with
        | Queue.Empty -> i, 0, to_run
    in
    let not_consumed, waiting, to_run = update to_run t.rate in
    let consumed = t.rate - not_consumed in
    if consumed = 0
    then
      (* there is no waiting threads for this key: we can clean the table *)
      MH.remove t.table key
    else elt.consumed <- consumed;
    (old_waiting+waiting, to_run)

  let rec clean_table t =
    let waiting,to_run = MH.fold (update_key t) t.table (0,[]) in
    t.waiting <- waiting;
    if waiting = 0 && to_run = []
    then
      (* the table is empty: we do not need to clean in 1 second *)
      t.cleaning <- None
    else launch_cleaning t;
    List.iter (fun u -> Lwt.wakeup u true) to_run

  and launch_cleaning t =
    t.cleaning <-
      let t =
        Lwt_unix.sleep 1. >>= fun () ->
        Lwt.catch
          (fun () ->
             clean_table t;
             Lwt.return_unit)
          (fun exn ->
             Lwt_log.fatal ~exn ~section "internal error")
      in
      Some t

  let really_wait t elt =
    let w,u = Lwt.task () in
    if t.max > t.waiting
    then (Queue.add u elt.queue;
          t.waiting <- succ t.waiting;
          w)
    else Lwt.return_false

  let wait t key =
    let res =
      try
        let elt = MH.find t.table key in
        if elt.consumed >= t.rate
        then really_wait t elt
        else (elt.consumed <- succ elt.consumed;
              Lwt.return_true)
      with
      | Not_found ->
        let elt = { consumed = 1;
                    queue = Queue.create () } in
        MH.add t.table key elt;
        Lwt.return_true
    in
    (match t.cleaning with
     | None -> launch_cleaning t
     | Some _ -> ());
    res

end
