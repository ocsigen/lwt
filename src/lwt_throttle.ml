(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_throttle
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

open Lwt

module type S = sig
  type key
  type t

  val create : rate:int -> max:int -> n:int -> t
  val wait : t -> key -> bool Lwt.t
end

module Make (H : Hashtbl.HashedType) : (S with type key = H.t) = struct
  module MH = Hashtbl.Make(H)

  type key = H.t
  type t = {
    mutex : Lwt_mutex.t;
    dt : float;
    max : float;
    timeout : int;
    table : (float ref * Lwt_timeout.t) MH.t
  }

  let create ~rate ~max ~n =
    if rate < 1 || max < 1 || n < 1 then
      invalid_arg "Lwt_throttle.S.create"
    else {
      mutex = Lwt_mutex.create ();
      dt = 1. /. (float_of_int rate);
      max = float_of_int max;
      timeout = max+1;
      table = MH.create n
    }

  let remove t k () =
    ignore begin
      Lwt_mutex.lock t.mutex >>= fun () ->
      MH.remove t.table k;
      return (Lwt_mutex.unlock t.mutex)
    end

  let wait t k =
    Lwt_mutex.lock t.mutex >>= fun () ->
    let now = Unix.time () in
    let (last, timeout) =
      try
        MH.find t.table k
      with Not_found ->
        let x = (ref now, Lwt_timeout.create t.timeout (remove t k)) in
        MH.add t.table k x;
        x
    in
    Lwt_timeout.stop timeout;
    let next = !last +. t.dt in
    let wait = next -. now in
    Lwt_timeout.start timeout;
    if wait > t.max then begin
      Lwt_mutex.unlock t.mutex;
      return false
    end else if wait > 0. then begin
      last := next;
      Lwt_mutex.unlock t.mutex;
      Lwt_unix.sleep wait >>= fun () -> return true
    end else begin
      last := now;
      Lwt_mutex.unlock t.mutex;
      return true
    end
end
