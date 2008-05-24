(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_mutex
 * Copyright (C) 2005-2008 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later
 * version.
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

type t = { mutable locked : bool; mutable waiting : unit Lwt.t list  }

let create () = { locked = false; waiting = [] }

let rec lock m =
  if m.locked then begin
    let res = Lwt.wait () in
    m.waiting <- res :: m.waiting;
    Lwt.bind res (fun () ->
    lock m)
  end else begin
    m.locked <- true;
    Lwt.return ()
  end

let unlock m =
  let w = m.waiting in
  m.waiting <- [];
  m.locked <- false;
  List.iter (fun t -> Lwt.wakeup t ()) w
