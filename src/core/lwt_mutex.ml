(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_mutex
 * Copyright (C) 2005-2008 Jérôme Vouillon
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

let (>>=) = Lwt.(>>=)

type t = { mutable locked : bool; mutable waiters : unit Lwt.u Lwt_sequence.t  }

let create () = { locked = false; waiters = Lwt_sequence.create () }

let rec lock m =
  if m.locked then
    Lwt.add_task_r m.waiters
  else begin
    m.locked <- true;
    Lwt.return_unit
  end

let unlock m =
  if m.locked then begin
    if Lwt_sequence.is_empty m.waiters then
      m.locked <- false
    else
      (* We do not use [Lwt.wakeup] here to avoid a stack overflow
         when unlocking a lot of threads. *)
      Lwt.wakeup_later (Lwt_sequence.take_l m.waiters) ()
  end

let with_lock m f =
  lock m >>= fun () ->
  Lwt.finalize f (fun () -> unlock m; Lwt.return_unit)

let is_locked m = m.locked
let is_empty m = Lwt_sequence.is_empty m.waiters
