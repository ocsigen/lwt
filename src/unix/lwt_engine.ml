(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_engine
 * Copyright (C) 2011 Jérémie Dimino
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

(* +-----------------------------------------------------------------+
   | Events                                                          |
   +-----------------------------------------------------------------+ *)

type event = unit Lazy.t

let make_event = Lazy.lazy_from_fun
let stop_event ev = Lazy.force ev
let fake_event = lazy ()

(* +-----------------------------------------------------------------+
   | Engines                                                         |
   +-----------------------------------------------------------------+ *)

type t = {
  on_readable : Unix.file_descr -> (unit -> unit) -> event;
  on_writable : Unix.file_descr -> (unit -> unit) -> event;
  on_timer : float -> (unit -> unit) -> event;
  on_signal : int -> (unit -> unit) -> event;
  on_suspend : (unit -> unit) -> event;
  on_resume : (unit -> unit) -> event;
}

(* +-----------------------------------------------------------------+
   | The libev engine                                                |
   +-----------------------------------------------------------------+ *)

type ev_io
type ev_signal
type ev_timer
type ev_suspend
type ev_resume

external ev_readable_init : Unix.file_descr -> (unit -> unit) -> ev_io = "lwt_libev_readable_init"
external ev_writable_init : Unix.file_descr -> (unit -> unit) -> ev_io = "lwt_libev_writable_init"
external ev_io_stop : ev_io -> unit = "lwt_libev_io_stop"
external ev_timer_init : float -> (unit -> unit) -> ev_timer = "lwt_libev_timer_init"
external ev_timer_stop : ev_timer -> unit  = "lwt_libev_timer_stop"
external ev_signal_init : int -> (unit -> unit) -> ev_signal = "lwt_libev_signal_init"
external ev_signal_stop : ev_signal -> unit  = "lwt_libev_signal_stop"
external ev_suspend_init : (unit -> unit) -> ev_suspend = "lwt_libev_suspend_init"
external ev_suspend_stop : ev_suspend -> unit = "lwt_libev_suspend_stop"
external ev_resume_init : (unit -> unit) -> ev_resume = "lwt_libev_resume_init"
external ev_resume_stop : ev_resume -> unit = "lwt_libev_resume_stop"

let libev_engine = {
  on_readable =
    (fun fd f ->
       let ev = ev_readable_init fd f in
       lazy(ev_io_stop ev));
  on_writable =
    (fun fd f ->
       let ev = ev_writable_init fd f in
       lazy(ev_io_stop ev));
  on_timer =
    (fun delay f ->
       let ev = ev_timer_init delay f in
       lazy(ev_timer_stop ev));
  on_signal =
    (fun signum f ->
       let ev = ev_signal_init signum f in
       lazy(ev_signal_stop ev));
  on_suspend =
    (fun f ->
       let ev = ev_suspend_init f in
       lazy(ev_suspend_stop ev));
  on_resume =
    (fun f ->
       let ev = ev_resume_init f in
       lazy(ev_resume_stop ev));
}

(* +-----------------------------------------------------------------+
   | The current engine                                              |
   +-----------------------------------------------------------------+ *)

let current = ref libev_engine
let get () = !current
let set engine = current := engine

let on_readable fd f = !current.on_readable fd f
let on_writable fd f = !current.on_writable fd f
let on_timer delay f = !current.on_timer delay f
let on_signal signum f = !current.on_signal signum f
let on_suspend f = !current.on_suspend f
let on_resume f = !current.on_resume f
