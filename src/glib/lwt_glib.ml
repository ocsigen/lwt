(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module glib
 * Copyright (C) 2009-2011 Jérémie Dimino
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

type source = {
  fd : Unix.file_descr;
  check_readable : bool;
  check_writable : bool;
}

external glib_init : unit -> unit = "lwt_glib_init"
external glib_stop : unit -> unit = "lwt_glib_stop"
external glib_get_sources : unit -> source array * float = "lwt_glib_get_sources"
external glib_check : unit -> unit = "lwt_glib_check"
external glib_mark_readable : int -> unit = "lwt_glib_mark_readable" "noalloc"
external glib_mark_writable : int -> unit = "lwt_glib_mark_readable" "noalloc"

let state = ref None
let events = ref []

let enter () =
  assert (!events = []);
  let sources, timeout = glib_get_sources () in
  for i = 0 to Array.length sources - 1 do
    let src = sources.(i) in
    if src.check_readable then
      events := Lwt_engine.on_readable src.fd (fun () -> glib_mark_readable i) :: !events;
    if src.check_writable then
      events := Lwt_engine.on_writable src.fd (fun () -> glib_mark_writable i) :: !events
  done;
  if timeout = 0. then
    ignore (Lwt_main.yield ())
  else if timeout > 0. then
    events := Lwt_engine.on_timer timeout ignore :: !events

let leave () =
  List.iter Lwt_engine.stop_event !events;
  events := [];
  glib_check ()

let install () =
  match !state with
    | Some _ ->
        ()
    | None ->
        glib_init ();
        state := Some(Lwt_sequence.add_l enter Lwt_main.enter_iter_hooks,
                      Lwt_sequence.add_l leave Lwt_main.leave_iter_hooks)

let remove () =
  match !state with
    | Some(node_enter, node_leave) ->
        state := None;
        Lwt_sequence.remove node_enter;
        Lwt_sequence.remove node_leave;
        List.iter Lwt_engine.stop_event !events;
        events := [];
        glib_stop ()
    | None ->
        ()
