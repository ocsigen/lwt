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

type state =
  | State_none
  | State_glib_into_lwt of (unit -> unit) Lwt_sequence.node * (unit -> unit) Lwt_sequence.node
  | State_lwt_into_glib of Lwt_engine.t

let state = ref State_none

(* +-----------------------------------------------------------------+
   | Glib-based engine                                               |
   +-----------------------------------------------------------------+ *)

external glib_poll : (Unix.file_descr * bool * bool) list -> int -> int -> (Unix.file_descr * bool * bool) list = "lwt_glib_poll"

class engine = object
  inherit Lwt_engine.poll_based
  method private poll fds timeout = glib_poll fds (List.length fds) (truncate (timeout *. 1000.))
end

(* +-----------------------------------------------------------------+
   | Glib --> Lwt based integration                                  |
   +-----------------------------------------------------------------+ *)

external glib_get_sources : unit -> source array * float = "lwt_glib_get_sources"
external glib_check : unit -> unit = "lwt_glib_check"
external glib_mark_readable : int -> unit = "lwt_glib_mark_readable" "noalloc"
external glib_mark_writable : int -> unit = "lwt_glib_mark_readable" "noalloc"

let events = ref []
let check = ref true

let enter () =
  if !check then begin
    check := false;
    let engine = Lwt_engine.get () in
    assert (!events = []);
    let sources, timeout = glib_get_sources () in
    for i = 0 to Array.length sources - 1 do
      let src = sources.(i) in
      if src.check_readable then
        events := engine#on_readable src.fd (fun _ -> glib_mark_readable i) :: !events;
      if src.check_writable then
        events := engine#on_writable src.fd (fun _ -> glib_mark_writable i) :: !events
    done;
    if timeout = 0. then
      ignore (Lwt_main.yield ())
    else if timeout > 0. then
      events := engine#on_timer timeout false ignore :: !events
  end

let leave () =
  if not !check then begin
    check := true;
    List.iter Lwt_engine.stop_event !events;
    events := [];
    glib_check ()
  end

(* +-----------------------------------------------------------------+
   | Installation/removal                                            |
   +-----------------------------------------------------------------+ *)

let install ?mode () =
  match !state with
    | State_lwt_into_glib _ | State_glib_into_lwt _ ->
        ()
    | State_none ->
        let mode =
          match mode with
            | Some mode -> mode
            | None -> if Lwt_sys.windows then `lwt_into_glib else `glib_into_lwt
        in
        glib_init ();
        match mode with
          | `glib_into_lwt ->
              state := State_glib_into_lwt(Lwt_sequence.add_l enter Lwt_main.enter_iter_hooks,
                                           Lwt_sequence.add_l leave Lwt_main.leave_iter_hooks)
          | `lwt_into_glib ->
              let engine = Lwt_engine.get () in
              Lwt_engine.set ~destroy:false (new engine);
              state := State_lwt_into_glib engine

let remove () =
  match !state with
    | State_none ->
        ()
    | State_glib_into_lwt(node_enter, node_leave) ->
        state := State_none;
        Lwt_sequence.remove node_enter;
        Lwt_sequence.remove node_leave;
        List.iter Lwt_engine.stop_event !events;
        events := [];
        glib_stop ()
    | State_lwt_into_glib engine ->
        Lwt_engine.set engine

(* +-----------------------------------------------------------------+
   | Misc                                                            |
   +-----------------------------------------------------------------+ *)

external iter : bool -> unit = "lwt_glib_iter"
external wakeup : unit -> unit = "lwt_glib_wakeup"
