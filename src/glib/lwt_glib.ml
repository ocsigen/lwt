(* Lightweight thread library for OCaml
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
  method private poll fds timeout = glib_poll fds (List.length fds) (max (-1) (truncate (timeout *. 1000.)))
end

(* +-----------------------------------------------------------------+
   | Glib --> Lwt based integration                                  |
   +-----------------------------------------------------------------+ *)

[@@@ocaml.warning "-37"]
type watch =
  | Watch_none
  | Watch_in
  | Watch_out
  | Watch_in_out
[@@@ocaml.warning "+37"]

external glib_get_sources : unit -> Unix.file_descr array * watch array * float = "lwt_glib_get_sources"
external glib_check : unit -> unit = "lwt_glib_check"
[@@@ocaml.warning "-3"]
external glib_mark_readable : int -> unit = "lwt_glib_mark_readable" "noalloc"
external glib_mark_writable : int -> unit = "lwt_glib_mark_readable" "noalloc"
[@@@ocaml.warning "+3"]

let events = ref []
let check = ref true

let enter () =
  if !check then begin
    check := false;
    let engine = Lwt_engine.get () in
    assert (!events = []);
    let fds, watches, timeout = glib_get_sources () in
    for i = 0 to Array.length fds - 1 do
      let fd = fds.(i) in
      match watches.(i) with
        | Watch_none ->
            ()
        | Watch_in ->
            events := engine#on_readable fd (fun _ -> glib_mark_readable i) :: !events
        | Watch_out ->
            events := engine#on_writable fd (fun _ -> glib_mark_writable i) :: !events
        | Watch_in_out ->
            events := engine#on_readable fd (fun _ -> glib_mark_readable i)
                   :: engine#on_writable fd (fun _ -> glib_mark_writable i)
                   :: !events
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

let install ?(mode=`lwt_into_glib) () =
  match !state with
    | State_lwt_into_glib _ | State_glib_into_lwt _ ->
        ()
    | State_none ->
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
