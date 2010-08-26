(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_switch
 * Copyright (C) 2010 Jérémie Dimino
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

type on_switch = {
  mutable hooks : (unit -> unit Lwt.t) list;
}

type state =
  | On of on_switch
  | Off

type t = { mutable state : state }

let create () = { state = On { hooks = [] } }

let is_on switch =
  match switch.state with
    | On _ -> true
    | Off -> false

let add_hook switch hook =
  match switch.state with
    | On os ->
        os.hooks <- hook :: os.hooks;
        return ()
    | Off ->
        hook ()

let turn_off switch =
  match switch.state with
    | On { hooks } ->
        switch.state <- Off;
        Lwt_list.iter_p (fun hook -> apply hook ()) hooks
    | Off ->
        return ()
