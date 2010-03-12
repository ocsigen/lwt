(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_glib
 * Copyright (C) 2009 Jérémie Dimino
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

(** Glib integration *)

(** This modules is intended to allow the use of Lwt in GTK
    applications. It integrates the Lwt scheduler into the glib main
    loop so light-weight threadscan run while GTK is running. *)

val init : ?setlocale:bool -> unit -> unit
  (** Initialises glib and integrates the Lwt scheduler into the glib
      main loop. *)

val quit : unit -> unit
  (** Calls [GMain.quit] and remove the Lwt scheduler from the glib
      main loop *)
