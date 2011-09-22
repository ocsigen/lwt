(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Interface Lwt_glib
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

(** This module allow to use Lwt in GTK applications.

    Here is what you have to do to make Lwt and GTK work together:
    - call {!install} at the beginning of your program (before or
    after GMain.init, it does not matter)
    - do not call GMain.main, write your application as a normal Lwt
    application instead.

    For example:

    {[
      let () = Lwt_main.run (
        (* Initializes GTK. *)
        ignore (GMain.init ());

        (* Install Lwt<->Glib integration. *)
        Lwt_glib.install ();

        (* Thread which is wakeup when the main window is closed. *)
        let waiter, wakener = Lwt.wait () in

        (* Create a window. *)
        let window = GWindow.window () in

        (* Display something inside the window. *)
        ignore (GMisc.label ~text:"Hello, world!" ~packing:window#add ());

        (* Quit when the window is closed. *)
        ignore (window#connect#destroy (Lwt.wakeup wakener));

        (* Show the window. *)
        window#show ();

        (* Wait for it to be closed. *)
        waiter
      )
    ]}
 *)

val install : ?mode : [ `glib_into_lwt | `lwt_into_glib ] -> unit -> unit
  (** Install the Glib<->Lwt integration.

      If [mode] is [`glib_into_lwt] then glib will use the Lwt main
      loop, and if [mode] is [`lwt_into_glib] then Lwt will use the
      Glib main loop.

      The first mode is better but for some unknown reason it does not
      work under Windows, so the second is used as default on Windows
      while the first one is used as default on Unix.

      If the integration is already active, this function does
      nothing. *)

val remove : unit -> unit
  (** Remove the Glib<->Lwt integration. *)

val iter : bool -> unit
  (** This function is not related to Lwt. [iter may_block] does the
      same as [Glib.Main.iteration may_block] but can safely be called
      in a multi-threaded program, it will not block the whole
      program.

      For example:

      {[
        let main () =
          while true do
            Lwt_glib.iter true
          done

        let thread = Thread.create main ()
      ]}

      Note: you can call this function only from one thread at a time,
      otherwise it will raise [Failure]. *)

val wakeup : unit -> unit
  (** If one thread is blocking on {!iter}, then [wakeup ()] make
      {!iter} to return immediatly. *)
