(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_exit_hook
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

(** Functions executed at program exit *)

(** This modules allow you to register functions launching threads
    which will be executed at the end of the program. *)

type t
  (** A hook identifier *)

val make : (unit -> unit Lwt.t) -> t
  (** [make f] creates an exit hook with callback function [f]. The
      exit hook is not added, it is just created. *)

val append : t -> unit
  (** [append hook] add the given hook after all previously defined
      hooks. *)

val prepend : t -> unit
  (** [prepend hook] add the given hook before all previously defined
      hooks. You should normally prefer [prepend] to [append]. *)

(** Notes:

    - hooks can be added at any point of the program, even in another
    hook

    - hooks are removed just before being called, so they are always
    executed exactly one time

    - exeception raised by hooks are always ignored *)

val get : t -> (unit -> unit Lwt.t)
  (** [get hook] returns the callback function of a hook *)

val set : t -> (unit -> unit Lwt.t) -> unit
  (** [set hook f] sets the callback function of a hook *)

val remove : t -> unit
  (** [remove id] remove a hook, without executing it. Hooks can be
      removed at any point of the program. *)

val exec : t -> unit Lwt.t
  (** [exec hook] is a short-hand for:

      {[
        remove hook;
        get hook ()
      ]} *)

val ensure_termination : unit Lwt.t -> unit
  (** [ensure_termination t] ensures the termination of the given
      thread before program termination. *)
