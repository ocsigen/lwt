(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_printf
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

(** Printf for lwt *)

val printf : ('a, unit, string, unit Lwt.t) format4 -> 'a
  (** [printf fmt ...] prints a string according to [fmt]. The string
      is first encoded to the system encoding before being printed. *)

val println : ('a, unit, string, unit Lwt.t) format4 -> 'a
  (** Same as {!printf} but also prints a newline after the string. *)

val eprintf : ('a, unit, string, unit Lwt.t) format4 -> 'a
  (** Same as {!printf} but prints on stderr *)

val eprintln : ('a, unit, string, unit Lwt.t) format4 -> 'a
  (** Same as {!println} but prints on stderr *)

val fprintf : Lwt_io.oc -> ('a, unit, string, unit Lwt.t) format4 -> 'a
  (** Same as {!print} but prints on the given channel. *)

val fprintln : Lwt_io.oc -> ('a, unit, string, unit Lwt.t) format4 -> 'a
  (** Same as {!println} but prints on the given channel. *)
