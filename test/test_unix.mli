(* OCaml promise library
 * http://www.ocsigen.org/lwt
 * Copyright (C) 2017 Anton Bachin
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



val temp_name : unit -> string
(** Generates the name of a temporary file (or directory) in [_build/]. Note
    that a file at the path may already exist. *)

val temp_file : unit -> string
(** Creates a temporary file in [_build/] and evaluates to its path. *)

val temp_directory : unit -> string
(** Creates a temporary directory in [build/] and evaluates to its path. *)
