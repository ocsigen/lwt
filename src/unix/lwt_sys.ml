(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_sys
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

#include "src/unix/lwt_config.ml"

exception Not_available of string

let () = Callback.register_exception "lwt:not-available" (Not_available "")

let windows = Sys.os_type <> "Unix"

type feature =
    [ `wait4
    | `get_cpu
    | `get_affinity
    | `set_affinity
    | `recv_msg
    | `send_msg
    | `fd_passing
    | `get_credentials
    | `mincore
    | `madvise
    | `fdatasync ]

let have = function
  | `wait4
  | `recv_msg
  | `send_msg
  | `mincore
  | `madvise -> not windows
  | `get_cpu -> <:optcomp< HAVE_GETCPU >>
  | `get_affinity
  | `set_affinity -> <:optcomp< HAVE_AFFINITY >>
  | `fd_passing -> <:optcomp< HAVE_FD_PASSING >>
  | `get_credentials -> <:optcomp< HAVE_GET_CREDENTIALS >>
  | `fdatasync -> <:optcomp< HAVE_FDATASYNC >>

type byte_order = Little_endian | Big_endian

external get_byte_order : unit -> byte_order = "lwt_unix_system_byte_order"

let byte_order = get_byte_order ()
