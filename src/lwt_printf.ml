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

open Lwt
open Lwt_io

let fprintf oc fmt =
  Printf.ksprintf (write_text oc) fmt

let fprintln oc fmt =
  Printf.ksprintf (fun str ->
                     atomic
                       (fun oc ->
                          write_text oc str;
                          >> write_text oc (if Lwt_term.raw_mode () then "\r\n" else "\n"))
                       oc) fmt

let printf fmt = fprintf stdout fmt
let println fmt = fprintln stdout fmt
let eprintf fmt = fprintf stderr fmt
let eprintln fmt = fprintln stderr fmt
