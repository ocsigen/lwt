(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Test_lwt_signal
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

open Test
open Lwt

let suite = suite "lwt_signal" [
  test "with_finaliser"
    (fun () ->
       let b = ref false in
       let f () = b := true in
       let signal, set = React.S.create 0 in
       let _ = Lwt_signal.with_finaliser f signal in
       Gc.full_major ();
       return !b);

  test "with_finaliser 2"
    (fun () ->
       let b = ref true in
       let f () = b := false in
       let signal, set = React.S.create 0 in
       let signal = Lwt_signal.with_finaliser f signal in
       Gc.full_major ();
       return (React.S.value signal = 0 && !b));
]
