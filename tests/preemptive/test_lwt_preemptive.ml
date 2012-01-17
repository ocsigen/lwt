(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Test_lwt_io
 * Copyright (C) 2012 Jérémie Dimino <jeremie@dimino.org>
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
open Test

let suite = suite "lwt_preemptive" [
  test "run_in_main"
    (fun () ->
       let f () =
         Lwt_preemptive.run_in_main
           (fun () ->
              lwt () = Lwt_unix.sleep 0.01 in
              return 42)
       in
       lwt x = Lwt_preemptive.detach f () in
       return (x = 42));
]
