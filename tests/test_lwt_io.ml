(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Test_lwt_io
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
open Test

let suite = suite "lwt_io" [
  test "auto-flush"
    (fun () ->
       let sent = ref [] in
       let oc = Lwt_io.make ~mode:output (fun buf ofs len ->
                                            sent := String.sub buf ofs len :: !sent;
                                            return len) in
       lwt () = write oc "foo" in
       lwt () = write oc "bar" in
       if !sent <> [] then
         return false
       else
         lwt () = Lwt_unix.yield () in
         return (!sent = ["foobar"]));

  test "auto-flush in atomic"
    (fun () ->
       let sent = ref [] in
       let oc = make ~mode:output (fun buf ofs len ->
                                     sent := String.sub buf ofs len :: !sent;
                                     return len) in
       atomic
         (fun oc ->
            lwt () = write oc "foo" in
            lwt () = write oc "bar" in
            if !sent <> [] then
              return false
            else
              lwt () = Lwt_unix.yield () in
              return (!sent = ["foobar"]))
         oc);
]
