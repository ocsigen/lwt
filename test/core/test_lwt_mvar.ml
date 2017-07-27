(* OCaml promise library
 * http://www.ocsigen.org/lwt
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

let suite =
  suite "lwt_mvar" [
    test "basic take"
      (fun () ->
         let x = Lwt_mvar.create 0 in
         let y = Lwt_mvar.take x in
         Lwt.return (Lwt.state y = Lwt.Return 0)
      );

    test "take_available (full)"
      (fun () ->
         let x = Lwt_mvar.create 0 in
         let y = Lwt_mvar.take_available x in
         Lwt.return (y = Some 0)
      );

    test "take_available (empty)"
      (fun () ->
         let x = Lwt_mvar.create_empty () in
         let y = Lwt_mvar.take_available x in
         Lwt.return (y = None)
      );

    test "is_empty (full)"
      (fun () ->
         let x = Lwt_mvar.create 0 in
         let y = Lwt_mvar.is_empty x in
         Lwt.return (not y)
      );

    test "is_empty (empty)"
      (fun () ->
         let x = Lwt_mvar.create_empty () in
         let y = Lwt_mvar.is_empty x in
         Lwt.return y
      );

    test "blocking put"
      (fun () ->
         let x = Lwt_mvar.create 0 in
         let y = Lwt_mvar.put x 1 in
         Lwt.return (Lwt.state y = Lwt.Sleep)
      );

    test "put-take"
      (fun () ->
         let x = Lwt_mvar.create_empty () in
         let _ = Lwt_mvar.put x 0 in
         let y = Lwt_mvar.take x in
         Lwt.return (Lwt.state y = Lwt.Return 0)
      );

    test "take-put"
      (fun () ->
         let x = Lwt_mvar.create 0 in
         let _ = Lwt_mvar.take x in
         let y = Lwt_mvar.put x 1 in
         Lwt.return (Lwt.state y = Lwt.Return ())
      );

    test "enqueued writer"
      (fun () ->
         let x = Lwt_mvar.create 1 in
         let y = Lwt_mvar.put x 2 in
         let z = Lwt_mvar.take x in
         Lwt.return (Lwt.state y = Lwt.Return ()
                     && Lwt.state z = Lwt.Return 1)
      );

    test "writer cancellation"
      (fun () ->
         let y = Lwt_mvar.create 1 in
         let r1 = Lwt_mvar.put y 2 in
         Lwt.cancel r1;
         Lwt.return ((Lwt.state (Lwt_mvar.take y) = Lwt.Return 1)
                     && (Lwt.state (Lwt_mvar.take y) = Lwt.Sleep)));
  ]
