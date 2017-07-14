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
open Lwt

let suite =
  suite "lwt_mvar" [
    test "basic take"
      (fun () ->
         let x = Lwt_mvar.create 0 in
         let y = Lwt_mvar.take x in
         return (y = Lwt.return 0)
      );

    test "blocking put"
      (fun () ->
         let x = Lwt_mvar.create 0 in
         let y = Lwt_mvar.put x 1 >>= (fun _ -> return 1) in
         return (y != Lwt.return 1)
      );

    test "put-take"
      (fun () ->
         let x = Lwt_mvar.create_empty () in
         let z = Lwt_mvar.put x 0 >>= (fun _ -> Lwt_mvar.take x) in
         return (z = Lwt.return 0)
      );

    test "take-put"
      (fun () ->
         let x = Lwt_mvar.create 0 in
         let _ = Lwt_mvar.take x in
         let z = Lwt_mvar.put x 1 >>= (fun _ -> return 2) in
         return (z = Lwt.return 2)
      );

    test "enqueued writer"
      (fun () ->
         let x = Lwt_mvar.create 1 in
         let _ = Lwt_mvar.put x 2 in
         let y =
           Lwt_mvar.take x >>= (fun v1 ->
           Lwt_mvar.take x >>= (fun v2 ->
             Lwt.return (v1 + v2))) in
         return (y = Lwt.return 3)
      );

    test "writer cancellation"
      (fun () ->
         let x = Lwt_mvar.create_empty () in
         let y = Lwt_mvar.create 1 in
         let t, _ = Lwt.task () in
         let r1 = t >>= (fun _ -> Lwt_mvar.put y 2) in
         let after_cancel () =
           let _ = Lwt_mvar.take y >>= (fun z -> Lwt_mvar.put x z); in
           () in
         let _ = Lwt.on_cancel r1 after_cancel in
         Lwt.cancel r1;
         return (Lwt_mvar.take x = Lwt.return 1)
      );

    test "reader cancellation"
      (fun () ->
         let x = Lwt_mvar.create_empty () in
         let t, _ = Lwt.task () in
         let r1 = t >>= (fun _ -> Lwt_mvar.take x) in
         let r2 = Lwt_mvar.take x in
         let _ = Lwt.on_cancel r1 (fun _ -> let _ = Lwt_mvar.put x 1; in ()) in
         Lwt.cancel r1;
         return (r2 = Lwt.return 1)
      );
  ]
