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
  suite "lwt_condition" [
    test "basic wait"
      (fun () ->
         let c = Lwt_condition.create () in
         let w = Lwt_condition.wait c in
         let () = Lwt_condition.signal c 1 in
         return (w = Lwt.return 1)
      );

    test "mutex wait"
      (fun () ->
         let c = Lwt_condition.create () in
         let m = Lwt_mutex.create () in
         let _ = Lwt_mutex.lock m in
         let w = Lwt_condition.wait ~mutex:m c in
         let () = Lwt_condition.signal c 1 in
         return ((w = Lwt.return 1) && (Lwt_mutex.is_locked m))
      );
  ]
