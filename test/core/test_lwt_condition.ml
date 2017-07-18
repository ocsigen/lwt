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

exception Dummy_error

let suite =
  suite "lwt_condition" [
    test "basic wait"
      (fun () ->
         let c = Lwt_condition.create () in
         let w = Lwt_condition.wait c in
         let () = Lwt_condition.signal c 1 in
         Lwt.return (Lwt.state w = Lwt.Return 1)
      );

    test "mutex unlocked during wait"
      (fun () ->
         let c = Lwt_condition.create () in
         let m = Lwt_mutex.create () in
         let _ = Lwt_mutex.lock m in
         let w = Lwt_condition.wait ~mutex:m c in
         Lwt.return (Lwt.state w = Lwt.Sleep
                     && not (Lwt_mutex.is_locked m))
      );

    test "mutex relocked after wait"
      (fun () ->
         let c = Lwt_condition.create () in
         let m = Lwt_mutex.create () in
         let _ = Lwt_mutex.lock m in
         let w = Lwt_condition.wait ~mutex:m c in
         let () = Lwt_condition.signal c 1 in
         Lwt.return (Lwt.state w = Lwt.Return 1 && Lwt_mutex.is_locked m)
      );

    test "signal is not sticky"
      (fun () ->
         let c = Lwt_condition.create () in
         let () = Lwt_condition.signal c 1 in
         let w = Lwt_condition.wait c in
         Lwt.return (Lwt.state w = Lwt.Sleep));

    test "broadcast"
      (fun () ->
         let c = Lwt_condition.create () in
         let w1 = Lwt_condition.wait c in
         let w2 = Lwt_condition.wait c in
         let () = Lwt_condition.broadcast c 1 in
         Lwt.return (Lwt.state w1 = Lwt.Return 1
                     && Lwt.state w2 = Lwt.Return 1)
      );

    test "broadcast exception"
      (fun () ->
         let c = Lwt_condition.create () in
         let w1 = Lwt_condition.wait c in
         let w2 = Lwt_condition.wait c in
         let () = Lwt_condition.broadcast_exn c Dummy_error in
         Lwt.return (Lwt.state w1 = Lwt.Fail Dummy_error
                     && Lwt.state w2 = Lwt.Fail Dummy_error)
      );
  ]
