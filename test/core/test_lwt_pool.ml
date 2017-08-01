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
  suite "lwt_pool" [
     test "exception during validation"
       (fun () ->
          let c = Lwt_condition.create () in
          let gen = (fun () -> let l = ref 0 in Lwt.return l) in
          let v l = if !l = 0 then Lwt.return true else Lwt.fail Dummy_error in
          let p = Lwt_pool.create 1 ~validate:v gen in
          let u1 = Lwt_pool.use p (fun l -> l := 1; Lwt_condition.wait c) in
          let u2 = Lwt_pool.use p (fun l -> Lwt.return !l) in
          let () = Lwt_condition.signal c "done" in
          Lwt.return (Lwt.state u1 = Lwt.Return "done"
                      && Lwt.state u2 = Lwt.Fail Dummy_error)
       )
   ]
