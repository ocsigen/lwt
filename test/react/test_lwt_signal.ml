(* OCaml promise library
 * http://www.ocsigen.org/lwt
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
  test "limit"
    (fun () ->
       let s, push = React.S.create 0 in
       let cond    = Lwt_condition.create () in
       let s'      = Lwt_react.S.limit (fun () -> Lwt_condition.wait cond) s in
       let l       = ref [] in
       let e       = React.E.map (fun x -> l := x :: !l) (React.S.changes s') in
         ignore e;
         Lwt_condition.signal cond ();
         push 1;
         push 0;
         push 2; (* overwrites previous 0 *)
         Lwt_condition.signal cond ();
         push 3;
         Lwt_condition.signal cond ();
         push 4;
         Lwt_condition.signal cond ();
         return (!l = [4; 3; 2; 1]));

]
