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

let suite = suite "lwt_event" [
  test "to_stream"
    (fun () ->
       let event, push = React.E.create () in
       let stream = Lwt_react.E.to_stream event in
       let t =  Lwt_stream.next stream in
       assert (state t = Sleep);
       push 42;
       return (state t = Return 42));

  test "to_stream 2"
    (fun () ->
       let event, push = React.E.create () in
       let stream = Lwt_react.E.to_stream event in
       push 1;
       push 2;
       push 3;
       Lwt.bind (Lwt_stream.nget 3 stream) (fun l ->
       return (l = [1; 2; 3])));

  test "map_s"
    (fun () ->
       let l = ref [] in
       let event, push = React.E.create () in
       let event' = Lwt_react.E.map_s (fun x -> l := x :: !l; return ()) event in
       ignore event';
       push 1;
       return (!l = [1]));

  test "map_p"
    (fun () ->
       let l = ref [] in
       let event, push = React.E.create () in
       let event' = Lwt_react.E.map_p (fun x -> l := x :: !l; return ()) event in
       ignore event';
       push 1;
       return (!l = [1]));

  test "of_stream"
    (fun () ->
       let stream, push = Lwt_stream.create () in
       let l = ref [] in
       let event = React.E.map (fun x -> l := x :: !l) (Lwt_react.E.of_stream stream) in
       ignore event;
       push (Some 1);
       push (Some 2);
       push (Some 3);
       Lwt.wakeup_paused ();
       return (!l = [3; 2; 1]));

  test "limit"
    (fun () ->
       let event, push = React.E.create () in
       let cond        = Lwt_condition.create () in
       let event'      = Lwt_react.E.limit (fun () -> Lwt_condition.wait cond) event in
       let l           = ref [] in
       let event''     = React.E.map (fun x -> l := x :: !l) event' in
         ignore event';
         ignore event'';
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
