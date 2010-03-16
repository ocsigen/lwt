(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Test_lwt_event
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
       let stream = Lwt_event.to_stream event in
       let t =  Lwt_stream.next stream in
       assert (state t = Sleep);
       push 42;
       return (state t = Return 42));

  test "to_stream 2"
    (fun () ->
       let event, push = React.E.create () in
       let stream = Lwt_event.to_stream event in
       push 1;
       push 2;
       push 3;
       lwt l = Lwt_stream.nget 3 stream in
       return (l = [1; 2; 3]));

  test "with_finaliser"
    (fun () ->
       let b = ref false in
       let f () = b := true in
       let ev, push = React.E.create () in
       let _ = Lwt_event.with_finaliser f ev in
       Gc.full_major ();
       return !b);

  test "with_finaliser 2"
    (fun () ->
       let b = ref true in
       let f () = b := false in
       let ev, push = React.E.create () in
       let ev = Lwt_event.with_finaliser f ev in
       Gc.full_major ();
       let thread = Lwt_event.next ev in
       push 1;
       lwt n = thread in
       return (n = 1 && !b));

  test "map_s"
    (fun () ->
       let l = ref [] in
       let event, push = React.E.create () in
       let event' = Lwt_event.map_s (fun x -> l := x :: !l; return ()) event in
       ignore event';
       push 1;
       return (!l = [1]));

  test "map_s (gc)"
    (fun () ->
       let l = ref [] in
       let event, push = React.E.create () in
       let _ = Lwt_event.map_s (fun x -> l := x :: !l; return ()) event in
       Gc.full_major ();
       push 1;
       return (!l = []));

  test "map_p"
    (fun () ->
       let l = ref [] in
       let event, push = React.E.create () in
       let event' = Lwt_event.map_p (fun x -> l := x :: !l; return ()) event in
       ignore event';
       push 1;
       return (!l = [1]));

  test "map_p (gc)"
    (fun () ->
       let l = ref [] in
       let event, push = React.E.create () in
       let _ = Lwt_event.map_p (fun x -> l := x :: !l; return ()) event in
       Gc.full_major ();
       push 1;
       return (!l = []));

  test "of_stream"
    (fun () ->
       let stream, push = Lwt_stream.create () in
       let l = ref [] in
       let event = React.E.map (fun x -> l := x :: !l) (Lwt_event.of_stream stream) in
       ignore event;
       push (Some 1);
       push (Some 2);
       push (Some 3);
       Lwt.wakeup_paused ();
       return (!l = [3; 2; 1]));

  test "of_stream (gc)"
    (fun () ->
       let b = ref false in
       let f _ = b := true in
       let stream, push = Lwt_stream.create () in
       Gc.finalise f stream;
       let _ = Lwt_event.of_stream stream in
       Lwt.wakeup_paused ();
       Gc.full_major ();
       return !b);
]
