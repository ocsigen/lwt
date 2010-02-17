(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Test_lwt_stream
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
open Test

let suite = suite "lwt_stream" [
  test "from"
    (fun () ->
       let mvar = Lwt_mvar.create_empty () in
       let stream = Lwt_stream.from (fun () ->
                                       lwt x = Lwt_mvar.take mvar in
                                       return (Some x)) in
       let t1 = Lwt_stream.next stream in
       let t2 = Lwt_stream.next stream in
       let t3 = Lwt_stream.next stream in
       lwt () = Lwt_mvar.put mvar 1 in
       lwt () = Lwt_mvar.put mvar 2 in
       lwt () = Lwt_mvar.put mvar 3 in
       lwt x1 = t1 and x2 = t2 and x3 = t3 in
       return ([x1; x2; x3] = [1; 2; 3]));

  test "clone"
    (fun () ->
       let stream1 = Lwt_stream.of_list [1; 2; 3] in
       let stream2 = Lwt_stream.clone stream1 in
       lwt x1_1 = Lwt_stream.next stream1 in
       lwt x2_1 = Lwt_stream.next stream2 in
       lwt x1_2 = Lwt_stream.next stream1
       and x1_3 = Lwt_stream.next stream1
       and x2_2 = Lwt_stream.next stream2
       and x2_3 = Lwt_stream.next stream2 in
       return ([x1_1; x1_2; x1_3] = [1; 2; 3] && [x2_1; x2_2; x2_3] = [1; 2; 3]));

  test "event"
    (fun () ->
       let event, push = React.E.create () in
       let stream = Lwt_stream.of_event event in
       push 1;
       push 2;
       push 3;
       lwt l = Lwt_stream.nget 3 stream in
       return (l = [1; 2; 3]));

  test "push_stream"
    (fun () ->
       let push, stream = Lwt_stream.push_stream () in
       push (`Data 1);
       push (`Data 2);
       push (`Data 3);
       push `End_of_stream;
       lwt l = Lwt_stream.to_list stream in
       let result = l = [1; 2; 3] in
       let push, stream = Lwt_stream.push_stream () in
       push (`Exn Exit);
       let t = Lwt_stream.get stream in
       return (result && Lwt.state t = Fail Exit));

  test "get_while"
    (fun () ->
       let stream = Lwt_stream.of_list [1; 2; 3; 4; 5] in
       lwt l1 = Lwt_stream.get_while (fun x -> x < 3) stream in
       lwt l2 = Lwt_stream.to_list stream in
       return (l1 = [1; 2] && l2 = [3; 4; 5]));

  test "peek"
    (fun () ->
       let stream = Lwt_stream.of_list [1; 2; 3; 4; 5] in
       lwt x = Lwt_stream.peek stream in
       lwt y = Lwt_stream.peek stream in
       lwt l = Lwt_stream.to_list stream in
       return (x = Some 1 && y = Some 1 && l = [1; 2; 3; 4; 5]));

  test "npeek"
    (fun () ->
       let stream = Lwt_stream.of_list [1; 2; 3; 4; 5] in
       lwt x = Lwt_stream.npeek 3 stream in
       lwt y = Lwt_stream.npeek 1 stream in
       lwt l = Lwt_stream.to_list stream in
       return (x = [1; 2; 3] && y = [1] && l = [1; 2; 3; 4; 5]));

  test "get_available"
    (fun () ->
       let push, stream = Lwt_stream.push_stream () in
       push (`Data 1);
       push (`Data 2);
       push (`Data 3);
       let l = Lwt_stream.get_available stream in
       push (`Data 4);
       lwt x = Lwt_stream.get stream in
       return (l = [1; 2; 3] && x = Some 4));

  test "get_available_up_to"
    (fun () ->
       let push, stream = Lwt_stream.push_stream () in
       push (`Data 1);
       push (`Data 2);
       push (`Data 3);
       push (`Data 4);
       let l = Lwt_stream.get_available_up_to 2 stream in
       lwt x = Lwt_stream.get stream in
       return (l = [1; 2] && x = Some 3));

  test "filter"
    (fun () ->
       let push, stream = Lwt_stream.push_stream () in
       push (`Data 1);
       push (`Data 2);
       push (`Data 3);
       push (`Data 4);
       let filtered = Lwt_stream.filter ((=) 3) stream in
       lwt x = Lwt_stream.get filtered in
       let l = Lwt_stream.get_available filtered in
       return (x = Some 3 && l = []));
]
