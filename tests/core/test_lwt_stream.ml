(* Lightweight thread library for OCaml
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
                                       Lwt_mvar.take mvar >>= fun x ->
                                       return (Some x)) in
       let t1 = Lwt_stream.next stream in
       let t2 = Lwt_stream.next stream in
       let t3 = Lwt_stream.next stream in
       Lwt_mvar.put mvar 1 >>= fun () ->
       t1 >>= fun x1 ->
       t2 >>= fun x2 ->
       t3 >>= fun x3 ->
       return ([x1; x2; x3] = [1; 1; 1]));

  test "of_list"
    (fun () ->
       let stream = Lwt_stream.of_list [1; 2; 3] in
       Lwt_stream.next stream >>= fun x1 ->
       Lwt_stream.next stream >>= fun x2 ->
       Lwt_stream.next stream >>= fun x3 ->
       return ([x1; x2; x3] = [1; 2; 3]));

  test "clone"
    (fun () ->
       let stream1 = Lwt_stream.of_list [1; 2; 3] in
       let stream2 = Lwt_stream.clone stream1 in
       Lwt_stream.next stream1 >>= fun x1_1 ->
       Lwt_stream.next stream2 >>= fun x2_1 ->
       Lwt_stream.next stream1 >>= fun x1_2 ->
       Lwt_stream.next stream1 >>= fun x1_3 ->
       Lwt_stream.next stream2 >>= fun x2_2 ->
       Lwt_stream.next stream2 >>= fun x2_3 ->
       return ([x1_1; x1_2; x1_3] = [1; 2; 3] && [x2_1; x2_2; x2_3] = [1; 2; 3]));

  test "clone 2"
    (fun () ->
       let stream1, push = Lwt_stream.create () in
       push (Some 1);
       let stream2 = Lwt_stream.clone stream1 in
       let x1_1 = poll (Lwt_stream.next stream1) in
       let x1_2 = poll (Lwt_stream.next stream1) in
       let x2_1 = poll (Lwt_stream.next stream2) in
       let x2_2 = poll (Lwt_stream.next stream2) in
       return ([x1_1;x1_2;x2_1;x2_2] = [Some 1;None;Some 1;None]));

  test "create"
    (fun () ->
       let stream, push = Lwt_stream.create () in
       push (Some 1);
       push (Some 2);
       push (Some 3);
       push None;
       Lwt_stream.to_list stream >>= fun l ->
       return (l = [1; 2; 3]));

  test "create 2"
    (fun () ->
       let stream, push = Lwt_stream.create () in
       push None;
       let t = Lwt_stream.next stream in
       return (Lwt.state t = Fail Lwt_stream.Empty));

  test "create_bounded"
    (fun () ->
       let stream, push = Lwt_stream.create_bounded 3 in
       let acc = true in
       let acc = acc && state (push#push 1) = Return () in
       let acc = acc && state (push#push 2) = Return () in
       let acc = acc && state (push#push 3) = Return () in
       let t = push#push 4 in
       let acc = acc && state t = Sleep in
       let acc = acc && state (push#push 5) = Fail Lwt_stream.Full in
       let acc = acc && state (push#push 6) = Fail Lwt_stream.Full in
       let acc = acc && state (Lwt_stream.get stream) = Return (Some 1) in
       (* Lwt_stream uses wakeup_later so we have to wait a bit. *)
       Lwt_unix.yield () >>= fun () ->
       let acc = acc && state t = Return () in
       let acc = acc && state (Lwt_stream.get stream) = Return (Some 2) in
       let acc = acc && state (push#push 7) = Return () in
       push#close;
       let acc = acc && state (push#push 8) = Fail Lwt_stream.Closed in
       let acc = acc && state (Lwt_stream.to_list stream) = Return [3; 4; 7] in
       return acc);

  test "create_bounded close"
    (fun () ->
       let stream, push = Lwt_stream.create_bounded 1 in
       let acc = true in
       let acc = acc && state (push#push 1) = Return () in
       let iter_delayed = Lwt_stream.to_list stream in
       Lwt_unix.yield () >>= fun () ->
       push#close;
       Lwt_unix.yield () >>= fun () ->
       let acc = acc && state iter_delayed = Return [1] in
       return acc
    );

  test "get_while"
    (fun () ->
       let stream = Lwt_stream.of_list [1; 2; 3; 4; 5] in
       Lwt_stream.get_while (fun x -> x < 3) stream >>= fun l1 ->
       Lwt_stream.to_list stream >>= fun l2 ->
       return (l1 = [1; 2] && l2 = [3; 4; 5]));

  test "peek"
    (fun () ->
       let stream = Lwt_stream.of_list [1; 2; 3; 4; 5] in
       Lwt_stream.peek stream >>= fun x ->
       Lwt_stream.peek stream >>= fun y ->
       Lwt_stream.to_list stream >>= fun l ->
       return (x = Some 1 && y = Some 1 && l = [1; 2; 3; 4; 5]));

  test "npeek"
    (fun () ->
       let stream = Lwt_stream.of_list [1; 2; 3; 4; 5] in
       Lwt_stream.npeek 3 stream >>= fun x ->
       Lwt_stream.npeek 1 stream >>= fun y ->
       Lwt_stream.to_list stream >>= fun l ->
       return (x = [1; 2; 3] && y = [1] && l = [1; 2; 3; 4; 5]));

  test "get_available"
    (fun () ->
       let stream, push = Lwt_stream.create () in
       push (Some 1);
       push (Some 2);
       push (Some 3);
       let l = Lwt_stream.get_available stream in
       push (Some 4);
       Lwt_stream.get stream >>= fun x ->
       return (l = [1; 2; 3] && x = Some 4));

  test "get_available_up_to"
    (fun () ->
       let stream, push = Lwt_stream.create () in
       push (Some 1);
       push (Some 2);
       push (Some 3);
       push (Some 4);
       let l = Lwt_stream.get_available_up_to 2 stream in
       Lwt_stream.get stream >>= fun x ->
       return (l = [1; 2] && x = Some 3));

  test "filter"
    (fun () ->
       let stream, push = Lwt_stream.create () in
       push (Some 1);
       push (Some 2);
       push (Some 3);
       push (Some 4);
       let filtered = Lwt_stream.filter ((=) 3) stream in
       Lwt_stream.get filtered >>= fun x ->
       let l = Lwt_stream.get_available filtered in
       return (x = Some 3 && l = []));

  test "filter_map"
    (fun () ->
       let stream, push = Lwt_stream.create () in
       push (Some 1);
       push (Some 2);
       push (Some 3);
       push (Some 4);
       let filtered = Lwt_stream.filter_map (function 3 ->  Some "3" | _ -> None ) stream in
       Lwt_stream.get filtered >>= fun x ->
       let l = Lwt_stream.get_available filtered in
       return (x = Some "3" && l = []));

  test "last_new"
    (fun () ->
       let stream, push = Lwt_stream.create () in
       push (Some 1);
       push (Some 2);
       push (Some 3);
       Lwt_stream.last_new stream >>= fun x ->
       return (x = 3));

  test "cancel push stream 1"
    (fun () ->
       let stream, push = Lwt_stream.create () in
       let t = Lwt_stream.next stream in
       cancel t;
       return (state t = Fail Canceled));

  test "cancel push stream 2"
    (fun () ->
       let stream, push = Lwt_stream.create () in
       let t = Lwt_stream.next stream in
       cancel t;
       push (Some 1);
       let t' = Lwt_stream.next stream in
       return (state t' = Return 1));

  test "cancel push stream 3"
    (fun () ->
       let stream, push = Lwt_stream.create () in
       let t1 = Lwt_stream.next stream in
       let t2 = Lwt_stream.next stream in
       cancel t1;
       push (Some 1);
       return (state t1 = Fail Canceled && state t2 = Return 1));

  (* check if the push function keeps references to the elements in
     the stream *)
  test "push and GC"
    (fun () ->
       let w = Weak.create 5 in
       (* Count the number of reachable elements in the stream. *)
       let count () =
         let rec loop acc idx =
           if idx = Weak.length w then
             acc
           else
             match Weak.get w idx with
               | None -> loop acc (idx + 1)
               | Some v -> loop (acc + 1) (idx + 1)
         in
         loop 0 0
       in
       (* Run some test and return the push function of the stream. *)
       let test () =
         let stream, push = Lwt_stream.create () in
         assert (count () = 0);
         let r1 = Some(ref 1) in
         push r1;
         Weak.set w 1 r1;
         let r2 = Some(ref 2) in
         push r2;
         Weak.set w 2 r2;
         let r3 = Some(ref 3) in
         push r3;
         Weak.set w 3 r3;
         assert (count () = 3);
         assert (state (Lwt_stream.next stream) = Return {contents = 1});
         Gc.full_major ();
         (* Ocaml can consider that stream is unreachable before the
            next line, hence freeing the whole data. *)
         assert (count () <= 3);
         push
       in
       let push = test () in
       Gc.full_major ();
       (* At this point [stream] is unreachable. *)
       assert (count () = 0);
       (* We have that to force caml to keep a reference on [push]. *)
       push (Some(ref 4));
       return true);

  test "map_exn"
    (fun () ->
       let open Lwt_stream in
       let l = [Value 1; Error Exit; Error (Failure "plop"); Value 42; Error End_of_file] in
       let q = ref l in
       let stream =
         Lwt_stream.from
           (fun () ->
              match !q with
                | [] ->
                    return None
                | Value x :: l ->
                    q := l;
                    return (Some x)
                | Error e :: l ->
                    q := l;
                    Lwt.fail e)
       in
       Lwt_stream.to_list (Lwt_stream.map_exn stream) >>= fun l' ->
       return (l = l'));

  test "is_closed"
    (fun () ->
      let st = Lwt_stream.of_list [1; 2] in
      let b1 = not (Lwt_stream.is_closed st) in
      ignore (Lwt_stream.peek st);
      let b2 = not (Lwt_stream.is_closed st) in
      ignore (Lwt_stream.junk st);
      ignore (Lwt_stream.peek st);
      let b3 = not (Lwt_stream.is_closed st) in
      ignore (Lwt_stream.junk st);
      ignore (Lwt_stream.peek st);
      let b4 = Lwt_stream.is_closed st in
      Lwt.return (b1 && b2 && b3 && b4));

  test "closed"
    (fun () ->
      let st = Lwt_stream.of_list [1; 2] in
      let b = ref false in
      let is_closed_in_notification = ref false in
      Lwt.async (fun () ->
        Lwt_stream.closed st >|= fun () ->
        b := true;
        is_closed_in_notification := Lwt_stream.is_closed st);
      ignore (Lwt_stream.peek st);
      let b1 = !b = false in
      ignore (Lwt_stream.junk st);
      ignore (Lwt_stream.peek st);
      let b2 = !b = false in
      ignore (Lwt_stream.junk st);
      ignore (Lwt_stream.peek st);
      let b3 = !b = true in
      Lwt.return (b1 && b2 && b3 && !is_closed_in_notification));

  test "on_termination"
    (fun () ->
      let st = Lwt_stream.of_list [1; 2] in
      let b = ref false in
      Lwt_stream.on_termination st (fun () -> b := true);
      ignore (Lwt_stream.peek st);
      let b1 = !b = false in
      ignore (Lwt_stream.junk st);
      ignore (Lwt_stream.peek st);
      let b2 = !b = false in
      ignore (Lwt_stream.junk st);
      ignore (Lwt_stream.peek st);
      let b3 = !b = true in
      Lwt.return (b1 && b2 && b3));

  test "on_termination when closed"
    (fun () ->
      let st = Lwt_stream.of_list [] in
      let b = ref false in
      let b1 = not (Lwt_stream.is_closed st) in
      ignore (Lwt_stream.junk st);
      let b2 = Lwt_stream.is_closed st in
      Lwt_stream.on_termination st (fun () -> b := true);
      Lwt.return (b1 && b2 && !b));

  test "choose_exhausted"
    (fun () ->
      Lwt_stream.(to_list (choose [of_list []])) >|= fun _ -> true);
]
