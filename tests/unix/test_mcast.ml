(* Lightweight thread library for OCaml
 * http://www.ocsigen.org/lwt
 * Module Test_mcast
 * Copyright (C) 2015 Nicolas Ojeda Bar
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

open Lwt.Infix
open Test

let hello = Bytes.unsafe_of_string "Hello, World!"
let mcast_addr = "225.0.0.1"
let mcast_port = 4321

let with_fd f =
  let fd = Lwt_unix.(socket PF_INET SOCK_DGRAM 0) in
  Lwt.finalize (fun () -> f fd) (fun () -> Lwt_unix.close fd)

let child join fd =
  (* Lwt_unix.setsockopt fd Lwt_unix.SO_REUSEADDR true; *)
  Lwt_unix.(bind fd (ADDR_INET (Unix.inet_addr_any, mcast_port)));
  if join then Lwt_unix.mcast_add_membership fd (Unix.inet_addr_of_string mcast_addr);
  let buf = Bytes.create 50 in
  Lwt_unix.read fd buf 0 50 >>= fun n ->
  (* Printf.printf "\nReceived multicast message %S\n%!" (Bytes.unsafe_to_string (Bytes.sub buf 0 n)); *)
  if Bytes.sub buf 0 n <> hello then
    Lwt.fail (Failure "unexpected multicast message")
  else
    Lwt.return_unit

let parent set_loop fd =
  Lwt_unix.mcast_set_loop fd set_loop;
  let rec loop () =
    Lwt_unix.(sendto fd hello 0 (Bytes.length hello) []
                (ADDR_INET (Unix.inet_addr_of_string mcast_addr, mcast_port))) >>= fun _ ->
    (* Printf.printf "\nSending multicast message %S to %s:%d\n%!" (Bytes.unsafe_to_string hello) *)
    (*   mcast_addr mcast_port; *)
    Lwt_unix.sleep 0.2 >>= loop
  in
  loop ()

let test_mcast join set_loop =
  let should_timeout = not join || not set_loop in
  let t = Lwt_unix.with_timeout 0.5 (fun () -> Lwt.pick [ with_fd (child join); with_fd (parent set_loop) ]) in
  Lwt.catch (fun () -> t >>= fun x -> Lwt.return (`Ok x)) (fun e -> Lwt.return (`Fail e)) >>= function
  | `Fail Lwt_unix.Timeout ->
    Lwt.return should_timeout
  | `Fail e ->
    Printf.eprintf "\ntest_mcast: unexpected failure: %S\n%!" (Printexc.to_string e);
    Lwt.return false
  | `Ok () ->
    Lwt.return true

let suite =
  suite "unix_mcast"
    [
      test "mcast-join-loop" (fun () -> test_mcast true true);
      test "mcast-nojoin-loop" (fun () -> test_mcast false true);
      test "mcast-join-noloop" (fun () -> test_mcast true false);
      test "mcast-nojoin-noloop" (fun () -> test_mcast false false);
    ]
