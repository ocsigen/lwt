(* Lightweight thread library for OCaml
 * http://www.ocsigen.org/lwt
 * Program Relay
 * Copyright (C) 2011 Jérémie Dimino
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

(* Relay data from an address to another. *)

open Lwt.Infix

(* +-----------------------------------------------------------------+
   | Relaying                                                        |
   +-----------------------------------------------------------------+ *)

(* Write exactly [len] bytes from [buf] at [ofs]. *)
let rec write_exactly fd buf ofs len =
  let%lwt n = Lwt_bytes.write fd buf ofs len in
  if n = len then
    (* Everything has been written, do nothing. *)
    Lwt.return_unit
  else
    (* Write remaining data. *)
    write_exactly fd buf (ofs + n) (len - n)

(* Copy continously data from [in_fd] to [out_fd]. *)
let relay in_fd out_fd =
  (* Queue of data received but not yet written. *)
  let queue = Queue.create () in

  (* Condition used to signal the writer that some data are
     available. *)
  let cond = Lwt_condition.create () in

  (* Boolean which tells whether the input socket has been closed. *)
  let end_of_input = ref false in

  (* Write continously data received to [out_fd]. *)
  let rec loop_write () =
    if Queue.is_empty queue then
      if !end_of_input then
        (* End of input reached, exit. *)
        Lwt.return_unit
      else
        (* There is no data pending, wait for some. *)
        let%lwt () = Lwt_condition.wait cond in
        loop_write ()
    else
      let (buf, len) = Queue.take queue in
      let%lwt () = write_exactly out_fd buf 0 len in
      loop_write ()
  in

  (* Start the writer. *)
  let writer = loop_write () in

  (* Read continously from [in_fd]. *)
  let rec loop_read () =
    let buf = Lwt_bytes.create 8192 in
    match%lwt Lwt_bytes.read in_fd buf 0 8192 with
      | 0 ->
          (* If we read nothing, this means that the connection has
             been closed. *)
          (* Mark the end of input has reached. *)
          end_of_input := true;
          (* Singal the writer in case it is waiting for data. *)
          Lwt_condition.signal cond ();
          (* Wait for it to terminate. *)
          writer
      | n ->
          (* Otherwise, send data to the writer. *)
          Queue.add (buf, n) queue;
          (* Singal the writer in case it is waiting for data. *)
          Lwt_condition.signal cond ();
          loop_read ()
  in

  (* Wait for either the reader to terminate or the writer to fail. *)
  Lwt.pick [writer; loop_read ()]

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

let usage () =
  prerr_endline "usage: relay <source-address>:<source-port> <destination-address>:<destination-port>";
  exit 2

(* Convert a string of the form "<host>:<port>" to an internet address
   object. *)
let addr_of_string str =
  (* Split the host and the port. *)
  let idx = try String.index str ':' with Not_found -> usage () in
  let host = String.sub str 0 idx and port = String.sub str (idx + 1) (String.length str - idx - 1) in
  (* Parse the port. *)
  let port = try int_of_string port with Failure _ -> usage () in
  (* Request the address of the host. *)
  let%lwt entry = Lwt_unix.gethostbyname host in
  if Array.length entry.Unix.h_addr_list = 0 then begin
    Printf.eprintf "no address found for host %S\n" host;
    exit 1
  end;
  Lwt.return (Unix.ADDR_INET (entry.Unix.h_addr_list.(0), port))

let%lwt () =
  if Array.length Sys.argv <> 3 then usage ();

  try%lwt
    (* Resolve addresses. *)
    let%lwt src_addr = addr_of_string Sys.argv.(1) and dst_addr = addr_of_string Sys.argv.(2) in

    (* Initialize the listening address. *)
    let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Lwt_unix.setsockopt sock Unix.SO_REUSEADDR true;
    Lwt_unix.bind sock src_addr;
    Lwt_unix.listen sock 1024;

    ignore (Lwt_log.notice "waiting for connection");

    (* Wait for a connection. *)
    let%lwt fd1, _ = Lwt_unix.accept sock in

    ignore (Lwt_log.notice "connection received, start relayling");

    (* Closes the no-more used listening socket. *)
    let%lwt () = Lwt_unix.close sock in

    (* Connect to the destination port. *)
    let fd2 = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let%lwt () = Lwt_unix.connect fd2 dst_addr in

    (* Start relaying. *)
    let%lwt () = Lwt.pick [relay fd1 fd2; relay fd2 fd1] in

    ignore (Lwt_log.notice "done relayling");

    Lwt.return_unit

  with exn ->
    ignore (Lwt_log.error ~exn "something went wrong");
    exit 1
