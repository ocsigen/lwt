(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_mmap
 * Copyright (C) 2010 Pierre Chambart
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

type t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external init_pagesize : unit -> int = "lwt_mmap_init_pagesize"
external mincore : t -> int -> int -> string = "lwt_mmap_mincore"
external lwt_mmap_write : Unix.file_descr -> t -> int -> int -> int = "lwt_mmap_write"
external lwt_mmap_init_pipe : unit -> Unix.file_descr = "lwt_mmap_init_pipe"
external lwt_mmap_launch_waiter : t -> int -> int32 -> unit = "lwt_mmap_launch_waiter"
external lwt_mmap_memcpy : t -> int -> string -> int -> int -> unit = "lwt_mmap_memcpy"

let pagesize = init_pagesize ()
let finished_pipe =
  let in_fd = Lwt_unix.of_unix_file_descr (lwt_mmap_init_pipe ()) in
  Lwt_unix.set_close_on_exec in_fd;
  Lwt_io.of_fd ~mode:Lwt_io.input ~buffer_size:256 in_fd


let mincore b offset len =
  let s = mincore b offset len in
  let f i = ( Char.code s.[i] ) land 1 = 1 in
  Array.init (String.length s) f

let bigarray_write ch buf pos len =
  if pos < 0 || len < 0 || pos > Bigarray.Array1.dim buf - len then
    invalid_arg "Lwt_mmap.bigarray_write"
  else
    Lwt_unix.wrap_syscall Lwt_unix.outputs ch (fun () -> lwt_mmap_write
      (Lwt_unix.unix_file_descr ch) buf pos len)

let waiter_tbl : (int32, unit Lwt.u) Hashtbl.t = Hashtbl.create 16

let detach =
  let count = ref 0l in
  fun t offset ->
    count := Int32.succ (!count);
    lwt_mmap_launch_waiter t offset !count;
    let (res,w) = task () in
    Hashtbl.add waiter_tbl !count w;
    Lwt.on_cancel res (fun _ -> Hashtbl.remove waiter_tbl !count);
    res

let launch_waker =
  let thread = ref None in
  let read_int32 =
    match Lwt_io.system_byte_order with
	| Lwt_io.Little_endian -> Lwt_io.LE.read_int32
	| Lwt_io.Big_endian -> Lwt_io.BE.read_int32
  in
  let rec loop () =
    lwt id = read_int32 finished_pipe in
    begin
      match (try Some (Hashtbl.find waiter_tbl id) with Not_found -> None) with
	| None -> ()
	| Some w ->
	  Hashtbl.remove waiter_tbl id;
	  wakeup w ()
    end;
    if Hashtbl.length waiter_tbl = 0
    then
      ( thread := None;
	return () )
    else loop ()
  in
  fun () ->
    match !thread with
      | Some _ -> ()
      | None -> thread := Some (loop ()); ()

let wait_mmap t offset =
  let offs = offset - (offset mod pagesize) in
  let res = mincore t offs pagesize in
  if res.(0)
  then return ()
  else
    ( launch_waker ();
      detach t offset )

let sendfile file fd offset len =
  let file_fd = Unix.openfile file [] 0 in
  let t = Bigarray.Array1.map_file file_fd Bigarray.char Bigarray.c_layout false (-1) in
  let rec aux offset len =
    lwt () = wait_mmap t offset in
    if len <= pagesize
    then lwt _ = bigarray_write fd t offset len in return ()
    else lwt n = bigarray_write fd t offset pagesize in
         if n < pagesize
	 then fail (Failure "sendfile")
	 else aux (offset + pagesize) (len - pagesize)
  in
  aux offset len

let make_io file =
  let file_fd = Unix.openfile file [] 0 in
  let t = Bigarray.Array1.map_file file_fd Bigarray.char Bigarray.c_layout false (-1) in
  let dim = Bigarray.Array1.dim t in
  let position = ref 0 in
  let read str offset len =
    if String.length str < offset + len
    then fail (Invalid_argument "Lwt_mmap.make_op/read")
    else
      let rest = !position mod pagesize in
      let len = min (min len (pagesize - rest)) (dim - !position) in
      lwt () = wait_mmap t offset in
      (* for i = 0 to len - 1 do
	str.[offset + i] <- t.{!position + i};
      done; *)
      lwt_mmap_memcpy t (!position) str offset len;
      position := !position + len;
      return len
  in
  let seek pos cmd =
    let new_pos =
      match cmd with
	| Unix.SEEK_SET -> pos
	| Unix.SEEK_CUR -> Int64.add (Int64.of_int !position) pos
	| Unix.SEEK_END -> Int64.add (Int64.of_int dim) pos
    in
    if (Int64.of_int dim) < new_pos || new_pos < 0L
    then fail (Invalid_argument "Lwt_mmap.make_op/seek")
    else (position := Int64.to_int new_pos; return (new_pos))
  in
  Lwt_io.make ~seek ~mode:Lwt_io.input read
