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
external lwt_mmap_mincore : t -> int -> int -> string = "lwt_mmap_mincore"
external lwt_mmap_write : Unix.file_descr -> t -> int -> int -> int = "lwt_mmap_write"
external lwt_mmap_init_pipe : unit -> Unix.file_descr = "lwt_mmap_init_pipe"
external lwt_mmap_launch_waiter : t -> int -> int32 -> unit = "lwt_mmap_launch_waiter"
external lwt_mmap_memcpy : t -> int -> string -> int -> int -> unit = "lwt_mmap_memcpy"

type madvise =
  | MADV_NORMAL
  | MADV_RANDOM
  | MADV_SEQUENTIAL
  | MADV_WILLNEED
  | MADV_DONTNEED

external lwt_mmap_madvise : t -> int -> int -> madvise -> unit = "lwt_mmap_madvise"

let pagesize = init_pagesize ()
let finished_pipe =
  let in_fd = Lwt_unix.of_unix_file_descr (lwt_mmap_init_pipe ()) in
  Lwt_unix.set_close_on_exec in_fd;
  in_fd

let batch_size = 16
let max_read_size = batch_size * pagesize

let sendfile_batch_size = 256
let max_sendfile_size = sendfile_batch_size * pagesize

let min_sleep = 0.00002
let max_sleep = 0.0005 (* about the time needed to launch a thread ( on my computer ) *)

let length_in_core b offset len =
  let num_pages = (len + pagesize - 1) / pagesize in
  let s = lwt_mmap_mincore b offset num_pages in
  let rec count i c =
    if i = num_pages - 1
    then
      let dim = Bigarray.Array1.dim b in
      if dim - offset - c >= pagesize
      then c + pagesize
      else dim - offset
    else
      if Char.code s.[i] land 1 = 1
      then count (i + 1) (c + pagesize)
      else c
  in
  let memory_ready = count 0 0 in
  let rest = offset mod pagesize in
  min (min len (memory_ready - rest)) ((Bigarray.Array1.dim b) - offset)

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

(* copy from Lwt_io *)
type byte_order = Little_endian | Big_endian
external get_system_byte_order : unit -> byte_order = "lwt_unix_system_byte_order"

let (pos32_0,pos32_1,pos32_2,pos32_3) =
  match get_system_byte_order () with
    | Little_endian -> (0,1,2,3)
    | Big_endian -> (3,2,1,0)

let read_int32 =
  let buf = String.create 4 in
  fun fd ->
    lwt n = Lwt_unix.read fd buf 0 4 in
    match n with
      | 4 ->
	let v0 = Char.code buf.[pos32_0]
	and v1 = Char.code buf.[pos32_1]
	and v2 = Char.code buf.[pos32_2]
	and v3 = Char.code buf.[pos32_3] in
	return (Int32.logor
                  (Int32.logor
                     (Int32.of_int v0)
                     (Int32.shift_left (Int32.of_int v1) 8))
                  (Int32.logor
                     (Int32.shift_left (Int32.of_int v2) 16)
                     (Int32.shift_left (Int32.of_int v3) 24)))
      | _ -> fail End_of_file

let launch_waker =
  let thread = ref None in
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

let rec wait_mmap t offset len =
  assert( len > 0 );
  let offs = offset - (offset mod pagesize) in
  let res = length_in_core t offs len in
  if res > 0
  then return res
  else
    begin
      lwt_mmap_madvise t offset len MADV_WILLNEED;
      let rec aux sleep =
        let res = length_in_core t offs len in
        if res > 0
        then return res
        else
        if sleep < max_sleep
        then
	  lwt () = Lwt_unix.sleep sleep in
          aux ( 2. *. sleep )
        else
          ( launch_waker ();
	    lwt () = detach t offset in
	    aux min_sleep )
      in
      lwt () = Lwt_unix.sleep min_sleep in
      aux ( 2. *. min_sleep )
    end

let sendfile file fd offset len =
  let file_fd = Unix.openfile file [] 0 in
  let t = Bigarray.Array1.map_file file_fd Bigarray.char Bigarray.c_layout false (-1) in
  let rec aux offset len =
    if len = 0
    then return ()
    else
      lwt ready = wait_mmap t offset (min max_sendfile_size len) in
      lwt n = bigarray_write fd t offset ready in
      aux (offset + n) (len - n)
  in
  if len > (Bigarray.Array1.dim t - offset)
  then fail (Invalid_argument "Lwt_mmap.sendfile")
  else aux offset len

let read t position str offset len =
  if (String.length str < offset + len) || ((Bigarray.Array1.dim t) < position)
  then fail (Invalid_argument "Lwt_mmap.read")
  else
    let file_rest = (Bigarray.Array1.dim t) - position in
    if file_rest <= 0
    then return 0
    else 
      lwt memory_ready = wait_mmap t position (min (min max_read_size len) file_rest) in
      lwt_mmap_memcpy t position str offset memory_ready;
      return memory_ready

let size = Bigarray.Array1.dim

let of_unix_fd fd =
  try
    Some (Bigarray.Array1.map_file fd Bigarray.char Bigarray.c_layout false (-1))
  with
    | Sys_error _ -> None
    | Unix.Unix_error _ -> None
