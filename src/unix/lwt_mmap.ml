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

type barray = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external init_pagesize : unit -> int = "lwt_mmap_init_pagesize"
external lwt_mmap_mincore : barray -> int -> int -> string = "lwt_mmap_mincore"
external lwt_mmap_write : Unix.file_descr -> barray -> int -> int -> int = "lwt_mmap_write"
external lwt_mmap_launch_waiter : barray -> int -> int -> unit = "lwt_mmap_launch_waiter"
external lwt_mmap_memcpy : barray -> int -> string -> int -> int -> unit = "lwt_mmap_memcpy"

type madvise =
  | MADV_NORMAL
  | MADV_RANDOM
  | MADV_SEQUENTIAL
  | MADV_WILLNEED
  | MADV_DONTNEED

external lwt_mmap_madvise : barray -> int -> int -> madvise -> unit = "lwt_mmap_madvise"

type t =
    { fd : Unix.file_descr;
      array : barray;
      dim : int; }

let pagesize = init_pagesize ()

let batch_size = 16
let max_read_size = batch_size * pagesize

let sendfile_batch_size = 256
let max_sendfile_size = sendfile_batch_size * pagesize

let min_sleep = 0.00002
let max_sleep = 0.0005 (* about the time needed to launch a thread ( on my computer ) *)

let length_in_core b offset len =
  let num_pages = (len + pagesize - 1) / pagesize in
  let s = lwt_mmap_mincore b.array offset num_pages in
  let rec count i c =
    if i = num_pages - 1
    then
      let dim = b.dim in
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
  min (min len (memory_ready - rest)) (b.dim - offset)

let bigarray_write ch buf pos len =
  if pos < 0 || len < 0 || pos > Bigarray.Array1.dim buf - len then
    invalid_arg "Lwt_mmap.bigarray_write"
  else
    Lwt_unix.wrap_syscall Lwt_unix.Write ch (fun () -> lwt_mmap_write
      (Lwt_unix.unix_file_descr ch) buf pos len)

let rec wait_mmap t offset len =
  assert( len > 0 );
  let offs = offset - (offset mod pagesize) in
  let res = length_in_core t offs len in
  if res > 0 then
    return res
  else begin
    lwt_mmap_madvise t.array offset len MADV_WILLNEED;
    let rec aux sleep =
      let res = length_in_core t offs len in
      if res > 0 then
        return res
      else
        if sleep < max_sleep then
	  lwt () = Lwt_unix.sleep sleep in
          aux (2. *. sleep)
        else begin
          let waiter, wakener = wait () in
          let id = Lwt_unix.make_notification ~once:true (wakeup wakener) in
          lwt_mmap_launch_waiter t.array offset id;
          lwt () = waiter in
	  aux min_sleep
        end
    in
    lwt () = Lwt_unix.sleep min_sleep in
    aux (2. *. min_sleep)
  end

let read t position str offset len =
  if (String.length str < offset + len) || (t.dim < position)
  then raise_lwt (Invalid_argument "Lwt_mmap.read")
  else
    let file_rest = t.dim - position in
    if file_rest <= 0
    then return 0
    else 
      lwt memory_ready = wait_mmap t position (min (min max_read_size len) file_rest) in
      lwt_mmap_memcpy t.array position str offset memory_ready;
      return memory_ready

let size t = t.dim

type t_ = t

module Fd_weak_tbl = Weak.Make
  ( struct
      type t = t_
      let equal t1 t2 = t1.fd = t2.fd
      let hash t = Hashtbl.hash t.fd
    end )

let fd_table = Fd_weak_tbl.create 0

let null_array = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0

let of_unix_fd fd =
  let finder = { array = null_array; fd = fd; dim = 0 } in
  try
    Some (
      try
	Fd_weak_tbl.find fd_table finder
      with
	| Not_found ->
	  let array = Bigarray.Array1.map_file fd Bigarray.char Bigarray.c_layout false (-1) in
	  let t = { fd = fd; array = array; dim = Bigarray.Array1.dim array } in
	  Fd_weak_tbl.add fd_table t;
          t )
  with
    | Sys_error _ -> None
    | Unix.Unix_error _ -> None

let sendfile file_fd fd offset len =
  match of_unix_fd file_fd with
    | None -> raise_lwt (Invalid_argument "Lwt_mmap.sendfile")
    | Some t ->
      let rec aux offset len =
	if len = 0
	then return ()
	else
	  lwt ready = wait_mmap t offset (min max_sendfile_size len) in
          lwt n = bigarray_write fd t.array offset ready in
          aux (offset + n) (len - n)
      in
      if len > (t.dim - offset)
      then raise_lwt (Invalid_argument "Lwt_mmap.sendfile")
      else aux offset len

