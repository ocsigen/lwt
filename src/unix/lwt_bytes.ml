(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_unix
 * Copyright (C) 2010 Jérémie Dimino
 *               2010 Pierre Chambart
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

#include "src/unix/lwt_config.ml"

open Bigarray
open Lwt

type t = (char, int8_unsigned_elt, c_layout) Array1.t

let create size = Array1.create char c_layout size
let length bytes = Array1.dim bytes

external get : t -> int -> char = "%caml_ba_ref_1"
external set : t -> int -> char -> unit = "%caml_ba_set_1"

external unsafe_get : t -> int -> char = "%caml_ba_unsafe_ref_1"
external unsafe_set : t -> int -> char -> unit = "%caml_ba_unsafe_set_1"

external unsafe_fill : t -> int -> int -> char -> unit = "lwt_unix_fill_bytes" "noalloc"

let fill bytes ofs len ch =
  if ofs < 0 || len < 0 || ofs > length bytes - len then
    invalid_arg "Lwt_bytes.fill"
  else
    unsafe_fill bytes ofs len ch

(* +-----------------------------------------------------------------+
   | Blitting                                                        |
   +-----------------------------------------------------------------+ *)

external unsafe_blit_string_bytes : string -> int -> t -> int -> int -> unit = "lwt_unix_blit_string_bytes" "noalloc"
external unsafe_blit_bytes_string : t -> int -> string -> int -> int -> unit = "lwt_unix_blit_bytes_string" "noalloc"
external unsafe_blit : t -> int -> t -> int -> int -> unit = "lwt_unix_blit_bytes_bytes" "noalloc"

let blit_string_bytes src_buf src_ofs dst_buf dst_ofs len =
  if (len < 0
      || src_ofs < 0 || src_ofs > String.length src_buf - len
      || dst_ofs < 0 || dst_ofs > length dst_buf - len) then
    invalid_arg "String.blit"
  else
    unsafe_blit_string_bytes src_buf src_ofs dst_buf dst_ofs len

let blit_bytes_string src_buf src_ofs dst_buf dst_ofs len =
  if (len < 0
      || src_ofs < 0 || src_ofs > length src_buf - len
      || dst_ofs < 0 || dst_ofs > String.length dst_buf - len) then
    invalid_arg "String.blit"
  else
    unsafe_blit_bytes_string src_buf src_ofs dst_buf dst_ofs len

let blit src_buf src_ofs dst_buf dst_ofs len =
  if (len < 0
      || src_ofs < 0 || src_ofs > length src_buf - len
      || dst_ofs < 0 || dst_ofs > length dst_buf - len) then
    invalid_arg "String.blit"
  else
    unsafe_blit src_buf src_ofs dst_buf dst_ofs len

let of_string str =
  let len = String.length str in
  let bytes = create len in
  unsafe_blit_string_bytes str 0 bytes 0 len;
  bytes

let to_string bytes =
  let len = length bytes in
  let str = String.create len in
  unsafe_blit_bytes_string bytes 0 str 0 len;
  str

let proxy = Array1.sub

let extract buf ofs len =
  if ofs < 0 || len < 0 || ofs > length buf - len then
    invalid_arg "Lwt_bytes.extract"
  else begin
    let buf' = create len in
    blit buf ofs buf' 0 len;
    buf'
  end

let copy buf =
  let len = length buf in
  let buf' = create len in
  blit buf 0 buf' 0 len;
  buf'

(* +-----------------------------------------------------------------+
   | IOs                                                             |
   +-----------------------------------------------------------------+ *)

open Lwt_unix

external stub_read : Unix.file_descr -> t -> int -> int -> int = "lwt_unix_bytes_read"
external read_job : Unix.file_descr -> t -> int -> int -> [ `unix_bytes_read ] job = "lwt_unix_bytes_read_job"
external read_result : [ `unix_bytes_read ] job -> int = "lwt_unix_bytes_read_result"
external read_free : [ `unix_bytes_read ] job -> unit = "lwt_unix_bytes_read_free" "noalloc"

let read fd buf pos len =
  if pos < 0 || len < 0 || pos > length buf - len then
    invalid_arg "Lwt_bytes.read"
  else
    blocking fd >>= function
      | true ->
          lwt () = wait_read fd in
          execute_job (read_job (unix_file_descr fd) buf pos len) read_result read_free
      | false ->
          wrap_syscall Read fd (fun () -> stub_read (unix_file_descr fd) buf pos len)

external stub_write : Unix.file_descr -> t -> int -> int -> int = "lwt_unix_bytes_write"
external write_job : Unix.file_descr -> t -> int -> int -> [ `unix_bytes_write ] job = "lwt_unix_bytes_write_job"
external write_result : [ `unix_bytes_write ] job -> int = "lwt_unix_bytes_write_result"
external write_free : [ `unix_bytes_write ] job -> unit = "lwt_unix_bytes_write_free" "noalloc"

let write fd buf pos len =
  if pos < 0 || len < 0 || pos > length buf - len then
    invalid_arg "Lwt_bytes.write"
  else
    blocking fd >>= function
      | true ->
          lwt () = wait_write fd in
          execute_job (write_job (unix_file_descr fd) buf pos len) write_result write_free
      | false ->
          wrap_syscall Write fd (fun () -> stub_write (unix_file_descr fd) buf pos len)

#if windows

let recv fd buf pos len flags =
  raise (Lwt_sys.Not_available "Lwt_bytes.recv")

#else

external stub_recv : Unix.file_descr -> t -> int -> int -> Unix.msg_flag list -> int = "lwt_unix_bytes_recv"

let recv fd buf pos len flags =
  if pos < 0 || len < 0 || pos > length buf - len then
    invalid_arg "recv"
  else
    wrap_syscall Read fd (fun () -> stub_recv (unix_file_descr fd) buf pos len flags)

#endif

#if windows

let send fd buf pos len flags =
  raise (Lwt_sys.Not_available "Lwt_bytes.send")

#else

external stub_send : Unix.file_descr -> t -> int -> int -> Unix.msg_flag list -> int = "lwt_unix_bytes_send"

let send fd buf pos len flags =
  if pos < 0 || len < 0 || pos > length buf - len then
    invalid_arg "send"
  else
    wrap_syscall Write fd (fun () -> stub_send (unix_file_descr fd) buf pos len flags)

#endif

type io_vector = {
  iov_buffer : t;
  iov_offset : int;
  iov_length : int;
}

let io_vector ~buffer ~offset ~length = {
  iov_buffer = buffer;
  iov_offset = offset;
  iov_length = length;
}

let check_io_vectors func_name iovs =
  List.iter
    (fun iov ->
       if iov.iov_offset < 0
         || iov.iov_length < 0
         || iov.iov_offset > length iov.iov_buffer - iov.iov_length then
           invalid_arg func_name)
    iovs

#if windows

let recv_msg ~socket ~io_vectors =
  raise (Lwt_sys.Not_available "recv_msg")

#else

external stub_recv_msg : Unix.file_descr -> int -> io_vector list -> int * Unix.file_descr list = "lwt_unix_bytes_recv_msg"

let recv_msg ~socket ~io_vectors =
  check_io_vectors "recv_msg" io_vectors;
  let n_iovs = List.length io_vectors in
  wrap_syscall Read socket
    (fun () ->
       stub_recv_msg (unix_file_descr socket) n_iovs io_vectors)

#endif

#if windows

let send_msg ~socket ~io_vectors ~fds =
  raise (Lwt_sys.Not_available "send_msg")

#else

external stub_send_msg : Unix.file_descr -> int -> io_vector list -> int -> Unix.file_descr list -> int = "lwt_unix_bytes_send_msg"

let send_msg ~socket ~io_vectors ~fds =
  check_io_vectors "send_msg" io_vectors;
  let n_iovs = List.length io_vectors and n_fds = List.length fds in
  wrap_syscall Write socket
    (fun () ->
       stub_send_msg (unix_file_descr socket) n_iovs io_vectors n_fds fds)

#endif

#if windows

let recvfrom fd buf pos len flags =
  raise (Lwt_sys.Not_available "Lwt_bytes.recvfrom")

#else

external stub_recvfrom : Unix.file_descr -> t -> int -> int -> Unix.msg_flag list -> int * Unix.sockaddr = "lwt_unix_bytes_recvfrom"

let recvfrom fd buf pos len flags =
  if pos < 0 || len < 0 || pos > length buf - len then
    invalid_arg "Lwt_bytes.recvfrom"
  else
    wrap_syscall Read fd (fun () -> stub_recvfrom (unix_file_descr fd) buf pos len flags)

#endif

#if windows

let sendto fd buf pos len flags addr =
  raise (Lwt_sys.Not_available "Lwt_bytes.sendto")

#else

external stub_sendto : Unix.file_descr -> t -> int -> int -> Unix.msg_flag list -> Unix.sockaddr -> int = "lwt_unix_bytes_sendto_byte" "lwt_unix_bytes_sendto"

let sendto fd buf pos len flags addr =
  if pos < 0 || len < 0 || pos > length buf - len then
    invalid_arg "Lwt_bytes.sendto"
  else
    wrap_syscall Write fd (fun () -> stub_sendto (unix_file_descr fd) buf pos len flags addr)

#endif

(* +-----------------------------------------------------------------+
   | Memory mapped files                                             |
   +-----------------------------------------------------------------+ *)

let map_file ~fd ?pos ~shared ?(size=(-1)) () =
  Array1.map_file fd ?pos char c_layout shared size

external mapped : t -> bool = "lwt_unix_mapped" "noalloc"

type advice =
  | MADV_NORMAL
  | MADV_RANDOM
  | MADV_SEQUENTIAL
  | MADV_WILLNEED
  | MADV_DONTNEED

#if windows

let madvise buf pos len advice =
  raise (Lwt_sys.Not_available "madvise")

#else

external stub_madvise : t -> int -> int -> advice -> unit = "lwt_unix_madvise"

let madvise buf pos len advice =
  if pos < 0 || len < 0 || pos > length buf - len then
    invalid_arg "Lwt_bytes.madvise"
  else
    stub_madvise buf pos len advice

#endif

external get_page_size : unit -> int = "lwt_unix_get_page_size"

let page_size = get_page_size ()

#if windows

let mincore buffer offset states =
  raise (Lwt_sys.Not_available "mincore")

let wait_mincore buffer offset =
  raise (Lwt_sys.Not_available "mincore")

#else

external stub_mincore : t -> int -> int -> bool array -> unit = "lwt_unix_mincore"

let mincore buffer offset states =
  if (offset mod page_size <> 0
      || offset < 0
      || offset > length buffer - (Array.length states * page_size)) then
    invalid_arg "Lwt_bytes.mincore"
  else
    stub_mincore buffer offset (Array.length states * page_size) states

external wait_mincore_job : t -> int -> [ `unix_wait_mincore ] job = "lwt_unix_wait_mincore_job"
external wait_mincore_free : [ `unix_wait_mincore ] job -> unit = "lwt_unix_wait_mincore_free" "noalloc"

let wait_mincore buffer offset =
  if offset < 0 || offset >= length buffer then
    invalid_arg "Lwt_bytes.wait_mincore"
  else begin
    let state = [|false|] in
    mincore buffer (offset - (offset mod page_size)) state;
    if state.(0) then
      return ()
    else
      execute_job (wait_mincore_job buffer offset) ignore wait_mincore_free
  end

#endif
