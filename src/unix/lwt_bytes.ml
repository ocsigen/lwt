(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_unix
 * Copyright (C) 2010 Jérémie Dimino
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

let windows_hack = Sys.os_type <> "Unix"

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

(* +-----------------------------------------------------------------+
   | IOs                                                             |
   +-----------------------------------------------------------------+ *)

external stub_read : Unix.file_descr -> t -> int -> int -> int = "lwt_unix_bytes_read"
external read_job : Unix.file_descr -> t -> int -> int -> [ `unix_bytes_read ] Lwt_unix.job = "lwt_unix_bytes_read_job"
external read_result : [ `unix_bytes_read ] Lwt_unix.job -> int = "lwt_unix_bytes_read_result" "noalloc"
external read_free : [ `unix_bytes_read ] Lwt_unix.job -> unit = "lwt_unix_bytes_read_free" "noalloc"

let read fd buf pos len =
  if pos < 0 || len < 0 || pos > length buf - len then
    invalid_arg "Lwt_bytes.read"
  else if windows_hack then
    invalid_arg "Lwt_bytes.read: not implemented"
  else
    Lwt_unix.blocking fd >>= function
      | true ->
          lwt () = Lwt_unix.wait_read fd in
          Lwt_unix.execute_job (read_job (Lwt_unix.unix_file_descr fd) buf pos len) read_result read_free
      | false ->
          Lwt_unix.wrap_syscall Lwt_unix.Read fd (fun () -> stub_read (Lwt_unix.unix_file_descr fd) buf pos len)

external stub_write : Unix.file_descr -> t -> int -> int -> int = "lwt_unix_bytes_write"
external write_job : Unix.file_descr -> t -> int -> int -> [ `unix_bytes_write ] Lwt_unix.job = "lwt_unix_bytes_write_job"
external write_result : [ `unix_bytes_write ] Lwt_unix.job -> int = "lwt_unix_bytes_write_result" "noalloc"
external write_free : [ `unix_bytes_write ] Lwt_unix.job -> unit = "lwt_unix_bytes_write_free" "noalloc"

let write fd buf pos len =
  if pos < 0 || len < 0 || pos > length buf - len then
    invalid_arg "Lwt_bytes.write"
  else if windows_hack then
    invalid_arg "Lwt_bytes.write: not implemented"
  else
    Lwt_unix.blocking fd >>= function
      | true ->
          lwt () = Lwt_unix.wait_write fd in
          Lwt_unix.execute_job (write_job (Lwt_unix.unix_file_descr fd) buf pos len) write_result write_free
      | false ->
          Lwt_unix.wrap_syscall Lwt_unix.Write fd (fun () -> stub_write (Lwt_unix.unix_file_descr fd) buf pos len)

external stub_recv : Unix.file_descr -> t -> int -> int -> Unix.msg_flag list -> int = "lwt_unix_bytes_recv"

let recv fd buf pos len flags =
  if pos < 0 || len < 0 || pos > length buf - len then
    invalid_arg "Lwt_unix.recv"
  else if windows_hack then
    invalid_arg "Lwt_bytes.recv: not implemented"
  else
    Lwt_unix.wrap_syscall Lwt_unix.Read fd (fun () -> stub_recv (Lwt_unix.unix_file_descr fd) buf pos len flags)

external stub_send : Unix.file_descr -> t -> int -> int -> Unix.msg_flag list -> int = "lwt_unix_bytes_send"

let send fd buf pos len flags =
  if pos < 0 || len < 0 || pos > length buf - len then
    invalid_arg "Lwt_unix.send"
  else if windows_hack then
    invalid_arg "Lwt_bytes.send: not implemented"
  else
    Lwt_unix.wrap_syscall Lwt_unix.Write fd (fun () -> stub_send (Lwt_unix.unix_file_descr fd) buf pos len flags)
