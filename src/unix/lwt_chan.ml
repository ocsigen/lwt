(* Lightweight thread library for OCaml
 * http://www.ocsigen.org/lwt
 * Module Lwt_chan
 * Copyright (C) 2005-2008 J�r�me Vouillon
 * Laboratoire PPS - CNRS Universit� Paris Diderot
 *                    2009 J�r�mie Dimino
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
open Lwt_io

type in_channel = Lwt_io.input_channel
type out_channel = Lwt_io.output_channel

let in_channel_of_descr fd = of_fd ~mode:Lwt_io.input fd

let make_in_channel ?close read =
  make ~mode:Lwt_io.input ?close
    (fun buf ofs len ->
       let str = String.create len in
       lwt n = read str 0 len in
       if (n > 0) then Lwt_bytes.blit_string_bytes str 0 buf ofs len;
       return n)

let input_line ic =
  let rec loop buf =
    read_char_opt ic >>= function
      | None | Some '\n' ->
          return (Buffer.contents buf)
      | Some char ->
          Buffer.add_char buf char;
          loop buf
  in
  read_char_opt ic >>= function
    | Some '\n' ->
        return ""
    | Some char ->
        let buf = Buffer.create 128 in
        Buffer.add_char buf char;
        loop buf
    | None ->
        raise_lwt End_of_file

let input_value = read_value
let input = read_into
let really_input = read_into_exactly
let input_char = read_char
let input_binary_int = BE.read_int
let open_in_gen flags perm fname = open_file ~flags ~perm ~mode:Lwt_io.input fname
let open_in fname = open_file ~mode:Lwt_io.input fname
let close_in = close
let out_channel_of_descr fd = of_fd ~mode:Lwt_io.output fd

let make_out_channel ?close write =
  make ~mode:Lwt_io.output ?close
    (fun buf ofs len ->
       let str = String.create len in
       Lwt_bytes.blit_bytes_string buf ofs str 0 len;
       write str 0 len)

let output = write_from_exactly
let flush = flush
let output_string = write
let output_value oc v = write_value oc v
let output_char = write_char
let output_binary_int = BE.write_int
let open_out_gen flags perm fname = open_file ~flags ~perm ~mode:Lwt_io.output fname
let open_out fname = open_file ~mode:Lwt_io.output fname
let close_out = close
let open_connection sockaddr = open_connection sockaddr
