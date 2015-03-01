(* Lightweight thread library for OCaml
 * http://www.ocsigen.org/lwt
 * Module Lwt_chan
 * Copyright (C) 2005-2008 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *                    2009 Jérémie Dimino
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

type in_channel = Lwt_io.input_channel
type out_channel = Lwt_io.output_channel

let in_channel_of_descr fd = Lwt_io.of_fd ~mode:Lwt_io.input fd

let make_in_channel ?close read =
  Lwt_io.make ~mode:Lwt_io.input ?close
    (fun buf ofs len ->
       let str = Bytes.create len in
       read str 0 len >>= fun n ->
       if (n > 0) then Lwt_bytes.blit_from_bytes str 0 buf ofs len;
       Lwt.return n)

let input_line ic =
  let rec loop buf =
    Lwt_io.read_char_opt ic >>= function
      | None | Some '\n' ->
          Lwt.return (Buffer.contents buf)
      | Some char ->
          Buffer.add_char buf char;
          loop buf
  in
  Lwt_io.read_char_opt ic >>= function
    | Some '\n' ->
        Lwt.return ""
    | Some char ->
        let buf = Buffer.create 128 in
        Buffer.add_char buf char;
        loop buf
    | None ->
        Lwt.fail End_of_file

let input_value = Lwt_io.read_value
let input = Lwt_io.read_into
let really_input = Lwt_io.read_into_exactly
let input_char = Lwt_io.read_char
let input_binary_int = Lwt_io.BE.read_int
let open_in_gen flags perm fname = Lwt_io.open_file ~flags ~perm ~mode:Lwt_io.input fname
let open_in fname = Lwt_io.open_file ~mode:Lwt_io.input fname
let close_in = Lwt_io.close
let out_channel_of_descr fd = Lwt_io.of_fd ~mode:Lwt_io.output fd

let make_out_channel ?close write =
  Lwt_io.make ~mode:Lwt_io.output ?close
    (fun buf ofs len ->
       let str = Bytes.create len in
       Lwt_bytes.blit_to_bytes buf ofs str 0 len;
       write str 0 len)

let output = Lwt_io.write_from_exactly
let flush = Lwt_io.flush
let output_string = Lwt_io.write
let output_value oc v = Lwt_io.write_value oc v
let output_char = Lwt_io.write_char
let output_binary_int = Lwt_io.BE.write_int
let open_out_gen flags perm fname = Lwt_io.open_file ~flags ~perm ~mode:Lwt_io.output fname
let open_out fname = Lwt_io.open_file ~mode:Lwt_io.output fname
let close_out = Lwt_io.close
let open_connection sockaddr = Lwt_io.open_connection sockaddr
