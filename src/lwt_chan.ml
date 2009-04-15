(* Lightweight thread library for Objective Caml
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

open Lwt_io

type in_channel = ic
type out_channel = oc

let in_channel_of_descr fd = of_fd ~mode:Lwt_io.input fd
let make_in_channel ?close read = make ~mode:Lwt_io.input ?close read
let input_line = get_line
let input_value = get_value
let input = get
let really_input = get_exactly
let input_char = get_char
let input_binary_int = LE.get_int
let open_in_gen flags perm fname =
  in_channel_of_descr (Lwt_unix.of_unix_file_descr (Unix.openfile fname flags perm))
let open_in fname =
  open_in_gen [Unix.O_RDONLY; Unix.O_NONBLOCK] 0 fname
let close_in = close
let out_channel_of_descr fd =
  make
    ~auto_flush:false
    ~close:(fun _ -> try Lwt_unix.close fd; Lwt.return () with e -> Lwt.fail e)
    ~seek:(fun pos cmd -> Lwt.return (Unix.LargeFile.lseek (Lwt_unix.unix_file_descr fd) pos cmd))
    ~mode:Lwt_io.output
    (Lwt_unix.write fd)
let make_out_channel ?close write = make ~auto_flush:false ~mode:Lwt_io.output ?close write
let output = put_exactly
let flush = force_flush
let output_string = put_string
let output_value oc v = put_value oc v
let output_char = put_char
let output_binary_int = LE.put_int
let open_out_gen flags perm fname =
  out_channel_of_descr (Lwt_unix.of_unix_file_descr (Unix.openfile fname flags perm))
let open_out fname = open_out_gen [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; Unix.O_NONBLOCK] 0o666 fname
let close_out = close
let open_connection sockaddr = open_connection sockaddr
