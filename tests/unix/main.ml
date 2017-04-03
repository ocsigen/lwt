(* Lightweight thread library for OCaml
 * http://www.ocsigen.org/lwt
 * Module Main
 * Copyright (C) 2009 Jérémie Dimino
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

let is_fd_open fd_ =
  let fd  = (Obj.magic (int_of_string fd_) : Unix.file_descr) in
  let buf = Bytes.create 42 in
    try
      ignore (Unix.read fd buf 0 42);
      true
    with Unix.Unix_error(Unix.EBADF, _, _) ->
      false

let () =
  try
    assert (not @@ is_fd_open @@ Unix.getenv Test_lwt_unix.assert_fd_closed);
    exit 0
  with Not_found -> ()

let () =
  try
    assert (is_fd_open @@ Unix.getenv Test_lwt_unix.assert_fd_open);
    exit 0
  with Not_found -> ()

let () =
  Test.run "unix" [
    Test_lwt_unix.suite;
    Test_lwt_io.suite;
    Test_lwt_io_non_block.suite;
    Test_lwt_process.suite;
    Test_lwt_engine.suite;
    Test_mcast.suite;
  ]
