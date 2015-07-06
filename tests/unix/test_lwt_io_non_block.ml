(* Lightweight thread library for OCaml
 * http://www.ocsigen.org/lwt
 * Module Test_lwt_io
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
open Lwt_io
open Test

let test_file = "Lwt_io_test"
let file_contents = "test file content"

let open_and_read_filename () =
  open_file ~mode:input test_file >>= fun in_chan ->
  read in_chan >>= fun s ->
  close in_chan >>= fun () ->
  assert (s = file_contents);
  return ()

let suite = suite "lwt_io non blocking io" [
  test "file does not exist"
    (fun () -> Lwt_unix.file_exists test_file >|= fun r -> not r);

  test "create file"
    (fun () ->
      open_file ~mode:output test_file >>= fun out_chan ->
      write out_chan file_contents >>= fun () ->
      close out_chan >>= fun () ->
      return true);

  test "file exists"
    (fun () -> Lwt_unix.file_exists test_file);

  test "read file"
    (fun () ->
      open_file ~mode:input test_file >>= fun in_chan ->
      read in_chan >>= fun s ->
      close in_chan >>= fun () ->
      return (s = file_contents));

  test "many read file"
    (fun () ->
      let rec loop i =
        open_and_read_filename () >>= fun () ->
        if i > 10000 then return true
        else loop (i + 1)
      in
      loop 0);

  test "remove file"
    (fun () ->
      Unix.unlink test_file;
      return true);

]
