(* Lightweight thread library for Objective Caml
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
  lwt in_chan = open_file ~mode:input test_file in
  lwt s = read in_chan in
  lwt () = close in_chan in
  assert (s = file_contents);
  return ()

let suite = suite "lwt_io non blocking io" [
  test "create file"
    (fun () ->
      lwt out_chan = open_file ~mode:output test_file in
      lwt () = write out_chan file_contents in
      lwt () = close out_chan in
      return true);

  test "read file"
    (fun () ->
      lwt in_chan = open_file ~mode:input test_file in
      lwt s = read in_chan in
      lwt () = close in_chan in
      return (s = file_contents));

  test "many read file"
    (fun () ->
      lwt () = for_lwt i = 0 to 10000 do
	try_lwt
	  open_and_read_filename ()
        with e -> lwt () = printf "\nstep %i\n" i in raise_lwt e
      done in
      return true);

  test "remove file"
    (fun () ->
      Unix.unlink test_file;
      return true);

]
