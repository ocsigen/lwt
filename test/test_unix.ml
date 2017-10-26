(* OCaml promise library
 * http://www.ocsigen.org/lwt
 * Copyright (C) 2017 Anton Bachin
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



let temp_name =
  let rng = Random.State.make_self_init () in
  fun () ->
    let number = Random.State.int rng 10000 in
    Printf.sprintf (*"_build/"*)"lwt-testing-%04d" number

let temp_file () =
  Filename.temp_file (*~temp_dir:"_build"*) "lwt-testing-" ""

let temp_directory () =
  let rec attempt () =
    let path = temp_name () in
    try
      Unix.mkdir path 0o755;
      path
    with Unix.Unix_error (Unix.EEXIST, "mkdir", _) -> attempt ()
  in
  attempt ()
