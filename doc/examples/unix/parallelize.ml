(* Lightweight thread library for OCaml
 * http://www.ocsigen.org/lwt
 * Program Parallelize
 * Copyright (C) 2011 Jérémie Dimino
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

(* Reads commands from standard input and launch them in parallel,
   using as many processes as the number of CPUs. *)

open Lwt.Infix

(* Reads one command, launch it and waits for when it termination,
   then start again: *)
let rec launch () =
  match%lwt Lwt_io.read_line_opt Lwt_io.stdin with
    | None ->
        Lwt.return_unit
    | Some line ->
        let%lwt exit_status = Lwt_process.exec (Lwt_process.shell line) in
        launch ()

(* Creates the initial <N> threads, where <N> is the number of
   CPUs: *)
let rec create_threads = function
  | 0 ->
      Lwt.return_unit
  | n ->
      launch () <&> create_threads (n - 1)

(* Counts the number of CPUs using "/proc/cpuinfo": *)
let cpus_count () =
  Lwt_stream.fold (fun _ n -> succ n)
    (Lwt_stream.filter
       (fun line ->
          try
            Scanf.sscanf line "processor :" true
          with _ ->
            false)
       (Lwt_io.lines_of_file "/proc/cpuinfo")) 0

let%lwt () = cpus_count () >>= create_threads
