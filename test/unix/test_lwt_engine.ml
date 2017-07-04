(* OCaml promise library
 * http://www.ocsigen.org/lwt
 * Copyright (C) 2016 Anton Bachin
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

open Test
open Lwt.Infix

let selection_tests = [
  test "libev: default when enabled in build bot"
    (fun () ->
      if not Lwt_config._HAVE_LIBEV then Lwt.return_true
      else
        (* Check if this is running inside Travis or AppVeyor. *)
        let in_travis =
          try ignore (Sys.getenv "TRAVIS_COMMIT"); true
          with Not_found -> false
        in

        let in_appveyor =
          try ignore (Sys.getenv "APPVEYOR_REPO_COMMIT"); true
          with Not_found -> false
        in

        if not (in_travis || in_appveyor) then Lwt.return_true
        else Lwt.return Lwt_config.libev_default);
]

let tests = selection_tests

let timing_tests = [
  test "libev: timer delays are not too short" begin fun () ->
    let start = Unix.gettimeofday () in

    Lwt.catch
      (fun () ->
        (* Block the entire process for one second. If using libev, libev's
           notion of the current time is not updated during this period. *)
        let () = Unix.sleep 1 in

        (* At this point, libev thinks that the time is what it was about one
           second ago. Now schedule exception Lwt_unix.Timeout to be raised in
           0.5 seconds. If the implementation is incorrect, the exception will
           be raised immediately, because the 0.5 seconds will be measured
           relative to libev's "current" time of one second ago. *)
        Lwt_unix.timeout 0.5)

      (function
      | Lwt_unix.Timeout ->
        Lwt.return (Unix.gettimeofday ())
      | exn ->
        Lwt.fail exn)

    >>= fun stop ->

    Lwt.return (stop -. start >= 1.5)
  end;
]

let tests = tests @ timing_tests

let suite = suite "lwt_engine" tests
