(* Lightweight thread library for OCaml
 * http://www.ocsigen.org/lwt
 * Program Logging
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

(* This example illustrate the use of the Lwt_log module from
   lwt.unix. *)

(* The logging section for this module: *)
let section = Lwt_log.Section.make "test"

let%lwt () =
  (* Enable all logging levels superior from [Info] to [Fatal]: *)
  Lwt_log.Section.set_level section Lwt_log.Info;

  (* A message with the default logger: *)
  let%lwt () = Lwt_log.log ~section ~level:Lwt_log.Info "this message will appear only on stderr" in

  (* Same as begore, but using [Lwt_log.info]: *)
  let%lwt () = Lwt_log.info ~section "this one too" in

  (* A message to a custom logger, logging simultaneously to [stderr]
     and to the system logger daemon: *)
  let logger =
    Lwt_log.broadcast
      [Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stderr ();
       Lwt_log.syslog ~facility:`User ()]
  in
  let%lwt () = Lwt_log.info ~section ~logger "this message will appear on stderr and in '/var/log/user.log'" in

  (* Logging of exceptions: *)
  Printexc.record_backtrace true;
  let f () : unit = raise Exit in
  let g () = f () in
  let h () = g () in
  let%lwt () =
    try
      h ();
      Lwt.return_unit
    with exn ->
      Lwt_log.error ~section ~exn "h failed with"
  in

  let logger = Lwt_log.channel ~template:"$(name): $(section): $(loc-file): $(loc-line): $(loc-column): $(message)" ~close_mode:`Keep ~channel:Lwt_io.stderr () in
  Lwt_log.info ~section ~logger "this message will appear with a location"
