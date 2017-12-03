(* OCaml promise library
 * http://www.ocsigen.org/lwt
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

(* [Lwt_sequence] is deprecated – we don't want users outside Lwt using it.
   However, it is still used internally by Lwt. So, briefly disable warning 3
   ("deprecated"), and create a local, non-deprecated alias for
   [Lwt_sequence] that can be referred to by the rest of the code in this
   module without triggering any more warnings. *)
[@@@ocaml.warning "-3"]
module Lwt_sequence = Lwt_sequence
[@@@ocaml.warning "+3"]

open Lwt.Infix

let rec copy ic logger =
  Lwt_io.read_line ic >>= fun line ->
  Lwt_log.log ?logger ~level:Lwt_log.Notice line >>= fun () ->
  copy ic logger

let redirect fd logger =
  let fd_r, fd_w = Unix.pipe () in
  Unix.set_close_on_exec fd_r;
  Unix.dup2 fd_w fd;
  Unix.close fd_w;
  let ic = Lwt_io.of_unix_fd ~mode:Lwt_io.input fd_r in
  Lwt.ignore_result (copy ic logger)

let redirect_output dev_null fd mode = match mode with
  | `Dev_null ->
    Unix.dup2 dev_null fd
  | `Close ->
    Unix.close fd
  | `Keep ->
    ()
  | `Log_default ->
    redirect fd None
  | `Log logger ->
    redirect fd (Some logger)

let daemonize ?(syslog=true) ?(stdin=`Dev_null) ?(stdout=`Log_default) ?(stderr=`Log_default) ?(directory="/") ?(umask=`Set 0o022) () =
  Unix.chdir directory;

  (* Exit the parent, and continue in the child: *)
  if Lwt_unix.fork () > 0 then begin
    (* Do not run exit hooks in the parent. *)
    Lwt_sequence.iter_node_l Lwt_sequence.remove Lwt_main.exit_hooks;
    exit 0
  end;

  if syslog then Lwt_log.default := Lwt_log.syslog ~facility:`Daemon ();

  (* Redirection of standard IOs *)
  let dev_null = Unix.openfile "/dev/null" [Unix.O_RDWR] 0o666 in
  begin match stdin with
    | `Dev_null ->
      Unix.dup2 dev_null Unix.stdin
    | `Close ->
      Unix.close Unix.stdin
    | `Keep ->
      ()
  end;
  redirect_output dev_null Unix.stdout stdout;
  redirect_output dev_null Unix.stderr stderr;
  Unix.close dev_null;

  begin match umask with
    | `Keep ->
      ()
    | `Set n ->
      ignore (Unix.umask n);
  end;

  ignore (Unix.setsid ())

