(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_io
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

open Lwt

let rec copy ic logger =
  lwt line = Lwt_io.read_line ic in
  lwt () = Lwt_log.log ?logger ~level:Lwt_log.Notice line in
  copy ic logger

let redirect fd logger =
  let fd_r, fd_w = Unix.pipe () in
  Unix.set_close_on_exec fd_r;
  Unix.dup2 fd_w fd;
  Unix.close fd_w;
  let ic = Lwt_io.of_unix_fd ~mode:Lwt_io.input fd in
  ignore (copy ic logger)

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
  if Unix.getppid () = 1 then
    (* If our parent is [init], then we already are a demon *)
    ()
  else begin
    Unix.chdir directory;

    (* Exit the parent, and continue in the child: *)
    if Unix.fork () > 0 then exit 0;

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
          ignore (Unix.umask 0o022);
    end;

    ignore (Unix.setsid ())
  end

