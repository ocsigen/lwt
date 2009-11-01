(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Interface Lwt_log
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


(* This code is an adaptation of [syslog-ocaml] *)

open Lwt

type facility =
    [ `Kernel
    | `User
    | `Mail
    | `Daemon
    | `Auth
    | `Syslog
    | `LPR
    | `News
    | `UUCP
    | `Cron
    | `Authpriv
    | `FTP
    | `NTP
    | `Security
    | `Console
    | `Local0
    | `Local1
    | `Local2
    | `Local3
    | `Local4
    | `Local5
    | `Local6
    | `Local7 ]

type level =
    [ `Emergency
    | `Alert
    | `Critical
    | `Error
    | `Warning
    | `Notice
    | `Info
    | `Debug ]

let facility_of_string s =
  match String.lowercase s with
    | "kern" | "kernel" -> `Kernel
    | "user" -> `User
    | "mail" -> `Mail
    | "daemon" -> `Daemon
    | "auth" -> `Auth
    | "syslog" -> `Syslog
    | "lpr" -> `LPR
    | "news" -> `News
    | "uucp" -> `UUCP
    | "cron" -> `Cron
    | "authpriv" -> `Authpriv
    | "ftp" -> `FTP
    | "ntp" -> `NTP
    | "security" -> `Security
    | "console" -> `Console
    | "local0" -> `Local0
    | "local1" -> `Local1
    | "local2" -> `Local2
    | "local3" -> `Local3
    | "local4" -> `Local4
    | "local5" -> `Local5
    | "local6" -> `Local6
    | "local7" -> `Local7
    | invalid -> Printf.ksprintf failwith "Lwt_log.facility_of_string: invalid facility, %S" invalid

let string_of_facility = function
  | `Kernel -> "kernel"
  | `User -> "user"
  | `Mail -> "mail"
  | `Daemon -> "daemon"
  | `Auth -> "auth"
  | `Syslog -> "syslog"
  | `LPR -> "lpr"
  | `News -> "news"
  | `UUCP -> "uucp"
  | `Cron -> "cron"
  | `Authpriv -> "authpriv"
  | `FTP -> "ftp"
  | `NTP -> "ntp"
  | `Security -> "security"
  | `Console -> "console"
  | `Local0 -> "local0"
  | `Local1 -> "local1"
  | `Local2 -> "local2"
  | `Local3 -> "local3"
  | `Local4 -> "local4"
  | `Local5 -> "local5"
  | `Local6 -> "local6"
  | `Local7 -> "local7"


let int_of_facility = function
  | `Kernel -> 0 lsl 3
  | `User -> 1 lsl 3
  | `Mail -> 2 lsl 3
  | `Daemon -> 3 lsl 3
  | `Auth -> 4 lsl 3
  | `Syslog -> 5 lsl 3
  | `LPR -> 6 lsl 3
  | `News -> 7 lsl 3
  | `UUCP -> 8 lsl 3
  | `Cron -> 9 lsl 3
  | `Authpriv -> 10 lsl 3
  | `FTP -> 11 lsl 3
  | `NTP -> 12 lsl 3
  | `Security -> 13 lsl 3
  | `Console -> 14 lsl 3
  | `Local0 -> 16 lsl 3
  | `Local1 -> 17 lsl 3
  | `Local2 -> 18 lsl 3
  | `Local3 -> 19 lsl 3
  | `Local4 -> 20 lsl 3
  | `Local5 -> 21 lsl 3
  | `Local6 -> 22 lsl 3
  | `Local7 -> 23 lsl 3

let int_of_level = function
  | `Emergency -> 0
  | `Alert -> 1
  | `Critical -> 2
  | `Error -> 3
  | `Warning -> 4
  | `Notice -> 5
  | `Info -> 6
  | `Debug -> 7

type logger = {
  mutable oc : Lwt_io.output_channel option;
  mutable path : string;
  name : string;
  facility : int;
  pid : bool;
  copy_to : Lwt_io.output_channel option;
  console : bool;
  mutable closed : bool;
}

let program_name = Filename.basename Sys.argv.(0)

(* Wait for syslog to comes up *)
let wait_for_syslog logger =
  let rec aux = function
    | 0 ->
        if logger.console then
          ignore_result
            (Lwt_io.eprintlf "%s: can not open the syslog socket or pipe, is syslogd running?" program_name);
        fail (Failure "Lwt_log.wait_for_syslog")
    | n ->
        try
          ignore (Unix.stat logger.path);
          return ()
        with Unix.Unix_error(Unix.ENOENT, _, _) ->
          (* Wait and try again *)
          lwt () = Lwt_unix.sleep 0.5 in
          aux (n - 1)
  in
  aux 30

let rec open_connection logger =
  match (Unix.stat logger.path).Unix.st_kind with
    | Unix.S_SOCK ->
	let addr = Unix.ADDR_UNIX logger.path in
        begin
          try
	    let fd = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_DGRAM 0 in
	    lwt () = Lwt_unix.connect fd addr in
            let oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in
            logger.oc <- Some oc;
            return oc
	  with Unix.Unix_error (Unix.EPROTOTYPE, _, _) ->
	    (* try again with a stream socket for syslog-ng *)
	    let fd = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
	    lwt () = Lwt_unix.connect fd addr in
            let oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in
            logger.oc <- Some oc;
            return oc
        end
    | Unix.S_FIFO ->
        let oc = Lwt_io.open_file ~mode:Lwt_io.output logger.path in
	logger.oc <- Some oc;
        return oc
    | _ ->
        fail (Failure "Lwt_log.open_connection: invalid log path, not a socket or pipe")

let create ?(path="/dev/log") ?(facility=`User) ?copy_to ?(console=true) ?(pid=true)
    ?(no_delay=false) ?(name=Filename.basename Sys.argv.(0)) () =
  let logger = {
    oc = None;
    path = path;
    facility = int_of_facility facility;
    copy_to = copy_to;
    pid = pid;
    name = name;
    closed = false;
    console = false;
  } in
  if no_delay then
    open_connection logger >> return logger
  else
    return logger

let close logger =
  logger.closed <- true;
  lwt () = begin
    match logger.oc with
      | None ->
          return ()
      | Some oc ->
          Lwt_io.close oc
  end in
  logger.oc <- None;
  return ()

let ascdate tm  =
  let asc_mon =
    match tm.Unix.tm_mon with
      | 0 -> "Jan"
      | 1 -> "Feb"
      | 2 -> "Mar"
      | 3 -> "Apr"
      | 4 -> "May"
      | 5 -> "Jun"
      | 6 -> "Jul"
      | 7 -> "Aug"
      | 8 -> "Sep"
      | 9 -> "Oct"
      | 10 -> "Nov"
      | 11 -> "Dec"
      | _ -> Printf.ksprintf failwith "Lwt_log.ascdate: invalid month, %d" tm.Unix.tm_mon
  in
  Printf.sprintf "%s %02d %02d:%02d:%02d" asc_mon tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let global = ref {
  oc = None;
  path = "/dev/log";
  facility = int_of_facility `User;
  copy_to = None;
  pid = true;
  name = program_name;
  closed = false;
  console = false;
}

let log_t ?logger ?facility ~level str =
  (* Choose the logger *)
  let logger = match logger with
    | None -> !global
    | Some logger -> logger
  in
  if logger.closed then
    return ()
  else begin
    let msg = Buffer.create 64 in
    let facility = match facility with
      | Some fac -> int_of_facility fac
      | None -> logger.facility
    in
    lwt oc = match logger.oc with
      | Some oc -> return oc
      | None -> open_connection logger
    in
    let levfac = facility lor (int_of_level level) and now = ascdate (Unix.localtime (Unix.time ())) in
    Printf.bprintf msg "<%d>%.15s " levfac now;
    let len1 = Buffer.length msg and len2 = String.length logger.name in
    if len1 + len2 < 64 then
      Buffer.add_string msg logger.name
    else
      Buffer.add_substring msg logger.name 0 (64 - len1);
    if logger.pid then
      Printf.bprintf msg "[%d]" (Unix.getpid());
    if String.length logger.name > 0 then
      Buffer.add_string msg ": ";
    Buffer.add_string msg str;
    let msg = ref (Buffer.contents msg) in
    if String.length !msg > 1023 then begin
      msg := String.sub !msg 0 1024;
      String.blit "<truncated>" 0 !msg 1012 11
    end;
    logger.oc <- None;
    lwt () =
      try_lwt
        Lwt_io.fprint oc !msg
      with _ ->
        return ()
    in
    match logger.copy_to with
      | None ->
          return ()
      | Some oc ->
          Lwt_io.fprintl oc !msg
  end

let rec log_multi_t ?logger ?facility ~level = function
  | [] ->
      return ()
  | x :: l ->
      lwt () = log_t ?logger ?facility ~level x in
      log_multi_t ?logger ?facility ~level l

let logf_t ?logger ?facility ~level fmt =
  Printf.ksprintf (log_t ?logger ?facility ~level) fmt

let log ?logger ?facility ~level str =
  ignore_result (log_t ?logger ?facility ~level str)

let log_multi ?logger ?facility ~level lines =
  ignore_result (log_multi_t ?logger ?facility ~level lines)

let logf ?logger ?facility ~level fmt =
  Printf.ksprintf (log ?logger ?facility ~level) fmt
