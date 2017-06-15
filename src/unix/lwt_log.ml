(* OCaml promise library
 * http://www.ocsigen.org/lwt
 * Copyright (C) 2002 Shawn Wagner <raevnos@pennmush.org>
 *               2009 Jérémie Dimino <jeremie@dimino.org>
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

include Lwt_log_core

open Lwt.Infix

let program_name = Filename.basename Sys.executable_name

(* Errors happening in this module are always logged to [stderr]: *)
let log_intern fmt =
  Printf.eprintf ("%s: Lwt_log: " ^^ fmt ^^ "\n%!") program_name

(* +-----------------------------------------------------------------+
   | Templates                                                       |
   +-----------------------------------------------------------------+ *)

let date_string time =
  let tm = Unix.localtime time in
  let month_string =
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
    | _ -> Printf.ksprintf failwith "Lwt_log.date_string: invalid month, %d" tm.Unix.tm_mon
  in
  Printf.sprintf "%s %2d %02d:%02d:%02d" month_string tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let render ~buffer ~template ~section ~level ~message =
  let time = lazy(Unix.gettimeofday ()) in
  let file, line, column =
    match Lwt.get location_key with
    | Some loc -> loc
    | None -> ("<unknown>", -1, -1)
  in
  Buffer.add_substitute buffer
    (function
      | "date" -> date_string (Lazy.force time)
      | "milliseconds" -> Printf.sprintf "%03.0f" (mod_float (Lazy.force time *. 1000.) 1000.)
      | "name" -> program_name
      | "pid" -> string_of_int (Unix.getpid ())
      | "message" -> message
      | "level" -> Lwt_log_core.string_of_level level
      | "section" -> Section.name section
      | "loc-file" -> file
      | "loc-line" -> string_of_int line
      | "loc-column" -> string_of_int column
      | var -> Printf.ksprintf invalid_arg "Lwt_log.render: unknown variable %S" var)
    template

(* +-----------------------------------------------------------------+
   | Predefined loggers                                              |
   +-----------------------------------------------------------------+ *)

let channel ?(template="$(name): $(section): $(message)") ~close_mode ~channel () =
  make
    ~output:(fun section level lines ->
      Lwt_io.atomic begin fun oc ->
        let buf = Buffer.create 42 in
        Lwt_list.iter_s
          (fun line ->
             Buffer.clear buf;
             render ~buffer:buf ~template ~section ~level ~message:line;
             Buffer.add_char buf '\n';
             Lwt_io.write oc (Buffer.contents buf))
          lines >>= fun () ->
        Lwt_io.flush oc
      end channel)
    ~close:(match close_mode with
      | `Keep -> Lwt.return
      | `Close -> (fun () -> Lwt_io.close channel))

let _ = Lwt_log_core.default := channel ~close_mode:`Keep ~channel:Lwt_io.stderr ()

let file ?(template="$(date): $(section): $(message)") ?(mode=`Append) ?(perm=0o640) ~file_name () =
  let flags = match mode with
    | `Append ->
      [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND; Unix.O_NONBLOCK]
    | `Truncate ->
      [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; Unix.O_NONBLOCK] in
  Lwt_unix.openfile file_name flags perm >>= fun fd ->
  Lwt_unix.set_close_on_exec fd;
  let oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in
  Lwt.return (channel ~template ~close_mode:`Close ~channel:oc ())

let level_code = function
  | Fatal -> 0
  | Error -> 3
  | Warning -> 4
  | Notice -> 5
  | Info -> 6
  | Debug -> 7

type syslog_facility =
  [ `Auth | `Authpriv | `Cron | `Daemon | `FTP | `Kernel
  | `Local0 | `Local1 | `Local2 | `Local3 | `Local4 | `Local5 | `Local6 | `Local7
  | `LPR | `Mail | `News | `Syslog | `User | `UUCP | `NTP | `Security | `Console ]

let facility_code = function
  | `Kernel -> 0
  | `User -> 1
  | `Mail -> 2
  | `Daemon -> 3
  | `Auth -> 4
  | `Syslog -> 5
  | `LPR -> 6
  | `News -> 7
  | `UUCP -> 8
  | `Cron -> 9
  | `Authpriv -> 10
  | `FTP -> 11
  | `NTP -> 12
  | `Security -> 13
  | `Console -> 14
  | `Local0 -> 16
  | `Local1 -> 17
  | `Local2 -> 18
  | `Local3 -> 19
  | `Local4 -> 20
  | `Local5 -> 21
  | `Local6 -> 22
  | `Local7 -> 23

type syslog_connection_type = STREAM | DGRAM

let shutdown fd =
  Lwt_unix.shutdown fd Unix.SHUTDOWN_ALL;
  Lwt_unix.close fd

(* Try to find a socket in [paths]. For each path it check that the
   file is a socket and try to establish connection in DGRAM mode then in
   STREAM mode. *)
let syslog_connect paths =
  let rec loop = function
    | [] ->
      (* No working socket found *)
      log_intern "no working socket found in {%s}; is syslogd running?"
        (String.concat ", " (List.map (Printf.sprintf "\"%s\"") paths));
      Lwt.fail (Sys_error(Unix.error_message Unix.ENOENT))
    | path :: paths ->
      begin try
          Lwt.return (Some (Unix.stat path).Unix.st_kind)
        with
        | Unix.Unix_error(Unix.ENOENT, _, _) ->
          Lwt.return_none
        | Unix.Unix_error(error, _, _) ->
          log_intern "can not stat \"%s\": %s" path (Unix.error_message error);
          Lwt.return_none
      end >>= (function
        | None ->
          loop paths
        | Some Unix.S_SOCK -> begin
            (* First, we try with a dgram socket : *)
            let fd = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_DGRAM 0 in
            Lwt.catch
              (fun () ->
                 Lwt_unix.connect fd (Unix.ADDR_UNIX path) >>= fun () ->
                 Lwt_unix.set_close_on_exec fd;
                 Lwt.return (DGRAM, fd))
              (function
                | Unix.Unix_error(Unix.EPROTOTYPE, _, _) -> begin
                    Lwt_unix.close fd >>= fun () ->
                    (* Then try with a stream socket: *)
                    let fd = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
                    Lwt.catch
                      (fun () ->
                         Lwt_unix.connect fd (Unix.ADDR_UNIX path) >>= fun () ->
                         Lwt_unix.set_close_on_exec fd;
                         Lwt.return (STREAM, fd))
                      (function
                        | Unix.Unix_error(error, _, _) ->
                          Lwt_unix.close fd >>= fun () ->
                          log_intern "can not connect to \"%s\": %s" path (Unix.error_message error);
                          loop paths
                        | exn -> Lwt.fail exn)
                  end
                | Unix.Unix_error(error, _, _) ->
                  Lwt_unix.close fd >>= fun () ->
                  log_intern "can not connect to \"%s\": %s" path (Unix.error_message error);
                  loop paths
                | exn -> Lwt.fail exn) [@ocaml.warning "-4"]
          end
        | Some _ ->
          log_intern "\"%s\" is not a socket" path;
          loop paths) [@ocaml.warning "-4"]
  in
  loop paths

(* Write the whole contents of a string on the given file
   descriptor: *)
let write_string fd str =
  let len = String.length str in
  let rec aux start_ofs =
    if start_ofs = len then
      Lwt.return_unit
    else
      Lwt_unix.write_string fd str start_ofs (len - start_ofs) >>= fun n ->
      if n <> 0 then
        aux (start_ofs + n)
      else
        Lwt.return_unit
  in
  aux 0

let truncate buf max =
  if Buffer.length buf > max then begin
    let suffix = "<truncated>" in
    let len_suffix = String.length suffix in
    Buffer.sub buf 0 (max - len_suffix) ^ suffix
  end else
    Buffer.contents buf

let syslog ?(template="$(date) $(name)[$(pid)]: $(section): $(message)") ?(paths=["/dev/log"; "/var/run/log"; "/var/run/syslog"]) ~facility () =
  let syslog_socket = ref None and mutex = Lwt_mutex.create () in
  let get_syslog () = match !syslog_socket with
    | Some x ->
      Lwt.return x
    | None ->
      syslog_connect paths >>= fun x ->
      syslog_socket := Some x;
      Lwt.return x
  in
  make
    ~output:(fun section level lines ->
      Lwt_mutex.with_lock mutex
        (fun () ->
           let buf = Buffer.create 42 in
           let make_line socket_type msg =
             Buffer.clear buf;
             Printf.bprintf buf "<%d>" ((facility_code facility lsl 3) lor level_code level);
             render ~buffer:buf ~template ~section ~level ~message:msg;
             if socket_type = STREAM then Buffer.add_char buf '\x00';
             truncate buf 1024
           in
           let rec print socket_type fd = function
             | [] ->
               Lwt.return_unit
             | line :: lines ->
               Lwt.catch
                 (fun () ->
                    write_string fd (make_line socket_type line) >>= fun () ->
                    print socket_type fd lines)
                 (function
                   | Unix.Unix_error(_, _, _) ->
                     (* Try to reconnect *)
                     shutdown fd >>= fun () ->
                     syslog_socket := None;
                     get_syslog () >>= fun (socket_type, fd) ->
                     write_string fd (make_line socket_type line) >>= fun () ->
                     print socket_type fd lines
                   | exn -> Lwt.fail exn)
           in
           get_syslog () >>= fun (socket_type, fd) ->
           print socket_type fd lines))
    ~close:(fun () ->
      match !syslog_socket with
      | None ->
        Lwt.return_unit
      | Some(_socket_type, fd) ->
        shutdown fd)
