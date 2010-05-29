(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_log
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

open Lwt

let program_name = Filename.basename Sys.argv.(0)

(* Errors happening in this module are always logged to [stderr]: *)
let log_intern fmt =
  Printf.ksprintf (fun msg -> ignore_result (Lwt_io.eprintlf "%s: Lwt_log: %s" program_name msg)) fmt

(* +-----------------------------------------------------------------+
   | Log levels                                                      |
   +-----------------------------------------------------------------+ *)

type level =
  | Debug
  | Info
  | Notice
  | Warning
  | Error
  | Fatal

let string_of_level = function
  | Debug -> "debug"
  | Info -> "info"
  | Notice -> "notice"
  | Warning -> "warning"
  | Error -> "error"
  | Fatal -> "fatal"

(* +-----------------------------------------------------------------+
   | Patterns andrules                                               |
   +-----------------------------------------------------------------+ *)

type pattern = string list
    (* A pattern is represented by a list of literals:

       For example ["foo*bar*"] is represented by ["foo"; "bar"; ""]. *)

let sub_equal str ofs patt =
  let str_len = String.length str and patt_len = String.length patt in
  let rec loop ofs ofs_patt =
    ofs_patt = patt_len || (str.[ofs] = patt.[ofs_patt] && loop (ofs + 1) (ofs_patt + 1))
  in
  ofs + patt_len <= str_len && loop ofs 0

let pattern_match pattern string =
  let length = String.length string in
  let rec loop offset pattern =
    if offset = length then
      pattern = [] || pattern = [""]
    else
      match pattern with
        | [] ->
            false
        | literal :: pattern ->
            let literal_length = String.length literal in
            let max_offset = length - literal_length in
            let rec search offset =
              offset <= max_offset
              && ((sub_equal string offset literal && loop (offset + literal_length) pattern)
                  || search (offset + 1))
            in
            search offset
  in
  match pattern with
    | [] ->
        string = ""
    | literal :: pattern ->
        sub_equal string 0 literal && loop (String.length literal) pattern

let split pattern =
  let len = String.length pattern in
  let rec loop ofs =
    if ofs = len then
      [""]
    else
      match try Some(String.index_from pattern ofs '*') with Not_found -> None with
        | Some ofs' ->
            String.sub pattern ofs (ofs' - ofs) :: loop (ofs' + 1)
        | None ->
            [String.sub pattern ofs (len - ofs)]
  in
  loop 0

let rules =
  match try Some(Sys.getenv "LWT_LOG") with Not_found -> None with
    | Some str ->
        let rec loop = function
          | [] ->
              []
          | (pattern, level) :: rest ->
              let pattern = split pattern in
              match String.lowercase level with
                | "debug" -> (pattern, Debug) :: loop rest
                | "info" -> (pattern, Info) :: loop rest
                | "notice" -> (pattern, Notice) :: loop rest
                | "warning" -> (pattern, Warning) :: loop rest
                | "error" -> (pattern, Error) :: loop rest
                | "fatal" -> (pattern, Fatal) :: loop rest
                | level -> log_intern "invalid log level (%s)" level; loop rest
        in
        loop (Lwt_log_rules.rules (Lexing.from_string str))
    | None ->
        []

(* +-----------------------------------------------------------------+
   | Sections                                                        |
   +-----------------------------------------------------------------+ *)

module Section =
struct
  type t = {
    name : string;
    mutable level : level;
  }

  let make name =
    let rec find_level = function
      | [] ->
          Notice
      | (pattern, level) :: rest ->
          if pattern_match pattern name then
            level
          else
            find_level rest
    in
    { name = name; level = find_level rules }

  let name section = section.name

  let main = make "main"

  let level section = section.level

  let set_level section level = section.level <- level
end

type section = Section.t

(* +-----------------------------------------------------------------+
   | Loggers                                                         |
   +-----------------------------------------------------------------+ *)

exception Logger_closed

type logger = {
  mutable lg_closed : bool;
  lg_output : section -> level -> string list -> unit Lwt.t;
  lg_close : unit Lwt.t Lazy.t;
}

let close logger =
  logger.lg_closed <- true;
  Lazy.force logger.lg_close

let make ~output ~close =
  {
    lg_closed = false;
    lg_output = output;
    lg_close = Lazy.lazy_from_fun close;
  }

let broadcast loggers =
  make
    ~output:(fun section level lines ->
               Lwt_list.iter_p (fun logger -> logger.lg_output section level lines) loggers)
    ~close:return

let dispatch f =
  make
    ~output:(fun section level lines -> (f section level).lg_output section level lines)
    ~close:return

(* +-----------------------------------------------------------------+
   | Templates                                                       |
   +-----------------------------------------------------------------+ *)

type template = string

let date_string () =
  let tm = Unix.localtime (Unix.time ()) in
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
      | _ -> Printf.ksprintf failwith "Lwt_log.ascdate: invalid month, %d" tm.Unix.tm_mon
  in
  Printf.sprintf "%s %2d %02d:%02d:%02d" month_string tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let render ~buffer ~template ~section ~level ~message =
  Buffer.add_substitute buffer
    (function
       | "date" -> date_string ()
       | "name" -> program_name
       | "pid" -> string_of_int (Unix.getpid ())
       | "message" -> message
       | "level" -> string_of_level level
       | "section" -> Section.name section
       | var -> Printf.ksprintf invalid_arg "Lwt_log.render_buffer: unknown variable %S" var)
    template

(* +-----------------------------------------------------------------+
   | Predefined loggers                                              |
   +-----------------------------------------------------------------+ *)

let null =
  make
    ~output:(fun section level lines -> return ())
    ~close:return

let channel ?(template="$(name): $(section): $(message)") ~close_mode ~channel () =
  make
    ~output:(fun section level lines ->
               Lwt_io.atomic begin fun oc ->
                 let buf = Buffer.create 42 in
                 lwt () =
                   Lwt_list.iter_s
                     (fun line ->
                        Buffer.clear buf;
                        render ~buffer:buf ~template ~section ~level ~message:line;
                        Buffer.add_char buf '\n';
                        Lwt_io.write oc (Buffer.contents buf))
                     lines
                 in
                 Lwt_io.flush oc
               end channel)
    ~close:(match close_mode with
              | `Keep -> return
              | `Close -> (fun () -> Lwt_io.close channel))

let default =
  ref(channel ~close_mode:`Keep ~channel:Lwt_io.stderr ())

let file ?(template="$(date): $(section): $(message)") ?(mode=`Append) ?(perm=0o640) ~file_name () =
  let flags = match mode with
    | `Append ->
        [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND; Unix.O_NONBLOCK]
    | `Truncate ->
        [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; Unix.O_NONBLOCK] in
  let fd = Lwt_unix.openfile file_name flags 0o666 in
  Lwt_unix.set_close_on_exec fd;
  let oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in
  channel ~template ~close_mode:`Close ~channel:oc ()

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
        fail (Sys_error(Unix.error_message Unix.ENOENT))
    | path :: paths ->
        begin try
          return (Some (Unix.stat path).Unix.st_kind)
        with
          | Unix.Unix_error(Unix.ENOENT, _, _) ->
              return None
          | Unix.Unix_error(error, _, _) ->
              log_intern "can not stat \"%s\": %s" path (Unix.error_message error);
              return None
        end >>= function
          | None ->
              loop paths
          | Some Unix.S_SOCK -> begin
              (* First, we try with a dgram socket : *)
              let fd = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_DGRAM 0 in
              try_lwt
                lwt () = Lwt_unix.connect fd (Unix.ADDR_UNIX path) in
                Lwt_unix.set_close_on_exec fd;
                return (DGRAM, fd)
              with
                | Unix.Unix_error(Unix.EPROTOTYPE, _, _) -> begin
                    Lwt_unix.close fd;
                    (* Then try with a stream socket: *)
                    let fd = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
                    try_lwt
	              lwt () = Lwt_unix.connect fd (Unix.ADDR_UNIX path) in
                      Lwt_unix.set_close_on_exec fd;
                      return (STREAM, fd)
                    with Unix.Unix_error(error, _, _) ->
                      Lwt_unix.close fd;
                      log_intern "can not connect to \"%s\": %s" path (Unix.error_message error);
                      loop paths
                  end
                | Unix.Unix_error(error, _, _) ->
                    Lwt_unix.close fd;
                    log_intern "can not connect to \"%s\": %s" path (Unix.error_message error);
                    loop paths
            end
          | Some _ ->
              log_intern "\"%s\" is not a socket" path;
              loop paths
  in
  loop paths

(* Write the whole contents of a string on the given file
   descriptor: *)
let write_string fd str =
  let len = String.length str in
  let rec aux start_ofs =
    if start_ofs = len then
      return ()
    else
      lwt n = Lwt_unix.write fd str start_ofs (len - start_ofs) in
      if n <> 0 then
        aux (start_ofs + n)
      else
        return ()
  in
  aux 0

let truncate buf max =
  if Buffer.length buf > max then begin
    let suffix = "<truncated>" in
    let len_suffix = String.length suffix in
    let str = Buffer.sub buf 0 max in
    StringLabels.blit ~src:suffix ~src_pos:0 ~dst:str ~dst_pos:(max - len_suffix) ~len:len_suffix;
    str
  end else
    Buffer.contents buf

let syslog ?(template="$(date) $(name)[$(pid)]: $(section): $(message)") ?(paths=["/dev/log"; "/var/run/log"]) ~facility () =
  let syslog_socket = ref None and mutex = Lwt_mutex.create () in
  let get_syslog () = match !syslog_socket with
    | Some x ->
        return x
    | None ->
        lwt x = syslog_connect paths in
        syslog_socket := Some x;
        return x
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
                          return ()
                      | line :: lines ->
                          try_lwt
                            lwt () = write_string fd (make_line socket_type line) in
                            print socket_type fd lines
                          with Unix.Unix_error(_, _, _) ->
                            (* Try to reconnect *)
                            shutdown fd;
                            syslog_socket := None;
                            lwt socket_type, fd = get_syslog () in
                            lwt () = write_string fd (make_line socket_type line) in
                            print socket_type fd lines
                    in
                    lwt socket_type, fd = get_syslog () in
                    print socket_type fd lines))
    ~close:(fun () ->
              match !syslog_socket with
                | None ->
                    return ()
                | Some(socket_type, fd) ->
                    shutdown fd;
                    return ())

(* +-----------------------------------------------------------------+
   | Logging functions                                               |
   +-----------------------------------------------------------------+ *)

let split str =
  let len = String.length str in
  let rec aux i =
    if i >= len then
      []
    else
      let j = try String.index_from str i '\n' with Not_found -> String.length str in
      String.sub str i (j - i) :: aux (j + 1)
  in
  aux 0

let log ?exn ?(section=Section.main) ?logger ~level message =
  let logger = match logger with
    | None -> !default
    | Some logger -> logger
  in
  if logger.lg_closed then
    fail Logger_closed
  else if level >= section.Section.level then
    match exn with
      | None ->
          logger.lg_output section level (split message)
      | Some exn ->
          let message = message ^ ": " ^ Printexc.to_string exn in
          let message =
            if Printexc.backtrace_status () then
              match Printexc.get_backtrace () with
                | "" -> message
                | backtrace -> message ^ "\nbacktrace:\n" ^ backtrace
            else
              message
          in
          logger.lg_output section level (split message)
  else
    return ()

let log_f ?exn ?section ?logger ~level format =
  Printf.ksprintf (log ?exn ?section ?logger ~level) format

let debug ?exn ?section ?logger msg = log ?exn ?section ?logger ~level:Debug msg
let debug_f ?exn ?section ?logger fmt = Printf.ksprintf (debug ?exn ?section ?logger) fmt
let info ?exn ?section ?logger msg = log ?exn ?section ?logger ~level:Info msg
let info_f ?exn ?section ?logger fmt = Printf.ksprintf (info ?exn ?section ?logger) fmt
let notice ?exn ?section ?logger msg = log ?exn ?section ?logger ~level:Notice msg
let notice_f ?exn ?section ?logger fmt = Printf.ksprintf (notice ?exn ?section ?logger) fmt
let warning ?exn ?section ?logger msg = log ?exn ?section ?logger ~level:Warning msg
let warning_f ?exn ?section ?logger fmt = Printf.ksprintf (warning ?exn ?section ?logger) fmt
let error ?exn ?section ?logger msg = log ?exn ?section ?logger ~level:Error msg
let error_f ?exn ?section ?logger fmt = Printf.ksprintf (error ?exn ?section ?logger) fmt
let fatal ?exn ?section ?logger msg = log ?exn ?section ?logger ~level:Fatal msg
let fatal_f ?exn ?section ?logger fmt = Printf.ksprintf (fatal ?exn ?section ?logger) fmt
