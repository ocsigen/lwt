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

type level =
  | Debug
  | Info
  | Warning
  | Error
  | Fatal

let string_of_level = function
  | Debug -> "debug"
  | Info -> "info"
  | Warning -> "warning"
  | Error -> "error"
  | Fatal -> "fatal"

let default_level =
  try
    match String.lowercase (Sys.getenv "LWT_LOG") with
      | "0" | "fatal" -> Fatal
      | "1" | "error" -> Error
      | "2" | "warning" -> Warning
      | "3" | "info" -> Info
      | "4" | "debug" -> Debug
      | str ->
          log_intern "wrong value for LWT_LOG: %S" str;
          Warning
  with Not_found ->
    Warning

(* +-----------------------------------------------------------------+
   | Loggers                                                         |
   +-----------------------------------------------------------------+ *)

exception Logger_closed

type end_point = {
  ep_output : level -> string list -> unit Lwt.t;
  (* [output facility level lines] adds [line] to the output *)

  ep_close : unit -> unit Lwt.t;
  (* [close ()] should free resources used for the destination *)

  ep_mutex : Lwt_mutex.t;
  (* Mutex used to serialize line addition *)

  mutable ep_level : level;
  (* The logging level *)

  mutable ep_parents : merge list;
  (* Parents are the merge in which the end-point is.

     For exmaple in the following code:

     {[
       lwt logger1 = syslog ()
       and logger2 = channel ~close_mode:`keep ~channel:Lwt_io.stderr ()
       and logger3 = file ~file_name "foo" in

       let m1 = merge [logger1; logger2]
       and m2 = merge [logger2; logger3] in
     ]}

     [logger1] have as parent [m1]
     [logger2] have as parents [m1] and [m2]
     [logger3] have as parent [m2]
  *)

  ep_logger : logger;
  (* The logger associated with end-point *)
}

and merge = {
  mutable merge_children : node list;
  (* Elements of the merge node *)

  mutable merge_parents : merge list;
  (* Same as for end-points *)

  mutable merge_level : level;
  (* The logging level of this logger *)

  merge_logger : logger;
  (* The logger associated with this merge-point *)
}

and node =
  | End_point of end_point
  | Merge of merge

and logger_state =
  | Opened of node
  | Closed

and logger = logger_state ref

let rec close_rec recursive node = match node with
  | End_point ep ->
      ep.ep_logger := Closed;
      (* Remove it from all its parents *)
      List.iter
        (fun parent ->
           (* Remove the child from its parent *)
           parent.merge_children <- List.filter ((!=) node) parent.merge_children)
        ep.ep_parents;
      ep.ep_close ()
  | Merge merge ->
      merge.merge_logger := Closed;
      List.iter
        (fun parent ->
           parent.merge_children <- List.filter ((!=) node) parent.merge_children)
        merge.merge_parents;
      if recursive then
        Lwt_list.iter_p (close_rec true) merge.merge_children
      else
        return ()

let close ?(recursive=false) logger = match !logger with
  | Closed ->
      return ()
  | Opened logger ->
      close_rec recursive logger

let _close logger = close logger

let make ?(level=default_level) ~output ~close () =
  let rec logger =
    ref(Opened(End_point{ ep_output = output;
                          ep_close = close;
                          ep_mutex = Lwt_mutex.create ();
                          ep_level = level;
                          ep_parents = [];
                          ep_logger = logger }))
  in
  Lwt_gc.finalise_or_exit _close logger;
  logger

let merge ?(level=default_level) loggers =
  let children = List.map (fun logger ->
                             match !logger with
                               | Opened node -> node
                               | Closed -> raise Logger_closed) loggers in
  let rec result = ref(Opened(Merge merge))
  and merge = { merge_children = children;
                merge_parents = [];
                merge_level = level;
                merge_logger = result } in
  List.iter
    (function
       | End_point ep ->
           ep.ep_parents <- merge :: ep.ep_parents
       | Merge merge ->
           merge.merge_parents <- merge :: merge.merge_parents)
    children;
  result

let level logger =
  match !logger with
    | Closed ->
        raise Logger_closed
    | Opened(End_point ep) ->
        ep.ep_level
    | Opened(Merge merge) ->
        merge.merge_level

let set_level logger level =
  match !logger with
    | Closed ->
        raise Logger_closed
    | Opened(End_point ep) ->
        ep.ep_level <- level
    | Opened(Merge merge) ->
        merge.merge_level <- level

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

let render ~buffer ~template ~level ~message =
  Buffer.add_substitute buffer
    (function
       | "date" -> date_string ()
       | "name" -> program_name
       | "pid" -> string_of_int (Unix.getpid ())
       | "message" -> message
       | "level" -> string_of_level level
       | var -> Printf.ksprintf invalid_arg "Lwt_log.render_buffer: unknown variable %S" var)
    template

(* +-----------------------------------------------------------------+
   | Predefined loggers                                              |
   +-----------------------------------------------------------------+ *)

let channel ?level ?(template="$(name): $(message)") ~close_mode ~channel () =
  make ?level
    ~output:(fun level lines ->
               Lwt_io.atomic begin fun oc ->
                 let buf = Buffer.create 42 in
                 lwt () = Lwt_list.iter_s begin fun line ->
                   Buffer.clear buf;
                   render ~buffer:buf ~template ~level ~message:line;
                   Buffer.add_char buf '\n';
                   Lwt_io.write oc (Buffer.contents buf)
                 end lines in
                 Lwt_io.flush oc
               end channel)
    ~close:(match close_mode with
              | `keep -> return
              | `close -> (fun () -> Lwt_io.close channel))
    ()

let default =
  ref(channel ~close_mode:`keep ~channel:Lwt_io.stderr ())

let file ?level ?(template="$(date): $(message)") ?(mode=`append) ?(perm=0o640) ~file_name () =
  let flags = match mode with
    | `append ->
        [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND; Unix.O_NONBLOCK]
    | `truncate ->
        [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; Unix.O_NONBLOCK] in
  let oc = Lwt_io.open_file ~mode:Lwt_io.output ~flags ~perm:0o644 file_name in
  channel ?level ~template ~close_mode:`close ~channel:oc ()

let level_code = function
  | Fatal -> 0
  | Error -> 3
  | Warning -> 4
  | Info -> 6
  | Debug -> 7

type facility =
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
                return (DGRAM, fd)
              with
                | Unix.Unix_error(Unix.EPROTOTYPE, _, _) -> begin
                    Lwt_unix.close fd;
                    (* Then try with a stream socket: *)
                    let fd = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
                    try_lwt
	              lwt () = Lwt_unix.connect fd (Unix.ADDR_UNIX path) in
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

let syslog ?level ?(template="$(date) $(name)[$(pid)]: $(message)") ?(paths=["/dev/log"; "/var/run/log"]) ~facility () =
  let syslog_socket = ref None in
  let get_syslog () = match !syslog_socket with
    | Some x ->
        return x
    | None ->
        lwt x = syslog_connect paths in
        syslog_socket := Some x;
        return x
  in
  make ?level
    ~output:(fun level lines ->
               let buf = Buffer.create 42 in
               let make_line socket_type msg =
                 Buffer.clear buf;
                 Printf.bprintf buf "<%d>" ((facility_code facility lsl 3) lor level_code level);
                 render ~buffer:buf ~template ~level ~message:msg;
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
               print socket_type fd lines)
    ~close:(fun () ->
              match !syslog_socket with
                | None ->
                    return ()
                | Some(socket_type, fd) ->
                    shutdown fd;
                    return ())
    ()

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

let rec log_rec node level lines = match node with
  | End_point ep ->
      if level >= ep.ep_level then
        Lwt_mutex.with_lock ep.ep_mutex
          (fun () -> ep.ep_output level lines)
      else
        return ()
  | Merge merge ->
      if level >= merge.merge_level then
        Lwt_list.iter_p (fun node -> log_rec node level lines) merge.merge_children
      else
        return ()

let get_logger = function
  | None ->
      !(!default)
  | Some logger ->
      !logger

let log ?logger ~level fmt =
  Printf.ksprintf begin fun message ->
    match get_logger logger with
      | Closed ->
          raise Logger_closed
      | Opened logger ->
          ignore_result (log_rec logger level (split message))
  end fmt

let exn ?logger ?(level=Error) ~exn fmt =
  Printf.ksprintf begin fun message ->
    match get_logger logger with
      | Closed ->
          raise Logger_closed
      | Opened logger ->
          let message = message ^ ": " ^ Printexc.to_string exn in
          let message =
            if Printexc.backtrace_status () then
              match Printexc.get_backtrace () with
                | "" -> message
                | backtrace -> message ^ "\nbacktrace:\n" ^ backtrace
            else
              message
          in
          ignore_result (log_rec logger level (split message))
  end fmt
