(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_log
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

let program_name = Filename.basename Sys.argv.(0)

(* Errors happening in this module are always logged to [stderr]: *)
let log_failure fmt =
  Printf.ksprintf (fun msg -> ignore_result (Lwt_io.eprintlf "%s: Lwt_log: %s" program_name msg)) fmt

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

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

(* +-----------------------------------------------------------------+
   | Codes                                                           |
   +-----------------------------------------------------------------+ *)


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

let level_code = function
  | `Emergency -> 0
  | `Alert -> 1
  | `Critical -> 2
  | `Error -> 3
  | `Warning -> 4
  | `Notice -> 5
  | `Info -> 6
  | `Debug -> 7

(* +-----------------------------------------------------------------+
   | Destination                                                     |
   +-----------------------------------------------------------------+ *)

type operations = {
  op_add_line : facility -> level -> string -> string -> unit Lwt.t;
  (* [op_add_line facility level name msg] adds [line] to the
     output *)

  op_close : unit -> unit Lwt.t;
  (* [op_close ()] should free resources used for the destination *)
}

type destination = unit -> operations Lwt.t

let print_date buf =
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
  Printf.bprintf buf "%s % 2d %02d:%02d:%02d" month_string tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let format buf pid date name msg =
  if date then begin
    print_date buf;
    Buffer.add_char buf ' '
  end;
  Buffer.add_string buf name;
  if pid then Printf.bprintf buf "[%d]" (Unix.getpid ());
  Buffer.add_string buf ": ";
  Buffer.add_string buf msg

let truncate buf max =
  if Buffer.length buf > max then begin
    let suffix = "<truncated>" in
    let len_suffix = String.length suffix in
    let str = Buffer.sub buf 0 max in
    StringLabels.blit ~src:suffix ~src_pos:0 ~dst:str ~dst_pos:(max - len_suffix) ~len:len_suffix;
    str
  end else
    Buffer.contents buf

let dest_channel ?(pid=true) ?(date=true) ~close_mode ~channel () = return {
  op_add_line = (fun facility level name msg ->
                   let buf = Buffer.create 42 in
                   format buf pid date name msg;
                   Buffer.add_char buf '\n';
                   lwt () = Lwt_io.write channel (Buffer.contents buf) in
                   Lwt_io.flush channel);
  op_close = match close_mode with
    | `keep -> return
    | `close -> (fun () -> Lwt_io.close channel)
}

let dest_stdout = dest_channel ~channel:Lwt_io.stdout ~close_mode:`keep ~pid:false ~date:false
let dest_stderr = dest_channel ~channel:Lwt_io.stderr ~close_mode:`keep ~pid:false ~date:false

let dest_file ?(pid=true) ?(date=true) ?(mode=`append) ?(perm=0o640) ~file_name () =
  let flags = match mode with
    | `append ->
        [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND; Unix.O_NONBLOCK]
    | `truncate ->
        [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; Unix.O_NONBLOCK] in
  let oc = Lwt_io.open_file ~mode:Lwt_io.output ~flags ~perm:0o644 file_name in
  dest_channel ~pid ~date ~close_mode:`close ~channel:oc ()

let dest_syslog ?(pid=true) ?(date=true) ?(path="/dev/log") () () =
  let path = "/dev/log" in
  (try
     return (Unix.stat path).Unix.st_kind
   with Unix.Unix_error(_, _, _) ->
     log_failure "can not open the syslog socket or pipe, is syslogd running?";
     fail (Failure "Lwt_log.dest_syslog"))
  >>= function
    | Unix.S_SOCK ->
        let open_connection socket_type =
	  let fd = Lwt_unix.socket Unix.PF_UNIX socket_type 0 in
	  lwt () = Lwt_unix.connect fd (Unix.ADDR_UNIX path) in
          return {
            op_add_line = (fun facility level name msg ->
                             let buf = Buffer.create 42 in
                             Printf.bprintf buf "<%d>" ((facility_code facility lsl 3) lor level_code level);
                             format buf pid date name msg;
                             let str = truncate buf 1024 in
                             lwt _ = Lwt_unix.write fd str 0 (String.length str) in
                             return ());
            op_close = (fun () ->
                          Lwt_unix.shutdown fd Unix.SHUTDOWN_ALL;
                          Lwt_unix.close fd;
                          return ());
          } in
        begin
          try
            open_connection Unix.SOCK_DGRAM
	  with Unix.Unix_error (Unix.EPROTOTYPE, _, _) ->
	    (* Try again with a stream socket for syslog-ng *)
            open_connection Unix.SOCK_STREAM
        end
    | Unix.S_FIFO ->
        let oc = Lwt_io.open_file ~mode:Lwt_io.output path in
        dest_channel ~channel:oc ~close_mode:`close ();
    | _ ->
        log_failure "%S is not a socket or a pipe" path;
        fail (Failure "Lwt_log.odest_syslog")

(* +-----------------------------------------------------------------+
   | Loggers                                                         |
   +-----------------------------------------------------------------+ *)

exception Logger_closed

type logger_info = {
  li_levels : bool array;
  li_name : string;
  li_facility : facility;
  li_destinations : operations list;
}

type logger_state =
  | Opened of logger_info
  | Closed

type logger = logger_state ref

let close logger = match !logger with
  | Closed ->
      return ()
  | Opened li ->
      logger := Closed;
      (* Close all destinations *)
      Lwt.join (List.map (fun op -> op.op_close ()) li.li_destinations)

let create ?(facility=`User) ?(name=program_name) destinations =
  let rec aux acc = function
    | e :: l ->
        lwt op =
          try_lwt
            e ()
          with exn ->
            log_failure "cannot open logging destination: %s" (Printexc.to_string exn);
            (* Closes previously opened loggers: *)
            lwt () = Lwt.join (List.map (fun op -> op.op_close ()) acc) in
            fail exn
        in
        aux (op :: acc) l
    | [] ->
        return (List.rev acc)
  in
  lwt destinations = aux [] destinations in
  let logger = ref (Opened {
                      li_levels = Array.make 8 true;
                      li_name = name;
                      li_facility = `User;
                      li_destinations = destinations;
                    }) in
  Lwt_gc.finalise_or_exit close logger;
  return logger

let default = ref (ref (Opened {
                          li_levels = [| true; true; true; true; true; true; false; false |];
                          li_name = program_name;
                          li_facility = `User;
                          li_destinations = [{ op_add_line = (fun facility level name msg ->
                                                                let buf = Buffer.create 42 in
                                                                format buf false false name msg;
                                                                Buffer.add_char buf '\n';
                                                                lwt () = Lwt_io.write Lwt_io.stderr (Buffer.contents buf) in
                                                                Lwt_io.flush Lwt_io.stderr);
                                               op_close  = return }];
                        }))

let get_logger = function
  | None -> !(!default)
  | Some logger -> !logger

let __unsafe_level logger num =
  match !logger with
    | Closed ->
        raise Logger_closed
    | Opened li ->
        li.li_levels.(num)

let level ?logger level =
  match get_logger logger with
    | Closed ->
        raise Logger_closed
    | Opened li ->
        li.li_levels.(level_code level)

let set_level ?logger level value =
  match get_logger logger with
    | Closed ->
        raise Logger_closed
    | Opened li ->
        li.li_levels.(level_code level) <- value

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

let log ?logger ?facility ~level str =
  match get_logger logger with
    | Closed ->
        raise Logger_closed
    | Opened li when li.li_levels.(level_code level) ->
        let facility = match facility with
          | Some facility -> facility
          | None -> li.li_facility
        in
        let lines = split str in
        List.iter
          (fun op -> List.iter (fun line -> ignore (op.op_add_line facility level li.li_name line)) lines)
          li.li_destinations
    | Opened li ->
        ()

let logf ?logger ?facility ~level fmt =
  Printf.ksprintf (log ?logger ?facility ~level) fmt
