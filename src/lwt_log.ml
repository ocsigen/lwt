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

type levels = {
  emergency : bool;
  alert : bool;
  critical : bool;
  error : bool;
  warning : bool;
  notice : bool;
  info : bool;
  debug : bool;
}

let levels_true = {
  emergency = true;
  alert = true;
  critical = true;
  error = true;
  warning = true;
  notice = true;
  info = true;
  debug = true;
}

let levels_false = {
  emergency = false;
  alert = false;
  critical = false;
  error = false;
  warning = false;
  notice = false;
  info = false;
  debug = false;
}

(* +-----------------------------------------------------------------+
   | Loggers                                                         |
   +-----------------------------------------------------------------+ *)

exception Logger_closed

type end_point = {
  ep_add_lines : facility -> level -> string list -> unit Lwt.t;
  (* [add_lines facility level lines] adds [line] to the output *)

  ep_close : unit -> unit Lwt.t;
  (* [close ()] should free resources used for the destination *)

  ep_mutex : Lwt_mutex.t;
  (* Mutex used to serialize line addition *)

  ep_levels : bool array;
  (* Enabled levels *)

  ep_facility : facility;
  (* The default facility *)

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

  merge_levels : int array;
  (* [merge_levels.(i)] represents the number of children for which
     the level [i] is enabled. *)

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

type logger_maker = ?pid : bool -> ?date : bool -> ?facility : facility ->
  ?levels : levels -> ?name : string -> unit -> logger

(* Decrement the number of children with level [level] enabled: *)
let rec decrement_level merge level =
  let count = merge.merge_levels.(level) in
  merge.merge_levels.(level) <- count - 1;
  if count = 1 then
    (* If the enablme state has changed then update also parents: *)
    List.iter (fun parent -> decrement_level parent level) merge.merge_parents

(* Increment the number of children with level [level] enabled: *)
let rec increment_level merge level =
  let count = merge.merge_levels.(level) in
  merge.merge_levels.(level) <- count + 1;
  if count = 0 then
    List.iter (fun parent -> increment_level parent level) merge.merge_parents

let rec close_rec recursive node = match node with
  | End_point ep ->
      ep.ep_logger := Closed;
      (* Remove it from all its parents *)
      List.iter begin fun parent ->
        (* Remove the child from its parent *)
        parent.merge_children <- List.filter ((!=) node) parent.merge_children;
        (* Update levels of the parent *)
        for i = 0 to 7 do
          if ep.ep_levels.(i) then decrement_level parent i
        done
      end ep.ep_parents;
      ep.ep_close ()
  | Merge merge ->
      merge.merge_logger := Closed;
      lwt () =
        if recursive then
          Lwt_util.iter (close_rec true) merge.merge_children
        else
          return ()
      in
      List.iter begin fun parent ->
        parent.merge_children <- List.filter ((!=) node) parent.merge_children;
        for i = 0 to 7 do
          if merge.merge_levels.(i) <> 0 then decrement_level parent i
        done
      end merge.merge_parents;
      return ()

let close ?(recursive=false) logger = match !logger with
  | Closed ->
      return ()
  | Opened logger ->
      close_rec recursive logger

let default = ref (ref Closed)

let get_logger = function
  | None -> !(!default)
  | Some logger -> !logger

let unsafe_level logger num =
  match !logger with
    | Closed ->
        raise Logger_closed
    | Opened(End_point ep) ->
        ep.ep_levels.(num)
    | Opened(Merge merge) ->
        merge.merge_levels.(num) > 0

let level ?logger level =
  unsafe_level
    (match logger with
       | None -> !default
       | Some logger -> logger)
    (level_code level)

let rec set_level_rec node level value = match node with
  | End_point ep ->
      let level = level_code level in
      if ep.ep_levels.(level) <> value then begin
        ep.ep_levels.(level) <- value;
        let change_level = if value then increment_level else decrement_level in
        List.iter (fun parent -> change_level parent level) ep.ep_parents
      end
  | Merge merge ->
      List.iter (fun child -> set_level_rec node level value) merge.merge_children

let set_level ?logger level value =
  match get_logger logger with
    | Closed ->
        raise Logger_closed
    | Opened node ->
        set_level_rec node level value

let merge loggers =
  let children = List.map (fun logger ->
                             match !logger with
                               | Opened node -> node
                               | Closed -> raise Logger_closed) loggers in
  let levels = Array.create 8 0 in
  let rec result = ref(Opened(Merge merge))
  and merge = { merge_children = children;
                merge_parents = [];
                merge_levels = levels;
                merge_logger = result } in
  List.iter
    (fun child ->
       match child with
         | End_point ep ->
             ep.ep_parents <- merge :: ep.ep_parents
         | Merge merge ->
             merge.merge_parents <- merge :: merge.merge_parents)
    children;
  List.iter
    (fun logger ->
       for i = 0 to 7 do
         if unsafe_level logger i then
           levels.(i) <- levels.(i) + 1
       done)
    loggers;
  result

(* +-----------------------------------------------------------------+
   | End-points                                                      |
   +-----------------------------------------------------------------+ *)

(* Add current date to [buf]: *)
let add_date buf =
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
  Printf.bprintf buf "%s %2d %02d:%02d:%02d" month_string tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

(* Construit ine ligne de log dans [buf]: *)
let add_line buf pid date name msg =
  if date then begin
    add_date buf;
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

let array_of_levels levels = [|
  levels.emergency;
  levels.alert;
  levels.critical;
  levels.error;
  levels.warning;
  levels.notice;
  levels.info;
  levels.debug;
|]

let channel ~close_mode ~channel
    ?(pid=true) ?(date=true) ?(facility=`User) ?(levels=levels_true) ?(name=program_name) () =
  let rec result = ref(Opened(End_point{
    ep_add_lines = begin
      fun facility level lines ->
        lwt () = Lwt_io.atomic begin fun oc ->
          let buf = Buffer.create 42 in
          Lwt_util.iter_serial begin fun line ->
            Buffer.clear buf;
            add_line buf pid date name line;
            Buffer.add_char buf '\n';
            Lwt_io.write oc (Buffer.contents buf)
          end lines
        end channel in
        Lwt_io.flush channel
    end;
    ep_close = begin
      match close_mode with
        | `keep -> return
        | `close -> (fun () -> Lwt_io.close channel)
    end;
    ep_mutex = Lwt_mutex.create ();
    ep_levels = array_of_levels levels;
    ep_facility = facility;
    ep_logger = result;
    ep_parents = [];
  })) in
  Lwt_gc.finalise_or_exit close result;
  result

let () = default := channel ~close_mode:`keep ~channel:Lwt_io.stderr ~pid:false ~date:false ()

let file ?(mode=`append) ?(perm=0o640) ~file_name
    ?(pid=true) ?(date=true) ?(facility=`User) ?(levels=levels_true) ?(name=program_name) () =
  let flags = match mode with
    | `append ->
        [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND; Unix.O_NONBLOCK]
    | `truncate ->
        [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; Unix.O_NONBLOCK] in
  let oc = Lwt_io.open_file ~mode:Lwt_io.output ~flags ~perm:0o644 file_name in
  channel ~pid ~date ~facility ~levels ~name ~close_mode:`close ~channel:oc ()

type syslog_connection =
  | SC_stream
  | SC_dgram

let shutdown fd =
  Lwt_unix.shutdown fd Unix.SHUTDOWN_ALL;
  Lwt_unix.close fd

let syslog_connect path =
  lwt kind =
    try
      return (Unix.stat path).Unix.st_kind
    with Unix.Unix_error(error, _, _) ->
      let msg = Unix.error_message error in
      log_intern "can not stat %S: %s. Is syslogd running?" path msg;
      fail (Sys_error msg)
  in
  if kind <> Unix.S_SOCK then begin
    log_intern "%S is not a socket" path;
    fail (Sys_error(Printf.sprintf  "%S is not a socket" path))
  end else begin
    (* First, we try with a dgram socket : *)
    let fd = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_DGRAM 0 in
    try_lwt
      lwt () = Lwt_unix.connect fd (Unix.ADDR_UNIX path) in
      return (SC_dgram, fd)
    with exn ->
      Lwt_unix.close fd;
      match exn with
        | Unix.Unix_error(Unix.EPROTOTYPE, _, _) ->
            (* Then try with a stream socket: *)
            let fd = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
            begin
              try_lwt
	        lwt () = Lwt_unix.connect fd (Unix.ADDR_UNIX path) in
                return (SC_stream, fd)
              with exn ->
                Lwt_unix.close fd;
                match exn with
                  | Unix.Unix_error(error, _, _) ->
                      log_intern "can not connect to %S: %s" path (Unix.error_message error);
                      fail (Sys_error(Unix.error_message error))
                  | _ ->
                      assert false
            end
        | Unix.Unix_error(error, _, _) ->
            log_intern "can not connect to %S: %s" path (Unix.error_message error);
            fail (Sys_error(Unix.error_message error))
        | _ ->
            assert false
  end

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

let syslog ?(path="/dev/log")
    ?(pid=true) ?(date=true) ?(facility=`User) ?(levels=levels_true) ?(name=program_name) () =
  let sc_fd = ref None in
  let get_sc_fd () = match !sc_fd with
    | Some x ->
        return x
    | None ->
        lwt x = syslog_connect path in
        sc_fd := Some x;
        return x
  in
  let rec result = ref(Opened(End_point{
                                ep_add_lines = begin
                                  fun facility level lines ->
                                    let buf = Buffer.create 42 in
                                    let make_line sc msg =
                                      Buffer.clear buf;
                                      Printf.bprintf buf "<%d>" ((facility_code facility lsl 3) lor level_code level);
                                      add_line buf pid date name msg;
                                      if sc = SC_stream then Buffer.add_char buf '\x00';
                                      Buffer.contents buf
                                    in
                                    let rec print sc fd = function
                                      | [] ->
                                          return ()
                                      | line :: lines ->
                                          try_lwt
                                            lwt () = write_string fd (make_line sc line) in
                                            print sc fd lines
                                          with Unix.Unix_error(_, _, _) ->
                                            (* Try to reconnect *)
                shutdown fd;
                sc_fd := None;
                lwt (sc, fd) = get_sc_fd () in
                lwt () = write_string fd (make_line sc line) in
                print sc fd lines
        in
        lwt (sc, fd) = get_sc_fd () in
        print sc fd lines
    end;
    ep_close = begin fun () ->
      match !sc_fd with
        | None ->
            return ()
        | Some(sc, fd) ->
            shutdown fd;
            return ()
    end;
    ep_mutex = Lwt_mutex.create ();
    ep_levels = array_of_levels levels;
    ep_facility = facility;
    ep_logger = result;
    ep_parents = [];
  })) in
  Lwt_gc.finalise_or_exit close result;
  result

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

let rec log_rec node facility level lines = match node with
  | End_point ep ->
      if ep.ep_levels.(level_code level) then
        Lwt_mutex.with_lock ep.ep_mutex
          (fun () -> ep.ep_add_lines
             (match facility with
                | None -> ep.ep_facility
                | Some facility -> facility)
             level lines)
      else
        return ()
  | Merge merge ->
      Lwt_util.iter (fun node -> log_rec node facility level lines) merge.merge_children

let log ?logger ?facility ~level fmt =
  Printf.ksprintf begin fun message ->
    match get_logger logger with
      | Closed ->
          raise Logger_closed
      | Opened logger ->
          ignore_result (log_rec logger facility level (split message))
  end fmt

let exn ?logger ?facility ?(level=`Error) exn fmt =
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
          ignore_result (log_rec logger facility level (split message))
  end fmt
