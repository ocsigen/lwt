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

(** Logging facility *)

(** This module provides functions to deals with logging. Messages
    that are loggen could be sent to the syslog daemon, written to a
    file, or whatever you want.

    You can use this module directly, or via the syntax extension
    [lwt.syntax.log] (recommeded).
*)

(** {6 Types} *)

(** The facility argument is used to specify what type of program is
    logging the message.  This lets the configuration file specify
    that messages from different facilities will be handled
    differently. *)
type facility =
  [ `Auth
      (** security/authorization messages (DEPRECATED Use [`Authpriv] instead) *)
  | `Authpriv
      (** security/authorization messages (private) *)
  | `Cron
      (** clock daemon (cron and at) *)
  | `Daemon
      (** system daemons without separate facility value *)
  | `FTP
      (** ftp daemon *)
  | `Kernel
      (** kernel messages (these can't be generated from user
          processes) *)
  | `Local0
  | `Local1
  | `Local2
  | `Local3
  | `Local4
  | `Local5
  | `Local6
  | `Local7
      (** reserved for local use *)
  | `LPR
      (** line printer subsystem *)
  | `Mail
      (** mail subsystem *)
  | `News
      (** USENET news subsystem *)
  | `Syslog
      (** messages generated internally by syslogd(8) *)
  | `User
      (** (the default) generic user-level messages *)
  | `UUCP
      (** UUCP subsystem *)
  | `NTP
      (** NTP subsystem *)
  | `Security
  | `Console ]

(** This determines the importance of the message. The levels are, in
    order of decreasing importance: *)
type level =
    [ `Emergency
        (** system is unusable *)
    | `Alert
        (** action must be taken immediately *)
    | `Critical
        (** critical conditions *)
    | `Error
        (** error conditions *)
    | `Warning
        (** warning conditions *)
    | `Notice
        (** normal, but significant, condition *)
    | `Info
        (** informational message *)
    | `Debug
        (** debug-level message *) ]

(** {6 Destinations} *)

(** A destination represent the object which receive messages. For
    example, if you write logs to a file, then the file is the
    destinaton. *)

type destination
  (** Type of destinations *)

val dest_syslog : ?pid : bool -> ?date : bool -> ?path : string -> unit -> destination
  (** The syslog daemon. *)

val dest_file : ?pid : bool -> ?date : bool ->
  ?mode : [ `truncate | `append ] -> ?perm : Unix.file_perm -> file_name : string -> destination
  (** [desf_file ?pid ?date ?mode ~file_name] means that the file
      named [file_name] will be used as destination for messages. This
      file can be a regular file, a FIFO, a socket, ...

      - if [mode = `truncate] then the file is truncated and
      previous contents will be lost.

      - if [mode = `append], new messages will be appended at the
      end of the file

      @param mode default to [`append]
      @param pid tell whether the pid should be sent with each
             messages. It defaultsto [true]
      @param date indicate whether the current date should be sent with
             each message. It defaults to [true].
  *)

val dest_channel : ?pid : bool -> ?date : bool ->
  close_mode : [ `close | `keep ] -> channel : Lwt_io.output_channel ->  destination
  (** [dest_channel ?pid ?date ~close_mode channel] creates a
      destination from a channel.

      If [close_mode = `close] then [channel] is closed when the logger is
      closed, otherwise it is kept open *)

val dest_stdout : destination
  (** The standart output. [pid] and [date] are set to [false] *)

val dest_stderr : destination
  (** The standart output for error messages. [pid] and [date] are set
      to [false] *)

(** {6 Loggers} *)

exception Logger_closed
  (** Exception raised when trying to use a closed logger. *)

type logger
  (** Type of a logger. A logger is an object which accept messages
      and print them on one or more destinations.

      Note: loggers are automatically closed when garbage collected or
      at the end of the program *)

val create : ?facility : facility -> ?name : string -> destination list -> logger Lwt.t
  (** [create ?facility ?name ?pid destinations] creates a new
      logger, sending messages to all destinations at the same time.

      @param facility is the default facility. It can be overwrited
             by the optionnal argument [facility] of {!log} and {!logf}.
             value for message which does not provide one. It defaults
             to [`User].

      @param name is the name of the program
      @param send_pid tell whether the pid should be sent for all
             messages. It default to [true].
      @param destination is a list of destination for messages.
  *)

val close : logger -> unit Lwt.t
  (** [close logger] closes [logger] and assiciated resources *)

val default : logger ref
  (** The default logger.

      It's initial value is [create [dest_stderr]]. *)

val level : ?logger : logger -> level -> bool
  (** [level ?logger level] returns whether the given level is
      active or not *)

val set_level : ?logger : logger -> level -> bool -> unit
  (** [set_level logger level value] sets the level state *)

(** {6 Logging functions} *)

val log : ?logger : logger -> ?facility : facility -> level : level -> string -> unit
  (** [log ?facility ?level msg] log a message.

      @param logger defaults to {!default}
      @param facility defaults to the logger defaults facility
      @param level defaults to [`Info]

      The resulting thread is ingored with [ignore_result]. *)

val logf : ?logger : logger -> ?facility : facility -> level : level -> ('a, unit, string, unit) format4 -> 'a
  (** Same as [log] but prints according to the given format *)

(**/**)

val __unsafe_level : logger -> int -> bool
  (* For the syntax extension, do not use directly! *)
