(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Interface Lwt_log
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

(** Logging facility *)

(** This module provides functions to deal with logging in your
    program. It support:

    - logging to the syslog daemon
    - logging to a channel (stderr, stdout, ...)
    - logging to a file
    - logging to multiple destination at the same time

    Each logger may accept or drop a message according to its
    logging {!level}.

    Here is a simple example on how to log messages:

    {[
      (* Create a logger which will display messages on [stderr]: *)
      lwt logger = Lwt_log.channel ~close_mode:`keet ~channel:Lwt_io.stderr () in

      (* Now log something: *)
      Lwt_log.log ~logger ~level:`Info "my pid is: %d" (Unix.getpid ())
    ]}

    You can also compose loggers:

    {[
      (* Create a logger which will send everything to the syslog daemon,
         and all non-debug message to [stderr]: *)
      lwt logger_1 = Lwt_log.channel ~close_mode:`kept ~channel:Lwt_io.stderr
                       ~levels:{ Lwt_log.levels_true with Lwt_log.debug = false } ()
      and logger_2 = Lwt_log.syslog () in

      (* Combine the two logger into 1: *)
      let logger = merge [logger_1; logger_2] in

      (* This message will be sent to the syslog daemon and stderr: *)
      Lwt_log.log ~logger ~level:`Info "debug message":

      (* This message will only be sent to the syslog daemon *)
      Lwt_log.log ~logger ~level:`Debug "debug message"
    ]}

    The [logger] argument is not mandatory, if you omit it, the
    {!default} logger will be used instead.

    Moreover, there is the syntax extension [lwt.syntax.log] which
    allow more convenient use of loggers.

    It allow the following constructions:

    {[
      Log#error format ...
    ]}

    which is translated to:

    {[
      if Lwt_log.__unsafe_level 3 then
        Lwt_log.__unsafe_log ~level:3 (module_name ^^ format) ...
    ]}

    The advantage of the syntax extension are:
    - it does not compute the arguments if they are not needed
    - it adds automatically the module name to error messages
    - it can removes debugging message at compile-time (for speedup)
*)

(** {6 Types} *)

(** The facility argument is used to specify what type of program is
    logging the message. This lets the configuration file specify that
    messages from different facilities will be handled differently. *)
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

val levels_true : levels
  (** All fields are [true] *)

val levels_false : levels
  (** All fields are [false] *)

(** {6 Loggers} *)

(** A logger is responsible for receiving messages and send them to
    thier destination *)

exception Logger_closed
  (** Exception raised when trying to use a closed logger. *)

type logger
  (** Type of loggers *)

val merge : logger list -> logger
  (** [merge loggers] creates a new logger which send messages to all
      the given loggers. *)

val close : ?recursive : bool -> logger -> unit Lwt.t
  (** [close ?recursive logger] closes [logger]. If [recursive] is
      [true] (the defaults) then all children of [loggers] are closed
      too. *)

val default : logger ref
  (** The default logger.

      Initially it logs everything except informative and debugging
      messages on [stderr], without the pid and the date. *)

val level : ?logger : logger -> level -> bool
  (** [level ?logger level] returns [true] iff messages with level
      [level] are enabled *)

val set_level : ?logger : logger -> level -> bool -> unit
  (** [set_level ?logger level value] set the enable state of the
      given logger for the level [level] *)

type logger_maker = ?pid : bool -> ?date : bool -> ?facility : facility ->
  ?levels : levels -> ?name : string -> unit -> logger
  (** Type of a function which create a logger.

      @param pid tell whether the pid should be sent with each
             messages. It defaultsto [true],
      @param date indicatewhether the current date should be sent with
             each message. It defaults to [true],
      @param facility is the default facility for message which does
             not specify one
      @param levels give the set of enabled/disabled levels. It
             defaults to {!levels_true}.
      @param name defaults to the program name
  *)

val syslog : ?path : string -> logger_maker
  (** The syslog daemon.

      @param path is the path of the socket on which syslogd is
      listenning. *)

val file : ?mode : [ `truncate | `append ] -> ?perm : Unix.file_perm -> file_name : string -> logger_maker
  (** [desf_file ?mode ~file_name] creates a logger which will write
      message to [file_name].

      - if [mode = `truncate] then the file is truncated and
        previous contents will be lost.

      - if [mode = `append], new messages will be appended at the
        end of the file

      @param mode defaults to [`append] *)

val channel : close_mode : [ `close | `keep ] -> channel : Lwt_io.output_channel -> logger_maker
  (** [channel ~close_mode ~channel] creates a destination
      from a channel.

      If [close_mode = `close] then [channel] is closed when the
      logger is closed, otherwise it is left open *)

(** {6 Logging functions} *)

val log : ?logger : logger -> ?facility : facility -> level : level -> ('a, unit, string, unit) format4 -> 'a
  (** [log ?logger ?facility ~level format] log a message.

      @param logger defaults to {!default}
      @param facility defaults to the logger's default facility
      @param level defaults to [`Info]

      The resulting thread is ingored with [ignore_result]. *)

val exn : ?logger : logger -> ?facility : facility -> ?level : level -> exn -> ('a, unit, string, unit) format4 -> 'a
  (** [exn ?logger ?facility ?level exn fmt@ logs an exception. 

      @param level default to [`Error] *)

(**/**)

(* For the syntax extension, do not use directly! *)
val unsafe_level : logger -> int -> bool
