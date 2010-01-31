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

    Each logger may accept or drop a message according to its logging
    {!level}. If a log message is emited with a logging level greater
    or equal to the {!logger} logging level it is accepted, otherwise
    it is discarded.

    Here is a simple example on how to log messages:

    {[
      (* Create a logger which will display messages on [stderr]: *)
      let logger = Lwt_log.channel ~close_mode:`keep ~channel:Lwt_io.stderr () in

      (* Now log something: *)
      Lwt_log.log ~logger ~level:Lwt_log.Info "my pid is: %d" (Unix.getpid ())
    ]}

    Here is an example on how to compose loggers:

    {[
      (* Logger which send all messages except debugging message to [stderr] *)
      let logger_1 = Lwt_log.channel ~close_mode:`kept ~channel:Lwt_io.stderr ~level:Lwt_log.Info () in

      (* Logger which send all messages to the syslog daemon *)
      let logger_2 = Lwt_log.syslog () in

      (* Combine the two logger into 1: *)
      let logger = Lwt_log.merge [logger_1; logger_2] in

      (* This message will be sent to the syslog daemon and stderr: *)
      Lwt_log.log ~logger ~level:Lwt_log.Info "debug message":

      (* This message will only be sent to the syslog daemon *)
      Lwt_log.log ~logger ~level:Lwt_log.Debug "debug message"
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
      if Lwt_log.Error >= Lwt_log.level () then
        Lwt_log.log ~level:Lwt_log.Error (module_name ^^ format) ...
    ]}

    The advantages of the syntax extension are:
    - it does not compute the arguments if they are not needed
    - it adds automatically the module name to error messages
    - it can removes debugging message at compile-time (for speedup)
*)

(** {6 Log levels} *)

(** This determines the importance of the message. The levels are, in
    order of increasing importance: *)
type level =
  | Debug
      (** Debugging message. They can be automatically removed byt hte
          syntax extension. *)
  | Info
      (** Informational message. Suitable to be displayed when the
          program is in verbose mode. *)
  | Notice
      (** Same as {!Info}, but is displayed by default. *)
  | Warning
      (** Something strange happend *)
  | Error
      (** An error message, which should not means the end of the
          program. *)
  | Fatal
      (** A fatal error happened, in most cases the program will end
          after a fatal error. *)

val default_level : level
  (** The default log level. It is initialised according to the
      environment variable [LWT_LOG], which must be equal to:

      - a number between [0] and [4]:
        - [0] for {!Fatal}
        - [1] for {!Error}
        - [2] for {!Warning}
        - [3] for {!Notice}
        - [4] for {!Info}
        - [5] for {!Debug}
      - a level name (case insensitive): "debug", "info", "notice",
        "warning", "error" or "fatal"

      A level name of "debug" or level number of [5] means that
      everyhting will be displayed. A level name of {!Fatal} or level
      number of [0] means that only fatal error messages are
      displayed.

      If [LWT_LOG] is not defined, then it default to {!Notice}. *)

(** In all the follwing function the optionnal argument [level]
    default to {!default_level} *)

(** {6 Loggers} *)

(** A logger is responsible for receiving messages and send them to
    thier destination. *)

exception Logger_closed
  (** Exception raised when trying to use a closed logger. *)

type logger
  (** Type of loggers *)

val make : ?level : level -> output : (level -> string list -> unit Lwt.t) -> close : (unit -> unit Lwt.t) -> unit -> logger
  (** [make ?level ~output ~close ()] creates a new logger.

      @param output must output a list of lines
      @param close lust close the logger.
  *)

val merge : ?level : level -> logger list -> logger
  (** [merge ?level loggers] creates a new logger which send messages
      to all the given loggers. *)

val close : ?recursive : bool -> logger -> unit Lwt.t
  (** [close ?recursive logger] closes [logger]. If [recursive] is
      [true] (the defaults) then all children of [loggers] are closed
      too. *)

val default : logger ref
  (** The default logger. Initially, it sends messages to the standard
      output for error messages. *)

val level : logger -> level
  (** [level logger] returns the logging level of [logger]. All
      messages with a logging level inferior to this level will be
      discarded. *)

val set_level : logger -> level -> unit
  (** [set_level logger level] sets the logging level of [logger] to
      [level] *)

(** {6 Predefined loggers} *)

type template = string
    (** A template is for generating log messages.

        It may contains varibles of the form [$(var)], where [var] is
        one of:

        - [date] which will be replaced with the current date
        - [name] which will be replaced by the program name
        - [pid] which will be replaced by the pid of the program
        - [message] which will be replaced by the message emited
        - [level] which will be replaced by the level
    *)

val render : buffer : Buffer.t -> template : template -> level : level -> message : string -> unit
  (** Instantiate all variables of the given template. *)

(** Syslog facility. Look at the SYSLOG(3) man page for a description
    of syslog facilities *)
type facility =
    [ `Auth | `Authpriv | `Cron | `Daemon | `FTP | `Kernel
    | `Local0 | `Local1 | `Local2 | `Local3 | `Local4 | `Local5 | `Local6 | `Local7
    | `LPR | `Mail | `News | `Syslog | `User | `UUCP | `NTP | `Security | `Console ]

val syslog : ?level : level -> ?template : template -> ?paths : string list -> facility : facility -> unit -> logger
  (** [syslog ?level ?render ?paths ~facility ()] creates a logger which send
      message to the system logger.

      @param paths is a list of path to try for the syslogd socket. It
             default to [\["/dev/log"; "/var/run/log"\]].
      @param template defaults to ["$(date) $(name)[$(pid)]: $(message)"]
  *)

val file : ?level : level -> ?template : template -> ?mode : [ `truncate | `append ] -> ?perm : Unix.file_perm -> file_name : string -> unit -> logger
  (** [desf_file ?level ?mode ?perm ~file_name ()] creates a logger
      which will write message to [file_name].

      - if [mode = `truncate] then the file is truncated and
      previous contents will be lost.

      - if [mode = `append], new messages will be appended at the
      end of the file

      @param mode defaults to [`append]
      @param template defaults to ["$(date): $(message)"]
  *)

val channel : ?level : level -> ?template : template -> close_mode : [ `close | `keep ] -> channel : Lwt_io.output_channel -> unit -> logger
  (** [channel ?level ~close_mode ~channel ()] creates a destination
      from a channel.

      If [close_mode = `close] then [channel] is closed when the
      logger is closed, otherwise it is left open.

      @param template defaults to ["$(name): $(message)"]
  *)

(** {6 Logging functions} *)

val log : ?logger : logger -> level : level -> ('a, unit, string, unit) format4 -> 'a
  (** [log ?logger ~level format] log a message.

      @param logger defaults to {!default}
      @param level defaults to {!Info}

      The resulting thread is ingored with [ignore_result]. *)

val exn : ?logger : logger -> ?level : level -> exn : exn -> ('a, unit, string, unit) format4 -> 'a
  (** [exn ?logger ?level exn format] logs an exception.

      @param level default to {!Error} *)
