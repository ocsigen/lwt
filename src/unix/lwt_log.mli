(* Lightweight thread library for OCaml
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

(** This module provides functions to deal with logging.
    It extends Lwt_log_core with Unix features.
    It adds:

    - logging to the syslog daemon
    - logging to a channel (stderr, stdout, ...)
    - logging to a file
*)

include module type of Lwt_log_core
  with type level = Lwt_log_core.level
   and type logger = Lwt_log_core.logger
   and type section = Lwt_log_core.section
   and type template = Lwt_log_core.template
   and module Section = Lwt_log_core.Section

(** {6 Types} *)

(** {8 logger} *)
  (** See {!Lwt_log_core.logger}.

      Lwt provides loggers sending log messages to a file, syslog,
      ... but you can also create you own logger.*)

(** {8 section} *)
  (** See {!Lwt_log_core.section}.

      Each logging message has a section. Sections can be used to
      structure your logs. For example you can choose different
      loggers according to the section.

      Each section carries a level, and messages with a lower log
      level than than the section level will be dropped.

      Section levels are initialised using [Lwt_log_core.load_rules] with
      the content of [LWT_LOG] environment
      variable if set.

      If [LWT_LOG] is not defined then the rule ["* -> notice"] is
      used instead. *)

(** {6 Log templates} *)

(** {8 template} *)
    (** See {!Lwt_log_core.template}.

        A template is for generating log messages.

        It is a string which may contains variables of the form
        [$(var)], where [var] is one of:

        - [date] which will be replaced with the current date
        - [milliseconds] which will be replaced by the fractionnal part
          of the current unix time
        - [name] which will be replaced by the program name
        - [pid] which will be replaced by the pid of the program
        - [message] which will be replaced by the message emited
        - [level] which will be replaced by a string representation of
          the level
        - [section] which will be replaced by the name of the
          message's section
        - [loc-file] which will be replaced by the file name of the
          calling logging function
        - [loc-line] which will be replaced by the line number of the
          calling logging function
        - [loc-column] which will be replaced by the column number of
           the calling logging function

        For example:
        - ["$(name): $(message)"]
        - ["$(date) $(name)[$(pid)]: $(message)"]
        - ["$(date).$(milliseconds) $(name)[$(pid)]: $(message)"]
        - ["$(date): $(loc-file): $(loc-line): $(loc-column): $(message)"]
    *)

(** {6 Predefined loggers} *)

(** Syslog facility. Look at the SYSLOG(3) man page for a description
    of syslog facilities *)
type syslog_facility =
    [ `Auth | `Authpriv | `Cron | `Daemon | `FTP | `Kernel
    | `Local0 | `Local1 | `Local2 | `Local3 | `Local4 | `Local5 | `Local6 | `Local7
    | `LPR | `Mail | `News | `Syslog | `User | `UUCP | `NTP | `Security | `Console ]

val syslog : ?template : template -> ?paths : string list -> facility : syslog_facility -> unit -> logger
  (** [syslog ?template ?paths ~facility ()] creates an logger
      which send message to the system logger.

      @param paths is a list of path to try for the syslogd socket. It
             default to [\["/dev/log"; "/var/run/log"\]].
      @param template defaults to ["$(date) $(name)[$(pid)]: $(section): $(message)"]
  *)

val file : ?template : template -> ?mode : [ `Truncate | `Append ] -> ?perm : Unix.file_perm -> file_name : string -> unit -> logger Lwt.t
  (** [desf_file ?template ?mode ?perm ~file_name ()] creates an
      logger which will write messages to [file_name].

      - if [mode = `Truncate] then the file is truncated and previous
      contents will be lost.

      - if [mode = `Append], new messages will be appended at the end
      of the file

      @param mode defaults to [`Append]
      @param template defaults to ["$(date): $(section): $(message)"]
  *)

val channel :?template : template -> close_mode : [ `Close | `Keep ] -> channel : Lwt_io.output_channel -> unit -> logger
  (** [channel ?template ~close_mode ~channel ()] creates a logger
      from a channel.

      If [close_mode = `Close] then [channel] is closed when the
      logger is closed, otherwise it is left open.

      @param template defaults to ["$(name): $(section): $(message)"] *)
