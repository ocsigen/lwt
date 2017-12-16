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

(** Logging functions

    This module provides logging functions. It is actually a thin extension of
    {!Lwt_log_core}, adding logging destinations that are only available on Unix
    and Windows (such as files, standard output, and the system log). Being a
    thin extension, most of the functions in this module are actually the ones
    in {!Lwt_log_core}. They are not repeated on this page, however, so please
    read both {!Lwt_log_core} {e and} this page for a complete understanding.

    Here is a simple, self-contained usage example:

{[
let () =
  Lwt_log.default :=
    Lwt_log.channel
      ~template:"$(date).$(milliseconds) [$(level)] $(message)"
      ~close_mode:`Keep
      ~channel:Lwt_io.stdout
      ();

  Lwt_log.add_rule "*" Lwt_log.Info;

  Lwt_main.run begin
    Lwt_log.info "Hello world!"
  end

(* ocamlfind opt -linkpkg -package lwt.unix log_example.ml && ./a.out *)
]}

    As an alternative to this module, we suggest trying
    {{: http://erratique.ch/software/logs/doc/Logs_lwt.html}[Logs_lwt]} from the
    {{: http://erratique.ch/software/logs} [logs]} library. *)

include module type of Lwt_log_core
  with type level = Lwt_log_core.level
   and type logger = Lwt_log_core.logger
   and type section = Lwt_log_core.section
   and type template = Lwt_log_core.template
   and module Section = Lwt_log_core.Section

val render : buffer : Buffer.t -> template : template -> section : section -> level : level -> message : string -> unit
  (** Same as {!Lwt_log_core.render}, except that the template may also contain the
      following variables:

      - [date], which will be replaced with the current local date and time,
      - [milliseconds], which will be replaced by the fractional part of the current Unix
        time, to millisecond accuracy.

      For example:
        - ["$(date) $(name)[$(pid)]: $(message)"]
        - ["$(date).$(milliseconds) $(name)[$(pid)]: $(message)"]
        - ["$(date): $(loc-file): $(loc-line): $(loc-column): $(message)"]
  *)

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
             default to [["/dev/log"; "/var/run/log"]].
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
