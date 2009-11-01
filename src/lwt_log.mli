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

(** An interface to the system logger *)

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

type logger
  (** Type of a logger *)

val create :
  ?path : string ->
  ?facility : facility ->
  ?copy_to : Lwt_io.output_channel ->
  ?console : bool ->
  ?pid : bool ->
  ?no_delay : bool ->
  ?name : string ->
  unit -> logger Lwt.t
  (** [create ?logpath ?facility ?console ?pid program_name ()]
      creates a new logger.

      @parap path is the path of the syslog socket
      @param facility is the default value for message which does not
             provides one.
      @param copy_to is a channel which will receive messages. A
             typical example is: [~copy_to:Lwt_io.stderr]
      @param console tell whether [Lwt_log] errors should be reported
             on [stderr]
      @param pid tell whether the PID will be included in each
             message. It defaults to [true]
      @param no_delay tell whether the connection is opened
             immediatly. The default is to open it on the first connection
      @param name is the name of the program
  *)

val close : logger -> unit Lwt.t
  (** [close logger] closes the connection with the system logger. *)

(** {6 Global logging} *)

val global : logger ref
  (** The global logger, used by default for function which do not
      provides one. *)

val log : ?logger : logger -> ?facility : facility -> level : level -> string -> unit
  (** [log ?facility ?level msg] sends a message to the system
      logger

      @param logger defaults to {!global}
      @param facility defaults to the logger defaults facility
      @param level defaults to [`Info]

      The resulting thread is ingored with [ignore_result]. *)

val log_multi : ?logger : logger -> ?facility : facility -> level : level -> string list -> unit
  (** [log ?facility ?level msg] sends a list of lines to the system
      logger *)

val logf : ?logger : logger -> ?facility : facility -> level : level -> ('a, unit, string, unit) format4 -> 'a
  (** Same as [log] but prints according to the givel format *)

val log_t : ?logger : logger -> ?facility : facility -> level : level -> string -> unit Lwt.t
  (** [log_t ?logger ?facility ~level msg] is a the same as [log] but
      the resulting thread is not ignored *)

val log_multi_t : ?logger : logger -> ?facility : facility -> level : level -> string list -> unit Lwt.t
  (** Idem *)

val logf_t : ?logger : logger -> ?facility : facility -> level : level -> ('a, unit, string, unit Lwt.t) format4 -> 'a
  (** Idem *)
