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

(** Logging facility *)

(** This module provides functions to deal with logging.
    It support:

    - logging to multiple destination at the same time
    - filtering logs per destination

*)

(** {2 Types} *)

(** Type of log levels. A level determines the importance of a
    message *)
type level =
  | Debug
      (** Debugging message. They can be automatically removed by the
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

type logger
  (** Type of a logger. A logger is responsible for dispatching messages
      and storing them somewhere. *)

type section
  (** Each logging message has a section. Sections can be used to
      structure your logs. For example you can choose different
      loggers according to the section.

      Each section carries a level, and messages with a lower log
      level than than the section level will be dropped.

      Section levels are initialised using the contents of the [LWT_LOG]
      environment variable, which must contain one or more rules of the form
      [pattern -> level] separated by ";". Where [pattern] is a string
      that may contain [*].

      For example, if [LWT_LOG] contains:
      {[
        access -> warning;
        foo[*] -> error
      ]}
      then the level of the section ["access"] is {!Warning} and the
      level of any section matching ["foo[*]"] is {!Error}.

      If the pattern is omited in a rule then the pattern ["*"] is
      used instead, so [LWT_LOG] may just contain ["debug"] for
      instance.

      By default, the following rule apply : ["* -> notice"] *)

val string_of_level : level -> string

val level_of_string : string -> level option

val load_rules : ?fail_on_error:bool -> string -> unit
  (** Reset the rules set when parsing the [LWT_LOG] environment variable using this
      string.

      @param fail_on_error defines if the function will raise Failure if
      it encounters a malformed rule
      @raise Failure if an invalid rule is found and [fail_on_error] is true

      [load_rules] parses the rules string and validates the rules before loading them.
      If [fail_on_error] is [true], invalid rules will cause this function to
      raise [Failure] and leave existing rules unchanged.
      If [fail_on_error] is [false] (this is the default), it tries to load as
      many rules as possible and ignore invalid ones.
      If the rules string itself cannot be parsed, existing rules are always left
      unchanged.

      Example:
      {[
Lwt_log_core.load_rules ~fail_on_error:true "* -> nosuchlevel" (* Raises Failure *)
Lwt_log_core.load_rules "* -> info"
      ]}
   *)

val add_rule : string -> level -> unit
  (** [add_rule pattern level] adds a rule for sections logging
      levels. The rule is added before all other rules. It takes
      effect immediately and affects all sections for which the level
      has not been set explicitly with {!Section.set_level}. [pattern]
      may contains [*]. For example:

      {[
        Lwt_log_core.add_rule "lwt*" Lwt_log_core.Info
      ]}
  *)

val append_rule : string -> level -> unit
  (** [append_rule pattern level] adds the given rule after all other
      rules. For example to set the default fallback rule:

      {[
        Lwt_log_core.append_rule "*" Lwt_log_core.Info
      ]}
  *)

val reset_rules : unit -> unit
  (** removes all rules.
   *)

(** {2 Logging functions} *)

val log : ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> level : level -> string -> unit Lwt.t
  (** [log ?section ?logger ~level message] logs a message.

      [section] defaults to {!Section.main}. If [logger] is not
      specified, then the default one is used instead (see
      {!default}).

      If [exn] is provided, then its string representation
      (= [Printexc.to_string exn]) will be append to the message, and if
      possible the backtrace will also be logged.

      [location] contains the location of the logging directive, it is
      of the form [(file_name, line, column)]. *)

val log_f : ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> level : level -> ('a, unit, string, unit Lwt.t) format4 -> 'a
  (** [log_f] is the same as [log] except that it takes a format
      string *)

val ign_log : ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> level : level -> string -> unit
  (** Same as {!log} but ignore the resulting thread. *)

val ign_log_f : ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> level : level -> ('a, unit, string, unit) format4 -> 'a
  (** Same as {!log_f} but ignore the resulting thread. *)

(** The following functions are the same as {!log} except that their
    name determines which level is used.

    For example {!info}[ msg] is the same as {!log}[ ~level:Info msg].
*)

val debug : ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> string -> unit Lwt.t
val debug_f : ?exn : exn ->  ?section : section -> ?location : (string * int * int) -> ?logger : logger -> ('a, unit, string, unit Lwt.t) format4 -> 'a
val ign_debug : ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> string -> unit
val ign_debug_f : ?exn : exn ->  ?section : section -> ?location : (string * int * int) -> ?logger : logger -> ('a, unit, string, unit) format4 -> 'a

val info : ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> string -> unit Lwt.t
val info_f : ?exn : exn ->  ?section : section -> ?location : (string * int * int) -> ?logger : logger -> ('a, unit, string, unit Lwt.t) format4 -> 'a
val ign_info : ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> string -> unit
val ign_info_f : ?exn : exn ->  ?section : section -> ?location : (string * int * int) -> ?logger : logger -> ('a, unit, string, unit) format4 -> 'a

val notice : ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> string -> unit Lwt.t
val notice_f : ?exn : exn ->  ?section : section -> ?location : (string * int * int) -> ?logger : logger -> ('a, unit, string, unit Lwt.t) format4 -> 'a
val ign_notice : ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> string -> unit
val ign_notice_f : ?exn : exn ->  ?section : section -> ?location : (string * int * int) -> ?logger : logger -> ('a, unit, string, unit) format4 -> 'a

val warning : ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> string -> unit Lwt.t
val warning_f : ?exn : exn ->  ?section : section -> ?location : (string * int * int) -> ?logger : logger -> ('a, unit, string, unit Lwt.t) format4 -> 'a
val ign_warning : ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> string -> unit
val ign_warning_f : ?exn : exn ->  ?section : section -> ?location : (string * int * int) -> ?logger : logger -> ('a, unit, string, unit) format4 -> 'a

val error : ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> string -> unit Lwt.t
val error_f : ?exn : exn ->  ?section : section -> ?location : (string * int * int) -> ?logger : logger -> ('a, unit, string, unit Lwt.t) format4 -> 'a
val ign_error : ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> string -> unit
val ign_error_f : ?exn : exn ->  ?section : section -> ?location : (string * int * int) -> ?logger : logger -> ('a, unit, string, unit) format4 -> 'a

val fatal : ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> string -> unit Lwt.t
val fatal_f : ?exn : exn ->  ?section : section -> ?location : (string * int * int) -> ?logger : logger -> ('a, unit, string, unit Lwt.t) format4 -> 'a
val ign_fatal : ?exn : exn -> ?section : section -> ?location : (string * int * int) -> ?logger : logger -> string -> unit
val ign_fatal_f : ?exn : exn ->  ?section : section -> ?location : (string * int * int) -> ?logger : logger -> ('a, unit, string, unit) format4 -> 'a

(** Sections *)
module Section : sig
  type t = section

  val make : string -> section
    (** [make name] creates a section with the given name. Two calls
        to {!make} with the same name will return the same section
        object. *)

  val name : section -> string
    (** [name section] returns the name of [section]. *)

  val main : section
    (** The main section. It is the section used by default when no
        one is provided. *)

  val level : section -> level
    (** [level section] returns the logging level of [section]. *)

  val set_level : section -> level -> unit
    (** [set_level section] sets the logging level of the given
        section. Modifications of rules using {!add_rule} won't affect
        the level of this section after this operation. *)

  val reset_level : section -> unit
    (** [reset_level section] resets the level of [section] to its
        default value, i.e. to the value obtained by applying
        rules. *)
end

(** {2 Log templates} *)

type template = string
    (** A template is for generating log messages.

        It is a string which may contains variables of the form
        [$(var)], where [var] is one of:
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
        - ["$(name): $(loc-file): $(loc-line): $(loc-column): $(message)"]
    *)

val render : buffer : Buffer.t -> template : template -> section : section -> level : level -> message : string -> unit
  (** [render ~buffer ~template ~section ~level ~message] instantiate
      all variables of [template], and store the result in
      [buffer]. The location is obtained from threads local
      storage. *)

val location_key : (string * int * int) Lwt.key
  (** The key for storing current location. *)

(** {2 Loggers} *)

exception Logger_closed
  (** Exception raised when trying to use a closed logger *)

val make :  output : (section -> level -> string list -> unit Lwt.t) -> close : (unit -> unit Lwt.t) -> logger
  (** [make ~output ~close] creates a new logger.

      @param output is used to write logs. It is a function which
      receive a section, a level and a list lines that must be logged
      together.
      @param close is used to close the logger. *)

val close : logger -> unit Lwt.t
  (** Close the given logger *)

val default : logger ref
  (** The default logger. It is used as default when no one is
      specified. If {!Lwt_core} is linked (in the package [lwt.unix])
      the default logger sends all messages to standard error.
      Otherwise the default logger is {!null}. *)

val broadcast : logger list -> logger
  (** [broadcast loggers] is a logger which send messages to all the
      given loggers.

      Note: closing a broadcast logger does not close its
      components. *)

val dispatch : (section -> level -> logger) -> logger
  (** [dispatch f] is a logger which dispatch logging instructions to
      different logger according to their level and/or section.

      Here is an example:

      {[
        let access_logger = Lwt_log.file "access.log"
        and error_logger = Lwt_log.file "error.log" in

        Lwt_log_core.dispatch
          (fun section level ->
            match Lwt_log_core.Section.name section, level with
              | "access", _ -> access_logger
              | _, Lwt_log_core.Error -> error_logger)
      ]}
  *)

(** {2 Predefined loggers} *)

val null : logger
  (** Logger which drops everything *)
