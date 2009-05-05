(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_process
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

(** Process management *)

type command = string * string array
    (** A command is a program name with a list of arguments *)

val sh : string -> command
  (** A command executed with ["/bin/sh"] *)

(** {6 Simple commands} *)

(** These functions are similar to the ones of the python [commands]
    module: *)

val get_output : command -> string Lwt.t
  (** [get_output command] executes [command] and returns its outputs *)

val get_status : command -> Unix.process_status Lwt.t
  (** [get_status command] executes [command] and returns its exit
      status *)

val get_status_output : command -> (Unix.process_status * string) Lwt.t
  (** [get_status_output command] executes [command] and returns its
      exits status and outputs *)

val map : command -> Text.t -> Text.t Lwt.t
  (** [map command txt] launch [command], feed it with [txt], and
      returns its outputs. *)

(** {6 Piped-commands} *)

(** Common processes operations *)
class type process_common = object
  method pid : int
    (** Pid of the sub-process *)

  method close : Unix.process_status Lwt.t
    (** Closes the process and returns its exit status. This close all
        channels used to communicate with the process *)
end

class process_in : ?env : string array -> command -> object
  inherit process_common

  method stdout : Lwt_io.ic
    (** The standard output of the process *)
end

val process_in : ?env : string array -> command -> process_in
val with_process_in : ?env : string array -> command -> (process_in -> 'a Lwt.t) -> 'a Lwt.t

class process_out : ?env : string array -> command -> object
  inherit process_common

  method stdin : Lwt_io.oc
    (** The standard input of the process *)
end

val process_out : ?env : string array -> command -> process_out
val with_process_out : ?env : string array -> command -> (process_out -> 'a Lwt.t) -> 'a Lwt.t

class process : ?env : string array -> command -> object
  inherit process_common

  method stdin : Lwt_io.oc
  method stdout : Lwt_io.ic
end

val process : ?env : string array -> command -> process
val with_process : ?env : string array -> command -> (process -> 'a Lwt.t) -> 'a Lwt.t

class process_full : ?env : string array -> command -> object
  inherit process_common

  method stdin : Lwt_io.oc
  method stdout : Lwt_io.ic
  method stderr : Lwt_io.ic
end

val process_full : ?env : string array -> command -> process_full
val with_process_full : ?env : string array -> command -> (process_full -> 'a Lwt.t) -> 'a Lwt.t
