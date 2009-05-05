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

val shell : string -> command
  (** A command executed with ["/bin/sh"] *)

(** {6 High-level functions} *)

val exec : ?env : string array -> command -> Unix.process_status Lwt.t
  (** [exec command] execute [command] and returns its exit status. *)

(** {8 Receiving} *)

val recv_bytes : ?env : string array -> command -> string Lwt.t
  (** [recv_bytes ?env command] runs [command] and returns its
      output *)

val recv_text : ?env : string array -> command -> Text.t Lwt.t
  (** [recv_text ?env command] runs [command] and returns its
      output *)

val recv_line : ?env : string array -> command -> Text.t Lwt.t
  (** [recv_line ?env command] runs [command] and returns its first
      output line *)

val recv_lines : ?env : string array -> command -> Text.t Lwt_stream.t
  (** [recv_lines ?env command] runs [command] and returns its output
      as a stream of lines *)

(** {8 Sending} *)

val send_bytes : ?env : string array -> command -> string -> Unix.process_status Lwt.t
  (** [send_bytes ?env command bytes] runs [command] and send it
      [bytes] *)

val send_text : ?env : string array -> command -> Text.t -> Unix.process_status Lwt.t
  (** [send_text ?env command text] runs [command] and send it
      [text] *)

val send_line : ?env : string array -> command -> Text.t -> Unix.process_status Lwt.t
  (** [send_line ?env command line] runs [command] and send it
      [line] *)

val send_lines : ?env : string array -> ?sep : Text.t -> command -> Text.t Lwt_stream.t -> Unix.process_status Lwt.t
  (** [send_lines ?env command lines] runs [command] and send it all
      lines of [lines] *)

(** {8 Mapping} *)

val map_bytes : ?env : string array -> command -> string -> string Lwt.t
  (** [map_bytes ?env command bytes] runs [command], send it [bytes]
      and returns its output *)

val map_text : ?env : string array -> command -> Text.t -> Text.t Lwt.t
  (** [map_text ?env command txt] runs [command], send it [txt] and
      reads its output. *)

val map_line : ?env : string array -> command -> Text.t -> Text.t Lwt.t
  (** Same as [map_text] but use {!Lwt_io.write_line} and
      {!Lwt_io.read_line} *)

val map_lines : ?env : string array -> ?sep : Text.t -> command -> Text.t Lwt_stream.t -> Text.t Lwt_stream.t
  (** [map_lines ?env command ?sep lines] runs [command] send it all
      lines of [lines], and returns its outputs as a stream of
      lines *)

(** {6 Spawning processes} *)

class process_none : ?env : string array -> command -> object
  method pid : int
    (** Pid of the sub-process *)

  method close : Unix.process_status Lwt.t
    (** Closes the process and returns its exit status. This close all
        channels used to communicate with the process *)
end

val process_none : ?env : string array -> command -> process_none
val with_process_none : ?env : string array -> command -> (process_none -> 'a Lwt.t) -> 'a Lwt.t

class process_in : ?env : string array -> command -> object
  inherit process_none

  method stdout : Lwt_io.ic
    (** The standard output of the process *)
end

val process_in : ?env : string array -> command -> process_in
val with_process_in : ?env : string array -> command -> (process_in -> 'a Lwt.t) -> 'a Lwt.t

class process_out : ?env : string array -> command -> object
  inherit process_none

  method stdin : Lwt_io.oc
    (** The standard input of the process *)
end

val process_out : ?env : string array -> command -> process_out
val with_process_out : ?env : string array -> command -> (process_out -> 'a Lwt.t) -> 'a Lwt.t

class process : ?env : string array -> command -> object
  inherit process_none

  method stdin : Lwt_io.oc
  method stdout : Lwt_io.ic
end

val process : ?env : string array -> command -> process
val with_process : ?env : string array -> command -> (process -> 'a Lwt.t) -> 'a Lwt.t

class process_full : ?env : string array -> command -> object
  inherit process_none

  method stdin : Lwt_io.oc
  method stdout : Lwt_io.ic
  method stderr : Lwt_io.ic
end

val process_full : ?env : string array -> command -> process_full
val with_process_full : ?env : string array -> command -> (process_full -> 'a Lwt.t) -> 'a Lwt.t
