(* Lightweight thread library for Objective Caml
 * http://www.output_channelsigen.org/lwt
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

(** This modules allow you to spawn processes and communicate with them. *)

type command = string * string array
    (** A command is a program name with a list of arguments *)

val shell : string -> command
  (** A command executed with ["/bin/sh"] *)

(** {6 High-level functions} *)

val exec : ?env : string array -> command -> Unix.process_status Lwt.t
  (** [exec command] execute [command] and returns its exit status. *)

(** {8 Receiving} *)

val pread : ?env : string array -> command -> string Lwt.t
val pread_chars : ?env : string array -> command -> char Lwt_stream.t
val pread_line : ?env : string array -> command -> string Lwt.t
val pread_lines : ?env : string array -> command -> string Lwt_stream.t

(** {8 Sending} *)

val pwrite : ?env : string array -> command -> string -> unit Lwt.t
val pwrite_chars : ?env : string array -> command -> char Lwt_stream.t -> unit Lwt.t
val pwrite_line : ?env : string array -> command -> string -> unit Lwt.t
val pwrite_lines : ?env : string array -> command -> string Lwt_stream.t -> unit Lwt.t

(** {8 Mapping} *)

val pmap : ?env : string array -> command -> string -> string Lwt.t
val pmap_chars : ?env : string array -> command -> char Lwt_stream.t -> char Lwt_stream.t
val pmap_line : ?env : string array -> command -> string -> string Lwt.t
val pmap_lines : ?env : string array -> command -> string Lwt_stream.t -> string Lwt_stream.t

(** {6 Spawning processes} *)

class process_none : ?env : string array -> command -> object
  method pid : int
    (** Pid of the sub-process *)

  method close : Unix.process_status Lwt.t
    (** Closes the process and returns its exit status. This close all
        channels used to communicate with the process *)
end

val open_process_none : ?env : string array -> command -> process_none
val with_process_none : ?env : string array -> command -> (process_none -> 'a Lwt.t) -> 'a Lwt.t

class process_in : ?env : string array -> command -> object
  inherit process_none

  method stdout : Lwt_io.input_channel
    (** The standard output of the process *)
end

val open_process_in : ?env : string array -> command -> process_in
val with_process_in : ?env : string array -> command -> (process_in -> 'a Lwt.t) -> 'a Lwt.t

class process_out : ?env : string array -> command -> object
  inherit process_none

  method stdin : Lwt_io.output_channel
    (** The standard input of the process *)
end

val open_process_out : ?env : string array -> command -> process_out
val with_process_out : ?env : string array -> command -> (process_out -> 'a Lwt.t) -> 'a Lwt.t

class process : ?env : string array -> command -> object
  inherit process_none

  method stdin : Lwt_io.output_channel
  method stdout : Lwt_io.input_channel
end

val open_process : ?env : string array -> command -> process
val with_process : ?env : string array -> command -> (process -> 'a Lwt.t) -> 'a Lwt.t

class process_full : ?env : string array -> command -> object
  inherit process_none

  method stdin : Lwt_io.output_channel
  method stdout : Lwt_io.input_channel
  method stderr : Lwt_io.input_channel
end

val open_process_full : ?env : string array -> command -> process_full
val with_process_full : ?env : string array -> command -> (process_full -> 'a Lwt.t) -> 'a Lwt.t
