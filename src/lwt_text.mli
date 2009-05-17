(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Interface Lwt_text
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

(** Text channels *)

(** This modules implements {b text channel}s. A {b text channel} is
    basically a {b byte channel} (as in {!Lwt_io}) plus a {b character
    encoding}. *)

(** {6 Types} *)

type 'mode channel
  (** Type of a text channel *)

type input_channel = Lwt_io.input channel
    (** Type of a text input channel *)

type output_channel = Lwt_io.output channel
    (** Type of a text output channel *)

(** {6 Standard channels} *)

val stdin : input_channel
  (** The standard input, it reads data from {!Lwt_io.stdin} *)

val stdout : output_channel
  (** The standard output, it writes data to {!Lwt_io.stdout} *)

val stderr : output_channel
  (** The standard output for error messages, it writes data to
      {!Lwt_io.stderr} *)

(** Note: [stdout] and [stderr] are auto-flushed *)

val zero : input_channel
  (** Inputs which returns always ['\x00']. It reads data from
      {!Lwt_io.zero} *)

val null : output_channel
  (** Output which drops everything. It writes data to
      {!Lwt_io.null} *)

(** {6 Creation/manipulation} *)

val make : ?strict : bool -> ?encoding : Encoding.t -> 'a Lwt_io.channel -> 'a channel
  (** [make ?strict ?encoding ch] creates a text channel from a byte
      channel.

      @param strict tell whether encoding/decoding must be ``strict'',
      which whether the encoder/decoder should fail on invalid
      sequence. In non-strict mode, it transparently fallback to
      ISO-8859-15. By the way it is ensured that [read*] functions
      always returns valid UTF-8 encoded text. [strict] defaults to
      [false].

      @param encoding is the character encoding used for the
      channel. It defaults to [Encoding.system]. *)

val byte_channel : 'a channel -> 'a Lwt_io.channel
  (** [byte_channel ch] returns the underlying byte channel of a text
      channel *)

val encoding : 'a channel -> Encoding.t
  (** [encoding ch] returns the character encoding of a channel. *)

val open_file :
  ?buffer_size : int ->
  ?strict : bool ->
  ?encoding : Encoding.t ->
  ?flags : Unix.open_flag list ->
  ?perm : Unix.file_perm ->
  mode : 'a Lwt_io.mode ->
  string -> 'a channel

val with_file :
  ?buffer_size : int ->
  ?strict : bool ->
  ?encoding : Encoding.t ->
  ?flags : Unix.open_flag list ->
  ?perm : Unix.file_perm ->
  mode : 'a Lwt_io.mode ->
  string -> ('a channel -> 'b Lwt.t) -> 'b Lwt.t

val close : 'a channel -> unit Lwt.t
  (** [close ch = Lwt_io.close (byte_channel ch)] *)

val flush : output_channel -> unit Lwt.t
  (** [flush ch = Lwt_io.flush (byte_channel ch)] *)

val atomic : ('a channel -> 'b Lwt.t) -> ('a channel -> 'b Lwt.t)
  (** [atomic f] transforms a sequence of io operations into one
      single atomic io operation. *)

(** {6 Reading} *)

val read_char : input_channel -> Text.t Lwt.t
  (** [read_char ic] reads the next character of [ic].

      @raise End_of_file if the end of the file is reached *)

val read_char_opt : input_channel -> Text.t option Lwt.t
  (** Same as {!read_char} but do not raises [End_of_file] on end of
      input *)

val read_chars : input_channel -> Text.t Lwt_stream.t
  (** [read_chars ic] returns a stream holding all characters of [ic] *)

val read : ?count : int -> input_channel -> Text.t Lwt.t
  (** [read ?count ic] reads at most [len] characters from [ic]. It
      returns [""] if the end of input is reached. If [count] is not
      specified, it reads all characters until the end of input. *)

val read_line : input_channel -> Text.t Lwt.t
  (** [read_line ic] reads one complete line from [ic] and returns it
      without the end of line. End of line is either ["\n"] or
      ["\r\n"].

      If the end of line is reached before reading any character,
      [End_of_file] is reached. If it is reached before reading an end
      of line but characters have already been read, they are
      returned. *)

val read_line_opt : input_channel -> Text.t option Lwt.t
  (** Same as [read_line] but do not raise [End_of_file] on end of
      input. *)

val read_lines : input_channel -> Text.t Lwt_stream.t
  (** [read_lines ic] returns a stream holding all lines of [ic] *)

(** {6 Writing} *)

val write_char : output_channel -> Text.t -> unit Lwt.t
  (** [write_char oc char] writes [char] on [oc] *)

val write : output_channel -> Text.t -> unit Lwt.t
  (** [write oc text] writes all characters of [text] on [oc] *)

val write_chars : output_channel -> Text.t Lwt_stream.t -> unit Lwt.t
  (** [write_bytes oc chars] writes all characters contained hold by
      [chars] on [oc] *)

val write_line : output_channel -> Text.t -> unit Lwt.t
  (** [write_line oc txt] writes [txt] on [oc] followed by a
      newline. *)

val write_lines : output_channel -> Text.t Lwt_stream.t -> unit Lwt.t
  (** [write_lines oc lines] writes all lines of [lines] on [oc],
      separated by newline characters. *)
