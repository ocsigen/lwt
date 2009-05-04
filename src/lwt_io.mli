(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_io
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

(** Buffered input/output channels *)

(** {6 Types} *)

type 'mode channel
  (** Type of buffered channels *)

type input
  (** Input mode *)

type output
  (** Output mode *)

(** Channel mode *)
type 'a mode =
    private
  | Input
  | Output

val input : input mode
  (** [input] input mode representation *)

val output : output mode
  (** [output] output mode representation *)

(* With GADTs:

   type 'a mode =
     | Input : input mode
     | Output : output mode
*)

type ic = input channel
    (** Type of input channels *)

type oc = output channel
    (** Type of output channels *)

val mode : 'a channel -> 'a mode
  (** [mode ch] returns the mode of a channel *)

(** Note: the three following functions are only needed because of the
    lack of GADTs: *)

external cast : 'a channel -> unit channel = "%identity"
  (** [cast ch] cast a channel to an unknown channel. *)

val cast_ic : 'a channel -> ic
  (** [cast_ic ch] cast a channel to an input channel. Raises
      [Invalid_argument] if [ch] is not an input channel. *)

val cast_oc : 'a channel -> oc
  (** [cast_oc ch] cast a channel to an output channel. Raises
      [Invalid_argument] if [ch] is not an output channel. *)

(** {6 Well-known instances} *)

val stdin : ic
  (** The standard input *)

val stdout : oc
  (** The standard output *)

val stderr : oc
  (** The standard output for error messages *)

(** Note: [stdout] and [stderr] are auto-flushed *)

val zero : ic
  (** Inputs which returns always ['\x00'] *)

val null : oc
  (** Output which drops everything *)

(** {6 Channels creation/manipulation} *)

val get_default_buffer_size : unit -> int
  (** Return the default size for buffers *)

val set_default_buffer_size : int -> unit
  (** Change the default buffer size. Raises [Invalid_argument] if the
      given size is smaller than [16] or greater than
      [Sys.max_string_length] *)

val pipe : ?buffer_size : int -> unit -> ic * oc
  (** [pipe ?buffer_size ()] creates a pipe *)

val make :
  ?auto_flush : bool ->
  ?encoding : Encoding.t ->
  ?buffer_size : int ->
  ?close : (unit -> unit Lwt.t) ->
  ?seek : (int64 -> Unix.seek_command -> int64 Lwt.t) ->
  mode : 'a mode ->
  (string -> int -> int -> int Lwt.t) -> 'a channel
  (** [make ?auto_flush ?buffer_size ?close ~mode perform_io] creates
      a channel.

      @param auto_flush tell wether the channel is
      ``auto-flushed''. Lwt is able to automatically flush the buffer
      before the program goes into idle, which is generally what we
      want. Set it to [false] to disable this behaviour. This
      parameter is not used if [mode = input].

      @param encoding is the channel encoding, for text
      input/output. It defaults to [Encoding.system].

      @param buffer_size is the size of the internal buffer. It is not
      used if [buffer] is provided.

      @param close is the close function of the channel. It defaults
      to [Lwt.return]

      @param seek has the same meaning as [Unix.lseek]

      @param mode is either {!input} or {!output}

      @param perform_io is the read or write function. *)

val encoding : 'a channel -> Encoding.t
  (** [encoding ch] returns the encoding of [ch] *)

val set_encoding : 'a channel -> Encoding.t -> unit
  (** [set_encoding ch enc] change the encoding used by [ch] to [enc] *)

val fallback : oc -> (Text.t -> Text.t option) ref
  (** [fallback oc] is a function used for character that can not be
      encoded in the channel encoding. *)

val default_fallback : Text.t -> Text.t option
  (** The default fallback function. *)

val of_fd : ?buffer_size : int -> mode : 'a mode -> Lwt_unix.file_descr -> 'a channel
  (** [of_fd ~mode ~fd] creates a channel from a file descriptor *)

val of_unix_fd : ?buffer_size : int -> mode : 'a mode -> Unix.file_descr -> 'a channel
  (** [of_unix_fd ~mode ~fd] is a short-hand for:

      [of_fd (Lwt_unix.of_unix_file_descr fd)] *)

val open_file :
  ?buffer_size : int ->
  ?flags : Unix.open_flag list ->
  ?perm : Unix.file_perm ->
  mode : 'a mode ->
  string -> 'a channel
  (** [open_file ?buffer_size ?flags ?perm ~mode filename] open a file *)

val with_file :
  ?buffer_size : int ->
  ?flags : Unix.open_flag list ->
  ?perm : Unix.file_perm ->
  mode : 'a mode ->
  string -> ('a channel -> 'b Lwt.t) -> 'b Lwt.t
  (** [open_file ?buffer_size ?flags ?perm ~mode filename f] open a file and pass
      the channel to [f] *)

val open_connection : ?buffer_size : int -> Unix.sockaddr -> (ic * oc) Lwt.t
  (** [open_connection ?buffer_size ~mode addr] *)

val with_connection : ?buffer_size : int -> Unix.sockaddr -> (ic * oc -> 'a Lwt.t) -> 'a Lwt.t
  (** [open_connection ?buffer_size ~mode addr] *)

val close : 'a channel -> unit Lwt.t
  (** [close ch] closes the given channel: wait for all pending
      operations to terminate, preventing new one to be queued, then
      flush the channel if it is an output channel and close it.

      [close] returns the result of the close function of the
      channel. Multiple calls to [close] will return exactly the same
      value. *)

val abort : 'a channel -> unit Lwt.t
  (** [abort ch] abort current operations and close the channel
      immediatly. *)

val atomic : ('a channel -> 'b Lwt.t) -> ('a channel -> 'b Lwt.t)
  (** [atomic f] transforms a sequence of io operations into one
      single atomic io operation.

      Note:
      - the channel passed to [f] is invalid after [f] terminates
      - [atomic] can be called inside another [atomic]
      - the ``auto-flushing'' is performed only when all operations on
      the main channel are terminated *)

val file_length : string -> int64 Lwt.t
  (** Returns the length of a file *)

(** {6 Random access} *)

val set_pos : 'a channel -> int64 -> unit Lwt.t
  (** [set_pos ch pos] Sets the position in the output channel. This
      does not work if the channel do not support random access. *)

val get_pos : 'a channel -> int64
  (** [get_pos ch] Returns the current position in the channel. *)

val length : 'a channel -> int64 Lwt.t
  (** Returns the length of the channel in bytes *)

(** {6 Text input} *)

val read_char : ic -> Text.t Lwt.t
  (** [read_char ic] reads one unicode character from [ic]. Raises
      [End_of_file] on end of input. *)

val peek_char : ic -> Text.t option Lwt.t
  (** Same as [read_char] but do not raise [End_of_file] on
      end of input. *)

val read_text : ic -> int -> Text.t Lwt.t
  (** [read_text ic int] reads up to [len] characters from [ic]. On
      end-of-file it returns the empty text [""] *)

val read_line : ic -> Text.t Lwt.t
  (** [read_line ic] reads one complete line from [ic] and returns it
      without the end of line. End of line is either ["\n"] or
      ["\r\n"] *)

val peek_line : ic -> Text.t option Lwt.t
  (** Same as [read_line] but do not raise [End_of_file] on
      end of input. *)

(** {6 Text output} *)

val write_char : oc -> Text.t -> unit Lwt.t
  (** [write_char oc ch] outputs [ch] on [oc] *)

val write_text : oc -> Text.t -> unit Lwt.t
  (** [write_text oc txt] outputs [txt] on [oc]. *)

val write_line : oc -> Text.t -> unit Lwt.t
  (** [write_line oc txt] outputs [txt] on [oc] followed by a
      newline. *)

(** {6 Binary input} *)

val get_byte : ic -> char Lwt.t
  (** [get_byte ic] reads one byte from [ic] *)

val peek_byte : ic -> char option Lwt.t
  (** [peek_byte ic] reads one byte and returns [None] if the end of
      input is reached *)

val get_bytes : ic -> int -> string Lwt.t
  (** [get_string ic len] reads at most [len] bytes from [ic]. Returns
      [""] if the end of input is reached. *)

val get : ic -> string -> int -> int -> int Lwt.t
  (** [get ic buffer offset length] reads up to [length] characters,
      stores them in [buffer] at offset [offset], and returns the
      number of characters read.

      Note: [get] do not raises [End_of_file], it returns a length
      of [0] instead. *)

val get_exactly : ic -> string -> int -> int -> unit Lwt.t
  (** [get_exactly ic buffer offset length] reads exactly [length]
      characters and stores them in [buffer] at offset [offset] *)

val get_value : ic -> 'a Lwt.t
  (** [get_value ic] reads a marshaled value from [ic] *)

(** {6 Binary output} *)

val force_flush : oc -> unit Lwt.t
  (** [force_flush oc] performs all pending writes on [oc] *)

val flush : oc -> unit Lwt.t
  (** [flush oc] does nothing if the channel is auto-flushed, and
      executes [force_flush] if not *)

val put_byte : oc -> char -> unit Lwt.t
  (** [put_byte oc ch] writes [ch] to [oc] *)

val put_bytes : oc -> string -> unit Lwt.t
  (** [put_bytes oc str] outputs [str] to [oc] *)

val put : oc -> string -> int -> int -> int Lwt.t
  (** [put oc buffer offset length] writes up to [length] bytes to
      [oc], from [buffer] at offset [offset] *)

val put_exactly : oc -> string -> int -> int -> unit Lwt.t
  (** [put oc buffer offset length] writes all [length] bytes from
      [buffer] at offset [offset] to [oc] *)

val put_value : oc -> ?flags : Marshal.extern_flags list -> 'a -> unit Lwt.t
  (** [put_value oc ?flags x] marshals [x] to [oc] *)

(** {6 Low-level access to the internal buffer} *)

val block : 'a channel  -> int -> (string -> int -> 'b Lwt.t) -> 'b Lwt.t
  (** [block ch size f] pass to [f] the internal buffer and an
      offset. The buffer contains [size] bytes at [offset]. [f] may
      reads or writes these bytes.

      @param size must verify [0 <= size <= 16] *)

val direct_access : 'a channel -> (string -> int -> int -> (int * 'b) Lwt.t) -> 'b Lwt.t
  (** [direct_access ch f] pass to [f] the internal buffer [buf], an
      offset [ofs] and a length [len]. [f] can read/write up to [len]
      bytes from/to [buf] at [ofs] and must returns how many bytes
      have been read/write. *)

(** {6 Input/output of integers} *)

module Byte_order : sig
  module type S = sig

    (** {8 Position of bits in a 16-bits number} *)

    val pos16_0 : int
    val pos16_1 : int

    (** {8 Position of bits in a 32-bits number} *)

    val pos32_0 : int
    val pos32_1 : int
    val pos32_2 : int
    val pos32_3 : int

    (** {8 Position of bits in a 32-bits number} *)

    val pos64_0 : int
    val pos64_1 : int
    val pos64_2 : int
    val pos64_3 : int
    val pos64_4 : int
    val pos64_5 : int
    val pos64_6 : int
    val pos64_7 : int
  end

  module LE : S
    (** Little-endian byte-order *)

  module BE : S
    (** Big-endian byte-order *)
end

(** Reading/writing integers *)
module type Number_io = sig

  (** {8 Reading} *)

  val get_int : ic -> int Lwt.t
    (** Reads a 32-bits integer as an ocaml int *)

  val get_int16 : ic -> int Lwt.t
  val get_int32 : ic -> int32 Lwt.t
  val get_int64 : ic -> int64 Lwt.t

  val get_float32 : ic -> float Lwt.t
    (** Reads an IEEE single precision floating point value *)

  val get_float64 : ic -> float Lwt.t
    (** Reads an IEEE double precision floating point value *)

  (** {8 Writing} *)

  val put_int : oc -> int -> unit Lwt.t
    (** Writes an ocaml int as a 32-bits integer *)

  val put_int16 : oc -> int -> unit Lwt.t
  val put_int32 : oc -> int32 -> unit Lwt.t
  val put_int64 : oc -> int64 -> unit Lwt.t

  val put_float32 : oc -> float -> unit Lwt.t
    (** Writes an IEEE single precision floating point value *)

  val put_float64 : oc -> float -> unit Lwt.t
    (** Writes an IEEE double precision floating point value *)
end

module Make_number_io(Byte_order : Byte_order.S) : Number_io
  (** Create number readers/Writers from the given byte-order *)

module LE : Number_io
  (** Reading/writing of integers in little-endian *)

module BE : Number_io
  (** Reading/writing of integers in big-endian *)
