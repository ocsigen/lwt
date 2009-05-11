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

(** Input/output channels *)

(** {10 Definition}

    A {b channel} is a high-level object for performing IOs. It allow
    to read and write things from the outside worlds in an efficient
    way, by minimising the number of system calls.

    An {b output channel} is a channel that can be used to send data
    and an {b input channel} is a channel that can used to receive
    data.

    A channels also know about {b character encodings}. This means
    that it know how to encode it when sending it to the outside world
    and how to decode it when receiving it from the outside world. By
    default the encoding of the system is used. For example, you can
    try this in the toplevel:

    {[
      # Encoding.system;;
      \- : Encoding.t = "UTF-8"
      # Lwt_io.encoding stdout;;
      \- : Encoding.t = "UTF-8"
    ]}

    It is always possible to use a channel for text IOs, or for binary
    IOs. You can mix the two but you must know what you are doing.

    Here is an example of text output:

    {[
      # let oc = open_file ~mode:output "some-file";;
      \- : Lwt_io.oc = <abstr>
      # run& write_line oc "Hello, world!";;
      \- : unit = ()
      # run& close oc;;
      \- : unit = ()
    ]}

    And text output:

    {[
      # let oc = open_file ~mode:output "file.data";;
      \- : Lwt_io.oc = <abstr>
      # run& Lwt_io.put_bytes oc buffer;;
      \- : unit = ()
      # run& close oc;;
      \- : unit = ()
    ]}

    {10 Naming convention}

    Function names follow some rules, which are also used in other
    modules of Lwt such as {!Lwt_process}.

    These rules are:

    - functions for reading text starts with {b read}
    - functions for writing text starts with {b write}
    - functions for binary input starts with {b get}
    - functions for binary output starts with {b put}
    - functions returning/taking a stream use the plural form

    For example, {!read_char} is for reading a character, {!get_byte}
    is for reading a byte, {!write_lines} is for writing a stream of
    lines to an output.

    For function returning a stream, elements are read as there are
    requested. *)

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

val pipe : ?buffer_size : int -> unit -> ic * oc
  (** [pipe ?buffer_size ()] creates a pipe.

      All data written to the writing side of the pipe can be read on
      the reading side. *)

val make :
  ?auto_flush : bool ->
  ?encoding : Encoding.t ->
  ?buffer_size : int ->
  ?close : (unit -> unit Lwt.t) ->
  ?seek : (int64 -> Unix.seek_command -> int64 Lwt.t) ->
  mode : 'a mode ->
  (string -> int -> int -> int Lwt.t) -> 'a channel
  (** [make ?auto_flush ?buffer_size ?close ~mode perform_io] is the
      main function for creating new channels.

      @param auto_flush tell wether the channel is
      ``auto-flushed''. Lwt is able to automatically flush the buffer
      before the program goes into idle, which is generally what we
      want. Set it to [false] to disable this behaviour. This
      parameter is not used if [mode = input].

      @param encoding is the channel encoding, for text
      input/output. It defaults to [Encoding.system ^ "//TRANSLIT"].
      The ["//TRANSLIT"] suffix means that characters that cannot be
      encoded are approximated. You can also use the ["//IGNORE"]
      suffix which means that these characters are discarded. Note
      that the suffix is not returned by {!encoding}.

      @param buffer_size is the size of the internal buffer. It is not
      used if [buffer] is provided.

      @param close is the close function of the channel. It defaults
      to [Lwt.return]

      @param seek has the same meaning as [Unix.lseek]

      @param mode is either {!input} or {!output}

      @param perform_io is the read or write function. It is called
      when more input is needed or when the buffer need to be
      flushed. *)

val encoding : 'a channel -> Encoding.t
  (** [encoding ch] returns the encoding of [ch] *)

val set_encoding : 'a channel -> Encoding.t -> unit
  (** [set_encoding ch enc] change the encoding used by [ch] to [enc] *)

val of_fd : ?buffer_size : int -> ?encoding : Encoding.t -> mode : 'a mode -> Lwt_unix.file_descr -> 'a channel
  (** [of_fd ~mode ~fd] creates a channel from a file descriptor *)

val of_unix_fd : ?buffer_size : int -> ?encoding : Encoding.t -> mode : 'a mode -> Unix.file_descr -> 'a channel
  (** [of_unix_fd ~mode ~fd] is a short-hand for:

      [of_fd (Lwt_unix.of_unix_file_descr fd)] *)

val open_file :
  ?buffer_size : int ->
  ?encoding : Encoding.t ->
  ?flags : Unix.open_flag list ->
  ?perm : Unix.file_perm ->
  mode : 'a mode ->
  string -> 'a channel
  (** [open_file ?buffer_size ?flags ?perm ~mode filename] open a file *)

val with_file :
  ?buffer_size : int ->
  ?encoding : Encoding.t ->
  ?flags : Unix.open_flag list ->
  ?perm : Unix.file_perm ->
  mode : 'a mode ->
  string -> ('a channel -> 'b Lwt.t) -> 'b Lwt.t
  (** [open_file ?buffer_size ?flags ?perm ~mode filename f] open a file and pass
      the channel to [f] *)

val open_connection : ?buffer_size : int -> ?encoding : Encoding.t -> Unix.sockaddr -> (ic * oc) Lwt.t
  (** [open_connection ?buffer_size ~mode addr] *)

val with_connection : ?buffer_size : int -> ?encoding : Encoding.t -> Unix.sockaddr -> (ic * oc -> 'a Lwt.t) -> 'a Lwt.t
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
  (** [read_char ic] reads one unicode character from [ic].

      @raise End_of_file on end of input *)

val read_char_opt : ic -> Text.t option Lwt.t
  (** Same as [read_char] but do not raises [End_of_file] on end of
      input *)

val read_chars : ic -> Text.t Lwt_stream.t
  (** [read_chars ic] returns a stream holding all characters of
      [ic] *)

val read : ?count : int -> ic -> Text.t Lwt.t
  (** [read ?count ic] reads up to [count] characters from [ic]. On
      end-of-file it returns the empty text [""].

      If [count] is not specified it reads all characters until the
      end of the file. *)

val read_line : ic -> Text.t Lwt.t
  (** [read_line ic] reads one complete line from [ic] and returns it
      without the end of line. End of line is either ["\n"] or
      ["\r\n"] *)

val read_line_opt : ic -> Text.t option Lwt.t
  (** Same as [read_line] but do not raise [End_of_file] on end of
      input. *)

val read_lines : ic -> Text.t Lwt_stream.t
  (** Returns a stream holding all lines of the given input channel *)

(** {6 Text output} *)

val write_char : oc -> Text.t -> unit Lwt.t
  (** [write_char oc ch] outputs [ch] on [oc] *)

val write : oc -> Text.t -> unit Lwt.t
  (** [write oc txt] writes [txt] on [oc] *)

val write_chars : oc -> Text.t Lwt_stream.t -> unit Lwt.t
  (** [write_chars oc st] writes all characters of [st] to [oc] *)

val write_line : oc -> Text.t -> unit Lwt.t
  (** [write_line oc txt] outputs [txt] on [oc] followed by a
      newline. *)

val write_lines : ?sep : Text.t -> oc -> Text.t Lwt_stream.t -> unit Lwt.t
  (** [write_lines ?sep oc lines] writes all lines of [lines] to [oc],
      separated by [sep] which defaults to ["\n"] *)

(** {6 Binary input} *)

val get_byte : ic -> char Lwt.t
  (** [get_byte ic] reads one byte from [ic] *)

val get_byte_opt : ic -> char option Lwt.t
  (** Same as {!get_byte} but do not raises [End_of_file] on end of
      input *)

val get_byte_array : ?count : int -> ic -> string Lwt.t
  (** [get_byte_array ?count ic] reads at most [len] bytes from
      [ic]. Returns [""] if the end of input is reached. If [count] is
      not specified, it reads all bytes until the end of input. *)

val get : ic -> string -> int -> int -> int Lwt.t
  (** [get ic buffer offset length] reads up to [length] characters,
      stores them in [buffer] at offset [offset], and returns the
      number of characters read.

      Note: [get] do not raises [End_of_file], it returns a length of
      [0] instead. *)

val get_exactly : ic -> string -> int -> int -> unit Lwt.t
  (** [get_exactly ic buffer offset length] reads exactly [length]
      characters and stores them in [buffer] at offset [offset].

      @raise End_of_file on end of input *)

val get_bytes : ic -> char Lwt_stream.t
  (** [get_bytes ic] returns a stream holding all bytes of [ic] *)

val get_value : ic -> 'a Lwt.t
  (** [get_value ic] reads a marshaled value from [ic] *)

(** {6 Binary output} *)

val flush : oc -> unit Lwt.t
  (** [flush oc] performs all pending writes on [oc] *)

val put_byte : oc -> char -> unit Lwt.t
  (** [put_byte oc byte] outputs [byte] to [oc] *)

val put_byte_array : oc -> string -> unit Lwt.t
  (** [put_bytes oc byte_array] outputs [byte_array] to [oc] *)

val put : oc -> string -> int -> int -> int Lwt.t
  (** [put oc buffer offset length] writes up to [length] bytes to
      [oc], from [buffer] at offset [offset] and returns the number of
      bytes actually written *)

val put_exactly : oc -> string -> int -> int -> unit Lwt.t
  (** [put oc buffer offset length] writes all [length] bytes from
      [buffer] at offset [offset] to [oc] *)

val put_bytes : oc -> char Lwt_stream.t -> unit Lwt.t
  (** [put_bytes oc st] writes all bytes of [st] to [oc] *)

val put_value : oc -> ?flags : Marshal.extern_flags list -> 'a -> unit Lwt.t
  (** [put_value oc ?flags x] marshals the value [x] to [oc] *)

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

(** {6 Misc} *)

(** Note: the three following functions are only needed because of the
    lack of GADTs: *)

external cast : 'a channel -> unit channel = "%identity"
  (** [cast ch] cast a channel to an unknown channel. *)

val cast_ic : 'a channel -> ic
  (** [cast_ic ch] cast a channel to an input channel.

      @raise Invalid_argument if [ch] is not an input channel. *)

val cast_oc : 'a channel -> oc
  (** [cast_oc ch] cast a channel to an output channel.

      @raise Invalid_argument if [ch] is not an output channel. *)

val get_default_buffer_size : unit -> int
  (** Return the default size for buffers. Channels that are created
      without specific size use this one. *)

val set_default_buffer_size : int -> unit
  (** Change the default buffer size.

      @raise Invalid_argument if the given size is smaller than [16]
      or greater than [Sys.max_string_length] *)
