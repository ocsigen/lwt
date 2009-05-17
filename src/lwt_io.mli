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

(** Buffered byte channels *)

(** A {b channel} is a high-level object for performing IOs. It allow
    to read/write things from/to the outside worlds in an efficient
    way, by minimising the number of system calls.

    An {b output channel} is a channel that can be used to send data
    and an {b input channel} is a channel that can used to receive
    data. *)

exception Channel_closed of string
  (** Exception raised whan a channel is closed. The parameter is a
      description of the channel. *)

(** {6 Types} *)

type 'mode channel
  (** Type of buffered byte channels *)

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

type input_channel = input channel
    (** Type of input channels *)

type output_channel = output channel
    (** Type of output channels *)

val mode : 'a channel -> 'a mode
  (** [mode ch] returns the mode of a channel *)

type byte = char
    (** Type of bytes *)

type byte_array = string
    (** Type of sequence of bytes *)

(** {6 Well-known instances} *)

val stdin : input_channel
  (** The standard input, it reads data from {!Lwt_unix.stdin} *)

val stdout : output_channel
  (** The standard output, it writes data to {!Lwt_unix.stdout} *)

val stderr : output_channel
  (** The standard output for error messages, it writes data to
      {!Lwt_unix.stderr} *)

(** Note: [stdout] and [stderr] are auto-flushed *)

val zero : input_channel
  (** Inputs which returns always ['\x00'] *)

val null : output_channel
  (** Output which drops everything *)

(** {6 Channels creation/manipulation} *)

val pipe : ?buffer_size : int -> unit -> input_channel * output_channel
  (** [pipe ?buffer_size ()] creates a pipe.

      All data written to the writing side of the pipe can be read on
      the reading side. *)

val make :
  ?buffer_size : int ->
  ?close : (unit -> unit Lwt.t) ->
  ?seek : (int64 -> Unix.seek_command -> int64 Lwt.t) ->
  mode : 'mode mode ->
  (string -> int -> int -> int Lwt.t) -> 'mode channel
  (** [make ?buffer_size ?close ~mode perform_io] is the
      main function for creating new channels.

      @param buffer_size is the size of the internal buffer. It is not
      used if [buffer] is provided.

      @param close is the close function of the channel. It defaults
      to [Lwt.return]

      @param seek has the same meaning as [Unix.lseek]

      @param mode is either {!input} or {!output}

      @param perform_io is the read or write function. It is called
      when more input is needed or when the buffer need to be
      flushed. *)

val of_fd : ?buffer_size : int -> mode : 'mode mode -> Lwt_unix.file_descr -> 'mode channel
  (** [of_fd ~mode ~fd] creates a channel from a file descriptor *)

val of_unix_fd : ?buffer_size : int -> mode : 'mode mode -> Unix.file_descr -> 'mode channel
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
  (** [with_file ?buffer_size ?flags ?perm ~mode filename f] open a
      file and pass the channel to [f] *)

val open_connection : ?buffer_size : int -> Unix.sockaddr -> (input_channel * output_channel) Lwt.t
  (** [open_connection ?buffer_size ~mode addr] *)

val with_connection : ?buffer_size : int -> Unix.sockaddr -> (input_channel * output_channel -> 'a Lwt.t) -> 'a Lwt.t
  (** [with_connection ?buffer_size ~mode addr f] *)

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

val buffered : 'a channel -> int
  (** [buffered oc] returns the number of bytes in the buffer *)

val flush : output_channel -> unit Lwt.t
  (** [flush oc] performs all pending writes on [oc] *)

(** {6 Random access} *)

val position : 'a channel -> int64
  (** [position ch] Returns the current position in the channel. *)

val set_position : 'a channel -> int64 -> unit Lwt.t
  (** [set_position ch pos] Sets the position in the output channel. This
      does not work if the channel do not support random access. *)

val length : 'a channel -> int64 Lwt.t
  (** Returns the length of the channel in bytes *)

(** {6 Reading} *)

val read_byte : input_channel -> byte Lwt.t
  (** [read_byte ic] reads the next byte of [ic].

      @raise End_of_file if the end of the file is reached *)

val read_byte_opt : input_channel -> byte option Lwt.t
  (** Same as {!read_byte} but do not raises [End_of_file] on end of
      input *)

val read_bytes : input_channel -> byte Lwt_stream.t
  (** [read_bytes ic] returns a stream holding all bytes of [ic] *)

val read_byte_array : ?count : int -> input_channel -> byte_array Lwt.t
  (** [read_byte_array ?count is] reads at most [len] bytes from
      [ic]. It returns [""] if the end of input is reached. If [count]
      is not specified, it reads all bytes until the end of input. *)

val read_into : input_channel -> string -> int -> int -> int Lwt.t
  (** [get ic buffer offset length] reads up to [length] bytes, stores
      them in [buffer] at offset [offset], and returns the number of
      bytes read.

      Note: [read_into] does not raise [End_of_file], it returns a
      length of [0] instead. *)

val read_into_exactly : input_channel -> string -> int -> int -> unit Lwt.t
  (** [read_into_exactly ic buffer offset length] reads exactly
      [length] bytes and stores them in [buffer] at offset [offset].

      @raise End_of_file on end of input *)

val read_value : input_channel -> 'a Lwt.t
  (** [read_value ic] reads a marshaled value from [ic] *)

(** {6 Writing} *)

val write_byte : output_channel -> byte -> unit Lwt.t
  (** [write_byte oc byte] writes [byte] on [oc] *)

val write_byte_array : output_channel -> byte_array -> unit Lwt.t
  (** [write_byte_array oc byte_array] writes all bytes of
      [byte_array] on [oc] *)

val write_bytes : output_channel -> byte Lwt_stream.t -> unit Lwt.t
  (** [write_bytes oc bytes] writes all bytes contained hold by
      [bytes] on [oc] *)

val write_from : output_channel -> string -> int -> int -> int Lwt.t
  (** [write_from oc buffer offset length] writes up to [length] bytes
      to [oc], from [buffer] at offset [offset] and returns the number
      of bytes actually written *)

val write_from_exactly : output_channel -> string -> int -> int -> unit Lwt.t
  (** [write_from_exactly oc buffer offset length] writes all [length]
      bytes from [buffer] at offset [offset] to [oc] *)

val write_value : output_channel -> ?flags : Marshal.extern_flags list -> 'a -> unit Lwt.t
  (** [write_value oc ?flags x] marshals the value [x] to [oc] *)

(** {6 Low-level access to the internal buffer} *)

val block : 'a channel  -> int -> (string -> int -> 'b Lwt.t) -> 'b Lwt.t
  (** [block ch size f] pass to [f] the internal buffer and an
      offset. The buffer contains [size] bytes at [offset]. [f] may
      reads or writes these bytes.

      @param size must verify [0 <= size <= 256] *)

(** Informations for accessing directly to the internal buffer of a
    channel: *)
type direct_access = {
  da_buffer : byte_array;
  (** The internal buffer *)
  mutable da_ptr : int;
  (** The pointer to:
      - the beginning of free space for output channels
      - the beginning of data for input channels *)
  mutable da_max : int;
  (** The maximum offset *)
  da_perform : unit -> int Lwt.t;
  (** - for input channels:
        refill the buffer and returns how many bytes have been read
      - for output channels:
        flush partially the buffer and returns how many bytes have been written *)
}

val direct_access : 'a channel -> (direct_access -> 'b Lwt.t) -> 'b Lwt.t
  (** [direct_access ch f] pass to [f] a {!direct_access}
      structure. [f] must use it and update [da_ptr] to reflect how
      many bytes have been read/written. *)

(** {6 Input/output of integers} *)

(** Reading/writing integers *)
module type NumberIO = sig

  (** {8 Reading} *)

  val read_int : input_channel -> int Lwt.t
    (** Reads a 32-bits integer as an ocaml int *)

  val read_int16 : input_channel -> int Lwt.t
  val read_int32 : input_channel -> int32 Lwt.t
  val read_int64 : input_channel -> int64 Lwt.t

  val read_float32 : input_channel -> float Lwt.t
    (** Reads an IEEE single precision floating point value *)

  val read_float64 : input_channel -> float Lwt.t
    (** Reads an IEEE double precision floating point value *)

  (** {8 Writing} *)

  val write_int : output_channel -> int -> unit Lwt.t
    (** Writes an ocaml int as a 32-bits integer *)

  val write_int16 : output_channel -> int -> unit Lwt.t
  val write_int32 : output_channel -> int32 -> unit Lwt.t
  val write_int64 : output_channel -> int64 -> unit Lwt.t

  val write_float32 : output_channel -> float -> unit Lwt.t
    (** Writes an IEEE single precision floating point value *)

  val write_float64 : output_channel -> float -> unit Lwt.t
    (** Writes an IEEE double precision floating point value *)
end

module LE : NumberIO
  (** Reading/writing of integers in little-endian *)

module BE : NumberIO
  (** Reading/writing of integers in big-endian *)

(** {6 Misc} *)

val default_buffer_size : unit -> int
  (** Return the default size for buffers. Channels that are created
      without specific size use this one. *)

val set_default_buffer_size : int -> unit
  (** Change the default buffer size.

      @raise Invalid_argument if the given size is smaller than [16]
      or greater than [Sys.max_string_length] *)
