(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_read_line
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

(** Interactive line input *)

(** {6 Definitions} *)

exception Interrupt
  (** Exception raised when the user press [Ctrl^D] *)

type edition_state = Text.t * Text.t
    (** An edition state, it is a pair of two UTF-8 encoded strings:

        - the input before the cursor
        - the input after the cursor *)

type prompt = Lwt_term.styled_text
    (** A prompt. It may contains colors. *)

(** {8 Completion} *)

type completion_result =
  | No_completion
  | Complete_with of edition_state
  | Possibilities of Text.t list

val complete : Text.t -> Text.t -> Text.t -> Text.t list -> completion_result
  (** [complete before word after words] basic completion
      functions. [words] is a list of possible completions for
      [word]. *)

val print_words : Lwt_io.oc -> int -> string list -> unit Lwt.t
  (** [print_words oc columns strs] pretty-prints a list of words. *)

(** {8 History} *)

type history = Text.t list
    (** Type of an history *)

val save_history : string -> history -> unit Lwt.t
  (** [save_history filename history] saves [history] to
      [filename]. History is saved by separating lines with a null
      character. *)

val load_history : string -> history Lwt.t
  (** [load_history filename] loads history from [filename]. Returns
      the empty history if the the file does not exit. *)

(** {8 Clipboards} *)

type clipboard = Text.t ref
    (** Type of a clipboard. *)

val clipboard : clipboard
  (** The global clipboard. All read-line instances which do not use a
      specific clipboard use this one. *)

(** {6 High-level functions} *)

val read_line :
  ?history : history ->
  ?complete : (edition_state -> completion_result Lwt.t) ->
  ?clipboard : clipboard ->
  prompt -> Text.t Lwt.t
  (** [readline ?history ?complete prompt] inputs some text from the
      user. If input is not a terminal, it defaults to
      [Lwt_io.read_line Lwt_io.stdin] *)

val read_password :
  ?clipboard : clipboard ->
  ?style : [ `empty | `clear | `text of Text.t ] ->
  prompt -> Text.t Lwt.t
  (** [read_password ?clipboard ?clear prompt] inputs a password from
      the user. This function fails if input is not a terminal.

      [style] tell how the password is echoed to the user:

      - with [`empty] nothing is printed
      - with [`clear] the password is displayed has it
      - with [`text ch] all characters are replaced by [ch]

      It defaults to [`text "*"].
  *)

val read_keyword :
  ?history : history ->
  prompt -> (Text.t * 'value) list -> 'value Lwt.t
  (** [read_keyword ?history prompt keywords] reads one word which is
      a member of [words]. And returns which keyword the user
      choosed. *)

val read_yes_no : ?history : history -> prompt -> bool Lwt.t
  (** [read_yes_no ?history prompt] is the same as:

      {[
        read_keyword ?history prompt [("yes", true); ("y", true); ("no", false); ("n", false)]
      ]}
  *)
