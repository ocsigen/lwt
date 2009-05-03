(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_term
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

type edition_state = Text.t * Text.t
    (** An edition state, it is a pair of two UTF-8 encoded strings:

        - the input before the cursor
        - the input after the cursor *)

(** {6 Completion} *)

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

(** {6 History} *)

type history = Text.t list
    (** Type of an history *)

val save_history : string -> history -> unit Lwt.t
  (** [save_history filename history] saves [history] to
      [filename]. History is saved by separating lines with a null
      character. *)

val load_history : string -> history Lwt.t
  (** [load_history filename] loads history from [filename]. Returns
      the empty history if the the file does not exit. *)

(** {6 Readline} *)

val readline :
  ?history : history ->
  ?complete : (edition_state -> completion_result Lwt.t) ->
  Lwt_term.styled_text -> Text.t Lwt.t
  (** [readline ?history ?complete prompt] inputs some text from the
      user. *)
