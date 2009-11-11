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

(** Result of a completion function: *)
type completion_result = {
  comp_state : edition_state;
  (** The new edition state *)

  comp_words : Text.t list;
  (** A list of possibilities *)
}

type completion = edition_state -> completion_result Lwt.t
      (** Type of a completion function. It takes as argument the
          current edition state.

          Note: the thread launched by the completion function is
          cancelled using {!Lwt.cancel} if the user continue typing
          text. *)

val lookup : Text.t -> Text.t list -> (Text.t * Text.t list)
  (** [lookup word words] lookup for completion of [word] into
      [words]. It returns [(prefix, possibilities)] where
      [possibilities] are all words starting with [word] and [prefix]
      is the longest common prefix of [words]. *)

val complete : ?suffix : Text.t -> Text.t -> Text.t -> Text.t -> Text.t list -> completion_result
  (** [complete ?suffix before word after words] basic completion
      functions. [words] is a list of possible completions for
      [word].

      If completion succeed [suffix] is append to the resulting
      text. It defaults to [" "]. *)

val print_words : Lwt_text.output_channel -> int -> string list -> unit Lwt.t
  (** [print_words oc columns strs] pretty-prints a list of words. *)

(** {8 History} *)

type history = Text.t list
    (** Type of an history *)

val add_entry : Text.t -> history -> history
  (** [add_entry line history] returns the history [history] plus
      [line] at the beginning. If [line] already appears at the
      beginning or contains only spaces, it is discarded. *)

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
  ?complete : completion ->
  ?clipboard : clipboard ->
  ?dynamic : bool ->
  prompt -> Text.t Lwt.t
  (** [readline ?history ?complete ?dynamic prompt] inputs some text
      from the user. If input is not a terminal, it defaults to
      [Lwt_io.read_line Lwt_io.stdin].

      If @param dynamic is [true], then possible competions will be
      shown to the user as he types. It default to [false]. *)

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
  ?case_sensitive : bool ->
  ?dynamic : bool ->
  prompt -> (Text.t * 'value) list -> 'value Lwt.t
  (** [read_keyword ?history ?case_sensitive ?dynamic prompt keywords]
      reads one word which is a member of [words]. And returns which
      keyword the user choosed.

      [case_sensitive] default to [false]. *)

val read_yes_no : ?history : history -> ?dynamic : bool -> prompt -> bool Lwt.t
  (** [read_yes_no ?history ?dynamic prompt] is the same as:

      {[
        read_keyword ?history ?dynamic prompt [("yes", true); ("y", true); ("no", false); ("n", false)]
      ]}
  *)

(** {6 Low-level interaction} *)

(** This part allow you to implements your own read-line function, or
    just to use the readline engine in another context (message box,
    ...). *)

(** Readline commands *)
module Command : sig

  (** Type of all read-line function: *)
  type t =
    | Nop
        (** Command which do nothing. Unknown keys maps to this commands. *)
    | Char of Text.t
        (** Any printable character. *)
    | Backward_delete_char
    | Forward_delete_char
    | Beginning_of_line
    | End_of_line
    | Complete
    | Kill_line
    | Accept_line
    | Backward_delete_word
    | Forward_delete_word
    | History_next
    | History_previous
    | Break
    | Clear_screen
    | Insert
    | Refresh
    | Backward_char
    | Forward_char
    | Set_mark
    | Paste
    | Copy
    | Cut
    | Uppercase
    | Lowercase
    | Capitalize
    | Backward_word
    | Forward_word
    | Complete_left
    | Complete_right
    | Complete_up
    | Complete_down

  val to_string : t -> string
    (** [to_string cmd] returns a string representation of a command *)

  val of_key : Lwt_term.key -> t
    (** [of_key key] returns the command to which a key is mapped. *)
end

(** Engine *)
module Engine : sig

  (** Note: this part know nothing about rendering or completion. *)

  (** State when the user is doing selection: *)
  type selection_state = {
    sel_text : Text.t;
    (** The whole input text on which the selection is working *)
    sel_mark : Text.pointer;
    (** Pointer to the mark *)
    sel_cursor : Text.pointer;
    (** Pointer to the text position *)
  }

  (** The engine mode: *)
  type mode =
    | Edition of edition_state
        (** The user is typing some text *)
    | Selection of selection_state
        (** The user is selecting some text *)

  (** An engine state: *)
  type state = {
    mode : mode;
    history : history * history;
    (** Cursor to the history position. *)
  }

  val init : history -> state
    (** [init history] return a initial state using the given
        history *)

  val reset : state -> state
    (** [reset state] reset the given state, if the user was doing a
        selection, it is canceled *)

  val update : state -> ?clipboard : clipboard -> Command.t -> state
    (** [update state clipboard command] update an engine state by
        processing the given command. It returns the new state, and
        may have the side effect of changing the clipboard contents.

        [clipboard] defaults to the global clipboard.
    *)

  val edition_state : state -> edition_state
    (** Returns the edition state of a state, whatever its mode is. *)

  val all_input : state -> Text.t
    (** Returns the current complete user input. *)
end

(** Rendering to the terminal *)
module Terminal : sig

  type state
    (** State of rendering *)

  val init : state
    (** Initial state *)

  val draw : ?map_text : (Text.t -> Text.t) -> ?completion : Text.t list ->
    state -> Engine.state -> prompt -> state Lwt.t
    (** [draw ?map_text ?completion state engine_state prompt] erase
        previous printed text, draw the new one, and return a new
        state for future redrawing.

        @param map_text is a function used to map user input before
        printing it, for example to hide passwords.

        @param completion is for dynamic completion mode
    *)

  val last_draw : ?map_text : (Text.t -> Text.t) -> ?completion : bool ->
    state -> Engine.state -> prompt -> unit Lwt.t
    (** Draw for the last time, i.e. the cursor is left after the text
        and not at current position. *)
end
