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

(** Terminal control *)

(** This modules allow you to write interactive programs using the
    terminal. *)

val with_raw_mode : (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [with_raw_mode f] executes [f] while the terminal is in ``raw
      mode''. Raw mode means that character are returned as the user
      type them (otherwise only complete line are returned to the
      program). *)

val raw_mode : unit -> bool
  (** Returns wether the terminal is currently in raw mode *)

val show_cursor : unit -> unit Lwt.t
  (** [show_cursor ()] makes the cursor visible *)

val hide_cursor : unit -> unit Lwt.t
  (** [hide_cursor ()] makes the cursor invisible *)

val clear_screen : unit -> unit Lwt.t
  (** [clear_screen ()] clears the entire screen *)

(** {6 Terminal informations} *)

(** Terminal sizes: *)
type size = {
  lines : int;
  columns : int;
}

val size : unit -> size
  (** [size ()] returns the current size of the terminal. *)

(** {6 Keys} *)

val parse_key_raw : Text.t Lwt_stream.t -> Text.t Lwt.t
  (** [parse_key_raw st] recognize escape sequence in a stream of
      unicode character.

      It returns either:
      - either single characters, like ["a"], ["é"], ...
      - either escape sequences
  *)

(** Type of ``decoded'' keys.

    This list is not exhaustive, but at least it should works on all
    terminals: *)
type key =
  | Key of Text.t
      (** A unicode character or an uninterpreted sequence *)
  | Key_up
  | Key_down
  | Key_left
  | Key_right
  | Key_escape
  | Key_f of int
  | Key_enter
  | Key_next_page
  | Key_previous_page
  | Key_home
  | Key_end
  | Key_insert
  | Key_backspace
  | Key_delete
  | Key_tab
  | Key_control of char
      (** A control key other than [Key_enter], [Key_escape],
          [Key_backspace] or [Key_tab] *)

val string_of_key : key -> string
  (** [string_of_key key] string representation of a key *)

val control_mapping : (int * char) list
  (** Mapping from control key codes to char code.

      Here is the list of control keys:

      {[
        +------+-------+------+------+------+-------+------------------------------------------------+
        | Char |   Oct |  Dec | Name |  Hex |  Key  | Comment                                        |
        +------+-------+------+------+------+-------+------------------------------------------------+
        |  '@' |  0o00 |    0 |  NUL | 0x00 | ^@ \0 | Null byte                                      |
        |  'a' |  0o01 |    1 |  SOH | 0x01 | ^A    | Start of heading                               |
        |  'b' |  0o02 |    2 |  STX | 0x02 | ^B    | Start of text                                  |
        |  'c' |  0o03 |    3 |  ETX | 0x03 | ^C    | End of text                                    |
        |  'd' |  0o04 |    4 |  EOT | 0x04 | ^D    | End of transmission                            |
        |  'e' |  0o05 |    5 |  ENQ | 0x05 | ^E    | Enquiry                                        |
        |  'f' |  0o06 |    6 |  ACK | 0x06 | ^F    | Acknowledge                                    |
        |  'g' |  0o07 |    7 |  BEL | 0x07 | ^G    | Ring terminal bell                             |
        |  'h' |  0o10 |    8 |   BS | 0x08 | ^H \b | Backspace                                      |
        |  'i' |  0o11 |    9 |   HT | 0x09 | ^I \t | Horizontal tab                                 |
        |  'j' |  0o12 |   10 |   LF | 0x0a | ^J \n | Line feed                                      |
        |  'k' |  0o13 |   11 |   VT | 0x0b | ^K    | Vertical tab                                   |
        |  'l' |  0o14 |   12 |   FF | 0x0c | ^L \f | Form feed                                      |
        |  'm' |  0o15 |   13 |   CR | 0x0d | ^M \r | Carriage return                                |
        |  'n' |  0o16 |   14 |   SO | 0x0e | ^N    | Shift out                                      |
        |  'o' |  0o17 |   15 |   SI | 0x0f | ^O    | Shift in                                       |
        |  'p' |  0o20 |   16 |  DLE | 0x10 | ^P    | Data link escape                               |
        |  'q' |  0o21 |   17 |  DC1 | 0x11 | ^Q    | Device control 1 (XON)                         |
        |  'r' |  0o22 |   18 |  DC2 | 0x12 | ^R    | Device control 2                               |
        |  's' |  0o23 |   19 |  DC3 | 0x13 | ^S    | Device control 3 (XOFF)                        |
        |  't' |  0o24 |   20 |  DC4 | 0x14 | ^T    | Device control 4                               |
        |  'u' |  0o25 |   21 |  NAK | 0x15 | ^U    | Negative acknowledge                           |
        |  'v' |  0o26 |   22 |  SYN | 0x16 | ^V    | Synchronous idle                               |
        |  'w' |  0o27 |   23 |  ETB | 0x17 | ^W    | End of transmission block                      |
        |  'x' |  0o30 |   24 |  CAN | 0x18 | ^X    | Cancel                                         |
        |  'y' |  0o31 |   25 |   EM | 0x19 | ^Y    | End of medium                                  |
        |  'z' |  0o32 |   26 |  SUB | 0x1a | ^Z    | Substitute character                           |
        |  '[' |  0o33 |   27 |  ESC | 0x1b | ^[    | Escape                                         |
        |  '\' |  0o34 |   28 |   FS | 0x1c | ^\    | File separator, Information separator four     |
        |  ']' |  0o35 |   29 |   GS | 0x1d | ^]    | Group separator, Information separator three   |
        |  '^' |  0o36 |   30 |   RS | 0x1e | ^^    | Record separator, Information separator two    |
        |  '_' |  0o37 |   31 |   US | 0x1f | ^_    | Unit separator, Information separator one      |
        |  '?' | 0o177 |  127 |  DEL | 0x7f | ^?    | Delete                                         |
        +------+-------+------+------+------+-------+------------------------------------------------+
      ]}
  *)

val sequence_mapping : (Text.t * key) list
  (** Mapping from sequence to keys *)

val decode_key : Text.t -> key
  (** Decode a key. *)

val standard_input : Text.t Lwt_stream.t
  (** The input stream used by [get_key] *)

val get_key : unit -> key Lwt.t
  (** Get and decode a key from {!Lwt_stream.standard_text} *)

(** {6 Styles} *)

type color = int
    (** Type of a color. Most modern terminals support either 88 or
        256 colors. *)

val set_color : color -> int * int * int -> unit Lwt.t
  (** [set_color num (red, green, blue)] sets the three components of
      the color number [num] *)

val set_colors : (color * (int * int * int)) list -> unit Lwt.t
  (** [set_colors cols] sets multiples colors at the same time *)

(** {8 Standard colors} *)

val default : color
val black : color
val red : color
val green : color
val yellow : color
val blue : color
val magenta : color
val cyan : color
val white : color
val default : color

(** {8 Character styles} *)

type style = {
  bold : bool;
  underlined : bool;
  blink : bool;
  inverse : bool;
  hidden : bool;
  foreground : color;
  background : color;
}

(** {8 Text with styles} *)

(** Elmement of a styled-text *)
type styled_text_instruction =
  | Text of Text.t
      (** Some text *)
  | Reset
      (** Resets all styles to default *)
  | Bold
  | Underlined
  | Blink
  | Inverse
  | Hidden
  | Foreground of color
  | Background of color

type styled_text = styled_text_instruction list
    (** A styled text is a list of instructions *)

val strip_styles : styled_text -> Text.t
  (** Drop all styles *)

val apply_styles : styled_text -> Text.t
  (** Replace all styles by their escape sequence. *)

val styled_length : styled_text -> int
  (** Returns the length (in unicode character) of the given styled
      text. The following equality holds for all styled-texts:

      [styled_length st = Text.length (strip_styles st)]
  *)

val cprint : styled_text -> unit Lwt.t
  (** [cprint st] prints the given styled text on standard output. If
      stdout is not a tty, then styles are stripped.

      The text is encoded to the system encoding before being
      output. *)

val ecprint : styled_text -> unit Lwt.t
  (** Same as [print] but prints on stderr. *)

val cprintln : styled_text -> unit Lwt.t
  (** [println st] prints [st], then reset styles and print a newline *)

val ecprintln : styled_text -> unit Lwt.t
  (** Same as [println] but prints on stderr *)

(** {6 Rendering} *)

(** A character on the screen: *)
type point = {
  char : Text.t;
  (** The character. *)
  style : style;
  (** The character style *)
}

val blank : point
  (** A space with default color and styles *)

val render : point array array -> string
  (** Convert an offscreen array to a string which just need to be
      output. It contains all escape sequence to go to the top-left
      corner, and set colors. *)
