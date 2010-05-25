(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_top
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

(* Integration with the toplevel:

   readline + let threads runs while reading user input. *)

open Lwt_unix
open Lwt
open Lwt_text
open Lwt_term

(* +-----------------------------------------------------------------+
   | Completion                                                      |
   +-----------------------------------------------------------------+ *)

module TextSet = Set.Make(Text)

let complete (before, after) =
  Lwt_ocaml_completion.complete_input before after (Lexing.from_string before)

(* +-----------------------------------------------------------------+
   | Read-line wrapper                                               |
   +-----------------------------------------------------------------+ *)

let mode = ref `real_time
let completion_mode () = !mode
let set_completion_mode m = mode := m

let history = ref []

let _ =
  let hist_name = Filename.concat (try Unix.getenv "HOME" with _ -> "") ".lwt-top-history" in
  Lwt_main.at_exit (fun () -> Lwt_read_line.save_history hist_name !history);
  history := Lwt_main.run (Lwt_read_line.load_history hist_name)

let input = ref ""
let pos = ref 0

let rec read_input prompt buffer len =
  try
    if !pos = String.length !input then begin
      let prompt' = if prompt = "  " then [fg blue; text "> "] else [fg yellow; text prompt] in
      !Lwt_ocaml_completion.restart ();
      let txt = Lwt_main.run begin
        lwt l = Lwt_read_line.Control.result
          (Lwt_read_line.Control.make
             ~complete
             ~mode:!mode
             ~history:(!history)
             ~prompt:(fun _ -> React.S.const prompt')
             ~filter:(fun state command ->
                        match command with
                          | Lwt_read_line.Command.Accept_line ->
                              (* Do not accept the line if it does not terminates with ";;" *)
                              let text = Lwt_read_line.Engine.all_input (Lwt_read_line.Control.engine_state state) in
                              if Text.ends_with (Text.rstrip text) ";;" then
                                return Lwt_read_line.Command.Accept_line
                              else
                                return (Lwt_read_line.Command.Char "\n")
                          | command ->
                              return command)
             ~map_result:return
             ())
        in
        lwt () = Lwt_text.flush Lwt_text.stdout in
        return l
      end in
      history := Lwt_read_line.add_entry txt !history;
      input := txt ^ "\n";
      pos := 0;
      read_input prompt buffer len
    end else begin
      let i = ref 0 in
      while !i < len && !pos < String.length !input do
        buffer.[!i] <- (!input).[!pos];
        incr i;
        incr pos
      done;
      (!i, false)
    end
  with
    | Lwt_read_line.Interrupt ->
        (0, true)

let read_input_non_interactive prompt buffer len =
  let rec loop i =
    if i = len then
      return (i, false)
    else
      Lwt_io.read_char_opt Lwt_io.stdin >>= function
        | Some c ->
            buffer.[i] <- c;
            if c = '\n' then
              return (i + 1, false)
            else
              loop (i + 1)
        | None ->
            return (i, true)
  in
  Lwt_main.run (Lwt_io.write Lwt_io.stdout prompt >> loop 0)

let _ =
  (* If input is a tty, use interactive read-line and display and
     welcome message: *)
  if Unix.isatty Unix.stdin then begin
    Toploop.read_interactive_input := read_input;

    let txt = "Welcome to the Lwt powered OCaml toplevel!" in
    let col_border = cyan and col_txt = yellow in
    let len = Text.length txt in
    let col = React.S.value Lwt_term.columns in
    let space = (col - 4 - len) / 2 in
    let rep n txt = text (Text.repeat n txt) in
    Lwt_main.run
      (printlc [fg col_border; rep space "─"; text "┬─"; rep len "─"; text "─┬"; rep (col - 4 - len - space) "─"] >>
       printlc [rep space " "; fg col_border; text "│ "; fg col_txt; text txt; fg col_border; text " │"] >>
       printlc [rep space " "; fg col_border; text "└─"; rep len "─"; text "─┘"])
  end else
    (* Otherwise fallback to classic non-interactive mode: *)
    Toploop.read_interactive_input := read_input_non_interactive;
