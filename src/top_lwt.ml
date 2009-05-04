(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Top_lwt
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

open Lwt_pervasives

(* +------------+
   | Completion |
   +------------+ *)

let keywords = [
  "and"; "as"; "assert"; "begin"; "class"; "constraint"; "do";
  "done"; "downto"; "else"; "end"; "exception"; "external"; "false";
  "fo"; "fun"; "function"; "functo"; "if"; "in"; "include";
  "inherit"; "initialize"; "lazy"; "let"; "match"; "method"; "module";
  "mutable"; "new"; "object";  "of";  "open"; "private";  "rec"; "sig";
  "struct";  "then";  "to";  "true";  "try";  "type";  "val"; "virtual";
  "when"; "while"; "with"; "try_lwt"; "finally";
]

let split_last_word txt =
  let rec aux ptr =
    match Text.prev ptr with
      | None ->
          ("", txt)
      | Some(ch, ptr') ->
          if Text.is_alnum ch || ch = "_" || ch = "'" || ch = "." then
            aux ptr'
          else
            (Text.chunk (Text.pointer_l txt) ptr, Text.chunk ptr (Text.pointer_r txt))
  in
  aux (Text.pointer_r txt)

let get_directive txt =
  let txt = Text.strip txt in
  if txt <> "" && Text.get txt 0 = "#" then
    let txt = Text.lchop txt in
    if Text.for_all (fun ch -> Text.is_alnum ch || ch = "_" || ch = "'" || ch = ".") txt then
      Some txt
    else
      None
  else
    None

let complete (before, after) =
  let before', word = split_last_word before in
  return (Lwt_read_line.complete before' word after
            (match get_directive before with
               | Some dir ->
                   Hashtbl.fold (fun k v l -> k :: l) Toploop.directive_table []
               | None ->
                   keywords))

(* +-------------------+
   | Read-line wrapper |
   +-------------------+ *)

let history = ref []
let input = ref ""
let pos = ref 0

let rec read_input prompt buffer len =
  if !pos = String.length !input then begin
    let sprompt = if prompt = "  " then [fg blue; text "> "] else [fg yellow; text prompt] in
    let txt = Lwt_main.run
      (lwt l = Lwt_read_line.read_line ~complete ~history:(!history) sprompt in
       Lwt_io.force_flush stdout >> return l) in
    if Text.strip txt <> "" then history := txt :: !history;
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

let read_input_non_interactive prompt buffer len =
  let rec loop i =
    if i = len then
      return (i, false)
    else
      Lwt_io.peek_byte stdin >>= function
        | Some c ->
            buffer.[i] <- c;
            if c = '\n' then
              return (i + 1, false)
            else
              loop (i + 1)
        | None ->
            return (i, true)
  in
  Lwt_main.run (write_text stdout prompt >> loop 0)

let _ =
  (* If input is a tty, use interactive read-line and display and
     welcome message: *)
  if Unix.isatty Unix.stdin then begin
    Toploop.read_interactive_input := read_input;

    let txt = "Welcome to the new Lwt powered OCaml toplevel!" in
    let col_border = cyan and col_txt = yellow in
    let len = Text.length txt in
    let col = Lwt_term.columns () in
    let space = (col - 4 - len) / 2 in
    let rep n txt = text (Text.repeat n txt) in
    Lwt_main.run
      (cprintln [fg col_border; rep space "─"; text "┬─"; rep len "─"; text "─┬"; rep (col - 4 - len - space) "─"] >>
         cprintln [rep space " "; fg col_border; text "│ "; fg col_txt; text txt; fg col_border; text " │"] >>
         cprintln [rep space " "; fg col_border; text "└─"; rep len "─"; text "─┘"])
  end else
    (* Otherwise fallback to classic non-interactive mode: *)
    Toploop.read_interactive_input := read_input_non_interactive
