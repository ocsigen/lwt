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

open Lwt
open Lwt_io
open Lwt_term

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
  return (Lwt_readline.complete before' word after
            (match get_directive before with
               | Some dir ->
                   Hashtbl.fold (fun k v l -> k :: l) Toploop.directive_table []
               | None ->
                   keywords))

(* +------------------+
   | Readline wrapper |
   +------------------+ *)

let history = ref []
let input = ref ""
let pos = ref 0

let rec read_input prompt buffer len =
  if !pos = String.length !input then begin
    let sprompt = if prompt = "  " then [Foreground blue; Text "> "] else [Foreground yellow; Text prompt] in
    input := Lwt_main.run
      (lwt l = Lwt_readline.readline ~complete ~history:(!history) sprompt in
       force_flush stdout >> return l);
    if Text.strip !input <> "" then history := !input :: !history;
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
      peek_byte stdin >>= function
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
  if Unix.isatty Unix.stdin then begin
    Toploop.read_interactive_input := read_input;

    let txt = "Welcome to the new Lwt powered OCaml toplevel!" in
    let col_border = cyan and col_txt = yellow in
    let len = Text.length txt in
    let { columns = col } = size () in
    let space = (col - 4 - len) / 2 in
    Lwt_main.run
      (cprintln [Foreground col_border; Text(Text.repeat space "─");
                 Text "┬─"; Text(Text.repeat len "─"); Text "─┬"; Text(Text.repeat (col - 4 - len - space) "─")]
       >> cprintln [Text(Text.repeat space " "); Foreground col_border; Text "│ "; Foreground col_txt; Text txt;
                    Foreground col_border; Text " │"]
       >> cprintln [Text(Text.repeat space " "); Foreground col_border; Text "└─"; Text(Text.repeat len "─"); Text "─┘"])
  end else
    Toploop.read_interactive_input := read_input_non_interactive
