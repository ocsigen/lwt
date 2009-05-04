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

(* +-------------+
   | Lwt-aliases |
   +-------------+ *)

let return = Lwt.return
let fail = Lwt.fail
let bind = Lwt.bind
let (>>=) = Lwt.(>>=)
let (=<<) = Lwt.(=<<)
let (>|=) = Lwt.(>|=)
let (=|<) = Lwt.(=|<)
let (<?>) = Lwt.(<?>)
let (<&>) = Lwt.(<&>)

(* +----------------+
   | Lwt-io aliases |
   +----------------+ *)

let input = Lwt_io.input
let output = Lwt_io.output
let stdin = Lwt_io.stdin
let stdout = Lwt_io.stdout
let stderr = Lwt_io.stderr
let open_file = Lwt_io.open_file
let with_file = Lwt_io.with_file
let read_char = Lwt_io.read_char
let peek_char = Lwt_io.peek_char
let read_text = Lwt_io.read_text
let read_line = Lwt_io.read_line
let peek_line = Lwt_io.peek_line
let write_char = Lwt_io.write_char
let write_text = Lwt_io.write_text
let write_line = Lwt_io.write_line
let close = Lwt_io.close

(* +----------+
   | Printing |
   +----------+ *)

let print = write_text stdout
let eprint = write_text stderr

let fprintl oc is_atty txt =
  if Lazy.force is_atty then
    Lwt_io.atomic
      (fun oc ->
         write_text oc txt
         >> write_text oc (if Lwt_term.raw_mode () then "\r\n" else "\n")) oc
  else
    write_line oc txt

let printl txt = fprintl stdout Lwt_term.stdout_is_atty txt
let eprintl txt = fprintl stderr Lwt_term.stderr_is_atty txt

let printf fmt = Printf.ksprintf print fmt
let printlf fmt = Printf.ksprintf printl fmt
let eprintf fmt = Printf.ksprintf eprint fmt
let eprintlf fmt = Printf.ksprintf eprintl fmt

(* +-----------------+
   | Styled printing |
   +-----------------+ *)

let printc = Lwt_term.printc
let eprintc = Lwt_term.eprintc
let printlc = Lwt_term.printlc
let eprintlc = Lwt_term.eprintlc

let textf fmt = Printf.ksprintf (fun txt -> Lwt_term.Text txt) fmt
let text txt = Lwt_term.Text txt
let reset = Lwt_term.Reset
let bold = Lwt_term.Bold
let underlined = Lwt_term.Underlined
let blink = Lwt_term.Blink
let inverse = Lwt_term.Inverse
let hidden = Lwt_term.Hidden
let fg col = Lwt_term.Foreground col
let bg col = Lwt_term.Background col

let default = Lwt_term.default
let black = Lwt_term.black
let red = Lwt_term.red
let green = Lwt_term.green
let yellow = Lwt_term.yellow
let blue = Lwt_term.blue
let magenta = Lwt_term.magenta
let cyan = Lwt_term.cyan
let white = Lwt_term.white
let default = Lwt_term.default

(* +------------------+
   | Stream utilities |
   +------------------+ *)

let lines_of_channel ?(auto_close=true) ic =
  Lwt_stream.from (fun _ -> peek_line ic >>= function
                     | None ->
                         if auto_close then
                           Lwt_io.close ic >> return None
                         else
                           return None
                     | x ->
                         return x)

let lines_to_channel ?(sep="\n") oc lines =
  Lwt_stream.iter_s (fun line -> Lwt_io.atomic (fun oc -> write_text oc line >> write_text oc sep) oc) lines

let lines_of_file filename = lines_of_channel (open_file ~mode:input filename)

let lines_to_file ?sep filename lines = with_file ~mode:output filename (fun oc -> lines_to_channel ?sep oc lines)

let lines_to_process ?sep cmd lines =
  Lwt_process.with_process_out cmd (fun process -> lines_to_channel ?sep process#stdin lines)

let lines_of_process cmd =
  let pr = Lwt_process.process_in cmd in
  Lwt_stream.from (fun _ -> peek_line pr#stdout >>= function
                     | None ->
                         pr#close >> return None
                     | x ->
                         return x)
