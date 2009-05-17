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

(* +-----------------------------------------------------------------+
   | Lwt-aliases                                                     |
   +-----------------------------------------------------------------+ *)

let return = Lwt.return
let fail = Lwt.fail
let bind = Lwt.bind
let (>>=) = Lwt.(>>=)
let (=<<) = Lwt.(=<<)
let (>|=) = Lwt.(>|=)
let (=|<) = Lwt.(=|<)
let (<?>) = Lwt.(<?>)
let (<&>) = Lwt.(<&>)

(* +-----------------------------------------------------------------+
   | Lwt-io aliases                                                  |
   +-----------------------------------------------------------------+ *)

let input = Lwt_io.input
let output = Lwt_io.output

(* +-----------------------------------------------------------------+
   | Printing                                                        |
   +-----------------------------------------------------------------+ *)

let print txt = Lwt_text.write Lwt_text.stdout txt
let printl txt = Lwt_text.write_line Lwt_text.stdout txt
let printf fmt = Printf.ksprintf print fmt
let printlf fmt = Printf.ksprintf printl fmt

let eprint txt = Lwt_text.write Lwt_text.stderr txt
let eprintl txt = Lwt_text.write_line Lwt_text.stderr txt
let eprintf fmt = Printf.ksprintf print fmt
let eprintlf fmt = Printf.ksprintf printl fmt

(* +-----------------------------------------------------------------+
   | Styled printing                                                 |
   +-----------------------------------------------------------------+ *)

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
let lblack = Lwt_term.lblack
let lred = Lwt_term.lred
let lgreen = Lwt_term.lgreen
let lyellow = Lwt_term.lyellow
let lblue = Lwt_term.lblue
let lmagenta = Lwt_term.lmagenta
let lcyan = Lwt_term.lcyan
let lwhite = Lwt_term.lwhite


(* +-----------------------------------------------------------------+
   | File utilities                                                  |
   +-----------------------------------------------------------------+ *)

type file_name = Text.t

let make_stream f close ic =
  Lwt_stream.from (fun _ ->
                     try_lwt
                       f ic >|= fun x -> Some x
                     with
                       | End_of_file ->
                           close ic >> return None)

let lines_of_file filename =
  make_stream Lwt_text.read_line Lwt_text.close (Lwt_text.open_file ~mode:input filename)

let lines_to_file filename lines =
  Lwt_text.with_file ~mode:output filename (fun oc -> Lwt_text.write_lines oc lines)

let chars_of_file filename =
  make_stream Lwt_text.read_char Lwt_text.close  (Lwt_text.open_file ~mode:input filename)

let chars_to_file filename chars =
  Lwt_text.with_file ~mode:output filename (fun oc -> Lwt_text.write_chars oc chars)

let bytes_of_file filename =
  make_stream Lwt_io.read_byte Lwt_io.close (Lwt_io.open_file ~mode:input filename)

let bytes_to_file filename bytes =
  Lwt_io.with_file ~mode:output filename (fun oc -> Lwt_io.write_bytes oc bytes)

(* +-----------------------------------------------------------------+
   | Misc                                                            |
   +-----------------------------------------------------------------+ *)

let sleep = Lwt_unix.sleep
let yield = Lwt_unix.yield
