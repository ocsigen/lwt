(* Lightweight thread library for OCaml
 * http://www.ocsigen.org/lwt
 * Module Lwt_simple_top
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

(* Integration with the toplevel for people who do not use the
   enhanced toplevel (the utop project). This module is deprecated. *)

[@@@ocaml.deprecated
" Use utop. See
   https://github.com/diml/utop"]

open Lwt.Infix

let read_input_non_interactive prompt buffer len =
  let rec loop i =
    if i = len then
      Lwt.return (i, false)
    else
      Lwt_io.read_char_opt Lwt_io.stdin >>= function
        | Some c ->
            Bytes.set buffer i c;
            if c = '\n' then
              Lwt.return (i + 1, false)
            else
              loop (i + 1)
        | None ->
            Lwt.return (i, true)
  in
  Lwt_main.run (Lwt_io.write Lwt_io.stdout prompt >>= fun () -> loop 0)

let () =
  Toploop.read_interactive_input := read_input_non_interactive
