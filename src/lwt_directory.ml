(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_directory
 * Copyright (C) 2005-2008 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *                    2009 Jérémie Dimino
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

open Lwt

let rec read_entry dir_handle =
  let entry = Unix.readdir dir_handle in
  if entry = "." || entry = ".." then
    read_entry dir_handle
  else
    entry

let stream ~path =
  let dir_handle =
    try
      Unix.opendir path
    with Unix.Unix_error(code, _, _) ->
      raise (Sys_error(Unix.error_message code))
  in
  let close = lazy(Unix.closedir dir_handle)
  and auto_yield = Lwt_unix.auto_yield 0.05 in
  let stream = Lwt_stream.from begin fun () ->
    lwt () = auto_yield () in
    try
      let entry = read_entry dir_handle in
      return (Some entry)
    with exn ->
      (try Lazy.force close with _ -> ());
      match exn with
        | End_of_file ->
            return None
        | Unix.Unix_error(code, _, _) ->
            fail(Sys_error(Unix.error_message code))
        | exn ->
            fail exn
  end in
  (object
     method stream = stream
     method stop = Lazy.force close
   end)

let list ~path =
  Lwt_stream.get_while (fun _ -> true) (stream ~path)#stream
