(* Lightweight thread library for OCaml
 * http://www.ocsigen.org/lwt
 *
 * Copyright (C) 2016 Simon Cruanes
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

(** Module [Lwt_result]: explicit error handling *)

open Result

type (+'a, +'b) t = ('a, 'b) Result.result Lwt.t

let return x = Lwt.return (Ok x)
let fail e = Lwt.return (Error e)

let lift = Lwt.return
let ok x = Lwt.map (fun y -> Ok y) x

let map f e =
  Lwt.map
    (function
      | Error e -> Error e
      | Ok x -> Ok (f x))
    e

let map_err f e =
  Lwt.map
    (function
      | Error e -> Error (f e)
      | Ok x -> Ok x)
    e

let catch e =
  Lwt.catch
    (fun () -> ok e)
    fail

let get_exn e =
  Lwt.bind e
    (function
      | Ok x -> Lwt.return x
      | Error e -> Lwt.fail e)

let bind e f =
  Lwt.bind e
    (function
      | Error e -> Lwt.return (Error e)
      | Ok x -> f x)

let bind_lwt e f =
  Lwt.bind e
    (function
      | Ok x -> ok (f x)
      | Error e -> fail e)

let bind_result e f =
  Lwt.map
    (function
      | Error e -> Error e
      | Ok x -> f x)
    e

let bind_lwt_err e f =
  Lwt.bind e
    (function
      | Error e -> Lwt.bind (f e) fail
      | Ok x -> return x)

module Infix = struct
  let (>>=) = bind
  let (>|=) e f = map f e
end

include Infix
