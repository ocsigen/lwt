
(* OCaml promise library
 * http://www.ocsigen.org/lwt
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

open Test

[@@@ocaml.warning "-3"]
module Lwt_sequence = Lwt_sequence
[@@@ocaml.warning "+3"]

let filled_sequence () =
  let s = Lwt_sequence.create () in
  let _ = Lwt_sequence.add_l (-1) s in
  let _ = Lwt_sequence.add_l (-2) s in
  let _ = Lwt_sequence.add_l (-3) s in
  let _ = Lwt_sequence.add_r 1 s in
  let _ = Lwt_sequence.add_r 2 s in
  let _ = Lwt_sequence.add_r 3 s in
  s

let filled_length = 6

let leftmost_value = (-3)

let rightmost_value = 3

let transfer_sequence () =
  let s = Lwt_sequence.create () in
  let _ = Lwt_sequence.add_r 4 s in
  let _ = Lwt_sequence.add_r 5 s in
  s

let transfer_length = 2

let suite = suite "lwt_sequence" [

  test "create" begin fun () ->
    let s = Lwt_sequence.create () in
    let _ = assert (Lwt_sequence.is_empty s) in
    let len = Lwt_sequence.length s in
    Lwt.return (len = 0)
  end;

  test "add_l" begin fun () ->
    let s = Lwt_sequence.create () in
    let n = Lwt_sequence.add_l 1 s in
    let _ = assert ((Lwt_sequence.get n) = 1) in
    let len = Lwt_sequence.length s in
    Lwt.return (len = 1)
  end;

  test "add_r" begin fun () ->
    let s = Lwt_sequence.create () in
    let n = Lwt_sequence.add_r 1 s in
    let _ = assert ((Lwt_sequence.get n) = 1) in
    let len = Lwt_sequence.length s in
    Lwt.return (len = 1)
  end;

  test "take_l Empty" begin fun () ->
    let s = Lwt_sequence.create () in
    Lwt.catch
      (fun () ->
        let _ = Lwt_sequence.take_l s in
        Lwt.return_false
      )
      (function
        | Lwt_sequence.Empty -> Lwt.return_true
        | _ -> Lwt.return_false
      )
  end;

  test "take_l" begin fun () ->
    let s = filled_sequence () in
    Lwt.catch
      (fun () ->
        let v = Lwt_sequence.take_l s in
        Lwt.return (leftmost_value = v)
      )
      (function
        | _ -> Lwt.return_false
      )
  end;

  test "take_r Empty" begin fun () ->
    let s = Lwt_sequence.create () in
    Lwt.catch
      (fun () ->
        let _ = Lwt_sequence.take_r s in
        Lwt.return_false
      )
      (function
        | Lwt_sequence.Empty -> Lwt.return_true
        | _ -> Lwt.return_false
      )
  end;

  test "take_r" begin fun () ->
    let s = filled_sequence () in
    Lwt.catch
      (fun () ->
        let v = Lwt_sequence.take_r s in
        Lwt.return (rightmost_value = v)
      )
      (function
        | _ -> Lwt.return_false
      )
  end;

  test "take_opt_l Empty" begin fun () ->
    let s = Lwt_sequence.create () in
    match Lwt_sequence.take_opt_l s with
    | None -> Lwt.return_true
    | _ -> Lwt.return_false
  end;

  test "take_opt_l" begin fun () ->
    let s = filled_sequence () in
    match Lwt_sequence.take_opt_l s with
    | None -> Lwt.return_false
    | Some v -> Lwt.return (leftmost_value = v)
  end;

  test "take_opt_r Empty" begin fun () ->
    let s = Lwt_sequence.create () in
    match Lwt_sequence.take_opt_r s with
    | None -> Lwt.return_true
    | _ -> Lwt.return_false
  end;

  test "take_opt_r" begin fun () ->
    let s = filled_sequence () in
    match Lwt_sequence.take_opt_r s with
    | None -> Lwt.return_false
    | Some v -> Lwt.return (rightmost_value = v)
  end;

  test "transfer_l Empty" begin fun () ->
    let s = filled_sequence () in
    let ts = Lwt_sequence.create () in
    let _ = Lwt_sequence.transfer_l ts s in
    let len = Lwt_sequence.length s in
    Lwt.return (filled_length = len)
  end;

  test "transfer_l " begin fun () ->
    let s = filled_sequence () in
    let ts = transfer_sequence () in
    let _ = Lwt_sequence.transfer_l ts s in
    let len = Lwt_sequence.length s in
    let _ = assert ((filled_length + transfer_length) = len) in
    match Lwt_sequence.take_opt_l s with
    | None -> Lwt.return_false
    | Some v -> Lwt.return (4 = v)
  end;

  test "transfer_r Empty" begin fun () ->
    let s = filled_sequence () in
    let ts = Lwt_sequence.create () in
    let _ = Lwt_sequence.transfer_r ts s in
    let len = Lwt_sequence.length s in
    Lwt.return (filled_length = len)
  end;

  test "transfer_r " begin fun () ->
    let s = filled_sequence () in
    let ts = transfer_sequence () in
    let _ = Lwt_sequence.transfer_r ts s in
    let len = Lwt_sequence.length s in
    let _ = assert ((filled_length + transfer_length) = len) in
    match Lwt_sequence.take_opt_r s with
    | None -> Lwt.return_false
    | Some v -> Lwt.return (5 = v)
  end;
]
