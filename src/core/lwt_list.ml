(* OCaml promise library
 * http://www.ocsigen.org/lwt
 * Copyright (C) 2010 Jérémie Dimino
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

(* A survey and measurements of more optimized implementations can be found at:

    https://jsthomas.github.io/map-comparison.html

   See discussion in https://github.com/ocsigen/lwt/pull/347. *)
let tail_recursive_map f l =
  List.rev (List.rev_map f l)

let tail_recursive_mapi f l =
  let rec inner acc i = function
    | [] -> List.rev acc
    | hd::tl -> (inner [@ocaml.tailcall]) ((f i hd)::acc) (i + 1) tl
  in
  inner [] 0 l

open Lwt.Infix

let rec iter_s f l =
  match l with
  | [] ->
    Lwt.return_unit
  | x :: l ->
    Lwt.apply f x >>= fun () ->
    iter_s f l

let iter_p f l =
  let ts = tail_recursive_map (Lwt.apply f) l in
  Lwt.join ts

let rec iteri_s i f l =
  match l with
  | [] ->
    Lwt.return_unit
  | x :: l ->
    Lwt.apply (f i) x >>= fun () ->
    iteri_s (i + 1) f l

let iteri_s f l = iteri_s 0 f l

let iteri_p f l =
  let f' i = Lwt.apply (f i) in
  let ts = tail_recursive_mapi f' l in
  Lwt.join ts

let map_s f l =
  let rec inner acc = function
    | [] -> List.rev acc |> Lwt.return
    | hd::tl ->
      Lwt.apply f hd >>= fun r ->
      (inner [@ocaml.tailcall]) (r::acc) tl
  in
  inner [] l

let rec _collect acc = function
  | [] ->
    List.rev acc |> Lwt.return
  | t::ts ->
    t >>= fun i ->
    (_collect [@ocaml.tailcall]) (i::acc) ts

let map_p f l =
  let ts = tail_recursive_map (Lwt.apply f) l in
  _collect [] ts

let rec filter_map_s f l =
  match l with
  | [] ->
    Lwt.return_nil
  | x :: l ->
    Lwt.apply f x >>= function
    | Some x ->
      filter_map_s f l >|= fun l ->
      x :: l
    | None ->
      filter_map_s f l

let rec filter_map_p f l =
  match l with
  | [] ->
    Lwt.return_nil
  | x :: l ->
    let tx = Lwt.apply f x and tl = filter_map_p f l in
    tx >>= function
    | Some x -> tl >|= fun l -> x :: l
    | None -> tl

let rec mapi_s i f l =
  match l with
  | [] ->
    Lwt.return_nil
  | x :: l ->
    Lwt.apply (f i) x >>= fun x ->
    mapi_s (i + 1) f l >|= fun l ->
    x :: l

let mapi_s f l = mapi_s 0 f l

let mapi_p f l =
  let f' i = Lwt.apply (f i) in
  let ts = tail_recursive_mapi f' l in
  _collect [] ts

let rec rev_map_append_s acc f l =
  match l with
  | [] ->
    Lwt.return acc
  | x :: l ->
    Lwt.apply f x >>= fun x ->
    rev_map_append_s (x :: acc) f l

let rev_map_s f l =
  rev_map_append_s [] f l

let rec rev_map_append_p acc f l =
  match l with
  | [] ->
    acc
  | x :: l ->
    rev_map_append_p
      (Lwt.apply f x >>= fun x ->
       acc >|= fun l ->
       x :: l) f l

let rev_map_p f l =
  rev_map_append_p Lwt.return_nil f l

let rec fold_left_s f acc l =
  match l with
  | [] ->
    Lwt.return acc
  | x :: l ->
    Lwt.apply (f acc) x >>= fun acc ->
    fold_left_s f acc l

let rec fold_right_s f l acc =
  match l with
  | [] ->
    Lwt.return acc
  | x :: l ->
    fold_right_s f l acc >>= fun acc ->
    Lwt.apply (f x) acc

let rec for_all_s f l =
  match l with
  | [] ->
    Lwt.return_true
  | x :: l ->
    Lwt.apply f x >>= function
    | true ->
      for_all_s f l
    | false ->
      Lwt.return_false

let rec for_all_p f l =
  match l with
  | [] ->
    Lwt.return_true
  | x :: l ->
    let tx = Lwt.apply f x and tl = for_all_p f l in
    tx >>= fun bx ->
    tl >|= fun bl ->
    bx && bl

let rec exists_s f l =
  match l with
  | [] ->
    Lwt.return_false
  | x :: l ->
    Lwt.apply f x >>= function
    | true ->
      Lwt.return_true
    | false ->
      exists_s f l

let rec exists_p f l =
  match l with
  | [] ->
    Lwt.return_false
  | x :: l ->
    let tx = Lwt.apply f x and tl = exists_p f l in
    tx >>= fun bx ->
    tl >|= fun bl ->
    bx || bl

let rec find_s f l =
  match l with
  | [] ->
    Lwt.fail Not_found
  | x :: l ->
    Lwt.apply f x >>= function
    | true ->
      Lwt.return x
    | false ->
      find_s f l

let rec filter_s f l =
  match l with
  | [] ->
    Lwt.return_nil
  | x :: l ->
    Lwt.apply f x >>= function
    | true ->
      filter_s f l >|= fun l ->
      x :: l
    | false ->
      filter_s f l

let rec filter_p f l =
  match l with
  | [] ->
    Lwt.return_nil
  | x :: l ->
    let tx = Lwt.apply f x and tl = filter_p f l in
    tx >>= fun bx ->
    tl >|= fun l ->
    if bx then
      x :: l
    else
      l

let return_nil_nil = Lwt.return ([], [])

let rec partition_s f l =
  match l with
  | [] ->
    return_nil_nil
  | x :: l ->
    Lwt.apply f x >>= fun bx ->
    partition_s f l >|= fun (l_l, l_r) ->
    if bx then
      (x :: l_l, l_r)
    else
      (l_l, x :: l_r)

let rec partition_p f l =
  match l with
  | [] ->
    return_nil_nil
  | x :: l ->
    let tx = Lwt.apply f x and tl = partition_p f l in
    tx >>= fun bx ->
    tl >|= fun (l_l, l_r) ->
    if bx then
      (x :: l_l, l_r)
    else
      (l_l, x :: l_r)
