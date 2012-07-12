(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_list
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

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let rec iter_s f l =
  match l with
    | [] ->
        Lwt.return_unit
    | x :: l ->
        f x >>= fun () ->
        iter_s f l

let rec iter_p f l =
  match l with
    | [] ->
        Lwt.return_unit
    | x :: l ->
        let tx = f x and tl = iter_p f l in
        tx >>= fun () -> tl

let rec map_s f l =
  match l with
    | [] ->
        Lwt.return_nil
    | x :: l ->
        f x >>= fun x ->
        map_s f l >|= fun l ->
        x :: l

let rec map_p f l =
  match l with
    | [] ->
        Lwt.return_nil
    | x :: l ->
        let tx = f x and tl = map_p f l in
        tx >>= fun x ->
        tl >|= fun l ->
        x :: l

let rec rev_map_append_s acc f l =
  match l with
    | [] ->
        Lwt.return acc
    | x :: l ->
        f x >>= fun x ->
        rev_map_append_s (x :: acc) f l

let rev_map_s f l =
  rev_map_append_s [] f l

let rec rev_map_append_p acc f l =
  match l with
    | [] ->
        acc
    | x :: l ->
        rev_map_append_p
          (f x >>= fun x ->
           acc >|= fun l ->
           x :: l) f l

let rev_map_p f l =
  rev_map_append_p Lwt.return_nil f l

let rec fold_left_s f acc l =
  match l with
    | [] ->
        Lwt.return acc
    | x :: l ->
        f acc x >>= fun acc ->
        fold_left_s f acc l

let rec fold_right_s f l acc =
  match l with
    | [] ->
        Lwt.return acc
    | x :: l ->
        fold_right_s f l acc >>= fun acc ->
        f x acc

let rec for_all_s f l =
  match l with
    | [] ->
        Lwt.return_true
    | x :: l ->
        f x >>= function
          | true ->
              for_all_s f l
          | false ->
              Lwt.return_false

let rec for_all_p f l =
  match l with
    | [] ->
        Lwt.return_true
    | x :: l ->
        let tx = f x and tl = for_all_p f l in
        tx >>= fun bx ->
        tl >|= fun bl ->
        bx && bl

let rec exists_s f l =
  match l with
    | [] ->
        Lwt.return_false
    | x :: l ->
        f x >>= function
          | true ->
              Lwt.return_true
          | false ->
              exists_s f l

let rec exists_p f l =
  match l with
    | [] ->
        Lwt.return_false
    | x :: l ->
        let tx = f x and tl = exists_p f l in
        tx >>= fun bx ->
        tl >|= fun bl ->
        bx || bl

let rec find_s f l =
  match l with
    | [] ->
        Lwt.fail Not_found
    | x :: l ->
        f x >>= function
          | true ->
              Lwt.return x
          | false ->
              find_s f l

let rec filter_s f l =
  match l with
    | [] ->
        Lwt.return_nil
    | x :: l ->
        f x >>= function
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
        let tx = f x and tl = filter_p f l in
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
        f x >>= fun bx ->
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
        let tx = f x and tl = partition_p f l in
        tx >>= fun bx ->
        tl >|= fun (l_l, l_r) ->
        if bx then
          (x :: l_l, l_r)
        else
          (l_l, x :: l_r)
