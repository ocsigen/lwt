(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_dlist
 * Copyright (C) 2009 Jérémie Dimino
 * Laboratoire PPS - CNRS Université Paris Diderot
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

type 'a node = {
  mutable prev : 'a node;
  data : 'a;
  mutable next : 'a node;
}

let make data =
  let rec node = { prev = node; data = data; next = node } in
  node

let data n = n.data
let next n = n.next
let prev n = n.prev

let remove node =
  node.prev.next <- node.next;
  node.next.prev <- node.prev;
  node.next <- node;
  node.prev <- node

let junk node =
  node.prev.next <- node.next;
  node.next.prev <- node.prev

let prepend_data data node =
  let n = { prev = node.prev; data = data; next = node } in
  node.prev.next <- n;
  node.prev <- n;
  n

let append_data data node =
  let n = { prev = node; data = data; next = node.next } in
  node.next.prev <- n;
  node.next <- n;
  n

let link a b =
  a.next.prev <- b.prev;
  b.prev.next <- a.next;
  a.next <- b;
  b.prev <- a

let iter f node =
  let rec loop n =
    if n != node then begin
      f n.data;
      loop n.next
    end
  in
  f node.data;
  loop node.next

let is_alone node = node.prev == node.next

let count node =
  let rec loop acc n =
    if n == node then
      acc
    else
      loop (acc + 1) n.next
  in
  loop 1 node.next
