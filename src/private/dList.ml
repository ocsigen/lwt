(*
 * dList.ml
 * --------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lwt.
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
