(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_exit_hook
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

open Lwt

type t = {
  mutable prev : t option;
  (* The previous hook, it is either another hook or the beginning of
     the list *)

  mutable next : t option;
  (* The next hook, it is either another hook or the end of the
     list *)

  mutable hook : unit -> unit Lwt.t;
  (* The hook function *)

  mutable enabled : bool;
  (* Wether the node is in the set or is removed *)
}

(* The first registred hook *)
let first = ref None

(* The last registred hook *)
let last = ref None

let get node = node.hook
let set node f = node.hook <- f

let make f = {
  prev = None;
  next = None;
  hook = f;
  enabled = false;
}

let remove node =
  if node.enabled then begin
    begin match node.next with
      | Some next ->
          next.prev <- node.prev
      | None ->
          (* It is the last node *)
          last := node.prev
    end;
    begin match node.prev with
      | Some prev ->
          prev.next <- node.next
      | None ->
          (* It is the first node *)
          first := node.next
    end;
    node.prev <- None;
    node.next <- None;
    node.enabled <- false
  end

let prepend node =
  remove node;
  (* The next node is the first one *)
  node.next <- !first;
  (* Prepend it to the first node *)
  begin match !first with
    | Some first ->
        first.prev <- Some node
    | None ->
        (* There is no others node, so this is also the last one *)
        last := (Some node)
  end;
  (* It is now the first node *)
  first := Some node;
  node.enabled <- true

let append node =
  remove node;
  (* The previous node is the last one *)
  node.prev <- !last;
  (* Append it to the first node *)
  begin match !last with
    | Some last ->
        last.next <- Some node
    | None ->
        (* There is no others node, so this is also the first one *)
        first := (Some node)
  end;
  (* It is now the last node *)
  last := Some node;
  node.enabled <- true

let exec node =
  remove node;
  catch node.hook (fun _ -> return ())

let ensure_termination t =
  if Lwt.state t = Lwt.Sleep then begin
    let hook = make (fun _ -> t) in
    prepend hook;
    (* Remove the hook when t has terminated *)
    ignore (finalize (fun _ -> t) (fun _ -> remove hook; return ()))
  end

let rec call_hooks _ = match !first with
  | None ->
      return ()
  | Some node ->
      exec node >>= call_hooks

let exit_function _ = Lwt_unix.run (call_hooks ())

let _ = at_exit exit_function
