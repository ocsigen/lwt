(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_stream
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

exception Empty

(* A node in a queue of pending data. *)
type 'a node = {
  mutable next : 'a node;
  (* Next node in the queue. For the last node it points to itself. *)
  mutable data : 'a option;
  (* Data of this node. For the last node it is always [None]. *)
}

(* Note: a queue for an exhausted stream is represented by a node
   containing [None] followed by a node with itself as next and [None]
   as data. *)

let new_node () =
  let rec node = { next = node; data = None } in
  node

(* Type of a stream source using a function to create new elements. *)
type 'a from = {
  from_create : unit -> 'a option Lwt.t;
  (* Function used to create new elements. *)
  mutable from_thread : unit Lwt.t;
  (* Thread which:

     - wait for the thread returned by the last call to [from_next],
     - add the next element to the end of the queue.

     If it is a sleeping thread, then it must be used instead of creating a
     new one with [from_create]. *)
}

(* Type of a stream source for push streams. *)
type push = {
  mutable push_signal : unit Lwt.t;
  (* Thread signaled when a new element is added to the stream. *)
  mutable push_waiting : bool;
  (* Is a thread waiting on [push_signal] ? *)
}

(* Source of a stream. *)
type 'a source =
  | From of 'a from
  | Push of push

type 'a t = {
  source : 'a source;
  (* The source of the stream. *)
  mutable node : 'a node;
  (* Pointer to first pending element, or to [last] if there is no
     pending element. *)
  last : 'a node ref;
  (* Node marking the end of the queue of pending elements. *)
}

(* The only difference between two clones is the pointer to the first
   pending element. *)
let clone s = {
  source = s.source;
  node = s.node;
  last = s.last;
}

let return0 = return ()

let from f =
  let last = new_node () in
  {
    source = From { from_create = f; from_thread = return0 };
    node = last;
    last = ref last;
  }

let of_list l =
  let l = ref l in
  from (fun () ->
          match !l with
            | [] -> return None
            | x :: l' -> l := l'; return (Some x))

let of_array a =
  let len = Array.length a and i = ref 0 in
  from (fun () ->
          if !i = len then
            return None
          else begin
            let c = Array.unsafe_get a !i in
            incr i;
            return (Some c)
          end)

let of_string s =
  let len = String.length s and i = ref 0 in
  from (fun () ->
          if !i = len then
            return None
          else begin
            let c = String.unsafe_get s !i in
            incr i;
            return (Some c)
          end)

let create () =
  (* Create the cell pointing to the end of the queue. *)
  let last = ref (new_node ()) in
  (* Create the source for notifications of new elements. *)
  let source, wakener_cell =
    let waiter, wakener = wait () in
    ({ push_signal = waiter;
       push_waiting = false },
     ref wakener)
  in
  (* The push function. It does not keep a reference to the stream. *)
  let push x =
    (* Push the element at the end of the queue. *)
    let node = !last and new_last = new_node () in
    node.data <- x;
    node.next <- new_last;
    last := new_last;
    (* Send a signal if at least one thread is waiting for a new
       element. *)
    if source.push_waiting then begin
      source.push_waiting <- false;
      (* Update threads. *)
      let old_wakener = !wakener_cell in
      let new_waiter, new_wakener = wait () in
      source.push_signal <- new_waiter;
      wakener_cell := new_wakener;
      (* Signal that a new value has been received. *)
      wakeup_later old_wakener ()
    end
  in
  ({ source = Push source;
     node = !last;
     last = last },
   push)

(* Wait for a new element to be added to the queue of pending element
   of the stream. *)
let feed s =
  match s.source with
    | From from ->
        (* There is already a thread started to create a new element,
           wait for this one to terminate. *)
        if is_sleeping from.from_thread then
          protected from.from_thread
        else begin
          (* Otherwise request a new element. *)
          let thread =
            lwt x = from.from_create () in
            (* Push the element to the end of the queue. *)
            let node = !(s.last) and new_last = new_node () in
            node.data <- x;
            node.next <- new_last;
            s.last := new_last;
            return ()
          in
          (* Allow other threads to access this thread. *)
          from.from_thread <- thread;
          protected thread
        end
    | Push push ->
        push.push_waiting <- true;
        protected push.push_signal

(* Remove [node] from the top of the queue, or do nothing if it was
   already consumed.

   Precondition: node.data <> None
*)
let consume s node =
  if node == s.node then
    s.node <- node.next

let rec peek_rec s node =
  if node == !(s.last) then
    feed s >> peek_rec s node
  else
    return node.data

let peek s = peek_rec s s.node

let rec npeek_rec node acc n s =
  if n <= 0 then
    return (List.rev acc)
  else if node == !(s.last) then
    feed s >> npeek_rec node acc n s
  else
    match node.data with
      | Some x ->
          npeek_rec node.next (x :: acc) (n - 1) s
      | None ->
          return (List.rev acc)

let npeek n s = npeek_rec s.node [] n s

let rec get_rec s node =
  if node == !(s.last) then
    feed s >> get_rec s node
  else begin
    if node.data <> None then consume s node;
    return node.data
  end

let get s = get_rec s s.node

let rec nget_rec node acc n s =
  if n <= 0 then
    return (List.rev acc)
  else if node == !(s.last) then
    feed s >> nget_rec node acc n s
  else
    match s.node.data with
      | Some x ->
          consume s node;
          nget_rec node.next (x :: acc) (n - 1) s
      | None ->
          return (List.rev acc)

let nget n s = nget_rec s.node [] n s

let rec get_while_rec node acc f s =
  if node == !(s.last) then
    feed s >> get_while_rec node acc f s
  else
    match node.data with
      | Some x ->
          let test = f x in
          if test then begin
            consume s node;
            get_while_rec node.next (x :: acc) f s
          end else
            return (List.rev acc)
      | None ->
          return (List.rev acc)

let get_while f s = get_while_rec s.node [] f s

let rec get_while_s_rec node acc f s =
  if node == !(s.last) then
    feed s >> get_while_s_rec node acc f s
  else
    match node.data with
      | Some x ->
          lwt test = f x in
          if test then begin
            consume s node;
            get_while_s_rec node.next (x :: acc) f s
          end else
            return (List.rev acc)
      | None ->
          return (List.rev acc)

let get_while_s f s = get_while_s_rec s.node [] f s

let rec next_rec s node =
  if node == !(s.last) then
    feed s >> next_rec s node
  else
    match node.data with
      | Some x ->
          consume s node;
          return x
      | None ->
          raise_lwt Empty

let next s = next_rec s s.node

let rec last_new_rec node x s =
  if node == !(s.last) then
    let thread = feed s in
    match state thread with
      | Return _ ->
          last_new_rec node x s
      | Fail exn ->
          raise_lwt exn
      | Sleep ->
          return x
  else
    match node.data with
      | Some x ->
          consume s node;
          last_new_rec node.next x s
      | None ->
          return x

let last_new s =
  let node = s.node in
  if node == !(s.last) then
    let thread = next s in
    match state thread with
      | Return x ->
          last_new_rec node x s
      | _ ->
          thread
  else
    match node.data with
      | Some x ->
          consume s node;
          last_new_rec node.next x s
      | None ->
          raise_lwt Empty

let rec to_list_rec node acc s =
  if node == !(s.last) then
    feed s >> to_list_rec node acc s
  else
    match node.data with
      | Some x ->
          consume s node;
          to_list_rec node.next (x :: acc) s
      | None ->
          return (List.rev acc)

let to_list s = to_list_rec s.node [] s

let rec to_string_rec node buf s =
  if node == !(s.last) then
    feed s >> to_string_rec node buf s
  else
    match node.data with
      | Some x ->
          consume s node;
          Buffer.add_char buf x;
          to_string_rec node.next buf s
      | None ->
          return (Buffer.contents buf)

let to_string s = to_string_rec s.node (Buffer.create 128) s

let junk s =
  let node = s.node in
  if node == !(s.last) then begin
    lwt _ = feed s in
    if node.data <> None then consume s node;
    return ()
  end else begin
    if node.data <> None then s.node <- node.next;
    return ()
  end

let rec njunk_rec node n s =
  if n <= 0 then
    return ()
  else if node == !(s.last) then
    feed s >> njunk_rec node n s
  else
    match node.data with
      | Some _ ->
          consume s node;
          njunk_rec node.next (n - 1) s
      | None ->
          return ()

let njunk n s = njunk_rec s.node n s

let rec junk_while_rec node f s =
  if node == !(s.last) then
    feed s >> junk_while_rec node f s
  else
    match node.data with
      | Some x ->
          let test = f x in
          if test then begin
            consume s node;
            junk_while_rec node.next f s
          end else
            return ()
      | None ->
          return ()

let junk_while f s = junk_while_rec s.node f s

let rec junk_while_s_rec node f s =
  if node == !(s.last) then
    feed s >> junk_while_s_rec node f s
  else
    match node.data with
      | Some x ->
          lwt test = f x in
          if test then begin
            consume s node;
            junk_while_s_rec node.next f s
          end else
            return ()
      | None ->
          return ()

let junk_while_s f s = junk_while_s_rec s.node f s

let rec junk_old_rec node s =
  if node == !(s.last) then
    let thread = feed s in
    match state thread with
      | Return _ ->
          junk_old_rec node s
      | Fail exn ->
          raise_lwt exn
      | Sleep ->
          return ()
  else
    match node.data with
      | Some _ ->
          consume s node;
          junk_old_rec node.next s
      | None ->
          return ()

let junk_old s = junk_old_rec s.node s

let rec get_available_rec node acc s =
  if node == !(s.last) then
    let thread = feed s in
    match state thread with
      | Return _ ->
          get_available_rec node acc s
      | Fail exn ->
          raise exn
      | Sleep ->
          List.rev acc
  else
    match node.data with
      | Some x ->
          consume s node;
          get_available_rec node.next (x :: acc) s
      | None ->
          List.rev acc

let get_available s = get_available_rec s.node [] s

let rec get_available_up_to_rec node acc n s =
  if n <= 0 then
    List.rev acc
  else if node == !(s.last) then
    let thread = feed s in
    match state thread with
      | Return _ ->
          get_available_up_to_rec node acc n s
      | Fail exn ->
          raise exn
      | Sleep ->
          List.rev acc
  else
    match s.node.data with
      | Some x ->
          consume s node;
          get_available_up_to_rec node.next (x :: acc) (n - 1) s
      | None ->
          List.rev acc

let get_available_up_to n s = get_available_up_to_rec s.node [] n s

let rec is_empty s =
  if s.node == !(s.last) then
    feed s >> is_empty s
  else
    return (s.node.data = None)

let map f s =
  from (fun () -> get s >>= function
          | Some x ->
              let x = f x in
              return (Some x)
          | None ->
              return None)

let map_s f s =
  from (fun () -> get s >>= function
          | Some x ->
              lwt x = f x in
              return (Some x)
          | None ->
              return None)

let filter f s =
  let rec next () =
    get s >>= function
      | Some x as result ->
          let test = f x in
          if test then
            return result
          else
            next ()
      | None ->
          return None
  in
  from next

let filter_s f s =
  let rec next () =
    get s >>= function
      | Some x as result ->
          lwt test = f x in
          if test then
            return result
          else
            next ()
      | None ->
          return None
  in
  from next

let filter_map f s =
  let rec next () =
    get s >>= function
      | Some x ->
          let x = f x in
          (match x with
             | Some _ ->
                 return x
             | None ->
                 next ())
      | None ->
          return None
  in
  from next

let filter_map_s f s =
  let rec next () =
    get s >>= function
      | Some x ->
          lwt x = f x in
          (match x with
             | Some _ ->
                 return x
             | None ->
                 next ())
      | None ->
          return None
  in
  from next

let map_list f s =
  let pendings = ref [] in
  let rec next () =
    match !pendings with
      | [] ->
          get s >>= (function
                                | Some x ->
                                    let l = f x in
                                    pendings := l;
                                    next ()
                                | None ->
                                    return None)
      | x :: l ->
          pendings := l;
          return (Some x)
  in
  from next

let map_list_s f s =
  let pendings = ref [] in
  let rec next () =
    match !pendings with
      | [] ->
          get s >>= (function
                       | Some x ->
                           lwt l = f x in
                           pendings := l;
                           next ()
                       | None ->
                           return None)
      | x :: l ->
          pendings := l;
          return (Some x)
  in
  from next

let flatten s =
  map_list (fun l -> l) s

let rec fold_rec node f s acc =
  if node == !(s.last) then
    feed s >> fold_rec node f s acc
  else
    match node.data with
      | Some x ->
          consume s node;
          let acc = f x acc in
          fold_rec node.next f s acc
      | None ->
          return acc

let fold f s acc = fold_rec s.node f s acc

let rec fold_s_rec node f s acc =
  if node == !(s.last) then
    feed s >> fold_s_rec node f s acc
  else
    match node.data with
      | Some x ->
          consume s node;
          lwt acc = f x acc in
          fold_s_rec node.next f s acc
      | None ->
          return acc

let fold_s f s acc = fold_s_rec s.node f s acc

let rec iter_rec node f s =
  if node == !(s.last) then
    feed s >> iter_rec node f s
  else
    match node.data with
      | Some x ->
          consume s node;
          let () = f x in
          iter_rec node.next f s
      | None ->
          return ()

let iter f s = iter_rec s.node f s

let rec iter_s_rec node f s =
  if node == !(s.last) then
    feed s >> iter_s_rec node f s
  else
    match node.data with
      | Some x ->
          consume s node;
          lwt () = f x in
          iter_s_rec node.next f s
      | None ->
          return ()

let iter_s f s = iter_s_rec s.node f s

let rec iter_p_rec node f s =
  if node == !(s.last) then
    feed s >> iter_p_rec node f s
  else
    match node.data with
      | Some x ->
          consume s node;
          f x <&> iter_p_rec node.next f s
      | None ->
          return ()

let iter_p f s = iter_p_rec s.node f s

let rec find_rec node f s =
  if node == !(s.last) then
    feed s >> find_rec node f s
  else
    match node.data with
      | Some x as result ->
          consume s node;
          let test = f x in
          if test then
            return result
          else
            find_rec node.next f s
      | None ->
          return None

let find f s = find_rec s.node f s

let rec find_s_rec node f s =
  if node == !(s.last) then
    feed s >> find_s_rec node f s
  else
    match node.data with
      | Some x as result ->
          consume s node;
          lwt test = f x in
          if test then
            return result
          else
            find_s_rec node.next f s
      | None ->
          return None

let find_s f s = find_s_rec s.node f s

let rec find_map_rec node f s =
  if node == !(s.last) then
    feed s >> find_map_rec node f s
  else
    match node.data with
      | Some x ->
          consume s node;
          let x = f x in
          if x = None then
            find_map_rec node.next f s
          else
            return x
      | None ->
          return None

let find_map f s = find_map_rec s.node f s

let rec find_map_s_rec node f s =
  if node == !(s.last) then
    feed s >> find_map_s_rec node f s
  else
    match node.data with
      | Some x ->
          consume s node;
          lwt x = f x in
          if x = None then
            find_map_s_rec node.next f s
          else
            return x
      | None ->
          return None

let find_map_s f s = find_map_s_rec s.node f s

let rec combine s1 s2 =
  let next () =
    lwt n1 = get s1 and n2 = get s2 in
    match n1, n2 with
      | Some x1, Some x2 ->
          return (Some(x1, x2))
      | _ ->
          return None
  in
  from next

let append s1 s2 =
  let current_s = ref s1 in
  let rec next () =
    get !current_s >>= function
      | Some _ as result ->
          return result
      | None ->
          if !current_s == s2 then
            return None
          else begin
            current_s := s2;
            next ()
          end
  in
  from next

let concat s_top =
  let current_s = ref (from (fun () -> return None)) in
  let rec next () =
    get !current_s >>= function
      | Some _ as result ->
          return result
      | None ->
          get s_top >>= function
            | Some s ->
                current_s := s;
                next ()
            | None ->
                return None
  in
  from next

let choose streams =
  let source s = (s, get s >|= fun x -> (s, x)) in
  let streams = ref (List.map source streams) in
  let rec next () =
    match !streams with
      | [] ->
          return None
      | l ->
          lwt s, x = Lwt.choose (List.map snd l) in
          let l = List.remove_assq s l in
          match x with
            | Some _ ->
                streams := source s :: l;
                return x
            | None ->
                next ()
  in
  from next

let parse s f =
  let node = s.node in
  try_lwt
    f s
  with exn ->
    s.node <- node;
    raise_lwt exn

let hexdump stream =
  let buf = Buffer.create 80 and num = ref 0 in
  from begin fun _ ->
    nget 16 stream >>= function
      | [] ->
          return None
      | l ->
          Buffer.clear buf;
          Printf.bprintf buf "%08x|  " !num;
          num := !num + 16;
          let rec bytes pos = function
            | [] ->
                blanks pos
            | x :: l ->
                if pos = 8 then Buffer.add_char buf ' ';
                Printf.bprintf buf "%02x " (Char.code x);
                bytes (pos + 1) l
          and blanks pos =
            if pos < 16 then begin
              if pos = 8 then
                Buffer.add_string buf "    "
              else
                Buffer.add_string buf "   ";
              blanks (pos + 1)
            end
          in
          bytes 0 l;
          Buffer.add_string buf " |";
          List.iter (fun ch -> Buffer.add_char buf (if ch >= '\x20' && ch <= '\x7e' then ch else '.')) l;
          Buffer.add_char buf '|';
          return (Some(Buffer.contents buf))
  end
