(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt
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

type 'a state =
  | Return of 'a
      (* [Return v] a terminated thread which has successfully
         terminated with the value [v] *)
  | Fail of exn
      (* [Fail exn] a terminated thread which has failed with the
         exception [exn] *)
  | Sleep of 'a node
      (* A sleeping thread with its list of waiters, which are thunk
         functions.

         Since doubly-linked lists cannot be empty the first node
         always contains garbage (= [ignore]) *)
  | Repr of 'a t
      (* [Repr t] a thread which behaves the same as [t] *)

and 'a t = 'a state ref

(* The list of waiters is represented by a doubly-linked list because
   we want the following operations:

   - remove one element (for [choose])
   - concatenate two lists
   - iterate over one list *)
and 'a node = {
  mutable prev : 'a node;
  func : 'a t -> unit;
  mutable next : 'a node;
}

(* Returns the represent of a thread, updating non-direct references: *)
let rec repr t =
  match !t with
    | Repr t' -> let t'' = repr t' in if t'' != t' then t := Repr t''; t''
    | _       -> t

(* Runs all waiters connected to the given node, skipping the first
   node itself since it contains garbage: *)
let run_waiters node t =
  let rec loop n =
    if n != node then begin
      n.func t;
      loop n.next
    end
  in
  loop node.next

(* Creates a new waiter node containing the thunk function [f] and
   connect it before [node]. *)
let new_waiter node f =
  let n = { next = node; func = f; prev = node.prev } in
  node.prev.next <- n;
  node.prev <- n;
  n

let add_waiter node f = ignore (new_waiter node f)

(* Restarts a sleeping thread [t]:

   - run all its waiters
   - set his state to the terminated state [state]
*)
let restart t state caller =
  let t = repr t in
  match !t with
    | Sleep waiters ->
        t := state;
        run_waiters waiters t
    | _ ->
        invalid_arg caller

let wakeup t v = restart t (Return v) "wakeup"
let wakeup_exn t e = restart t (Fail e) "wakeup_exn"

(* Connects the two processes [t1] and [t2] when [t2] finishes up,
   where [t1] must be a sleeping thread.

   Connecting means running all the waiters for [t2] and assigning the
   state of [t2] to [t1].
*)
let rec connect t1 t2 =
  let t1 = repr t1 and t2 = repr t2 in
  match !t1 with
    | Sleep waiters1 ->
        if t1 == t2 then
          (* Do nothing if the two threads already have the same
             representation *)
          ()
        else begin
          match !t2 with
            | Sleep waiters2 ->
                (* If [t2] is sleeping, then makes it behave as [t1]: *)
                t2 := Repr t1;
                (* and add all its waiters to the waiters of [t1] (the
                   first node of [waiters2] is skipped since it contains
                   garbage): *)
                waiters1.prev.next <- waiters2.next;
                waiters2.next.prev <- waiters1.prev;
                waiters1.prev <- waiters2.prev;
                waiters2.prev.next <- waiters1
            | state2 ->
                (* [t2] has already terminated, assing its state to [t1]: *)
                t1 := state2;
                (* and run all the waiters of [t1]: *)
                run_waiters waiters1 t1
        end
    | _ ->
        (* [t1] is not asleep: *)
        invalid_arg "connect"

(* apply function, reifying explicit exceptions into the thread type
   apply: ('a -(exn)-> 'b t) -> ('a -(n)-> 'b t)
   semantically a natural transformation TE -> T, where T is the thread
   monad, which is layered over exception monad E.
*)
let apply f x = try f x with e -> ref (Fail e)

let return v = ref (Return v)
let fail e = ref (Fail e)

let wait () =
  let rec n = { prev = n; func = ignore; next = n } in
  ref (Sleep n)

let rec bind t f =
  match !(repr t) with
    | Return v ->
        (* we don't use apply here so that tail recursion is not
           broken *)
        f v
    | Fail e ->
        fail e
    | Sleep waiters ->
        let res = wait () in
        add_waiter waiters (fun x -> connect res (bind x (apply f)));
        res
    | Repr _ ->
        assert false
let (>>=) = bind

let rec catch_rec t f =
  match !(repr t) with
    | Return v ->
        t
    | Fail e ->
        f e
    | Sleep waiters ->
        let res = wait () in
        add_waiter waiters (fun x -> connect res (catch_rec x (apply f)));
        res
    | Repr _ ->
        assert false

let catch x f = catch_rec (apply x ()) f

let rec try_bind_rec t f g =
  match !(repr t) with
    | Return v ->
        f v
    | Fail e ->
        apply g e
    | Sleep waiters ->
        let res = wait () in
        add_waiter waiters (fun x -> connect res (try_bind_rec x (apply f) g));
        res
    | Repr _ ->
        assert false

let try_bind x f = try_bind_rec (apply x ()) f

let poll t =
  match !(repr t) with
    | Fail e   -> raise e
    | Return v -> Some v
    | Sleep _  -> None
    | Repr _   -> assert false

let rec ignore_result t =
  match !(repr t) with
    | Return v ->
        ()
    | Fail e ->
        raise e
    | Sleep waiters ->
        add_waiter waiters (fun x -> ignore_result x)
    | Repr _ ->
        assert false

let rec nth_ready l n =
  match l with
      [] ->
        assert false
    | x :: rem ->
        let x = repr x in
        match !x with
          | Sleep _ ->
              nth_ready rem n
          | _ when n > 0 ->
              nth_ready rem (n - 1)
          | _ ->
              x

(* Removes a node from any doubly-linked list it is part of. The node
   itself must bot be reused after that: *)
let junk_waiter node =
  node.prev.next <- node.next;
  node.next.prev <- node.prev

let choose l =
  let ready = List.fold_left (fun acc x -> match !(repr x) with Sleep _ -> acc | _ -> acc + 1) 0 l in
  if ready > 0 then
    nth_ready l (Random.int ready)
  else
    let res = wait () in
    (* The list of nodes used for the waiter: *)
    let nodes = ref [] in
    let clear t =
      (* Removes all nodes so we do not leak memory: *)
      List.iter junk_waiter !nodes;
      (* This will not fail because it is called at most one time,
         since all other waiters have been removed: *)
      connect res t
    in
    List.iter begin fun t ->
      match !(repr t) with
        | Sleep waiters ->
            nodes := new_waiter waiters clear :: !nodes
        | _ ->
            assert false
    end l;
    res

let finalize f g =
  try_bind f
    (fun x -> g () >>= fun () -> return x)
    (fun e -> g () >>= fun () -> fail e)
