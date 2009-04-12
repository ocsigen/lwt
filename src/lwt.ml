(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt
 * Copyright (C) 2005-2008 Jérôme Vouillon
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

type 'a clist =
    CNil
  | CSingle of 'a
  | CApp of 'a clist * 'a clist

let ccons v l =
  match l with
    CNil -> CSingle v
  | _    -> CApp (CSingle v, l)

let capp l l' =
  match l, l' with
    CNil, _ -> l'
  | _, CNil -> l
  | _       -> CApp (l, l')

let rec citer_rec x l rem =
  match l, rem with
    CNil, [] ->
      ()
  | CNil, l :: rem ->
      citer_rec x l rem
  | CSingle f, [] ->
      f x
  | CSingle f, l :: rem ->
      f x;
      citer_rec x l rem
  | CApp (l, l'), _ ->
      citer_rec x l (l' :: rem)

let citer x l = citer_rec x l []

(* Either a thread ['a t] has terminated, either successfully [Return of 'a] or
 *  unsuccessfully [Fail of exn], or it is sleeping, or it behaves the same
 *  as some representant.
 * A sleeping thread could have several [waiters], which are thunk functions.
 *)
type 'a state =
    Return of 'a
  | Fail of exn
  | Sleep of ('a t -> unit) clist
  | Repr of 'a t  (* implements union-find *)

and 'a t = 'a state ref

let rec repr t =
  match !t with
    | Repr t' -> let t'' = repr t' in if t'' != t' then t := Repr t''; t''
    | _       -> t

(* restart a sleeping thread [t], run all its waiters
 * and running all the waiters, and make the terminating state [st]
 * [caller] is a string that describes the caller
 *)
let restart t st caller =
  let t = repr t in
  match !t with
    | Sleep waiters ->
        t := st;
        citer t waiters
    | _ ->
        invalid_arg caller

(*
 * pre-condition: [!t] is [Sleep _] (i.e., not terminated)
 * [connect t t'] connects the two processes when t' finishes up
 * connecting means: running all the waiters for [t']
 * and assigning the state of [t'] to [t]
 *)
let rec connect t t' =
  let t = repr t and t' = repr t' in
  match !t with
    | Sleep waiters ->
        if t == t' then
          ()
        else begin
          match !t' with
            | Sleep waiters' ->
                t' := Repr t;
                t := Sleep(capp waiters waiters')
            | state' ->
                t := state';
                citer t waiters
        end
    | _ ->
        invalid_arg "connect"

(* similar to [connect t t']; does nothing instead of raising exception when
 * [t] is not asleep
 *)
let rec try_connect t t' =
  let t = repr t and t' = repr t' in
  match !t with
    | Sleep waiters ->
        if t == t' then
          ()
        else begin
          match !t' with
            | Sleep waiters' ->
                t' := Sleep(ccons (fun t' -> try_connect t t') waiters')
            | state' ->
                t := state';
                citer t waiters
        end
    | _ ->
        ()

(* apply function, reifying explicit exceptions into the thread type
 * apply: ('a -(exn)-> 'b t) -> ('a -(n)-> 'b t)
 * semantically a natural transformation TE -> T, where T is the thread
 * monad, which is layered over exception monad E.
 *)
let apply f x = try f x with e -> ref (Fail e)

(****)

let return v = ref (Return v)
let fail e = ref (Fail e)

let wait () = ref (Sleep CNil)
let wakeup t v = restart t (Return v) "wakeup"
let wakeup_exn t e = restart t (Fail e) "wakeup_exn"

let rec bind t f =
  match !(repr t) with
    | Return v ->
        (* we don't use apply here so that tail recursion is not broken *)
        f v
    | Fail e ->
        fail e
    | Sleep waiters ->
        let res = wait () in
        t := Sleep(ccons (fun x -> connect res (bind x (apply f))) waiters);
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
        t := Sleep(ccons (fun x -> connect res (catch_rec x (apply f))) waiters);
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
        t := Sleep(ccons (fun x -> connect res (try_bind_rec x (apply f) g)) waiters);
        res
    | Repr _ ->
        assert false

let try_bind x f = try_bind_rec (apply x ()) f

let poll t =
  match !(repr t) with
    Fail e   -> raise e
  | Return v -> Some v
  | Sleep _  -> None
  | Repr _   -> assert false

let rec ignore_result t =
  match !(repr t) with
    Return v ->
      ()
  | Fail e ->
      raise e
  | Sleep waiters ->
      t := Sleep(ccons (fun x -> ignore_result x) waiters)
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

let choose l =
  let ready = List.fold_left (fun acc x -> match !x with Sleep _ -> acc | _ -> acc + 1) 0 l in
  if ready > 0 then
    nth_ready l (Random.int ready)
  else
    let res = wait () in
    (* XXX We may leak memory here, if we repeatedly select the same event *)
    List.iter (fun x -> try_connect res x) l;
    res

let finalize f g =
  try_bind f
    (fun x -> g () >>= fun () -> return x)
    (fun e -> g () >>= fun () -> fail e)
