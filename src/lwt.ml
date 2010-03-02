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

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

exception Canceled

type +'a t
type -'a u

type cancel_list
  (* Type of a lists of threads, with possibly different types. With
     existential types, it would be:

     {[
       type cancel_list = (exists 'a. 'a t) list
     ]}
  *)

external make_cancel_list : 'a t list -> cancel_list = "%identity"
external cast_cancel_list : cancel_list -> 'a t list = "%identity"

(* Reason for a thread to be a sleeping thread: *)
type sleep_reason =
  | Task
      (* It is a cancealable task *)
  | Wait
      (* It is a thread created with [wait] *)
  | Temp of cancel_list
      (* [Temp threads] is a temporary thread that is meant to be
         later connected to another one. [threads] is the list of
         threads to cancel when this one is cancelled. *)

and 'a thread_state =
  | Return of 'a
      (* [Return v] a terminated thread which has successfully
         terminated with the value [v] *)
  | Fail of exn
      (* [Fail exn] a terminated thread which has failed with the
         exception [exn] *)
  | Sleep of 'a sleeper
      (* [Sleep sleeper] is a sleeping thread *)
  | Repr of 'a thread_repr
      (* [Repr t] a thread which behaves the same as [t] *)

and 'a thread_repr = 'a thread_state ref

and 'a sleeper = {
  reason : sleep_reason;
  (* Reason why the thread is sleeping *)
  mutable waiters : 'a waiter_set;
  (* All thunk functions *)
  mutable removed : int;
  (* Number of waiter that have been disabled. When this number
     reaches [max_removed], they are effectively removed from
     [waiters]. *)
}

(* A waiter which can be removed from its set: *)
and 'a removable = {
  waiter : 'a t -> unit;
  mutable active : bool;
}

(* Type of set of waiters: *)
and 'a waiter_set =
  | Empty
  | Removable of 'a removable
  | Immutable of ('a t -> unit)
  | Append of 'a waiter_set * 'a waiter_set

external thread_repr : 'a t -> 'a thread_repr = "%identity"
external thread : 'a thread_repr -> 'a t = "%identity"
external wakener : 'a thread_repr -> 'a u = "%identity"
external wakener_repr : 'a u -> 'a thread_repr = "%identity"

(* Maximum number of disabled waiters a waiter set can contains before
   being cleaned: *)
let max_removed = 42

(* +-----------------------------------------------------------------+
   | Restarting/connecting threads                                   |
   +-----------------------------------------------------------------+ *)

(* Returns the representative of a thread, updating non-direct references: *)
let rec repr_rec t =
  match !t with
    | Repr t' -> let t'' = repr_rec t' in if t'' != t' then t := Repr t''; t''
    | _       -> t
let repr t = repr_rec (thread_repr t)

let rec run_waiters_rec t ws rem =
  match ws, rem with
    | Empty, [] ->
        ()
    | Empty, ws :: rem ->
        run_waiters_rec t ws rem
    | Immutable f, [] ->
        f t
    | Immutable f, ws :: rem ->
        f t;
        run_waiters_rec t ws rem
    | Removable w, [] ->
        if w.active then w.waiter t
    | Removable w, ws :: rem ->
        if w.active then w.waiter t;
        run_waiters_rec t ws rem
    | Append(ws1, ws2), _ ->
        run_waiters_rec t ws1 (ws2 :: rem)

(* Run all waiters waiting on [t]: *)
let run_waiters waiters t =
  run_waiters_rec (thread t) waiters []

(* Restarts a sleeping thread [t]:

   - run all its waiters
   - set his state to the terminated state [state]
*)
let restart t state caller =
  let t = repr_rec (wakener_repr t) in
  match !t with
    | Sleep{ waiters = waiters } ->
        t := state;
        run_waiters waiters t
    | Fail Canceled ->
        (* Do not fail if the thread has been canceled: *)
        ()
    | _ ->
        invalid_arg caller

let wakeup t v = restart t (Return v) "wakeup"
let wakeup_exn t e = restart t (Fail e) "wakeup_exn"

let rec cancel t =
  match !(repr t) with
    | Sleep{ reason = Task } ->
        wakeup_exn (wakener (thread_repr t)) Canceled
    | Sleep{ reason = Temp l } ->
        List.iter cancel (cast_cancel_list l)
    | _ ->
        ()

let append l1 l2 =
  match l1, l2 with
    | Empty, _ -> l2
    | _, Empty -> l1
    | _ -> Append(l1, l2)

(* Remove all disbaled waiters of a waiter set: *)
let rec cleanup = function
  | Removable{ active = false } ->
      Empty
  | Append(l1, l2) ->
      append (cleanup l1) (cleanup l2)
  | ws ->
      ws

(* Connects the two processes [t1] and [t2] when [t2] finishes up,
   where [t1] must be a sleeping thread.

   Connecting means running all the waiters for [t2] and assigning the
   state of [t2] to [t1].
*)
let rec connect t1 t2 =
  let t1 = repr t1 and t2 = repr t2 in
  match !t1 with
    | Sleep sleeper1 ->
        if t1 == t2 then
          (* Do nothing if the two threads already have the same
             representation *)
          ()
        else begin
          match !t2 with
            | Sleep sleeper2 ->
                (* If [t2] is sleeping, then makes it behave as [t1]: *)
                t2 := Repr t1;
                (* Merge the two sets of waiters: *)
                let waiters = append sleeper1.waiters sleeper2.waiters
                and removed = sleeper1.removed + sleeper2.removed in
                if removed > max_removed then begin
                  (* Remove disabled threads *)
                  sleeper1.removed <- 0;
                  sleeper1.waiters <- cleanup waiters
                end else begin
                  sleeper1.removed <- removed;
                  sleeper1.waiters <- waiters
                end
            | state2 ->
                (* [t2] has already terminated, assing its state to [t1]: *)
                t1 := state2;
                (* and run all the waiters of [t1]: *)
                run_waiters sleeper1.waiters t1
        end
    | _ ->
        (* [t1] is not asleep: *)
        invalid_arg "connect"

(* +-----------------------------------------------------------------+
   | Threads conctruction and combining                              |
   +-----------------------------------------------------------------+ *)

let return v =
  thread (ref (Return v))
let fail e =
  thread (ref (Fail e))
let temp l =
  thread (ref (Sleep{ reason = Temp(make_cancel_list l);
                      waiters = Empty;
                      removed = 0 }))
let wait () =
  let t = ref (Sleep{ reason = Wait;
                      waiters = Empty;
                      removed = 0 }) in
  (thread t, wakener t)
let task () =
  let t = ref (Sleep{ reason = Task;
                      waiters = Empty;
                      removed = 0 }) in
  (thread t, wakener t)

(* apply function, reifying explicit exceptions into the thread type
   apply: ('a -(exn)-> 'b t) -> ('a -(n)-> 'b t)
   semantically a natural transformation TE -> T, where T is the thread
   monad, which is layered over exception monad E.
*)
let apply f x = try f x with e -> fail e

let add_immutable_waiter sleeper waiter =
  sleeper.waiters <- (match sleeper.waiters with
                        | Empty -> Immutable waiter
                        | _ -> Append(Immutable waiter, sleeper.waiters))

let add_removable_waiter sleeper waiter =
  sleeper.waiters <- (match sleeper.waiters with
                        | Empty -> Removable waiter
                        | _ -> Append(Removable waiter, sleeper.waiters))

let on_cancel t f =
  let t = repr t in
  match !t with
    | Sleep sleeper ->
        add_immutable_waiter sleeper
          (fun x ->
             match !(repr x) with
               | Fail Canceled -> (try f () with _ -> ())
               | _ -> ())
    | Fail Canceled ->
        f ()
    | _ ->
        ()

let rec bind t f =
  match !(repr t) with
    | Return v ->
        (* we don't use apply here so that tail recursion is not
           broken *)
        f v
    | Fail e ->
        fail e
    | Sleep sleeper ->
        let res = temp [t] in
        add_immutable_waiter sleeper (fun x -> connect res (bind x (apply f)));
        res
    | Repr _ ->
        assert false
let (>>=) t f = bind t f
let (=<<) f t = bind t f

let map f t = bind t (fun x -> return (f x))
let (>|=) t f = map f t
let (=|<) f t = map f t

let rec catch_rec t f =
  match !(repr t) with
    | Return v ->
        t
    | Fail e ->
        f e
    | Sleep sleeper ->
        let res = temp [t] in
        add_immutable_waiter sleeper (fun x -> connect res (catch_rec x (apply f)));
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
    | Sleep sleeper ->
        let res = temp [t] in
        add_immutable_waiter sleeper (fun x -> connect res (try_bind_rec x (apply f) g));
        res
    | Repr _ ->
        assert false

let try_bind x f g = try_bind_rec (apply x ()) f g

let poll t =
  match !(repr t) with
    | Fail e -> raise e
    | Return v -> Some v
    | Sleep _ -> None
    | Repr _ -> assert false

let rec ignore_result t =
  match !(repr t) with
    | Return v ->
        ()
    | Fail e ->
        raise e
    | Sleep sleeper ->
        add_immutable_waiter sleeper (fun x -> ignore_result x)
    | Repr _ ->
        assert false

let protected t =
  match !(repr t) with
    | Sleep _ ->
        let waiter, wakener = task () in
        ignore (try_bind_rec t
                  (fun value -> (try wakeup wakener value with Invalid_argument _ -> ()); return ())
                  (fun exn -> (try wakeup_exn wakener exn with Invalid_argument _ -> ()); return ()));
        waiter
    | Return _ | Fail _ ->
        t
    | Repr _ ->
        assert false

let rec nth_ready l n =
  match l with
    | [] ->
        assert false
    | x :: rem ->
        let x = repr x in
        match !x with
          | Sleep _ ->
              nth_ready rem n
          | _ when n > 0 ->
              nth_ready rem (n - 1)
          | _ ->
              thread x

let ready_count l =
  List.fold_left (fun acc x -> match !(repr x) with Sleep _ -> acc | _ -> acc + 1) 0 l

let remove_waiters l =
  List.iter
    (fun t ->
       match !(repr t) with
         | Sleep sleeper ->
             let removed = sleeper.removed + 1 in
             if removed > max_removed then begin
               sleeper.removed <- 0;
               sleeper.waiters <- cleanup sleeper.waiters
             end else
               sleeper.removed <- removed
         | _ ->
             ())
    l

let choose l =
  let ready = ready_count l in
  if ready > 0 then
    nth_ready l (Random.int ready)
  else begin
    let res = temp l in
    let rec waiter = { active = true; waiter = handle_result }
    and handle_result t =
      (* Disable the waiter now: *)
      waiter.active <- false;
      (* Removes all waiters so we do not leak memory: *)
      remove_waiters l;
      (* This will not fail because it is called at most one time,
         since all other waiters have been removed: *)
      connect res t
    in
    List.iter
      (fun t ->
         match !(repr t) with
           | Sleep sleeper ->
               add_removable_waiter sleeper waiter;
           | _ ->
               assert false)
      l;
    res
  end

let select l =
  let ready = ready_count l in
  if ready > 0 then
    nth_ready l (Random.int ready)
  else begin
    let res = temp l in
    let rec waiter = { active = true; waiter = handle_result }
    and handle_result t =
      waiter.active <- false;
      remove_waiters l;
      (* Cancel all other threads: *)
      List.iter cancel l;
      connect res t
    in
    List.iter
      (fun t ->
         match !(repr t) with
           | Sleep sleeper ->
               add_removable_waiter sleeper waiter;
           | _ ->
               assert false)
      l;
    res
  end

let join l =
  let res = temp l and sleeping = ref 0 (* Number of threads still sleeping *) in
  let rec waiter = { active = true; waiter = handle_result }
  and handle_result t = match !(repr t) with
    | Fail exn ->
        (* The thread has failed, exit immediatly without waiting for
           other threads *)
        remove_waiters l;
        connect res t
    | _ ->
        decr sleeping;
        (* Every threads has finished, we can wakeup the result: *)
        if !sleeping = 0 then begin
          waiter.active <- false;
          connect res t
        end
  in
  let rec init = function
    | [] ->
        if !sleeping = 0 then
          (* No threads is sleeping, returns immediately: *)
          return ()
        else
          res
    | t :: rest ->
        match !(repr t) with
          | Fail exn ->
              (* One of the thread already failed, remove the waiter
                 from all already visited sleeping threads and
                 fail: *)
              let rec loop = function
                | [] ->
                    t
                | t :: l ->
                    match !(repr t) with
                      | Fail _ ->
                          t
                      | Sleep sleeper ->
                          let removed = sleeper.removed + 1 in
                          if removed > max_removed then begin
                            sleeper.removed <- 0;
                            sleeper.waiters <- cleanup sleeper.waiters
                          end else
                            sleeper.removed <- removed;
                          loop l
                      | _ ->
                          loop l
              in
              waiter.active <- false;
              loop l
          | Sleep sleeper ->
              incr sleeping;
              add_removable_waiter sleeper waiter;
              init rest
          | _ ->
              init rest
  in
  init l

let ( <?> ) t1 t2 = choose [t1; t2]
let ( <&> ) t1 t2 = join [t1; t2]

let finalize f g =
  try_bind f
    (fun x -> g () >>= fun () -> return x)
    (fun e -> g () >>= fun () -> fail e)

module State = struct
  type 'a state =
    | Return of 'a
    | Fail of exn
    | Sleep
end

let state t = match !(repr t) with
  | Return v -> State.Return v
  | Fail exn -> State.Fail exn
  | Sleep _ -> State.Sleep
  | Repr _ -> assert false

include State
