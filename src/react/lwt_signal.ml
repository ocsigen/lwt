(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_signal
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

include Lwt_react.S

open Lwt_react
open Lwt

(* +-----------------------------------------------------------------+
   | Notifiers                                                       |
   +-----------------------------------------------------------------+ *)

type notifier = unit React.signal Lwt_sequence.node

let notifiers = Lwt_sequence.create ()

let disable n =
  Lwt_sequence.remove n;
  stop (Lwt_sequence.get n)

let notify f signal =
  Lwt_sequence.add_l (S.map f signal) notifiers

let notify_p f signal =
  Lwt_sequence.add_l (S.map (fun x -> Lwt.ignore_result (f x)) signal) notifiers

let notify_s f signal =
  let mutex = Lwt_mutex.create () in
  Lwt_sequence.add_l (S.map (fun x -> Lwt.ignore_result (Lwt_mutex.with_lock mutex (fun () -> f x))) signal) notifiers

let always_notify f signal =
  ignore (notify f signal)

let always_notify_p f signal =
  ignore (notify_p f signal)

let always_notify_s f signal =
  ignore (notify_s f signal)

(* +-----------------------------------------------------------------+
   | Lwt-specific utilities                                          |
   +-----------------------------------------------------------------+ *)

let delay thread =
  match poll thread with
    | Some signal ->
        let event1, send1 = React.E.create () in
        let event2, send2 = React.E.create () in
        ignore (
          (* If the thread has already terminated, we make a pause to
             prevent the first occurence to be lost *)
          lwt () = pause () in
          send1 (value signal);
          send2 (changes signal);
          React.E.stop event1;
          React.E.stop event2;
          return ()
        );
        React.E.switch event1 event2
    | None ->
        let event1, send1 = React.E.create () in
        let event2, send2 = React.E.create () in
        ignore (
          lwt signal = thread in
          send1 (value signal);
          send2 (changes signal);
          React.E.stop event1;
          React.E.stop event2;
          return ()
        );
        React.E.switch event1 event2

(* +-----------------------------------------------------------------+
   | Signal transofrmations                                          |
   +-----------------------------------------------------------------+ *)

let run_s ?eq i s =
  let event, push = E.create () in
  let mutex = Lwt_mutex.create () in
  let iter = E.fmap (fun t -> on_success (Lwt_mutex.with_lock mutex (fun () -> t)) push; None) (changes s) in
  on_success (Lwt_mutex.with_lock mutex (fun () -> value s)) push;
  hold ?eq i (E.select [iter; event])

let map_s ?eq f i s =
  let event, push = E.create () in
  let mutex = Lwt_mutex.create () in
  let iter = E.fmap (fun x -> on_success (Lwt_mutex.with_lock mutex (fun () -> f x)) push; None) (changes s) in
  on_success (Lwt_mutex.with_lock mutex (fun () -> f (value s))) push;
  hold ?eq i (E.select [iter; event])

let app_s ?eq sf i s =
  let event, push = E.create () in
  let mutex = Lwt_mutex.create () in
  let iter = E.fmap (fun (f, x) -> on_success (Lwt_mutex.with_lock mutex (fun () -> f x)) push; None) (E.app (E.map (fun f x -> (f, x)) (changes sf)) (changes s)) in
  on_success (Lwt_mutex.with_lock mutex (fun () -> (value sf) (value s))) push;
  hold ?eq i (E.select [iter; event])

let filter_s ?eq f i s =
  let event, push = E.create () in
  let mutex = Lwt_mutex.create () in
  let iter = E.fmap (fun x -> on_success (Lwt_mutex.with_lock mutex (fun () -> f x)) (function true -> push x | false -> ()); None) (changes s) in
  let x = value s in
  on_success
    (Lwt_mutex.with_lock mutex (fun () -> f x))
    (function
       | true ->
           push x
       | false ->
           ());
  hold ?eq i (E.select [iter; event])

let fmap_s ?eq f i s =
  let event, push = E.create () in
  let mutex = Lwt_mutex.create () in
  let iter = E.fmap (fun x -> on_success (Lwt_mutex.with_lock mutex (fun () -> f x)) (function Some x -> push x | None -> ()); None) (changes s) in
  on_success
    (Lwt_mutex.with_lock mutex (fun () -> f (value s)))
    (function
       | Some x ->
           push x
       | None ->
           ());
  hold ?eq i (E.select [iter; event])

let rec rev_fold f acc = function
  | [] ->
      return acc
  | x :: l ->
      lwt acc = rev_fold f acc l in
      f acc x

let merge_s ?eq f acc sl =
  let s = merge (fun acc x -> x :: acc) [] sl in
  let event, push = E.create () in
  let mutex = Lwt_mutex.create () in
  let iter = E.fmap (fun l -> on_success (Lwt_mutex.with_lock mutex (fun () -> rev_fold f acc l)) push; None) (changes s) in
  on_success (Lwt_mutex.with_lock mutex (fun () -> rev_fold f acc (value s))) push;
  hold ?eq acc (E.select [iter; event])

let l1_s ?eq f i s1 =
  map_s ?eq f i s1

let l2_s ?eq f i s1 s2 =
  map_s ?eq (fun (x1, x2) -> f x1 x2) i (l2 (fun x1 x2 -> (x1, x2)) s1 s2)

let l3_s ?eq f i s1 s2 s3 =
  map_s ?eq (fun (x1, x2, x3) -> f x1 x2 x3) i (l3 (fun x1 x2 x3-> (x1, x2, x3)) s1 s2 s3)

let l4_s ?eq f i s1 s2 s3 s4 =
  map_s ?eq (fun (x1, x2, x3, x4) -> f x1 x2 x3 x4) i (l4 (fun x1 x2 x3 x4-> (x1, x2, x3, x4)) s1 s2 s3 s4)

let l5_s ?eq f i s1 s2 s3 s4 s5 =
  map_s ?eq (fun (x1, x2, x3, x4, x5) -> f x1 x2 x3 x4 x5) i (l5 (fun x1 x2 x3 x4 x5-> (x1, x2, x3, x4, x5)) s1 s2 s3 s4 s5)

let l6_s ?eq f i s1 s2 s3 s4 s5 s6 =
  map_s ?eq (fun (x1, x2, x3, x4, x5, x6) -> f x1 x2 x3 x4 x5 x6) i (l6 (fun x1 x2 x3 x4 x5 x6-> (x1, x2, x3, x4, x5, x6)) s1 s2 s3 s4 s5 s6)
