(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_event
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

type notifier = unit React.event Lwt_sequence.node

let notifiers = Lwt_sequence.create ()

let disable n =
  Lwt_sequence.remove n;
  React.E.stop (Lwt_sequence.get n)

let notify f event =
  Lwt_sequence.add_l (React.E.map f event) notifiers

let notify_p f event =
  Lwt_sequence.add_l (React.E.map (fun x -> Lwt.ignore_result (f x)) event) notifiers

let notify_s f event =
  let mutex = Lwt_mutex.create () in
  Lwt_sequence.add_l (React.E.map (fun x -> Lwt.ignore_result (Lwt_mutex.with_lock mutex (fun () -> f x))) event) notifiers

let always_notify f event =
  ignore (notify f event)

let always_notify_p f event =
  ignore (notify_p f event)

let always_notify_s f event =
  ignore (notify_s f event)

let _disable n _ = disable n

let next ev =
  let waiter, wakener = Lwt.task () in
  let stop = ref ignore in
  let notifier = notify
    (fun x ->
       !stop ();
       Lwt.wakeup wakener x)
    (React.E.once ev)
  in
  stop := _disable notifier;
  Lwt.on_cancel waiter (_disable notifier);
  Gc.finalise (_disable notifier) waiter;
  waiter

let from f =
  let quit_waiter, quit_wakener = Lwt.wait () in
  let event, send = React.E.create () in
  let rec loop () =
    Lwt.select [quit_waiter; f () >|= (fun x -> `Value x)] >>= function
      | `Quit ->
          return ()
      | `Value x ->
          send x;
          loop ()
  in
  ignore (loop ());
  let stop = lazy(React.E.stop event; Lwt.wakeup quit_wakener `Quit) in
  (object
     method stop = Lazy.force stop
     method event = event
   end)

let limit f event =
  let event1, push1 = React.E.create () in
  let sleep = ref (return ()) and stop = ref None in
  let event2 =
    React.E.filter
      (fun x ->
         if state !sleep <> Sleep then begin
           sleep := f ();
           true
         end else begin
           let _ =
             let waiter, wakener = wait () in
             let () =
               match !stop with
                 | Some wakener' ->
                     stop := Some wakener;
                     wakeup_exn wakener' Exit
                 | None ->
                     stop := Some wakener
             in
             lwt () = !sleep <?> waiter in
             stop := None;
             sleep := f ();
             push1 x;
             return ()
           in
           false
         end)
      event
  in
  React.E.select [event1; event2]

module EQueue :
sig
  type 'a t
  val create : 'a React.event -> 'a t
  val pop : 'a t -> 'a option Lwt.t
end =
struct

  type 'a state =
    | No_mail
    | Waiting of 'a option Lwt.u
    | Full of 'a Queue.t

  type 'a t = {
    mutable state : 'a state;
    mutable event : unit React.event;
    (* field used to prevent garbage collection *)
  }

  let create event =
    let box = { state = No_mail; event = React.E.never } in
    let push v =
      match box.state with
	| No_mail ->
	    let q = Queue.create () in
	    Queue.push v q;
	    box.state <- Full q
	| Waiting wakener ->
            box.state <- No_mail;
            wakeup wakener (Some v)
	| Full q ->
	    Queue.push v q
    in
    box.event <- React.E.map push event;
    box

  let pop b = match b.state with
    | No_mail ->
	let waiter, wakener = wait () in
	b.state <- Waiting wakener;
	waiter
    | Waiting _ ->
        (* Calls to next are serialized, so this case will never
           happened *)
	assert false
    | Full q ->
	let v = Queue.take q in
	if Queue.is_empty q then b.state <- No_mail;
        return (Some v)
end

let to_stream event =
  let box = EQueue.create event in
  Lwt_stream.from (fun () -> EQueue.pop box)
