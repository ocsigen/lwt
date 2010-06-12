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

(* +-----------------------------------------------------------------+
   | Notifiers                                                       |
   +-----------------------------------------------------------------+ *)

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

(* +-----------------------------------------------------------------+
   | Lwt-specific utilities                                          |
   +-----------------------------------------------------------------+ *)

let finalise f _ = f ()

let with_finaliser f event =
  let r = ref () in
  Gc.finalise (finalise f) r;
  React.E.map (fun x -> ignore r; x) event

let stop_next notifier _ = disable notifier

let next ev =
  let waiter, wakener = Lwt.task () in
  let stop = ref ignore in
  let notifier = notify
    (fun x ->
       !stop ();
       Lwt.wakeup wakener x)
    (React.E.once ev)
  in
  stop := stop_next notifier;
  Lwt.on_cancel waiter (stop_next notifier);
  Gc.finalise (stop_next notifier) waiter;
  waiter

let limit f event =
  (* Event for delayed delivering when the limiter is sleeping *)
  let event_delayed, push_delayed = React.E.create () in

  (* [limiter] is a thread which prevent [event] to be delivered while
     it is sleeping *)
  let limiter = ref (return ())

  (* [stopper] is a thread which is wakeup if an event is received
     before [limiter] returns *)
  and stopper = ref None in
  let event_immediate =
    React.E.filter
      (fun x ->
         if state !limiter <> Sleep then begin
           (* Limit for futurer events: *)
           limiter := f ();
           (* The limiter is not sleeping, we can deliver the event
              right now: *)
           true
         end else begin
           (* The limiter is sleeping, we have to wait for its
              termination before delivering the event *)
           let _ =
             let waiter, wakener = wait () in
             let () =
               match !stopper with
                 | Some wakener' ->
                     stopper := Some wakener;
                     (* If an event is already queued, drop it: *)
                     wakeup_exn wakener' Exit
                 | None ->
                     stopper := Some wakener
             in
             lwt () = !limiter <?> waiter in
             (* If we reach this point, the limiter has already
                terminated *)
             stopper := None;
             limiter := f ();
             (* Make a pause to be sure we are not in an update cycle
                of [event]: *)
             lwt () = pause () in

             push_delayed x;
             return ()
           in
           false
         end)
      event
  in
  React.E.select [event_immediate; event_delayed]

let stop_from wakener () =
  wakeup wakener None

let from f =
  let event, push = React.E.create () in
  let abort_waiter, abort_wakener = Lwt.wait () in
  let rec loop () =
    pick [f () >|= (fun x -> Some x); abort_waiter] >>= function
      | Some v ->
          push v;
          loop ()
      | None ->
          React.E.stop event;
          return ()
  in
  ignore_result (pause () >>= loop);
  with_finaliser (stop_from abort_wakener) event

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
	let waiter, wakener = task () in
        Lwt.on_cancel waiter (fun () -> b.state <- No_mail);
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

let stop_stream wakener () =
  wakeup wakener None

let of_stream stream =
  let event, push = React.E.create () in
  let abort_waiter, abort_wakener = Lwt.wait () in
  let rec loop () =
    pick [Lwt_stream.get stream; abort_waiter] >>= function
      | Some value ->
          push value;
          loop ()
      | None ->
          React.E.stop event;
          return ()
  in
  ignore_result (pause () >>= loop);
  with_finaliser (stop_stream abort_wakener) event

(* +-----------------------------------------------------------------+
   | Event transofrmations                                           |
   +-----------------------------------------------------------------+ *)

let fmap_s f event =
  let event_delayed, push_delayed = React.E.create () in
  let mutex = Lwt_mutex.create () in
  let event_immediate =
    React.E.fmap
      (fun x ->
         if Lwt_mutex.is_locked mutex then begin
           let _ =
             Lwt_mutex.with_lock mutex
               (fun () ->
                  f x >>= function
                    | Some x ->
                        lwt () = pause () in
                        push_delayed x;
                        return ()
                    | None ->
                        return ())
           in
           None
         end else begin
           let _ = Lwt_mutex.lock mutex in
           try
             let thread = f x in
             match poll thread with
               | Some(Some _ as opt) ->
                   Lwt_mutex.unlock mutex;
                   opt
               | Some None ->
                   None
               | None ->
                   let _ =
                     try_lwt
                       thread >>= function
                         | Some x ->
                             lwt () = pause () in
                             push_delayed x;
                             return ()
                         | None ->
                             return ()
                     finally
                       Lwt_mutex.unlock mutex;
                       return ()
                   in
                   None
           with exn ->
             Lwt_mutex.unlock mutex;
             raise exn
         end)
      event
  in
  React.E.select [event_immediate; event_delayed]

let fmap_p f event =
  let event_delayed, push_delayed = React.E.create () in
  let event_immediate =
    React.E.fmap
      (fun x ->
         let thread = f x in
         match poll thread with
           | Some(Some _ as opt) ->
               opt
           | Some None ->
               None
           | None ->
               let _ =
                 thread >>= function
                   | Some x ->
                       lwt () = pause () in
                       push_delayed x;
                       return ()
                   | None ->
                       return ()
               in
               None)
      event
  in
  React.E.select [event_immediate; event_delayed]

let some x = Some x

let run_s event = fmap_s (fun thread -> thread >|= some) event
let run_p event = fmap_p (fun thread -> thread >|= some) event

let map_s f event = fmap_s (fun x -> f x >|= some) event
let map_p f event = fmap_p (fun x -> f x >|= some) event

let app_s event_f event_x = fmap_s (fun (f, x) -> f x >|= some) (React.E.app (React.E.map (fun f x -> (f, x)) event_f) event_x)
let app_p event_f event_x = fmap_p (fun (f, x) -> f x >|= some) (React.E.app (React.E.map (fun f x -> (f, x)) event_f) event_x)

let filter_s f event = fmap_s (fun x -> f x >|= function true -> Some x | false -> None) event
let filter_p f event = fmap_p (fun x -> f x >|= function true -> Some x | false -> None) event

let diff_s f event =
  let previous = ref None in
  fmap_s
    (fun x ->
       match !previous with
         | None ->
             previous := Some x;
             return None
         | Some y ->
             previous := Some x;
             f x y >|= some)
    event

let accum_s event_f acc =
  let acc = ref acc in
  fmap_s (fun f -> f !acc >|= fun x -> acc := x; Some x) event_f

let fold_s f acc event =
  let acc = ref acc in
  fmap_s (fun x -> f !acc x >|= fun x -> acc := x; Some x) event

let rec rev_fold f acc = function
  | [] ->
      return acc
  | x :: l ->
      lwt acc = rev_fold f acc l in
      f acc x

let merge_s f acc events =
  fmap_s
    (fun l -> rev_fold f acc l >|= some)
    (React.E.merge (fun acc x -> x :: acc) [] events)
