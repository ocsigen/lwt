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

open Lwt

(* +-----------------------------------------------------------------+
   | Notifiers                                                       |
   +-----------------------------------------------------------------+ *)

type notifier = unit React.signal Lwt_sequence.node

let notifiers = Lwt_sequence.create ()

let disable n =
  Lwt_sequence.remove n;
  React.S.stop (Lwt_sequence.get n)

let notify f signal =
  Lwt_sequence.add_l (React.S.map f signal) notifiers

let notify_p f signal =
  Lwt_sequence.add_l (React.S.map (fun x -> Lwt.ignore_result (f x)) signal) notifiers

let notify_s f signal =
  let mutex = Lwt_mutex.create () in
  Lwt_sequence.add_l (React.S.map (fun x -> Lwt.ignore_result (Lwt_mutex.with_lock mutex (fun () -> f x))) signal) notifiers

let always_notify f signal =
  ignore (notify f signal)

let always_notify_p f signal =
  ignore (notify_p f signal)

let always_notify_s f signal =
  ignore (notify_s f signal)

(* +-----------------------------------------------------------------+
   | Lwt-specific utilities                                          |
   +-----------------------------------------------------------------+ *)

let finalise f _ = f ()

let with_finaliser f signal =
  let r = ref () in
  Gc.finalise (finalise f) r;
  React.S.map (fun x -> ignore r; x) signal

let limit ?eq f signal =
  let event_delayed, push_delayed = React.E.create () in
  let limiter = ref (f ()) and stopper = ref None in
  let event_immediate =
    React.E.filter
      (fun x ->
         if state !limiter <> Sleep then begin
           limiter := f ();
           true
         end else begin
           let _ =
             let waiter, wakener = wait () in
             let () =
               match !stopper with
                 | Some wakener' ->
                     stopper := Some wakener;
                     wakeup_exn wakener' Exit
                 | None ->
                     stopper := Some wakener
             in
             lwt () = !limiter <?> waiter in
             stopper := None;
             limiter := f ();
             lwt () = pause () in
             push_delayed x;
             return ()
           in
           false
         end)
      (React.S.changes signal)
  in
  React.S.hold ?eq (React.S.value signal) (React.E.select [event_immediate; event_delayed])

(* +-----------------------------------------------------------------+
   | Signal transofrmations                                          |
   +-----------------------------------------------------------------+ *)

let fmap_s ?eq f initial signal =
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
      (React.S.changes signal)
  in
  let thread = f (React.S.value signal) in
  match poll thread with
    | Some(Some x) ->
        React.S.hold ?eq x (React.E.select [event_immediate; event_delayed])
    | Some None ->
        React.S.hold ?eq initial (React.E.select [event_immediate; event_delayed])
    | None ->
        let _ =
          Lwt_mutex.with_lock mutex
            (fun () ->
               thread >>= function
                 | Some x ->
                     lwt () = pause () in
                     push_delayed x;
                     return ()
                 | None ->
                     return ())
        in
        React.S.hold ?eq initial (React.E.select [event_immediate; event_delayed])

let some x = Some x

let run_s ?eq initial signal =
  fmap_s ?eq (fun thread -> thread >|= some) initial signal

let app_s ?eq signal_f initial signal_x =
  fmap_s ?eq
    (fun (f, x) -> f x >|= some)
    initial
    (React.S.l2 ~eq:(==) (fun f x -> (f, x)) signal_f signal_x)

let map_s ?eq f initial signal =
  fmap_s ?eq (fun x -> f x >|= some) initial signal

let filter_s ?eq f initial signal =
  fmap_s ?eq (fun x -> f x >|= function true -> Some x | false -> None) initial signal

let diff_s f signal =
  let previous = ref (React.S.value signal) in
  Lwt_event.map_s
    (fun x ->
       let y = !previous in
       previous := x;
       f x y)
    (React.S.changes signal)

let sample_s f event signal =
  Lwt_event.map_s (fun x -> f x (React.S.value signal)) event

let accum_s ?eq event_f initial =
  React.S.hold ?eq initial (Lwt_event.accum_s event_f initial)

let fold_s ?eq f acc event =
  React.S.hold ?eq acc (Lwt_event.fold_s f acc event)

let rec rev_fold f acc = function
  | [] ->
      return acc
  | x :: l ->
      lwt acc = rev_fold f acc l in
      f acc x

let merge_s ?eq f acc signals =
  fmap_s ?eq
    (fun l -> rev_fold f acc l >|= some)
    acc
    (React.S.merge (fun l x -> x :: l) [] signals)

let l1_s ?eq f initial s1 =
  fmap_s ?eq (fun x1 -> f x1 >|= some) initial s1

let l2_s ?eq f initial s1 s2 =
  fmap_s ?eq (fun (x1, x2) -> f x1 x2 >|= some) initial (React.S.l2 (fun x1 x2 -> (x1, x2)) s1 s2)

let l3_s ?eq f initial s1 s2 s3 =
  fmap_s ?eq (fun (x1, x2, x3) -> f x1 x2 x3 >|= some) initial (React.S.l3 (fun x1 x2 x3-> (x1, x2, x3)) s1 s2 s3)

let l4_s ?eq f initial s1 s2 s3 s4 =
  fmap_s ?eq (fun (x1, x2, x3, x4) -> f x1 x2 x3 x4 >|= some) initial (React.S.l4 (fun x1 x2 x3 x4-> (x1, x2, x3, x4)) s1 s2 s3 s4)

let l5_s ?eq f initial s1 s2 s3 s4 s5 =
  fmap_s ?eq (fun (x1, x2, x3, x4, x5) -> f x1 x2 x3 x4 x5 >|= some) initial (React.S.l5 (fun x1 x2 x3 x4 x5-> (x1, x2, x3, x4, x5)) s1 s2 s3 s4 s5)

let l6_s ?eq f initial s1 s2 s3 s4 s5 s6 =
  fmap_s ?eq (fun (x1, x2, x3, x4, x5, x6) -> f x1 x2 x3 x4 x5 x6 >|= some) initial (React.S.l6 (fun x1 x2 x3 x4 x5 x6-> (x1, x2, x3, x4, x5, x6)) s1 s2 s3 s4 s5 s6)

(* +-----------------------------------------------------------------+
   | Monadic interface                                               |
   +-----------------------------------------------------------------+ *)

let return = React.S.const
let bind m f = React.S.switch (f (React.S.value m)) (React.E.map f (React.S.changes m))
