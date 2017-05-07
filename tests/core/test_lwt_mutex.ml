(* Lightweight thread library for OCaml
 * http://www.ocsigen.org/lwt
 *
 * Copyright (C) 2016 Anton Bachin
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

open Lwt.Infix
open Test

let suite = suite "lwt_mutex" [
  (* See https://github.com/ocsigen/lwt/pull/202#issue-123451878. *)
  test "cancel"
    (fun () ->
      let mutex = Lwt_mutex.create () in

      (* Thread 1: take the mutex and wait. *)
      let thread_1_wait, resume_thread_1 = Lwt.wait () in
      let thread_1 = Lwt_mutex.with_lock mutex (fun () -> thread_1_wait) in

      (* Thread 2: block on the mutex. *)
      let thread_2_locked_mutex = ref false in
      let thread_2 =
        Lwt_mutex.lock mutex >|= fun () ->
        thread_2_locked_mutex := true
      in

      (* Cancel thread 2, and make sure it is canceled. *)
      Lwt.cancel thread_2;
      Lwt.catch
        (fun () -> thread_2 >>= fun () -> Lwt.return_false)
        (function
        | Lwt.Canceled -> Lwt.return_true
        | _ -> Lwt.return_false)
      >>= fun thread_2_canceled ->

      (* Thread 1: release the mutex. *)
      Lwt.wakeup resume_thread_1 ();
      thread_1 >>= fun () ->

      (* Thread 3: try to take the mutex. Thread 2 should not have it locked,
         since thread 2 was canceled. *)
      Lwt_mutex.lock mutex >|= fun () ->

      not !thread_2_locked_mutex && thread_2_canceled);

  (* See https://github.com/ocsigen/lwt/pull/202#issuecomment-227092595. *)
  test "cancel while queued by unlock"
    (fun () ->
      let mutex = Lwt_mutex.create () in

      (* Thread 1: take the mutex and wait. *)
      let thread_1_wait, resume_thread_1 = Lwt.wait () in
      let thread_1 = Lwt_mutex.with_lock mutex (fun () -> thread_1_wait) in

      (* Thread 2: block on the mutex, then set a flag and release it. *)
      let thread_2_waiter_executed = ref false in
      let thread_2 =
        Lwt_mutex.lock mutex >|= fun () ->
        thread_2_waiter_executed := true;
        Lwt_mutex.unlock mutex
      in

      (* Thread 3: wrap the wakeup of thread 2 in a wakeup of thread 3. *)
      let top_level_waiter, wake_top_level_waiter = Lwt.wait () in
      let while_waking =
        top_level_waiter >>= fun () ->
        (* Inside thread 3 wakeup. *)

        (* Thread 1: release the mutex. This queues thread 2 using
           wakeup_later inside Lwt_mutex.unlock. *)
        Lwt.wakeup resume_thread_1 ();
        thread_1 >>= fun () ->

        (* Confirm the mutex is now considered locked by thread 2. *)
        let mutex_passed = Lwt_mutex.is_locked mutex in
        (* Confirm thread 2 hasn't executed its bind (well, map). It is
           queued. *)
        let thread_2_was_queued = not !thread_2_waiter_executed in

        (* Try to cancel thread 2. *)
        Lwt.cancel thread_2;

        (* Complete thread 2 and check it has not been canceled. *)
        Lwt.catch
          (fun () -> thread_2 >>= fun () -> Lwt.return_false)
          (function
          | Lwt.Canceled -> Lwt.return_true
          | _ -> Lwt.return_false)
        >|= fun thread_2_canceled ->

        (* Confirm that thread 2 ran, and released the mutex. *)
        mutex_passed &&
          thread_2_was_queued &&
          not thread_2_canceled &&
          !thread_2_waiter_executed &&
          not (Lwt_mutex.is_locked mutex)
      in

      (* Run thread 3.
       * Keep this as wakeup_later to test the issue on 2.3.2 reported in
       * https://github.com/ocsigen/lwt/pull/202
       * See also:
       * https://github.com/ocsigen/lwt/pull/261
       *)
      Lwt.wakeup_later wake_top_level_waiter ();
      while_waking);

  (* See https://github.com/ocsigen/lwt/pull/202 - This is an actual reproducer on 2.3.2 *)
  test "mutex issue"
    (fun () ->
       (* Create and lock the mutex *)
       let mtx = Lwt_mutex.create () in
       let _prelock = Lwt_mutex.lock mtx in

       (* create a task (lock-try-noop-finally-unlock) pending on the lock *)
       let t = Lwt_mutex.with_lock mtx (fun () -> Lwt.return ()) in

       (* create a task to unlock and immediately cancel t *)
       let waiter, wakener = Lwt.task () in
       let _ = Lwt.bind waiter (fun () ->
           let () = Lwt_mutex.unlock mtx in
           let () = Lwt.cancel t in
           Lwt.return ())
       in
       (* run this unlock task with wakeup_later, such that
          Lwt.wakeuping (2.3.2) or Lwt.wakening is set.
          This pushes the unlocked task onto the Lwt.to_wakeup queue.
          The cancel will then change the state of the task
          while it's sitting on the queue, making the ultimate
          ignore_wakeup (2.3.2) a noop.
          This causes the unlock in with_lock to be lost.
       *)
       let () = Lwt.wakeup_later wakener () in
       let r = Lwt.catch (fun () -> t) (fun _ -> Lwt.return ()) in
       Lwt.bind r (fun () ->
         Lwt.return (not (Lwt_mutex.is_locked mtx))));
]
