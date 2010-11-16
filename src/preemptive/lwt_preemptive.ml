(* Ocsigen
 * http://www.ocsigen.org
 * Module lwt_preemptive.ml
 * Copyright (C) 2005 Nataliya Guts, Vincent Balat, Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *               2009 Jérémie Dimino
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exceptions;
 * either version 2.1 of the License, or (at your option) any later version.
 * See COPYING file for details.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

let section = Lwt_log.Section.make "lwt(preemptive)"

open Lwt
open Lwt_io

(* +-----------------------------------------------------------------+
   | Parameters                                                      |
   +-----------------------------------------------------------------+ *)

(* Minimum number of preemptive threads: *)
let min_threads : int ref = ref 0

(* Maximum number of preemptive threads: *)
let max_threads : int ref = ref 0

(* Size of the waiting queue: *)
let max_thread_queued = ref 1000

let get_max_number_of_threads_queued _ =
  !max_thread_queued

let set_max_number_of_threads_queued n =
  if n < 0 then invalid_arg "Lwt_preemptive.set_max_number_of_threads_queued";
  max_thread_queued := n

(* The function for logging errors: *)
let error_log = ref (fun msg -> ignore (Lwt_log.error ~section msg))

(* The total number of preemptive threads currently running: *)
let threads_count = ref 0

(* +-----------------------------------------------------------------+
   | Preemptive threads management                                   |
   +-----------------------------------------------------------------+ *)

type thread = {
  task_channel: (int * (unit -> unit)) Event.channel;
  (* Channel used to communicate notification id and tasks to the
     worker thread. *)

  mutable thread : Thread.t;
  (* The worker thread. *)

  mutable reuse : bool;
  (* Whether the thread must be readded to the pool when the work is
     done. *)
}

(* Pool of worker threads: *)
let workers : thread Queue.t = Queue.create ()

(* Queue of clients waiting for a worker to be available: *)
let waiters : thread Lwt.u Lwt_sequence.t = Lwt_sequence.create ()

(* Code executed by a worker: *)
let rec worker_loop worker =
  let id, task = Event.sync (Event.receive worker.task_channel) in
  task ();
  (* If there is too much threads, exit. This can happen if the user
     decreased the maximum: *)
  if !threads_count > !max_threads then worker.reuse <- false;
  (* Tell the main thread that work is done: *)
  Lwt_unix.send_notification id;
  if worker.reuse then worker_loop worker

(* create a new worker: *)
let make_worker _ =
  incr threads_count;
  let worker = {
    task_channel = Event.new_channel ();
    thread = Thread.self ();
    reuse = true;
  } in
  worker.thread <- Thread.create worker_loop worker;
  worker

(* Add a worker to the pool: *)
let add_worker worker =
  match Lwt_sequence.take_opt_l waiters with
    | None ->
        Queue.add worker workers
    | Some w ->
        wakeup w worker

(* Wait for worker to be available, then return it: *)
let rec get_worker _ =
  if not (Queue.is_empty workers) then
    return (Queue.take workers)
  else if !threads_count < !max_threads then
    return (make_worker ())
  else begin
    let (res, w) = Lwt.task () in
    let node = Lwt_sequence.add_r w waiters in
    Lwt.on_cancel res (fun _ -> Lwt_sequence.remove node);
    res
  end

(* +-----------------------------------------------------------------+
   | Initialisation, and dynamic parameters reset                    |
   +-----------------------------------------------------------------+ *)

let get_bounds _ = (!min_threads, !max_threads)

let set_bounds (min, max) =
  if min < 0 || max < min then invalid_arg "Lwt_preemptive.set_bounds";
  let diff = min - !threads_count in
  min_threads := min;
  max_threads := max;
  (* Launch new workers: *)
  for i = 1 to diff do
    add_worker (make_worker ())
  done

let initialized = ref false

let init min max errlog =
  initialized := true;
  error_log := errlog;
  set_bounds (min, max)

let simple_init _ =
  if not !initialized then begin
    initialized := true;
    set_bounds (0, 4)
  end

let nbthreads _ = !threads_count
let nbthreadsqueued _ = Lwt_sequence.fold_l (fun _ x -> x + 1) waiters 0
let nbthreadsbusy _ = !threads_count - Queue.length workers

(* +-----------------------------------------------------------------+
   | Detaching                                                       |
   +-----------------------------------------------------------------+ *)

let detach f args =
  simple_init ();
  let result = ref `Nothing in
  (* The task for the worker thread: *)
  let task () =
    try
      result := `Success(f args)
    with exn ->
      result := `Failure exn
  in
  lwt worker = get_worker () in
  let waiter, wakener = wait () in
  let id =
    Lwt_unix.make_notification ~once:true
      (fun () ->
         match !result with
           | `Nothing ->
               wakeup_exn wakener (Failure "Lwt_preemptive.detach")
           | `Success value ->
               wakeup wakener value
           | `Failure exn ->
               wakeup_exn wakener exn)
  in
  try_lwt
    (* Send the id and the task to the worker: *)
    Event.sync (Event.send worker.task_channel (id, task));
    waiter
  finally
    if worker.reuse then
      (* Put back the worker to the pool: *)
      add_worker worker
    else begin
      decr threads_count;
      (* Or wait for the thread to terminates, to free its associated
         resources: *)
      Thread.join worker.thread
    end;
    return ()
