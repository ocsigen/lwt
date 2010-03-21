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

(* Pipe used to communicate between worker threads and the main
   thread. Each time a thread finish its work, it send its id to the
   main thread: *)
let finished_pipe =
  let (in_fd, out_fd) = Lwt_unix.pipe_in () in
  Lwt_unix.set_close_on_exec in_fd;
  Unix.set_close_on_exec out_fd;
  (Lwt_io.of_fd ~mode:Lwt_io.input ~buffer_size:256 in_fd, Unix.out_channel_of_descr out_fd)

(* Mutex used to prevent concurrent access to [finished_pipe] by
   preemptive worker threads: *)
let pipe_lock = Mutex.create ()

type thread = {
  task_channel: (unit -> unit) Event.channel;
  (* Channel used to communicate a task to the worker thread. *)

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

(* Mapping from thread ids to client lwt-thread: *)
let clients : (int, unit Lwt.u) Hashtbl.t = Hashtbl.create 16

(* Code executed by a worker: *)
let rec worker_loop worker =
  let task = Event.sync (Event.receive worker.task_channel) in
  task ();
  (* If there is too much threads, exit. This can happen if the user
     decreased the maximum: *)
  if !threads_count > !max_threads then worker.reuse <- false;
  (* Tell the main thread that work is done: *)
  Mutex.lock pipe_lock;
  let oc = snd finished_pipe in
  Pervasives.output_string oc (string_of_int (Thread.id worker.thread) ^ "\n");
  Pervasives.flush oc;
  Mutex.unlock pipe_lock;
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
   | Dispatcher                                                      |
   +-----------------------------------------------------------------+ *)

(* The dispatcher is responsible for reading id of threads which have
   finished their work and wakeup the corresponding lwt thread: *)

let rec dispatch () =
  begin
    try_lwt
      (* Read the id of the next thread that has finished his work: *)
      lwt n = int_of_string =|< read_line (fst finished_pipe) in
      ignore begin
        (* Here we want to do the recursive call as soon as possible
           (and before the wakeup) because if Lwt_unix.run is called
           by the waiters of the thread beeing awoken, and if that run
           wants to use detach, the pipe won't be available, and the
           run will never finish ...  and block the other run
           (remember that an invocation of [run] will not terminate
           before all subsequent invocations are terminated) *)
        lwt () = Lwt_unix.yield () in
        let w = Hashtbl.find clients n in
        Hashtbl.remove clients n;
        wakeup w ();
        return ()
      end;
      return `continue
    with
      | Channel_closed _ ->
          return `break
      | exn ->
          Printf.ksprintf !error_log
            "Internal error in lwt_preemptive.ml (read failed on the pipe) %s - Please check if Lwt_preemptive is initialized and that lwt_preemptive.cmo is linked only once. Otherwise, please report the bug"
            (Printexc.to_string exn);
          fail exn
  end >>= function
    | `continue -> dispatch ()
    | `break -> return ()

let dispatcher_running = ref false

let dispatcher = lazy(dispatcher_running := true; dispatch ())

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

let init min max errlog =
  error_log := errlog;
  set_bounds (min, max);
  Lazy.force dispatcher

let simple_init _ =
  if not !dispatcher_running then set_bounds (0, 4);
  Lazy.force dispatcher

let nbthreads _ = !threads_count
let nbthreadsqueued _ = Lwt_sequence.fold_l (fun _ x -> x + 1) waiters 0
let nbthreadsbusy _ = !threads_count - Queue.length workers

(* +-----------------------------------------------------------------+
   | Detaching                                                       |
   +-----------------------------------------------------------------+ *)

let detach f args =
  let _ = simple_init () in
  let result = ref `Nothing in
  (* The task for the worker thread: *)
  let task () =
    try
      result := `Success(f args)
    with exn ->
      result := `Failure exn
  in
  lwt worker = get_worker () in
  let (res, w) =  Lwt.wait () in
  Hashtbl.add clients (Thread.id worker.thread) w;
  (* Send the task to the worker: *)
  Event.sync (Event.send worker.task_channel task);
  try_lwt
    (* Wait for notification of the dispatcher: *)
    res >>
      match !result with
      | `Nothing ->
          fail (Failure "Lwt_preemptive.detach")
      | `Success v ->
          return v
      | `Failure exn ->
          fail exn
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
