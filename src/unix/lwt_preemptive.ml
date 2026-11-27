(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* [Lwt_sequence] is deprecated – we don't want users outside Lwt using it.
   However, it is still used internally by Lwt. So, briefly disable warning 3
   ("deprecated"), and create a local, non-deprecated alias for
   [Lwt_sequence] that can be referred to by the rest of the code in this
   module without triggering any more warnings. *)
module Lwt_sequence = Lwt_sequence
open Lwt.Infix

(* One pool per loop, because a domain does not terminate while one of its
   threads runs: a shared pool would keep a spawned domain alive. A loop's
   pool is touched by its own domain only, so it needs no lock, and its
   workers end when its domain exits. See the interface for the bounds and
   [run_in_main]. *)
[@@@alert "-trespassing"]

module Lwt_dls = Lwt_dls

[@@@alert "+trespassing"]

(* +-----------------------------------------------------------------+
   | Parameters                                                      |
   +-----------------------------------------------------------------+ *)

(* Settings shared by every loop and applied per loop. They are read and
   written from any domain, hence atomics. *)

(* Minimum number of preemptive threads, per loop: *)
let min_threads = Atomic.make 0

(* Maximum number of preemptive threads, per loop: *)
let max_threads = Atomic.make 0

(* Size of the waiting queue: *)
let max_thread_queued = Atomic.make 1000
let get_max_number_of_threads_queued _ = Atomic.get max_thread_queued

let set_max_number_of_threads_queued n =
  if n < 0 then invalid_arg "Lwt_preemptive.set_max_number_of_threads_queued";
  Atomic.set max_thread_queued n

(* +-----------------------------------------------------------------+
   | Preemptive threads management                                   |
   +-----------------------------------------------------------------+ *)

module CELL : sig
  type 'a t

  val make : unit -> 'a t

  val get : 'a t -> 'a option
  (** The value set, or [None] once the cell is closed and empty. *)

  val set : 'a t -> 'a -> unit

  val close : 'a t -> unit
  (** A value already set is still returned by the next [get]. *)
end = struct
  type 'a t =
    { m : Mutex.t
    ; cv : Condition.t
    ; mutable cell : 'a option
    ; mutable closed : bool }

  let make () =
    {m = Mutex.create (); cv = Condition.create (); cell = None; closed = false}

  let get t =
    let rec await_value t =
      match t.cell with
      | Some v ->
          t.cell <- None;
          Mutex.unlock t.m;
          Some v
      | None when t.closed -> Mutex.unlock t.m; None
      | None -> Condition.wait t.cv t.m; await_value t
    in
    Mutex.lock t.m; await_value t

  let set t v =
    Mutex.lock t.m;
    t.cell <- Some v;
    Mutex.unlock t.m;
    Condition.signal t.cv

  let close t =
    Mutex.lock t.m;
    t.closed <- true;
    Mutex.unlock t.m;
    Condition.broadcast t.cv
end

type thread =
  { task_cell : (Lwt_unix.notification * (unit -> unit)) CELL.t
    (* Channel used to communicate notification id and tasks to the
       worker thread. *)
  ; mutable thread : Thread.t (* The worker thread. *)
  ; reuse : bool Atomic.t
    (* Whether the thread must be re-added to the pool when the work is done.
       Atomic because the worker writes it and its loop reads it. *)
  }

(* Per loop, and touched by its own domain only. *)
type pool =
  { free : thread Queue.t (* Free workers. *)
  ; waiters : thread Lwt.u Lwt_sequence.t
    (* Callers waiting for a free worker. The promises belong to this
       domain. *)
  ; mutable count : int (* How many workers this loop has created. *)
  ; mutable all : thread list
    (* Every worker created, so that they can be joined at domain exit. *)
  ; stopping : bool Atomic.t
    (* Set at domain exit: tells a worker to stop looping. Read by the workers,
       hence atomic. *)
  ; pending : (unit -> bool) Lwt_sequence.t
    (* For each detached call whose result has not reached this loop yet, a
       function that resolves its promise if the task has run, and says whether
       it did. [shutdown] calls them once the workers are joined, so that a
       promise does not stay pending when its notification is not handled. *)
  }

let shutdown_pool : (pool -> unit) ref = ref (fun _ -> ())

let pool_slot : pool Lwt_dls.t =
  Lwt_dls.new_key (fun () ->
    let pool =
      { free = Queue.create ()
      ; waiters = Lwt_sequence.create ()
      ; count = 0
      ; all = []
      ; stopping = Atomic.make false
      ; pending = Lwt_sequence.create () }
    in
    (* Not on the main domain: at the exit of the process, this would wait for
       every detached computation, for ever if one blocks.

       A pool first created during the exit of its domain, by an exit hook that
       detaches, is never shut down: OCaml does not run a callback registered
       then. Its workers then keep the domain alive. *)
    if not (Lwt_dls.is_main_domain ())
    then Lwt_dls.at_domain_exit (fun () -> !shutdown_pool pool);
    pool)

let[@inline] self_pool () = Lwt_dls.get pool_slot

(* Code executed by a worker: *)
let rec worker_loop pool worker =
  match CELL.get worker.task_cell with
  | None ->
      (* Closed by [shutdown]: the domain is exiting. *)
      ()
  | Some (id, task) -> worker_task pool worker id task

and worker_task pool worker id task =
  task ();
  (* If there is too much threads, exit. This can happen if the user
     decreased the maximum: *)
  if pool.count > Atomic.get max_threads then Atomic.set worker.reuse false;
  (* Tell the loop that submitted this task that the work is done. The id names
     that loop's notification channel. *)
  Lwt_unix.send_notification id;
  if Atomic.get worker.reuse && not (Atomic.get pool.stopping)
  then worker_loop pool worker

(* create a new worker: *)
let make_worker pool =
  pool.count <- pool.count + 1;
  let worker =
    {task_cell = CELL.make (); thread = Thread.self (); reuse = Atomic.make true}
  in
  worker.thread <- Thread.create (worker_loop pool) worker;
  pool.all <- worker :: pool.all;
  worker

(* Add a worker to the pool: *)
let add_worker pool worker =
  match Lwt_sequence.take_opt_l pool.waiters with
  | None -> Queue.add worker pool.free
  | Some w -> Lwt.wakeup w worker

(* Wait for worker to be available, then return it: *)
let get_worker pool =
  if not (Queue.is_empty pool.free)
  then Lwt.return (Queue.take pool.free)
  else if pool.count < Atomic.get max_threads
  then Lwt.return (make_worker pool)
  else (Lwt.add_task_r [@ocaml.warning "-3"]) pool.waiters

(* Ends this loop's workers and waits for them, so that the domain can
   terminate. The cell of every worker is closed, not only those of the free
   ones. A worker that finished its task after the loop stopped waits for
   another task, but it is not in [free]: only the loop puts workers back
   there. Once its cell is closed, a waiting worker leaves, after running a
   task already in the cell. A busy worker sees [stopping] once its task is
   done, and joining it waits for that.

   This may run before or after [Lwt_main]'s drain of exit hooks. A worker
   created after [shutdown], by an exit hook that detaches, sees [stopping],
   leaves after its one task, and is joined by [detach]. *)
let shutdown pool =
  Atomic.set pool.stopping true;
  List.iter (fun worker -> CELL.close worker.task_cell) pool.all;
  (* The free workers leave now. A busy one is counted down by the [detach]
     that holds it, once its task is done. *)
  pool.count <- pool.count - Queue.length pool.free;
  Queue.clear pool.free;
  List.iter (fun worker -> Thread.join worker.thread) pool.all;
  pool.all <- [];
  (* Every task given to a worker is done now. Resolve the promises whose
     notification has not been handled: the loop may not run again, or its
     channel may be retired already. *)
  Lwt_sequence.iter_node_l
    (fun node -> if Lwt_sequence.get node () then Lwt_sequence.remove node)
    pool.pending

let () = shutdown_pool := shutdown

(* +-----------------------------------------------------------------+
   | Initialisation, and dynamic parameters reset                    |
   +-----------------------------------------------------------------+ *)

let get_bounds () = Atomic.get min_threads, Atomic.get max_threads

(* The bounds are process-wide; the workers they launch are this loop's. *)
let set_bounds (min, max) =
  if min < 0 || max < min then invalid_arg "Lwt_preemptive.set_bounds";
  let pool = self_pool () in
  let diff = min - pool.count in
  Atomic.set min_threads min;
  Atomic.set max_threads max;
  (* Launch new workers, but not after [shutdown], since nothing would end
     them: *)
  if not (Atomic.get pool.stopping)
  then
    for _i = 1 to diff do
      add_worker pool (make_worker pool)
    done

(* Whether the bounds have been set, per loop: a domain arriving later must
   still get its own minimum of workers. *)
let initialized : bool ref Lwt_dls.t = Lwt_dls.new_key (fun () -> ref false)

let init min max _errlog =
  Lwt_dls.get initialized := true;
  set_bounds (min, max)

let simple_init () =
  let flag = Lwt_dls.get initialized in
  if not !flag
  then begin
    flag := true;
    (* Only take the default bounds if nobody has set any. *)
    if Atomic.get max_threads = 0
    then set_bounds (0, 4)
    else set_bounds (Atomic.get min_threads, Atomic.get max_threads)
  end

(* All three report on the calling loop's pool. *)
let nbthreads () = (self_pool ()).count

let nbthreadsqueued () =
  Lwt_sequence.fold_l (fun _ x -> x + 1) (self_pool ()).waiters 0

let nbthreadsbusy () =
  let pool = self_pool () in
  pool.count - Queue.length pool.free

(* +-----------------------------------------------------------------+
   | Detaching                                                       |
   +-----------------------------------------------------------------+ *)

let init_result = Result.Error (Failure "Lwt_preemptive.detach")

(* Gives the task to the worker, waits for its result, then puts the worker
   back, or joins it. *)
let run_task pool worker id task waiter =
  Lwt.finalize
    (fun () ->
       (* Send the id and the task to the worker: *)
       CELL.set worker.task_cell (id, task);
       waiter)
    (fun () ->
       if Atomic.get worker.reuse && not (Atomic.get pool.stopping)
       then
         (* Put back the worker to the pool: *)
         add_worker pool worker
       else begin
         pool.count <- pool.count - 1;
         (* Or wait for the thread to terminates, to free its associated
            resources: *)
         Thread.join worker.thread;
         (* After [shutdown], no worker goes back to the pool. So a caller
            still waiting for one gets a new worker, which leaves after its
            task. *)
         if Atomic.get pool.stopping
         then
           match Lwt_sequence.take_opt_l pool.waiters with
           | Some w -> Lwt.wakeup w (make_worker pool)
           | None -> ()
       end;
       Lwt.return_unit)

let detach f args =
  if (Lwt_unix.notification_channel_closed [@alert "-trespassing"]) ()
  then
    (* The domain has exited, and its loop can no longer be told that a worker
       is done. So [f] runs here. *)
    match f args with
    | v -> Lwt.return v
    | exception exn when Lwt.Exception_filter.run exn -> Lwt.fail exn
  else begin
    let result = ref init_result in
    (* The task for the worker thread: *)
    let task () =
      try result := Result.Ok (f args)
      with exn when Lwt.Exception_filter.run exn -> result := Result.Error exn
    in
    (* The notification first, before anything that may take or start a
       worker: if it failed, nothing would end that worker. *)
    let waiter, wakener = Lwt.wait () in
    let complete () =
      if Lwt.state waiter = Lwt.Sleep then Lwt.wakeup_result wakener !result
    in
    let pool = self_pool () in
    let node =
      Lwt_sequence.add_r
        (fun () ->
           let ran = !result != init_result in
           if ran then complete ();
           ran)
        pool.pending
    in
    let id =
      Lwt_unix.make_notification ~once:true (fun () ->
        Lwt_sequence.remove node; complete ())
    in
    simple_init ();
    Lwt.try_bind
      (fun () -> get_worker pool)
      (fun worker -> run_task pool worker id task waiter)
      (fun exn ->
         Lwt_sequence.remove node;
         Lwt_unix.stop_notification id;
         Lwt.reraise exn)
  end

(* +-----------------------------------------------------------------+
   | Running Lwt threads in the main thread                          |
   +-----------------------------------------------------------------+ *)

(* Queue of [unit -> unit Lwt.t] functions. *)
let jobs = Queue.create ()

(* Mutex to protect access to [jobs]. *)
let jobs_mutex = Mutex.create ()

let job_notification =
  Lwt_unix.make_notification (fun () ->
    (* Take the first job. The queue is never empty at this
       point. *)
    Mutex.lock jobs_mutex;
    let thunk = Queue.take jobs in
    Mutex.unlock jobs_mutex;
    ignore (thunk ()))

let run_in_main_dont_wait f =
  (* Add the job to the queue. *)
  Mutex.lock jobs_mutex;
  Queue.add f jobs;
  Mutex.unlock jobs_mutex;
  (* Notify the main thread. *)
  Lwt_unix.send_notification job_notification

(* There is a potential performance issue from creating a cell every time this
   function is called. See:
   https://github.com/ocsigen/lwt/issues/218
   https://github.com/ocsigen/lwt/pull/219
   https://github.com/ocaml/ocaml/issues/7158 *)
let run_in_main f =
  let cell = CELL.make () in
  (* Create the job. *)
  let job () =
    (* Execute [f] and wait for its result. *)
    Lwt.try_bind f
      (fun ret -> Lwt.return (Result.Ok ret))
      (fun exn -> Lwt.return (Result.Error exn))
    >>= fun result ->
    (* Send the result. *)
    CELL.set cell result; Lwt.return_unit
  in
  run_in_main_dont_wait job;
  (* Wait for the result. *)
  match CELL.get cell with
  | Some (Result.Ok ret) -> ret
  | Some (Result.Error exn) -> raise exn
  | None -> (* This cell is never closed. *) assert false

(* This version shadows the one above, adding an exception handler *)
let run_in_main_dont_wait f handler =
  let f () = Lwt.catch f (fun exc -> handler exc; Lwt.return_unit) in
  run_in_main_dont_wait f
