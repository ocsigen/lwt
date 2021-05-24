open Lwt.Infix

module C = Domainslib.Chan

(* [Lwt_sequence] is deprecated – we don't want users outside Lwt using it.
   However, it is still used internally by Lwt. So, briefly disable warning 3
   ("deprecated"), and create a local, non-deprecated alias for
   [Lwt_sequence] that can be referred to by the rest of the code in this
   module without triggering any more warnings. *)
[@@@ocaml.warning "-3"]
module Lwt_sequence = Lwt_sequence
[@@@ocaml.warning "+3"]

(* Minimum number of domains: *)
let min_domains : int ref = ref 0

(* Maximum number of domains: *)
let max_domains : int ref = ref 0

(* Size of the waiting queue: *)
let max_domains_queued = ref 128

let get_max_number_of_domains_queued _ =
  !max_domains_queued

let set_max_number_of_domains_queued n =
  if n < 0 then invalid_arg "Lwt_domain.set_max_number_of_domains_queued";
  max_domains_queued := n

let domains_count = ref 0

type task =
  Task of (int * (unit -> unit))
  | Quit

type dom = {
  task_chan : task C.t;
  mutable domain: unit Domain.t
}

let workers : dom Queue.t = Queue.create ()

let waiters : dom Lwt.u Lwt_sequence.t = Lwt_sequence.create ()

let rec worker_loop worker =
  match C.recv worker.task_chan with
  | Task (id, task) ->
      task ();
      Lwt_unix.send_notification id;
      worker_loop worker
  | Quit -> ()

let make_worker () =
  incr domains_count;
  let worker = {
    task_chan = C.make_bounded 0;
    domain = Domain.spawn (fun _ -> ())
  } in
  Domain.join worker.domain;
  worker.domain <- Domain.spawn(fun _ -> worker_loop worker);
  worker

let add_worker worker =
  match Lwt_sequence.take_opt_l waiters with
  | None ->
    Queue.add worker workers
  | Some w ->
    Lwt.wakeup w worker

let get_worker () =
  if not (Queue.is_empty workers) then
    Lwt.return (Queue.take workers)
  else if !domains_count < !max_domains then
    Lwt.return (make_worker ())
  else
    (Lwt.add_task_r [@ocaml.warning "-3"]) waiters

let get_bounds () = (!min_domains, !max_domains)

let set_bounds (min, max) =
  if min < 0 || max < min then invalid_arg "Lwt_domain.set_bounds";
  (* Close excess domains *)
  if (max < !domains_count) then begin
    for _i = 1 to (!domains_count - max) do
       let worker = Queue.take workers in
       C.send worker.task_chan Quit
    done;
  end;
  let diff = min - !domains_count in
  min_domains := min;
  max_domains := max;

  for _i = 1 to diff do
    add_worker (make_worker ())
  done

let initialized = ref false

let init min max _errlog =
  initialized := true;
  set_bounds (min, max)

let simple_init () =
  if not !initialized then begin
    initialized := true;
    set_bounds (0, 4)
  end

let init_result = Result.Error (Failure "Lwt_domain.detach")

let detach f args =
  simple_init ();
  let result = ref init_result in
  let task () =
    try
      result := Result.Ok (f args)
    with exn ->
      result := Result.Error exn
  in
  get_worker () >>= fun worker ->
  let waiter, wakener = Lwt.wait () in
  let id =
    Lwt_unix.make_notification ~once:true
      (fun () -> Lwt.wakeup_result wakener !result)
  in
  Lwt.finalize
  (fun () ->
    C.send worker.task_chan (Task (id, task));
    waiter)
  (fun () ->
    add_worker worker;
    Lwt.return_unit)

let nbdomains () = !domains_count
let nbdomainsqueued () = Lwt_sequence.fold_l (fun _ x -> x + 1) waiters 0
let nbdomainsbusy () = !domains_count - Queue.length workers
(* +-----------------------------------------------------------------+
   | Running Lwt threads in the main domain                          |
   +-----------------------------------------------------------------+ *)

(* Jobs to be run in the main domain *)
let jobs = C.make_unbounded ()

let job_notification =
  Lwt_unix.make_notification
    (fun () ->
      let thunk = C.recv jobs in
      ignore (thunk ()))

let run_in_main f =
  let res = ref init_result in
  let job () =
    Lwt.try_bind f
      (fun ret -> Lwt.return (Result.Ok ret))
      (fun exn -> Lwt.return (Result.Error exn)) >>= fun result ->
    res := result;
    Lwt.return_unit
  in
  C.send jobs job;

  Lwt_unix.send_notification job_notification;
  match !res with
  | Result.Ok ret -> ret
  | Result.Error exn -> raise exn