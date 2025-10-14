(* Direct-style wrapper for Lwt code

   The implementation of the direct-style wrapper relies on ocaml5's effect
   system capturing continuations and adding them as a callback to some lwt
   promises. *)

(* part 1: tasks, getting the scheduler to call them *)

let tasks : (unit -> unit) Queue.t ref = ref (Queue.create ())

let[@inline] push_task f : unit = Queue.push f !tasks

let absolute_max_number_of_steps =
  (* TODO 6.0: what's a good number here? should it be customisable? *)
  10_000

let run_all_tasks () : unit =
  let n_processed = ref 0 in
  let max_number_of_steps = min absolute_max_number_of_steps (2 * Queue.length !tasks) in
  while (not (Queue.is_empty !tasks)) && !n_processed < max_number_of_steps do
    let t = Queue.pop !tasks in
    incr n_processed;
    try t ()
    with exn ->
      (* TODO 6.0: change async_exception handler to accept a backtrace, pass it
         here and at the other use site. *)
      (* TODO 6.0: this and other try-with: respect exception-filter *)
      !Lwt.async_exception_hook exn
  done;
  (* In the case where there are no promises ready for wakeup, the scheduler's
     engine will pause until some IO completes. There might never be completed
     IO, depending on the program structure and the state of the world. If this
     happens and the queue is not empty, we add a [pause] so that the engine has
     something to wakeup for so that the rest of the queue can be processed. *)
  if not (Queue.is_empty !tasks) && Lwt.paused_count () = 0 then ignore (Lwt.pause () : unit Lwt.t)

let setup_hooks =
  let already_done = ref false in
  fun () ->
    if not !already_done then (
      already_done := true;
      (* TODO 6.0: assess whether we should have both hooks or just one (which
         one). Tempted to say we should only have the enter hook. *)
      let _hook1 = Lwt_main.Enter_iter_hooks.add_first run_all_tasks in
      let _hook2 = Lwt_main.Leave_iter_hooks.add_first run_all_tasks in
      ()
    )

(* part 2: effects, performing them *)

type _ Effect.t +=
  | Await : 'a Lwt.t -> 'a Effect.t
  | Yield : unit Effect.t

let await (fut : 'a Lwt.t) : 'a =
  match Lwt.state fut with
  | Lwt.Return x -> x
  | Lwt.Fail exn -> raise exn
  | Lwt.Sleep -> Effect.perform (Await fut)

let yield () : unit = Effect.perform Yield

(* interlude: task-local storage helpers *)

module Storage = struct
  [@@@alert "-trespassing"]
  module Lwt_storage = Lwt.Private.Sequence_associated_storage
  [@@@alert "+trespassing"]
  type 'a key = 'a Lwt.key
  let new_key = Lwt.new_key
  let get = Lwt.get
  let set k v =
    let open Lwt_storage in
    current_storage := (modify_storage k (Some v) !current_storage)
  let remove k =
    let open Lwt_storage in
    current_storage := (modify_storage k None !current_storage)
  let reset_to_empty () =
    let open Lwt_storage in
    current_storage := empty_storage
  let save_current () = !Lwt_storage.current_storage
  let restore_current saved = Lwt_storage.current_storage := saved
end

(* part 3: handling effects *)

let handler : _ Effect.Deep.effect_handler =
  let effc : type b. b Effect.t -> ((b, unit) Effect.Deep.continuation -> 'a) option =
    function
    | Yield ->
      Some (fun k ->
        let storage = Storage.save_current () in
        push_task (fun () ->
          Storage.restore_current storage;
          Effect.Deep.continue k ()))
    | Await fut ->
      Some
        (fun k ->
          let storage = Storage.save_current () in
          Lwt.on_any fut
            (fun res -> push_task (fun () ->
              Storage.restore_current storage; Effect.Deep.continue k res))
            (fun exn -> push_task (fun () ->
              Storage.restore_current storage; Effect.Deep.discontinue k exn)))
    | _ -> None
  in
  { effc }

(* part 4: putting it all together: running tasks *)

let run_inside_effect_handler_and_resolve_ (type a) (promise : a Lwt.u) f () : unit =
  let run_f_and_set_res () =
    Storage.reset_to_empty();
    match f () with
    | res -> Lwt.wakeup promise res
    | exception exc -> Lwt.wakeup_exn promise exc
  in
  Effect.Deep.try_with run_f_and_set_res () handler

let spawn f : _ Lwt.t =
  setup_hooks ();
  let lwt, resolve = Lwt.wait () in
  push_task (run_inside_effect_handler_and_resolve_ resolve f);
  lwt

(* part 4 (encore): running a task in the background *)

let run_inside_effect_handler_in_the_background_ f () : unit =
  let run_f () : unit =
    Storage.reset_to_empty();
    try
      f ()
    with exn ->
      !Lwt.async_exception_hook exn
  in
  Effect.Deep.try_with run_f () handler

let spawn_in_the_background f : unit =
  setup_hooks ();
  push_task (run_inside_effect_handler_in_the_background_ f)
