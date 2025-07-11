module ED = Effect.Deep

module Storage = struct
  module Lwt_storage=  Lwt.Private.Sequence_associated_storage
  type 'a key = 'a Lwt.key
  let new_key = Lwt.new_key
  let get = Lwt.get
  let set k v = Lwt_storage.(current_storage := modify_storage k (Some v) !current_storage)
  let remove k = Lwt_storage.(current_storage := modify_storage k None !current_storage)

  let reset_to_empty () = Lwt_storage.(current_storage := empty_storage)
  let save_current () = !Lwt_storage.current_storage
  let restore_current saved = Lwt_storage.current_storage := saved
end

type _ Effect.t +=
  | Await : 'a Lwt.t -> 'a Effect.t
  | Yield : unit Effect.t

(** Queue of microtasks that are ready *)
let tasks : (unit -> unit) Queue.t = Queue.create ()

let[@inline] push_task f : unit = Queue.push f tasks

let default_on_uncaught_exn exn bt =
  Printf.eprintf "lwt_task: uncaught task exception:\n%s\n%s\n%!"
    (Printexc.to_string exn)
    (Printexc.raw_backtrace_to_string bt)

let run_all_tasks () : unit =
  let n_processed = ref 0 in
  let max_number_of_steps = min 10_000 (2 * Queue.length tasks) in
  while (not (Queue.is_empty tasks)) && !n_processed < max_number_of_steps do
    let t = Queue.pop tasks in
    incr n_processed;
    try t ()
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      default_on_uncaught_exn exn bt
  done;
  (* make sure we don't sleep forever if there's no lwt promise
    ready but [tasks] contains ready tasks *)
  if not (Queue.is_empty tasks) then ignore (Lwt.pause () : unit Lwt.t)

let setup_hooks =
  let already_done = ref false in
  fun () ->
    if not !already_done then (
      already_done := true;
      let _hook1 = Lwt_main.Enter_iter_hooks.add_first run_all_tasks in
      let _hook2 = Lwt_main.Leave_iter_hooks.add_first run_all_tasks in
      ()
    )

let await (fut : 'a Lwt.t) : 'a =
  match Lwt.state fut with
  | Lwt.Return x -> x
  | Lwt.Fail exn -> raise exn
  | Lwt.Sleep -> Effect.perform (Await fut)

let yield () : unit = Effect.perform Yield

(** the main effect handler *)
let handler : _ ED.effect_handler =
  let effc : type b. b Effect.t -> ((b, unit) ED.continuation -> 'a) option =
    function
    | Yield ->
      Some (fun k ->
        let storage = Storage.save_current () in
        push_task (fun () ->
          Storage.restore_current storage;
          ED.continue k ()))
    | Await fut ->
      Some
        (fun k ->
          let storage = Storage.save_current () in
          Lwt.on_any fut
            (fun res -> push_task (fun () ->
              Storage.restore_current storage; ED.continue k res))
            (fun exn -> push_task (fun () ->
              Storage.restore_current storage; ED.discontinue k exn)))
    | _ -> None
  in
  { effc }

let run_inside_effect_handler_and_resolve_ (type a) (promise : a Lwt.u) f () : unit =
  let run_f_and_set_res () =
    let res = ref (Error (Failure "not resolved")) in
    Storage.reset_to_empty();
    (try
       let r = f () in
       res := Ok r
     with exn -> res := Error exn);
    Lwt.wakeup_result promise !res
  in
  ED.try_with run_f_and_set_res () handler

let run f : _ Lwt.t =
  setup_hooks ();
  let lwt, resolve = Lwt.wait () in
  push_task (run_inside_effect_handler_and_resolve_ resolve f);
  lwt

let run_inside_effect_handler_in_the_background_ ~on_uncaught_exn f () : unit =
  let run_f () : unit =
    Storage.reset_to_empty();
    try
       f ()
     with exn ->
      let bt = Printexc.get_raw_backtrace () in
      on_uncaught_exn exn bt
  in
  ED.try_with run_f () handler

let run_in_the_background ?(on_uncaught_exn=default_on_uncaught_exn) f : unit =
  setup_hooks ();
  push_task (run_inside_effect_handler_in_the_background_ ~on_uncaught_exn f)
