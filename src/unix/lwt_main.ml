(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



(* [Lwt_sequence] is deprecated – we don't want users outside Lwt using it.
   However, it is still used internally by Lwt. So, briefly disable warning 3
   ("deprecated"), and create a local, non-deprecated alias for
   [Lwt_sequence] that can be referred to by the rest of the code in this
   module without triggering any more warnings. *)
module Lwt_sequence = Lwt_sequence

open Lwt.Infix

let enter_iter_hooks = Domain.DLS.new_key (fun () -> Lwt_sequence.create ())
let leave_iter_hooks = Domain.DLS.new_key (fun () -> Lwt_sequence.create ())

let yield = Lwt.pause

let abandon_yielded_and_paused () =
  Lwt.abandon_paused ()

let run p =
  let domain_id = Domain.self () in
  let () = if Lwt.is_alredy_registered domain_id then
    ()
  else begin
    let n = Lwt_unix.make_notification domain_id (fun () ->
      let cbs = Lwt.get_sent_callbacks domain_id in
      Lwt_sequence.iter_l (fun f -> f ()) cbs
    ) in
    Lwt.register_notification domain_id (fun () -> Lwt_unix.send_notification domain_id n)
  end
  in
  let rec run_loop () =
    match Lwt.poll p with
    | Some x ->
      x
    | None ->
      (* Call enter hooks. *)
      Lwt_sequence.iter_l (fun f -> f ()) (Domain.DLS.get enter_iter_hooks);

      (* Do the main loop call. *)
      let should_block_waiting_for_io = Lwt.paused_count () = 0 in
      Lwt_engine.iter should_block_waiting_for_io;

      (* Fulfill paused promises. *)
      Lwt.wakeup_paused ();

      (* Call leave hooks. *)
      Lwt_sequence.iter_l (fun f -> f ()) (Domain.DLS.get leave_iter_hooks);

      (* Repeat. *)
      run_loop ()
  in

  run_loop ()

let run_already_called = Domain.DLS.new_key (fun () -> `No)
let run_already_called_mutex = Domain.DLS.new_key (fun () -> Mutex.create ())

let finished () =
  Mutex.lock (Domain.DLS.get run_already_called_mutex);
  Domain.DLS.set run_already_called `No;
  Mutex.unlock (Domain.DLS.get run_already_called_mutex)

let run p =
  (* Fail in case a call to Lwt_main.run is nested under another invocation of
     Lwt_main.run. *)
  Mutex.lock (Domain.DLS.get run_already_called_mutex);
  let error_message_if_call_is_nested =
      match (Domain.DLS.get run_already_called) with
      (* `From is effectively disabled for the time being, because there is a bug,
         present in all versions of OCaml supported by Lwt, where, with the
         bytecode runtime, if one changes the working directory and then attempts
         to retrieve the backtrace, the runtime calls [abort] at the C level and
         exits the program ungracefully. It is especially likely that a daemon
         would change directory before calling [Lwt_main.run], so we can't have it
         retrieving the backtrace, even though a daemon is not likely to be
         compiled to bytecode.

         This can be addressed with detection. Starting with 4.04, there is a
         type [Sys.backend_type] that could be used. *)
      | `From backtrace_string ->
        Some (Printf.sprintf "%s\n%s\n%s"
          "Nested calls to Lwt_main.run are not allowed"
          "Lwt_main.run already called from:"
          backtrace_string)
      | `From_somewhere ->
        Some ("Nested calls to Lwt_main.run are not allowed")
      | `No ->
        let called_from =
          (* See comment above.
          if Printexc.backtrace_status () then
            let backtrace =
              try raise Exit
              with Exit -> Printexc.get_backtrace ()
            in
            `From backtrace
          else *)
            `From_somewhere
        in
        Domain.DLS.set run_already_called called_from;
        None
  in
  Mutex.unlock (Domain.DLS.get run_already_called_mutex);

  begin match error_message_if_call_is_nested with
  | Some message -> failwith message
  | None -> ()
  end;

  match run p with
  | result ->
    finished ();
    result
  | exception exn when Lwt.Exception_filter.run exn ->
    finished ();
    raise exn

let exit_hooks = Domain.DLS.new_key (fun () -> Lwt_sequence.create ())

let rec call_hooks () =
  match Lwt_sequence.take_opt_l (Domain.DLS.get exit_hooks) with
  | None ->
    Lwt.return_unit
  | Some f ->
    Lwt.catch
      (fun () -> f ())
      (fun _  -> Lwt.return_unit) >>= fun () ->
    call_hooks ()

let () =
  at_exit (fun () ->
    if not (Lwt_sequence.is_empty (Domain.DLS.get exit_hooks)) then begin
      Lwt.abandon_wakeups ();
      finished ();
      run (call_hooks ())
    end)

let at_exit f = ignore (Lwt_sequence.add_l f (Domain.DLS.get exit_hooks))

module type Hooks =
sig
  type 'return_value kind
  type hook

  val add_first : (unit -> unit kind) -> hook
  val add_last : (unit -> unit kind) -> hook
  val remove : hook -> unit
  val remove_all : unit -> unit
end

module type Hook_sequence =
sig
  type 'return_value kind
  val sequence : (unit -> unit kind) Lwt_sequence.t Domain.DLS.key
end

module Wrap_hooks (Sequence : Hook_sequence) =
struct
  type 'a kind = 'a Sequence.kind
  type hook = (unit -> unit Sequence.kind) Lwt_sequence.node

  let add_first hook_fn =
    let hook_node = Lwt_sequence.add_l hook_fn (Domain.DLS.get Sequence.sequence) in
    hook_node

  let add_last hook_fn =
    let hook_node = Lwt_sequence.add_r hook_fn (Domain.DLS.get Sequence.sequence) in
    hook_node

  let remove hook_node =
    Lwt_sequence.remove hook_node

  let remove_all () =
    Lwt_sequence.iter_node_l Lwt_sequence.remove (Domain.DLS.get Sequence.sequence)
end

module Enter_iter_hooks =
  Wrap_hooks (struct
    type 'return_value kind = 'return_value
    let sequence = enter_iter_hooks
  end)

module Leave_iter_hooks =
  Wrap_hooks (struct
    type 'return_value kind = 'return_value
    let sequence = leave_iter_hooks
  end)

module Exit_hooks =
  Wrap_hooks (struct
    type 'return_value kind = 'return_value Lwt.t
    let sequence = exit_hooks
  end)
