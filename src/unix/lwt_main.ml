(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* [Lwt_sequence] is deprecated – we don't want users outside Lwt using it.
   However, it is still used internally by Lwt. So, briefly disable warning 3
   ("deprecated"), and create a local, non-deprecated alias for
   [Lwt_sequence] that can be referred to by the rest of the code in this
   module without triggering any more warnings. *)
module Lwt_sequence = Lwt_sequence
open Lwt.Infix

(* Per domain, in one slot: iteration hooks run around an iteration of the
   domain's engine, and exit hooks return promises that only the loop of their
   domain can run.

   On a spawned domain, the slot initializer arranges, with
   [Lwt_dls.at_domain_exit], for the exit hooks to run when the domain exits,
   even if it never calls [run]. The main domain keeps the [Stdlib.at_exit]
   below, so that Lwt's hooks still run after a user's own [at_exit]:
   [Domain.at_exit] callbacks run before them all. *)
type hooks =
  { enter_iter : (unit -> unit) Lwt_sequence.t
  ; leave_iter : (unit -> unit) Lwt_sequence.t
  ; exits : (unit -> unit Lwt.t) Lwt_sequence.t }

let drain_exit_hooks = ref (fun () -> ())

[@@@alert "-trespassing"]

module Lwt_dls = Lwt_dls

[@@@alert "+trespassing"]

let hooks : hooks Lwt_dls.t =
  Lwt_dls.new_key (fun () ->
    if not (Lwt_dls.is_main_domain ())
    then Lwt_dls.at_domain_exit (fun () -> !drain_exit_hooks ());
    { enter_iter = Lwt_sequence.create ()
    ; leave_iter = Lwt_sequence.create ()
    ; exits = Lwt_sequence.create () })

let enter_iter_hooks () = (Lwt_dls.get hooks).enter_iter
let leave_iter_hooks () = (Lwt_dls.get hooks).leave_iter
let exit_hooks () = (Lwt_dls.get hooks).exits
let yield = Lwt.pause
let abandon_yielded_and_paused () = Lwt.abandon_paused ()

let run p =
  let rec run_loop () =
    Lwt_rte.emit_sch_lap ();
    Lwt_unix.write_job_count_runtimte_event ();
    Lwt_rte.emit_paused_count (Lwt.paused_count ());
    match Lwt.poll p with
    | Some x -> x
    | None ->
        (* Call enter hooks. *)
        Lwt_sequence.iter_l (fun f -> f ()) (enter_iter_hooks ());
        (* Do the main loop call. *)
        let should_block_waiting_for_io = Lwt.paused_count () = 0 in
        Lwt_engine.iter should_block_waiting_for_io;
        (* Fulfill paused promises. *)
        Lwt.wakeup_paused ();
        (* Call leave hooks. *)
        Lwt_sequence.iter_l (fun f -> f ()) (leave_iter_hooks ());
        (* Repeat. *)
        run_loop ()
  in
  Lwt_rte.emit_sch_call_begin ();
  Fun.protect
    ~finally:(fun () -> Lwt_rte.emit_sch_call_end ())
    (fun () -> run_loop ())

let run_already_called = ref `No
let run_already_called_mutex = Mutex.create ()

let finished () =
  Mutex.lock run_already_called_mutex;
  run_already_called := `No;
  Mutex.unlock run_already_called_mutex

let run p =
  (* Fail in case a call to Lwt_main.run is nested under another invocation of
     Lwt_main.run. *)
  Mutex.lock run_already_called_mutex;
  let error_message_if_call_is_nested =
    match !run_already_called with
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
        Some
          (Printf.sprintf "%s\n%s\n%s"
             "Nested calls to Lwt_main.run are not allowed"
             "Lwt_main.run already called from:" backtrace_string)
    | `From_somewhere -> Some "Nested calls to Lwt_main.run are not allowed"
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
        run_already_called := called_from;
        None
  in
  Mutex.unlock run_already_called_mutex;
  begin match error_message_if_call_is_nested with
  | Some message -> failwith message
  | None -> ()
  end;
  Lwt_unix.install_sigchld_handler ();
  match run p with
  | result -> finished (); result
  | exception exn when Lwt.Exception_filter.run exn -> finished (); raise exn

let rec call_hooks () =
  match Lwt_sequence.take_opt_l (exit_hooks ()) with
  | None -> Lwt.return_unit
  | Some f ->
      Lwt.catch (fun () -> f ()) (fun _ -> Lwt.return_unit) >>= fun () ->
      call_hooks ()

(* Runs this domain's exit hooks: at process exit on the main domain, through
   the [Stdlib.at_exit] below, and when the domain exits on the others, through
   the [Lwt_dls.at_domain_exit] registered by the [hooks] slot. *)
let drain () =
  if not (Lwt_sequence.is_empty (exit_hooks ()))
  then begin
    Lwt.abandon_wakeups ();
    finished ();
    run (call_hooks ())
  end

let () = drain_exit_hooks := drain
let () = at_exit drain
let at_exit f = ignore (Lwt_sequence.add_l f (exit_hooks ()))

module type Hooks = sig
  type 'return_value kind
  type hook

  val add_first : (unit -> unit kind) -> hook
  val add_last : (unit -> unit kind) -> hook
  val remove : hook -> unit
  val remove_all : unit -> unit
end

module type Hook_sequence = sig
  type 'return_value kind

  (* The sequence is per domain, so it is looked up each time a hook is added or
     removed, rather than once when the functor is applied. *)
  val sequence : unit -> (unit -> unit kind) Lwt_sequence.t
end

module Wrap_hooks (Sequence : Hook_sequence) = struct
  type 'a kind = 'a Sequence.kind
  type hook = (unit -> unit Sequence.kind) Lwt_sequence.node

  let add_first hook_fn =
    let hook_node = Lwt_sequence.add_l hook_fn (Sequence.sequence ()) in
    hook_node

  let add_last hook_fn =
    let hook_node = Lwt_sequence.add_r hook_fn (Sequence.sequence ()) in
    hook_node

  let remove hook_node = Lwt_sequence.remove hook_node

  let remove_all () =
    Lwt_sequence.iter_node_l Lwt_sequence.remove (Sequence.sequence ())
end

module Enter_iter_hooks = Wrap_hooks (struct
    type 'return_value kind = 'return_value

    let sequence = enter_iter_hooks
  end)

module Leave_iter_hooks = Wrap_hooks (struct
    type 'return_value kind = 'return_value

    let sequence = leave_iter_hooks
  end)

module Exit_hooks = Wrap_hooks (struct
    type 'return_value kind = 'return_value Lwt.t

    let sequence = exit_hooks
  end)
