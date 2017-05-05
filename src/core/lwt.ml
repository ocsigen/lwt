(* OCaml promise library
 * http://www.ocsigen.org/lwt
 * Copyright (C) 2005-2008 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *               2009-2012 Jérémie Dimino
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



module Storage_map = Map.Make(struct type t = int let compare = compare end)

type storage = (unit -> unit) Storage_map.t



type +'a t
type -'a u

module type Existential_promise = sig
  type a
  val promise : a t
end

type a_promise = (module Existential_promise)

let pack_promise (type x) t =
  let module M = struct type a = x let promise = t end in
  (module M : Existential_promise)

module type Existential_promise_list = sig
  type a
  val promise_list : a t list
end

type a_promise_list = (module Existential_promise_list)

let pack_promise_list (type x) l =
  let module M = struct type a = x let promise_list = l end in
  (module M : Existential_promise_list)



module Main_internal_types =
struct
  type 'a promise_state =
    | Resolved of 'a
    | Failed of exn
    | Pending of 'a callbacks
    | Unified_with of 'a promise

  and 'a promise = {
    mutable state : 'a promise_state;
  }

  and 'a callbacks = {
    mutable regular_callbacks : 'a regular_callback_list;
    mutable cancel_callbacks  : 'a cancel_callback_list;
    mutable how_to_cancel     : how_to_cancel;
    mutable cleanups_deferred : int;
  }

  and how_to_cancel =
    | Not_cancelable
    | Cancel_this_promise
    | Propagate_cancel_to_one of a_promise
    | Propagate_cancel_to_several of a_promise_list

  and 'a regular_callback_list =
    | Regular_callback_list_empty
    | Regular_callback_list_concat of
      'a regular_callback_list * 'a regular_callback_list
    | Regular_callback_list_explicitly_removable_callback of
      ('a promise_state -> unit) option ref
    | Regular_callback_list_implicitly_removed_callback of
      ('a promise_state -> unit)

  and 'a cancel_callback_list =
    | Cancel_callback_list_empty
    | Cancel_callback_list_concat of
      'a cancel_callback_list * 'a cancel_callback_list
    | Cancel_callback_list_callback of storage * (unit -> unit)
    | Cancel_callback_list_remove_sequence_node of 'a u Lwt_sequence.node
end
open Main_internal_types



module Public_types =
struct

external to_internal_promise : 'a t -> 'a promise = "%identity"
external to_public_promise : 'a promise -> 'a t = "%identity"
external to_public_resolver : 'a promise -> 'a u = "%identity"
external to_internal_resolver : 'a u -> 'a promise = "%identity"

type +'a result = ('a, exn) Result.result

  let state_of_result = function
    | Result.Ok x -> Resolved x
    | Result.Error e -> Failed e

let make_value v = Result.Ok v
let make_error exn = Result.Error exn

end
include Public_types



module Basic_helpers =
struct

let rec underlying t =
  match t.state with
    | Unified_with t' ->
      let t'' = underlying t' in if t'' != t' then t.state <- Unified_with t''; t''
    | Resolved _ | Failed _ | Pending _ -> t

let repr t = underlying (to_internal_promise t)

end
open Basic_helpers



module Sequence_associated_storage =
struct
  type 'a key = {
    id : int;
    mutable store : 'a option;
  }

  let next_key_id = ref 0

  let new_key () =
    let id = !next_key_id in
    next_key_id := id + 1;
    { id = id; store = None }

  let current_storage = ref Storage_map.empty

  let get key =
    try
      Storage_map.find key.id !current_storage ();
      let value = key.store in
      key.store <- None;
      value
    with Not_found ->
      None

  let with_value key value f =
    let save = !current_storage in
    let data =
      match value with
        | Some _ ->
            Storage_map.add key.id (fun () -> key.store <- value) save
        | None ->
            Storage_map.remove key.id save
    in
    current_storage := data;
    try
      let result = f () in
      current_storage := save;
      result
    with exn ->
      current_storage := save;
      raise exn
end
include Sequence_associated_storage



module Callbacks =
struct
  let concat_regular_callbacks l1 l2 =
    (match l1, l2 with
      | Regular_callback_list_empty, _ -> l2
      | _, Regular_callback_list_empty -> l1
      | _ -> Regular_callback_list_concat (l1, l2))
    [@ocaml.warning "-4"]

  let concat_cancel_callbacks l1 l2 =
    (match l1, l2 with
      | Cancel_callback_list_empty, _ -> l2
      | _, Cancel_callback_list_empty -> l1
      | _ -> Cancel_callback_list_concat (l1, l2))
    [@ocaml.warning "-4"]

  let rec clean_up_callback_cells = function
    | Regular_callback_list_explicitly_removable_callback { contents = None } ->
        Regular_callback_list_empty
    | Regular_callback_list_concat (l1, l2) ->
        concat_regular_callbacks (clean_up_callback_cells l1) (clean_up_callback_cells l2)
    | Regular_callback_list_empty
    | Regular_callback_list_explicitly_removable_callback _
    | Regular_callback_list_implicitly_removed_callback _ as ws ->
        ws

  let cleanup_throttle = 42

  let remove_waiters l =
    List.iter
      (fun t ->
         match (repr t).state with
           | Pending ({ regular_callbacks =
                   Regular_callback_list_explicitly_removable_callback _; _ } as sleeper) ->
               sleeper.regular_callbacks <- Regular_callback_list_empty
           | Pending ({regular_callbacks =
                   Regular_callback_list_empty
                 | Regular_callback_list_implicitly_removed_callback _
                 | Regular_callback_list_concat _; _} as sleeper) ->
               let removed = sleeper.cleanups_deferred + 1 in
               if removed > cleanup_throttle then begin
                 sleeper.cleanups_deferred <- 0;
                 sleeper.regular_callbacks <- clean_up_callback_cells sleeper.regular_callbacks
               end else
                 sleeper.cleanups_deferred <- removed
           | Resolved _ | Failed _ | Unified_with _ ->
               ())
      l

  let add_regular_callback_list_node callbacks node =
    callbacks.regular_callbacks <- (match callbacks.regular_callbacks with
                          | Regular_callback_list_empty -> node
                          | Regular_callback_list_implicitly_removed_callback _
                          | Regular_callback_list_explicitly_removable_callback _
                          | Regular_callback_list_concat _ as ws ->
                            Regular_callback_list_concat (node, ws))

  let add_implicitly_removed_callback callbacks f =
    add_regular_callback_list_node callbacks (Regular_callback_list_implicitly_removed_callback f)

  let add_explicitly_removable_callback_to_each_of ps f =
    let node = Regular_callback_list_explicitly_removable_callback f in
    List.iter
      (fun t ->
         match (repr t).state with
           | Pending sleeper ->
               add_regular_callback_list_node sleeper node
           | Resolved _ | Failed _ | Unified_with _ ->
               assert false)
      ps
end
open Callbacks



module Completion_loop =
struct
  let async_exception_hook =
    ref (fun exn ->
           prerr_string "Fatal error: exception ";
           prerr_string (Printexc.to_string exn);
           prerr_char '\n';
           Printexc.print_backtrace stderr;
           flush stderr;
           exit 2)

  let handle_with_async_exception_hook f v =
    try
      f v
    with exn ->
      !async_exception_hook exn

  exception Canceled

let rec run_waiters_rec state ws rem =
  match ws with
    | Regular_callback_list_empty ->
        run_waiters_rec_next state rem
    | Regular_callback_list_implicitly_removed_callback f ->
        f state;
        run_waiters_rec_next state rem
    | Regular_callback_list_explicitly_removable_callback { contents = None } ->
        run_waiters_rec_next state rem
    | Regular_callback_list_explicitly_removable_callback { contents = Some f } ->
        f state;
        run_waiters_rec_next state rem
    | Regular_callback_list_concat (ws1, ws2) ->
        run_waiters_rec state ws1 (ws2 :: rem)

and run_waiters_rec_next state rem =
  match rem with
    | [] ->
        ()
    | ws :: rem ->
        run_waiters_rec state ws rem

let rec run_cancel_handlers_rec chs rem =
  match chs with
    | Cancel_callback_list_empty ->
        run_cancel_handlers_rec_next rem
    | Cancel_callback_list_callback (data, f) ->
        current_storage := data;
        handle_with_async_exception_hook f ();
        run_cancel_handlers_rec_next rem
    | Cancel_callback_list_remove_sequence_node n ->
        Lwt_sequence.remove n;
        run_cancel_handlers_rec_next rem
    | Cancel_callback_list_concat (chs1, chs2) ->
        run_cancel_handlers_rec chs1 (chs2 :: rem)

and run_cancel_handlers_rec_next rem =
  match rem with
    | [] ->
        ()
    | chs :: rem ->
        run_cancel_handlers_rec chs rem

  let run_callbacks callbacks result =
    (match result with
       | Failed Canceled ->
           run_cancel_handlers_rec callbacks.cancel_callbacks []
       | Resolved _ | Failed _ | Pending _ | Unified_with _ ->
           ());
    run_waiters_rec result callbacks.regular_callbacks []

  let complete p result =
    let t = repr p in
    match t.state with
      | Pending sleeper ->
          t.state <- result;
          run_callbacks sleeper result
      | Resolved _ | Failed _ | Unified_with _ ->
          assert false

  let currently_in_completion_loop = ref false

module type A_queued_callbacks = sig
  type a
  val callbacks : a callbacks
  val state : a promise_state
end

let queued_callbacks = Queue.create ()

  let enter_completion_loop () =
    let snapshot = !current_storage in
    let already_wakening =
      if !currently_in_completion_loop then
        true
      else begin
        currently_in_completion_loop := true;
        false
      end
    in
    (already_wakening, snapshot)

  let leave_completion_loop (already_wakening, snapshot) =
    if not already_wakening then begin
      while not (Queue.is_empty queued_callbacks) do
        let closure = Queue.pop queued_callbacks in
        let module M = (val closure : A_queued_callbacks) in
        run_callbacks M.callbacks M.state
      done;
      currently_in_completion_loop := false;
      current_storage := snapshot
    end else
      current_storage := snapshot

  (* See https://github.com/ocsigen/lwt/issues/48. *)
  let abandon_wakeups () =
    if !currently_in_completion_loop then leave_completion_loop (false, Storage_map.empty)

  let run_in_completion_loop sleeper state =
    let ctx = enter_completion_loop () in
    run_callbacks sleeper state;
    leave_completion_loop ctx

  let wakeup_result r result =
    let t = underlying (to_internal_resolver r) in
    match t.state with
      | Pending sleeper ->
          let state = state_of_result result in
          t.state <- state;
          run_in_completion_loop sleeper state
      | Failed Canceled ->
          ()
      | Resolved _ | Failed _ | Unified_with _ ->
          invalid_arg "Lwt.wakeup_result"

  let wakeup r v = wakeup_result r (make_value v)
  let wakeup_exn r exn = wakeup_result r (make_error exn)

  let wakeup_later_result (type x) r result =
    let t = underlying (to_internal_resolver r) in
    match t.state with
      | Pending sleeper ->
          let state = state_of_result result in
          t.state <- state;
          if !currently_in_completion_loop then begin
            let module M = struct
              type a = x
              let callbacks = sleeper
              let state = state
            end in
            Queue.push (module M : A_queued_callbacks) queued_callbacks
          end else
            run_in_completion_loop sleeper state
      | Failed Canceled ->
          ()
      | Resolved _ | Failed _ | Unified_with _ ->
          invalid_arg "Lwt.wakeup_later_result"

  let wakeup_later r v = wakeup_later_result r (make_value v)
  let wakeup_later_exn r exn = wakeup_later_result r (make_error exn)

module type Packed_callbacks = sig
  type a
  val callbacks : a callbacks
end

type packed_callbacks = (module Packed_callbacks)

let pack_callbacks (type x) sleeper =
  let module M = struct type a = x let callbacks = sleeper end in
  (module M : Packed_callbacks)

  let cancel p =
    let state = Failed Canceled in
    let rec collect : 'a. packed_callbacks list -> 'a t -> packed_callbacks list = fun acc t ->
      let t = repr t in
      match t.state with
        | Pending ({ how_to_cancel; _ } as sleeper) -> begin
            match how_to_cancel with
              | Not_cancelable ->
                  acc
              | Cancel_this_promise ->
                  t.state <- state;
                  (pack_callbacks sleeper) :: acc
              | Propagate_cancel_to_one m ->
                  let module M = (val m : Existential_promise) in
                  collect acc M.promise
              | Propagate_cancel_to_several m ->
                  let module M = (val m : Existential_promise_list) in
                  List.fold_left collect acc M.promise_list
          end
        | Resolved _ | Failed _ | Unified_with _ ->
            acc
    in
    let sleepers = collect [] p in
    let ctx = enter_completion_loop () in
    List.iter
      (fun sleeper ->
         let module M = (val sleeper : Packed_callbacks) in
         run_cancel_handlers_rec M.callbacks.cancel_callbacks [];
         run_waiters_rec state M.callbacks.regular_callbacks [])
      sleepers;
    leave_completion_loop ctx
end
include Completion_loop



(* This function is redundant and will be removed in refactoring. *)
let fast_connect_if t state =
  let t = repr t in
  match t.state with
    | Pending sleeper ->
        t.state <- state;
        run_callbacks sleeper state
    | Resolved _ | Failed _ | Unified_with _ ->
        ()



module Trivial_promises =
struct
  let return v =
    to_public_promise { state = Resolved v }

  let state_return_unit = Resolved ()
  let return_unit = to_public_promise { state = state_return_unit }
  let return_none = return None
  let return_some x = return (Some x)
  let return_nil = return []
  let return_true = return true
  let return_false = return false
  let return_ok x = return (Result.Ok x)
  let return_error x = return (Result.Error x)

  let of_result result =
    to_public_promise { state = state_of_result result }

  let fail exn =
    to_public_promise { state = Failed exn }

  let fail_with msg =
    to_public_promise { state = Failed (Failure msg) }

  let fail_invalid_arg msg =
    to_public_promise { state = Failed (Invalid_argument msg) }
end
include Trivial_promises



module Pending_promises =
struct

let temp t =
  to_public_promise {
    state = Pending { how_to_cancel = Propagate_cancel_to_one (pack_promise (to_public_promise t));
                    regular_callbacks = Regular_callback_list_empty;
                    cleanups_deferred = 0;
                    cancel_callbacks = Cancel_callback_list_empty }
  }

let temp_many l =
  to_public_promise {
    state = Pending { how_to_cancel = Propagate_cancel_to_several (pack_promise_list l);
                    regular_callbacks = Regular_callback_list_empty;
                    cleanups_deferred = 0;
                    cancel_callbacks = Cancel_callback_list_empty }
  }

let wait_aux () = {
  state = Pending { how_to_cancel = Not_cancelable;
                  regular_callbacks = Regular_callback_list_empty;
                  cleanups_deferred = 0;
                  cancel_callbacks = Cancel_callback_list_empty }
}

  let wait () =
    let t = wait_aux () in
    (to_public_promise t, to_public_resolver t)

let task_aux () = {
  state = Pending { how_to_cancel = Cancel_this_promise;
                  regular_callbacks = Regular_callback_list_empty;
                  cleanups_deferred = 0;
                  cancel_callbacks = Cancel_callback_list_empty }
}

  let task () =
    let t = task_aux () in
    (to_public_promise t, to_public_resolver t)

  let add_task_r sequence =
    let sleeper = {
      how_to_cancel = Cancel_this_promise;
      regular_callbacks = Regular_callback_list_empty;
      cleanups_deferred = 0;
      cancel_callbacks = Cancel_callback_list_empty
    } in
    let t = { state = Pending sleeper } in
    let node = Lwt_sequence.add_r (to_public_resolver t) sequence in
    sleeper.cancel_callbacks <- Cancel_callback_list_remove_sequence_node node;
    to_public_promise t

  let add_task_l sequence =
    let sleeper = {
      how_to_cancel = Cancel_this_promise;
      regular_callbacks = Regular_callback_list_empty;
      cleanups_deferred = 0;
      cancel_callbacks = Cancel_callback_list_empty
    }in
    let t = { state = Pending sleeper } in
    let node = Lwt_sequence.add_l (to_public_resolver t) sequence in
    sleeper.cancel_callbacks <- Cancel_callback_list_remove_sequence_node node;
    to_public_promise t

  let waiter_of_wakener r = to_public_promise (to_internal_resolver r)

  let no_cancel p =
    match (repr p).state with
      | Pending sleeper ->
          let res = to_public_promise (wait_aux ()) in
          add_implicitly_removed_callback sleeper (complete res);
          res
      | Resolved _ | Failed _ ->
          p
      | Unified_with _ ->
          assert false
end
include Pending_promises



module Sequential_composition =
struct
  let unify t1 t2 =
    let t1 = repr t1 and t2 = repr t2 in
    match t1.state with
      | Pending sleeper1 ->
          if t1 == t2 then
            ()
          else begin
            match t2.state with
              | Pending sleeper2 ->
                  t2.state <- Unified_with t1;

                  sleeper1.how_to_cancel <- sleeper2.how_to_cancel;

                  let waiters =
                    concat_regular_callbacks sleeper1.regular_callbacks sleeper2.regular_callbacks
                  and removed =
                    sleeper1.cleanups_deferred + sleeper2.cleanups_deferred in
                  if removed > cleanup_throttle then begin
                    sleeper1.cleanups_deferred <- 0;
                    sleeper1.regular_callbacks <- clean_up_callback_cells waiters
                  end else begin
                    sleeper1.cleanups_deferred <- removed;
                    sleeper1.regular_callbacks <- waiters
                  end;
                  sleeper1.cancel_callbacks <-
                    concat_cancel_callbacks sleeper1.cancel_callbacks sleeper2.cancel_callbacks
              | Resolved _ | Failed _ | Unified_with _ as state2 ->
                  t1.state <- state2;
                  run_callbacks sleeper1 state2
          end
      | Resolved _ | Failed _ | Unified_with _ ->
           assert false

  let bind p f =
    let t = repr p in
    match t.state with
      | Resolved v ->
          f v
      | Failed _ as state ->
          to_public_promise { state }
      | Pending sleeper ->
          let res = temp t in
          let data = !current_storage in
          add_implicitly_removed_callback sleeper
            (function
               | Resolved v ->
                current_storage := data; unify res (try f v with exn -> fail exn)
               | Failed _ as state -> complete res state
               | Pending _ | Unified_with _ -> assert false);
          res
      | Unified_with _ ->
          assert false

  let backtrace_bind add_loc p f =
    let t = repr p in
    match t.state with
      | Resolved v ->
          f v
      | Failed exn ->
          to_public_promise { state = Failed(add_loc exn) }
      | Pending sleeper ->
          let res = temp t in
          let data = !current_storage in
          add_implicitly_removed_callback sleeper
            (function
               | Resolved v ->
                 current_storage := data; unify res (try f v with exn -> fail (add_loc exn))
               | Failed exn -> complete res (Failed(add_loc exn))
               | Pending _ | Unified_with _ -> assert false);
          res
      | Unified_with _ ->
          assert false

let (>>=) t f = bind t f
let (=<<) f t = bind t f

  let map f p =
    let t = repr p in
    match t.state with
      | Resolved v ->
          to_public_promise { state = try Resolved (f v) with exn -> Failed exn }
      | Failed _ as state ->
          to_public_promise { state }
      | Pending sleeper ->
          let res = temp t in
          let data = !current_storage in
          add_implicitly_removed_callback sleeper
            (function
               | Resolved v ->
                 current_storage := data;
                 complete res (try Resolved (f v) with exn -> Failed exn)
               | Failed _ as state -> complete res state
               | Pending _ | Unified_with _ -> assert false);
          res
      | Unified_with _ ->
          assert false

let (>|=) t f = map f t
let (=|<) f t = map f t

  let catch f h =
    let t = repr (try f () with exn -> fail exn) in
    match t.state with
      | Resolved _ ->
          to_public_promise t
      | Failed exn ->
          h exn
      | Pending sleeper ->
          let res = temp t in
          let data = !current_storage in
          add_implicitly_removed_callback sleeper
            (function
               | Resolved _ as state -> complete res state
               | Failed exn ->
                 current_storage := data; unify res (try h exn with exn -> fail exn)
               | Pending _ | Unified_with _ -> assert false);
          res
      | Unified_with _ ->
          assert false

  let backtrace_catch add_loc f h =
    let t = repr (try f () with exn -> fail exn) in
    match t.state with
      | Resolved _ ->
          to_public_promise t
      | Failed exn ->
          h (add_loc exn)
      | Pending sleeper ->
          let res = temp t in
          let data = !current_storage in
          add_implicitly_removed_callback sleeper
            (function
               | Resolved _ as state -> complete res state
               | Failed exn ->
                 current_storage := data; unify res (try h exn with exn -> fail (add_loc exn))
               | Pending _ | Unified_with _ -> assert false);
          res
      | Unified_with _ ->
          assert false

  let try_bind f f' h =
    let t = repr (try f () with exn -> fail exn) in
    match t.state with
      | Resolved v ->
          f' v
      | Failed exn ->
          h exn
      | Pending sleeper ->
          let res = temp t in
          let data = !current_storage in
          add_implicitly_removed_callback sleeper
            (function
               | Resolved v -> current_storage := data; unify res (try f' v with exn -> fail exn)
               | Failed exn -> current_storage := data; unify res (try h exn with exn -> fail exn)
               | Pending _ | Unified_with _ -> assert false);
          res
      | Unified_with _ ->
          assert false

  let backtrace_try_bind add_loc f f' h =
    let t = repr (try f () with exn -> fail exn) in
    match t.state with
      | Resolved v ->
          f' v
      | Failed exn ->
          h (add_loc exn)
      | Pending sleeper ->
          let res = temp t in
          let data = !current_storage in
          add_implicitly_removed_callback sleeper
            (function
               | Resolved v ->
                 current_storage := data; unify res (try f' v with exn -> fail (add_loc exn))
               | Failed exn ->
                 current_storage := data; unify res (try h exn with exn -> fail (add_loc exn))
               | Pending _ | Unified_with _ -> assert false);
          res
      | Unified_with _ ->
          assert false

  let finalize f f' =
    try_bind f
      (fun x -> f' () >>= fun () -> return x)
      (fun e -> f' () >>= fun () -> fail e)

  let backtrace_finalize add_loc f f' =
    backtrace_try_bind add_loc f
      (fun x -> f' () >>= fun () -> return x)
      (fun e -> f' () >>= fun () -> fail (add_loc e))

  let on_cancel p f =
    match (repr p).state with
      | Pending sleeper ->
          let handler = Cancel_callback_list_callback (!current_storage, f) in
          sleeper.cancel_callbacks <- (
            match sleeper.cancel_callbacks with
              | Cancel_callback_list_empty -> handler
              | Cancel_callback_list_callback _
              | Cancel_callback_list_remove_sequence_node _
              | Cancel_callback_list_concat _ as chs ->
                Cancel_callback_list_concat (handler, chs)
          )
      | Failed Canceled ->
          handle_with_async_exception_hook f ()
      | Resolved _ | Failed _ | Unified_with _ ->
          ()

  let on_success p f =
    match (repr p).state with
      | Resolved v ->
          handle_with_async_exception_hook f v
      | Failed _ ->
          ()
      | Pending sleeper ->
          let data = !current_storage in
          add_implicitly_removed_callback sleeper
            (function
               | Resolved v -> current_storage := data; handle_with_async_exception_hook f v
               | Failed _ -> ()
               | Pending _ | Unified_with _ -> assert false)
      | Unified_with _ ->
          assert false

  let on_failure p f =
    match (repr p).state with
      | Resolved _ ->
          ()
      | Failed exn ->
          handle_with_async_exception_hook f exn
      | Pending sleeper ->
          let data = !current_storage in
          add_implicitly_removed_callback sleeper
            (function
               | Resolved _ -> ()
               | Failed exn -> current_storage := data; handle_with_async_exception_hook f exn
               | Pending _ | Unified_with _ -> assert false)
      | Unified_with _ ->
          assert false

  let on_termination p f =
    match (repr p).state with
      | Resolved _
      | Failed _ ->
          handle_with_async_exception_hook f ()
      | Pending sleeper ->
          let data = !current_storage in
          add_implicitly_removed_callback sleeper
            (function
               | Resolved _
               | Failed _ -> current_storage := data; handle_with_async_exception_hook f ()
               | Pending _ | Unified_with _ -> assert false)
      | Unified_with _ ->
          assert false

  let on_any p f g =
    match (repr p).state with
      | Resolved v ->
          handle_with_async_exception_hook f v
      | Failed exn ->
          handle_with_async_exception_hook g exn
      | Pending sleeper ->
          let data = !current_storage in
          add_implicitly_removed_callback sleeper
            (function
               | Resolved v -> current_storage := data; handle_with_async_exception_hook f v
               | Failed exn -> current_storage := data; handle_with_async_exception_hook g exn
               | Pending _ | Unified_with _ -> assert false)
      | Unified_with _ ->
          assert false
end
include Sequential_composition



module Concurrent_composition =
struct
  let async f =
    let t = repr (try f () with exn -> fail exn) in
    match t.state with
      | Resolved _ ->
          ()
      | Failed exn ->
          !async_exception_hook exn
      | Pending sleeper ->
          add_implicitly_removed_callback sleeper
            (function
               | Resolved _ -> ()
               | Failed exn -> !async_exception_hook exn
               | Pending _ | Unified_with _ -> assert false)
      | Unified_with _ ->
          assert false

  let ignore_result p =
    match (repr p).state with
      | Resolved _ ->
          ()
      | Failed e ->
          raise e
      | Pending sleeper ->
          add_implicitly_removed_callback sleeper
            (function
               | Resolved _ -> ()
               | Failed exn -> !async_exception_hook exn
               | Pending _ | Unified_with _ -> assert false)
      | Unified_with _ ->
          assert false

  let join ps =
    let res = temp_many ps
    and sleeping = ref 0
    and return_state = ref state_return_unit in
    let handle_result state =
      begin
        match !return_state, state with
          | Resolved _, Failed _ -> return_state := state
          | _ -> ()
      end [@ocaml.warning "-4"];
      decr sleeping;
      if !sleeping = 0 then complete res !return_state
    in
    let rec init = function
      | [] ->
          if !sleeping = 0 then
            to_public_promise { state = !return_state }
          else
            res
      | t :: rest ->
          match (repr t).state with
            | Pending sleeper ->
                incr sleeping;
                add_implicitly_removed_callback sleeper handle_result;
                init rest
            | Failed _ as state -> begin
                match !return_state with
                  | Resolved _ ->
                      return_state := state;
                      init rest
                  | Failed _ | Pending _ | Unified_with _ ->
                      init rest
              end
            | Resolved _ | Unified_with _ ->
                init rest
    in
    init ps

  let count_completed_promises_in ps =
    List.fold_left (fun acc x ->
      match (repr x).state with
      | Pending _ -> acc
      | Resolved _ | Failed _ | Unified_with _ -> acc + 1) 0 ps

  let rec nth_completed ps n =
    match ps with
      | [] ->
          assert false
      | t :: l ->
          match (repr t).state with
            | Pending _ ->
                nth_completed l n
            | Resolved _ | Failed _ | Unified_with _ ->
                if n > 0 then
                  nth_completed l (n - 1)
                else
                  t

  let rec nth_completed_and_cancel_pending ps n =
    match ps with
      | [] ->
          assert false
      | t :: l ->
          match (repr t).state with
            | Pending _ ->
                cancel t;
                nth_completed_and_cancel_pending l n
            | Resolved _ | Failed _ | Unified_with _ ->
                if n > 0 then
                  nth_completed_and_cancel_pending l (n - 1)
                else begin
                  List.iter cancel l;
                  t
                end

  (* The PRNG state is initialized with a constant to make non-IO-based
     programs deterministic. *)
  let prng = lazy (Random.State.make [||])

  let choose ps =
    let ready = count_completed_promises_in ps in
    if ready > 0 then
      if ready = 1 then
        nth_completed ps 0
      else
        nth_completed ps (Random.State.int (Lazy.force prng) ready)
    else begin
      let res = temp_many ps in
      let rec waiter = ref (Some handle_result)
      and handle_result state =
        waiter := None;
        remove_waiters ps;
        complete res state
      in
      add_explicitly_removable_callback_to_each_of ps waiter;
      res
    end

  let pick ps =
    let ready = count_completed_promises_in ps in
    if ready > 0 then
      if ready = 1 then
        nth_completed_and_cancel_pending ps 0
      else
        nth_completed_and_cancel_pending ps (Random.State.int (Lazy.force prng) ready)
    else begin
      let res = temp_many ps in
      let rec waiter = ref (Some handle_result)
      and handle_result state =
        waiter := None;
        remove_waiters ps;
        List.iter cancel ps;
        complete res state
      in
      add_explicitly_removable_callback_to_each_of ps waiter;
      res
    end

  let rec finish_nchoose_or_npick_after_pending to_complete results = function
    | [] ->
        complete to_complete (Resolved (List.rev results))
    | t :: l ->
        match (repr t).state with
          | Resolved x ->
              finish_nchoose_or_npick_after_pending to_complete (x :: results) l
          | Failed _ as state ->
              complete to_complete state
          | Pending _ | Unified_with _ ->
              finish_nchoose_or_npick_after_pending to_complete results l

let nchoose_sleep ps =
  let res = temp_many ps in
  let rec waiter = ref (Some handle_result)
  and handle_result _state =
    waiter := None;
    remove_waiters ps;
    finish_nchoose_or_npick_after_pending res [] ps
  in
  add_explicitly_removable_callback_to_each_of ps waiter;
  res

  let nchoose ps =
    let rec init = function
      | [] ->
          nchoose_sleep ps
      | t :: l ->
          match (repr t).state with
            | Resolved x ->
                collect [x] l
            | Failed _ as state ->
                to_public_promise { state }
            | Pending _ | Unified_with _ ->
                init l
    and collect acc = function
      | [] ->
          return (List.rev acc)
      | t :: l ->
          match (repr t).state with
            | Resolved x ->
                collect (x :: acc) l
            | Failed _ as state ->
                to_public_promise { state }
            | Pending _ | Unified_with _ ->
                collect acc l
    in
    init ps

let npick_sleep ps =
  let res = temp_many ps in
  let rec waiter = ref (Some handle_result)
  and handle_result _state =
    waiter := None;
    remove_waiters ps;
    List.iter cancel ps;
    finish_nchoose_or_npick_after_pending res [] ps
  in
  add_explicitly_removable_callback_to_each_of ps waiter;
  res

  let npick ps =
    let rec init = function
      | [] ->
          npick_sleep ps
      | t :: l ->
          match (repr t).state with
            | Resolved x ->
                collect [x] l
            | Failed _ as state ->
                List.iter cancel ps;
                to_public_promise { state }
            | Pending _ | Unified_with _ ->
                init l
    and collect acc = function
      | [] ->
          List.iter cancel ps;
          return (List.rev acc)
      | t :: l ->
          match (repr t).state with
            | Resolved x ->
                collect (x :: acc) l
            | Failed _ as state ->
                List.iter cancel ps;
                to_public_promise { state }
            | Pending _ | Unified_with _ ->
                collect acc l
    in
    init ps

let rec nchoose_split_terminate res acc_terminated acc_sleeping = function
  | [] ->
      complete res (Resolved (List.rev acc_terminated, List.rev acc_sleeping))
  | t :: l ->
      match (repr t).state with
        | Resolved x ->
            nchoose_split_terminate res (x :: acc_terminated) acc_sleeping l
        | Failed _ as state ->
            complete res state
        | Pending _ | Unified_with _ ->
            nchoose_split_terminate res acc_terminated (t :: acc_sleeping) l

let nchoose_split_sleep l =
  let res = temp_many l in
  let rec waiter = ref (Some handle_result)
  and handle_result _state =
    waiter := None;
    remove_waiters l;
    nchoose_split_terminate res [] [] l
  in
  add_explicitly_removable_callback_to_each_of l waiter;
  res

  let nchoose_split ps =
    let rec init acc_sleeping = function
      | [] ->
          nchoose_split_sleep ps
      | t :: l ->
          match (repr t).state with
            | Resolved x ->
                collect [x] acc_sleeping l
            | Failed _ as state ->
                to_public_promise { state }
            | Pending _ | Unified_with _ ->
                init (t :: acc_sleeping) l
    and collect acc_terminated acc_sleeping = function
      | [] ->
          return (List.rev acc_terminated, acc_sleeping)
      | t :: l ->
          match (repr t).state with
            | Resolved x ->
                collect (x :: acc_terminated) acc_sleeping l
            | Failed _ as state ->
                to_public_promise { state }
            | Pending _ | Unified_with _ ->
                collect acc_terminated (t :: acc_sleeping) l
    in
    init [] ps

let protected p =
  match (repr p).state with
    | Pending _ ->
        let res = to_public_promise (task_aux ()) in
        let rec waiter_cell = ref (Some waiter)
        and waiter state = fast_connect_if res state in
        add_explicitly_removable_callback_to_each_of [p] waiter_cell;
        on_cancel res (fun () ->
          waiter_cell := None;
          remove_waiters [p]);
        res

    | Resolved _ | Failed _ ->
        p
    | Unified_with _ ->
        assert false

let ( <?> ) t1 t2 = choose [t1; t2]
let ( <&> ) t1 t2 = join [t1; t2]

end
include Concurrent_composition



module Miscellaneous =
struct

module State = struct
  type 'a state =
    | Return of 'a
    | Fail of exn
    | Sleep
end

  let state t = match (repr t).state with
    | Resolved v -> State.Return v
    | Failed exn -> State.Fail exn
    | Pending _ -> State.Sleep
    | Unified_with _ -> assert false

  include State

let rec is_sleeping_rec t =
  match t.state with
    | Resolved _ | Failed _ ->
        false
    | Pending _ ->
        true
    | Unified_with t ->
        is_sleeping_rec t

  let is_sleeping p = is_sleeping_rec (to_internal_promise p)

  let poll p =
    match (repr p).state with
      | Failed e -> raise e
      | Resolved v -> Some v
      | Pending _ -> None
      | Unified_with _ -> assert false

  let apply f x = try f x with e -> fail e

  let wrap f = try return (f ()) with exn -> fail exn

  let wrap1 f x1 = try return (f x1) with exn -> fail exn
  let wrap2 f x1 x2 = try return (f x1 x2) with exn -> fail exn
  let wrap3 f x1 x2 x3 = try return (f x1 x2 x3) with exn -> fail exn
  let wrap4 f x1 x2 x3 x4 = try return (f x1 x2 x3 x4) with exn -> fail exn
  let wrap5 f x1 x2 x3 x4 x5 = try return (f x1 x2 x3 x4 x5) with exn -> fail exn
  let wrap6 f x1 x2 x3 x4 x5 x6 = try return (f x1 x2 x3 x4 x5 x6) with exn -> fail exn
  let wrap7 f x1 x2 x3 x4 x5 x6 x7 = try return (f x1 x2 x3 x4 x5 x6 x7) with exn -> fail exn



  let pause_hook = ref ignore

  let paused = Lwt_sequence.create ()
  let paused_count = ref 0

  let pause () =
    let waiter = add_task_r paused in
    incr paused_count;
    !pause_hook !paused_count;
    waiter

  let wakeup_paused () =
    if Lwt_sequence.is_empty paused then
      paused_count := 0
    else begin
      let tmp = Lwt_sequence.create () in
      Lwt_sequence.transfer_r paused tmp;
      paused_count := 0;
      Lwt_sequence.iter_l (fun wakener -> wakeup wakener ()) tmp
    end

  let register_pause_notifier f = pause_hook := f

  let paused_count () = !paused_count
end
include Miscellaneous



module Infix = struct
  let (>>=) = (>>=)
  let (=<<) = (=<<)
  let (>|=) = (>|=)
  let (=|<) = (=|<)
  let (<&>) = (<&>)
  let (<?>) = (<?>)
end
