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



type underlying
type proxy

type completed
type pending

module Main_internal_types =
struct
  type ('a, 'u, 'c) promise = {
    mutable state : ('a, 'u, 'c) state;
  }

  and (_, _, _) state =
    | Resolved     : 'a                  -> ('a, underlying, completed) state
    | Failed       : exn                 -> ( _, underlying, completed) state
    | Pending      : 'a callbacks        -> ('a, underlying, pending)   state
    | Unified_with : ('a, _, 'c) promise -> ('a, proxy,      'c)        state

  and 'a callbacks = {
    mutable regular_callbacks : 'a regular_callback_list;
    mutable cancel_callbacks  : 'a cancel_callback_list;
    mutable how_to_cancel     : how_to_cancel;
    mutable cleanups_deferred : int;
  }

  and 'a regular_callback = in_completion_loop -> ('a, underlying, completed) state -> unit

  and cancel_callback = in_completion_loop -> unit

  and in_completion_loop   (* = unit. *)

  and how_to_cancel =
    | Not_cancelable              :                           how_to_cancel
    | Cancel_this_promise         :                           how_to_cancel
    | Propagate_cancel_to_one     : (_, _, _) promise      -> how_to_cancel
    | Propagate_cancel_to_several : (_, _, _) promise list -> how_to_cancel

  and 'a regular_callback_list =
    | Regular_callback_list_empty
    | Regular_callback_list_concat of
      'a regular_callback_list * 'a regular_callback_list
    | Regular_callback_list_implicitly_removed_callback of
      'a regular_callback
    | Regular_callback_list_explicitly_removable_callback of
      'a regular_callback option ref

  and _ cancel_callback_list =
    | Cancel_callback_list_empty :
      _ cancel_callback_list
    | Cancel_callback_list_concat :
      'a cancel_callback_list * 'a cancel_callback_list ->
        'a cancel_callback_list
    | Cancel_callback_list_callback :
      storage * cancel_callback ->
        _ cancel_callback_list
    | Cancel_callback_list_remove_sequence_node :
      ('a, _, _) promise Lwt_sequence.node ->
        'a cancel_callback_list
end
open Main_internal_types



module Public_types =
struct
  let to_public_promise : ('a, _, _) promise -> 'a t = Obj.magic
  let to_public_resolver : ('a, _, _) promise -> 'a u = Obj.magic

  type _ packed_promise =
    | Internal : ('a, _, _) promise -> 'a packed_promise
    [@@ocaml.unboxed]

  let to_internal_promise (p : 'a t) : 'a packed_promise =
    Internal (Obj.magic p)
  let to_internal_resolver (r : 'a u) : 'a packed_promise =
    Internal (Obj.magic r)

  type +'a lwt_result = ('a, exn) Result.result

  let state_of_result = function
    | Result.Ok x -> Resolved x
    | Result.Error exn -> Failed exn
end
include Public_types



module Basic_helpers =
struct
  let identical p1 p2 =
    (to_public_promise p1) == (to_public_promise p2)

  let rec underlying
      : 'u 'c. ('a, 'u, 'c) promise -> ('a, underlying, 'c) promise =
    fun
      (type u)
      (type c)
      (p : ('a, u, c) promise) ->

    match p.state with
    | Resolved _ -> (p : (_, underlying, _) promise)
    | Failed _ -> p
    | Pending _ -> p
    | Unified_with p' ->
      let p'' = underlying p' in
      if not (identical p'' p') then
        p.state <- Unified_with p'';
      p''



  type ('a, 'u, 'c) state_changed =
    | State_may_have_changed of ('a, 'u, 'c) promise
    [@@ocaml.unboxed]

  let set_promise_state p state =
    let p : (_, _, _) promise = Obj.magic p in
    p.state <- state;
    State_may_have_changed p



  type 'a may_now_be_proxy =
    | State_may_now_be_pending_proxy :
      ('a, _, pending) promise -> 'a may_now_be_proxy
    [@@ocaml.unboxed]

  let may_now_be_proxy p = State_may_now_be_pending_proxy p
end
open Basic_helpers



module Sequence_associated_storage =
struct
  type 'v key = {
    id : int;
    mutable value : 'v option;
  }

  let next_key_id = ref 0

  let new_key () =
    let id = !next_key_id in
    next_key_id := id + 1;
    { id = id; value = None }

  let current_storage = ref Storage_map.empty

  let get key =
    try
      Storage_map.find key.id !current_storage ();
      let value = key.value in
      key.value <- None;
      value
    with Not_found ->
      None

  let with_value key value f =
    let save = !current_storage in
    let data =
      match value with
        | Some _ ->
            Storage_map.add key.id (fun () -> key.value <- value) save
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

  let clear_explicitly_removable_callback_cell cell ~originally_added_to:ps =
    cell := None;

    List.iter
      (fun p ->
      let Internal p = to_internal_promise p in
      match (underlying p).state with
           | Pending ({ regular_callbacks =
                   Regular_callback_list_explicitly_removable_callback _; _ } as callbacks) ->
               callbacks.regular_callbacks <- Regular_callback_list_empty
           | Pending ({regular_callbacks =
                   Regular_callback_list_empty
                 | Regular_callback_list_implicitly_removed_callback _
                 | Regular_callback_list_concat _; _} as callbacks) ->
               let removed = callbacks.cleanups_deferred + 1 in
               if removed > cleanup_throttle then begin
                 callbacks.cleanups_deferred <- 0;
                 callbacks.regular_callbacks <- clean_up_callback_cells callbacks.regular_callbacks
               end else
                 callbacks.cleanups_deferred <- removed
           | Resolved _ -> ()
           | Failed _ -> ())
      ps

  let add_regular_callback_list_node callbacks node =
    callbacks.regular_callbacks <- (match callbacks.regular_callbacks with
                          | Regular_callback_list_empty -> node
                          | Regular_callback_list_implicitly_removed_callback _
                          | Regular_callback_list_explicitly_removable_callback _
                          | Regular_callback_list_concat _ as existing ->
                            Regular_callback_list_concat (node, existing))

  let add_implicitly_removed_callback callbacks f =
    add_regular_callback_list_node callbacks (Regular_callback_list_implicitly_removed_callback f)

  let add_explicitly_removable_callback_and_give_cell ps f =
    let rec cell = ref (Some self_removing_callback_wrapper)
    and self_removing_callback_wrapper result =
      clear_explicitly_removable_callback_cell cell ~originally_added_to:ps;
      f result
    in

    let node = Regular_callback_list_explicitly_removable_callback cell in
    ps |> List.iter (fun p ->
      let Internal p = to_internal_promise p in
      match (underlying p).state with
      | Pending callbacks -> add_regular_callback_list_node callbacks node
      | Resolved _ -> assert false
      | Failed _ -> assert false);

    cell

  let add_explicitly_removable_callback_to_each_of ps f =
    ignore (add_explicitly_removable_callback_and_give_cell ps f)

  let add_explicitly_removable_callback_and_give_remove_function ps f =
    let cell = add_explicitly_removable_callback_and_give_cell ps f in
    fun () ->
      clear_explicitly_removable_callback_cell cell ~originally_added_to:ps

  let add_cancel_callback callbacks f =
    (* Ugly cast :( *)
    let cast_cancel_callback : (unit -> unit) -> (in_completion_loop -> unit) =
      Obj.magic in
    let f = cast_cancel_callback f in

    let node = Cancel_callback_list_callback (!current_storage, f) in

    callbacks.cancel_callbacks <-
      match callbacks.cancel_callbacks with
      | Cancel_callback_list_empty ->
        node

      | Cancel_callback_list_callback _
      | Cancel_callback_list_remove_sequence_node _
      | Cancel_callback_list_concat _ ->
        Cancel_callback_list_concat (node, callbacks.cancel_callbacks)

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

  let run_callbacks in_completion_loop callbacks result =
    let run_cancel_callbacks fs =
      let rec iter_callback_list fs rest =
        match fs with
        | Cancel_callback_list_empty ->
          iter_list rest
        | Cancel_callback_list_callback (storage, f) ->
          current_storage := storage;
          handle_with_async_exception_hook f in_completion_loop;
          iter_list rest
        | Cancel_callback_list_remove_sequence_node node ->
          Lwt_sequence.remove node;
          iter_list rest
        | Cancel_callback_list_concat (fs, fs') ->
          iter_callback_list fs (fs'::rest)

      and iter_list rest =
        match rest with
        | [] -> ()
        | fs::rest -> iter_callback_list fs rest

      in

      iter_callback_list fs []
    in

    let run_regular_callbacks fs =
      let rec iter_callback_list fs rest =
        match fs with
        | Regular_callback_list_empty ->
          iter_list rest
        | Regular_callback_list_implicitly_removed_callback f ->
          f in_completion_loop result;
          iter_list rest
        | Regular_callback_list_explicitly_removable_callback
            {contents = None} ->
          iter_list rest
        | Regular_callback_list_explicitly_removable_callback
            {contents = Some f} ->
          f in_completion_loop result;
          iter_list rest
        | Regular_callback_list_concat (fs, fs') ->
          iter_callback_list fs (fs'::rest)

      and iter_list rest =
        match rest with
        | [] -> ()
        | fs::rest -> iter_callback_list fs rest

      in

      iter_callback_list fs []
    in

    (* Pattern matching is much faster than polymorphic comparison. *)
    let is_canceled =
      match result with
      | Failed Canceled -> true
      | Failed _ -> false
      | Resolved _ -> false
    in
    if is_canceled then
      run_cancel_callbacks callbacks.cancel_callbacks;
    run_regular_callbacks callbacks.regular_callbacks

  let complete in_completion_loop p result =
    let Pending callbacks = p.state in
    let p = set_promise_state p result in
    run_callbacks in_completion_loop callbacks result;
    p

  let currently_in_completion_loop = ref false

  type queued_callbacks =
    Queued : ('a callbacks * ('a, underlying, completed) state) -> queued_callbacks
    [@@ocaml.unboxed]

  let queued_callbacks : queued_callbacks Queue.t = Queue.create ()

  let enter_completion_loop () =
    let storage_snapshot = !current_storage in
    let already_wakening =
      if !currently_in_completion_loop then
        true
      else begin
        currently_in_completion_loop := true;
        false
      end
    in
    (already_wakening, storage_snapshot)

  let leave_completion_loop
      in_completion_loop
      already_wakening
      storage_snapshot =

    if not already_wakening then begin
      while not (Queue.is_empty queued_callbacks) do
        let Queued (callbacks, result) = Queue.pop queued_callbacks in
        run_callbacks in_completion_loop callbacks result
      done;
      currently_in_completion_loop := false;
      current_storage := storage_snapshot
    end else
      current_storage := storage_snapshot

  (* See https://github.com/ocsigen/lwt/issues/48. *)
  let abandon_wakeups () =
    if !currently_in_completion_loop then
      let in_completion_loop : in_completion_loop = Obj.magic () in
      leave_completion_loop in_completion_loop false Storage_map.empty

  let run_in_completion_loop (f : in_completion_loop -> unit) : unit =
    let top_level_entry, storage_snapshot = enter_completion_loop () in
    let in_completion_loop : in_completion_loop = Obj.magic () in
    f in_completion_loop;
    leave_completion_loop in_completion_loop top_level_entry storage_snapshot

  let wakeup_result r result =
    let Internal p = to_internal_resolver r in
    let p = underlying p in

    match p.state with
      | Pending callbacks ->
      let result = state_of_result result in
      let State_may_have_changed p = set_promise_state p result in
      ignore p;
      run_in_completion_loop (fun in_completion_loop ->
        run_callbacks in_completion_loop callbacks result)
      | Failed Canceled ->
          ()
      | Resolved _ ->
        invalid_arg "Lwt.wakeup_result"
      | Failed _ ->
        invalid_arg "Lwt.wakeup_result"

  let wakeup r v = wakeup_result r (Result.Ok v)
  let wakeup_exn r exn = wakeup_result r (Result.Error exn)

  let wakeup_later_result r result =
    let Internal p = to_internal_resolver r in
    let p = underlying p in

    match p.state with
      | Pending callbacks ->
      let result = state_of_result result in
      let State_may_have_changed p = set_promise_state p result in
      ignore p;
          if !currently_in_completion_loop then begin
          Queue.push (Queued (callbacks, result)) queued_callbacks
          end else
          run_in_completion_loop (fun in_completion_loop ->
            run_callbacks in_completion_loop callbacks result)
      | Failed Canceled ->
          ()
      | Resolved _ ->
          invalid_arg "Lwt.wakeup_later_result"
      | Failed _ ->
          invalid_arg "Lwt.wakeup_later_result"

  let wakeup_later r v = wakeup_later_result r (Result.Ok v)
  let wakeup_later_exn r exn = wakeup_later_result r (Result.Error exn)

  type packed_callbacks =
    | Packed : _ callbacks -> packed_callbacks
    [@@ocaml.unboxed]

  let cancel p =
    let canceled_result = Failed Canceled in

      let rec cancel_and_collect_callbacks :
          'a 'u 'c. packed_callbacks list -> ('a, 'u, 'c) promise ->
            packed_callbacks list =
          fun (type c) callbacks_accumulator (p : (_, _, c) promise) ->

        let p = underlying p in
      match p.state with
        | Pending ({ how_to_cancel; _ } as callbacks) -> begin
            match how_to_cancel with
              | Not_cancelable ->
            callbacks_accumulator
              | Cancel_this_promise ->
            let State_may_have_changed p = set_promise_state p canceled_result in
            ignore p;
            (Packed callbacks)::callbacks_accumulator
              | Propagate_cancel_to_one p ->
            cancel_and_collect_callbacks callbacks_accumulator p
              | Propagate_cancel_to_several ps ->
            List.fold_left cancel_and_collect_callbacks callbacks_accumulator ps
          end
        | Resolved _ ->
          callbacks_accumulator
        | Failed _ ->
          callbacks_accumulator
    in

    let Internal p = to_internal_promise p in
    let callbacks = cancel_and_collect_callbacks [] p in

    run_in_completion_loop (fun in_completion_loop ->
      callbacks |> List.iter (fun (Packed callbacks) ->
        run_callbacks in_completion_loop callbacks canceled_result))
end
include Completion_loop



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
  let new_pending ~how_to_cancel =
    let state =
      Pending {
        regular_callbacks = Regular_callback_list_empty;
        cancel_callbacks = Cancel_callback_list_empty;
        how_to_cancel;
        cleanups_deferred = 0;
      }
    in
    {state}

  let propagate_cancel_to_several ps =
    (* Using a dirty cast here to avoid rebuilding the list :( Not bothering
       with the invariants, because [Propagate_cancel_to_several] packs them,
       and code that matches on [Propagate_cancel_to_several] doesn't care about
       them anyway. *)
    let cast_promise_list : 'a t list -> ('a, _, _) promise list = Obj.magic in
    Propagate_cancel_to_several (cast_promise_list ps)



  let wait () =
    let p = new_pending ~how_to_cancel:Not_cancelable in
    to_public_promise p, to_public_resolver p

  let task () =
    let p = new_pending ~how_to_cancel:Cancel_this_promise in
    to_public_promise p, to_public_resolver p



  let cast_sequence_node
      (node : 'a u Lwt_sequence.node)
      (_actual_content:('a, 'u, 'c) promise)
        : ('a, 'u, 'c) promise Lwt_sequence.node =
    Obj.magic node

  let add_task_r sequence =
    let p = new_pending ~how_to_cancel:Cancel_this_promise in
    let node = Lwt_sequence.add_r (to_public_resolver p) sequence in
    let node = cast_sequence_node node p in

    let Pending callbacks = p.state in
    callbacks.cancel_callbacks <- Cancel_callback_list_remove_sequence_node node;
    to_public_promise p

  let add_task_l sequence =
    let p = new_pending ~how_to_cancel:Cancel_this_promise in
    let node = Lwt_sequence.add_l (to_public_resolver p) sequence in
    let node = cast_sequence_node node p in

    let Pending callbacks = p.state in
    callbacks.cancel_callbacks <- Cancel_callback_list_remove_sequence_node node;
    to_public_promise p

  let waiter_of_wakener r =
    let Internal r = to_internal_resolver r in
    let p = r in
    to_public_promise p

  let no_cancel p =
    let Internal p_internal = to_internal_promise p in
    match (underlying p_internal).state with
      | Pending callbacks ->
      let p' = new_pending ~how_to_cancel:Not_cancelable in

      let callback in_completion_loop p_result =
        let State_may_now_be_pending_proxy p' = may_now_be_proxy p' in
        let p' = underlying p' in

        let State_may_have_changed p' =
          complete in_completion_loop p' p_result in
        ignore p'
      in
      add_implicitly_removed_callback callbacks callback;

          to_public_promise p'
      | Resolved _ ->
          p
      | Failed _ ->
          p
end
include Pending_promises



module Sequential_composition =
struct
  let unify
      (type c)
      in_completion_loop
      (t1 : ('a, underlying, pending) promise)
      (t2 : ('a, _, c) promise) =

    let t2 = underlying t2 in

          if identical t1 t2 then
            State_may_have_changed t2
          else begin
            match t2.state with
              | Pending callbacks2 ->
        let State_may_have_changed t2 =
          set_promise_state t2 (Unified_with t1) in
        ignore t2;

                  let Pending callbacks1 = t1.state in

                  callbacks1.how_to_cancel <- callbacks2.how_to_cancel;

                  let waiters =
                    concat_regular_callbacks callbacks1.regular_callbacks callbacks2.regular_callbacks
                  and removed =
                    callbacks1.cleanups_deferred + callbacks2.cleanups_deferred in
                  if removed > cleanup_throttle then begin
                    callbacks1.cleanups_deferred <- 0;
                    callbacks1.regular_callbacks <- clean_up_callback_cells waiters
                  end else begin
                    callbacks1.cleanups_deferred <- removed;
                    callbacks1.regular_callbacks <- waiters
                  end;
                  callbacks1.cancel_callbacks <-
                    concat_cancel_callbacks callbacks1.cancel_callbacks callbacks2.cancel_callbacks;

        State_may_have_changed t1

      | Resolved _ ->
        complete in_completion_loop t1 t2.state
      | Failed _ ->
        complete in_completion_loop t1 t2.state
          end

  let bind p f =
    let Internal p = to_internal_promise p in
    let p = underlying p in

    match p.state with
      | Resolved v ->
          f v
      | Failed _ as result ->
          to_public_promise { state = result }
      | Pending p_callbacks ->
      let p'' = new_pending ~how_to_cancel:(Propagate_cancel_to_one p) in
          let saved_storage = !current_storage in

      let callback in_completion_loop p_result =
        let State_may_now_be_pending_proxy p'' = may_now_be_proxy p'' in
        let p'' = underlying p'' in

        match p_result with
               | Resolved v ->

          current_storage := saved_storage;

          let p' = try f v with exn -> fail exn in
          let Internal p' = to_internal_promise p' in

          let State_may_have_changed p'' = unify in_completion_loop p'' p' in
          ignore p''

               | Failed _ as state ->

          let State_may_have_changed p'' =
            complete in_completion_loop p'' state in
          ignore p''
      in
      add_implicitly_removed_callback p_callbacks callback;

      to_public_promise p''

  let backtrace_bind add_loc p f =
    let Internal p = to_internal_promise p in
    let p = underlying p in

    match p.state with
      | Resolved v ->
          f v
      | Failed exn ->
          to_public_promise { state = Failed(add_loc exn) }
      | Pending p_callbacks ->
      let p'' = new_pending ~how_to_cancel:(Propagate_cancel_to_one p) in
          let saved_storage = !current_storage in

      let callback in_completion_loop p_result =
        let State_may_now_be_pending_proxy p'' = may_now_be_proxy p'' in
        let p'' = underlying p'' in

        match p_result with
               | Resolved v ->
          current_storage := saved_storage;

          let p' = try f v with exn -> fail (add_loc exn) in
          let Internal p' = to_internal_promise p' in

          let State_may_have_changed p'' = unify in_completion_loop p'' p' in
          ignore p''

               | Failed exn ->

          let State_may_have_changed p'' =
            complete in_completion_loop p'' (Failed(add_loc exn)) in
          ignore p''
      in
      add_implicitly_removed_callback p_callbacks callback;

      to_public_promise p''

let (>>=) t f = bind t f
let (=<<) f t = bind t f

  let map f p =
    let Internal p = to_internal_promise p in
    let p = underlying p in

    match p.state with
      | Resolved v ->
          to_public_promise { state = try Resolved (f v) with exn -> Failed exn }
      | Failed _ as result ->
          to_public_promise { state = result }
      | Pending p_callbacks ->
      let p'' = new_pending ~how_to_cancel:(Propagate_cancel_to_one p) in
          let saved_storage = !current_storage in

      let callback in_completion_loop p_result =
        let State_may_now_be_pending_proxy p'' = may_now_be_proxy p'' in
        let p'' = underlying p'' in

        match p_result with
               | Resolved v ->
                 current_storage := saved_storage;

          let p''_result = try Resolved (f v) with exn -> Failed exn in
          let State_may_have_changed p'' =
            complete in_completion_loop p'' p''_result in
          ignore p''

        | Failed _ as p_result ->
          let State_may_have_changed p'' =
            complete in_completion_loop p'' p_result in
          ignore p''
      in
      add_implicitly_removed_callback p_callbacks callback;

      to_public_promise p''

let (>|=) t f = map f t
let (=|<) f t = map f t

  let catch f h =
    let p = try f () with exn -> fail exn in
    let Internal p = to_internal_promise p in
    let p = underlying p in

    match p.state with
      | Resolved _ ->
          to_public_promise p
      | Failed exn ->
          h exn
      | Pending p_callbacks ->
      let p'' = new_pending ~how_to_cancel:(Propagate_cancel_to_one p) in
          let saved_storage = !current_storage in

      let callback in_completion_loop p_result =
        let State_may_now_be_pending_proxy p'' = may_now_be_proxy p'' in
        let p'' = underlying p'' in

        match p_result with
               | Resolved _ as p_result ->
          let State_may_have_changed p'' =
            complete in_completion_loop p'' p_result in
          ignore p''

               | Failed exn ->
          current_storage := saved_storage;

          let p' = try h exn with exn -> fail exn in
          let Internal p' = to_internal_promise p' in

          let State_may_have_changed p'' = unify in_completion_loop p'' p' in
          ignore p''
      in
      add_implicitly_removed_callback p_callbacks callback;

      to_public_promise p''

  let backtrace_catch add_loc f h =
    let p = try f () with exn -> fail exn in
    let Internal p = to_internal_promise p in
    let p = underlying p in

    match p.state with
      | Resolved _ ->
          to_public_promise p
      | Failed exn ->
          h (add_loc exn)
      | Pending p_callbacks ->
      let p'' = new_pending ~how_to_cancel:(Propagate_cancel_to_one p) in
          let saved_storage = !current_storage in

      let callback in_completion_loop p_result =
        let State_may_now_be_pending_proxy p'' = may_now_be_proxy p'' in
        let p'' = underlying p'' in

        match p_result with
               | Resolved _ as p_result ->
          let State_may_have_changed p'' =
            complete in_completion_loop p'' p_result in
          ignore p''

               | Failed exn ->
          current_storage := saved_storage;

          let p' = try h exn with exn -> fail (add_loc exn) in
          let Internal p' = to_internal_promise p' in

          let State_may_have_changed p'' = unify in_completion_loop p'' p' in
          ignore p''
      in
      add_implicitly_removed_callback p_callbacks callback;

      to_public_promise p''

  let try_bind f f' h =
    let p = try f () with exn -> fail exn in
    let Internal p = to_internal_promise p in
    let p = underlying p in

    match p.state with
      | Resolved v ->
          f' v
      | Failed exn ->
          h exn
      | Pending p_callbacks ->
      let p'' = new_pending ~how_to_cancel:(Propagate_cancel_to_one p) in
          let saved_storage = !current_storage in

      let callback in_completion_loop p_result =
        let State_may_now_be_pending_proxy p'' = may_now_be_proxy p'' in
        let p'' = underlying p'' in

        match p_result with
               | Resolved v ->
          current_storage := saved_storage;

          let p' = try f' v with exn -> fail exn in
          let Internal p' = to_internal_promise p' in

          let State_may_have_changed p'' = unify in_completion_loop p'' p' in
          ignore p''

               | Failed exn ->
          current_storage := saved_storage;

          let p' = try h exn with exn -> fail exn in
          let Internal p' = to_internal_promise p' in

          let State_may_have_changed p'' = unify in_completion_loop p'' p' in
          ignore p''
      in
      add_implicitly_removed_callback p_callbacks callback;

      to_public_promise p''

  let backtrace_try_bind add_loc f f' h =
    let p = try f () with exn -> fail exn in
    let Internal p = to_internal_promise p in
    let p = underlying p in

    match p.state with
      | Resolved v ->
          f' v
      | Failed exn ->
          h (add_loc exn)
      | Pending p_callbacks ->
      let p'' = new_pending ~how_to_cancel:(Propagate_cancel_to_one p) in
          let saved_storage = !current_storage in

      let callback in_completion_loop p_result =
        let State_may_now_be_pending_proxy p'' = may_now_be_proxy p'' in
        let p'' = underlying p'' in

        match p_result with
               | Resolved v ->
          current_storage := saved_storage;

          let p' = try f' v with exn -> fail (add_loc exn) in
          let Internal p' = to_internal_promise p' in

          let State_may_have_changed p'' = unify in_completion_loop p'' p' in
          ignore p''

               | Failed exn ->
          current_storage := saved_storage;

          let p' = try h exn with exn -> fail (add_loc exn) in
          let Internal p' = to_internal_promise p' in

          let State_may_have_changed p'' = unify in_completion_loop p'' p' in
          ignore p''
      in
      add_implicitly_removed_callback p_callbacks callback;

          to_public_promise p''

  let finalize f f' =
    try_bind f
      (fun x -> f' () >>= fun () -> return x)
      (fun e -> f' () >>= fun () -> fail e)

  let backtrace_finalize add_loc f f' =
    backtrace_try_bind add_loc f
      (fun x -> f' () >>= fun () -> return x)
      (fun e -> f' () >>= fun () -> fail (add_loc e))

  let on_cancel p f =
    let Internal p = to_internal_promise p in

    match (underlying p).state with
      | Pending callbacks ->
          add_cancel_callback callbacks f
      | Failed Canceled ->
          handle_with_async_exception_hook f ()
      | Resolved _ -> ()
      | Failed _ -> ()

  let on_success p f =
    let Internal p = to_internal_promise p in

    match (underlying p).state with
      | Resolved v ->
          handle_with_async_exception_hook f v
      | Failed _ ->
          ()
      | Pending p_callbacks ->
          let saved_storage = !current_storage in

      let callback _in_completion_loop result =
        match result with
        | Resolved v ->
          current_storage := saved_storage;
          handle_with_async_exception_hook f v
        | Failed _ ->
          ()
      in
      add_implicitly_removed_callback p_callbacks callback

  let on_failure p f =
    let Internal p = to_internal_promise p in

    match (underlying p).state with
      | Resolved _ ->
          ()
      | Failed exn ->
          handle_with_async_exception_hook f exn
      | Pending p_callbacks ->
          let saved_storage = !current_storage in

      let callback _in_completion_loop result =
        match result with
        | Resolved _ ->
          ()
        | Failed exn ->
          current_storage := saved_storage;
          handle_with_async_exception_hook f exn
      in
      add_implicitly_removed_callback p_callbacks callback

  let on_termination p f =
    let Internal p = to_internal_promise p in

    match (underlying p).state with
      | Resolved _ ->
      handle_with_async_exception_hook f ()
      | Failed _ ->
          handle_with_async_exception_hook f ()
      | Pending p_callbacks ->
          let saved_storage = !current_storage in

      let callback _in_completion_loop _result =
        current_storage := saved_storage;
        handle_with_async_exception_hook f ()
      in
      add_implicitly_removed_callback p_callbacks callback

  let on_any p f g =
    let Internal p = to_internal_promise p in

    match (underlying p).state with
      | Resolved v ->
          handle_with_async_exception_hook f v
      | Failed exn ->
          handle_with_async_exception_hook g exn
      | Pending p_callbacks ->
          let saved_storage = !current_storage in

      let callback _in_completion_loop result =
        match result with
        | Resolved v ->
          current_storage := saved_storage;
          handle_with_async_exception_hook f v
        | Failed exn ->
          current_storage := saved_storage;
          handle_with_async_exception_hook g exn
      in
      add_implicitly_removed_callback p_callbacks callback
end
include Sequential_composition



module Concurrent_composition =
struct
  let async f =
    let p = try f () with exn -> fail exn in
    let Internal p = to_internal_promise p in

    match (underlying p).state with
      | Resolved _ ->
          ()
      | Failed exn ->
          !async_exception_hook exn
      | Pending p_callbacks ->
      let callback _in_completion_loop result =
        match result with
        | Resolved _ ->
          ()
        | Failed exn ->
          !async_exception_hook exn
      in
      add_implicitly_removed_callback p_callbacks callback

  let ignore_result p =
    let Internal p = to_internal_promise p in

    match (underlying p).state with
      | Resolved _ ->
          ()
      | Failed e ->
          raise e
      | Pending p_callbacks ->
      let callback _in_completion_loop result =
        match result with
        | Resolved _ ->
          ()
        | Failed exn ->
          !async_exception_hook exn
      in
      add_implicitly_removed_callback p_callbacks callback

  let join ps =
    let p' = new_pending ~how_to_cancel:(propagate_cancel_to_several ps) in
    let number_pending_in_ps = ref 0
    and join_result = ref state_return_unit in
    let callback in_completion_loop new_result =
      let State_may_now_be_pending_proxy p' = may_now_be_proxy p' in

      begin
        match !join_result, new_result with
          | Resolved _, Failed _ -> join_result := new_result
          | _ -> ()
      end [@ocaml.warning "-4"];
      decr number_pending_in_ps;
      if !number_pending_in_ps = 0 then begin
        let p' = underlying p' in
        let State_may_have_changed p' =
          complete in_completion_loop (underlying p') !join_result in
        ignore p'
      end
    in
    let rec init = function
      | [] ->
          if !number_pending_in_ps = 0 then
            to_public_promise { state = !join_result }
          else
            to_public_promise p'
      | p :: rest ->
        let Internal p = to_internal_promise p in
          match (underlying p).state with
            | Pending callbacks ->
                incr number_pending_in_ps;
                add_implicitly_removed_callback callbacks callback;
                init rest
            | Failed _ as p_result -> begin
                match !join_result with
                  | Resolved _ ->
                      join_result := p_result;
                      init rest
                  | Failed _ ->
                      init rest
              end
            | Resolved _ ->
                init rest
    in
    init ps

  let count_completed_promises_in ps =
    List.fold_left (fun total p ->
      let Internal p = to_internal_promise p in
      match (underlying p).state with
      | Pending _ -> total
      | Resolved _ -> total + 1
      | Failed _ -> total + 1) 0 ps

  let rec nth_completed ps n =
    match ps with
      | [] ->
          assert false
      | p :: ps ->
      let Internal p' = to_internal_promise p in
          match (underlying p').state with
            | Pending _ ->
                nth_completed ps n

      | Resolved _ ->
        if n <= 0 then p
        else nth_completed ps (n - 1)
      | Failed _ ->
        if n <= 0 then p
        else nth_completed ps (n - 1)

  let rec nth_completed_and_cancel_pending ps n =
    match ps with
      | [] ->
          assert false
      | p :: ps ->
      let Internal p' = to_internal_promise p in
          match (underlying p').state with
            | Pending _ ->
                cancel p;
                nth_completed_and_cancel_pending ps n

      | Resolved _ ->
        if n <= 0 then (List.iter cancel ps; p)
        else nth_completed_and_cancel_pending ps (n - 1)
      | Failed _ ->
        if n <= 0 then (List.iter cancel ps; p)
        else nth_completed_and_cancel_pending ps (n - 1)

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
      let p = new_pending ~how_to_cancel:(propagate_cancel_to_several ps) in

      let callback in_completion_loop result =
        let State_may_now_be_pending_proxy p = may_now_be_proxy p in
        let p = underlying p in
        let State_may_have_changed p = complete in_completion_loop p result in
        ignore p
      in
      add_explicitly_removable_callback_to_each_of ps callback;
      to_public_promise p
    end

  let pick ps =
    let ready = count_completed_promises_in ps in
    if ready > 0 then
      if ready = 1 then
        nth_completed_and_cancel_pending ps 0
      else
        nth_completed_and_cancel_pending ps (Random.State.int (Lazy.force prng) ready)
    else begin
      let p = new_pending ~how_to_cancel:(propagate_cancel_to_several ps) in

      let callback in_completion_loop result =
        let State_may_now_be_pending_proxy p = may_now_be_proxy p in
        let p = underlying p in
        List.iter cancel ps;
        let State_may_have_changed p = complete in_completion_loop p result in
        ignore p
      in
      add_explicitly_removable_callback_to_each_of ps callback;
      to_public_promise p
    end

  let rec finish_nchoose_or_npick_after_pending
      in_completion_loop
      to_complete
      results = function
    | [] ->
        complete in_completion_loop to_complete (Resolved (List.rev results))
    | p :: ps ->
      let Internal p = to_internal_promise p in

        match (underlying p).state with
          | Resolved v ->
              finish_nchoose_or_npick_after_pending in_completion_loop to_complete (v :: results) ps
          | Failed _ as state ->
              complete in_completion_loop to_complete state
          | Pending _ ->
              finish_nchoose_or_npick_after_pending in_completion_loop to_complete results ps

let nchoose_sleep ps =
  let p = new_pending ~how_to_cancel:(propagate_cancel_to_several ps) in

  let callback in_completion_loop _result =
          let State_may_now_be_pending_proxy p = may_now_be_proxy p in
          let p = underlying p in
          let State_may_have_changed p =
            finish_nchoose_or_npick_after_pending in_completion_loop p [] ps in
          ignore p
  in
  add_explicitly_removable_callback_to_each_of ps callback;
  to_public_promise p

  let nchoose ps =
    let rec init = function
      | [] ->
          nchoose_sleep ps
      | p :: ps ->
        let Internal p = to_internal_promise p in
          match (underlying p).state with
            | Resolved x ->
                collect [x] ps
            | Failed _ as state ->
                to_public_promise { state }
            | Pending _ ->
                init ps
    and collect acc = function
      | [] ->
          return (List.rev acc)
      | p :: ps ->
        let Internal p = to_internal_promise p in
          match (underlying p).state with
            | Resolved v ->
                collect (v :: acc) ps
            | Failed _ as state ->
                to_public_promise { state }
            | Pending _ ->
                collect acc ps
    in
    init ps

let npick_sleep ps =
  let p = new_pending ~how_to_cancel:(propagate_cancel_to_several ps) in
  let callback in_completion_loop _result =
          let State_may_now_be_pending_proxy p = may_now_be_proxy p in
          let p = underlying p in
    List.iter cancel ps;

          let State_may_have_changed p =
            finish_nchoose_or_npick_after_pending in_completion_loop p [] ps in
          ignore p
  in
  add_explicitly_removable_callback_to_each_of ps callback;
  to_public_promise p

  let npick ps =
    let rec init = function
      | [] ->
          npick_sleep ps
      | p :: ps' ->
        let Internal p = to_internal_promise p in
          match (underlying p).state with
            | Resolved v ->
                collect [v] ps'
            | Failed _ as state ->
                List.iter cancel ps;
                to_public_promise { state }
            | Pending _ ->
                init ps'
    and collect acc = function
      | [] ->
          List.iter cancel ps;
          return (List.rev acc)
      | p :: ps' ->
        let Internal p = to_internal_promise p in
          match (underlying p).state with
            | Resolved v ->
                collect (v :: acc) ps'
            | Failed _ as state ->
                List.iter cancel ps;
                to_public_promise { state }
            | Pending _ ->
                collect acc ps'
    in
    init ps

let rec nchoose_split_terminate in_completion_loop to_complete resolved pending = function
  | [] ->
      complete in_completion_loop to_complete (Resolved (List.rev resolved, List.rev pending))
  | p :: ps ->
        let Internal p_internal = to_internal_promise p in
      match (underlying p_internal).state with
        | Resolved v ->
            nchoose_split_terminate in_completion_loop to_complete (v :: resolved) pending ps
        | Failed _ as state ->
            complete in_completion_loop to_complete state
        | Pending _ ->
            nchoose_split_terminate in_completion_loop to_complete resolved (p :: pending) ps

let nchoose_split_sleep ps =
  let p = new_pending ~how_to_cancel:(propagate_cancel_to_several ps) in
  let callback in_completion_loop _result =
          let State_may_now_be_pending_proxy p = may_now_be_proxy p in
          let p = underlying p in
          let State_may_have_changed p = nchoose_split_terminate in_completion_loop p [] [] ps in
          ignore p
  in
  add_explicitly_removable_callback_to_each_of ps callback;
  to_public_promise p

  let nchoose_split ps =
    let rec init acc_sleeping = function
      | [] ->
          nchoose_split_sleep ps
      | p :: ps' ->
        let Internal p_internal = to_internal_promise p in
          match (underlying p_internal).state with
            | Resolved v ->
                collect [v] acc_sleeping ps'
            | Failed _ as state ->
                to_public_promise { state }
            | Pending _ ->
                init (p :: acc_sleeping) ps'
    and collect acc_terminated acc_sleeping = function
      | [] ->
          return (List.rev acc_terminated, acc_sleeping)
      | p :: ps ->
        let Internal p_internal = to_internal_promise p in
          match (underlying p_internal).state with
            | Resolved v ->
                collect (v :: acc_terminated) acc_sleeping ps
            | Failed _ as state ->
                to_public_promise { state }
            | Pending _ ->
                collect acc_terminated (p :: acc_sleeping) ps
    in
    init [] ps

let protected p =
    let Internal p_internal = to_internal_promise p in
  match (underlying p_internal).state with
    | Pending _ ->
      let p' = new_pending ~how_to_cancel:Cancel_this_promise in

      let callback in_completion_loop p_result =
        let State_may_now_be_pending_proxy p' = may_now_be_proxy p' in
        let p' = underlying p' in

        let State_may_have_changed p' =
          complete in_completion_loop p' p_result in
        ignore p'
      in

      let remove_the_callback =
        add_explicitly_removable_callback_and_give_remove_function
          [p] callback
      in

      let Pending p'_callbacks = p'.state in
      add_cancel_callback p'_callbacks remove_the_callback;

        to_public_promise p'

    | Resolved _ ->
        p
    | Failed _ -> p

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

  let state p =
    let Internal p = to_internal_promise p in
    match (underlying p).state with
    | Resolved v -> State.Return v
    | Failed exn -> State.Fail exn
    | Pending _ -> State.Sleep

  include State

  let is_sleeping p =
    let Internal p = to_internal_promise p in
    match (underlying p).state with
    | Resolved _ -> false
    | Failed _ -> false
    | Pending _ -> true

  let poll p =
    let Internal p = to_internal_promise p in
    match (underlying p).state with
      | Failed e -> raise e
      | Resolved v -> Some v
      | Pending _ -> None

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



module Lwt_result_type =
struct
  type +'a result = 'a lwt_result

  let make_value v = Result.Ok v
  let make_error exn = Result.Error exn
end
include Lwt_result_type
