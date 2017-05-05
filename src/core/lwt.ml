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



exception Canceled

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

type 'a promise_state =
  | Resolved of 'a
  | Failed of exn
  | Pending of 'a callbacks
  | Unified_with of 'a promise

and 'a promise = {
  mutable state : 'a promise_state;
}

and 'a callbacks = {
  mutable cancel : cancel;
  mutable waiters : 'a waiter_set;
  mutable removed : int;
  mutable cancel_handlers : 'a cancel_handler_set;
}

and cancel =
  | Cancel_no
  | Cancel_me
  | Cancel_link of a_promise
  | Cancel_links of a_promise_list

and 'a waiter_set =
  | Empty
  | Removable of ('a promise_state -> unit) option ref
  | Immutable of ('a promise_state -> unit)
  | Append of 'a waiter_set * 'a waiter_set

and 'a cancel_handler_set =
  | Chs_empty
  | Chs_func of storage * (unit -> unit)
  | Chs_node of 'a u Lwt_sequence.node
  | Chs_append of 'a cancel_handler_set * 'a cancel_handler_set

external thread_repr : 'a t -> 'a promise = "%identity"
external thread : 'a promise -> 'a t = "%identity"
external wakener : 'a promise -> 'a u = "%identity"
external wakener_repr : 'a u -> 'a promise = "%identity"

let max_removed = 42


type 'a key = {
  id : int;
  mutable store : 'a option;
}

let next_key_id = ref 0

let new_key () =
  let id = !next_key_id in
  next_key_id := id + 1;
  { id = id; store = None }

let current_data = ref Storage_map.empty

let get key =
  try
    Storage_map.find key.id !current_data ();
    let value = key.store in
    key.store <- None;
    value
  with Not_found ->
    None



let rec repr_rec t =
  match t.state with
    | Unified_with t' -> let t'' = repr_rec t' in if t'' != t' then t.state <- Unified_with t''; t''
    | Resolved _ | Failed _ | Pending _ -> t

let repr t = repr_rec (thread_repr t)

let async_exception_hook =
  ref (fun exn ->
         prerr_string "Fatal error: exception ";
         prerr_string (Printexc.to_string exn);
         prerr_char '\n';
         Printexc.print_backtrace stderr;
         flush stderr;
         exit 2)

let call_unsafe f x =
  try
    f x
  with exn ->
    !async_exception_hook exn

let rec run_waiters_rec state ws rem =
  match ws with
    | Empty ->
        run_waiters_rec_next state rem
    | Immutable f ->
        f state;
        run_waiters_rec_next state rem
    | Removable { contents = None } ->
        run_waiters_rec_next state rem
    | Removable { contents = Some f } ->
        f state;
        run_waiters_rec_next state rem
    | Append (ws1, ws2) ->
        run_waiters_rec state ws1 (ws2 :: rem)

and run_waiters_rec_next state rem =
  match rem with
    | [] ->
        ()
    | ws :: rem ->
        run_waiters_rec state ws rem

let rec run_cancel_handlers_rec chs rem =
  match chs with
    | Chs_empty ->
        run_cancel_handlers_rec_next rem
    | Chs_func (data, f) ->
        current_data := data;
        call_unsafe f ();
        run_cancel_handlers_rec_next rem
    | Chs_node n ->
        Lwt_sequence.remove n;
        run_cancel_handlers_rec_next rem
    | Chs_append (chs1, chs2) ->
        run_cancel_handlers_rec chs1 (chs2 :: rem)

and run_cancel_handlers_rec_next rem =
  match rem with
    | [] ->
        ()
    | chs :: rem ->
        run_cancel_handlers_rec chs rem

let unsafe_run_waiters sleeper state =
  (match state with
     | Failed Canceled ->
         run_cancel_handlers_rec sleeper.cancel_handlers []
     | Resolved _ | Failed _ | Pending _ | Unified_with _ ->
         ());
  run_waiters_rec state sleeper.waiters []

let wakening = ref false

module type A_closure = sig
  type a
  val sleeper : a callbacks
  val state : a promise_state
end

let to_wakeup = Queue.create ()

let enter_wakeup () =
  let snapshot = !current_data in
  let already_wakening =
    if !wakening then
      true
    else begin
      wakening := true;
      false
    end
  in
  (already_wakening, snapshot)

let leave_wakeup (already_wakening, snapshot) =
  if not already_wakening then begin
    while not (Queue.is_empty to_wakeup) do
      let closure = Queue.pop to_wakeup in
      let module M = (val closure : A_closure) in
      unsafe_run_waiters M.sleeper M.state
    done;
    wakening := false;
    current_data := snapshot
  end else
    current_data := snapshot

(* See https://github.com/ocsigen/lwt/issues/48. *)
let abandon_wakeups () =
  if !wakening then leave_wakeup (false, Storage_map.empty)

let safe_run_waiters sleeper state =
  let ctx = enter_wakeup () in
  unsafe_run_waiters sleeper state;
  leave_wakeup ctx

type +'a result = ('a, exn) Result.result

let state_of_result
  : 'a result -> 'a promise_state
  = function
  | Result.Ok x -> Resolved x
  | Result.Error e -> Failed e

let make_value v = Result.Ok v
let make_error e = Result.Error e

let wakeup_result t result =
  let t = repr_rec (wakener_repr t) in
  match t.state with
    | Pending sleeper ->
        let state = state_of_result result in
        t.state <- state;
        safe_run_waiters sleeper state
    | Failed Canceled ->
        ()
    | Resolved _ | Failed _ | Unified_with _ ->
        invalid_arg "Lwt.wakeup_result"

let wakeup t v = wakeup_result t (make_value v)
let wakeup_exn t e = wakeup_result t (make_error e)

let wakeup_later_result (type x) t result =
  let t = repr_rec (wakener_repr t) in
  match t.state with
    | Pending sleeper ->
        let state = state_of_result result in
        t.state <- state;
        if !wakening then begin
          let module M = struct
            type a = x
            let sleeper = sleeper
            let state = state
          end in
          Queue.push (module M : A_closure) to_wakeup
        end else
          safe_run_waiters sleeper state
    | Failed Canceled ->
        ()
    | Resolved _ | Failed _ | Unified_with _ ->
        invalid_arg "Lwt.wakeup_later_result"

let wakeup_later t v = wakeup_later_result t (make_value v)
let wakeup_later_exn t e = wakeup_later_result t (make_error e)

module type A_sleeper = sig
  type a
  val sleeper : a callbacks
end

type a_sleeper = (module A_sleeper)

let pack_sleeper (type x) sleeper =
  let module M = struct type a = x let sleeper = sleeper end in
  (module M : A_sleeper)

let cancel t =
  let state = Failed Canceled in
  let rec collect : 'a. a_sleeper list -> 'a t -> a_sleeper list = fun acc t ->
    let t = repr t in
    match t.state with
      | Pending ({ cancel; _ } as sleeper) -> begin
          match cancel with
            | Cancel_no ->
                acc
            | Cancel_me ->
                t.state <- state;
                (pack_sleeper sleeper) :: acc
            | Cancel_link m ->
                let module M = (val m : Existential_promise) in
                collect acc M.promise
            | Cancel_links m ->
                let module M = (val m : Existential_promise_list) in
                List.fold_left collect acc M.promise_list
        end
      | Resolved _ | Failed _ | Unified_with _ ->
          acc
  in
  let sleepers = collect [] t in
  let ctx = enter_wakeup () in
  List.iter
    (fun sleeper ->
       let module M = (val sleeper : A_sleeper) in
       run_cancel_handlers_rec M.sleeper.cancel_handlers [];
       run_waiters_rec state M.sleeper.waiters [])
    sleepers;
  leave_wakeup ctx

let append l1 l2 =
  (match l1, l2 with
    | Empty, _ -> l2
    | _, Empty -> l1
    | _ -> Append (l1, l2))
  [@ocaml.warning "-4"]

let chs_append l1 l2 =
  (match l1, l2 with
    | Chs_empty, _ -> l2
    | _, Chs_empty -> l1
    | _ -> Chs_append (l1, l2))
  [@ocaml.warning "-4"]

let rec cleanup = function
  | Removable { contents = None } ->
      Empty
  | Append (l1, l2) ->
      append (cleanup l1) (cleanup l2)
  | Empty | Removable _ | Immutable _ as ws ->
      ws

let connect t1 t2 =
  let t1 = repr t1 and t2 = repr t2 in
  match t1.state with
    | Pending sleeper1 ->
        if t1 == t2 then
          ()
        else begin
          match t2.state with
            | Pending sleeper2 ->
                t2.state <- Unified_with t1;

                sleeper1.cancel <- sleeper2.cancel;

                let waiters = append sleeper1.waiters sleeper2.waiters
                and removed = sleeper1.removed + sleeper2.removed in
                if removed > max_removed then begin
                  sleeper1.removed <- 0;
                  sleeper1.waiters <- cleanup waiters
                end else begin
                  sleeper1.removed <- removed;
                  sleeper1.waiters <- waiters
                end;
                sleeper1.cancel_handlers <- chs_append sleeper1.cancel_handlers sleeper2.cancel_handlers
            | Resolved _ | Failed _ | Unified_with _ as state2 ->
                t1.state <- state2;
                unsafe_run_waiters sleeper1 state2
        end
    | Resolved _ | Failed _ | Unified_with _ ->
         assert false

let fast_connect t state =
  let t = repr t in
  match t.state with
    | Pending sleeper ->
        t.state <- state;
        unsafe_run_waiters sleeper state
    | Resolved _ | Failed _ | Unified_with _ ->
        assert false

let fast_connect_if t state =
  let t = repr t in
  match t.state with
    | Pending sleeper ->
        t.state <- state;
        unsafe_run_waiters sleeper state
    | Resolved _ | Failed _ | Unified_with _ ->
        ()



let return v =
  thread { state = Resolved v }

let state_return_unit = Resolved ()
let return_unit = thread { state = state_return_unit }
let return_none = return None
let return_some x = return (Some x)
let return_nil = return []
let return_true = return true
let return_false = return false
let return_ok x = return (Result.Ok x)
let return_error x = return (Result.Error x)

let of_result result =
  thread { state = state_of_result result }

let fail e =
  thread { state = Failed e }

let fail_with msg =
  thread { state = Failed (Failure msg) }

let fail_invalid_arg msg =
  thread { state = Failed (Invalid_argument msg) }

let temp t =
  thread {
    state = Pending { cancel = Cancel_link (pack_promise (thread t));
                    waiters = Empty;
                    removed = 0;
                    cancel_handlers = Chs_empty }
  }

let temp_many l =
  thread {
    state = Pending { cancel = Cancel_links (pack_promise_list l);
                    waiters = Empty;
                    removed = 0;
                    cancel_handlers = Chs_empty }
  }

let wait_aux () = {
  state = Pending { cancel = Cancel_no;
                  waiters = Empty;
                  removed = 0;
                  cancel_handlers = Chs_empty }
}

let wait () =
  let t = wait_aux () in
  (thread t, wakener t)

let task_aux () = {
  state = Pending { cancel = Cancel_me;
                  waiters = Empty;
                  removed = 0;
                  cancel_handlers = Chs_empty }
}

let task () =
  let t = task_aux () in
  (thread t, wakener t)

let add_task_r seq =
  let sleeper = {
    cancel = Cancel_me;
    waiters = Empty;
    removed = 0;
    cancel_handlers = Chs_empty
  } in
  let t = { state = Pending sleeper } in
  let node = Lwt_sequence.add_r (wakener t) seq in
  sleeper.cancel_handlers <- Chs_node node;
  thread t

let add_task_l seq =
  let sleeper = {
    cancel = Cancel_me;
    waiters = Empty;
    removed = 0;
    cancel_handlers = Chs_empty
  }in
  let t = { state = Pending sleeper } in
  let node = Lwt_sequence.add_l (wakener t) seq in
  sleeper.cancel_handlers <- Chs_node node;
  thread t

let waiter_of_wakener wakener = thread (wakener_repr wakener)

let apply f x = try f x with e -> fail e

let wrap f = try return (f ()) with exn -> fail exn

let wrap1 f x1 = try return (f x1) with exn -> fail exn
let wrap2 f x1 x2 = try return (f x1 x2) with exn -> fail exn
let wrap3 f x1 x2 x3 = try return (f x1 x2 x3) with exn -> fail exn
let wrap4 f x1 x2 x3 x4 = try return (f x1 x2 x3 x4) with exn -> fail exn
let wrap5 f x1 x2 x3 x4 x5 = try return (f x1 x2 x3 x4 x5) with exn -> fail exn
let wrap6 f x1 x2 x3 x4 x5 x6 = try return (f x1 x2 x3 x4 x5 x6) with exn -> fail exn
let wrap7 f x1 x2 x3 x4 x5 x6 x7 = try return (f x1 x2 x3 x4 x5 x6 x7) with exn -> fail exn

let add_waiter sleeper waiter =
  sleeper.waiters <- (match sleeper.waiters with
                        | Empty -> waiter
                        | Immutable _ | Removable _ | Append _ as ws ->
                          Append (waiter, ws))

let add_immutable_waiter sleeper waiter =
  add_waiter sleeper (Immutable waiter)

let on_cancel t f =
  match (repr t).state with
    | Pending sleeper ->
        let handler = Chs_func (!current_data, f) in
        sleeper.cancel_handlers <- (
          match sleeper.cancel_handlers with
            | Chs_empty -> handler
            | Chs_func _ | Chs_node _ | Chs_append _ as chs ->
              Chs_append (handler, chs)
        )
    | Failed Canceled ->
        call_unsafe f ()
    | Resolved _ | Failed _ | Unified_with _ ->
        ()

let bind t f =
  let t = repr t in
  match t.state with
    | Resolved v ->
        f v
    | Failed _ as state ->
        thread { state }
    | Pending sleeper ->
        let res = temp t in
        let data = !current_data in
        add_immutable_waiter sleeper
          (function
             | Resolved v -> current_data := data; connect res (try f v with exn -> fail exn)
             | Failed _ as state -> fast_connect res state
             | Pending _ | Unified_with _ -> assert false);
        res
    | Unified_with _ ->
        assert false

let (>>=) t f = bind t f
let (=<<) f t = bind t f

let map f t =
  let t = repr t in
  match t.state with
    | Resolved v ->
        thread { state = try Resolved (f v) with exn -> Failed exn }
    | Failed _ as state ->
        thread { state }
    | Pending sleeper ->
        let res = temp t in
        let data = !current_data in
        add_immutable_waiter sleeper
          (function
             | Resolved v -> current_data := data; fast_connect res (try Resolved (f v) with exn -> Failed exn)
             | Failed _ as state -> fast_connect res state
             | Pending _ | Unified_with _ -> assert false);
        res
    | Unified_with _ ->
        assert false

let (>|=) t f = map f t
let (=|<) f t = map f t

let catch x f =
  let t = repr (try x () with exn -> fail exn) in
  match t.state with
    | Resolved _ ->
        thread t
    | Failed exn ->
        f exn
    | Pending sleeper ->
        let res = temp t in
        let data = !current_data in
        add_immutable_waiter sleeper
          (function
             | Resolved _ as state -> fast_connect res state
             | Failed exn -> current_data := data; connect res (try f exn with exn -> fail exn)
             | Pending _ | Unified_with _ -> assert false);
        res
    | Unified_with _ ->
        assert false

let on_success t f =
  match (repr t).state with
    | Resolved v ->
        call_unsafe f v
    | Failed _ ->
        ()
    | Pending sleeper ->
        let data = !current_data in
        add_immutable_waiter sleeper
          (function
             | Resolved v -> current_data := data; call_unsafe f v
             | Failed _ -> ()
             | Pending _ | Unified_with _ -> assert false)
    | Unified_with _ ->
        assert false

let on_failure t f =
  match (repr t).state with
    | Resolved _ ->
        ()
    | Failed exn ->
        call_unsafe f exn
    | Pending sleeper ->
        let data = !current_data in
        add_immutable_waiter sleeper
          (function
             | Resolved _ -> ()
             | Failed exn -> current_data := data; call_unsafe f exn
             | Pending _ | Unified_with _ -> assert false)
    | Unified_with _ ->
        assert false

let on_termination t f =
  match (repr t).state with
    | Resolved _
    | Failed _ ->
        call_unsafe f ()
    | Pending sleeper ->
        let data = !current_data in
        add_immutable_waiter sleeper
          (function
             | Resolved _
             | Failed _ -> current_data := data; call_unsafe f ()
             | Pending _ | Unified_with _ -> assert false)
    | Unified_with _ ->
        assert false

let on_any t f g =
  match (repr t).state with
    | Resolved v ->
        call_unsafe f v
    | Failed exn ->
        call_unsafe g exn
    | Pending sleeper ->
        let data = !current_data in
        add_immutable_waiter sleeper
          (function
             | Resolved v -> current_data := data; call_unsafe f v
             | Failed exn -> current_data := data; call_unsafe g exn
             | Pending _ | Unified_with _ -> assert false)
    | Unified_with _ ->
        assert false

let try_bind x f g =
  let t = repr (try x () with exn -> fail exn) in
  match t.state with
    | Resolved v ->
        f v
    | Failed exn ->
        g exn
    | Pending sleeper ->
        let res = temp t in
        let data = !current_data in
        add_immutable_waiter sleeper
          (function
             | Resolved v -> current_data := data; connect res (try f v with exn -> fail exn)
             | Failed exn -> current_data := data; connect res (try g exn with exn -> fail exn)
             | Pending _ | Unified_with _ -> assert false);
        res
    | Unified_with _ ->
        assert false

let poll t =
  match (repr t).state with
    | Failed e -> raise e
    | Resolved v -> Some v
    | Pending _ -> None
    | Unified_with _ -> assert false

let async f =
  let t = repr (try f () with exn -> fail exn) in
  match t.state with
    | Resolved _ ->
        ()
    | Failed exn ->
        !async_exception_hook exn
    | Pending sleeper ->
        add_immutable_waiter sleeper
          (function
             | Resolved _ -> ()
             | Failed exn -> !async_exception_hook exn
             | Pending _ | Unified_with _ -> assert false)
    | Unified_with _ ->
        assert false

let ignore_result t =
  match (repr t).state with
    | Resolved _ ->
        ()
    | Failed e ->
        raise e
    | Pending sleeper ->
        add_immutable_waiter sleeper
          (function
             | Resolved _ -> ()
             | Failed exn -> !async_exception_hook exn
             | Pending _ | Unified_with _ -> assert false)
    | Unified_with _ ->
        assert false

let no_cancel t =
  match (repr t).state with
    | Pending sleeper ->
        let res = thread (wait_aux ()) in
        add_immutable_waiter sleeper (fast_connect res);
        res
    | Resolved _ | Failed _ ->
        t
    | Unified_with _ ->
        assert false

let rec nth_ready l n =
  match l with
    | [] ->
        assert false
    | t :: l ->
        match (repr t).state with
          | Pending _ ->
              nth_ready l n
          | Resolved _ | Failed _ | Unified_with _ ->
              if n > 0 then
                nth_ready l (n - 1)
              else
                t

let ready_count l =
  List.fold_left (fun acc x ->
    match (repr x).state with
    | Pending _ -> acc
    | Resolved _ | Failed _ | Unified_with _ -> acc + 1) 0 l

let remove_waiters l =
  List.iter
    (fun t ->
       match (repr t).state with
         | Pending ({ waiters = Removable _; _ } as sleeper) ->
             sleeper.waiters <- Empty
         | Pending ({waiters = Empty | Immutable _ | Append _; _} as sleeper) ->
             let removed = sleeper.removed + 1 in
             if removed > max_removed then begin
               sleeper.removed <- 0;
               sleeper.waiters <- cleanup sleeper.waiters
             end else
               sleeper.removed <- removed
         | Resolved _ | Failed _ | Unified_with _ ->
             ())
    l

let add_removable_waiter threads waiter =
  let node = Removable waiter in
  List.iter
    (fun t ->
       match (repr t).state with
         | Pending sleeper ->
             add_waiter sleeper node
         | Resolved _ | Failed _ | Unified_with _ ->
             assert false)
    threads

(* The PRNG state is initialized with a constant to make non-IO-based
   programs deterministic. *)
let random_state = lazy (Random.State.make [||])

let choose l =
  let ready = ready_count l in
  if ready > 0 then
    if ready = 1 then
      nth_ready l 0
    else
      nth_ready l (Random.State.int (Lazy.force random_state) ready)
  else begin
    let res = temp_many l in
    let rec waiter = ref (Some handle_result)
    and handle_result state =
      waiter := None;
      remove_waiters l;
      fast_connect res state
    in
    add_removable_waiter l waiter;
    res
  end

let rec nchoose_terminate res acc = function
  | [] ->
      fast_connect res (Resolved (List.rev acc))
  | t :: l ->
      match (repr t).state with
        | Resolved x ->
            nchoose_terminate res (x :: acc) l
        | Failed _ as state ->
            fast_connect res state
        | Pending _ | Unified_with _ ->
            nchoose_terminate res acc l

let nchoose_sleep l =
  let res = temp_many l in
  let rec waiter = ref (Some handle_result)
  and handle_result _state =
    waiter := None;
    remove_waiters l;
    nchoose_terminate res [] l
  in
  add_removable_waiter l waiter;
  res

let nchoose l =
  let rec init = function
    | [] ->
        nchoose_sleep l
    | t :: l ->
        match (repr t).state with
          | Resolved x ->
              collect [x] l
          | Failed _ as state ->
              thread { state }
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
              thread { state }
          | Pending _ | Unified_with _ ->
              collect acc l
  in
  init l

let rec nchoose_split_terminate res acc_terminated acc_sleeping = function
  | [] ->
      fast_connect res (Resolved (List.rev acc_terminated, List.rev acc_sleeping))
  | t :: l ->
      match (repr t).state with
        | Resolved x ->
            nchoose_split_terminate res (x :: acc_terminated) acc_sleeping l
        | Failed _ as state ->
            fast_connect res state
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
  add_removable_waiter l waiter;
  res

let nchoose_split l =
  let rec init acc_sleeping = function
    | [] ->
        nchoose_split_sleep l
    | t :: l ->
        match (repr t).state with
          | Resolved x ->
              collect [x] acc_sleeping l
          | Failed _ as state ->
              thread { state }
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
              thread { state }
          | Pending _ | Unified_with _ ->
              collect acc_terminated (t :: acc_sleeping) l
  in
  init [] l

let rec cancel_and_nth_ready l n =
  match l with
    | [] ->
        assert false
    | t :: l ->
        match (repr t).state with
          | Pending _ ->
              cancel t;
              cancel_and_nth_ready l n
          | Resolved _ | Failed _ | Unified_with _ ->
              if n > 0 then
                cancel_and_nth_ready l (n - 1)
              else begin
                List.iter cancel l;
                t
              end

let pick l =
  let ready = ready_count l in
  if ready > 0 then
    if ready = 1 then
      cancel_and_nth_ready l 0
    else
      cancel_and_nth_ready l (Random.State.int (Lazy.force random_state) ready)
  else begin
    let res = temp_many l in
    let rec waiter = ref (Some handle_result)
    and handle_result state =
      waiter := None;
      remove_waiters l;
      List.iter cancel l;
      fast_connect res state
    in
    add_removable_waiter l waiter;
    res
  end

let npick_sleep l =
  let res = temp_many l in
  let rec waiter = ref (Some handle_result)
  and handle_result _state =
    waiter := None;
    remove_waiters l;
    List.iter cancel l;
    nchoose_terminate res [] l
  in
  add_removable_waiter l waiter;
  res

let npick threads =
  let rec init = function
    | [] ->
        npick_sleep threads
    | t :: l ->
        match (repr t).state with
          | Resolved x ->
              collect [x] l
          | Failed _ as state ->
              List.iter cancel threads;
              thread { state }
          | Pending _ | Unified_with _ ->
              init l
  and collect acc = function
    | [] ->
        List.iter cancel threads;
        return (List.rev acc)
    | t :: l ->
        match (repr t).state with
          | Resolved x ->
              collect (x :: acc) l
          | Failed _ as state ->
              List.iter cancel threads;
              thread { state }
          | Pending _ | Unified_with _ ->
              collect acc l
  in
  init threads

let protected t =
  match (repr t).state with
    | Pending _ ->
        let res = thread (task_aux ()) in
        let rec waiter_cell = ref (Some waiter)
        and waiter state = fast_connect_if res state in
        add_removable_waiter [t] waiter_cell;
        on_cancel res (fun () ->
          waiter_cell := None;
          remove_waiters [t]);
        res

    | Resolved _ | Failed _ ->
        t
    | Unified_with _ ->
        assert false

let join l =
  let res = temp_many l
  and sleeping = ref 0
  and return_state = ref state_return_unit in
  let handle_result state =
    begin
      match !return_state, state with
        | Resolved _, Failed _ -> return_state := state
        | _ -> ()
    end [@ocaml.warning "-4"];
    decr sleeping;
    if !sleeping = 0 then fast_connect res !return_state
  in
  let rec init = function
    | [] ->
        if !sleeping = 0 then
          thread { state = !return_state }
        else
          res
    | t :: rest ->
        match (repr t).state with
          | Pending sleeper ->
              incr sleeping;
              add_immutable_waiter sleeper handle_result;
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
  init l

let ( <?> ) t1 t2 = choose [t1; t2]
let ( <&> ) t1 t2 = join [t1; t2]

let finalize f g =
  try_bind f
    (fun x -> g () >>= fun () -> return x)
    (fun e -> g () >>= fun () -> fail e)

let with_value key value f =
  let save = !current_data in
  let data =
    match value with
      | Some _ ->
          Storage_map.add key.id (fun () -> key.store <- value) save
      | None ->
          Storage_map.remove key.id save
  in
  current_data := data;
  try
    let result = f () in
    current_data := save;
    result
  with exn ->
    current_data := save;
    raise exn



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



let backtrace_bind add_loc t f =
  let t = repr t in
  match t.state with
    | Resolved v ->
        f v
    | Failed exn ->
        thread { state = Failed(add_loc exn) }
    | Pending sleeper ->
        let res = temp t in
        let data = !current_data in
        add_immutable_waiter sleeper
          (function
             | Resolved v -> current_data := data; connect res (try f v with exn -> fail (add_loc exn))
             | Failed exn -> fast_connect res (Failed(add_loc exn))
             | Pending _ | Unified_with _ -> assert false);
        res
    | Unified_with _ ->
        assert false

let backtrace_catch add_loc x f =
  let t = repr (try x () with exn -> fail exn) in
  match t.state with
    | Resolved _ ->
        thread t
    | Failed exn ->
        f (add_loc exn)
    | Pending sleeper ->
        let res = temp t in
        let data = !current_data in
        add_immutable_waiter sleeper
          (function
             | Resolved _ as state -> fast_connect res state
             | Failed exn -> current_data := data; connect res (try f exn with exn -> fail (add_loc exn))
             | Pending _ | Unified_with _ -> assert false);
        res
    | Unified_with _ ->
        assert false

let backtrace_try_bind add_loc x f g =
  let t = repr (try x () with exn -> fail exn) in
  match t.state with
    | Resolved v ->
        f v
    | Failed exn ->
        g (add_loc exn)
    | Pending sleeper ->
        let res = temp t in
        let data = !current_data in
        add_immutable_waiter sleeper
          (function
             | Resolved v -> current_data := data; connect res (try f v with exn -> fail (add_loc exn))
             | Failed exn -> current_data := data; connect res (try g exn with exn -> fail (add_loc exn))
             | Pending _ | Unified_with _ -> assert false);
        res
    | Unified_with _ ->
        assert false

let backtrace_finalize add_loc f g =
  backtrace_try_bind add_loc f
    (fun x -> g () >>= fun () -> return x)
    (fun e -> g () >>= fun () -> fail (add_loc e))



let rec is_sleeping_rec t =
  match t.state with
    | Resolved _ | Failed _ ->
        false
    | Pending _ ->
        true
    | Unified_with t ->
        is_sleeping_rec t

let is_sleeping t = is_sleeping_rec (thread_repr t)

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


module Infix = struct
  let (>>=) = (>>=)
  let (=<<) = (=<<)
  let (>|=) = (>|=)
  let (=|<) = (=|<)
  let (<&>) = (<&>)
  let (<?>) = (<?>)
end
