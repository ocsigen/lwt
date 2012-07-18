(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt
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

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

exception Canceled

module Int_map = Map.Make(struct type t = int let compare = compare end)

type data = (unit -> unit) Int_map.t
  (* Type of data carried by threads *)

type +'a t
type -'a u

(* A type + a thread of this type. *)
module type A_thread = sig
  type a
  val thread : a t
end

type a_thread = (module A_thread)
    (* a_thread = exists 'a. 'a t *)

(* Pack a thread into a A_thread module. *)
let pack_thread (type x) t =
  let module M = struct type a = x let thread = t end in
  (module M : A_thread)

(* A type + a list of threads of this type. *)
module type A_threads = sig
  type a
  val threads : a t list
end

type a_threads = (module A_threads)
    (* a_threads = exists 'a. 'a t list *)

(* Pack a list of threads into a A_threads module. *)
let pack_threads (type x) l =
  let module M = struct type a = x let threads = l end in
  (module M : A_threads)

type 'a thread_state =
  | Return of 'a
      (* [Return v] a terminated thread which has successfully
         terminated with the value [v] *)
  | Fail of exn
      (* [Fail exn] a terminated thread which has failed with the
         exception [exn]. *)
  | Sleep of 'a sleeper
      (* [Sleep sleeper] is a sleeping thread *)
  | Repr of 'a thread_repr
      (* [Repr t] a thread which behaves the same as [t] *)

and 'a thread_repr = {
  mutable state : 'a thread_state;
  (* The state of the thread *)
}

and 'a sleeper = {
  mutable cancel : cancel;
  (* How to cancel this thread. *)
  mutable waiters : 'a waiter_set;
  (* All thunk functions. These functions are always called inside a
     enter_wakeup/leave_wakeup block. *)
  mutable removed : int;
  (* Number of waiter that have been disabled. When this number
     reaches [max_removed], they are effectively removed from
     [waiters]. *)
  mutable cancel_handlers : 'a cancel_handler_set;
  (* Functions to execute when this thread is canceled. Theses
     functions must be executed before waiters. *)
}

and cancel =
  | Cancel_no
      (* This thread cannot be canceled, it was created with
         [wait]. *)
  | Cancel_me
      (* Restart this thread with [Fail Canceled]. It was created with
         [task]. *)
  | Cancel_link of a_thread
      (* Cancel this thread. *)
  | Cancel_links of a_threads
      (* Cancel all these threads. *)

(* Type of set of waiters: *)
and 'a waiter_set =
  | Empty
  | Removable of ('a thread_state -> unit) option ref
  | Immutable of ('a thread_state -> unit)
  | Append of 'a waiter_set * 'a waiter_set

and 'a cancel_handler_set =
  | Chs_empty
  | Chs_func of data * (unit -> unit)
      (* The callback may raise an exception. *)
  | Chs_node of 'a u Lwt_sequence.node
      (* This is the same as:

         {[
           Chs_func (fun () -> Lwt_sequence.remove node)
         ]}

         but it is the common case so it is inlined. *)
  | Chs_append of 'a cancel_handler_set * 'a cancel_handler_set

external thread_repr : 'a t -> 'a thread_repr = "%identity"
external thread : 'a thread_repr -> 'a t = "%identity"
external wakener : 'a thread_repr -> 'a u = "%identity"
external wakener_repr : 'a u -> 'a thread_repr = "%identity"

(* Maximum number of disabled waiters a waiter set can contains before
   being cleaned: *)
let max_removed = 42

(* +-----------------------------------------------------------------+
   | Local storage                                                   |
   +-----------------------------------------------------------------+ *)

type 'a key = {
  id : int;
  mutable store : 'a option;
}

let next_key_id = ref 0

let new_key () =
  let id = !next_key_id in
  next_key_id := id + 1;
  { id = id; store = None }

let current_data = ref Int_map.empty

let get key =
  try
    Int_map.find key.id !current_data ();
    let value = key.store in
    key.store <- None;
    value
  with Not_found ->
    None

(* +-----------------------------------------------------------------+
   | Restarting/connecting threads                                   |
   +-----------------------------------------------------------------+ *)

(* Returns the representative of a thread, updating non-direct references: *)
let rec repr_rec t =
  match t.state with
    | Repr t' -> let t'' = repr_rec t' in if t'' != t' then t.state <- Repr t''; t''
    | _       -> t
let repr t = repr_rec (thread_repr t)

let async_exception_hook =
  ref (fun exn ->
         prerr_string "Fatal error: exception ";
         prerr_string (Printexc.to_string exn);
         prerr_char '\n';
         Printexc.print_backtrace stderr;
         flush stderr;
         exit 2)

(* Execute [f x] and handle any raised exception with
   [async_exception_hook]. *)
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

(* Run all waiters waiting on [t]. This must always be done inside a
   enter_wakeup/leave_wakeup block.

   This function must never raise an exception. *)
let unsafe_run_waiters sleeper state =
  (* Call cancel handlers if this is a thread cancellation. *)
  (match state with
     | Fail Canceled ->
         run_cancel_handlers_rec sleeper.cancel_handlers []
     | _ ->
         ());
  (* Restart waiters. *)
  run_waiters_rec state sleeper.waiters []

(* [true] if we are in a wakeup. *)
let wakening = ref false

(* A sleeper + its state. *)
module type A_closure = sig
  type a
  val sleeper : a sleeper
  val state : a thread_state
end

(* Queue of sleepers to wakeup. *)
let to_wakeup = Queue.create ()

(* Enter a wakeup operation. *)
let enter_wakeup () =
  let snapshot = !current_data in
  let already_wakening =
    if !wakening then
      (* If we are already in a wakeup, do nothing. *)
      true
    else begin
      (* Otherwise mark that a wakeup operation has started. *)
      wakening := true;
      false
    end
  in
  (already_wakening, snapshot)

(* Leave a wakeup operation. *)
let leave_wakeup (already_wakening, snapshot) =
  if not already_wakening then begin
    (* This was the first wakeup on the call stack, wakeup remaining
       sleeping threads. *)
    while not (Queue.is_empty to_wakeup) do
      let closure = Queue.pop to_wakeup in
      let module M = (val closure : A_closure) in
      unsafe_run_waiters M.sleeper M.state
    done;
    (* We are done wakening threads. *)
    wakening := false;
    current_data := snapshot
  end else
    current_data := snapshot

let safe_run_waiters sleeper state =
  let ctx = enter_wakeup () in
  unsafe_run_waiters sleeper state;
  leave_wakeup ctx

(* A ['a result] is either [Return of 'a] or [Fail of exn] so it is
   covariant. *)

type +'a result (* = 'a thread_state *)
external result_of_state : 'a thread_state -> 'a result = "%identity"
external state_of_result : 'a result -> 'a thread_state = "%identity"

let make_value v = result_of_state (Return v)
let make_error e = result_of_state (Fail e)

let wakeup_result t result =
  let t = repr_rec (wakener_repr t) in
  match t.state with
    | Sleep sleeper ->
        let state = state_of_result result in
        t.state <- state;
        safe_run_waiters sleeper state
    | Fail Canceled ->
        (* Do not fail if the thread has been canceled: *)
        ()
    | _ ->
        invalid_arg "Lwt.wakeup_result"

let wakeup t v = wakeup_result t (make_value v)
let wakeup_exn t e = wakeup_result t (make_error e)

let wakeup_later_result (type x) t result =
  let t = repr_rec (wakener_repr t) in
  match t.state with
    | Sleep sleeper ->
        let state = state_of_result result in
        t.state <- state;
        if !wakening then begin
          (* Already wakening => create the closure for later wakening. *)
          let module M = struct
            type a = x
            let sleeper = sleeper
            let state = state
          end in
          Queue.push (module M : A_closure) to_wakeup
        end else
          (* Otherwise restart threads now. *)
          safe_run_waiters sleeper state
    | Fail Canceled ->
        ()
    | _ ->
        invalid_arg "Lwt.wakeup_later_result"

let wakeup_later t v = wakeup_later_result t (make_value v)
let wakeup_later_exn t e = wakeup_later_result t (make_error e)

module type A_sleeper = sig
  type a
  val sleeper : a sleeper
end

type a_sleeper = (module A_sleeper)

let pack_sleeper (type x) sleeper =
  let module M = struct type a = x let sleeper = sleeper end in
  (module M : A_sleeper)

let cancel (type x) t =
  let state = Fail Canceled in
  (* - collect all sleepers to restart
     - set the state of all threads to cancel to [Fail Canceled] *)
  let rec collect : 'a. a_sleeper list -> 'a t -> a_sleeper list = fun acc t ->
    let t = repr t in
    match t.state with
      | Sleep ({ cancel } as sleeper) -> begin
          match cancel with
            | Cancel_no ->
                acc
            | Cancel_me ->
                (* Set the state of [t] immediately so it won't be
                   collected again. *)
                t.state <- state;
                (pack_sleeper sleeper) :: acc
            | Cancel_link m ->
                let module M = (val m : A_thread) in
                collect acc M.thread
            | Cancel_links m ->
                let module M = (val m : A_threads) in
                List.fold_left collect acc M.threads
        end
      | _ ->
          acc
  in
  let sleepers = collect [] t in
  (* Restart all sleepers. *)
  let ctx = enter_wakeup () in
  List.iter
    (fun sleeper ->
       let module M = (val sleeper : A_sleeper) in
       run_cancel_handlers_rec M.sleeper.cancel_handlers [];
       run_waiters_rec state M.sleeper.waiters [])
    sleepers;
  leave_wakeup ctx

let append l1 l2 =
  match l1, l2 with
    | Empty, _ -> l2
    | _, Empty -> l1
    | _ -> Append (l1, l2)

let chs_append l1 l2 =
  match l1, l2 with
    | Chs_empty, _ -> l2
    | _, Chs_empty -> l1
    | _ -> Chs_append (l1, l2)

(* Remove all disbaled waiters of a waiter set: *)
let rec cleanup = function
  | Removable { contents = None } ->
      Empty
  | Append (l1, l2) ->
      append (cleanup l1) (cleanup l2)
  | ws ->
      ws

(* Make [t1] and [t2] behave the same way, where [t1] is a sleeping
   thread. This means that they must share the same representation.

   [connect] assumes it is called inside a enter_wakeup/leave_wakeup
   block. *)
let connect t1 t2 =
  let t1 = repr t1 and t2 = repr t2 in
  match t1.state with
    | Sleep sleeper1 ->
        if t1 == t2 then
          (* Do nothing if the two threads already have the same
             representation *)
          ()
        else begin
          match t2.state with
            | Sleep sleeper2 ->
                (* If [t2] is sleeping, then makes it behave as [t1]: *)
                t2.state <- Repr t1;
                (* Note that the order is important: the user have no
                   access to [t2] but may keep a reference to [t1]. If
                   we inverse the order, i.e. we do:

                   [t1.state <- Repr t2]

                   then we have a possible leak. For example:

                   {[
                     let rec loop ()==
                       lwt () = Lwt_unix.yield () in
                       loop ()

                     lwt () =
                       let t = loop () in
                       ...
                   ]}

                   Here, after [n] iterations, [t] will contains:

                   [ref(Repr(ref(Repr(ref(Repr ... ref Sleep)))))]
                   \-------------[n]--------------/
                *)

                (* Cancelling [t1] is now the same as canceling [t2]: *)
                sleeper1.cancel <- sleeper2.cancel;

                (* Merge the two sets of waiters: *)
                let waiters = append sleeper1.waiters sleeper2.waiters
                and removed = sleeper1.removed + sleeper2.removed in
                if removed > max_removed then begin
                  (* Remove disabled threads *)
                  sleeper1.removed <- 0;
                  sleeper1.waiters <- cleanup waiters
                end else begin
                  sleeper1.removed <- removed;
                  sleeper1.waiters <- waiters
                end;
                sleeper1.cancel_handlers <- chs_append sleeper1.cancel_handlers sleeper2.cancel_handlers
            | state2 ->
                (* [t2] is already terminated, assing its state to [t1]: *)
                t1.state <- state2;
                (* and run all the waiters of [t1]: *)
                unsafe_run_waiters sleeper1 state2
        end
    | _ ->
        (* [t1] is not asleep: *)
         assert false

(* Same as [connect] except that we know that [t2] has alreayd
   terminated. *)
let fast_connect t state =
  let t = repr t in
  match t.state with
    | Sleep sleeper ->
        t.state <- state;
        unsafe_run_waiters sleeper state
    | _ ->
        assert false

(* Same as [fast_connect] except that it does nothing if [t] has
   terminated. *)
let fast_connect_if t state =
  let t = repr t in
  match t.state with
    | Sleep sleeper ->
        t.state <- state;
        unsafe_run_waiters sleeper state
    | _ ->
        ()

(* +-----------------------------------------------------------------+
   | Threads conctruction and combining                              |
   +-----------------------------------------------------------------+ *)

let return v =
  thread { state = Return v }

let state_return_unit = Return ()
let return_unit = thread { state = state_return_unit }
let return_none = return None
let return_nil = return []
let return_true = return true
let return_false = return false

let of_result result =
  thread { state = state_of_result result }

let fail e =
  thread { state = Fail e }

let temp t =
  thread {
    state = Sleep { cancel = Cancel_link (pack_thread (thread t));
                    waiters = Empty;
                    removed = 0;
                    cancel_handlers = Chs_empty }
  }

let temp_many l =
  thread {
    state = Sleep { cancel = Cancel_links (pack_threads l);
                    waiters = Empty;
                    removed = 0;
                    cancel_handlers = Chs_empty }
  }

let wait_aux () = {
  state = Sleep { cancel = Cancel_no;
                  waiters = Empty;
                  removed = 0;
                  cancel_handlers = Chs_empty }
}

let wait () =
  let t = wait_aux () in
  (thread t, wakener t)

let task_aux () = {
  state = Sleep { cancel = Cancel_me;
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
  let t = { state = Sleep sleeper } in
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
  let t = { state = Sleep sleeper } in
  let node = Lwt_sequence.add_l (wakener t) seq in
  sleeper.cancel_handlers <- Chs_node node;
  thread t

let waiter_of_wakener wakener = thread (wakener_repr wakener)

(* apply function, reifying explicit exceptions into the thread type
   apply: ('a -(exn)-> 'b t) -> ('a -(n)-> 'b t)
   semantically a natural transformation TE -> T, where T is the thread
   monad, which is layered over exception monad E.
*)
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
                        | ws -> Append (waiter, ws))

let add_immutable_waiter sleeper waiter =
  add_waiter sleeper (Immutable waiter)

let on_cancel t f =
  match (repr t).state with
    | Sleep sleeper ->
        let handler = Chs_func (!current_data, f) in
        sleeper.cancel_handlers <- (
          match sleeper.cancel_handlers with
            | Chs_empty -> handler
            | chs -> Chs_append (handler, chs)
        )
    | Fail Canceled ->
        call_unsafe f ()
    | _ ->
        ()

let bind t f =
  let t = repr t in
  match t.state with
    | Return v ->
        f v
    | Fail _ as state ->
        thread { state }
    | Sleep sleeper ->
        let res = temp t in
        let data = !current_data in
        add_immutable_waiter sleeper
          (function
             | Return v -> current_data := data; connect res (try f v with exn -> fail exn)
             | Fail _ as state -> fast_connect res state
             | _ -> assert false);
        res
    | Repr _ ->
        assert false

let (>>=) t f = bind t f
let (=<<) f t = bind t f

let map f t =
  let t = repr t in
  match t.state with
    | Return v ->
        thread { state = try Return (f v) with exn -> Fail exn }
    | Fail _ as state ->
        thread { state }
    | Sleep sleeper ->
        let res = temp t in
        let data = !current_data in
        add_immutable_waiter sleeper
          (function
             | Return v -> current_data := data; fast_connect res (try Return (f v) with exn -> Fail exn)
             | Fail _ as state -> fast_connect res state
             | _ -> assert false);
        res
    | Repr _ ->
        assert false

let (>|=) t f = map f t
let (=|<) f t = map f t

let catch x f =
  let t = repr (try x () with exn -> fail exn) in
  match t.state with
    | Return _ ->
        thread t
    | Fail exn ->
        f exn
    | Sleep sleeper ->
        let res = temp t in
        let data = !current_data in
        add_immutable_waiter sleeper
          (function
             | Return _ as state -> fast_connect res state
             | Fail exn -> current_data := data; connect res (try f exn with exn -> fail exn)
             | _ -> assert false);
        res
    | Repr _ ->
        assert false

let on_success t f =
  match (repr t).state with
    | Return v ->
        call_unsafe f v
    | Fail exn ->
        ()
    | Sleep sleeper ->
        let data = !current_data in
        add_immutable_waiter sleeper
          (function
             | Return v -> current_data := data; call_unsafe f v
             | Fail exn -> ()
             | _ -> assert false)
    | Repr _ ->
        assert false

let on_failure t f =
  match (repr t).state with
    | Return v ->
        ()
    | Fail exn ->
        call_unsafe f exn
    | Sleep sleeper ->
        let data = !current_data in
        add_immutable_waiter sleeper
          (function
             | Return v -> ()
             | Fail exn -> current_data := data; call_unsafe f exn
             | _ -> assert false)
    | Repr _ ->
        assert false

let on_termination t f =
  match (repr t).state with
    | Return v ->
        call_unsafe f ()
    | Fail exn ->
        call_unsafe f ()
    | Sleep sleeper ->
        let data = !current_data in
        add_immutable_waiter sleeper
          (function
             | Return v -> current_data := data; call_unsafe f ()
             | Fail exn -> current_data := data; call_unsafe f ()
             | _ -> assert false)
    | Repr _ ->
        assert false

let on_any t f g =
  match (repr t).state with
    | Return v ->
        call_unsafe f v
    | Fail exn ->
        call_unsafe g exn
    | Sleep sleeper ->
        let data = !current_data in
        add_immutable_waiter sleeper
          (function
             | Return v -> current_data := data; call_unsafe f v
             | Fail exn -> current_data := data; call_unsafe g exn
             | _ -> assert false)
    | Repr _ ->
        assert false

let try_bind x f g =
  let t = repr (try x () with exn -> fail exn) in
  match t.state with
    | Return v ->
        f v
    | Fail exn ->
        g exn
    | Sleep sleeper ->
        let res = temp t in
        let data = !current_data in
        add_immutable_waiter sleeper
          (function
             | Return v -> current_data := data; connect res (try f v with exn -> fail exn)
             | Fail exn -> current_data := data; connect res (try g exn with exn -> fail exn)
             | _ -> assert false);
        res
    | Repr _ ->
        assert false

let poll t =
  match (repr t).state with
    | Fail e -> raise e
    | Return v -> Some v
    | Sleep _ -> None
    | Repr _ -> assert false

let async f =
  let t = repr (try f () with exn -> fail exn) in
  match t.state with
    | Return _ ->
        ()
    | Fail exn ->
        !async_exception_hook exn
    | Sleep sleeper ->
        add_immutable_waiter sleeper
          (function
             | Return _ -> ()
             | Fail exn -> !async_exception_hook exn
             | _ -> assert false)
    | Repr _ ->
        assert false

let ignore_result t =
  match (repr t).state with
    | Return _ ->
        ()
    | Fail e ->
        raise e
    | Sleep sleeper ->
        add_immutable_waiter sleeper
          (function
             | Return _ -> ()
             | Fail exn -> !async_exception_hook exn
             | _ -> assert false)
    | Repr _ ->
        assert false

let protected t =
  match (repr t).state with
    | Sleep sleeper ->
        let res = thread (task_aux ()) in
        (* We use [fact_connect_if] because when [res] is canceled, it
           will always terminate before [t]. *)
        add_immutable_waiter sleeper (fast_connect_if res);
        res
    | Return _ | Fail _ ->
        t
    | Repr _ ->
        assert false

let no_cancel t =
  match (repr t).state with
    | Sleep sleeper ->
        let res = thread (wait_aux ()) in
        add_immutable_waiter sleeper (fast_connect res);
        res
    | Return _ | Fail _ ->
        t
    | Repr _ ->
        assert false

let rec nth_ready l n =
  match l with
    | [] ->
        assert false
    | t :: l ->
        match (repr t).state with
          | Sleep _ ->
              nth_ready l n
          | _ ->
              if n > 0 then
                nth_ready l (n - 1)
              else
                t

let ready_count l =
  List.fold_left (fun acc x -> match (repr x).state with Sleep _ -> acc | _ -> acc + 1) 0 l

let remove_waiters l =
  List.iter
    (fun t ->
       match (repr t).state with
         | Sleep ({ waiters = Removable _ } as sleeper) ->
             (* There is only one waiter, it is the removed one. *)
             sleeper.waiters <- Empty
         | Sleep sleeper ->
             let removed = sleeper.removed + 1 in
             if removed > max_removed then begin
               sleeper.removed <- 0;
               sleeper.waiters <- cleanup sleeper.waiters
             end else
               sleeper.removed <- removed
         | _ ->
             ())
    l

let add_removable_waiter threads waiter =
  let node = Removable waiter in
  List.iter
    (fun t ->
       match (repr t).state with
         | Sleep sleeper ->
             add_waiter sleeper node
         | _ ->
             assert false)
    threads

(* The PRNG state is initialized with a constant to make non-IO-based
   programs deterministic. *)
let random_state = lazy (Random.State.make [||])

let choose l =
  let ready = ready_count l in
  if ready > 0 then
    if ready = 1 then
      (* Optimisation for the common case: *)
      nth_ready l 0
    else
      nth_ready l (Random.State.int (Lazy.force random_state) ready)
  else begin
    let res = temp_many l in
    let rec waiter = ref (Some handle_result)
    and handle_result state =
      (* Disable the waiter now: *)
      waiter := None;
      (* Removes all waiters so we do not leak memory: *)
      remove_waiters l;
      (* This will not fail because it is called at most one time,
         since all other waiters have been removed: *)
      fast_connect res state
    in
    add_removable_waiter l waiter;
    res
  end

let rec nchoose_terminate res acc = function
  | [] ->
      fast_connect res (Return (List.rev acc))
  | t :: l ->
      match (repr t).state with
        | Return x ->
            nchoose_terminate res (x :: acc) l
        | Fail _ as state ->
            fast_connect res state
        | _ ->
            nchoose_terminate res acc l

let nchoose_sleep l =
  let res = temp_many l in
  let rec waiter = ref (Some handle_result)
  and handle_result state =
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
          | Return x ->
              collect [x] l
          | Fail _ as state ->
              thread { state }
          | _ ->
              init l
  and collect acc = function
    | [] ->
        return (List.rev acc)
    | t :: l ->
        match (repr t).state with
          | Return x ->
              collect (x :: acc) l
          | Fail _ as state ->
              thread { state }
          | _ ->
              collect acc l
  in
  init l

let rec nchoose_split_terminate res acc_terminated acc_sleeping = function
  | [] ->
      fast_connect res (Return (List.rev acc_terminated, List.rev acc_sleeping))
  | t :: l ->
      match (repr t).state with
        | Return x ->
            nchoose_split_terminate res (x :: acc_terminated) acc_sleeping l
        | Fail _ as state ->
            fast_connect res state
        | _ ->
            nchoose_split_terminate res acc_terminated (t :: acc_sleeping) l

let nchoose_split_sleep l =
  let res = temp_many l in
  let rec waiter = ref (Some handle_result)
  and handle_result state =
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
          | Return x ->
              collect [x] acc_sleeping l
          | Fail _ as state ->
              thread { state }
          | _ ->
              init (t :: acc_sleeping) l
  and collect acc_terminated acc_sleeping = function
    | [] ->
        return (List.rev acc_terminated, acc_sleeping)
    | t :: l ->
        match (repr t).state with
          | Return x ->
              collect (x :: acc_terminated) acc_sleeping l
          | Fail _ as state ->
              thread { state }
          | _ ->
              collect acc_terminated (t :: acc_sleeping) l
  in
  init [] l

(* Return the nth ready thread, and cancel all others *)
let rec cancel_and_nth_ready l n =
  match l with
    | [] ->
        assert false
    | t :: l ->
        match (repr t).state with
          | Sleep _ ->
              cancel t;
              cancel_and_nth_ready l n
          | _ ->
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
      (* Optimisation for the common case: *)
      cancel_and_nth_ready l 0
    else
      cancel_and_nth_ready l (Random.State.int (Lazy.force random_state) ready)
  else begin
    let res = temp_many l in
    let rec waiter = ref (Some handle_result)
    and handle_result state =
      waiter := None;
      remove_waiters l;
      (* Cancel all other threads: *)
      List.iter cancel l;
      fast_connect res state
    in
    add_removable_waiter l waiter;
    res
  end

let npick_sleep l =
  let res = temp_many l in
  let rec waiter = ref (Some handle_result)
  and handle_result state =
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
          | Return x ->
              collect [x] l
          | Fail _ as state ->
              List.iter cancel threads;
              thread { state }
          | _ ->
              init l
  and collect acc = function
    | [] ->
        List.iter cancel threads;
        return (List.rev acc)
    | t :: l ->
        match (repr t).state with
          | Return x ->
              collect (x :: acc) l
          | Fail _ as state ->
              List.iter cancel threads;
              thread { state }
          | _ ->
              collect acc l
  in
  init threads

let join l =
  let res = temp_many l
  (* Number of threads still sleeping: *)
  and sleeping = ref 0
  (* The state that must be returned: *)
  and return_state = ref state_return_unit in
  let handle_result state =
    begin
      match !return_state, state with
        | Return _, Fail _ -> return_state := state
        | _ -> ()
    end;
    decr sleeping;
    (* All threads are terminated, we can wakeup the result: *)
    if !sleeping = 0 then fast_connect res !return_state
  in
  let rec init = function
    | [] ->
        if !sleeping = 0 then
          (* No thread is sleeping, returns immediately: *)
          thread { state = !return_state }
        else
          res
    | t :: rest ->
        match (repr t).state with
          | Sleep sleeper ->
              incr sleeping;
              add_immutable_waiter sleeper handle_result;
              init rest
          | Fail _ as state -> begin
              match !return_state with
                | Return _ ->
                    return_state := state;
                    init rest
                | _ ->
                    init rest
            end
          | _ ->
              init rest
  in
  init l

let ( <?> ) t1 t2 = choose [t1; t2]
let ( <&> ) t1 t2 = join [t1; t2]

let finalize f g =
  try_bind f
    (fun x -> g () >>= fun () -> return x)
    (fun e -> g () >>= fun () -> fail e)

let update_data key = function
  | Some _ as value ->
      current_data := Int_map.add key.id (fun () -> key.store <- value) !current_data
  | None ->
      current_data := Int_map.remove key.id !current_data

let with_value key value f =
  let save = !current_data in
  let data =
    match value with
      | Some _ ->
          Int_map.add key.id (fun () -> key.store <- value) save
      | None ->
          Int_map.remove key.id save
  in
  current_data := data;
  try
    let result = f () in
    current_data := save;
    result
  with exn ->
    current_data := save;
    raise exn

(* +-----------------------------------------------------------------+
   | Paused threads                                                  |
   +-----------------------------------------------------------------+ *)

let pause_hook = ref ignore

let paused = Lwt_sequence.create ()
let paused_count = ref 0

let pause () =
  let waiter = add_task_r paused in
  incr paused_count;
  !pause_hook !paused_count;
  waiter

let wakeup_paused () =
  if not (Lwt_sequence.is_empty paused) then begin
    let tmp = Lwt_sequence.create () in
    Lwt_sequence.transfer_r paused tmp;
    paused_count := 0;
    Lwt_sequence.iter_l (fun wakener -> wakeup wakener ()) tmp
  end

let register_pause_notifier f = pause_hook := f

let paused_count () = !paused_count

(* +-----------------------------------------------------------------+
   | Bakctrace support                                               |
   +-----------------------------------------------------------------+ *)

let backtrace_bind add_loc t f =
  let t = repr t in
  match t.state with
    | Return v ->
        f v
    | Fail exn ->
        thread { state = Fail(add_loc exn) }
    | Sleep sleeper ->
        let res = temp t in
        let data = !current_data in
        add_immutable_waiter sleeper
          (function
             | Return v -> current_data := data; connect res (try f v with exn -> fail (add_loc exn))
             | Fail exn -> fast_connect res (Fail(add_loc exn))
             | _ -> assert false);
        res
    | Repr _ ->
        assert false

let backtrace_catch add_loc x f =
  let t = repr (try x () with exn -> fail exn) in
  match t.state with
    | Return _ ->
        thread t
    | Fail exn ->
        f (add_loc exn)
    | Sleep sleeper ->
        let res = temp t in
        let data = !current_data in
        add_immutable_waiter sleeper
          (function
             | Return _ as state -> fast_connect res state
             | Fail exn -> current_data := data; connect res (try f exn with exn -> fail (add_loc exn))
             | _ -> assert false);
        res
    | Repr _ ->
        assert false

let backtrace_try_bind add_loc x f g =
  let t = repr (try x () with exn -> fail exn) in
  match t.state with
    | Return v ->
        f v
    | Fail exn ->
        g (add_loc exn)
    | Sleep sleeper ->
        let res = temp t in
        let data = !current_data in
        add_immutable_waiter sleeper
          (function
             | Return v -> current_data := data; connect res (try f v with exn -> fail (add_loc exn))
             | Fail exn -> current_data := data; connect res (try g exn with exn -> fail (add_loc exn))
             | _ -> assert false);
        res
    | Repr _ ->
        assert false

let backtrace_finalize add_loc f g =
  backtrace_try_bind add_loc f
    (fun x -> g () >>= fun () -> return x)
    (fun e -> g () >>= fun () -> fail (add_loc e))

(* +-----------------------------------------------------------------+
   | Threads state query                                             |
   +-----------------------------------------------------------------+ *)

let rec is_sleeping_rec t =
  match t.state with
    | Return _ | Fail _ ->
        false
    | Sleep _ ->
        true
    | Repr t ->
        is_sleeping_rec t

let is_sleeping t = is_sleeping_rec (thread_repr t)

module State = struct
  type 'a state =
    | Return of 'a
    | Fail of exn
    | Sleep
end

let state t = match (repr t).state with
  | Return v -> State.Return v
  | Fail exn -> State.Fail exn
  | Sleep _ -> State.Sleep
  | Repr _ -> assert false

include State
