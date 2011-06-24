(*
 * lwt_react.ml
 * ------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of lwt.
 *)

open Lwt

type 'a event = 'a React.event
type 'a signal = 'a React.signal

module E = struct
  include React.E

  (* +---------------------------------------------------------------+
     | Lwt-specific utilities                                        |
     +---------------------------------------------------------------+ *)

  let finalise f _ = f ()

  let with_finaliser f event =
    let r = ref () in
    Gc.finalise (finalise f) r;
    map (fun x -> ignore r; x) event

  let next ev =
    let waiter, wakener = Lwt.task () in
    let ev = map (fun x -> Lwt.wakeup wakener x) (once ev) in
    Lwt.on_cancel waiter (fun () -> stop ev);
    waiter

  let limit f e =
    (* Thread which prevent [e] to occur while it is sleeping *)
    let limiter = ref (return ()) in

    (* The occurence that is delayed until the limiter returns. *)
    let delayed = ref None in

    (* The resulting event. *)
    let event, push = create () in

    let iter =
      fmap
        (fun x ->
           if state !limiter = Sleep then begin
             (* The limiter is sleeping, we queue the event for later
                delivering. *)
             match !delayed with
               | Some cell ->
                   (* An occurence is alreayd queued, replace it. *)
                   cell := x;
                   None
               | None ->
                   let cell = ref x in
                   delayed := Some cell;
                   on_success !limiter (fun () -> push !cell);
                   None
           end else begin
             (* Set the limiter for future events. *)
             limiter := f ();
             (* Send the occurence now. *)
             push x;
             None
           end)
        e
    in

    select [iter; event]

  let stop_from wakener () =
    wakeup wakener None

  let from f =
    let event, push = create () in
    let abort_waiter, abort_wakener = Lwt.wait () in
    let rec loop () =
      pick [f () >|= (fun x -> Some x); abort_waiter] >>= function
        | Some v ->
            push v;
            loop ()
        | None ->
            stop event;
            return ()
    in
    ignore_result (pause () >>= loop);
    with_finaliser (stop_from abort_wakener) event

  module EQueue :
  sig
    type 'a t
    val create : 'a React.event -> 'a t
    val pop : 'a t -> 'a option Lwt.t
  end =
  struct

    type 'a state =
      | No_mail
      | Waiting of 'a option Lwt.u
      | Full of 'a Queue.t

    type 'a t = {
      mutable state : 'a state;
      mutable event : unit React.event;
      (* field used to prevent garbage collection *)
    }

    let create event =
      let box = { state = No_mail; event = never } in
      let push v =
        match box.state with
	  | No_mail ->
	      let q = Queue.create () in
	      Queue.push v q;
	      box.state <- Full q
	  | Waiting wakener ->
              box.state <- No_mail;
              wakeup_later wakener (Some v)
	  | Full q ->
	      Queue.push v q
      in
      box.event <- map push event;
      box

    let pop b = match b.state with
      | No_mail ->
	  let waiter, wakener = task () in
          Lwt.on_cancel waiter (fun () -> b.state <- No_mail);
	  b.state <- Waiting wakener;
	  waiter
      | Waiting _ ->
          (* Calls to next are serialized, so this case will never
             happened *)
	  assert false
      | Full q ->
	  let v = Queue.take q in
	  if Queue.is_empty q then b.state <- No_mail;
          return (Some v)
  end

  let to_stream event =
    let box = EQueue.create event in
    Lwt_stream.from (fun () -> EQueue.pop box)

  let stop_stream wakener () =
    wakeup wakener None

  let of_stream stream =
    let event, push = create () in
    let abort_waiter, abort_wakener = Lwt.wait () in
    let rec loop () =
      pick [Lwt_stream.get stream; abort_waiter] >>= function
        | Some value ->
            push value;
            loop ()
        | None ->
            stop event;
            return ()
    in
    ignore_result (pause () >>= loop);
    with_finaliser (stop_stream abort_wakener) event

  let delay thread =
    match poll thread with
      | Some e ->
          e
      | None ->
          let event, send = create () in
          on_success thread (fun e -> send e; stop event);
          switch never event

  let keeped = ref []

  let keep e =
    keeped := map ignore e :: !keeped

  (* +---------------------------------------------------------------+
     | Event transofrmations                                         |
     +---------------------------------------------------------------+ *)

  let run_p e =
    let event, push = create () in
    let iter = fmap (fun t -> on_success t push; None) e in
    select [iter; event]

  let run_s e =
    let event, push = create () in
    let mutex = Lwt_mutex.create () in
    let iter = fmap (fun t -> on_success (Lwt_mutex.with_lock mutex (fun () -> t)) push; None) e in
    select [iter; event]

  let map_p f e =
    let event, push = create () in
    let iter = fmap (fun x -> on_success (f x) push; None) e in
    select [iter; event]

  let map_s f e =
    let event, push = create () in
    let mutex = Lwt_mutex.create () in
    let iter = fmap (fun x -> on_success (Lwt_mutex.with_lock mutex (fun () -> f x)) push; None) e in
    select [iter; event]

  let app_p ef e =
    let event, push = create () in
    let iter = fmap (fun (f, x) -> on_success (f x) push; None) (app (map (fun f x -> (f, x)) ef) e) in
    select [iter; event]

  let app_s ef e =
    let event, push = create () in
    let mutex = Lwt_mutex.create () in
    let iter = fmap (fun (f, x) -> on_success (Lwt_mutex.with_lock mutex (fun () -> f x)) push; None) (app (map (fun f x -> (f, x)) ef) e) in
    select [iter; event]

  let filter_p f e =
    let event, push = create () in
    let iter = fmap (fun x -> on_success (f x) (function true -> push x | false -> ()); None) e in
    select [iter; event]

  let filter_s f e =
    let event, push = create () in
    let mutex = Lwt_mutex.create () in
    let iter = fmap (fun x -> on_success (Lwt_mutex.with_lock mutex (fun () -> f x)) (function true -> push x | false -> ()); None) e in
    select [iter; event]

  let fmap_p f e =
    let event, push = create () in
    let iter = fmap (fun x -> on_success (f x) (function Some x -> push x | None -> ()); None) e in
    select [iter; event]

  let fmap_s f e =
    let event, push = create () in
    let mutex = Lwt_mutex.create () in
    let iter = fmap (fun x -> on_success (Lwt_mutex.with_lock mutex (fun () -> f x)) (function Some x -> push x | None -> ()); None) e in
    select [iter; event]

  let diff_s f e =
    let previous = ref None in
    let event, push = create () in
    let mutex = Lwt_mutex.create () in
    let iter =
      fmap
        (fun x ->
           match !previous with
             | None ->
                 previous := Some x;
                 None
             | Some y ->
                 previous := Some x;
                 on_success (Lwt_mutex.with_lock mutex (fun () -> f x y)) push;
                 None)
        e
    in
    select [iter; event]

  let accum_s ef acc =
    let acc = ref acc in
    let event, push = create () in
    let mutex = Lwt_mutex.create () in
    let iter = fmap (fun f -> on_success (Lwt_mutex.with_lock mutex (fun () -> f !acc)) (fun x -> acc := x; push x); None) ef in
    select [iter; event]

  let fold_s f acc e =
    let acc = ref acc in
    let event, push = create () in
    let mutex = Lwt_mutex.create () in
    let iter = fmap (fun x -> on_success (Lwt_mutex.with_lock mutex (fun () -> f !acc x)) (fun x -> acc := x; push x); None) e in
    select [iter; event]

  let rec rev_fold f acc = function
    | [] ->
        return acc
    | x :: l ->
        lwt acc = rev_fold f acc l in
        f acc x

  let merge_s f acc el =
    let event, push = create () in
    let mutex = Lwt_mutex.create () in
    let iter = fmap (fun l -> on_success (Lwt_mutex.with_lock mutex (fun () -> rev_fold f acc l)) push; None) (merge (fun acc x -> x :: acc) [] el) in
    select [iter; event]
end

module S = struct
  include React.S

  (* +---------------------------------------------------------------+
     | Lwt-specific utilities                                        |
     +---------------------------------------------------------------+ *)

  let finalise f _ = f ()

  let with_finaliser f signal =
    let r = ref () in
    Gc.finalise (finalise f) r;
    map (fun x -> ignore r; x) signal

  let limit ?eq f s =
    (* Thread which prevent [s] to changes while it is sleeping *)
    let limiter = ref (f ()) in

    (* The occurence that is delayed until the limiter returns. *)
    let delayed = ref None in

    (* The resulting event. *)
    let event, push = E.create () in

    let iter =
      E.fmap
        (fun x ->
           if state !limiter = Sleep then begin
             (* The limiter is sleeping, we queue the event for later
                delivering. *)
             match !delayed with
               | Some cell ->
                   (* An occurence is alreayd queued, replace it. *)
                   cell := x;
                   None
               | None ->
                   let cell = ref x in
                   delayed := Some cell;
                   on_success !limiter (fun () -> push !cell);
                   None
           end else begin
             (* Set the limiter for future events. *)
             limiter := f ();
             (* Send the occurence now. *)
             push x;
             None
           end)
        (changes s)
    in

    hold ?eq (value s) (E.select [iter; event])

  let keeped = ref []

  let keep s =
    keeped := map ignore s :: !keeped

  (* +---------------------------------------------------------------+
     | Signal transofrmations                                        |
     +---------------------------------------------------------------+ *)

  let run_s ?eq s =
    let event, push = E.create () in
    let mutex = Lwt_mutex.create () in
    let iter = E.fmap (fun t -> on_success (Lwt_mutex.with_lock mutex (fun () -> t)) push; None) (changes s) in
    lwt x = Lwt_mutex.with_lock mutex (fun () -> value s) in
    return (hold ?eq x (E.select [iter; event]))

  let map_s ?eq f s =
    let event, push = E.create () in
    let mutex = Lwt_mutex.create () in
    let iter = E.fmap (fun x -> on_success (Lwt_mutex.with_lock mutex (fun () -> f x)) push; None) (changes s) in
    lwt x = Lwt_mutex.with_lock mutex (fun () -> f (value s)) in
    return (hold ?eq x (E.select [iter; event]))

  let app_s ?eq sf s =
    let event, push = E.create () in
    let mutex = Lwt_mutex.create () in
    let iter = E.fmap (fun (f, x) -> on_success (Lwt_mutex.with_lock mutex (fun () -> f x)) push; None) (E.app (E.map (fun f x -> (f, x)) (changes sf)) (changes s)) in
    lwt x = Lwt_mutex.with_lock mutex (fun () -> (value sf) (value s)) in
    return (hold ?eq x (E.select [iter; event]))

  let filter_s ?eq f i s =
    let event, push = E.create () in
    let mutex = Lwt_mutex.create () in
    let iter = E.fmap (fun x -> on_success (Lwt_mutex.with_lock mutex (fun () -> f x)) (function true -> push x | false -> ()); None) (changes s) in
    let x = value s in
    Lwt_mutex.with_lock mutex (fun () -> f x) >>= function
      | true ->
          return (hold ?eq x (E.select [iter; event]))
      | false ->
          return (hold ?eq i (E.select [iter; event]))

  let fmap_s ?eq f i s =
    let event, push = E.create () in
    let mutex = Lwt_mutex.create () in
    let iter = E.fmap (fun x -> on_success (Lwt_mutex.with_lock mutex (fun () -> f x)) (function Some x -> push x | None -> ()); None) (changes s) in
    Lwt_mutex.with_lock mutex (fun () -> f (value s)) >>= function
      | Some x ->
          return (hold ?eq x (E.select [iter; event]))
      | None ->
          return (hold ?eq i (E.select [iter; event]))

  let diff_s f s =
    let previous = ref (value s) in
    let event, push = E.create () in
    let mutex = Lwt_mutex.create () in
    let iter =
      E.fmap
        (fun x ->
           let y = !previous in
           previous := x;
           on_success (Lwt_mutex.with_lock mutex (fun () -> f x y)) push;
           None)
        (changes s)
    in
    E.select [iter; event]

  let sample_s f e s =
    E.map_s (fun x -> f x (value s)) e

  let accum_s ?eq ef i =
    hold ?eq i (E.accum_s ef i)

  let fold_s ?eq f i e =
    hold ?eq i (E.fold_s f i e)

  let rec rev_fold f acc = function
    | [] ->
        return acc
    | x :: l ->
        lwt acc = rev_fold f acc l in
        f acc x

  let merge_s ?eq f acc sl =
    let s = merge (fun acc x -> x :: acc) [] sl in
    let event, push = E.create () in
    let mutex = Lwt_mutex.create () in
    let iter = E.fmap (fun l -> on_success (Lwt_mutex.with_lock mutex (fun () -> rev_fold f acc l)) push; None) (changes s) in
    lwt x = Lwt_mutex.with_lock mutex (fun () -> rev_fold f acc (value s)) in
    return (hold ?eq x (E.select [iter; event]))

  let l1_s ?eq f s1 =
    map_s ?eq f s1

  let l2_s ?eq f s1 s2 =
    map_s ?eq (fun (x1, x2) -> f x1 x2) (l2 (fun x1 x2 -> (x1, x2)) s1 s2)

  let l3_s ?eq f s1 s2 s3 =
    map_s ?eq (fun (x1, x2, x3) -> f x1 x2 x3) (l3 (fun x1 x2 x3-> (x1, x2, x3)) s1 s2 s3)

  let l4_s ?eq f s1 s2 s3 s4 =
    map_s ?eq (fun (x1, x2, x3, x4) -> f x1 x2 x3 x4) (l4 (fun x1 x2 x3 x4-> (x1, x2, x3, x4)) s1 s2 s3 s4)

  let l5_s ?eq f s1 s2 s3 s4 s5 =
    map_s ?eq (fun (x1, x2, x3, x4, x5) -> f x1 x2 x3 x4 x5) (l5 (fun x1 x2 x3 x4 x5-> (x1, x2, x3, x4, x5)) s1 s2 s3 s4 s5)

  let l6_s ?eq f s1 s2 s3 s4 s5 s6 =
    map_s ?eq (fun (x1, x2, x3, x4, x5, x6) -> f x1 x2 x3 x4 x5 x6) (l6 (fun x1 x2 x3 x4 x5 x6-> (x1, x2, x3, x4, x5, x6)) s1 s2 s3 s4 s5 s6)

  (* +---------------------------------------------------------------+
     | Monadic interface                                             |
     +---------------------------------------------------------------+ *)

  let return =
    const

  let bind ?eq s f =
    switch ?eq (f (value s)) (E.map f (changes s))

  let bind_s ?eq s f =
    let event, push = E.create () in
    let mutex = Lwt_mutex.create () in
    let iter = E.fmap (fun x -> on_success (Lwt_mutex.with_lock mutex (fun () -> f x)) push; None) (changes s) in
    lwt x = Lwt_mutex.with_lock mutex (fun () -> f (value s)) in
    Lwt.return (switch ?eq x (E.select [iter; event]))
end
