(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* The timer wheel is per domain: it is driven by an engine timer, and a shared
   wheel would have its lists modified from two domains at once. A timeout
   records its wheel, and [start], [stop] and [change] refuse a timeout whose
   wheel is not the calling domain's. *)
type t =
  { mutable delay : int
  ; action : unit -> unit
  ; mutable prev : t
  ; mutable next : t
  ; owner : wheel }

and wheel =
  { mutable count : int
  ; mutable buckets : t array
  ; mutable curr : int
  ; mutable stopped : bool }

let make owner delay action =
  let rec x = {delay; action; prev = x; next = x; owner} in
  x

let lst_empty owner = make owner (-1) (fun () -> ())

let lst_remove x =
  let p = x.prev in
  let n = x.next in
  p.next <- n;
  n.prev <- p;
  x.next <- x;
  x.prev <- x

let lst_insert p x =
  let n = p.next in
  p.next <- x;
  x.prev <- p;
  x.next <- n;
  n.prev <- x

let lst_in_list x = x.next != x
let lst_is_empty set = set.next == set

let lst_peek s =
  let x = s.next in
  lst_remove x; x

(****)

[@@@alert "-trespassing"]

module Lwt_dls = Lwt_dls

[@@@alert "+trespassing"]

(* The buckets start empty and [size] grows them, which is also what breaks the
   circularity: a bucket's sentinel needs its wheel, so the wheel is built
   first. *)
let wheel : wheel Lwt_dls.t =
  Lwt_dls.new_key (fun () ->
    {count = 0; buckets = [||]; curr = 0; stopped = true})

let[@inline] self_wheel () = Lwt_dls.get wheel

let[@inline never] not_our_wheel name =
  invalid_arg (name ^ ": this timeout belongs to another domain")

let[@inline] check_owner name w x = if x.owner != w then not_our_wheel name

let size w l =
  let len = Array.length w.buckets in
  if l >= len
  then begin
    let b = Array.init (l + 1) (fun _ -> lst_empty w) in
    Array.blit w.buckets w.curr b 0 (len - w.curr);
    Array.blit w.buckets 0 b (len - w.curr) w.curr;
    w.buckets <- b;
    w.curr <- 0
  end

(****)

(* Process-wide, like [Lwt.async_exception_hook] it defaults to. Atomic, as
   for [Lwt.Exception_filter]. *)
let handle_exn = Atomic.make (fun exn -> !Lwt.async_exception_hook exn)
let set_exn_handler f = Atomic.set handle_exn f

let rec loop w =
  w.stopped <- false;
  Lwt.bind (Lwt_unix.sleep 1.) (fun () ->
    let s = w.buckets.(w.curr) in
    while not (lst_is_empty s) do
      let x = lst_peek s in
      w.count <- w.count - 1;
      (*XXX Should probably report any exception *)
      try x.action ()
      with e when Lwt.Exception_filter.run e -> (Atomic.get handle_exn) e
    done;
    w.curr <- (w.curr + 1) mod Array.length w.buckets;
    if w.count > 0
    then loop w
    else begin
      w.stopped <- true;
      Lwt.return_unit
    end)

let start x =
  let w = self_wheel () in
  check_owner "Lwt_timeout.start" w x;
  let in_list = lst_in_list x in
  let slot = (w.curr + x.delay) mod Array.length w.buckets in
  lst_remove x;
  lst_insert w.buckets.(slot) x;
  if not in_list
  then begin
    w.count <- w.count + 1;
    if w.count = 1 && w.stopped then ignore (loop w)
  end

let create delay action =
  if delay < 1 then invalid_arg "Lwt_timeout.create";
  let w = self_wheel () in
  let x = make w delay action in
  size w delay; x

let stop x =
  check_owner "Lwt_timeout.stop" (self_wheel ()) x;
  if lst_in_list x
  then begin
    lst_remove x;
    x.owner.count <- x.owner.count - 1
  end

let change x delay =
  if delay < 1 then invalid_arg "Lwt_timeout.change";
  let w = self_wheel () in
  check_owner "Lwt_timeout.change" w x;
  x.delay <- delay;
  size w delay;
  if lst_in_list x then start x
