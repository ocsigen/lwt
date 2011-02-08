(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_engine
 * Copyright (C) 2011 Jérémie Dimino
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
   | Events                                                          |
   +-----------------------------------------------------------------+ *)

type event = {
  stop : unit Lazy.t;
  (* The stop method of the event. *)
  node : Obj.t Lwt_sequence.node;
  (* The node in the sequence of registered events. *)
}

external cast_node : 'a Lwt_sequence.node -> Obj.t Lwt_sequence.node = "%identity"

let stop_event ev =
  Lwt_sequence.remove ev.node;
  Lazy.force ev.stop

let fake_event = {
  stop = lazy ();
  node = Lwt_sequence.add_l (Obj.repr ()) (Lwt_sequence.create ());
}

(* +-----------------------------------------------------------------+
   | Engines                                                         |
   +-----------------------------------------------------------------+ *)

class virtual t = object(self)
  method virtual iter : bool -> unit
  method virtual private cleanup : unit
  method virtual private register_readable : Unix.file_descr -> (unit -> unit) -> unit Lazy.t
  method virtual private register_writable : Unix.file_descr -> (unit -> unit) -> unit Lazy.t
  method virtual private register_timer : float -> bool -> (unit -> unit) -> unit Lazy.t

  val readables = Lwt_sequence.create ()
    (* Sequence of callbacks waiting for a file descriptor to become
       readable. *)

  val writables = Lwt_sequence.create ()
    (* Sequence of callbacks waiting for a file descriptor to become
       writable. *)

  val timers = Lwt_sequence.create ()
    (* Sequence of timers. *)

  method destroy =
    Lwt_sequence.empty readables;
    Lwt_sequence.empty writables;
    Lwt_sequence.empty timers;
    self#cleanup

  method copy (engine : t) =
    Lwt_sequence.iter_l (fun (fd, f) -> ignore (engine#on_readable fd f)) readables;
    Lwt_sequence.iter_l (fun (fd, f) -> ignore (engine#on_readable fd f)) writables;
    Lwt_sequence.iter_l (fun (delay, repeat, f) -> ignore (engine#on_timer delay repeat f)) timers

  method on_readable fd f =
    let ev_cell = ref fake_event in
    let stop = self#register_readable fd (fun () -> f !ev_cell) in
    let ev = { stop = stop; node = cast_node (Lwt_sequence.add_r (fd, f) readables) } in
    ev_cell := ev;
    ev

  method on_writable fd f =
    let ev_cell = ref fake_event in
    let stop = self#register_writable fd (fun () -> f !ev_cell) in
    let ev = { stop = stop; node = cast_node (Lwt_sequence.add_r (fd, f) writables) } in
    ev_cell := ev;
    ev

  method on_timer delay repeat f =
    let ev_cell = ref fake_event in
    let stop = self#register_timer delay repeat (fun () -> f !ev_cell) in
    let ev = { stop = stop; node = cast_node (Lwt_sequence.add_r (delay, repeat, f) timers) } in
    ev_cell := ev;
    ev
end

(* +-----------------------------------------------------------------+
   | The libev engine                                                |
   +-----------------------------------------------------------------+ *)

type ev_loop
type ev_io
type ev_timer

external ev_init : unit -> ev_loop = "lwt_libev_init"
external ev_stop : ev_loop -> unit = "lwt_libev_stop"
external ev_loop : ev_loop -> bool -> unit = "lwt_libev_loop"
external ev_unloop : ev_loop -> unit = "lwt_libev_unloop"
external ev_readable_init : ev_loop -> Unix.file_descr -> (unit -> unit) -> ev_io = "lwt_libev_readable_init"
external ev_writable_init : ev_loop -> Unix.file_descr -> (unit -> unit) -> ev_io = "lwt_libev_writable_init"
external ev_io_stop : ev_loop -> ev_io -> unit = "lwt_libev_io_stop"
external ev_timer_init : ev_loop -> float -> bool -> (unit -> unit) -> ev_timer = "lwt_libev_timer_init"
external ev_timer_stop : ev_loop -> ev_timer -> unit  = "lwt_libev_timer_stop"

class libev = object
  inherit t

  val loop = ev_init ()
  method loop = loop

  method private cleanup = ev_stop loop

  method iter block =
    try
      ev_loop loop block
    with exn ->
      ev_unloop loop;
      raise exn

  method private register_readable fd f =
    let ev = ev_readable_init loop fd f in
    lazy(ev_io_stop loop ev)

  method private register_writable fd f =
    let ev = ev_writable_init loop fd f in
    lazy(ev_io_stop loop ev)

  method private register_timer delay repeat f =
    let ev = ev_timer_init loop delay repeat f in
    lazy(ev_timer_stop loop ev)
end

(* +-----------------------------------------------------------------+
   | The select engine                                               |
   +-----------------------------------------------------------------+ *)

(* Type of a sleeper for the select engine. *)
type sleeper = {
  mutable time : float;
  (* The time at which the sleeper should be wakeup. *)

  mutable stopped : bool;
  (* [true] iff the event has been stopped. *)

  action : unit -> unit;
  (* The action for the sleeper. *)
}

module Sleep_queue =
  Lwt_pqueue.Make(struct
                    type t = sleeper
                    let compare { time = t1 } { time = t2 } = compare t1 t2
                  end)

module Fd_map = Map.Make(struct type t = Unix.file_descr let compare = compare end)

let rec restart_actions sleep_queue now =
  match Sleep_queue.lookup_min sleep_queue with
    | Some{ stopped = true } ->
        restart_actions (Sleep_queue.remove_min sleep_queue) now
    | Some{ time = time; action = action } when time <= now ->
        action ();
        restart_actions (Sleep_queue.remove_min sleep_queue) now
    | _ ->
        sleep_queue

let rec get_next_timeout sleep_queue =
  match Sleep_queue.lookup_min sleep_queue with
    | Some{ stopped = true } ->
        get_next_timeout (Sleep_queue.remove_min sleep_queue)
    | Some{ time = time } ->
        max 0. (time -. Unix.gettimeofday ())
    | None ->
        -1.

let bad_fd fd =
  try
    let _ = Unix.fstat fd in
    false
  with Unix.Unix_error (_, _, _) ->
    true

class select = object(self)
  inherit t

  val mutable sleep_queue = Sleep_queue.empty
    (* Threads waiting for a timeout to expire. *)

  val mutable new_sleeps = []
    (* Sleepers added since the last iteration of the main loop:

       They are not added immediatly to the main sleep queue in order
       to prevent them from being wakeup immediatly.  *)

  val mutable wait_readable = Fd_map.empty
    (* Sequences of actions waiting for file descriptors to become
       readable. *)

  val mutable wait_writable = Fd_map.empty
    (* Sequences of actions waiting for file descriptors to become
       writable. *)

  method private cleanup = ()

  method iter block =
    (* Transfer all sleepers added since the last iteration to the
       main sleep queue: *)
    sleep_queue <- List.fold_left (fun q e -> Sleep_queue.add e q) sleep_queue new_sleeps;
    new_sleeps <- [];
    (* Collect file descriptors. *)
    let fds_r = Fd_map.fold (fun fd _ l -> fd :: l) wait_readable [] in
    let fds_w = Fd_map.fold (fun fd _ l -> fd :: l) wait_writable [] in
    (* Compute the timeout. *)
    let timeout = if block then get_next_timeout sleep_queue else 0. in
    (* Do the blocking call *)
    let fds_r, fds_w, fds_e =
      try
        Unix.select fds_r fds_w [] timeout
      with
        | Unix.Unix_error (Unix.EINTR, _, _) ->
            ([], [], [])
        | Unix.Unix_error (Unix.EBADF, _, _) ->
            (* Keeps only bad file descriptors. Actions registered on
               them have to handle the error: *)
            (List.filter bad_fd fds_r,
             List.filter bad_fd fds_w,
             [])
    in
    (* Restart threads waiting for a timeout: *)
    sleep_queue <- restart_actions sleep_queue (Unix.gettimeofday ());
    (* Restart threads waiting on a file descriptors: *)
    List.iter (fun fd -> Lwt_sequence.iter_l (fun f -> f ()) (Fd_map.find fd wait_readable)) fds_r;
    List.iter (fun fd -> Lwt_sequence.iter_l (fun f -> f ()) (Fd_map.find fd wait_writable)) fds_w

  method private register_timer delay repeat f =
    if repeat then begin
      let rec sleeper = { time = Unix.gettimeofday () +. delay; stopped = false; action = g }
      and g () =
        sleeper.time <- Unix.gettimeofday () +. delay;
        new_sleeps <- sleeper :: new_sleeps;
        f ()
      in
      new_sleeps <- sleeper :: new_sleeps;
      lazy(sleeper.stopped <- true)
    end else begin
      let sleeper = { time = Unix.gettimeofday () +. delay; stopped = false; action = f } in
      new_sleeps <- sleeper :: new_sleeps;
      lazy(sleeper.stopped <- true)
    end

  method private register_readable fd f =
    let actions =
      try
        Fd_map.find fd wait_readable
      with Not_found ->
        let actions = Lwt_sequence.create () in
        wait_readable <- Fd_map.add fd actions wait_readable;
        actions
    in
    let node = Lwt_sequence.add_l f actions in
    lazy(Lwt_sequence.remove node;
         if Lwt_sequence.is_empty actions then wait_readable <- Fd_map.remove fd wait_readable)

  method private register_writable fd f =
    let actions =
      try
        Fd_map.find fd wait_writable
      with Not_found ->
        let actions = Lwt_sequence.create () in
        wait_writable <- Fd_map.add fd actions wait_writable;
        actions
    in
    let node = Lwt_sequence.add_l f actions in
    lazy(Lwt_sequence.remove node;
         if Lwt_sequence.is_empty actions then wait_writable <- Fd_map.remove fd wait_writable)
end

(* +-----------------------------------------------------------------+
   | The current engine                                              |
   +-----------------------------------------------------------------+ *)

let current = ref (if Sys.os_type <> "Unix" then (new select :> t) else (new libev :> t))

let get () =
  !current

let set engine =
  !current#copy engine;
  !current#destroy;
  current := engine

let iter block = !current#iter block
let on_readable fd f = !current#on_readable fd f
let on_writable fd f = !current#on_writable fd f
let on_timer delay repeat f = !current#on_timer delay repeat f
