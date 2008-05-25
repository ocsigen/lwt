(* Ocsigen
 * http://www.ocsigen.org
 * Module lwt_preemptive.ml
 * Copyright (C) 2005 Nataliya Guts, Vincent Balat, Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Lwt;;

exception Task_failed

let minthreads : int ref = ref 0
let maxthreads : int ref = ref 0

let maxthreadqueued = ref 100

let get_max_number_of_threads_queued () = !maxthreadqueued
let set_max_number_of_threads_queued n = maxthreadqueued := n

let finishedpipe =
  let (in_fd, out_fd) = Lwt_unix.pipe_in () in
  Lwt_unix.set_close_on_exec in_fd;
  Unix.set_close_on_exec out_fd;
  (Lwt_chan.in_channel_of_descr in_fd, Unix.out_channel_of_descr out_fd)

let pipelock = Mutex.create ()

let worker_chan n : (unit -> unit) Event.channel = Event.new_channel ()
type th = {mutable client: unit Lwt.t;
           mutable busy: bool;
           taskchannel: (unit -> unit) Event.channel;
           mutable worker: Thread.t option}
let pool : th array ref = ref [||]

let busylock = Mutex.create ()
let setbusy n b =
  Mutex.lock busylock;
  !pool.(n).busy <- b;
  Mutex.unlock busylock

let rec worker (n : int) : unit Lwt.t =
  let g = Event.sync (Event.receive !pool.(n).taskchannel) in
  g ();
  let buf = string_of_int n in
  Mutex.lock pipelock;
  output_string (snd finishedpipe) (buf^"\n");
  flush (snd finishedpipe);
  Mutex.unlock pipelock;
  worker n


exception All_preemptive_threads_are_busy
let free, nbthreadsqueued =
  let nb_threads_queued = ref 0 in
  let max_thread_waiting_queue =
    get_max_number_of_threads_queued () in
  let rec free1 i : int =
    if i >= !maxthreads
    then raise All_preemptive_threads_are_busy
    else if not !pool.(i).busy then i else free1 (i+1)
  in
  let launch_threads first =
    let rec aux last n =
      !pool.(n).worker <- Some (Thread.create worker n);
      if n<last then aux last (n+1)
    in
    match !pool.(first).worker with
      None ->
        let last = (min (first + (max !minthreads 10)) !maxthreads) - 1 in
        aux last first
    | _ -> ()
  in
  let rec aux () =
    try
      Mutex.lock busylock;
      let libre = free1 0 in
      !pool.(libre).busy <- true;
      Mutex.unlock busylock;
      launch_threads libre;
      Lwt.return libre
    with
      All_preemptive_threads_are_busy ->
        Mutex.unlock busylock;
        if (!maxthreads = 0) ||
        (!nb_threads_queued >= max_thread_waiting_queue)
        then fail All_preemptive_threads_are_busy
        else (nb_threads_queued := !nb_threads_queued + 1;
              Lwt_unix.sleep 1.0 >>= (fun () ->
                nb_threads_queued := !nb_threads_queued -1 ;
                aux ()))
    | e -> Mutex.unlock busylock; fail e
  in
  (aux,(fun () -> !nb_threads_queued))

let detach (f : 'a -> 'b) (args : 'a) : 'b Lwt.t =
  let res : 'b option ref = ref None in
  let exc : exn option ref = ref None in
  let g () = try res := Some (f args) with e -> exc := Some e
  in
  free () >>= (fun whatthread ->
    Event.sync (Event.send !pool.(whatthread).taskchannel g);
    !pool.(whatthread).client <- Lwt.wait ();
    !pool.(whatthread).client >>=
    (fun () -> match !res with
    | None ->
        (match !exc with
        | None ->
            setbusy whatthread false;
            fail Task_failed
        | Some e ->
            setbusy whatthread false;
            fail e)
    | Some r ->
        setbusy whatthread false;
        Lwt.return r))


let dispatch errlog =
  let rec aux () =
    (catch
       (fun () ->
         Lwt_chan.input_line (fst finishedpipe) >>=
         (fun v ->
           let n = int_of_string v in

           ignore (
           (* Here we want to do the recursive call as soon as possible
              (and before the wakeup)
              because if Lwt_unix.run is called by the waiters of the
              thread beeing awoken,
              and if that run wants to use detach,
              the pipe won't be available, and the run will never finish ...
              and block the other run
              (remember that an invocation of [run] will not terminate
              before all subsequent invocations are terminated)
            *)
           Lwt_unix.yield () >>= fun () ->
             wakeup !pool.(n).client ();
             return ());
           return ()))

       (fun e ->
          errlog
            ("Internal error in lwt_preemptive.ml (read failed on the pipe) "^
               Printexc.to_string e ^" - Please check if Lwt_preemptive is initialized and that lwt_preemptive.cmo is linked only once. Otherwise, please report the bug");
         return ())
    ) >>= (fun () -> aux ())
  in aux ()


let initthread =
  let def = Lwt.return () in
  fun n ->
    {client = def;
     busy = false;
     taskchannel = worker_chan n;
     worker = None}

let rec start_initial_threads min n =
  if n<min
  then begin
    !pool.(n).worker <- Some (Thread.create worker n);
    start_initial_threads min (n+1);
  end

let init min max errlog =
  pool := Array.init max initthread;
  minthreads := min;
  maxthreads := max;
  start_initial_threads min 0;
  dispatch errlog


let nbthreads () =
  Array.fold_left (fun nb elt ->
    match elt.worker with None -> nb | _ -> nb+1) 0 !pool

let nbthreadsbusy () =
  Mutex.lock busylock;
  let r =
    Array.fold_left (fun nb elt -> if elt.busy then nb+1 else nb) 0 !pool in
  Mutex.unlock busylock;
  r

