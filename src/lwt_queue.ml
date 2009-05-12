(* -*- Mode: Caml; indent-tabs-mode: nil -*- *)
(******************************************************************************)
(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Interface Lwt_queue
 ******************************************************************************
 * Copyright (c) 2009, Metaweb Technologies, Inc.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials provided
 *       with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY METAWEB TECHNOLOGIES ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL METAWEB TECHNOLOGIES BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 ******************************************************************************)

open Lwt

type 'a t = { queue : 'a Queue.t; mon : Lwt_monitor.t; cv : unit Lwt_condition.t }

let create () =
  { queue = Queue.create ();
    mon = Lwt_monitor.create ();
    cv = Lwt_condition.create () }

let add a q =
  Lwt_monitor.with_lock q.mon begin fun _ ->
    Queue.add a q.queue;
    Lwt_monitor.notify q.mon q.cv ();
    return ()
  end

let push = add

let take q =
  Lwt_monitor.with_lock q.mon begin fun _ ->
    begin
      if Queue.is_empty q.queue then
        Lwt_monitor.wait q.mon q.cv
      else
        return ()
    end >> return (Queue.take q.queue)
  end

let pop = take
let peek q = Queue.peek q.queue
let top = peek
let clear q = Lwt_monitor.with_lock q.mon (fun _ -> return (Queue.clear q.queue))

let copy q =
  Lwt_monitor.with_lock q.mon begin fun () ->
    return { queue = Queue.copy q.queue;
             mon = Lwt_monitor.create ();
             cv = Lwt_condition.create () }
  end

let is_empty q = Queue.is_empty q.queue
let length q = Queue.length q.queue
let iter f q = Lwt_monitor.with_lock q.mon (fun _ -> return (Queue.iter f q.queue))
let fold f a q = Lwt_monitor.with_lock q.mon (fun _ -> return (Queue.fold f a q.queue))
let transfer q1 q2 =
  Lwt_monitor.with_lock q1.mon
    (fun _ ->
       Lwt_monitor.with_lock q2.mon
         (fun _ ->
            return (Queue.transfer q1.queue q2.queue)))
