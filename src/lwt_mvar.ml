(* -*- Mode: Caml; indent-tabs-mode: nil -*- *)
(******************************************************************************)
(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Interface Lwt_mon
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

type 'a t = {
  mutable writers : unit Lwt.t Queue.t;
  (* Threads waiting to be notified that the value they put has been
     read *)

  mutable readers : 'a Lwt.t Queue.t;
  (* Threads waiting for a value *)

  mutable values : 'a Queue.t;
  (* Value queued *)
}

let create () = { writers = Queue.create ();
                  readers = Queue.create ();
                  values = Queue.create () }

let put mvar v =
  if Queue.is_empty mvar.readers then begin
    Queue.add v mvar.values;
    if Queue.length mvar.values = 1 then
      return ()
    else begin
      let w = wait () in
      Queue.add w mvar.writers;
      w
    end
  end else begin
    wakeup (Queue.take mvar.readers) v;
    return ()
  end

let take mvar =
  if Queue.is_empty mvar.values then begin
    let w = wait () in
    Queue.add w mvar.readers;
    w
  end else
    return (Queue.take mvar.values)
