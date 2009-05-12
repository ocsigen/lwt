(* -*- Mode: Caml; indent-tabs-mode: nil -*- *)
(******************************************************************************)
(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Interface Lwt_monitor
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

type t = { mutable locked : bool; enter : unit Lwt_condition.t }

let create () = { locked = false; enter = Lwt_condition.create () }

let rec lock m =
  if m.locked then begin
    Lwt_condition.wait m.enter >> lock m
  end else begin
    assert (not m.locked);
    m.locked <- true;
    return ()
  end

let unlock m =
  assert (m.locked);
  m.locked <- false;
  Lwt_condition.notify m.enter ()

let wait m cond =
  let t = Lwt_condition.wait cond in
  unlock m;
  lwt arg = t in
  lock m >> return arg

let notify m cond arg =
  assert (m.locked);
  Lwt_condition.notify cond arg

let notify_all m cond arg =
  assert (m.locked);
  Lwt_condition.notify_all cond arg

let with_lock m thunk =
  lock m >>
    try_lwt
      lwt rv = thunk () in
      unlock m;
      return rv
    with exn ->
      unlock m;
      fail exn

