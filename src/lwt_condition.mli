(* -*- Mode: Caml; indent-tabs-mode: nil -*- *)
(******************************************************************************)
(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Interface Lwt_condition
 ******************************************************************************
 * Copyright (c) 2009, Metaweb Technologies, Inc.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above
 * copyright notice, this list of conditions and the following
 * disclaimer in the documentation and/or other materials provided
 * with the distribution.
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

(** Condition variables *)

(** Condition variables implement the ability for Lwt threads to wait
    for notification. *)

type 'a t
    (** Condition variable type. The type parameter denotes the type of
        value propagated from notifier to waiter. Condition variables should
        be used in conjunction with {Lwt_monitor.t} to synchronize
        notifications. *)

val create : unit -> 'a t
    (** [create ()] creates a new condition variable. *)

val wait : 'a t -> 'a Lwt.t
    (** [wait condvar] waits for a condition notification to occur.
        When the awaited condition is notified, the value parameter passed
        to {notify} is returned. *)

val notify : 'a t -> 'a -> unit
    (** [notify condvar value] notifies that a condition is ready. A single
        waiting thread will be awoken and will receive the notification value
        which will be returned from {wait}. Note that condition notification
        is not "sticky", i.e. if there is no waiter when {notify} is called,
        the notification will be missed and the value discarded. *)

val notify_all : 'a t -> 'a -> unit
    (** [notify_all condvar value] notifies all waiting threads. Each will
        be awoken in turn and will receive the same notification value. *)
