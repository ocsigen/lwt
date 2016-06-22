(*****************************************************************************)
(* Lightweight thread library for OCaml
 * http://www.ocsigen.org/lwt
 * Interface Lwt_ivar
 ******************************************************************************
 * Copyright (c) 2016, Richard M Neswold, Jr.
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
 *****************************************************************************)

(** Immutable variables *)

(** Immutable variables implement a variable whose value may be
    resolved at some future time. Threads attempting to get the
    variable's value before it is determined will be blocked. *)

exeception Already_set

type 'a t
(** The type of an immutable variable. IVars are used to hold values
    that are determined at a future point in time. Once determined, an
    IVar's value never changes. *)

val create : unit -> 'a t
(** [create ()] creates a new, undetermined ivar. *)

val put : 'a t -> 'a -> unit Lwt.t
(** [put ivar value] puts a value into an ivar. All threads currently
     waiting for a value (via get) will complete. Trying to assign a
     different value to an ivar raises [Already_set]. *)

val get : 'a t -> 'a Lwt.t
(** [get ivar] will return the value associated with the ivar or block
    until the value is determined. *)
