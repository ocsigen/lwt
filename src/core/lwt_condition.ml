(* OCaml promise library
 * https://ocsigen.org/lwt
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
 *)

(* [Lwt_sequence] is deprecated – we don't want users outside Lwt using it.
   However, it is still used internally by Lwt. So, briefly disable warning 3
   ("deprecated"), and create a local, non-deprecated alias for
   [Lwt_sequence] that can be referred to by the rest of the code in this
   module without triggering any more warnings. *)
module Lwt_sequence = Lwt_sequence

(* A condition variable belongs to the domain that created it, like a mutex.
   Signaling from another domain would wake promises that domain does not own,
   or, with no waiter, silently do nothing. *)
[@@@alert "-trespassing"]

module Lwt_dls = Lwt_dls

[@@@alert "+trespassing"]

type 'a t = {waiters : 'a Lwt.u Lwt_sequence.t; owner : Lwt_dls.token}

let create () = {waiters = Lwt_sequence.create (); owner = Lwt_dls.self_token ()}

let wait ?mutex cvar =
  Lwt_dls.check_owner "Lwt_condition.wait" cvar.owner;
  let waiter = (Lwt.add_task_r [@ocaml.warning "-3"]) cvar.waiters in
  let () =
    match mutex with
    | Some m -> (
      (* With another domain's mutex, [unlock] raises [Invalid_argument]. Cancel
         the waiter just queued, which removes it, or it would swallow the next
         signal. *)
      try Lwt_mutex.unlock m
      with Invalid_argument _ as exn -> Lwt.cancel waiter; Lwt.reraise exn)
    | None -> ()
  in
  Lwt.finalize
    (fun () -> waiter)
    (fun () ->
       match mutex with Some m -> Lwt_mutex.lock m | None -> Lwt.return_unit)

let signal cvar arg =
  Lwt_dls.check_owner "Lwt_condition.signal" cvar.owner;
  try Lwt.wakeup_later (Lwt_sequence.take_l cvar.waiters) arg
  with Lwt_sequence.Empty -> ()

let take_all cvar =
  let wakeners = Lwt_sequence.fold_r (fun x l -> x :: l) cvar.waiters [] in
  Lwt_sequence.iter_node_l Lwt_sequence.remove cvar.waiters;
  wakeners

let broadcast cvar arg =
  Lwt_dls.check_owner "Lwt_condition.broadcast" cvar.owner;
  List.iter (fun wakener -> Lwt.wakeup_later wakener arg) (take_all cvar)

let broadcast_exn cvar exn =
  Lwt_dls.check_owner "Lwt_condition.broadcast_exn" cvar.owner;
  List.iter (fun wakener -> Lwt.wakeup_later_exn wakener exn) (take_all cvar)
