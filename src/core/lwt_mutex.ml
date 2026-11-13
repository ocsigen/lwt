(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* [Lwt_sequence] is deprecated – we don't want users outside Lwt using it.
   However, it is still used internally by Lwt. So, briefly disable warning 3
   ("deprecated"), and create a local, non-deprecated alias for
   [Lwt_sequence] that can be referred to by the rest of the code in this
   module without triggering any more warnings. *)
module Lwt_sequence = Lwt_sequence
open Lwt.Infix

(* A mutex belongs to the domain that created it. Locking a free mutex only
   sets a flag, with no promise for the core to check, so a module-level mutex
   used from another domain would be corrupted silently. Each operation checks
   the domain recorded at creation. *)
[@@@alert "-trespassing"]

module Lwt_dls = Lwt_dls

[@@@alert "+trespassing"]

type t =
  { mutable locked : bool
  ; waiters : unit Lwt.u Lwt_sequence.t
  ; owner : Lwt_dls.token }

let create () =
  { locked = false
  ; waiters = Lwt_sequence.create ()
  ; owner = Lwt_dls.self_token () }

let lock m =
  Lwt_dls.check_owner "Lwt_mutex.lock" m.owner;
  if m.locked
  then (Lwt.add_task_r [@ocaml.warning "-3"]) m.waiters
  else begin
    m.locked <- true;
    Lwt.return_unit
  end

let unlock m =
  Lwt_dls.check_owner "Lwt_mutex.unlock" m.owner;
  if m.locked
  then
    begin if Lwt_sequence.is_empty m.waiters
    then m.locked <- false
    else
      (* We do not use [Lwt.wakeup] here to avoid a stack overflow
         when unlocking a lot of threads. *)
      Lwt.wakeup_later (Lwt_sequence.take_l m.waiters) ()
    end

let with_lock m f =
  lock m >>= fun () -> Lwt.finalize f (fun () -> unlock m; Lwt.return_unit)

(* Not checked: they only read the mutex, like [Lwt.state]. *)
let is_locked m = m.locked
let is_empty m = Lwt_sequence.is_empty m.waiters
