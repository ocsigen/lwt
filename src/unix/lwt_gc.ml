(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_gc
 * Copyright (C) 2009 Jérémie Dimino
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

let ensure_termination t =
  if Lwt.state t = Lwt.Sleep then begin
    let hook = Lwt_sequence.add_l (fun _ -> t) Lwt_main.exit_hooks in
    (* Remove the hook when t has terminated *)
    ignore (try_lwt t finally Lwt_sequence.remove hook; Lwt.return_unit)
  end

let finaliser f =
  (* In order not to create a reference to the value in the
     notification callback, we use an initially unset option cell
     which will be filled when the finaliser is called. *)
  let opt = ref None in
  let id =
    Lwt_unix.make_notification
      ~once:true
      (fun () ->
         match !opt with
           | None ->
               assert false
           | Some x ->
               opt := None;
               ensure_termination (f x))
  in
  (* The real finaliser: fill the cell and send a notification. *)
  (fun x ->
     opt := Some x;
     Lwt_unix.send_notification id)

let finalise f x =
  Gc.finalise (finaliser f) x

(* Exit hook for a finalise_or_exit *)
let foe_exit f called weak () =
  match Weak.get weak 0 with
    | None ->
        (* The value has been garbage collected, normally this point
           is never reached *)
        Lwt.return_unit
    | Some x ->
        (* Just to avoid double finalisation *)
        Weak.set weak 0 None;
        if !called then
          Lwt.return_unit
        else begin
          called := true;
          f x
        end

(* Finaliser for a finalise_or_exit *)
let foe_finaliser f called hook =
  finaliser
    (fun x ->
       (* Remove the exit hook, it is not needed anymore. *)
       Lwt_sequence.remove hook;
       (* Call the real finaliser. *)
       if !called then
         Lwt.return_unit
       else begin
         called := true;
         f x
       end)

let finalise_or_exit f x =
  (* Create a weak pointer, so the exit-hook does not keep a reference
     to [x]. *)
  let weak = Weak.create 1 in
  Weak.set weak 0 (Some x);
  let called = ref false in
  let hook = Lwt_sequence.add_l (foe_exit f called weak) Lwt_main.exit_hooks in
  Gc.finalise (foe_finaliser f called hook) x
