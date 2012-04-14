(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_main
 * Copyright (C) 2009-2011 Jérémie Dimino
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

open Lwt

let enter_iter_hooks = Lwt_sequence.create ()
let leave_iter_hooks = Lwt_sequence.create ()
let yielded = Lwt_sequence.create ()

let yield () = add_task_r yielded

(* Do not raise exceptions at random places. *)
let () = on_uncaught_exception := ignore

(* Check for uncaught exceptions. *)
let check () =
  if not (Queue.is_empty uncaught_exceptions) then begin
    (* This will be the one raised, we have to choose one. *)
    let exn, _ = Queue.peek uncaught_exceptions in
    (* Print backtraces. *)
    while not (Queue.is_empty uncaught_exceptions) do
      let exn, bt = Queue.take uncaught_exceptions in
      Printf.eprintf "Fatal error: exception %s\n" (Printexc.to_string exn);
      prerr_string (string_of_backtrace bt)
    done;
    flush stderr;
    raise exn
  end

let rec run t =
  (* Wakeup paused threads now. *)
  Lwt.wakeup_paused ();
  check ();
  match Lwt.poll t with
    | Some x ->
        x
    | None ->
        (* Call enter hooks. *)
        Lwt_sequence.iter_l (fun f -> f (); check ()) enter_iter_hooks;
        (* Do the main loop call. *)
        Lwt_engine.iter (Lwt.paused_count () = 0 && Lwt_sequence.is_empty yielded);
        check ();
        (* Wakeup paused threads again. *)
        Lwt.wakeup_paused ();
        check ();
        (* Wakeup yielded threads now. *)
        if not (Lwt_sequence.is_empty yielded) then begin
          let tmp = Lwt_sequence.create () in
          Lwt_sequence.transfer_r yielded tmp;
          Lwt_sequence.iter_l (fun wakener -> wakeup wakener (); check ()) tmp
        end;
        (* Call leave hooks. *)
        Lwt_sequence.iter_l (fun f -> f (); check ()) leave_iter_hooks;
        run t

let exit_hooks = Lwt_sequence.create ()

let rec call_hooks () =
  match Lwt_sequence.take_opt_l exit_hooks with
    | None ->
        return ()
    | Some f ->
        lwt () =
          try_lwt
            f ()
          with exn ->
            return ()
        in
        call_hooks ()

let () = at_exit (fun () -> run (call_hooks ()))
let at_exit f = ignore (Lwt_sequence.add_l f exit_hooks)
