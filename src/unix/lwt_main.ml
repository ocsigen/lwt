(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_main
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

open Lwt

external libev_init : unit -> unit = "lwt_libev_init"
external libev_loop : unit -> unit = "lwt_libev_loop"
external libev_loop_no_wait : unit -> unit = "lwt_libev_loop_no_wait"
external libev_unloop : unit -> unit = "lwt_libev_unloop"

let () = libev_init ()

let yielded = Lwt_sequence.create ()

let yield () =
  let waiter, wakener = task () in
  let node = Lwt_sequence.add_l wakener yielded in
  on_cancel waiter (fun () -> Lwt_sequence.remove node);
  waiter

let rec run t =
  Lwt.wakeup_paused ();
  match Lwt.poll t with
    | Some x ->
        x
    | None ->
        begin
          try
            if Lwt_sequence.is_empty yielded then
              libev_loop ()
            else begin
              libev_loop_no_wait ();
              let wakeners = Lwt_sequence.fold_r (fun x l -> x :: l) yielded [] in
              Lwt_sequence.iter_node_l Lwt_sequence.remove yielded;
              List.iter (fun wakener -> wakeup wakener ()) wakeners
            end
          with exn ->
            libev_unloop ();
            raise exn
        end;
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
