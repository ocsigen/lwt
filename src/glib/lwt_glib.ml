(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_glib
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

type glib_poll_fds

type poll_fd = {
  fd : Unix.file_descr;
  mutable events : int;
}

external lwt_glib_real_poll : glib_poll_fds -> int -> poll_fd list -> int -> int -> int = "lwt_glib_real_poll"
external lwt_glib_setup : unit -> unit = "lwt_glib_setup"
external lwt_glib_reset : unit -> unit = "lwt_glib_reset"
external lwt_glib_get_poll_bits : unit -> int * int * int = "lwt_glib_get_poll_bits"

let pollin, pollout, pollerr = lwt_glib_get_poll_bits ()

module FdSet = Set.Make(struct type t = Unix.file_descr let compare = compare end)

let set_of_list l = List.fold_left (fun acc x -> FdSet.add x acc) FdSet.empty l

let select result glib_poll_fds glib_poll_fds_count glib_timeout set_r set_w set_e timeout =
  let set_r = set_of_list set_r and set_w = set_of_list set_w and set_e = set_of_list set_e in
  let poll_fds = FdSet.fold
    (fun fd acc ->
       { fd = fd;
         events = (if FdSet.mem fd set_r then pollin else 0) lor
           (if FdSet.mem fd set_w then pollout else 0) lor
           (if FdSet.mem fd set_e then pollerr else 0) } :: acc)
    (FdSet.union set_r (FdSet.union set_w set_e)) [] in
  let timeout = match timeout with
    | None -> glib_timeout
    | Some t ->
        let t = truncate (t *. 1000.0) in
        if glib_timeout < 0 then
          t
        else
          min glib_timeout t
  in
  result := lwt_glib_real_poll glib_poll_fds glib_poll_fds_count poll_fds (List.length poll_fds) timeout;
  let set_r, set_w, set_e = List.fold_left
    (fun (set_r, set_w, set_e) { fd = fd; events = ev } ->
       ((if ev land pollin <> 0 then fd :: set_r else set_r),
        (if ev land pollout <> 0 then fd :: set_w else set_w),
        (if ev land pollerr <> 0 then fd :: set_e else set_e)))
    ([], [], []) poll_fds in
  (Lazy.lazy_from_fun Unix.gettimeofday, set_r, set_w, set_e)

let select_wrapper glib_poll_fds glib_poll_fds_count glib_timeout =
  let result = ref 0 in
  Lwt.wakeup_paused ();
  ignore (Lwt_main.apply_filters (select result glib_poll_fds glib_poll_fds_count glib_timeout) [] [] [] None);
  !result

let _ = Callback.register "lwt-glib-select" select_wrapper

let integrated = ref false

let init ?setlocale _ =
  let _ = GMain.init ?setlocale () in
  if not !integrated then begin
    lwt_glib_setup ();
    Lwt_main.main_loop_iteration := (fun _ -> ignore (Glib.Main.iteration true));
    integrated := true
  end

let quit _ =
  if !integrated then begin
    lwt_glib_reset ();
    integrated := false
  end;
  GMain.quit ()
