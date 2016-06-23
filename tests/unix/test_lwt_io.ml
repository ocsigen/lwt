(* Lightweight thread library for OCaml
 * http://www.ocsigen.org/lwt
 * Module Test_lwt_io
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
open Lwt_io
open Test

let local = Unix.ADDR_INET (Unix.inet_addr_loopback, 4321)

let suite = suite "lwt_io" [
  test "auto-flush"
    (fun () ->
       let sent = ref [] in
       let oc = Lwt_io.make ~mode:output (fun buf ofs len ->
                                            let bytes = Bytes.create len in
                                            Lwt_bytes.blit_to_bytes buf ofs bytes 0 len;
                                            sent := bytes :: !sent;
                                            return len) in
       write oc "foo" >>= fun () ->
       write oc "bar" >>= fun () ->
       if !sent <> [] then
         return false
       else
         Lwt_unix.yield () >>= fun () ->
         return (!sent = [Bytes.of_string "foobar"]));

  test "auto-flush in atomic"
    (fun () ->
       let sent = ref [] in
       let oc = make ~mode:output (fun buf ofs len ->
                                     let bytes = Bytes.create len in
                                     Lwt_bytes.blit_to_bytes buf ofs bytes 0 len;
                                     sent := bytes :: !sent;
                                     return len) in
       atomic
         (fun oc ->
            write oc "foo" >>= fun () ->
            write oc "bar" >>= fun () ->
            if !sent <> [] then
              return false
            else
              Lwt_unix.yield () >>= fun () ->
              return (!sent = [Bytes.of_string "foobar"]))
         oc);

  (* Without the corresponding bugfix, which is to handle ENOTCONN from
     Lwt_unix.shutdown, this test raises an exception from the handler's calls
     to close. *)
  test "establish_server: shutdown: client closes first"
    (fun () ->
      let wait_for_client, client_finished = Lwt.wait () in

      let handler_wait, run_handler = Lwt.wait () in
      let handler =
        handler_wait >>= fun (in_channel, out_channel) ->
        wait_for_client >>= fun () ->
        Lwt_io.close in_channel >>= fun () ->
        Lwt_io.close out_channel >>= fun () ->
        Lwt.return_true
      in

      let server =
        Lwt_io.establish_server
          local (fun channels -> Lwt.wakeup run_handler channels)
      in

      with_connection local (fun _ -> Lwt.return_unit) >>= fun () ->
      Lwt.wakeup client_finished ();
      Lwt_io.shutdown_server server;
      handler);

  (* Counterpart to establish_server: shutdown test. Confirms that shutdown is
     implemented correctly in open_connection. *)
  test "open_connection: shutdown: server closes first"
    (fun () ->
      let wait_for_server, server_finished = Lwt.wait () in

      let server =
        Lwt_io.establish_server local (fun (in_channel, out_channel) ->
          Lwt.async (fun () ->
            Lwt_io.close in_channel >>= fun () ->
            Lwt_io.close out_channel >|= fun () ->
            Lwt.wakeup server_finished ()))
      in

      with_connection local (fun _ ->
        wait_for_server >>= fun () ->
        Lwt.return_true)

      >|= fun result ->

      Lwt_io.shutdown_server server;
      result);
]
