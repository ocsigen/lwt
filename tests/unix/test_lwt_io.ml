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

open Test
open Lwt.Infix

let with_async_exception_hook hook f =
  let old_hook = !Lwt.async_exception_hook in
  Lwt.async_exception_hook := hook;
  f () >|= fun v ->
  Lwt.async_exception_hook := old_hook;
  v

let local = Unix.ADDR_INET (Unix.inet_addr_loopback, 4321)

(* Helpers for [establish_server] tests. *)
module Establish_server =
struct
  let with_client f =
    let handler_finished, notify_handler_finished = Lwt.wait () in

    Lwt_io.establish_server
      local
      (fun channels ->
        Lwt.finalize
          (fun () -> f channels)
          (fun () ->
            Lwt.wakeup notify_handler_finished ();
            Lwt.return_unit))

    >>= fun server ->

    let client_finished =
      Lwt_io.with_connection
        local
        (fun (_, out_channel) ->
          Lwt_io.write out_channel "hello world" >>= fun () ->
          handler_finished)
    in

    client_finished >>= fun () ->
    Lwt_io.shutdown_server server

  (* Dirty hack for forcing [Lwt_io.close] to fail, to test response to [close]
     exceptions. Impolitely closes the [n]th last file descriptor allocated by
     the system, without going through [Lwt_io].

     This assumes that the system allocates contiguously-increasing file
     descriptors whenever possible, and can only be "correctly" done due to
     exact control of file descriptors during testing.

     The reasons for writing this are as follows:
     - [EBADF] is the only error we can reliably produce on [close].
     - This requires a closed file descriptor.
     - If we go through [Lwt_io] to close twice, it will not run the second
       [close] operation, but simply return the result of the first.
     - The [Lwt_io] interface does not allow retrieving a file descriptor from a
       channel.
     - Indeed, channels are not, in general, associated with file descriptors,
       and the file descriptors for channels that are, are floating in closures.
       So, there is no internal state that can be easily exposed to get the file
       descriptor, even when there is one.

     This may not work on some systems, so the corresponding tests will have to
     be disabled. *)
  let close_last_fd n =
    let guess_fd () =
      (* Using a pipe because it is easy and has no file system consequences. *)
      let fd1, fd2 = Unix.pipe () in
      Unix.close fd1;
      Unix.close fd2;

      (* Make it possible to do arithmetic on file descriptors. Dreams can come
         true! *)
      let
        module Pierce_abstraction =
        struct
          external pierce_fd : Unix.file_descr -> int = "%identity"
          external hide_fd : int -> Unix.file_descr = "%identity"
        end
      in

      let fd1, fd2 = Pierce_abstraction.(pierce_fd fd1, pierce_fd fd2) in
      let lowest = min fd1 fd2 in
      let fd = lowest - n in
      Pierce_abstraction.hide_fd fd
    in

    Unix.close (guess_fd ())

  (* Hacky is_closed functions that attempt to read from/write to the channels
     to see if they are closed. *)
  let is_closed_in channel =
    Lwt.catch
      (fun () -> Lwt_io.read_char channel >|= fun _ -> false)
      (function
      | Lwt_io.Channel_closed _ -> Lwt.return_true
      | _ -> Lwt.return_false)

  let is_closed_out channel =
    Lwt.catch
      (fun () -> Lwt_io.write_char channel 'a' >|= fun () -> false)
      (function
      | Lwt_io.Channel_closed _ -> Lwt.return_true
      | _ -> Lwt.return_false)
end

let suite = suite "lwt_io" [
  test "auto-flush"
    (fun () ->
       let sent = ref [] in
       let oc = Lwt_io.make ~mode:Lwt_io.output (fun buf ofs len ->
                                            let bytes = Bytes.create len in
                                            Lwt_bytes.blit_to_bytes buf ofs bytes 0 len;
                                            sent := bytes :: !sent;
                                            Lwt.return len) in
       Lwt_io.write oc "foo" >>= fun () ->
       Lwt_io.write oc "bar" >>= fun () ->
       if !sent <> [] then
         Lwt.return false
       else
         Lwt_unix.yield () >>= fun () ->
         Lwt.return (!sent = [Bytes.of_string "foobar"]));

  test "auto-flush in atomic"
    (fun () ->
       let sent = ref [] in
       let oc = Lwt_io.make ~mode:Lwt_io.output (fun buf ofs len ->
                                     let bytes = Bytes.create len in
                                     Lwt_bytes.blit_to_bytes buf ofs bytes 0 len;
                                     sent := bytes :: !sent;
                                     Lwt.return len) in
       Lwt_io.atomic
         (fun oc ->
            Lwt_io.write oc "foo" >>= fun () ->
            Lwt_io.write oc "bar" >>= fun () ->
            if !sent <> [] then
              Lwt.return false
            else
              Lwt_unix.yield () >>= fun () ->
              Lwt.return (!sent = [Bytes.of_string "foobar"]))
         oc);

  (* Without the corresponding bugfix, which is to handle ENOTCONN from
     Lwt_unix.shutdown, this test raises an exception from the handler's calls
     to close. *)
  test "establish_server_1: shutdown: client closes first"
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
        (Lwt_io.Versioned.establish_server_1 [@ocaml.warning "-3"])
          local (fun channels -> Lwt.wakeup run_handler channels)
      in

      Lwt_io.with_connection local (fun _ -> Lwt.return_unit) >>= fun () ->
      Lwt.wakeup client_finished ();
      Lwt_io.shutdown_server server >>= fun () ->
      handler);

  (* Counterpart to establish_server: shutdown test. Confirms that shutdown is
     implemented correctly in open_connection. *)
  test "open_connection: shutdown: server closes first"
    (fun () ->
      let wait_for_server, server_finished = Lwt.wait () in

      let server =
        (Lwt_io.Versioned.establish_server_1 [@ocaml.warning "-3"])
          local (fun (in_channel, out_channel) ->
            Lwt.async (fun () ->
              Lwt_io.close in_channel >>= fun () ->
              Lwt_io.close out_channel >|= fun () ->
              Lwt.wakeup server_finished ()))
      in

      Lwt_io.with_connection local (fun _ ->
        wait_for_server >>= fun () ->
        Lwt.return_true)

      >>= fun result ->

      Lwt_io.shutdown_server server >|= fun () ->
      result);

  test "establish_server: implicit close"
    (fun () ->
      let open Establish_server in

      let in_channel' = ref Lwt_io.stdin in
      let out_channel' = ref Lwt_io.stdout in

      let in_open_in_handler = ref false in
      let out_open_in_handler = ref false in

      let run =
        Establish_server.with_client
          (fun (in_channel, out_channel) ->
            in_channel' := in_channel;
            out_channel' := out_channel;

            is_closed_out out_channel >>= fun yes ->
            out_open_in_handler := not yes;

            is_closed_in in_channel >|= fun yes ->
            in_open_in_handler := not yes)
      in

      run >>= fun () ->
      (* Give a little time for the close system calls on the connection sockets
         to complete. The Lwt_io and Lwt_unix APIs do not currently allow
         binding on the implicit closes of these sockets, so resorting to a
         delay. *)
      Lwt_unix.sleep 0.05 >>= fun () ->

      is_closed_in !in_channel' >>= fun in_closed_after_handler ->
      is_closed_out !out_channel' >|= fun out_closed_after_handler ->

      !out_open_in_handler &&
      !in_open_in_handler &&
      in_closed_after_handler &&
      out_closed_after_handler);

  test "establish_server: implicit close on exception"
    (fun () ->
      let open Establish_server in

      let in_channel' = ref Lwt_io.stdin in
      let out_channel' = ref Lwt_io.stdout in
      let exit_raised = ref false in

      let run () =
        Establish_server.with_client
          (fun (in_channel, out_channel) ->
            in_channel' := in_channel;
            out_channel' := out_channel;
            raise Exit)
      in

      with_async_exception_hook
        (function
        | Exit -> exit_raised := true;
        | _ -> ())
        run

      >>= fun () ->
      (* See comment in other implicit close test. *)
      Lwt_unix.sleep 0.05 >>= fun () ->

      is_closed_in !in_channel' >>= fun in_closed_after_handler ->
      is_closed_out !out_channel' >|= fun out_closed_after_handler ->

      in_closed_after_handler && out_closed_after_handler);

  (* This does a simple double close of the channels (second close is implicit).
     If something breaks, the test will finish with an exception, or
     Lwt.async_exception_hook will kill the process. *)
  test "establish_server: explicit close"
    (fun () ->
      let open Establish_server in

      let closed_explicitly = ref false in

      let run =
        Establish_server.with_client
          (fun (in_channel, out_channel) ->
            Lwt_io.close in_channel >>= fun () ->
            Lwt_io.close out_channel >>= fun () ->
            is_closed_in in_channel >>= fun in_closed_in_handler ->
            is_closed_out out_channel >|= fun out_closed_in_handler ->
            closed_explicitly := in_closed_in_handler && out_closed_in_handler)
      in

      run >|= fun () ->
      !closed_explicitly);

  (* Screws up the open sockets so that shutdown or close results in EBADF.
     Then, closes the channels and observes the expected exceptions. Then,
     allows the implicit closing code to run. If this code tries to close the
     sockets again, the exception will go to Lwt.async_exception_hook and kill
     the tester. The correct behavior is for implicit close to do nothing if the
     user already tried to close the sockets. *)
  test "establish_server: no duplicate exceptions"
    ~only_if:(fun () -> not Sys.win32)
    (fun () ->
      let open Establish_server in

      let exceptions_observed = ref 0 in
      let expecting_ebadf f =
        Lwt.catch f (function
          | Unix.Unix_error (Unix.EBADF, _, _) ->
            exceptions_observed := !exceptions_observed + 1;
            Lwt.return_unit
          | exn -> Lwt.fail exn) [@ocaml.warning "-4"]
      in

      let run =
        Establish_server.with_client
          (fun (in_channel, out_channel) ->
            close_last_fd 1;
            expecting_ebadf (fun () -> Lwt_io.close in_channel) >>= fun () ->
            expecting_ebadf (fun () -> Lwt_io.close out_channel))
      in

      run >|= fun () ->
      !exceptions_observed = 2);

  (* Screws up the open sockets so closing them fails with EBADF. Then, raises
     an exception from the handler. Checks that the handler exception arrives
     at Lwt.async_exception_hook before the exceptions from implicit close. *)
  test "establish_server: order of exceptions"
    ~only_if:(fun () -> not Sys.win32)
    (fun () ->
      let open Establish_server in

      let exceptions_observed = ref 0 in
      let correct_exceptions = ref true in
      let see_exception exn =
        exceptions_observed := !exceptions_observed + 1;
        (match !exceptions_observed, exn with
        | 1, Exit
        | (2 | 3), Unix.Unix_error (Unix.EBADF, _, _) -> ()
        | _ -> correct_exceptions := false) [@ocaml.warning "-4"]
      in

      let run () =
        Establish_server.with_client
          (fun (_in_channel, _out_channel) ->
            close_last_fd 1;
            raise Exit)
      in

      with_async_exception_hook see_exception run >|= fun () ->
      !exceptions_observed = 3 && !correct_exceptions);

  test "with_connection"
    (fun () ->
      let open Establish_server in

      let in_channel' = ref Lwt_io.stdin in
      let out_channel' = ref Lwt_io.stdout in

      Lwt_io.establish_server local (fun _ -> Lwt.return_unit)
      >>= fun server ->

      Lwt_io.with_connection local (fun (in_channel, out_channel) ->
        in_channel' := in_channel;
        out_channel' := out_channel;
        Lwt.return_unit)

      >>= fun () ->
      Lwt_io.shutdown_server server >>= fun () ->
      is_closed_in !in_channel' >>= fun in_closed ->
      is_closed_out !out_channel' >|= fun out_closed ->
      in_closed && out_closed);

  (* Makes the socket fail with EBADF on close. Tries to close the socket
     manually, and handles the exception. When with_connection tries to close
     the socket again implicitly, that should not raise the exception again. *)
  test "with_connection: no duplicate exceptions"
    ~only_if:(fun () -> not Sys.win32)
    (fun () ->
      let open Establish_server in

      let exceptions_observed = ref 0 in
      let expecting_ebadf f =
        Lwt.catch f (function
          | Unix.Unix_error (Unix.EBADF, _, _) ->
            exceptions_observed := !exceptions_observed + 1;
            Lwt.return_unit
          | exn -> Lwt.fail exn) [@ocaml.warning "-4"]
      in

      let handler_started, notify_handler_started = Lwt.wait () in
      let finish_server, resume_server = Lwt.wait () in
      Lwt_io.establish_server local
        (fun _ ->
          Lwt.wakeup notify_handler_started ();
          finish_server) >>= fun server ->

      expecting_ebadf (fun () ->
        Lwt_io.with_connection local (fun (in_channel, out_channel) ->
          handler_started >>= fun () ->
          close_last_fd 2;
          expecting_ebadf (fun () -> Lwt_io.close in_channel) >>= fun () ->
          expecting_ebadf (fun () -> Lwt_io.close out_channel)))

      >>= fun () ->
      Lwt.wakeup resume_server ();
      Lwt_io.shutdown_server server >|= fun () ->
      !exceptions_observed = 2);
]
