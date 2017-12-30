(* OCaml promise library
 * http://www.ocsigen.org/lwt
 * Copyright (C) 2005-2008 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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

open Lwt.Infix

type t =
    Plain
  | SSL of Ssl.socket

type socket = Lwt_unix.file_descr * t

type uninitialized_socket = Lwt_unix.file_descr * Ssl.socket

let ssl_socket (_fd, kind) =
  match kind with
  | Plain -> None
  | SSL socket -> Some socket

let ssl_socket_of_uninitialized_socket (_fd, socket) = socket

let is_ssl s =
  match snd s with
    Plain -> false
  | SSL _ -> true

let wrap_call f () =
  try
    f ()
  with
    (Ssl.Connection_error err | Ssl.Accept_error err |
     Ssl.Read_error err | Ssl.Write_error err) as e ->
    (match err with
       Ssl.Error_want_read ->
       raise Lwt_unix.Retry_read
     | Ssl.Error_want_write ->
       raise Lwt_unix.Retry_write
     | _ ->
       raise e) [@ocaml.warning "-4"]

let repeat_call fd f =
  (* See https://github.com/ocsigen/lwt/issues/530. *)
  Lwt_unix.blocking fd >>= fun _blocking ->
  try
    Lwt.return (wrap_call f ())
  with
    Lwt_unix.Retry_read ->
    Lwt_unix.register_action Lwt_unix.Read fd (wrap_call f)
  | Lwt_unix.Retry_write ->
    Lwt_unix.register_action Lwt_unix.Write fd (wrap_call f)
  | e ->
    Lwt.fail e

(****)

let plain fd = (fd, Plain)

let embed_socket fd context = (fd, SSL(Ssl.embed_socket (Lwt_unix.unix_file_descr fd) context))
let embed_uninitialized_socket fd context = (fd, Ssl.embed_socket (Lwt_unix.unix_file_descr fd) context)

let ssl_accept fd ctx =
  let socket = Ssl.embed_socket (Lwt_unix.unix_file_descr fd) ctx in
  Lwt.bind
    (repeat_call fd (fun () -> Ssl.accept socket)) (fun () ->
      Lwt.return (fd, SSL socket))

let ssl_connect fd ctx =
  let socket = Ssl.embed_socket (Lwt_unix.unix_file_descr fd) ctx in
  Lwt.bind
    (repeat_call fd (fun () -> Ssl.connect socket)) (fun () ->
      Lwt.return (fd, SSL socket))

let ssl_accept_handshake (fd, socket) =
  Lwt.bind
    (repeat_call fd (fun () -> Ssl.accept socket)) (fun () ->
      Lwt.return (fd, SSL socket))

let ssl_perform_handshake (fd, socket) =
  Lwt.bind
    (repeat_call fd (fun () -> Ssl.connect socket)) (fun () ->
      Lwt.return (fd, SSL socket))

let read (fd, s) buf pos len =
  match s with
  | Plain ->
    Lwt_unix.read fd buf pos len
  | SSL s ->
    if len = 0 then
      Lwt.return 0
    else
      repeat_call fd
        (fun () ->
           try
             Ssl.read s buf pos len
           with Ssl.Read_error Ssl.Error_zero_return ->
             0)

let read_bytes (fd, s) buf pos len =
  match s with
  | Plain ->
    Lwt_bytes.read fd buf pos len
  | SSL s ->
    if len = 0 then
      Lwt.return 0
    else
      repeat_call fd
        (fun () ->
           try
             Ssl.read_into_bigarray s buf pos len
           with Ssl.Read_error Ssl.Error_zero_return ->
             0)

let write (fd, s) buf pos len =
  match s with
  | Plain ->
    Lwt_unix.write fd buf pos len
  | SSL s ->
    if len = 0 then
      Lwt.return 0
    else
      repeat_call fd
        (fun () ->
           Ssl.write s buf pos len)

let write_bytes (fd, s) buf pos len =
  match s with
  | Plain ->
    Lwt_bytes.write fd buf pos len
  | SSL s ->
    if len = 0 then
      Lwt.return 0
    else
      repeat_call fd
        (fun () -> Ssl.write_bigarray s buf pos len)

let wait_read (fd, s) =
  match s with
    Plain -> Lwt_unix.wait_read fd
  | SSL _ -> Lwt_unix.yield ()

let wait_write (fd, s) =
  match s with
    Plain -> Lwt_unix.wait_write fd
  | SSL _ -> Lwt_unix.yield ()

let ssl_shutdown (fd, s) =
  match s with
    Plain -> Lwt.return_unit
  | SSL s -> repeat_call fd (fun () -> Ssl.shutdown s)

let shutdown (fd, _) cmd = Lwt_unix.shutdown fd cmd

let close (fd, _) = Lwt_unix.close fd

let abort (fd, _) = Lwt_unix.abort fd

let shutdown_and_close s =
  ssl_shutdown s >>= fun () ->
  Lwt.wrap2 shutdown s Unix.SHUTDOWN_ALL >>= fun () ->
  close s

let out_channel_of_descr ?buffer s =
  Lwt_io.make
    ?buffer
    ~mode:Lwt_io.output
    ~close:(fun () -> shutdown_and_close s)
    (fun buf pos len -> write_bytes s buf pos len)

let in_channel_of_descr ?buffer s =
  Lwt_io.make
    ?buffer
    ~mode:Lwt_io.input
    ~close:(fun () -> shutdown_and_close s)
    (fun buf pos len -> read_bytes s buf pos len)

let get_fd (fd, _socket) = fd

let get_unix_fd (fd,socket) =
  match socket with
  | Plain -> Lwt_unix.unix_file_descr fd
  | SSL socket -> (Ssl.file_descr_of_socket socket)

let getsockname s =
  Unix.getsockname (get_unix_fd s)

let getpeername s =
  Unix.getpeername (get_unix_fd s)
