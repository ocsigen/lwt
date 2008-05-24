
type t =
    Plain
  | SSL of Ssl.socket

type socket = Lwt_unix.file_descr * t

let is_ssl s =
  match snd s with
    Plain -> false
  | _ -> true

let wrap_call f () =
  try
    f ()
  with
    (Ssl.Connection_error err | Ssl.Accept_error err |
     Ssl.Read_error err | Ssl.Write_error err) as e ->
      match err with
        Ssl.Error_want_read ->
          raise Lwt_unix.Retry_read
     | Ssl.Error_want_write ->
          raise Lwt_unix.Retry_write
     | _ ->
          raise e

let repeat_call fd f =
  try
    Lwt_unix.check_descriptor fd;
    Lwt.return (wrap_call f ())
  with
    Lwt_unix.Retry_read ->
      Lwt_unix.register_action Lwt_unix.inputs fd (wrap_call f)
  | Lwt_unix.Retry_write ->
      Lwt_unix.register_action Lwt_unix.outputs fd (wrap_call f)
  | e ->
      Lwt.fail e

(****)

let plain fd = (fd, Plain)

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

let read (fd, s) buf pos len =
  match s with
    Plain -> Lwt_unix.read fd buf pos len
  | SSL s -> if len = 0 then Lwt.return 0 else
             repeat_call fd (fun () ->
             try Ssl.read s buf pos len
             with Ssl.Read_error Ssl.Error_zero_return -> 0)

let write (fd, s) buf pos len =
  match s with
    Plain -> Lwt_unix.write fd buf pos len
  | SSL s -> if len = 0 then Lwt.return 0 else
             repeat_call fd (fun () -> Ssl.write s buf pos len)

let wait_read (fd, s) =
  match s with
    Plain -> Lwt_unix.wait_read fd
  | SSL _ -> Lwt_unix.yield ()

let wait_write (fd, s) =
  match s with
    Plain -> Lwt_unix.wait_write fd
  | SSL _ -> Lwt_unix.yield ()

let out_channel_of_descr s =
  Lwt_chan.make_out_channel (fun buf pos len -> write s buf pos len)

let in_channel_of_descr s =
  Lwt_chan.make_in_channel (fun buf pos len -> read s buf pos len)

let ssl_shutdown (fd, s) =
  match s with
    Plain -> Lwt.return ()
  | SSL s -> repeat_call fd (fun () -> Ssl.shutdown s)

let shutdown (fd, _) cmd = Lwt_unix.shutdown fd cmd

let close (fd, _) = Lwt_unix.close fd

let abort (fd, _) = Lwt_unix.abort fd
