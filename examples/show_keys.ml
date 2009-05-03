
(* Show code of key pressed *)

open Lwt
open Lwt_io

let rec loop () =
  lwt raw_key = Lwt_term.parse_key_raw Lwt_stream.standard_text in
  let key = Lwt_term.decode_key raw_key in
  Lwt_printf.printf "raw_key = %S, key = %s\r\n" raw_key (Lwt_term.string_of_key key) >>
  if key = Lwt_term.Key_escape then
    return ()
  else
    loop ()

let _ =
  Lwt_main.run
    (put_line stdout "Press escape to exit." >> Lwt_term.with_raw_mode loop)
