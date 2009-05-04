
(* Show code of key pressed *)

open Lwt_pervasives

let rec loop () =
  lwt raw_key = Lwt_term.parse_key_raw Lwt_term.standard_input in
  let key = Lwt_term.decode_key raw_key in
  printlf "raw_key = %S, key = %s" raw_key (Lwt_term.string_of_key key) >>
  if key = Lwt_term.Key_escape then
    return ()
  else
    loop ()

let _ =
  Lwt_main.run
    (printl "Press escape to exit." >> Lwt_term.with_raw_mode loop)
