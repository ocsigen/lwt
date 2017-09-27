#use "topfind";;
#require "lwt";;
#require "lwt.ppx";;

let _ =
  match%lwt
    Lwt.return ()
  with
  | () ->
    Lwt.return ()
  | exception End_of_file ->
    Lwt.return 5
