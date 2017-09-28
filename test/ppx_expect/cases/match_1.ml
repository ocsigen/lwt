#use "topfind";;
#require "lwt";;
#require "lwt.ppx";;

let _ =
  match%lwt
    Lwt.return 5
  with
  | () ->
    Lwt.return ();;
