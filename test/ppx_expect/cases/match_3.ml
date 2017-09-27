#use "topfind";;
#require "lwt";;
#require "lwt.ppx";;

let _ =
  match%lwt
    Lwt.return ()
  with
  | () ->
    5;;
