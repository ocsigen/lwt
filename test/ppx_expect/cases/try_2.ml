#use "topfind";;
#require "lwt";;
#require "lwt.ppx";;

let _ =
  try%lwt
    Lwt.return ()
  with _ -> 5;;
