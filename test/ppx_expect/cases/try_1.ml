#use "topfind";;
#require "lwt";;
#require "lwt.ppx";;

let _ =
  try%lwt
    5
  with _ -> Lwt.return ();;
