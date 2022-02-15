
open Lwt.Syntax

let suspend f =
  let* () = Lwt.pause() in
  f()

let f n =
  suspend @@ fun () ->
  let+ () = Lwt_unix.sleep (float n) in
  Printf.printf "sleep %d done \n%!" n

let main () =
  let l = List.init 15 f in
  suspend @@ fun () ->
  List.iter Lwt.await l;
  Lwt.return()

let () =
  Lwt_main.run @@ main ()
