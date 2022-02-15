
open Lwt.Syntax

let suspend f =
  let* () = Lwt.pause() in
  f()

let rec sleep_rec n =
  let before = if n >= 2 then sleep_rec (n-1) else Lwt.return () in
  let cur = Lwt_unix.sleep (float n) in
  suspend @@ fun () ->
  Lwt.await (Lwt.join [before; cur]);
  Printf.printf "sleep %d done \n%!" n;
  Lwt.return ()

let main () =
  let l = List.init 10 sleep_rec in
  suspend @@ fun () ->
  List.iter Lwt.await l;
  Lwt.return()

let () =
  Lwt_main.run @@ main ()
