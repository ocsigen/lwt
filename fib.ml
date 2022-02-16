
open Lwt.Syntax

let suspend f =
  let* () = Lwt.pause() in
  f()

let rec sleepy_fib n =
  if n <= 2 then (
    Lwt.await @@ Lwt_unix.sleep 0.2;
    Lwt.return 1
  ) else (
    let f1 = sleepy_fib (n-1) in
    let f2 = sleepy_fib (n-2) in
    Lwt.await @@ Lwt_unix.sleep 0.2;
    let res = Lwt.await f1 + Lwt.await f2 in
    Printf.printf "fib %d = %d\n%!" n res;
    Lwt.return res
  )

let main () =
  suspend @@ fun () ->
  let _res = Lwt.await @@ sleepy_fib 10 in
  Lwt.return()

let () =
  Lwt_main.run @@ main ()
