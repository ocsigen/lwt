open Lwt
open Bigarray

let file = ref ""
let () = Arg.parse [] (fun x -> file := x) "plop"

let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0
let () = Lwt_unix.run (Lwt_unix.connect sock (Unix.ADDR_INET ( Unix.inet_addr_loopback , 4567) ))

let len = (Unix.stat !file).Unix.st_size
let file_fd = Unix.openfile !file [] 0

let rec ping () =
  lwt () = Lwt_unix.sleep 0.1 in
  lwt () = Lwt_io.print "." in
  ping ()

let _ = ping ()

let start = Unix.gettimeofday ()
let n = Lwt_unix.run (Lwt_mmap.sendfile file_fd sock 0 len)
let stop = Unix.gettimeofday ()
let time = stop -. start
let _ = Printf.printf "\ntime: %fs speed: %fmo/s\n%!" time (((float len)/.time) /. 1048576.)
