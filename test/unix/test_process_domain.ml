(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* With [posix_spawnp], Lwt_process works while other domains are alive (fork
   would fail). Only meaningful when posix_spawnp is available and there is no
   [?cwd]. *)

open Lwt.Infix

let () =
  let stop = Atomic.make false in
  let d =
    Domain.spawn (fun () ->
      while not (Atomic.get stop) do
        Domain.cpu_relax ()
      done)
  in
  let ok =
    Lwt_main.run
      (Lwt_list.for_all_p
         (fun i ->
            Lwt_process.pread_line ("echo", [|"echo"; string_of_int i|])
            >|= fun l -> l = string_of_int i)
         (List.init 50 Fun.id))
  in
  Atomic.set stop true;
  Domain.join d;
  if ok then print_endline "ok" else (prerr_endline "FAIL"; exit 1)
