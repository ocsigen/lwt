(*
 * parallelize.ml
 * --------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

(* Reads commands from standard input and launch them in parallel,
   using as many processes as the number of CPUs. *)

open Lwt_unix
open Lwt
open Lwt_io
open Lwt_process

(* Reads one command, launch it and waits for when it termination,
   then start again: *)
let rec launch () =
  read_line_opt stdin >>= function
    | None ->
        return ()
    | Some line ->
        exec (shell line) >> launch ()

(* Creates the initial <N> threads, where <N> is the number of
   CPUs: *)
let rec create_threads = function
  | 0 ->
      return ()
  | n ->
      launch () <&> create_threads (n - 1)

(* Counts the number of CPUs using "/proc/cpuinfo": *)
let cpus_count () =
  Lwt_stream.fold (fun _ n -> succ n)
    (Lwt_stream.filter
       (fun line -> match Text.words line with
          | "processor" :: _ -> true
          | _ -> false)
       (lines_of_file "/proc/cpuinfo")) 0

let _ = Lwt_main.run (cpus_count () >>= create_threads)
