(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* The table of open output channels is per domain: [flush_all] flushes the
   calling domain's channels only, and a domain flushes its own channels when
   it terminates. The channels are plain files, so every operation resolves
   without an engine iteration or a job. *)

let failures = ref 0

let check name b =
  if not b
  then (
    Printf.eprintf "FAILED: %s\n" name;
    incr failures)

(* An output channel on a regular file whose writes never need the engine. *)
let channel_on path =
  let fd =
    Unix.openfile path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600
  in
  Lwt_io.of_fd ~mode:Lwt_io.output
    (Lwt_unix.of_unix_file_descr ~blocking:false ~set_flags:false fd)

let contents path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic; s

let () =
  let ours = Filename.temp_file "lwt-chan-ours" ".txt" in
  let theirs = Filename.temp_file "lwt-chan-theirs" ".txt" in
  let at_exit_file = Filename.temp_file "lwt-chan-atexit" ".txt" in
  (* Buffered, and not flushed. *)
  let oc = channel_on ours in
  Lwt_main.run (Lwt_io.write oc "ours");
  check "our write is still buffered" (contents ours = "");
  (* The other domain flushes; that must reach its channel and not ours. *)
  Domain.join
    (Domain.spawn (fun () ->
       let oc = channel_on theirs in
       Lwt_main.run
         ( Lwt_io.write oc "theirs" |> fun p ->
           Lwt.bind p (fun () -> Lwt_io.flush_all ()) );
       check "the other domain flushed its own channel"
         (contents theirs = "theirs");
       check "and left ours untouched" (contents ours = "")));
  check "our channel is still unflushed after their flush_all"
    (contents ours = "");
  (* A domain flushes its own channels when it ends, before [join] returns. *)
  Domain.join
    (Domain.spawn (fun () ->
       let oc = channel_on at_exit_file in
       Lwt_main.run (Lwt_io.write oc "bye");
       check "not flushed yet on the other domain" (contents at_exit_file = "")));
  check "the other domain flushed its channel when it terminated"
    (contents at_exit_file = "bye");
  (* Ours is flushed here, by this domain. *)
  Lwt_main.run (Lwt_io.flush_all ());
  check "our own flush_all reaches our channel" (contents ours = "ours");
  (* A hook registered before the domain's first channel must run before the
     flush of that domain's channels, as on the main domain, where the flush is
     registered at module initialization, ahead of every hook. *)
  let late = Filename.temp_file "lwt-chan-late" ".txt" in
  Domain.join
    (Domain.spawn (fun () ->
       let oc = ref None in
       Lwt_main.at_exit (fun () ->
         match !oc with
         | Some oc -> Lwt_io.write oc " and the hook's"
         | None -> Lwt.return_unit);
       let c = channel_on late in
       oc := Some c;
       Lwt_main.run (Lwt_io.write c "the body's")));
  check "a hook registered before the first channel is flushed too"
    (contents late = "the body's and the hook's");
  (try Sys.remove late with _ -> ());
  List.iter
    (fun p -> try Sys.remove p with _ -> ())
    [ours; theirs; at_exit_file];
  if !failures > 0 then exit 1;
  print_endline "per-domain channel registry: ok"
