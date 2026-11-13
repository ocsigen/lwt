(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* Every Lwt_main.run installs the SIGCHLD handler, so several loops install
   it at the same time; a shared [lazy] forced from two domains at once would
   raise [Lazy.Undefined]. *)

let check name b =
  if not b
  then (
    Printf.eprintf "FAILED: %s\n" name;
    exit 1)

let domains = 4

(* Spin until every domain has arrived, so that all of them act at once. *)
let rendezvous arrived () =
  ignore (Atomic.fetch_and_add arrived 1);
  while Atomic.get arrived < domains do
    Domain.cpu_relax ()
  done

let at_once f =
  let arrived = Atomic.make 0 in
  let others =
    List.init (domains - 1) (fun _ ->
      Domain.spawn (fun () -> rendezvous arrived (); f ()))
  in
  rendezvous arrived ();
  let here = f () in
  here :: List.map Domain.join others

let () =
  let installed =
    at_once (fun () ->
      match Lwt_unix.install_sigchld_handler () with
      | () -> true
      | exception e ->
          Printf.eprintf "install_sigchld_handler raised %s\n"
            (Printexc.to_string e);
          false)
  in
  check "four domains install the SIGCHLD handler at once"
    (List.for_all Fun.id installed);
  print_endline "SIGCHLD handler installed from several domains: ok"

(* Lwt_io's generator of temporary file names must not be a shared [lazy]
   either: several domains use it at once. No loop runs on the other domains,
   so the directory is created synchronously: [Async_none] runs the job on
   the calling thread, without the loop. *)
let () =
  Lwt_unix.set_default_async_method Lwt_unix.Async_none;
  let dirs =
    at_once (fun () ->
      match Lwt.state (Lwt_io.create_temp_dir ()) with
      | Lwt.Return dir -> Some dir
      | Lwt.Fail e ->
          Printf.eprintf "create_temp_dir failed with %s\n"
            (Printexc.to_string e);
          None
      | Lwt.Sleep ->
          prerr_endline "create_temp_dir did not resolve synchronously";
          None
      | exception e ->
          Printf.eprintf "create_temp_dir raised %s\n" (Printexc.to_string e);
          None)
  in
  List.iter (Option.iter Unix.rmdir) dirs;
  check "four domains draw temporary names at once"
    (List.for_all Option.is_some dirs);
  print_endline "temporary names drawn from several domains: ok"
