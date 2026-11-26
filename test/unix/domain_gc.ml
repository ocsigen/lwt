(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* [Lwt_gc] runs the deferred function on the loop that asked for it, since
   its finalizer sends a notification to that loop. A finalizer whose
   registering domain has gone sends to a retired channel, which drops it:
   the function is not run, and nothing crashes. Needs a second domain, hence
   OCaml 5. *)

let failures = ref 0

let check name b =
  if not b
  then (
    Printf.eprintf "FAILED: %s\n" name;
    incr failures)

let ran_on = Atomic.make (-1)
let ran_after_death = Atomic.make false

let () =
  (* A hang, at a domain exit for example, must show as a failure. A thread
     rather than an alarm, since the test also runs on Windows. *)
  ignore
    (Thread.create
       (fun () ->
          Thread.delay 60.;
          prerr_endline "domain_gc: timed out";
          Unix._exit 2)
       ());
  (* The deferred function runs on the domain that registered it. *)
  let spawned =
    Domain.join
      (Domain.spawn (fun () ->
         let mine = (Domain.self () :> int) in
         (let r = ref 0 in
          Lwt_gc.finalise
            (fun _ ->
               Atomic.set ran_on (Domain.self () :> int);
               Lwt.return_unit)
            r;
          ignore (Sys.opaque_identity r));
         Gc.full_major ();
         Lwt_main.run (Lwt_unix.sleep 0.05);
         mine))
  in
  check "the deferred function ran on the domain that registered it"
    (Atomic.get ran_on = spawned);
  (* And one registered by a domain that then dies is dropped, quietly. *)
  Domain.join
    (Domain.spawn (fun () ->
       let r = ref 0 in
       Lwt_gc.finalise
         (fun _ ->
            Atomic.set ran_after_death true;
            Lwt.return_unit)
         r;
       ignore (Sys.opaque_identity r)));
  Gc.full_major ();
  Lwt_main.run (Lwt_unix.sleep 0.05);
  check "a finalizer of a departed domain is dropped rather than misrouted"
    (not (Atomic.get ran_after_death));
  (* A channel whose domain has gone: the domain closed it in its exit hook,
     and its finalizer, run on another domain, must leave it alone instead of
     raising. *)
  let path = Filename.temp_file "lwt-orphan" ".txt" in
  let oc = open_out path in
  output_string oc "one\ntwo\n";
  close_out oc;
  (* The lowest free descriptors, sixteen of them: the same list once the
     domain has gone means it left nothing open, its loop included. Windows
     does not hand out the lowest free handle first, so the check is skipped
     there. *)
  let free_fds () =
    let fds =
      List.init 16 (fun _ -> Unix.openfile Filename.null [Unix.O_RDONLY] 0)
    in
    List.iter Unix.close fds; fds
  in
  let before = free_fds () in
  Domain.join
    (Domain.spawn (fun () ->
       Lwt_main.run
         (Lwt.map (fun _ -> ()) (Lwt_stream.get (Lwt_io.lines_of_file path)))));
  if not Sys.win32
  then
    check "a departed domain closed its abandoned channel" (free_fds () = before);
  let adopted_finaliser_raised =
    match Gc.full_major (); Gc.full_major () with
    | () -> false
    | exception Lwt.Domain_crossing -> true
  in
  (try Sys.remove path with Sys_error _ -> ());
  check "a departed domain's channel finalizer does not raise here"
    (not adopted_finaliser_raised);
  if !failures > 0 then exit 1;
  print_endline "Lwt_gc follows the domain that registered: ok"
