(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* Exit hooks run on the domain that registered them, while that domain is
   still alive. *)

let () =
  (* A hook of the main domain: the spawned domain's exit must not run it. *)
  let main_hook_runs = ref 0 in
  Lwt_main.at_exit (fun () -> incr main_hook_runs; Lwt.return_unit);
  let ran_on = Atomic.make (-1) in
  let spawned_id = Atomic.make (-2) in
  let d =
    Domain.spawn (fun () ->
      Atomic.set spawned_id (Domain.self () :> int);
      Lwt_main.at_exit (fun () ->
        Atomic.set ran_on (Domain.self () :> int);
        Lwt.return_unit))
  in
  Domain.join d;
  (* Domain.at_exit fires before the join returns, so by now the hook must have
     run, and it must have run on the spawned domain. *)
  let ran = Atomic.get ran_on and spawned = Atomic.get spawned_id in
  if ran = -1
  then begin
    prerr_endline
      "the spawned domain's exit hook did not run when the domain exited";
    exit 1
  end;
  if ran <> spawned
  then begin
    Printf.eprintf
      "the spawned domain's exit hook ran on domain %d instead of %d\n" ran
      spawned;
    exit 1
  end;
  (* And the main domain's hooks are its own: the spawned domain's exit did not
     drain them. They run at process exit, after this. *)
  if !main_hook_runs <> 0
  then begin
    Printf.eprintf
      "the main domain's exit hook ran %d times when the spawned domain exited\n"
      !main_hook_runs;
    exit 1
  end;
  print_endline "per-domain exit hooks: ok"
